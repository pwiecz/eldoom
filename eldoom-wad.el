;;; eldoom-wad.el --- Reading Doom WAD files -*- lexical-binding:t -*-

;; Copyright (C) 2022 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;;; Commentary

;;; Code

(require 'cl-lib)
(require 'bindat)
(require 'generator)
(require 'seq)
(require 'eldoom-info)
(require 'eldoom-math)

(defvar-local ewd-current-map nil)

(defun ewd--read-bytes (file-name &optional beg end)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally file-name nil beg end)
    (buffer-substring-no-properties (point-min) (point-max))))

(defconst ewd--prefix-bindat-spec
  (bindat-type
    (:wad-type str 4)
    (:directory-entry-count sint 32 t)
    (:directory-offset sint 32 t)))

(defconst ewd--directory-entry-bindat-spec
  (bindat-type
    (:offset sint 32 t)
    (:size sint 32 t)
    (:name strz 8)))

(defconst ewd--patch-header-bindat-spec
  (bindat-type
    (:width uint 16 t)
    (:height uint 16 t)
    (:x-offset sint 16 t)
    (:y-offset sint 16 t)
    (:column-offsets vec width uint 32 t)))

(defconst ewd--patch-post-bindat-spec
  (bindat-type
    (:y-offset byte)
    (:size byte)
    (_ fill 1)
    (:data vec size byte)
    (_ fill 1)))

(defconst ewd--colormap-bindat-spec
  (bindat-type
    (_ vec 34 vec 256 byte)))

(defconst ewd--palette-bindat-spec
  (bindat-type
    (_ vec 14 vec 256 vec 3 byte)))

(defconst ewd--blockmap-header-spec
  (bindat-type
    (:origin-x sint 16 t)
    (:origin-y sint 16 t)
    (:column-count uint 16 t)
    (:row-count uint 16 t)))

(defun ewd--blockmap-offsets-spec (offset-count)
  (bindat-type
    (_ vec offset-count uint 16 t)))

(defun ewd--linedefs-bindat-spec (linedef-count)
  (bindat-type
    (_ vec linedef-count struct
       (:start-vertex uint 16 t)
       (:end-vertex uint 16 t)
       (:flags uint 16 t)
       (:special uint 16 t)
       (:sector-tag uint 16 t)
       (:front-sidedef uint 16 t)
       (:back-sidedef uint 16 t))))

(defun ewd--things-bindat-spec (thing-count)
  (bindat-type 
    (_ vec thing-count struct
       (:x sint 16 t)
       (:y sint 16 t)
       (:angle sint 16 t)
       (:type sint 16 t)
       (:flags sint 16 t))))

(defun ewd--sidedefs-bindat-spec (sidedef-count)
  (bindat-type
    (_ vec sidedef-count struct
       (:x sint 16 t)
       (:y sint 16 t)
       (:upper strz 8)
       (:lower strz 8)
       (:middle strz 8)
       (:sector uint 16 t))))

(defun ewd--vertices-bindat-spec (vertex-count)
  (bindat-type
    (_ vec vertex-count struct
       (:x sint 16 t)
       (:y sint 16 t))))

(defun ewd--segs-bindat-spec (seg-count)
  (bindat-type
    (_ vec seg-count struct
       (:start-vertex uint 16 t)
       (:end-vertex uint 16 t)
       (:angle sint 16 t)
       (:linedef uint 16 t)
       (:direction sint 16 t)
       (:offset sint 16 t))))

(defun ewd--subsectors-bindat-spec (subsector-count)
  (bindat-type
    (_ vec subsector-count struct
       (:seg-count uint 16 t)
       (:first-seg uint 16 t))))

(defun ewd--nodes-bindat-spec (node-count)
  (bindat-type
    (_ vec node-count struct
       (:x sint 16 t)
       (:y sint 16 t)
       (:dx sint 16 t)
       (:dy sint 16 t)
       (:right-bbox vec 4 sint 16 t)
       (:left-bbox vec 4 sint 16 t)
       (:right-child uint 16 t)
       (:left-child uint 16 t))))

(defun ewd--sectors-bindat-spec (sector-count)
  (bindat-type
    (_ vec sector-count struct
       (:floor-height sint 16 t)
       (:ceiling-height sint 16 t)
       (:floor-texture strz 8)
       (:ceiling-texture strz 8)
       (:light sint 16 t)
       (:special sint 16 t)
       (:tag sint 16 t))))

(defun ewd--pnames-bindat-spec (pname-count)
  (bindat-type
    (_ vec pname-count strz 8)))

(defun ewd--texture-offsets-bindat-spec (texture-count)
  (bindat-type
    (_ vec texture-count uint 32 t)))

(defconst ewd--texture-bindat-spec
  (bindat-type
    (:name strz 8)
    (_ fill 4)
    (:width uint 16 t)
    (:height uint 16 t)
    (_ fill 4)
    (:patch-count uint 16 t)))

(defun ewd--patches-bindat-spec (patch-count)
  (bindat-type
    (_ vec patch-count struct
       (:origin-x sint 16 t)
       (:origin-y sint 16 t)
       (:patch-index uint 16 t)
       (_ fill 4))))

(defconst ewd--bindat-specs
  (list (cons :directory-entry ewd--directory-entry-bindat-spec)
	(cons :patch-header ewd--patch-header-bindat-spec)
	(cons :patch-post ewd--patch-post-bindat-spec)
	(cons :colormap ewd--colormap-bindat-spec)
	(cons :palette ewd--palette-bindat-spec)))

(defconst ewd--map-lump-names 
  (list (cons "THINGS" :things)
	(cons "LINEDEFS" :linedefs)
	(cons "SIDEDEFS" :sidedefs)
	(cons "VERTEXES" :vertices)
	(cons "SEGS" :segs)
	(cons "SSECTORS" :subsectors)
	(cons "NODES" :nodes)
	(cons "SECTORS" :sectors)
	(cons "REJECT" :reject)
	(cons "BLOCKMAP" :blockmap)))

(defun ewd-make-directory ()
  (list (cons :maps (make-hash-table :test 'equal))
	(cons :flats (make-hash-table :test 'equal))
	(cons :sprites (make-hash-table :test 'equal))
	(cons :patches (make-hash-table :test 'equal))
	(cons :lumps (make-hash-table :test 'equal))))

(defun ewd-read-directory (file-name)
  (let* ((directory (ewd-make-directory))
         (wad-prefix (ewd--read-bytes file-name 0 12))
	 (wad-header (bindat-unpack ewd--prefix-bindat-spec wad-prefix))
	 (wad-type (bindat-get-field wad-header :wad-type))
	 (entry-count (bindat-get-field wad-header :directory-entry-count))
	 (offset (bindat-get-field wad-header :directory-offset)))
    (cl-assert (or (string= wad-type "IWAD") (string= wad-type "PWAD")) nil
               "Unrecognized wad type %s" wad-type)
    (cl-assert (>= entry-count 0) nil "Directory entry count <0: %d" entry-count)
    (cl-assert (>= offset 0) "Directory offset <0: %d" offset)
    (let ((directory-iterator (ewd--directory-iter file-name offset entry-count))
	  (maps (alist-get :maps directory))
	  (flats (alist-get :flats directory))
	  (sprites (alist-get :sprites directory))
	  (patches (alist-get :patches directory))
	  (lumps (alist-get :lumps directory)))
      (iter-do (entry directory-iterator)
	(let ((name (alist-get :name entry)))
	  (cond
	   ((ewd--map-name-p name)
	    (let ((map (list (cons :name name))))
	      (dolist (name-symbol ewd--map-lump-names)
		(let* ((expected-name (car name-symbol))
		       (symbol (cdr name-symbol))
		       (map-entry (iter-next directory-iterator))
		       (map-entry-name (alist-get :name map-entry)))
		  (cl-assert (string= map-entry-name expected-name) t
		             "Unexpected lump name. Expecting %s, got %s" expected-name map-entry-name)
		  (push (cons symbol map-entry) map)))
	      (puthash name map maps)))
	   ((string= name "F_START")
	    (ewd--read-directory-entries-into-hash-table directory-iterator flats "F_END"))
	   ((string= name "P_START")
	    (ewd--read-directory-entries-into-hash-table directory-iterator patches "P_END"))
	   ((string= name "S_START")
	    (ewd--read-directory-entries-into-hash-table directory-iterator sprites "S_END"))
	   ((string= name "SS_START")
	    (ewd--read-directory-entries-into-hash-table directory-iterator sprites "SS_END"))
	   (t
	    (puthash name entry lumps))))))
    directory))

(cl-defstruct (ewd-node (:constructor ewd--make-node) (:copier nil))
  segment right-bbox left-bbox right-child left-child)

(defconst ewd--subsector-flag #x8000)
(defconst ewd--subsector-mask #x7fff)

(defun ewd--bbox-from-int-vector (v)
  (eldoom-math-make-bbox :top (eldoom-math-fixed-from-int (aref v 0))
                         :bottom (eldoom-math-fixed-from-int (aref v 1))
                         :left (eldoom-math-fixed-from-int (aref v 2))
                         :right (eldoom-math-fixed-from-int (aref v 3))))

(defun ewd--build-node (node-ix nodes subsectors)
  (let* ((raw-node (aref nodes node-ix))
         (x1-fx (eldoom-math-fixed-from-int (alist-get :x raw-node)))
         (y1-fx (eldoom-math-fixed-from-int (alist-get :y raw-node)))
         (dx-fx (eldoom-math-fixed-from-int (alist-get :dx raw-node)))
         (dy-fx (eldoom-math-fixed-from-int (alist-get :dy raw-node)))
         (right-child-ix (alist-get :right-child raw-node))
         (right-child (if (= (logand right-child-ix ewd--subsector-flag) 0)
                          (ewd--build-node right-child-ix nodes subsectors)
                        (aref subsectors (logand right-child-ix ewd--subsector-mask))))
         (left-child-ix (alist-get :left-child raw-node))
         (left-child (if (= (logand left-child-ix ewd--subsector-flag) 0)
                         (ewd--build-node left-child-ix nodes subsectors)
                       (aref subsectors (logand left-child-ix ewd--subsector-mask)))))
    (ewd--make-node
     :segment (eldoom-math-make-segment :x1 x1-fx :y1 y1-fx :x2 (+ x1-fx dx-fx) :y2 (+ y1-fx dy-fx))
     :right-bbox (ewd--bbox-from-int-vector (alist-get :right-bbox raw-node))
     :left-bbox (ewd--bbox-from-int-vector (alist-get :left-bbox raw-node))
     :right-child right-child
     :left-child left-child)))

(cl-defstruct (ewd-vertex (:constructor ewd--make-vertex) (:copier nil))
  x y)

(defun ewd--build-vertices (vertices)
  (cl-map 'vector (lambda (vertex)  
                    (ewd--make-vertex
                     :x (alist-get :x vertex) 
                     :y (alist-get :y vertex)))
          vertices))

(cl-defstruct (ewd-sector (:constructor ewd--make-sector) (:copier nil))
  (index 0 :type fixnum)
  (floor-height-fx 0 :type fixnum)
  (ceiling-height-fx 0 :type fixnum)
  floor-texture
  ceiling-texture
  (light 0 :type fixnum)
  (special 0 :type fixnum)
  (tag 0 :type fixnum)
  linedefs
  (blockbox-top 0 :type fixnum)
  (blockbox-right 0 :type fixnum)
  (blockbox-bottom 0 :type fixnum)
  (blockbox-left 0 :type fixnum)
  (sound-origin-x-fx 0 :type fixnum)
  (sound-origin-y-fx 0 :type fixnum)
  sound-target
  (sound-traversed 0 :type fixnum)
  (special-data nil :documentation "Laziliy initialized data abour door,
crashers, etc...")
  thing ;; first element in a list of things in the sector
  (iteration -1 :type fixnum :documentation "Used to prevent processing
the same sector more than once while iterating through its things")
  reject)

(defun ewd--build-sectors (raw-sectors flats)
  (let ((sectors (make-vector (length raw-sectors) nil)))
    (dotimes (i (length raw-sectors))
      (let ((sector (aref raw-sectors i)))
        (let* ((floor-texture-name (alist-get :floor-texture sector))
               (ceiling-texture-name (alist-get :ceiling-texture sector))
               (floor-texture (if (string-equal floor-texture-name "F_SKY1")
                                  'sky
                                (gethash floor-texture-name flats)))
               (ceiling-texture (if (string-equal ceiling-texture-name "F_SKY1")
                                    'sky
                                  (gethash ceiling-texture-name flats))))
          (cl-assert floor-texture t "Missing floor texture %s" floor-texture-name)
          (cl-assert ceiling-texture t "Missing ceiling texture %s" ceiling-texture-name)
          (aset sectors i
                (ewd--make-sector
                 :index i
                 :floor-height-fx (eldoom-math-fixed-from-int (alist-get :floor-height sector))
                 :ceiling-height-fx (eldoom-math-fixed-from-int (alist-get :ceiling-height sector))
                 :floor-texture floor-texture
                 :ceiling-texture ceiling-texture
                 :light (alist-get :light sector)
                 :special (alist-get :special sector)
                 :tag (alist-get :tag sector))))))
    sectors))

(defun ewd--build-sectors-by-tag (sectors)
  (let ((sectors-by-tag (make-hash-table)))
    (cl-loop for sector across sectors do
             (let* ((tag (ewd-sector-tag sector))
                    (sectors-with-tag (gethash tag sectors-by-tag)))
               (puthash tag (cons sector sectors-with-tag) sectors-by-tag)))
    (maphash (lambda (tag sectors) (puthash tag
                                            (vconcat (nreverse sectors))
                                            sectors-by-tag))
             sectors-by-tag)
    sectors-by-tag))

(cl-defstruct (ewd-sidedef (:constructor ewd--make-sidedef) (:copier nil))
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  upper lower middle
  (texture-offset 0 :type fixnum)
  sector)

(defun ewd--texture-for-name (name textures)
  (when (not (or (string-empty-p name)
                 (string-equal name "-")))
    (let ((texture (gethash (upcase name) textures)))
      (cl-assert texture nil "Unknown texture %s" name)
      texture)))

(defun ewd--build-sidedefs (sidedefs sectors textures)
  (cl-map 'vector (lambda (sidedef)
                    (ewd--make-sidedef
                     :x (alist-get :x sidedef)
                     :y (alist-get :y sidedef)
                     :upper (ewd--texture-for-name (alist-get :upper sidedef) textures)
                     :lower (ewd--texture-for-name (alist-get :lower sidedef) textures)
                     :middle (ewd--texture-for-name (alist-get :middle sidedef) textures)
                     :sector (aref sectors (alist-get :sector sidedef))))
          sidedefs))

(cl-defstruct (ewd-thing (:constructor ewd--make-thing) (:copier nil))
  x-fx y-fx
  angle-deg
  type
  flags)

(defun ewd--build-things (things)
  (cl-map 'vector (lambda (thing)
                    (let* ((type (alist-get :type thing))
                           (angle (alist-get :angle thing)))
                      (ewd--make-thing
                       :x-fx (eldoom-math-fixed-from-int (alist-get :x thing))
                       :y-fx (eldoom-math-fixed-from-int (alist-get :y thing))
                       :angle-deg angle
                       :type type
                       :flags (alist-get :flags thing))))
          things))

(cl-defstruct (ewd-linedef (:constructor ewd--make-linedef) (:copier nil))
  segment
  (flags 0 :type fixnum)
  (special 0 :type fixnum)
  on-use
  on-walk
  on-shoot
  (sector-tag 0 :type fixnum)
  tagged-sectors
  front-sidedef
  back-sidedef
  front-sector
  back-sector
  slope-type
  bbox
  (next-hit nil :documentation "Next linedef hit by current thing move")
  (iteration -1 :type fixnum :documentation "Used to prevent processing the same linedef more than once while tracing"))

(defun ewd--build-linedefs (linedefs vertices sidedefs sectors-by-tag)
  (let ((empty-vector (make-vector 0 nil)))
    (cl-map `vector (lambda (linedef)
                      (let* ((start-vertex (aref vertices (alist-get :start-vertex linedef)))
                             (x1-fx (eldoom-math-fixed-from-int (ewd-vertex-x start-vertex)))
                             (y1-fx (eldoom-math-fixed-from-int (ewd-vertex-y start-vertex)))
                             (end-vertex (aref vertices (alist-get :end-vertex linedef)))
                             (x2-fx (eldoom-math-fixed-from-int (ewd-vertex-x end-vertex)))
                             (y2-fx (eldoom-math-fixed-from-int (ewd-vertex-y end-vertex)))
                             (dx-fx (- x2-fx x1-fx))
                             (dy-fx (- y2-fx y1-fx))
                             (slope-type
                              (cond
                               ((zerop dx-fx) :vertical)
                               ((zerop dy-fx) :horizontal)
                               ((> (eldoom-math-div dy-fx dx-fx) 0) :positive)
                               (t :negative)))
                             (front-sidedef (let ((front-sidedef (alist-get :front-sidedef linedef)))
                                              (when (< front-sidedef #xffff)
                                                (aref sidedefs front-sidedef))))
                             (back-sidedef (let ((back-sidedef (alist-get :back-sidedef linedef)))
                                             (when (< back-sidedef #xffff)
                                               (aref sidedefs back-sidedef))))
                             (front-sector (when front-sidedef (ewd-sidedef-sector front-sidedef)))
                             (back-sector (when back-sidedef (ewd-sidedef-sector back-sidedef)))
                             (tagged-sectors (gethash (alist-get :sector-tag linedef) 
                                                      sectors-by-tag)))
                        (ewd--make-linedef
                         :segment (eldoom-math-make-segment :x1 x1-fx :y1 y1-fx :x2 x2-fx :y2 y2-fx)
                         :slope-type slope-type
                         :bbox (eldoom-math-make-bbox :left (min x1-fx x2-fx)
                                                      :right (max x1-fx x2-fx)
                                                      :bottom (min y1-fx y2-fx)
                                                      :top (max y1-fx y2-fx))
                         :flags (alist-get :flags linedef)
                         :special (alist-get :special linedef)
                         :sector-tag (alist-get :sector-tag linedef)
                         :tagged-sectors (or tagged-sectors empty-vector)
                         :front-sidedef front-sidedef
                         :back-sidedef back-sidedef
                         :front-sector front-sector
                         :back-sector back-sector)))
            linedefs)))

(cl-defstruct (ewd-seg (:constructor ewd--make-seg) (:copier nil))
  segment
  length-fx angle linedef direction offset near-sidedef near-sector far-sidedef far-sector)

(defun ewd--build-segs (segs vertices linedefs)
  (cl-map `vector (lambda (seg)
                    (let* ((direction (alist-get :direction seg))
                           (linedef-index (alist-get :linedef seg))
                           (linedef (aref linedefs linedef-index))
                           (near-sidedef (if (= direction 0)
                                             (ewd-linedef-front-sidedef linedef)
                                           (ewd-linedef-back-sidedef linedef)))
                           (far-sidedef (if (= direction 0)
                                            (ewd-linedef-back-sidedef linedef)
                                          (ewd-linedef-front-sidedef linedef)))
                           (near-sector (when near-sidedef
                                          (ewd-sidedef-sector near-sidedef)))
                           (far-sector (when far-sidedef
                                         (ewd-sidedef-sector far-sidedef)))
                           (start-vertex (aref vertices (alist-get :start-vertex seg)))
                           (x1-fx (eldoom-math-fixed-from-int (ewd-vertex-x start-vertex)))
                           (y1-fx (eldoom-math-fixed-from-int (ewd-vertex-y start-vertex)))
                           (end-vertex (aref vertices (alist-get :end-vertex seg)))
                           (x2-fx (eldoom-math-fixed-from-int (ewd-vertex-x end-vertex)))
                           (y2-fx (eldoom-math-fixed-from-int (ewd-vertex-y end-vertex)))
                           (dx-fx (- x2-fx x1-fx))
                           (dy-fx (- y2-fx y1-fx))
                           (length-fx (eldoom-math-hypot dx-fx dy-fx)))
                      (ewd--make-seg
                       :segment (eldoom-math-make-segment :x1 x1-fx :y1 y1-fx :x2 x2-fx :y2 y2-fx)
                       :length-fx length-fx
                       :angle (alist-get :angle seg)
                       :linedef linedef
                       :direction direction
                       :offset (alist-get :offset seg)
                       :near-sidedef near-sidedef
                       :far-sidedef far-sidedef
                       :near-sector near-sector
                       :far-sector far-sector)))
          segs))

(cl-defstruct (ewd-subsector (:constructor ewd--make-subsector) (:copier nil))
  (segs nil :read-only t)
  (sector nil :read-only t))

(defun ewd--build-subsectors (subsectors segs)
  (cl-map `vector (lambda (subsector)
                    (let* ((seg-count (alist-get :seg-count subsector))
                           (first-seg-ix (alist-get :first-seg subsector))
                           (first-seg (aref segs first-seg-ix))
                           (sector (ewd-seg-near-sector first-seg))
                           (subsector-segs (make-vector seg-count nil)))
                      (dotimes (i seg-count)
                        (aset subsector-segs i (aref segs (+ first-seg-ix i))))
                      (ewd--make-subsector 
                       :segs subsector-segs
                       :sector sector)))
          subsectors))

(defun ewd--fill-sector-linedefs (linedefs sectors)
  (cl-loop for linedef across linedefs do
           (let ((front-sector (ewd-linedef-front-sector linedef))
                 (back-sector (ewd-linedef-back-sector linedef)))
             (when front-sector
               (setf (ewd-sector-linedefs front-sector) (cons linedef (ewd-sector-linedefs front-sector))))
             (when (and back-sector (not (eq front-sector back-sector)))
               (setf (ewd-sector-linedefs back-sector) (cons linedef (ewd-sector-linedefs back-sector))))))
  (cl-loop for sector across sectors do
           (setf (ewd-sector-linedefs sector) (vconcat (ewd-sector-linedefs sector)))))

(defsubst ewd--digit-p (c) (<= ?0 c ?9))

(defun ewd--map-name-p (name)
  (or
   (and (= 5 (length name))
	(string-prefix-p "MAP" name)
	(ewd--digit-p (aref name 3))
	(ewd--digit-p (aref name 4)))
   (and (= 4 (length name))
	(= ?E (aref name 0))
	(= ?M (aref name 2))
	(ewd--digit-p (aref name 1))
	(ewd--digit-p (aref name 3)))))

(defun ewd--read-directory-entry (contents offset)
  (let ((entry (bindat-unpack ewd--directory-entry-bindat-spec contents offset)))
    (let-alist entry
      (cl-assert (>= .:size 0) nil "Lump size <0: %d" .:size)
      (cl-assert (>= .:offset 0) nil "Lump offset <0: %d" .:offset)
      entry)))

(iter-defun ewd--directory-iter (file-name offset entry-count)
  (let ((directory-contents (ewd--read-bytes file-name offset (+ offset (* entry-count 16)))))
    (dotimes (i entry-count)
      (let* ((entry-start (* i 16))
	     (entry-end (+ entry-start 16))
	     (entry (ewd--read-directory-entry directory-contents entry-start)))
        (setf (alist-get :file-name entry) file-name
              (alist-get :index entry) i)
	(iter-yield entry)))
    nil))

(defun ewd--read-directory-entries-into-hash-table (iterator hash-map end-name)
  (let* ((entry (iter-next iterator))
	 (entry-name (alist-get :name entry)))
    (while (not (string= entry-name end-name))
      (puthash entry-name entry hash-map)
      (setq entry (iter-next iterator))
      (setq entry-name (alist-get :name entry)))))

(defsubst ewd--lump-contents (directory-entry file-contents)
  (let-alist directory-entry
    (substring-no-properties file-contents .:offset (+ .:size .:offset))))

(defsubst ewd--read-lump-with-spec (file-contents directory-entry spec)
  (bindat-unpack spec file-contents (alist-get :offset directory-entry)))

(defsubst ewd--read-vector-lump (file-contents entry spec-f record-size)
  (let ((count (/ (alist-get :size entry) record-size)))
    (ewd--read-lump-with-spec file-contents entry (funcall spec-f count))))

(defsubst ewd--read-lump-bindat (file-contents directory-entry type)
  (let ((spec (alist-get type ewd--bindat-specs)))
    (ewd--read-lump-with-spec file-contents directory-entry spec)))

(defsubst ewd--read-things (file-contents entry)
  (ewd--read-vector-lump file-contents entry #'ewd--things-bindat-spec 10))
(defsubst ewd--read-linedefs (file-contents entry)
  (ewd--read-vector-lump file-contents entry #'ewd--linedefs-bindat-spec 14))
(defsubst ewd--read-sidedefs (file-contents entry)
  (ewd--read-vector-lump file-contents entry #'ewd--sidedefs-bindat-spec 30))
(defsubst ewd--read-vertices (file-contents entry)
  (ewd--read-vector-lump file-contents entry #'ewd--vertices-bindat-spec 4))
(defsubst ewd--read-segs (file-contents entry)
  (ewd--read-vector-lump file-contents entry #'ewd--segs-bindat-spec 12))
(defsubst ewd--read-subsectors (file-contents entry)
  (ewd--read-vector-lump file-contents entry #'ewd--subsectors-bindat-spec 4))
(defsubst ewd--read-nodes (file-contents entry)
  (ewd--read-vector-lump file-contents entry #'ewd--nodes-bindat-spec 28))
(defsubst ewd--read-sectors (file-contents entry)
  (ewd--read-vector-lump file-contents entry #'ewd--sectors-bindat-spec 26))
(defun ewd--read-colormaps (file-contents colormap-directory-entry)
  (ewd--read-lump-bindat file-contents colormap-directory-entry :colormap))

(defun ewd--read-palettes (file-contents palette-directory-entry)
  (ewd--read-lump-bindat file-contents palette-directory-entry :palette))

(defconst ewd--s16-spec
  (bindat-type (_ sint 16 t)))

(defun ewd--read-s16 (contents offset)
  (bindat-unpack ewd--s16-spec contents offset))

(defconst ewd--s32-spec
  (bindat-type (_ sint 32 t)))

(defun ewd--read-s32 (contents offset)
  (bindat-unpack ewd--s32-spec contents offset))

(cl-defstruct (ewd-blockmap 
               (:constructor ewd--make-blockmap)
               (:copier nil))
  origin-x-fx
  origin-y-fx
  column-count
  row-count
  lines-blockmap
  mobj-blockmap)

(defun ewd--read-blockmap (file-contents entry linedefs)
  (let* ((lump-offset (alist-get :offset entry))
         (lump-size (alist-get :size entry))
         (header (bindat-unpack ewd--blockmap-header-spec file-contents lump-offset))
         (column-count (alist-get :column-count header))
         (row-count (alist-get :row-count header))
         (offset-count (* column-count row-count))
         (offsets (bindat-unpack (ewd--blockmap-offsets-spec offset-count) file-contents (+ 8 lump-offset)))
         (blockmap (make-vector offset-count nil)))
    (dotimes (i offset-count)
      (let* ((offset (* 2 (aref offsets i)))
             (blockmap-linedefs ())
             (linedef-index (ewd--read-s16 file-contents (+ lump-offset offset))))
        (while (/= linedef-index -1)
          (cl-assert (< offset lump-size) nil "Invalid offset in blockmap, %d >= %d" offset lump-size)
          (cl-assert (< -1 linedef-index (length linedefs)) nil 
                     "Invalid linedef index in blockmap, %d (length %d)" linedef-index (length linedefs))
          (let ((linedef (aref linedefs linedef-index)))
            (setf blockmap-linedefs (cons linedef blockmap-linedefs)
                  offset (+ 2 offset)
                  linedef-index (ewd--read-s16 file-contents ( + lump-offset offset)))))
        (aset blockmap i (vconcat blockmap-linedefs))))
    (ewd--make-blockmap
     :origin-x-fx (eldoom-math-fixed-from-int (alist-get :origin-x header))
     :origin-y-fx (eldoom-math-fixed-from-int (alist-get :origin-y header))
     :column-count column-count
     :row-count row-count
     :lines-blockmap blockmap)))

(defun ewd--fill-sector-rejects (file-contents entry sectors)
  (let* ((data (ewd--lump-contents entry file-contents))
         (sector-count (length sectors))
         (byte-ix 0)
         (bit-ix 0)
         (byte (if (zerop (length data)) 0 (aref data 0))))
    (dotimes (i sector-count)
      (let ((sector-reject (make-bool-vector sector-count nil)))
        (dotimes (j sector-count)
          (aset sector-reject j (/= 0 (logand byte 1)))
          (cl-incf bit-ix)
          (setq byte (ash byte -1))
          (when (= 8 bit-ix)
            (cl-incf byte-ix)
            (setq bit-ix 0
                  byte (if (< byte-ix (length data)) (aref data byte-ix) 0))))
        (setf (ewd-sector-reject (aref sectors i)) sector-reject)))))
         
(defconst ewd-max-radius (eldoom-math-fixed-from-int 32))
(defconst ewd-map-block-shift (- -7 eldoom-math-bits))
(defconst ewd-map-block-to-fixed -7)

(defun ewd--fill-sector-bboxes (sectors blockmap)
  (let ((origin-x (ewd-blockmap-origin-x-fx blockmap))
        (origin-y (ewd-blockmap-origin-y-fx blockmap))
        (max-block-x (- (ewd-blockmap-column-count blockmap) 1))
        (max-block-y (- (ewd-blockmap-row-count blockmap) 1)))
    (cl-loop for sector across sectors do
             (let ((top eldoom-math-int-min)
                   (right eldoom-math-int-min)
                   (bottom eldoom-math-int-max)
                   (left eldoom-math-int-max))
               (cl-loop for linedef across (ewd-sector-linedefs sector) do
                        (let* ((seg (ewd-linedef-segment linedef))
                               (x1 (eldoom-math-segment-x1 seg))
                               (y1 (eldoom-math-segment-y1 seg))
                               (x2 (eldoom-math-segment-x2 seg))
                               (y2 (eldoom-math-segment-y2 seg)))
                          (setq left (min left x1 x2)
                                right (max right x1 x2)
                                bottom (min bottom y1 y2)
                                top (max top y1 y2))))
               (setf (ewd-sector-sound-origin-x-fx sector) (/ (+ right left) 2)
                     (ewd-sector-sound-origin-y-fx sector) (/ (+ top bottom) 2)
                     (ewd-sector-blockbox-top sector)
                     (min (ash (+ (- top origin-y) ewd-max-radius) ewd-map-block-shift) max-block-y)
                     (ewd-sector-blockbox-bottom sector)
                     (max (ash (- bottom origin-y ewd-max-radius) ewd-map-block-shift) 0)
                     (ewd-sector-blockbox-right sector)
                     (min (ash (+ (- right origin-x) ewd-max-radius) ewd-map-block-shift) max-block-x)
                     (ewd-sector-blockbox-left sector)
                     (max (ash (- left origin-x ewd-max-radius) ewd-map-block-shift) 0))))))

(defun ewd--read-pnames (file-contents raw-pnames)
  (let* ((lump-offset (alist-get :offset raw-pnames))
         (count (ewd--read-s32 file-contents lump-offset))
         (pnames (bindat-unpack (ewd--pnames-bindat-spec count) file-contents (+ lump-offset 4))))
    (dotimes (i count)
      (aset pnames i (upcase (aref pnames i))))
    pnames))

(cl-defstruct (ewd-patch-post
               (:constructor ewd--make-patch-post)
               (:copier nil))
  y-offset
  size
  data)

(cl-defstruct (ewd-patch
               (:constructor ewd--make-patch)
               (:copier nil))
  columns
  height
  width
  x-offset
  y-offset)

(defun ewd--mirror-patch (patch)
  (ewd--make-patch
   :columns (reverse (ewd-patch-columns patch))
   :height (ewd-patch-height patch)
   :width (ewd-patch-width patch)
   :x-offset (ewd-patch-x-offset patch)
   :y-offset (ewd-patch-y-offset patch)))

(defun ewd--build-patch-from-entry (file-contents entry)
  (let* ((header-offset (alist-get :offset entry))
         (patch-header (ewd--read-lump-bindat file-contents entry :patch-header))
         (height (bindat-get-field patch-header :height))
         (column-offsets (bindat-get-field patch-header :column-offsets))
         (columns (make-vector (length column-offsets) ()))
         (column-index 0))
    (cl-loop for column-offset across column-offsets do
             (let* ((offset (+ header-offset column-offset))
	            (posts ()))
	       (while (/= (aref file-contents offset) 255)
	         (let ((post (bindat-unpack ewd--patch-post-bindat-spec file-contents offset)))
		   (push (ewd--make-patch-post 
                          :y-offset (alist-get :y-offset post)
                          :size (alist-get :size post)
                          :data (alist-get :data post)) posts)
		   (cl-incf offset (bindat-length ewd--patch-post-bindat-spec post))))
	       (aset columns column-index (vconcat posts))
	       (cl-incf column-index)))
    (ewd--make-patch
     :columns columns
     :height height
     :width (bindat-get-field patch-header :width)
     :x-offset (bindat-get-field patch-header :x-offset)
     :y-offset (bindat-get-field patch-header :y-offset))))

(defun ewd--build-sprites (file-contents sprite-directory)
  (let ((sprites (make-vector (length enf-sprite-names) nil))
        (sprite-indices (make-hash-table :test 'equal))
        (sprite-frame-count (make-vector (length enf-sprite-names) 0)))
    (dotimes (i (length enf-sprite-names))
      (puthash (aref enf-sprite-names i) i sprite-indices))
    ;; First gather all the sprite names (4 letter prefixes) 
    ;; and count of frames of animation for each of them
    (maphash (lambda (name _)
               (let ((l (length name)))
                 (cl-assert (and (>= l 6) (zerop (% l 2))) nil "Invalid sprite name `%s'" name)
                 (let* ((prefix (seq-take name 4))
                        (sprite-index (gethash prefix sprite-indices)))
                   ;; ignoring sprites not defined in the info file (such as pistol pick up sprite)
                   (when sprite-index
                     (cl-loop for i from 4 below l by 2 do
                              (let ((s (- (elt name i) ?A))
                                    (rot (- (elt name (1+ i)) ?1)))
                                (cl-assert (and (>= s 0) (<= -1 rot 7)) nil "Invalid sprite name `%s'" name)
                                (aset sprite-frame-count sprite-index (max (1+ s) (aref sprite-frame-count sprite-index)))))))))
             sprite-directory)
    ;; Then initialize sprite vector to hold all the frames and rotations
    (dotimes (i (length sprite-frame-count))
      (let* ((frame-count (aref sprite-frame-count i))
             (frames (make-vector frame-count nil)))
        (dotimes (i frame-count)
          (aset frames i (make-vector 8 nil)))
        (aset sprites i frames)))
    ;; Fill all the frames and rotations slots with actual sprites
    (maphash (lambda (name raw-sprite)
               (let* ((sprite (ewd--build-patch-from-entry file-contents raw-sprite))
                      (l (length name))
                      (prefix (seq-take name 4))
                      (sprite-index (gethash prefix sprite-indices))
                      (prefix-sprites (when sprite-index (aref sprites sprite-index))))
                 (when sprite-index
                   (let* ((s (- (elt name 4) ?A))
                          (s-sprites (aref prefix-sprites s))
                          (rot (- (elt name 5) ?1)))
                     (if (>= rot 0)
                         (aset s-sprites rot sprite)
                       (fillarray s-sprites sprite)))
                   (when (= l 8)
                     (let* ((s (- (elt name 6) ?A))
                            (s-sprites (aref prefix-sprites s))
                            (rot (- (elt name 7) ?1)))
                       (cl-assert (>= rot 0) nil "Invalid sprite name '%s'" name)
                       (aset s-sprites rot (ewd--mirror-patch sprite)))))))
             sprite-directory)
    sprites))

;; Don't eagerly build patches for all entries in pnames as some undreferenced
;; pnames entries may not have corresponding patch lumps
(cl-defstruct (ewd--patches
               (:constructor ewd--make-patches)
               (:copier nil))
  pnames
  raw-patches
  patches)

(defun ewd--get-patch (file-contents index patches)
  (when (not (ewd--patches-patches patches))
    (setf (ewd--patches-patches patches) 
          (make-vector (length (ewd--patches-pnames patches)) nil)))
  (let ((patch-list (ewd--patches-patches patches)))
    (if (aref patch-list index)
        (aref patch-list index)
      (when-let* ((patch-name (aref (ewd--patches-pnames patches) index))
                  (raw-patch (gethash patch-name (ewd--patches-raw-patches patches)))
                  (patch (ewd--build-patch-from-entry file-contents raw-patch)))
        (aset patch-list index patch)
        patch))))

(cl-defstruct (ewd-texture
               (:constructor ewd--make-texture)
               (:copier nil))
  name
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  columns
  (index 0 :type fixnum)
  animation
  (animation-frame 0 :type fixnum)
  switched)

(defun ewd-make-texture-from-color (color)
  (ewd--make-texture
   :name "fake"
   :width 1
   :height 1
   :columns (make-vector 1 (make-vector 1 color))
   :index -1))

(defun ewd--add-textures-from-lump (file-contents entry patches textures next-index)
  (let* ((lump-offset (alist-get :offset entry))
         (count (ewd--read-s32 file-contents lump-offset))
         (texture-offsets (bindat-unpack (ewd--texture-offsets-bindat-spec count) file-contents (+ 4 lump-offset))))
    (dotimes (i count)
      (let* ((texture-offset (+ lump-offset (aref texture-offsets i)))
             (texture (bindat-unpack ewd--texture-bindat-spec file-contents texture-offset))
             (name (alist-get :name texture))
             (width (alist-get :width texture))
             (height (alist-get :height texture))
             (patch-count (alist-get :patch-count texture))
             (columns (make-vector width nil))
             (patch-refs (bindat-unpack (ewd--patches-bindat-spec patch-count)
                                        file-contents (+ texture-offset 22))))
        (dotimes (x width)
          (aset columns x (make-vector height nil)))
        (dotimes (j patch-count)
          (when-let* ((patch-ref (aref patch-refs j))
                      (origin-x (alist-get :origin-x patch-ref))
                      (origin-y (alist-get :origin-y patch-ref))
                      (patch-index (alist-get :patch-index patch-ref))
                      (patch (ewd--get-patch file-contents patch-index patches))
                      (patch-columns (ewd-patch-columns patch)))
            (dotimes (patch-x (length patch-columns))
              (let ((texture-x (+ origin-x patch-x)))
                (when (and (<= 0 texture-x) (< texture-x width))
                  (seq-doseq (post (aref patch-columns patch-x))
                    (let* ((y-offset (ewd-patch-post-y-offset post))
                           (texture-y (+ origin-y y-offset))
                           (data (ewd-patch-post-data post))
                           (column (aref columns texture-x)))
                      (dotimes (i (length data))
                        (let ((y (+ texture-y i)))
                          (when (and (<= 0 y) (< y height))
                            (aset column y (aref data i))))))))))))
        (puthash name (ewd--make-texture
                       :name name
                       :width width
                       :height height
                       :columns columns
                       :index next-index)
                 textures))
      (cl-incf next-index)))
  next-index)

(defun ewd--build-textures (file-contents lumps patches)
  (let ((textures (make-hash-table :test 'equal))
        (texture1 (gethash "TEXTURE1" lumps))
        (texture2 (gethash "TEXTURE2" lumps))
        (next-index 0))
    (dolist (entry (list texture1 texture2))
      (when entry
        (setq next-index (ewd--add-textures-from-lump file-contents entry patches
                                                      textures next-index))))
    textures))

(defun ewd--find-texture-with-index (textures index)
  (catch :texture 
    (maphash (lambda (_name texture)
               (when (= (ewd-texture-index texture) index)
                 (throw :texture texture)))
             textures)))

(defun ewd--init-texture-animation (textures start-texture-name end-texture-name)
  (when-let* ((start-texture (gethash start-texture-name textures))
              (end-texture (gethash end-texture-name textures))
              (start-texture-index (ewd-texture-index start-texture))
              (end-texture-index (ewd-texture-index end-texture))
              (animation (make-vector (1+ (- end-texture-index start-texture-index)) nil)))
    (cl-loop for i from start-texture-index to end-texture-index do
             (let ((texture (ewd--find-texture-with-index textures i))
                   (animation-frame (- i start-texture-index)))
               (cl-assert texture)
               (aset animation animation-frame texture)
               (cl-assert (not (ewd-texture-animation texture)))
               (setf (ewd-texture-animation texture) animation
                     (ewd-texture-animation-frame texture) animation-frame)))))

(defun ewd--init-texture-animations (textures)
  (ewd--init-texture-animation textures "BLODGR1" "BLODGR4")
  (ewd--init-texture-animation textures "SLADRIP1" "SLADRIP3")
  (ewd--init-texture-animation textures "BLODRIP1" "BLODRIP4")
  (ewd--init-texture-animation textures "FIREWALA" "FIREWALL")
  (ewd--init-texture-animation textures "FIREMAG1" "FIREMAG3")
  (ewd--init-texture-animation textures "FIREBLU1" "FIREBLU2")
  (ewd--init-texture-animation textures "ROCKRED1" "ROCKRED3")
  (ewd--init-texture-animation textures "BFALL1" "BFALL4")
  (ewd--init-texture-animation textures "SFALL1" "SFALL4")
  (ewd--init-texture-animation textures "WFALL1" "WFALL4")
  (ewd--init-texture-animation textures "DBRAIN1" "DBRAIN4"))

(defun ewd--init-switched-texture-pair (textures texture1-name texture2-name)
  (when-let* ((texture1 (gethash texture1-name textures))
              (texture2 (gethash texture2-name textures)))
    (setf (ewd-texture-switched texture1) texture2
          (ewd-texture-switched texture2) texture1)))

(defun ewd--init-switched-textures (textures)
  (ewd--init-switched-texture-pair textures "SW1BRCOM" "SW2BRCOM")
  (ewd--init-switched-texture-pair textures "SW1BRN1" "SW2BRN1")
  (ewd--init-switched-texture-pair textures "SW1BRN2" "SW2BRN2")
  (ewd--init-switched-texture-pair textures "SW1BRNGN" "SW2BRNGN")
  (ewd--init-switched-texture-pair textures "SW1BROWN" "SW2BROWN")
  (ewd--init-switched-texture-pair textures "SW1COMM" "SW2COMM")
  (ewd--init-switched-texture-pair textures "SW1COMP" "SW2COMP")
  (ewd--init-switched-texture-pair textures "SW1DIRT" "SW2DIRT")
  (ewd--init-switched-texture-pair textures "SW1EXIT" "SW2EXIT")
  (ewd--init-switched-texture-pair textures "SW1GRAY" "SW2GRAY")
  (ewd--init-switched-texture-pair textures "SW1GRAY1" "SW2GRAY1")
  (ewd--init-switched-texture-pair textures "SW1METAL" "SW2METAL")
  (ewd--init-switched-texture-pair textures "SW1PIPE" "SW2PIPE")
  (ewd--init-switched-texture-pair textures "SW1SLAD" "SW2SLAD")
  (ewd--init-switched-texture-pair textures "SW1STARG" "SW2STARG")
  (ewd--init-switched-texture-pair textures "SW1STON1" "SW2STON1")
  (ewd--init-switched-texture-pair textures "SW1STON2" "SW2STON2")
  (ewd--init-switched-texture-pair textures "SW1STONE" "SW2STONE")
  (ewd--init-switched-texture-pair textures "SW1STRTN" "SW2STRTN")
  ;; Doom registered episodes 2&3 switches
  (ewd--init-switched-texture-pair textures "SW1BLUE" "SW2BLUE")
  (ewd--init-switched-texture-pair textures "SW1CMT" "SW2CMT")
  (ewd--init-switched-texture-pair textures "SW1GARG" "SW2GARG")
  (ewd--init-switched-texture-pair textures "SW1GSTON" "SW2GSTON")
  (ewd--init-switched-texture-pair textures "SW1HOT" "SW2HOT")
  (ewd--init-switched-texture-pair textures "SW1LION" "SW2LION")
  (ewd--init-switched-texture-pair textures "SW1SATYR" "SW2SATYR")
  (ewd--init-switched-texture-pair textures "SW1SKIN" "SW2SKIN")
  (ewd--init-switched-texture-pair textures "SW1VINE" "SW2VINE")
  (ewd--init-switched-texture-pair textures "SW1WOOD" "SW2WOOD")
  ;; Doom II switches
  (ewd--init-switched-texture-pair textures "SW1PANEL" "SW2PANEL")
  (ewd--init-switched-texture-pair textures "SW1ROCK" "SW2ROCK")
  (ewd--init-switched-texture-pair textures "SW1MET2" "SW2MET2")
  (ewd--init-switched-texture-pair textures "SW1WDMET" "SW2WDMET")
  (ewd--init-switched-texture-pair textures "SW1BRIK" "SW2BRIK")
  (ewd--init-switched-texture-pair textures "SW1MOD1" "SW2MOD1")
  (ewd--init-switched-texture-pair textures "SW1ZIM" "SW2ZIM")
  (ewd--init-switched-texture-pair textures "SW1STON6" "SW2STON6")
  (ewd--init-switched-texture-pair textures "SW1TEK" "SW2TEK")
  (ewd--init-switched-texture-pair textures "SW1MARB" "SW2MARB")
  (ewd--init-switched-texture-pair textures "SW1SKULL" "SW2SKULL"))

(cl-defstruct (ewd-flat (:constructor ewd--make-flat) (:copier nil))
  data
  (index 0 :type fixnum)
  animation
  (animation-frame 0 :type fixnum))

(defun ewd--build-flats (file-contents raw-flats)
  (let ((flats (make-hash-table :test 'equal)))
    (maphash (lambda (name raw-flat)
               (let ((flat (ewd--make-flat
                            :data (ewd--lump-contents raw-flat file-contents)
                            :index (alist-get :index raw-flat))))
                 (puthash name flat flats)))
             raw-flats)
    flats))

(defun ewd--find-flat-with-index (flats index)
  (catch :flat 
    (maphash (lambda (_name flat)
               (when (= (ewd-flat-index flat) index)
                 (throw :flat flat)))
             flats)))

(defun ewd--init-flat-animation (flats start-flat-name end-flat-name)
  (when-let* ((start-flat (gethash start-flat-name flats))
              (end-flat (gethash end-flat-name flats))
              (start-flat-index (ewd-flat-index start-flat))
              (end-flat-index (ewd-flat-index end-flat))
              (animation (make-vector (1+ (- end-flat-index start-flat-index)) nil)))
    (cl-loop for i from start-flat-index to end-flat-index do
             (let ((flat (ewd--find-flat-with-index flats i))
                   (animation-frame (- i start-flat-index)))
               (cl-assert flat)
               (aset animation animation-frame flat)
               (cl-assert (not (ewd-flat-animation flat)))
               (setf (ewd-flat-animation flat) animation
                     (ewd-flat-animation-frame flat) animation-frame)))))

(defun ewd--init-flat-animations (flats)
  (ewd--init-flat-animation flats "NUKAGE1" "NUKAGE3")
  (ewd--init-flat-animation flats "FWATER1" "FWATER4")
  (ewd--init-flat-animation flats "SWATER1" "SWATER4")
  (ewd--init-flat-animation flats "LAVA1" "LAVA4")
  (ewd--init-flat-animation flats "BLOOD1" "BLOOD3")
  (ewd--init-flat-animation flats "RROCK05" "RROCK08")
  (ewd--init-flat-animation flats "SLIME01" "SLIME04")
  (ewd--init-flat-animation flats "SLIME05" "SLIME08")
  (ewd--init-flat-animation flats "SLIME09" "SLIME12"))

(cl-defstruct (ewd-map (:constructor ewd--make-map) (:copier nil))
  root-node
  things
  sky-texture
  blockmap
  colormaps
  palettes
  sectors
  linedefs
  sprites
  sector-count)

(defun ewd-load-map (file-name directory name)
  (let* ((file-contents (ewd--read-bytes file-name))
         (lumps (alist-get :lumps directory))
         (raw-palettes (gethash "PLAYPAL" lumps))
         (palettes (ewd--read-palettes file-contents raw-palettes))
         (raw-colormaps (gethash "COLORMAP" lumps))
         (colormaps (ewd--read-colormaps file-contents raw-colormaps))
         (raw-pnames (gethash "PNAMES" lumps))
         (pnames (ewd--read-pnames file-contents raw-pnames))
         (raw-patches (alist-get :patches directory))
         (patches (ewd--make-patches
                   :pnames pnames
                   :raw-patches raw-patches))
         (maps (alist-get :maps directory))
         (map-entries (gethash name maps))
         (_ (cl-assert map-entries))
	 (raw-sidedefs (ewd--read-sidedefs file-contents (alist-get :sidedefs map-entries)))
         (textures (ewd--build-textures file-contents lumps patches))
         (_ (ewd--init-texture-animations textures))
         (_ (ewd--init-switched-textures textures))
         ;; Sector flats name this F_SKY1; the rendered sky uses a TEXTURE1/2 lump.
         (sky-texture (or (gethash "SKY1" textures)
                          (gethash "SKY2" textures)
                          (gethash "SKY3" textures)
                          (gethash "SKY4" textures)))
         (raw-sprites (alist-get :sprites directory))
         (sprites (ewd--build-sprites file-contents raw-sprites))
	 (raw-sectors (ewd--read-sectors file-contents (alist-get :sectors map-entries)))
         (raw-flats (alist-get :flats directory))
         (flats (ewd--build-flats file-contents raw-flats))
         (_ (ewd--init-flat-animations flats))
         (sectors (ewd--build-sectors raw-sectors flats))
         (sectors-by-tag (ewd--build-sectors-by-tag sectors))
         (sidedefs (ewd--build-sidedefs raw-sidedefs sectors textures))
         (raw-things (ewd--read-things file-contents (alist-get :things map-entries)))
         (things (ewd--build-things raw-things))
	 (raw-vertices (ewd--read-vertices file-contents (alist-get :vertices map-entries)))
         (vertices (ewd--build-vertices raw-vertices))
	 (raw-linedefs (ewd--read-linedefs file-contents (alist-get :linedefs map-entries)))
         (linedefs (ewd--build-linedefs raw-linedefs vertices sidedefs sectors-by-tag))
         (_ (ewd--fill-sector-linedefs linedefs sectors))
	 (raw-segs (ewd--read-segs file-contents (alist-get :segs map-entries)))
         (segs (ewd--build-segs raw-segs vertices linedefs))
	 (raw-subsectors (ewd--read-subsectors file-contents (alist-get :subsectors map-entries)))
         (subsectors (ewd--build-subsectors raw-subsectors segs))
	 (blockmap (ewd--read-blockmap file-contents (alist-get :blockmap map-entries) linedefs))
         (_ (ewd--fill-sector-rejects file-contents (alist-get :reject map-entries)
                                      sectors))
         (_ (ewd--fill-sector-bboxes sectors blockmap))
	 (raw-nodes (ewd--read-nodes file-contents (alist-get :nodes map-entries)))
         (root-node-ix (1- (length raw-nodes)))
         (root-node (ewd--build-node root-node-ix raw-nodes subsectors)))
    (cl-assert sky-texture nil "Missing sky texture")
    (ewd--make-map 
     :root-node root-node
     :things things
     :sky-texture sky-texture
     :palettes palettes
     :colormaps colormaps
     :blockmap blockmap
     :sectors sectors
     :linedefs linedefs
     :sprites sprites
     :sector-count (length sectors))))

(provide 'eldoom-wad)

;; Local Variables:
;; read-symbol-shorthands: (("ewd-" . "eldoom-wad-") ("enf-" . "eldoom-info-"))
;; End:
;;; eldoom-wad.el ends here
