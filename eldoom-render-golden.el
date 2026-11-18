;;; eldoom-render-golden.el --- Render regression golden-file harness -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1

;;; Commentary:
;;
;; Compare BSP renderer output against checked-in golden images.
;;
;; Golden files are raw RGB byte strings (320x200x3) stored under
;; `test/render-golden/`.  Test cases are defined in
;; `erg--render-cases'; add entries there or call
;; `erg-generate-golden' for a one-off viewpoint.
;;
;; Typical workflow:
;;
;;   M-x erg-generate-all-goldens RET   ; refresh goldens after intentional change
;;   M-x erg-check-all-goldens RET      ; compare current renderer output
;;   emacs --batch -L . -l eldoom-render-golden.el -f erg-check-all-goldens-batch
;;
;;; Code:

(require 'cl-lib)

(require 'eldoom-canvas)
(require 'eldoom-config)
(require 'eldoom-math)
(require 'eldoom-render)
(require 'eldoom-wad)

(defgroup eldoom-render-golden nil
  "Golden-file regression tests for the BSP renderer."
  :group 'eldoom)

(defcustom erg-wad-path nil
  "WAD file used for render golden tests.
When nil, `erg--find-wad-path' searches `erg-wad-search-paths'."
  :type '(choice (const :tag "Search default paths" nil)
                 file)
  :group 'eldoom-render-golden)

(defcustom erg-wad-search-paths
  '("/usr/share/games/doom/freedoom2.wad"
    "~/wad/freedoom2.wad"
    "c:/wad/freedoom2.wad")
  "Candidate WAD paths tried by `erg--find-wad-path'."
  :type '(repeat file)
  :group 'eldoom-render-golden)

(defconst erg--golden-subdir "test/render-golden"
  "Directory under the repo root holding golden RGB binaries.")

(defconst erg--bytes-per-frame
  (* eldoom-canvas-width eldoom-canvas-height 3)
  "Size of one golden frame in bytes.")

;; Each case is a plist:
;;   :name     string, used as the golden filename stem
;;   :map      map lump name, e.g. \"MAP01\"
;;   :x :y     view position in world units (float or integer)
;;   :angle    view angle in radians
;;   :z        optional view height; nil uses floor + 41
(defconst erg--render-cases
  '((:name "map01-start"
           :map "MAP01"
           :from-player-start t)
    (:name "map01-turn-left"
           :map "MAP01"
           :from-player-start t
           :angle-offset 1.5707963267948966)
    (:name "map05-start"
           :map "MAP05"
           :from-player-start t)
    (:name "map11-start"
           :map "MAP11"
           :from-player-start t)
    (:name "map20-start"
           :map "MAP20"
           :from-player-start t))
  "Render regression viewpoints.
Add entries here, then run `erg-generate-all-goldens'.
Use `:from-player-start t' to take x/y/angle from the map player 1 start.
Optional `:angle-offset' adds radians to the player start angle.
Alternatively specify `:x-fx', `:y-fx', and `:angle-bam' explicitly.")

(defvar erg--repo-root nil
  "Cached repo root containing `erg--golden-subdir'.")

(defvar erg--wad-cache nil
  "WAD path string when `erg--ensure-wad-loaded' has run.")

(defvar erg--wad-directory nil
  "Shared WAD directory object for loading maps.")

(defvar erg--map-cache (make-hash-table :test 'equal)
  "Map name string -> built map object.")

(defconst erg--repo-marker "eldoom-render.el"
  "File that must exist in the repo root.")

(defun erg--repo-root ()
  (or erg--repo-root
      (setq erg--repo-root
            (let ((dir (expand-file-name default-directory)))
              (while (and dir (not (file-readable-p (expand-file-name erg--repo-marker dir))))
                (setq dir (file-name-directory (directory-file-name dir))))
              (or dir (error "Could not find %s; run from the repo root" erg--repo-marker))))))

(defun erg--golden-dir ()
  (expand-file-name erg--golden-subdir (erg--repo-root)))

(defun erg--golden-path (name)
  (expand-file-name (concat name ".rgb") (erg--golden-dir)))

(defun erg--find-wad-path ()
  (or (and erg-wad-path (progn
                          (unless (file-readable-p erg-wad-path)
                            (error "WAD not readable: %s" erg-wad-path))
                          erg-wad-path))
      (cl-loop for path in erg-wad-search-paths
               for expanded = (expand-file-name path)
               when (file-readable-p expanded)
               return expanded)
      (error "No WAD found; set `erg-wad-path' or install freedoom2.wad")))

(defun erg-wad-available-p ()
  "Return non-nil when a usable WAD file can be found."
  (condition-case nil
      (progn (erg--find-wad-path) t)
    (error nil)))

(defun erg--ensure-wad-loaded ()
  (unless erg--wad-cache
    (let ((wad (erg--find-wad-path)))
      (setq erg--wad-directory (ewd-read-directory wad)
            erg--wad-cache wad))))

(defun erg--load-map (map-name)
  (erg--ensure-wad-loaded)
  (or (gethash map-name erg--map-cache)
      (let ((map (ewd-load-map erg-wad-path erg--wad-directory map-name)))
        (message "loading map %s" map-name)
        ;;        (eldoom-render-init-thing-by-subsector map)
        (puthash map-name map erg--map-cache)
        map)))

(defun erg--player-start (map-name)
  (let ((map (erg--load-map map-name)))
    (or (cl-find-if (lambda (thing)
                      (= (eldoom-wad-thing-type thing) 1))
                    (eldoom-wad-map-things map))
        (error "No player 1 start in %s" map-name))))

(defun erg--resolve-case (case)
  "Return CASE plist with :x :y :angle filled in."
  (if (plist-get case :from-player-start)
      (let* ((map-name (plist-get case :map))
             (player (erg--player-start map-name))
             (angle-deg (ewd-thing-angle-deg player))
             (offset (eldoom-math-radians-to-bam (or (plist-get case :angle-offset) 0))))
        (list :name (plist-get case :name)
              :map map-name
              :x-fx (eldoom-wad-thing-x-fx player)
              :y-fx (eldoom-wad-thing-y-fx player)
              :angle-bam (eldoom-math-ang (+ (eldoom-math-degrees-to-bam angle-deg)
                                             (eldoom-math-radians-to-bam offset)))
              :z-fx (plist-get case :z-fx)))
    (unless (and (plist-member case :x-fx)
                 (plist-member case :y-fx)
                 (plist-member case :angle-bam))
      (error "Case %s needs :x-fx :y-fx :angle-bam or :from-player-start"
             (plist-get case :name)))
    case))

(defun erg--extract-rgb (canvas)
  (let* ((bitmap (eldoom-canvas-bitmap canvas))
         (offset (eldoom-canvas-data-offset canvas))
         (len erg--bytes-per-frame)
         (buf (make-string len 0)))
    (dotimes (i len)
      (aset buf i (aref bitmap (+ offset i))))
    buf))

(defun erg--render-case (case)
  "Render CASE and return its raw RGB bytes as a unibyte string."
  (let* ((resolved (erg--resolve-case case))
         (map-name (plist-get resolved :map))
         (map (erg--load-map map-name))
         (scene (eldoom-render-scene-for-map map))
         (canvas (eldoom-create-canvas eldoom-canvas-width eldoom-canvas-height)))
    (eldoom-render-render-level map scene canvas
                                   (plist-get resolved :x-fx)
                                   (plist-get resolved :y-fx)
                                   (plist-get resolved :angle-bam)
                                   (plist-get resolved :z-fx)
                                   nil)
    (erg--extract-rgb canvas)))

(defun erg--read-golden (name)
  (let ((path (erg--golden-path name)))
    (unless (file-readable-p path)
      (error "Missing golden file: %s" path))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents path)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun erg--write-golden (name rgb)
  (let ((dir (erg--golden-dir)))
    (make-directory dir t)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert rgb)
      (write-region (point-min) (point-max) (erg--golden-path name) nil 'silent))))

(defun erg-compare-rgb (expected actual)
  "Compare two RGB byte strings.
Return plist :identical-p, :diff-bytes, :diff-pixels, :total-pixels."
  (unless (= (length expected) (length actual))
    (error "Length mismatch: expected %d, got %d"
           (length expected) (length actual)))
  (let ((diff-bytes 0)
        (diff-pixels (make-hash-table)))
    (dotimes (i (length expected))
      (unless (= (aref expected i) (aref actual i))
        (cl-incf diff-bytes)
        (puthash (/ i 3) t diff-pixels)))
    (list :identical-p (= diff-bytes 0)
          :diff-bytes diff-bytes
          :diff-pixels (hash-table-count diff-pixels)
          :total-pixels (/ (length expected) 3))))

(defun erg-generate-golden (case)
  "Render CASE and write its golden RGB file.
CASE is a plist from `erg--render-cases', or a case name string
when interactive."
  (interactive
   (let* ((name (completing-read "Golden case: "
                                 (mapcar (lambda (c) (plist-get c :name))
                                         erg--render-cases)
                                 nil t))
          (found (cl-find name erg--render-cases
                          :key (lambda (c) (plist-get c :name))
                          :test #'string=)))
     (list (or found (error "Unknown case: %s" name)))))
  (when (stringp case)
    (setq case (cl-find case erg--render-cases
                        :key (lambda (c) (plist-get c :name))
                        :test #'string=)))
  (let* ((resolved (erg--resolve-case case))
         (name (plist-get resolved :name))
         (rgb (erg--render-case case)))
    (unless (= (length rgb) erg--bytes-per-frame)
      (error "Unexpected render size: %d" (length rgb)))
    (erg--write-golden name rgb)
    (message "Wrote golden %s (%d bytes)" (erg--golden-path name) (length rgb))
    name))

(defun erg-generate-view-golden (name map x-fx y-fx angle-bam &optional z-fx)
  "Render a one-off viewpoint and write golden NAME.rgb.
Use this to add new cases before committing them to `erg--render-cases'."
  (interactive "sGolden name: \nsMap: \nX: \nY: \nAngle (radians): \nZ (optional): ")
  (let ((case (list :name name :map map :x-fx x-fx :y-fx y-fx :angle-bam angle-bam :z-fx z-fx)))
    (erg-generate-golden case)))

(defun erg-generate-all-goldens (&optional cases)
  "Generate golden files for all CASES (default `erg--render-cases')."
  (interactive)
  (dolist (case (or cases erg--render-cases))
    (erg-generate-golden case))
  (message "Generated %d golden files in %s"
           (length (or cases erg--render-cases))
           (erg--golden-dir)))

(defun erg-check-case (case)
  "Render CASE and compare against its golden file.
Return plist :name :identical-p :diff-bytes :diff-pixels :total-pixels."
  (let* ((resolved (erg--resolve-case case))
         (name (plist-get resolved :name))
         (expected (erg--read-golden name))
         (actual (erg--render-case case))
         (result (erg-compare-rgb expected actual)))
    (append (list :name name) result)))

(defun erg-check-all-goldens (&optional cases)
  "Compare all CASES against golden files; return a list of result plists."
  (interactive)
  (let ((results (mapcar #'erg-check-case (or cases erg--render-cases)))
        (failures 0))
    (dolist (result results)
      (let ((name (plist-get result :name))
            (pixels (plist-get result :diff-pixels))
            (total (plist-get result :total-pixels)))
        (if (plist-get result :identical-p)
            (message "OK   %s" name)
          (cl-incf failures)
          (message "FAIL %s: %d/%d pixels differ (%d bytes)"
                   name pixels total (plist-get result :diff-bytes)))))
    (message "Render golden check: %d/%d passed, %d failed"
             (- (length results) failures) (length results) failures)
    results))

(defun erg-generate-all-goldens-batch ()
  "Non-interactive wrapper for `erg-generate-all-goldens'."
  (erg-generate-all-goldens))

(defun erg-check-all-goldens-batch ()
  "Non-interactive check; exit with code 1 if any golden differs."
  (let ((results (erg-check-all-goldens)))
    (unless (cl-every (lambda (r) (plist-get r :identical-p)) results)
      (kill-emacs 1))))

(provide 'eldoom-render-golden)

;; Local Variables:
;; read-symbol-shorthands: (("ewd-" . "eldoom-wad-"))
;; End:
;;; eldoom-render-golden.el ends here
