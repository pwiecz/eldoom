;;; eldoom-config.el --- Configuration variables for Emacs Doom  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Piotr Wieczorek

;; Author: Piotr Wieczorek <piotr@gmail.com>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defconst eldoom-fov (* float-pi 0.5))
(defconst eldoom-canvas-width 320)
(defconst eldoom-canvas-height 200)
(defconst eldoom-pixel-ratio 1.2)

(provide 'eldoom-config)
;;; eldoom-config.el ends here
