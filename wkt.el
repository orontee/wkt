;;; wkt.el --- Support for Well Known Text format    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Matthias Meulien

;; Author: Matthias Meulien <orontee@gmail.com>
;; Keywords: languages

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

;; http://docs.opengeospatial.org/is/12-063r5/12-063r5.html

;; TODO
;; - Browse ID's
;; - Add geometries
;; - Display keywords in upper case
;; - flymake
;; - pretty-print
;; - Info-lookup support?

;;; Code:

(defgroup wkt nil
  "Major mode for editing files in Well Known Text format"
  :version "27"
  :prefix "wkt-")

(defcustom wkt-indent-level 2
  "Number of spaces for each indentation step."
  :type 'number
  :group 'wkt)

(defvar wkt-keywords
  '("abridgedTransformation" "anchor" "angleUnit" "area" "axis"
    "baseEngCRS" "baseGeodCRS" "baseParamCRS" "baseProjCRS"
    "baseTimeCRS" "baseVertCRS" "bBox" "bearing" "boundCRS"
    "citation" "compoundCRS" "conversion" "coordinateOperation"
    "cs" "datum" "derivingConversion" "eDatum" "ellipsoid" "engCRS"
    "engineeringCRS" "engineeringDatum" "geodCRS" "geodeticCRS"
    "geodeticDatum" "id" "iDatum" "imageCRS" "imageDatum" "interpolationCRS"
    "lengthUnit" "meridian" "method" "operationAccuracy" "order"
    "parameter" "parameterFile" "parametricCRS" "parametricDatum"
    "parametricUnit" "pDatum" "primeM" "primeMeridian" "projCRS"
    "projectedCRS" "projection" "remark" "scaleUnit" "scope" "sourceCRS"
    "spheroid" "targetCRS" "tDatum" "timeCRS" "timeDatum" "timeExtent"
    "timeOrigin" "timeUnit" "unit" "uri" "vDatum" "vertCRS" "verticalCRS"
    "verticalDatum" "verticalExtent"))

(defvar wkt-enumeration-axis-direction
  '("north" "northNorthEast" "northEast" "eastNorthEast" "east"
    "eastSouthEast" "southEast" "southSouthEast" "south" "southWest"
    "westSouthWest" "west" "westNorthWest" "northWest" "northNorthWest"
    "geocentricX" "geocentricY" "geocentricZ" "up" "down" "forward"
    "aft" "port" "starboard" "clockwise" "counterClockwise" "columnPositive"
    "columnNegative" "rowPositive" "rowNegative" "displayRight" "displayLeft"
    "displayUp" "displayDown" "future" "past" "towards" "awayFrom"
    "unspecified"))

(defvar wkt-enumeration-cs-type
  '("affine" "Cartesian" "cylindrical" "ellipsoidal" "linear" "parametric"
    "polar" "spherical" "temporal" "vertical"))

(defvar wkt-enumeration-pixelincell
  '("cellCentre" "cellCenter" "cellCorner"))

(defvar wkt-font-lock-keywords
  `((,(regexp-opt wkt-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt (append wkt-enumeration-axis-direction
			  wkt-enumeration-cs-type)
		  'symbols)
     . font-lock-builtin-face))
  "List of keywards for search-based fontification")

(defvar wkt-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\[ "(" st)
    (modify-syntax-entry ?\] ")" st)
    (modify-syntax-entry ?\( "(" st)
    (modify-syntax-entry ?\) ")" st)
    (modify-syntax-entry ?\, "." st)
    (modify-syntax-entry ?\" "\"" st)
    st)
  "Syntax table used while in `wkt-mode'.")

(defvar wkt-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `wkt-mode'.")

(defun wkt-indent-line ()
  "Indent current line"
  (interactive)
  (let* ((parse-status
	  (save-excursion (syntax-ppss (point-at-bol))))
	 (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (unless (nth 3 parse-status)
      (indent-line-to (* (nth 0 parse-status) wkt-indent-level))
      (when (> offset 0) (forward-char offset)))))

(define-derived-mode wkt-mode
  prog-mode "WKT"
  "Major mode for editing files in Well Known Text format."
  :group 'wkt
  (require 'font-lock)
  (setq-local case-fold-search nil)
  (setq-local comment-start nil)
  (setq-local font-lock-defaults
	      '(wkt-font-lock-keywords nil t))
  (setq-local indent-line-function 'wkt-indent-line)
  (goto-address-mode t)
  (electric-pair-local-mode t))

(add-to-list 'auto-mode-alist '("\\.wkt\\'" . wkt-mode))
(add-to-list 'auto-mode-alist '("\\.prj\\'" . wkt-mode))

(provide 'wkt)
;;; wkt.el ends here
