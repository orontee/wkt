;;; wkt-mode.el --- Support for Well Known Text format    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Matthias Meulien

;; Author: Matthias Meulien <orontee@gmail.com>
;; Keywords: languages
;; Version: 0.1
;; URL: https://github.com/orontee/wkt-mode

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

;; Emacs major mode to edit files in Well Known Text format

;; The mode provides:
;; - Syntax higlighting
;; - Line indentation
;; - Command to beautify buffer content

;; Files with .prj or .wkt extensions have their major mode defined to
;; be `wkt-mode'.

;; To beautify the buffer content, either enter the key sequence "C-c
;; C-f" or call the command `wkt-beautify': It adds new lines before
;; keywords preceded by a comma then reindent. This command applies to
;; the active region or whole buffer.

;;; Code:

(defgroup wkt-mode nil
  "Major mode for editing files in Well Known Text format"
  :version "27"
  :prefix "wkt-")

(defcustom wkt-mode-indent-level 2
  "Number of spaces for each indentation step."
  :type 'number
  :group 'wkt)

(defvar wkt-mode-keywords
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

(defvar wkt-mode-enumeration-axis-direction
  '("north" "northNorthEast" "northEast" "eastNorthEast" "east"
    "eastSouthEast" "southEast" "southSouthEast" "south" "southWest"
    "westSouthWest" "west" "westNorthWest" "northWest" "northNorthWest"
    "geocentricX" "geocentricY" "geocentricZ" "up" "down" "forward"
    "aft" "port" "starboard" "clockwise" "counterClockwise" "columnPositive"
    "columnNegative" "rowPositive" "rowNegative" "displayRight" "displayLeft"
    "displayUp" "displayDown" "future" "past" "towards" "awayFrom"
    "unspecified"))

(defvar wkt-mode-enumeration-cs-type
  '("affine" "Cartesian" "cylindrical" "ellipsoidal" "linear" "parametric"
    "polar" "spherical" "temporal" "vertical"))

(defvar wkt-mode-enumeration-pixelincell
  '("cellCentre" "cellCenter" "cellCorner"))

(defvar wkt-mode-font-lock-keywords
  `((,(regexp-opt wkt-mode-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt (append wkt-mode-enumeration-axis-direction
			  wkt-mode-enumeration-cs-type)
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

(defun wkt-mode--reformat-region (begin end)
  (interactive "*r")
  (let ((start-line (line-number-at-pos begin))
	(case-fold-search t)
	(regex (regexp-opt wkt-mode-keywords ",[\s-]*\\(" t)))
    (save-excursion
      (save-restriction
	(narrow-to-region begin end)
	(goto-char (point-min))
	(while (re-search-forward regex nil t)
	  (replace-match ",\n\\1" t nil))
	(indent-region (point-min) (point-max))))))

;;;###autoload
(defun wkt-beautify ()
  "Beautify the active region or entire buffer if no active
region."
  (interactive)
  (if (use-region-p)
      (wkt-mode--reformat-region (region-beginning) (region-end))
    (wkt-mode--reformat-region (buffer-end -1) (buffer-end 1))))

(defun wkt-mode-indent-line ()
  "Indent current line"
  (interactive)
  (let* ((parse-status
	  (save-excursion (syntax-ppss (point-at-bol))))
	 (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (unless (nth 3 parse-status)
      (indent-line-to (* (nth 0 parse-status) wkt-mode-indent-level))
      (when (> offset 0) (forward-char offset)))))

;;;###autoload
(define-derived-mode wkt-mode
  prog-mode "WKT"
  "Major mode for editing files in Well Known Text format."
  :group 'wkt
  (require 'font-lock)
  (setq-local case-fold-search t)
  (setq-local comment-start nil)
  (setq-local font-lock-defaults
	      '(wkt-mode-font-lock-keywords nil t))
  (setq-local indent-line-function 'wkt-mode-indent-line)
  (define-key wkt-mode-map (kbd "C-c C-f") 'wkt-beautify)
  (goto-address-mode t)
  (electric-pair-local-mode t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wkt\\'" . wkt-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.prj\\'" . wkt-mode))

(provide 'wkt-mode)
;;; wkt-mode.el ends here
