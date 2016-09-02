;;; auto-compile.el --- automatically compile Emacs Lisp libraries

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Package-Requires: ((emacs "24.3") (packed "0.5.3"))
;; Package-Version: 20160711.1012
;; Homepage: https://github.com/tarsius/auto-compile
;; Keywords: compile, convenience, lisp

;;; Commentary:

;; This package automatically recompiles Emacs Lisp source files.

;; Setup
;; -----
;;     (require 'auto-compile)

;;; Code:

;; if automatically compiling files, we must load the newer preferentially
(setq load-prefer-newer t)

(require 'bytecomp)
(require 'cl-lib)
(require 'package-require)
(package-require 'packed)

(defun auto-compile-delete-dest (dest &optional failurep)
  (unless failurep
    (--when-let (get-file-buffer (packed-el-file dest))
      (with-current-buffer it
        (kill-local-variable 'auto-compile-pretend-byte-compiled))))
  (condition-case nil
      (when (file-exists-p dest)
        (message "Deleting %s..." dest)
        (delete-file dest)
        (message "Deleting %s...done" dest))
    (file-error
     (auto-compile-ding)
     (message "Deleting %s...failed" dest))))


;;; Auto-Compile-On-Load Mode

;;;###autoload
(define-minor-mode auto-compile-on-load-mode
  "Before loading a library recompile it if it needs recompilation.

A library needs to be recompiled if the source file is newer than
it's byte-compile destination.  Without this advice the outdated
byte code file would be loaded instead."
  :lighter auto-compile-on-load-mode-lighter
  :group 'auto-compile
  :global t
  (cond (auto-compile-on-load-mode
         (ad-enable-advice  'load    'before 'auto-compile-on-load)
         (ad-enable-advice  'require 'before 'auto-compile-on-load)
         (ad-activate 'load)
         (ad-activate 'require))
        (t
         (ad-disable-advice 'load    'before 'auto-compile-on-load)
         (ad-disable-advice 'require 'before 'auto-compile-on-load))))

(defvar auto-compile-on-load-mode-lighter ""
  "Mode lighter for Auto-Compile-On-Load Mode.")

(defadvice load (before auto-compile-on-load disable)
  ;; (file &optional noerror nomessage nosuffix must-suffix)
  "Before loading the library recompile it if it needs recompilation.
It needs recompilation if it is newer than the byte-compile
destination.  Without this advice the outdated byte-compiled
file would get loaded."
  (auto-compile-on-load file nosuffix))

(defadvice require (before auto-compile-on-load disable)
  ;; (feature &optional FILENAME NOERROR)
  "Before loading the library recompile it if it needs recompilation.
It needs recompilation if it is newer than the byte-compile
destination.  Without this advice the outdated byte-compiled
file would get loaded."
  (unless (featurep feature)
    (auto-compile-on-load (or filename (symbol-name feature)))))

(defvar auto-compile--loading nil)

(defvar auto-compile-always-compile t)

(defun auto-compile-on-load (file &optional nosuffix)
  (unless (member file auto-compile--loading)
    (let ((auto-compile--loading (cons file auto-compile--loading))
          byte-compile-verbose el elc el*)
      (condition-case nil
          (when (setq el (packed-locate-library file nosuffix))
            (setq elc (byte-compile-dest-file el))
              (when (if (file-exists-p elc)
                        (and
                         (file-writable-p elc)
                         (file-newer-than-file-p el elc))
                      (and
                       (equal (substring el -3) ".el")
                       auto-compile-always-compile))
              (message "Recompiling %s..." el)
              (packed-byte-compile-file el)
              (message "Recompiling %s...done" el))
            (when auto-compile-delete-stray-dest
              (setq el* (locate-library file))
              (unless (equal (file-name-directory el)
                             (file-name-directory el*))
                (auto-compile-delete-dest el* t))))
        (error
         (message "Recompiling %s...failed" el)
         (when elc
           (auto-compile-delete-dest elc t)))))))

(provide 'auto-compile)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
