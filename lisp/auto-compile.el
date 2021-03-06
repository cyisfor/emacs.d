;;; auto-compile.el --- automatically compile Emacs Lisp libraries

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; heavily modded by cy to just do what it's supposed to do.
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

(defcustom auto-compile-always-compile t
  "Should we compile files for which no .elc exists, but a .el file exists?

Skips some obviously bad to compile names..."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-delete-stray-dest t
  "Whether to remove stray byte code files.

If this is non-nil byte code files without a corresponding source
file are removed as they are encountered.  This happens in the
functions `auto-compile-on-save' and `toggle-auto-compile'.  The
main purpose of this functionality is to prevent leftover byte
code files from shadowing a source or byte code file in a
directory that comes later in the `load-path'."
  :group 'auto-compile
  :type 'boolean)

;; if automatically compiling files, we must load the newer preferentially
(setq load-prefer-newer t)

(require 'bytecomp)
(require 'cl-lib)
(require 'package-require)
(package-require 'packed)

(defun auto-compile-delete-dest (dest &optional failurep)
  (condition-case nil
      (when (file-exists-p dest)
        (message "Deleting %s..." dest)
        (delete-file dest))
    (file-error
     (ding)
     (message "Deleting %s...failed" dest))))

(defadvice load (before auto-compile-on-load activate preactivate compile)
  ;; (file &optional noerror nomessage nosuffix must-suffix)
  "Before loading the library recompile it if it needs recompilation.
It needs recompilation if it is newer than the byte-compile
destination.  Without this advice the outdated byte-compiled
file would get loaded."
  (auto-compile-on-load file nosuffix))

(defadvice require (before auto-compile-on-load activate preactivate compile)
  ;; (feature &optional FILENAME NOERROR)
  "Before loading the library recompile it if it needs recompilation.
It needs recompilation if it is newer than the byte-compile
destination.  Without this advice the outdated byte-compiled
file would get loaded."
  (unless (featurep feature)
    (auto-compile-on-load (or filename (symbol-name feature)))))

(defvar auto-compile--loading nil)

(defun auto-compile-on-load (file &optional nosuffix)
  (unless (member file auto-compile--loading)
    (let ((auto-compile--loading (cons file auto-compile--loading))
          byte-compile-verbose el elc el*)
      (when (setq el (packed-locate-library file nosuffix))
        (setq elc (byte-compile-dest-file el))
        (when (if (file-exists-p elc)
                  (and
                   (file-writable-p elc)
                   (file-newer-than-file-p el elc))
                (and
                 auto-compile-always-compile
                 (file-writable-p elc)
                 (equal (substring el -3) ".el")))
          (message "um %s %s %s %s" elc
                   (file-exists-p elc)
                   (file-writable-p elc)
                   (file-newer-than-file-p el elc))
          (message "Recompiling %s..." el)
          (condition-case nil
              (progn
                (message "Recompiling %s..." el)
                (packed-byte-compile-file el))
            (error
             (message "Recompiling %s...failed" el)
             (when elc
               (auto-compile-delete-dest elc t))))
          (when auto-compile-delete-stray-dest
            (setq el* (locate-library file))
            (unless (equal (file-name-directory el)
                           (file-name-directory el*))
              (auto-compile-delete-dest el* t))))))))

(provide 'auto-compile)

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
