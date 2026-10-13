;;; calibre-lite-vui.el --- Table front end for calibre-lite  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin  <ginqi7@gmail.com>
;; Keywords: books, hypermedia

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

;; Renders `calibre-lite-list' as a sortable, selectable table using
;; the `vui' and `vui-hash-table' packages.
;;
;; SPC toggles the checkbox of the row at point, RET opens the action
;; menu for the checked rows.

;;; Code:

(require 'vui)
(require 'vui-hash-table)
(require 'transient)

;; `vui-hash-table-defcomponent' builds a component that calls
;; `vui-muted', which lives in vui-components.el.  vui-hash-table only
;; requires `vui', so pull the module in here or the table fails to
;; render with a void-function error.
(require 'vui-components)

(require 'calibre-lite)

(defgroup calibre-lite-vui nil
  "Table front end for `calibre-lite'."
  :group 'calibre-lite)

;;; Formatters

(defun calibre-lite-vui--book-format (formats)
  "Return the file extensions of FORMATS as a comma separated string.
FORMATS is the list of absolute file names calibredb reports for a
book, so only the extension is interesting here."
  (mapconcat (lambda (file) (upcase (or (file-name-extension file) "")))
             formats ","))

(defun calibre-lite-vui--book-tags (tags)
  "Return TAGS joined into a single comma separated string."
  (string-join tags ","))

;;; Custom variables

(defcustom calibre-lite-vui-headers
  '(;; The first "id" entry renders the selection checkbox, the second
    ;; the id number itself.  Both read the same hash key, so the
    ;; duplicate is deliberate -- `vui-hash-table' has no way to give a
    ;; column its own key.
    ("id" . (:header "" :formatter vui-hash-table-format-select))
    ("id" . (:header "ID" :width-ratio 0.1 :truncate t
             :formatter number-to-string :sort-func <))
    ("title" . (:header "TITLE" :width-ratio 0.4 :truncate t))
    ("formats" . (:header "FORMAT" :width-ratio 0.1 :truncate t
                  :formatter calibre-lite-vui--book-format))
    ("authors" . (:header "AUTHORS" :width-ratio 0.1 :truncate t))
    ("tags" . (:header "TAGS" :width-ratio 0.1 :truncate t
               :formatter calibre-lite-vui--book-tags)))
  "Columns of the book table, as accepted by `vui-hash-table'."
  :type '(alist :key-type string :value-type (plist))
  :group 'calibre-lite-vui)

;;; Selected-row context

(defvar calibre-lite-vui--context nil
  "Cons of (BUFFER . SELECTED-KEYS) captured when the action menu opened.
BUFFER is the table the selection came from; SELECTED-KEYS the book ids
that were checked.")

(defun calibre-lite-vui--selected-keys ()
  "Return the book ids selected when the action menu was opened."
  (cdr calibre-lite-vui--context))

(defun calibre-lite-vui--in-source-buffer (function)
  "Call FUNCTION with the table buffer current, if it is still alive.
The transient runs its commands from its own buffer, and the book list
is cached buffer-locally in the table, so anything that looks a book up
has to run from there or it re-fetches the whole library."
  (let ((buffer (car calibre-lite-vui--context)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (funcall function)))))

(defun calibre-lite-vui--finish ()
  "Clear the selection in the table the action menu was opened from.
Re-renders so the checkboxes stop displaying as checked."
  (let ((buffer (car calibre-lite-vui--context)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (vui-hash-table-clear-selected)
        ;; `vui-refresh' would insert a `g' at point if point happens to
        ;; sit in a widget field, hence the move to the top first.
        (save-excursion
          (goto-char (point-min))
          (vui-refresh))))))

;;; Actions

(defun calibre-lite-vui-open ()
  "Open every selected book.
A book that cannot be opened is reported and the rest still open."
  (interactive)
  (let ((keys (calibre-lite-vui--selected-keys)))
    (calibre-lite-vui--finish)
    (calibre-lite-vui--in-source-buffer
     (lambda ()
       (dolist (id keys)
         (condition-case err
             (calibre-lite-open-file id)
           (error (message "calibre-lite: could not open book %s: %s"
                           id (error-message-string err)))))))))

(defun calibre-lite-vui-edit-metadata ()
  "Prompt once for a metadata change and apply it to every selected book.
The selection is only cleared once something was actually typed, so
that aborting the prompt leaves it alone."
  (interactive)
  (let ((keys (calibre-lite-vui--selected-keys)))
    (when keys
      (let ((change (calibre-lite-read-metadata)))
        (calibre-lite-vui--finish)
        (mapc (lambda (id)
                (calibre-lite-set-metadata id (car change) (cdr change)))
              keys)))))

(defun calibre-lite-vui-remove ()
  "Remove the selected books from the calibre library, after confirming."
  (interactive)
  (let ((keys (calibre-lite-vui--selected-keys)))
    (when (and keys
               (yes-or-no-p (format "Remove %d book(s) from the library? "
                                    (length keys))))
      (calibre-lite-vui--finish)
      (calibre-lite-remove keys))))

(defun calibre-lite-vui-ret (selected-keys)
  "Open the action menu for the table rows in SELECTED-KEYS."
  (setq calibre-lite-vui--context (cons (current-buffer) selected-keys))
  (calibre-lite-vui-prefix))

;;; Transient menu

;; Defined at load time rather than on each RET so that the prefix is
;; available to `transient' (and to `M-x') straight away.
(transient-define-prefix calibre-lite-vui-prefix ()
  "Actions on the selected books."
  [:description
   (lambda ()
     (format "Selected books: %s"
             (propertize (format "%s" (calibre-lite-vui--selected-keys))
                         'face 'highlight)))
   ("RET" "Open" calibre-lite-vui-open)
   ("e" "Edit metadata" calibre-lite-vui-edit-metadata)
   ("d" "Remove from library" calibre-lite-vui-remove)])

;;; The command

(vui-hash-table-defcomponent
 :name 'calibre-lite-vui
 :border :unicode
 :sticky-header t)

;;;###autoload
(defun calibre-lite-vui ()
  "Display the calibre library as a sortable, selectable table."
  (interactive)
  ;; The list callback runs with the table buffer current, which is what
  ;; caches `calibre-lite--list-data' there for `calibre-lite-get' to
  ;; reuse when a book is opened.
  (with-current-buffer (get-buffer-create "*calibre-lite-vui*")
    (pop-to-buffer (current-buffer))
    (calibre-lite-list
     (lambda (data)
       ;; Unlock around the mount so that re-running the command works.
       (setq buffer-read-only nil)
       (vui-hash-table-mount
        'calibre-lite-vui
        "*calibre-lite-vui*"
        #'calibre-lite-vui-ret
        :headers calibre-lite-vui-headers
        :table data)
       (setq buffer-read-only t)
       ;; Mounting turns on `vui-mode', and a major mode kills every
       ;; buffer-local, so the copy `calibre-lite-list' cached has just
       ;; been thrown away.  Put it back, or `calibre-lite-get' misses
       ;; the cache and re-fetches the whole library on every open.
       (setq-local calibre-lite--list-data data)))))

(provide 'calibre-lite-vui)
;;; calibre-lite-vui.el ends here
