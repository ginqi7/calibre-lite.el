;;; calibre-lite.el --- Browse and edit a calibre library  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin  <ginqi7@gmail.com>
;; URL: https://github.com/ginqi7/calibre-lite.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (vui "1.4") (vui-hash-table "0.01"))
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

;; A thin asynchronous wrapper around the `calibredb' executable.
;;
;; `calibre-lite-list' fetches the whole library as a list of hash
;; tables, one per book, and caches it in the current buffer.  The
;; other commands shell out to `calibredb' without touching that cache,
;; so call `calibre-lite-list' again to refresh it.
;;
;; The front end lives in calibre-lite-vui.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ansi-color)

(declare-function epub-reader-open "epub-reader" (file))
(declare-function dired-get-marked-files "dired" (&optional localp))

(defgroup calibre-lite nil
  "Browse and edit a calibre library from Emacs."
  :group 'external)

(defcustom calibre-lite-executable (executable-find "calibredb")
  "Path to the `calibredb' executable."
  :type '(choice (const :tag "Not found" nil) file)
  :group 'calibre-lite)

(defcustom calibre-lite-library ""
  "Path to the calibre library to operate on.
The empty string means \"use whichever library calibre itself has
configured\", which is what you want unless you keep several."
  :type 'string
  :group 'calibre-lite)

(defcustom calibre-lite-debug nil
  "When non-nil, keep the output buffers and log every calibredb call.
Useful when a command fails and you want to read its raw output."
  :type 'boolean
  :group 'calibre-lite)

(defcustom calibre-lite-metadata-fields
  '("title" "authors" "series" "tags" "publisher" "comments" "rating" "languages")
  "Metadata fields offered when editing a book.
Run \"calibredb set_metadata --list-fields\" for the full list."
  :type '(repeat string)
  :group 'calibre-lite)

(defcustom calibre-lite-file-openers
  '(("epub" . epub-reader-open))
  "How to open a book file, keyed by lower-case file name extension.
Each value is called with the file name.  Extensions that are not
listed here are handed to the system default application instead, so
PDFs open in Preview or whatever you have registered for them."
  :type '(alist :key-type (string :tag "Extension")
                :value-type (function :tag "Opener"))
  :group 'calibre-lite)

(defcustom calibre-lite-system-opener
  (cond ((eq system-type 'darwin) '("open"))
        ((eq system-type 'windows-nt) '("cmd" "/c" "start"))
        (t '("xdg-open")))
  "Command used to open a book file in the system default application."
  :type '(repeat string)
  :group 'calibre-lite)

(defvar-local calibre-lite--list-data nil
  "Cached output of `calibre-lite-list' for the current buffer.")

;;; Process plumbing

(defun calibre-lite--json-parse (json-str)
  "Parse JSON-STR into a list of hash tables.
Returns nil, after reporting why, when JSON-STR is empty or malformed.
This is called from a process sentinel, where signalling would abort
the callback that asked for the parse, so a bad input is reported
rather than raised.  calibredb exits 0 printing nothing when it has no
library to read, which is the case that matters most in practice."
  (cond
   ((or (null json-str) (string-empty-p (string-trim json-str)))
    (message "calibre-lite: calibredb said nothing; is `calibre-lite-library' set?")
    nil)
   (t
    (condition-case err
        (json-parse-string json-str :array-type 'list)
      (error
       (message "calibre-lite: could not parse calibredb output: %s"
                (error-message-string err))
       nil)))))

(defun calibre-lite--library-args ()
  "Return the \"--with-library\" arguments for `calibre-lite-library'.
Returns nil when no library is configured, so that calibredb falls back
to its own default instead of being handed an empty path."
  (unless (string-empty-p calibre-lite-library)
    (list "--with-library" calibre-lite-library)))

(defun calibre-lite--buffer-string (buffer)
  "Return the contents of BUFFER with ANSI escapes stripped.
Returns nil when BUFFER is not live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (ansi-color-apply-on-region (point-min) (point-max))
      (buffer-string))))

(defun calibre-lite--stderr-string (buffer)
  "Return what calibredb wrote to the stderr BUFFER.
`make-process' wires `:stderr' to a pipe process whose own sentinel
appends a \"Process ... stderr finished\" line, which is bookkeeping
rather than program output and so is stripped here."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (ansi-color-apply-on-region (point-min) (point-max))
      (string-trim
       (replace-regexp-in-string
        "^Process .* stderr finished$" "" (buffer-string))))))

(defun calibre-lite--summarize-error (text)
  "Return a single-line summary of calibredb's stderr TEXT.
calibredb is a Python program, so a crash prints a traceback whose first
line is always the uninformative \"Traceback (most recent call last):\"
and whose last line is the exception that actually happened, so that is
the line worth reporting.  Anything else is reported as its first line,
which is where calibredb puts a plain warning or error."
  (let* ((lines (delq nil (mapcar (lambda (line)
                                    (let ((line (string-trim line)))
                                      (unless (string-empty-p line) line)))
                                  (split-string text "\n"))))
         ;; calibredb prints warnings before the traceback, so look for
         ;; the header anywhere rather than only at the top.
         (crash (seq-some (lambda (line)
                            (string-prefix-p "Traceback (most recent call last):"
                                             line))
                          lines))
         (line (if crash (car (last lines)) (car lines))))
    (if (> (length line) 200)
        (concat (substring line 0 200) "...")
      line)))

(defun calibre-lite--process-sentinel (original-buffer proc _event
                                                     callback err-callback err-buffer)
  "Run CALLBACK or ERR-CALLBACK on PROC's output, then clean up.
ORIGINAL-BUFFER is the buffer the process was started from; both
callbacks run with that buffer current.  ERR-BUFFER is the buffer
`:stderr' was wired to, which has to be passed in because
`(process-get PROC \\='stderr)' does not return it.

The two temporary buffers are killed afterwards, but survive under a
visible name when `calibre-lite-debug' is non-nil or when calibredb
wrote to stderr, because a traceback is far more useful read whole than
summarised down to one line."
  (when (memq (process-status proc) '(exit signal))
    (let* ((out-buffer (process-buffer proc))
           (status (process-exit-status proc))
           (out (calibre-lite--buffer-string out-buffer))
           (err (calibre-lite--stderr-string err-buffer))
           (err-p (and err (not (string-empty-p err)))))
      ;; A leading space keeps the buffer out of the buffer list, which
      ;; is the wrong thing for one that is being kept for reading.
      (when err-p
        (with-current-buffer err-buffer
          (rename-buffer (format "*calibre-lite-%s:err*" (process-name proc)) t)))
      (unwind-protect
          (progn
            (when (and callback (buffer-live-p original-buffer))
              (with-current-buffer original-buffer
                (funcall callback out)))
            (when (and err-p (buffer-live-p original-buffer))
              (with-current-buffer original-buffer
                (if err-callback
                    (funcall err-callback err)
                  ;; Nothing else will surface this, so say something.
                  ;; The exit status sets the tone: calibredb happily
                  ;; prints a traceback and still does what was asked,
                  ;; as when a book is removed but its file is gone.
                  (message "calibre-lite%s: %s (full output in buffer %s)"
                           (if (and (integerp status) (zerop status))
                               " warning" "")
                           (calibre-lite--summarize-error err)
                           (buffer-name err-buffer))))))
        (unless (or calibre-lite-debug err-p)
          (dolist (buffer (list out-buffer err-buffer))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(defun calibre-lite--run (name callback err-callback &rest program-args)
  "Run calibredb with PROGRAM-ARGS, asynchronously.
NAME labels the process and its temporary buffers.  CALLBACK is called
with the process output, ERR-CALLBACK with any output on stderr; both
run in the buffer this was called from.  When ERR-CALLBACK is nil the
error is reported with `message' instead of being discarded."
  (unless calibre-lite-executable
    (user-error "Calibredb not found; customize `calibre-lite-executable'"))
  (let ((original-buffer (current-buffer))
        ;; Leading space keeps these out of the buffer list.
        (out-buffer (generate-new-buffer (format " *calibre-lite-%s*" name)))
        (err-buffer (generate-new-buffer (format " *calibre-lite-%s:err*" name))))
    (when calibre-lite-debug
      (message "calibre-lite %s: %S" name (cons calibre-lite-executable program-args)))
    (condition-case err
        (make-process
         :name name
         :buffer out-buffer
         :stderr err-buffer
         :command (cons calibre-lite-executable program-args)
         :noquery t
         :sentinel (lambda (proc event)
                     (calibre-lite--process-sentinel
                      original-buffer proc event callback err-callback err-buffer)))
      ;; The process never started, so its buffers would leak.
      (error
       (kill-buffer out-buffer)
       (kill-buffer err-buffer)
       (signal (car err) (cdr err))))))

;;; Library commands

(defun calibre-lite-list (&optional callback)
  "Fetch the whole library and call CALLBACK with it.
The result is a list of hash tables, one per book, keyed by the field
names calibredb reports (\"id\", \"title\", \"formats\", ...).  It is
also cached in `calibre-lite--list-data', whether or not CALLBACK is
given."
  (apply #'calibre-lite--run
         "list"
         (lambda (json)
           (setq-local calibre-lite--list-data (calibre-lite--json-parse json))
           (when callback
             (funcall callback calibre-lite--list-data)))
         nil
         (append (calibre-lite--library-args)
                 '("list" "--for-machine" "-f" "all"))))

(defun calibre-lite-get (id callback)
  "Call CALLBACK with the book record whose id is ID.
The library is fetched first when it is not already cached in the
current buffer."
  (if calibre-lite--list-data
      (let ((book (cl-find id calibre-lite--list-data
                           :key (apply-partially #'gethash "id")
                           :test #'=)))
        (if book
            (funcall callback book)
          (message "calibre-lite: no book with id %s" id)))
    (calibre-lite-list (lambda (_books) (calibre-lite-get id callback)))))

(defun calibre-lite--book-file (book)
  "Return the file to open for BOOK, a calibredb record.
The \"formats\" field lists one absolute file name per format in no
particular order, so prefer one that `calibre-lite-file-openers' knows
how to handle and only fall back to the first otherwise."
  (let ((files (gethash "formats" book)))
    (or (cl-find-if (lambda (file)
                      (assoc-string (file-name-extension file)
                                    calibre-lite-file-openers t))
                    files)
        (car files))))

(defun calibre-lite--open-with-opener (file)
  "Open FILE with the opener registered for its extension.
See `calibre-lite-file-openers'; when nothing is registered for the
extension, hand the file to `calibre-lite-system-opener'."
  (let* ((extension (file-name-extension file))
         (opener (and extension
                      (cdr (assoc-string extension calibre-lite-file-openers t)))))
    (cond
     ((null opener)
      (apply #'start-process "calibre-lite-open" nil
             (append calibre-lite-system-opener (list file))))
     ((fboundp opener)
      (funcall opener file))
     (t
      (user-error "calibre-lite: %s is not a known opener (see `calibre-lite-file-openers')"
                  opener)))))

(defun calibre-lite-open-file (id)
  "Open the book file of the book with id ID.
The file goes to the opener registered for its extension in
`calibre-lite-file-openers', or to `calibre-lite-system-opener' when
the extension is not registered."
  (calibre-lite-get
   id
   (lambda (book)
     (let ((file (calibre-lite--book-file book)))
       (if file
           (calibre-lite--open-with-opener file)
         (message "calibre-lite: book %s has no book file" id))))))

(defun calibre-lite-add (files &optional callback)
  "Add FILES to the calibre library.
FILES is a path or a list of paths, each a book file or a directory to
scan.  In Dired, the marked files are used.
CALLBACK, when given, is called with calibredb's output."
  (interactive
   (list (if (derived-mode-p 'dired-mode)
             (dired-get-marked-files)
           (list (read-file-name "Add to calibre library: ")))))
  (apply #'calibre-lite--run
         "add"
         callback
         nil
         (append (calibre-lite--library-args)
                 '("add" "--")
                 (if (consp files) files (list files)))))

(defun calibre-lite-remove (ids &optional callback)
  "Remove the books IDS from the calibre library.
IDS is an id or a list of ids.  The books go to calibre's trash rather
than being deleted outright.
CALLBACK, when given, is called with calibredb's output."
  (apply #'calibre-lite--run
         "remove"
         callback
         nil
         (append (calibre-lite--library-args)
                 (list "remove"
                       (mapconcat (lambda (id) (format "%s" id))
                                  (if (consp ids) ids (list ids))
                                  ",")))))

(defun calibre-lite-set-metadata (id field value &optional callback)
  "Set FIELD of the book ID to VALUE.
FIELD is a calibre field name such as \"title\", \"tags\" or
\"series\"; VALUE is the new value as a string."
  (apply #'calibre-lite--run
         "set-metadata"
         callback
         nil
         (append (calibre-lite--library-args)
                 (list "set_metadata"
                       (format "%s" id)
                       "-f" (format "%s:%s" field value)))))

(defun calibre-lite-read-metadata ()
  "Prompt for a metadata field and a new value.
Returns a cons of (FIELD . VALUE)."
  (let ((field (completing-read "Field: " calibre-lite-metadata-fields nil t)))
    (cons field (read-string (format "New %s: " field)))))

(defun calibre-lite-edit-metadata (id &optional callback)
  "Prompt for a metadata field and value, then apply them to book ID.
CALLBACK is passed through to `calibre-lite-set-metadata'."
  (interactive "nBook id: ")
  (let ((change (calibre-lite-read-metadata)))
    (calibre-lite-set-metadata id (car change) (cdr change) callback)))

(provide 'calibre-lite)
;;; calibre-lite.el ends here
