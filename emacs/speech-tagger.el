;;; speech-tagger.el --- tag parts of speech using coreNLP

;; Copyright 2015 Danny McClanahan

;; Author: Danny McClanahan <danieldmcclanahan@gmail.com>
;; Version: 2015.08.27
;; Package-Requires: ((cl-lib "0.5"))
;; Package-Version: 0.0.0
;; Keywords: speech, tag, nlp, language, corenlp, parsing, natural
;; URL: https://github.com/cosmicexplorer/speech-tagger

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The interactive functions exported by this extension follow a common
;; protocol: if a region is active, then modify the region; otherwise modify
;; the entire buffer. If a prefix argument is provided, they read in a buffer
;; to modify the entirety of. A given region will be expanded to whitespace
;; boundaries (so if region is around the l characters in he|ll|o, the entirety
;; of |hello| will be selected).

;; Requires a "java" binary on the PATH. Downloads a mildly large jar file
;; (20.7M), which causes a large pause on the first usage, but none
;; afterwards. You can customize `speech-tagger-jar-path' to determine where it
;; looks for the presence of the jar. You can also download the jar manually
;; from `https://cosmicexplorer.github.io/speech-tagger/speech-tagger.jar'.

;;; Usage:

;; M-x `speech-tagger-tag-dwim'
;; - Tag parts of speech in the appropriate region
;; - "dwim" is an abbreviation for "do what I mean;" hopefully what I the
;; developer mean is close enough to what you the user mean.
;; - Tagging, as shown in the image above, colors a part of speech and adds a
;; tooltip to it so that if you mouse over or move point over the part of
;; speech, you get a description of the part of speech and example of that part
;; of speech.

;; M-x `speech-tagger-clear-tags-dwim'
;; - As above, but clears the region of all such tags.

;; ESC-: (`speech-tagger-clear-state')
;; - Useful in the case that something screws up and you wish to debug.
;; - Should revert all lisp code back to the same as when first loaded.
;; - Does NOT delete the jar file, since the file takes an annoyingly long time
;; to download.

;;; Code:

(require 'json)
(require 'cl-lib)

(defgroup speech-tagger-faces nil "Faces for speech-tag extension."
  :group 'processes)

;; tag descriptions json path
(defvar speech-tagger-this-file-dir
  (if load-file-name (file-name-directory load-file-name) default-directory))
(defvar speech-tagger-pos-json-file "penn_treebank_tags.json")
(defvar speech-tagger-pos-json-path
  (concat speech-tagger-this-file-dir speech-tagger-pos-json-file))
(defvar speech-tagger-*pos-hash* nil)

;; part-of-speech face specs
(defconst speech-tagger-+macro-charset+
  '(96 39 34 40 41 59 35 91 93)
  "` ' \" ( ) ; # [ ]")
(defun speech-tagger-uniquify-list (l)
  "Return new list with only unique elements of L, in same order."
  (cl-loop for el in l
           with new-list = nil
           do (unless (cl-find el new-list) (push el new-list))
           finally (return (reverse new-list))))

;; assert macro charset's uniqueness
(unless (equal speech-tagger-+macro-charset+
               (speech-tagger-uniquify-list speech-tagger-+macro-charset+))
  (throw 'speech-tagger-invalid-charset "macro charset is non-unique"))

(defconst speech-tagger-+macro-regex+
  (regexp-opt-charset speech-tagger-+macro-charset+))
(defun speech-tagger-find-free-char (ch charset)
  "Find a character usable to represent CH which isn't a member of CHARSET."
  (cl-loop while (cl-find ch charset)
           do (cl-incf ch)
           finally (return ch)))
(defconst speech-tagger-+macro-charset-escapes+
  (let ((tbl (make-hash-table :test #'equal)))
    (cl-loop
     for ch in speech-tagger-+macro-charset+
     do (puthash
         (make-string 1 ch)             ; convert to string for extensibility
         (make-string
          1
          (speech-tagger-find-free-char ch speech-tagger-+macro-charset+))
         tbl)
     finally (return tbl))))
(defun speech-tagger-escape-macro-characters (str)
  "Replace characters not allowed to represent Lisp symbols in STR."
  (replace-regexp-in-string
   speech-tagger-+macro-regex+
   (lambda (ch-str) (gethash ch-str speech-tagger-+macro-charset-escapes+))
   str))

(defun speech-tagger-hash-pos-for-color (pos-str)
  "Pseudo-random color based on `md5' hash for POS-STR."
  (format "#%x" (string-to-number (substring (md5 pos-str) 0 6) 16)))

(defun speech-tagger-destructure-json-table (entry face)
  "Transform json in ENTRY with given FACE for a part of speech into plist."
  (let ((desc (aref entry 0))
        (examples (aref entry 1)))
    (list :description desc :examples examples :face face)))
(defun speech-tagger-get-json-table (path)
  "Construct hash table of parts of speech from .json file at PATH."
  (let ((tbl (let ((json-object-type 'hash-table))
               (json-read-file path))))
    (maphash
     (lambda (key val)
       (puthash
        key
        (speech-tagger-destructure-json-table
         val
         (custom-declare-face
          (intern (concat "speech-tagger-"
                          (speech-tagger-escape-macro-characters key)))
          `((default (:foreground ,(speech-tagger-hash-pos-for-color key))))
          ;; first of the value stored in the hash is description of pos
          (aref val 0)
          :group 'speech-tagger-faces))
        tbl))
     tbl)
    tbl))
(defun speech-tagger-refresh-table ()
  "Set `speech-tagger-*pos-hash*' to hash table created from .json file."
  (setq speech-tagger-*pos-hash*
        (speech-tagger-get-json-table speech-tagger-pos-json-path)))

(defconst speech-tagger-jar-name "speech-tagger.jar")
(defconst speech-tagger-jar-path
  (concat speech-tagger-this-file-dir speech-tagger-jar-name)
  "Path to speech-tagger.jar required to run the part-of-speech tagging.")

(defvar speech-tagger-*tag-proc* nil)
(defconst speech-tagger-+tag-proc-name+ "speech-tagger")
(defconst speech-tagger-+tag-proc-buf-name+ "*speech-tagger*")

(defvar speech-tagger-*job-id-counter* 0)
(defvar speech-tagger-*jobs* nil)

(defface speech-tagger-loading-text
  `((default (:background "#005540")))
  "Face used for loading text being analyzed."
  :group 'speech-tagger-faces)
(defconst speech-tagger-+loading-text-msg+
  "Loading parts of speech from process...")
(defun speech-tagger-lock-region (beg end)
  "Lock region between BEG and END from editing.
Apply face `speech-tagger-loading-text'."
  (put-text-property beg end 'read-only t)
  (let ((olay (make-overlay beg end nil t)))
    (overlay-put olay 'face 'speech-tagger-loading-text)
    (overlay-put olay 'help-echo speech-tagger-+loading-text-msg+)
    (overlay-put olay 'speech-tagger-point-hover
                 speech-tagger-+loading-text-msg+)))

(defun speech-tagger-unlock-region (beg end)
  "Inverse `speech-tagger-lock-region' for region between BEG and END."
  (let ((inhibit-read-only t))
    (put-text-property beg end 'read-only nil)
    (remove-overlays beg end 'face 'speech-tagger-loading-text)))

(defun speech-tagger-make-region-log (beg end buf)
  "Create job entry for part-of-speech tagging between BEG and END in BUF."
  (goto-char beg)
  (let ((beg-mark (point-marker)))
    (set-marker-insertion-type beg-mark t)
    (goto-char end)
    (let ((end-mark (point-marker)))
      (set-marker-insertion-type end-mark t)
      (list :beg beg-mark :end end-mark :buffer buf
            :text (with-current-buffer buf (buffer-substring beg end))))))

(defun speech-tagger-lock-region-and-log (beg end id)
  "Lock region between BEG and END from editing.
Insert job with key ID into `speech-tagger-*jobs*'."
  (speech-tagger-lock-region beg end)
  (puthash id (speech-tagger-make-region-log beg end (current-buffer))
           speech-tagger-*jobs*))

(defun speech-tagger-make-tag-proc-json (beg end id)
  "Construct json to send to process for BEG to END with given ID."
  (list :job-id id
        :string (buffer-substring-no-properties beg end)))

(defun speech-tagger-search-for-whitespace (direction)
  "Move point to frontier of whitespace in given DIRECTION."
  (let ((space-regex "[[:space:]\r\n]"))
    (cond ((eq direction 'backward)
           (unless (let ((ch (char-before)))
                     (and ch (string-match-p space-regex (make-string 1 ch))))
             (when (re-search-backward space-regex nil t) (forward-char))))
          ((eq direction 'forward)
           (unless (let ((ch (char-after)))
                     (and ch (string-match-p space-regex (make-string 1 ch))))
             (when (re-search-forward space-regex nil t) (backward-char))))
          (t (throw 'speech-tagger-bad-search-direction
                    "whitespace search direction not recognized")))))

(defun speech-tagger-widen-region-to-word-bounds (beg end)
  "Widen region between BEG and END with `speech-tagger-search-for-whitespace'."
  (goto-char beg)
  (speech-tagger-search-for-whitespace 'backward)
  (let ((new-beg (point)))
    (goto-char end)
    (speech-tagger-search-for-whitespace 'forward)
    (list beg end)))

(defun speech-tagger-get-job-id ()
  "Create new global job id for better concurrent commmunication with process."
  (cl-loop with first-id = (1- speech-tagger-*job-id-counter*)
           while (gethash speech-tagger-*job-id-counter* speech-tagger-*jobs*)
           do (if (= speech-tagger-*job-id-counter* first-id)
                  ;; should never happen unless rest of code is awful
                  (throw 'no-available-jobs "no free job ids found")
                (cl-incf speech-tagger-*job-id-counter*))
           finally (return speech-tagger-*job-id-counter*)))

(defvar speech-tagger-*tag-proc-cur-line* "")

(defun speech-tagger-mark-parts-of-speech (beg tagged-string)
  "Mark parts of speech between BEG and END according to tags in TAGGED-STRING."
  (cl-loop
   for tagged-section across tagged-string
   do (let ((offset (plist-get tagged-section :start))
            (final (plist-get tagged-section :end))
            (text (plist-get tagged-section :text))
            (tag (plist-get tagged-section :tag))
            (inhibit-read-only t))
        (let* ((beg-ind (+ beg offset))
               (end-ind (+ beg final))
               (new-txt (buffer-substring beg-ind end-ind)))
          (unless (equal text new-txt)
            (throw 'speech-tagger-different-text
                   (format "%s \"%s\" %s \"%s\"" "previous text" text
                           "is different than current text" new-txt)))
          (let* ((olay (make-overlay beg-ind end-ind nil t))
                 (tag-hash (gethash tag speech-tagger-*pos-hash*))
                 (help-info
                  (format "%s: e.g %s"
                          (propertize (plist-get tag-hash :description)
                                      'face 'font-lock-keyword-face)
                          (plist-get tag-hash :examples))))
            (overlay-put olay 'face (plist-get tag-hash :face))
            (overlay-put olay 'speech-tagger t)
            (overlay-put olay 'help-echo help-info)
            (overlay-put olay 'speech-tagger-point-hover help-info)
            (overlay-put olay 'mouse-face 'mode-line-highlight))))))

(defun speech-tagger-process-tag-proc-json (plist)
  "Take json message PLIST from the external process.
Use PLIST to highlight text in region marked by the job-id key of PLIST.  Pops
job-id off of `speech-tagger-*jobs*'"
  (let* ((job-id (plist-get plist :job-id))
         (tagged-string (plist-get plist :tagged-string))
         (reg-log (gethash job-id speech-tagger-*jobs*)))
    (if (not reg-log)
        (throw 'speech-tagger-no-such-job
               (format "%s %d %s" "no job with id" job-id "found"))
      (cl-destructuring-bind (:beg beg :end end :buffer buf :text text) reg-log
        (with-current-buffer buf
          (let ((cur-text (buffer-substring beg end)))
            (unless (equal text cur-text)
              (throw 'speech-tagger-different-text
                     (format "%s \"%s\" %s \"%s\"" "previous text" text
                             "is different than current text" cur-text))))
          (speech-tagger-mark-parts-of-speech beg tagged-string)
          (remhash job-id speech-tagger-*jobs*)
          (speech-tagger-unlock-region beg end))))))

;; all json is received as a single line of text, making stream parsing easier
(defun speech-tagger-receive-tag-proc-string (str)
  "Receive string STR from process filter and line-buffer.
Send line-buffered json string to `speech-tagger-process-tag-proc-json'."
  (let ((newline-match (string-match-p "\n" str)))
    ;; in case json message is larger than emacs's process output buffer
    ;; (unlikely if we don't send in massive strings to tagging process)
    (if (not newline-match)
        (setq speech-tagger-*tag-proc-cur-line*
              (concat speech-tagger-*tag-proc-cur-line* str))
      (let* ((msg (concat speech-tagger-*tag-proc-cur-line*
                          (substring str 0 newline-match)))
             (json-msg (let ((json-object-type 'plist))
                         (json-read-from-string msg))))
        (speech-tagger-process-tag-proc-json json-msg)
        (setq speech-tagger-*tag-proc-cur-line* "")
        (speech-tagger-receive-tag-proc-string
         (substring str (1+ newline-match)))))))

(defun speech-tagger-message-process-buffer (proc msg)
  "Send PROC the string MSG while also inserting into PROC's process buffer."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (when (eolp) (insert "\n"))
    (insert msg)))

(defun speech-tagger-start-tag-process ()
  "Create part-of-speech tagging process."
  (setq
   speech-tagger-*tag-proc*
   (let ((new-proc
          (start-process
           speech-tagger-+tag-proc-name+ speech-tagger-+tag-proc-buf-name+
           "java" "-jar" speech-tagger-jar-path)))
     (set-process-filter
      new-proc (lambda (proc msg)
                 (speech-tagger-message-process-buffer proc msg)
                 (speech-tagger-receive-tag-proc-string msg)))
     (set-process-sentinel
      new-proc (lambda (proc msg)
                 (speech-tagger-message-process-buffer proc msg)
                 (message "%s %s %s"
                          (process-name proc) "exited with message" msg)))
     new-proc)))

(defun speech-tagger-post-command-fn ()
  "Run after interactive commands to highlight part of speech in minibuffer."
  (let ((p (get-char-property (point) 'speech-tagger-point-hover)))
    (when p (message "%s" p))))

(defun speech-tagger-send-region-to-tag-proc (beg end proc)
  "Send region between BEG and END to PROC.
Apply widening with `speech-tagger-widen-region-to-word-bounds'."
  (let* ((id (speech-tagger-get-job-id))
         (bounds (speech-tagger-widen-region-to-word-bounds beg end))
         (new-beg (cl-first bounds))
         (new-end (cl-second bounds)))
    (speech-tagger-lock-region-and-log new-beg new-end id)
    (process-send-string
     proc
     (concat (json-encode
              (speech-tagger-make-tag-proc-json new-beg new-end id))
             "\n"))))

(defun speech-tagger-clear-state ()
  "Utility function used to reset Lisp code to initial state."
  (when speech-tagger-*jobs* (clrhash speech-tagger-*jobs*))
  (when speech-tagger-*pos-hash* (clrhash speech-tagger-*pos-hash*))
  (setq speech-tagger-*job-id-counter* 0
        speech-tagger-*tag-proc* nil
        speech-tagger-*tag-proc-cur-line* ""
        speech-tagger-*jobs* nil
        speech-tagger-*pos-hash* nil)
  (mapcar
   (lambda (proc)
     (when (equal (buffer-name (process-buffer proc))
                  speech-tagger-+tag-proc-buf-name+)
       (delete-process proc)))
   (process-list))
  (when (bufferp speech-tagger-+tag-proc-buf-name+)
    (kill-buffer speech-tagger-+tag-proc-buf-name+)))

(defun speech-tagger-clear-overlays (&optional beg end)
  "Clear overlays from `speech-tagger-mark-parts-of-speech' between BEG, END."
  (let ((b (or beg (point-min))) (e (or end (point-max))))
    (let ((inhibit-read-only t))
      (put-text-property b e 'read-only nil)
      (remove-overlays b e 'speech-tagger t))))

;;;###autoload
(defun speech-tagger-clear-tags-dwim (pfx)
  "Clear tag overlays from highlighted region or buffer.
If PFX given, read buffer name to clear tags from."
  (interactive "P")
  (if (not pfx)
      (if (use-region-p)
          (speech-tagger-clear-overlays (region-beginning) (region-end))
        (speech-tagger-clear-overlays))
    (let ((bufname (read-buffer "buffer to clear tags from: " nil t)))
      (with-current-buffer bufname (speech-tagger-clear-overlays)))))

(defun speech-tagger-setup ()
  "Initialize globals as required.
Must be re-run after using `speech-tagger-clear-state'."
  (unless speech-tagger-*pos-hash* (speech-tagger-refresh-table))
  (unless speech-tagger-*jobs* (setq speech-tagger-*jobs* (make-hash-table)))
  ;; pretty harmless to add this, even if permanent, since it won't affect other
  ;; overlays unless they use the 'speech-tagger-point-hover property
  (add-hook 'post-command-hook #'speech-tagger-post-command-fn)
  (unless (process-live-p (get-process speech-tagger-+tag-proc-name+))
    (speech-tagger-start-tag-process)))

;;;###autoload
(defun speech-tagger-tag-dwim (pfx)
  "Create tag overlays in selected region or buffer for parts of speech.
Send selected region to external process for analysis.  Call
`speech-tagger-setup' as required.  If PFX given, read buffer name to tag.  Be
warned that this function may take some time on large selections or buffers."
  (interactive "P")
  (speech-tagger-setup)
  (if (not pfx)
      (if (use-region-p)
          (let ((wide-range (speech-tagger-widen-region-to-word-bounds
                              (region-beginning) (region-end))))
            (speech-tagger-clear-overlays
             (cl-first wide-range) (cl-second wide-range))
            (speech-tagger-send-region-to-tag-proc
             (region-beginning) (region-end) speech-tagger-*tag-proc*))
        (speech-tagger-clear-overlays (point-min) (point-max))
        (speech-tagger-send-region-to-tag-proc
         (point-min) (point-max) speech-tagger-*tag-proc*))
    (let ((bufname (read-buffer "buffer to tag: " nil t)))
      (with-current-buffer bufname
        (speech-tagger-clear-overlays (point-min) (point-max))
        (speech-tagger-send-region-to-tag-proc
         (point-min) (point-max) speech-tagger-*tag-proc*)))))

(provide 'speech-tagger)

;;; speech-tagger.el ends here
