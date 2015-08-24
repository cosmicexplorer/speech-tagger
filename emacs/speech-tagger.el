(require 'json)

;;; TODO: add package prefix to all symbols

(defvar this-file-dir
  (if load-file-name (file-name-directory load-file-name) default-directory))
(defvar pos-json-file "penn_treebank_tags.json")
(defvar pos-json-path (concat this-file-dir pos-json-file))
(defvar *pos-hash* (let ((json-object-type 'hash-table))
                     (json-read-file pos-json-path)))

;;; TODO: make a defcustom for the location of the jar; if none provided,
;;; download the jar and save it to the directory of this file
(defvar jar-dir "/home/cosmicexplorer/projects/active/speech-tagger/target/")
(defvar jar-name "speech-tagger-0.0.0-SNAPSHOT-standalone.jar")
(defvar jar-path (concat jar-dir jar-name))

(defvar *tag-proc* nil)
(defconst +tag-proc-name+ "speech-tagger")
(defconst +tag-proc-buf-name+ "*speech-tagger*")
(defun start-tag-process ()
  (setq *tag-proc*
        (start-process +tag-proc-name+ +tag-proc-buf-name+ "java"
                       "-jar" jar-path)))

(defvar *job-id-counter* 0)
(defvar *speech-tag-jobs* (make-hash-table))

(defun lock-region (beg end)
  ;; TODO: add some other face to indicate text is being analyzed? make a
  ;; defcustom so this can be turned off
  (put-text-property beg end 'read-only t))

(defun unlock-region (beg end)
  "Inverse of `lock-region'."
  (put-text-property beg end 'read-only nil))

(defun make-region-log (beg end buf)
  (goto-char beg)
  (let ((beg-mark (point-marker)))
    (set-marker-insertion-type beg-mark t)
    (goto-char end)
    (let ((end-mark (point-marker)))
      (set-marker-insertion-type end-mark t)
      (list :beg beg-mark :end end-mark :buffer buf))))

(defun lock-region-and-log (beg end id)
  ;; lock from editing
  (lock-region beg end)
  (puthash id (make-region-log beg end (current-buffer)) *speech-tag-jobs*))

(defun make-tag-proc-json (beg end id)
  (list :job-id id
        :string (buffer-substring-no-properties beg end)))

(defun search-for-whitespace (direction)
  (let ((space-regex "[[:space:]\n]"))
    (cond ((eq direction 'backward)
           (unless (string-match-p space-regex (make-string 1 (char-before)))
             (re-search-backward space-regex)
             (forward-char)))
          ((eq direction 'forward)
           (unless (string-match-p space-regex (make-string 1 (char-after)))
             (re-search-forward space-regex)
             (backward-char)))
          (t (throw 'bad-search-direction
                    "whitespace search direction not recognized")))))

(defun widen-region-to-word-bounds (beg end)
  (goto-char beg)
  (search-for-whitespace 'backward)
  (let ((new-beg (point)))
    (goto-char end)
    (search-for-whitespace 'forward)
    (list beg end)))

(defun send-region-to-tag-proc (beg end proc &optional buf)
  (with-current-buffer (or buf (current-buffer))
    (let* ((id (incf *job-id-counter*))
           (bounds (widen-region-to-word-bounds beg end))
           (new-beg (first bounds))
           (new-end (second bounds)))
      (lock-region-and-log new-beg new-end id)
      (process-send-string
       proc
       (concat (json-encode (make-tag-proc-json new-beg new-end id)) "\n")))))

(defvar *tag-proc-cur-line* "")

;;; TODO: write `mark-parts-of-speech' and make interface to access
(defun process-tag-proc-json (plist)
  "Takes a json message PLIST from the external process and uses it to highlight
text in the region marked by the job-id key of PLIST. Pops the job-id off of
`*speech-tag-jobs*'"
  (let* ((job-id (plist-get plist :job-id))
         (tagged-string (plist-get plist :tagged-string))
         (reg-log (gethash job-id *speech-tag-jobs*)))
    (if (not reg-log)
        (throw 'no-such-job
               (format "%s %d %s" "no job with id" job-id "found"))
      (let ((beg (plist-get reg-log :beg))
            (end (plist-get reg-log :end))
            (buf (plist-get reg-log :buffer)))
        (with-current-buffer buf
          (mark-parts-of-speech beg end buf tagged-string)
          (remhash job-id *speech-tag-jobs*)
          (unlock-region beg end))))))

;;; all json is received as a single line of text, making stream parsing easier
(defun receive-tag-proc-string (str)
  (let ((newline-match (string-match-p "\n" str)))
    ;; in case json message is larger than emacs's process output buffer
    ;; (unlikely if we don't send in massive strings to tagging process)
    (if (not newline-match)
        (setq *tag-proc-cur-line* (concat *tag-proc-cur-line* str))
      (let* ((msg (concat *tag-proc-cur-line* (substring str 0 newline-match)))
             (json-msg (json-read-from-string msg)))
        (process-tag-proc-json json-msg)
        (setq *tag-proc-cur-line* "")
        (receive-tag-proc-string (substring str (1+ newline-match)))))))
