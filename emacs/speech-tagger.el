(require 'json)
(require 'cl-lib)

(defgroup speech-tagger/faces nil "Faces for speech-tag extension.")

;;; tag descs json path
(defvar speech-tagger/this-file-dir
  (if load-file-name (file-name-directory load-file-name) default-directory))
(defvar speech-tagger/pos-json-file "penn_treebank_tags.json")
(defvar speech-tagger/pos-json-path
  (concat speech-tagger/this-file-dir speech-tagger/pos-json-file))
(defvar speech-tagger/*pos-hash* nil)

;;; pos face specs
(defconst speech-tagger/+macro-charset+
  '(96 39 34 40 41 59 35 91 93)
  "` ' \" ( ) ; # [ ]")
(defun speech-tagger/uniquify-list (l)
  (cl-loop for el in l
           with new-list = nil
           do (unless (cl-find el new-list) (push el new-list))
           finally (return (reverse new-list))))
(unless (equal speech-tagger/+macro-charset+
               (speech-tagger/uniquify-list speech-tagger/+macro-charset+))
  (throw 'speech-tagger/invalid-charset "macro charset is non-unique"))

(defconst speech-tagger/+macro-regex+
  (regexp-opt-charset speech-tagger/+macro-charset+))
(defun speech-tagger/find-free-char (ch charset)
  (cl-loop while (cl-find ch charset)
           do (incf ch)
           finally (return ch)))
(defconst speech-tagger/+macro-charset-escapes+
  (let ((tbl (make-hash-table :test #'equal)))
    (cl-loop
     for ch in speech-tagger/+macro-charset+
     do (puthash
         (make-string 1 ch)             ; convert to string for extensibility
         (make-string
          1
          (speech-tagger/find-free-char ch speech-tagger/+macro-charset+))
         tbl)
     finally (return tbl))))
(defun speech-tagger/escape-macro-characters (str)
  (replace-regexp-in-string
   speech-tagger/+macro-regex+
   (lambda (ch-str) (gethash ch-str speech-tagger/+macro-charset-escapes+))
   str))
(defun speech-tagger/group-string-by-size (str n)
  "Splits list SET into N subsets of approximately equal size"
  (cl-loop with cur-ind = 0 and out-lists = (make-list n nil)
           for el across str
           do (progn
                (push el (nth cur-ind out-lists))
                (setq cur-ind (mod (1+ cur-ind) n)))
           finally (return out-lists)))
(defconst speech-tagger/+max-rgb-value+ 255)
(defun speech-tagger/split-string-into-thirds (str)
  (mapcar
   (lambda (l)
     (if l (cl-reduce
            (lambda (a b) (mod (* a b) speech-tagger/+max-rgb-value+)) l)
       speech-tagger/+max-rgb-value+))
   (speech-tagger/group-string-by-size str 3)))
(defun speech-tagger/hash-pos-for-color (pos-str)
  (concat
   "#"
   (reduce (lambda (a b)
             (concat (if (stringp a) a (format "%X" a))
                     (format "%X" b)))
           (speech-tagger/split-string-into-thirds pos-str))))

(defun speech-tagger/destructure-json-table (entry face)
  "Transforms json in a table entry for a part of speech into a plist."
  (let ((desc (aref entry 0))
        (examples (aref entry 1)))
    (list :description desc :examples examples :face face)))
(defun speech-tagger/get-json-table (path)
  (let ((tbl (let ((json-object-type 'hash-table))
               (json-read-file path))))
    (maphash
     (lambda (key val)
       (puthash
        key
        (speech-tagger/destructure-json-table
         val
         (custom-declare-face
          (intern (concat "speech-tagger/"
                          (speech-tagger/escape-macro-characters key)))
          `((default (:foreground ,(speech-tagger/hash-pos-for-color key))))
          ;; first of the value stored in the hash is description of pos
          (aref val 0)))
        tbl))
     tbl)
    tbl))
(defun speech-tagger/refresh-table ()
  (setq speech-tagger/*pos-hash*
        (speech-tagger/get-json-table speech-tagger/pos-json-path)))

;;; TODO: make a defcustom for the location of the jar; if none provided,
;;; download the jar and save it to the directory of this file
(defvar speech-tagger/jar-dir
  "/home/cosmicexplorer/projects/active/speech-tagger/target/")
(defvar speech-tagger/jar-name "speech-tagger-0.0.0-SNAPSHOT-standalone.jar")
(defvar speech-tagger/jar-path
  (concat speech-tagger/jar-dir speech-tagger/jar-name))

(defvar speech-tagger/*tag-proc* nil)
(defconst speech-tagger/+tag-proc-name+ "speech-tagger")
(defconst speech-tagger/+tag-proc-buf-name+ "*speech-tagger*")

(defvar speech-tagger/*job-id-counter* 0)
(defvar speech-tagger/*jobs* nil)

(defun speech-tagger/lock-region (beg end)
  ;; TODO: add some other face to indicate text is being analyzed
  (put-text-property beg end 'read-only t))

(defun speech-tagger/unlock-region (beg end)
  "Inverse of `speech-tagger/lock-region'."
  (let ((inhibit-read-only t))
    (put-text-property beg end 'read-only nil)))

(defun speech-tagger/make-region-log (beg end buf)
  (goto-char beg)
  (let ((beg-mark (point-marker)))
    (set-marker-insertion-type beg-mark t)
    (goto-char end)
    (let ((end-mark (point-marker)))
      (set-marker-insertion-type end-mark t)
      (list :beg beg-mark :end end-mark :buffer buf
            :text (with-current-buffer buf (buffer-substring beg end))))))

(defun speech-tagger/lock-region-and-log (beg end id)
  ;; lock from editing
  (speech-tagger/lock-region beg end)
  (puthash id (speech-tagger/make-region-log beg end (current-buffer))
           speech-tagger/*jobs*))

(defun speech-tagger/make-tag-proc-json (beg end id)
  (list :job-id id
        :string (buffer-substring-no-properties beg end)))

(defun speech-tagger/search-for-whitespace (direction)
  (let ((space-regex "[[:space:]\r\n]"))
    (cond ((eq direction 'backward)
           (unless (let ((ch (char-before)))
                     (and ch (string-match-p space-regex (make-string 1 ch))))
             (when (re-search-backward space-regex nil t) (forward-char))))
          ((eq direction 'forward)
           (unless (let ((ch (char-after)))
                     (and ch (string-match-p space-regex (make-string 1 ch))))
             (when (re-search-forward space-regex nil t) (backward-char))))
          (t (throw 'speech-tagger/bad-search-direction
                    "whitespace search direction not recognized")))))

(defun speech-tagger/widen-region-to-word-bounds (beg end)
  (goto-char beg)
  (speech-tagger/search-for-whitespace 'backward)
  (let ((new-beg (point)))
    (goto-char end)
    (speech-tagger/search-for-whitespace 'forward)
    (list beg end)))

(defun speech-tagger/get-job-id ()
  (cl-loop with first-id = (1- speech-tagger/*job-id-counter*)
           while (gethash speech-tagger/*job-id-counter* speech-tagger/*jobs*)
           do (if (= speech-tagger/*job-id-counter* first-id)
                  ;; should never happen unless rest of code is awful
                  (throw 'no-available-jobs "no free job ids found")
                (incf speech-tagger/*job-id-counter*))
           finally (return speech-tagger/*job-id-counter*)))

(defvar speech-tagger/*tag-proc-cur-line* "")

(defun speech-tagger/mark-parts-of-speech (beg tagged-string)
  "Marks parts of speech between BEG and END according to tags in
TAGGED-STRING."
  (cl-loop
   for tagged-section across tagged-string
   do (let ((offset (plist-get tagged-section :start))
            (final (plist-get tagged-section :end))
            (text (plist-get tagged-section :text))
            (tag (plist-get tagged-section :tag)))
        (let* ((beg-ind (+ beg offset))
               (end-ind (+ beg final))
               (new-txt (buffer-substring beg-ind end-ind)))
          (unless (equal text new-txt)
            (throw 'speech-tagger/different-text
                   (format "%s \"%s\" %s \"%s\"" "previous text" text
                           "is different than current text" new-txt)))
          (let ((olay (make-overlay beg-ind end-ind))
                (tag-hash (gethash tag speech-tagger/*pos-hash*)))
            (overlay-put olay 'face (plist-get tag-hash :face))
            (overlay-put olay 'speech-tagger t)
            (overlay-put olay 'help-echo
                         (format "%s: e.g %s"
                                 (plist-get tag-hash :description)
                                 (plist-get tag-hash :examples)))
            (overlay-put olay 'mouse-face 'mode-line-highlight))))))

(defun speech-tagger/process-tag-proc-json (plist)
  "Takes a json message PLIST from the external process and uses it to highlight
text in the region marked by the job-id key of PLIST. Pops the job-id off of
`speech-tagger/*jobs*'"
  (let* ((job-id (plist-get plist :job-id))
         (tagged-string (plist-get plist :tagged-string))
         (reg-log (gethash job-id speech-tagger/*jobs*)))
    (if (not reg-log)
        (throw 'speech-tagger/no-such-job
               (format "%s %d %s" "no job with id" job-id "found"))
      (cl-destructuring-bind (:beg beg :end end :buffer buf :text text) reg-log
        (with-current-buffer buf
          (let ((cur-text (buffer-substring beg end)))
            (unless (equal text cur-text)
              (throw 'speech-tagger/different-text
                     (format "%s \"%s\" %s \"%s\"" "previous text" text
                             "is different than current text" cur-text))))
          (speech-tagger/mark-parts-of-speech beg tagged-string)
          (remhash job-id speech-tagger/*jobs*)
          (speech-tagger/unlock-region beg end))))))

;;; all json is received as a single line of text, making stream parsing easier
(defun speech-tagger/receive-tag-proc-string (str)
  (let ((newline-match (string-match-p "\n" str)))
    ;; in case json message is larger than emacs's process output buffer
    ;; (unlikely if we don't send in massive strings to tagging process)
    (if (not newline-match)
        (setq speech-tagger/*tag-proc-cur-line*
              (concat speech-tagger/*tag-proc-cur-line* str))
      (let* ((msg (concat speech-tagger/*tag-proc-cur-line*
                          (substring str 0 newline-match)))
             (json-msg (let ((json-object-type 'plist))
                         (json-read-from-string msg))))
        (speech-tagger/process-tag-proc-json json-msg)
        (setq speech-tagger/*tag-proc-cur-line* "")
        (speech-tagger/receive-tag-proc-string
         (substring str (1+ newline-match)))))))

(defun speech-tagger/message-process-buffer (proc msg)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (when (eolp) (insert "\n"))
    (insert msg)))

(defun speech-tagger/start-tag-process ()
  (setq
   speech-tagger/*tag-proc*
   (let ((new-proc
          (start-process
           speech-tagger/+tag-proc-name+ speech-tagger/+tag-proc-buf-name+
           "java" "-jar" speech-tagger/jar-path)))
     (set-process-filter
      new-proc (lambda (proc msg)
                 (speech-tagger/message-process-buffer proc msg)
                 (speech-tagger/receive-tag-proc-string msg)))
     (set-process-sentinel
      new-proc (lambda (proc msg)
                 (speech-tagger/message-process-buffer proc msg)
                 (message "%s %s %s"
                          (process-name proc) "exited with message" msg)))
     new-proc)))

(defun speech-tagger/setup ()
  (unless speech-tagger/*pos-hash* (speech-tagger/refresh-table))
  (unless speech-tagger/*jobs* (setq speech-tagger/*jobs* (make-hash-table)))
  (unless (process-live-p (get-process speech-tagger/+tag-proc-name+))
    (speech-tagger/start-tag-process)))

(defun speech-tagger/send-region-to-tag-proc (beg end proc)
  (let* ((id (speech-tagger/get-job-id))
         (bounds (speech-tagger/widen-region-to-word-bounds beg end))
         (new-beg (first bounds))
         (new-end (second bounds)))
    (speech-tagger/lock-region-and-log new-beg new-end id)
    (process-send-string
     proc
     (concat (json-encode
              (speech-tagger/make-tag-proc-json new-beg new-end id))
             "\n"))))

(defun speech-tagger/clear-state ()
  (when speech-tagger/*jobs* (clrhash speech-tagger/*jobs*))
  (when speech-tagger/*pos-hash* (clrhash speech-tagger/*pos-hash*))
  (setq speech-tagger/*job-id-counter* 0
        speech-tagger/*tag-proc* nil
        speech-tagger/*tag-proc-cur-line* ""
        speech-tagger/*jobs* nil
        speech-tagger/*pos-hash* nil)
  (mapcar
   (lambda (proc)
     (when (equal (buffer-name (process-buffer proc))
                  speech-tagger/+tag-proc-buf-name+)
       (delete-process proc)))
   (process-list))
  (when (bufferp speech-tagger/+tag-proc-buf-name+)
    (kill-buffer speech-tagger/+tag-proc-buf-name+)))

(defun speech-tagger/clear-overlays (&optional beg end)
  (let ((b (or beg (point-min))) (e (or end (point-max))))
    (let ((inhibit-read-only t))
      (put-text-property b e 'read-only nil))
    (remove-overlays b e 'speech-tagger t)))

(defun speech-tagger/tag-dwim (pfx)
  (interactive "P")
  (speech-tagger/setup)
  (if (not pfx)
      (if (use-region-p)
          (let ((wide-range (speech-tagger/widen-region-to-word-bounds
                              (region-beginning) (region-end))))
            (speech-tagger/clear-overlays
             (first wide-range) (second wide-range))
            (speech-tagger/send-region-to-tag-proc
             (region-beginning) (region-end) speech-tagger/*tag-proc*))
        (speech-tagger/clear-overlays (point-min) (point-max))
        (speech-tagger/send-region-to-tag-proc
         (point-min) (point-max) speech-tagger/*tag-proc*))
    (let ((bufname (read-buffer "buffer to tag: " nil t)))
      (with-current-buffer bufname
        (speech-tagger/clear-overlays (point-min) (point-max))
        (speech-tagger/send-region-to-tag-proc (point-min) (point-max)
                                               speech-tagger/*tag-proc*)))))
