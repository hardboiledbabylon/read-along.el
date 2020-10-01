;; Copyright © 2018 by D. F. Hall <authorfunction@hardboiledbabylon.com>

;; Permission to use, copy, modify, and/or distribute this software for
;; any purpose with or without fee is hereby granted, provided that the
;; above copyright notice and this permission notice appear in all
;; copies.

;; THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

;; a collection of various string-related functions

;; replace all matches of RGX in STR with
;; a string of equal length composed of the
;; character REP
(defun dfh-str-nullify (str rgx rep)
  (let ((lenstr (length str))
	(w 0)
	(out ""))
    (if (= 0 lenstr)
	""
      (save-match-data
	(while (string-match rgx str w)
	  (let* ((mend (match-end 0))
		 (mbeg (match-beginning 0))
		 (lm (- mend mbeg)))
	    (setq out (concat out (substring str w mbeg) (make-string lm rep)))
	    (setq w mend))))
      (if (string= "" out)
	  str
	(let ((lo (length out)))
	  (if (< lo lenstr)
	      (setq out (concat out (substring str lo lenstr)))))
	out))))

;; replace all matches of RGX in STR with
;; a string of equal length composed of the
;; character REP only if the match is not equal
;; to one of the strings in the list XMPT
(defun dfh-str-nullify-unless (str rgx rep xmpt)
  (let ((lenstr (length str))
	(w 0)
	(out ""))
    (if (= 0 lenstr)
	""
      (save-match-data
	(while (string-match rgx str w)
	  (let* ((mend (match-end 0))
		 (mbeg (match-beginning 0))
		 (lm (- mend mbeg))
                 (ms (match-string-no-properties 0 str)))
            (if (member ms xmpt)
                (setq out (concat out (substring str w mend)))
	      (setq out (concat out (substring str w mbeg)
                                (make-string lm rep))))
	    (setq w mend))))
      (if (string= "" out)
	  str
	(let ((lo (length out)))
	  (if (< lo lenstr)
	      (setq out (concat out (substring str lo lenstr)))))
	out))))

(defun dfh-str-collapse-whitespace (str)
  (replace-regexp-in-string "[[:space:]]+" " " str))

(defun dfh-str-not-blankp (str)
  (let ((xstr (replace-regexp-in-string "[[:space:]]" "" str)))
    (if (= (length xstr) 0)
        nil
      t)))

(defun dfh-trim-str-l (str)
  (when str
    (replace-regexp-in-string "\\`[[:space:]\t\n\r]+" "" str)))

(defun dfh-trim-str-r (str)
  (when str
    (replace-regexp-in-string "[[:space:]\t\n\r]+\\'" "" str)))

(defun dfh-trim-str (str)
  (dfh-trim-str-l (dfh-trim-str-r str)))

(defun preceding-string (len)
  (if (bobp)
      ""
    (if (< (point) len)
	""
      (buffer-substring-no-properties (- (point) len) (point)))))

(defun following-string (len)
  (if (eobp)
      ""
    (if (< (- (point-max) (point)) len)
	""
      (buffer-substring-no-properties (point) (+ (point) len) ))))

;; this should do the lifting for above
;; macro ???
(defun following-string-at (pos len)
  (if (eobp)
      ""
    (if (< (- (point-max) pos) len)
	""
      (buffer-substring-no-properties pos (+ pos len) ))))

(defun string-after (p len)
  (save-excursion
    (goto-char p)
    (following-string len)))

(defun string-before (p len)
  (save-excursion
    (goto-char p)
    (preceding-string len)))

(defun skip-strings-forward (strings end)
  (let ((ret 0))
    (if strings
        (catch 'ret
          (dolist (mstr strings ret)
            (let* ((lstr (length mstr)) (fstr (following-string lstr)))
              (when (string= fstr mstr)
                (when (<= (+ (point) lstr) end)
                  (forward-char lstr)
                  (throw 'ret lstr))))))
      ret)))

(defun skip-strings-backward (strings end)
  (let ((ret 0))
    (if strings
        (catch 'ret
        (dolist (mstr strings ret)
          (let* ((lstr (length mstr)) (fstr (preceding-string lstr)))
            (when (string= fstr mstr)
              (when (>= (- (point) lstr) end)
                (backward-char lstr)
                (throw 'ret (* -1 lstr)))))))
      ret)))

(defun nuke-newlines (str)
  (replace-regexp-in-string "[\n\r]" " " str))

(provide 'dfh-str)
