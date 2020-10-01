;;; read-along.el --- A simple interface to TTS engines

;; Originally based on read-aloud.el by
;; Alexander Gromnitsky <alexander.gromnitsky@gmail.com>
;; ORIGINAL URL: https://github.com/gromnitsky/read-aloud.el

;;; Commentary:

;; This package invokes an external TTS engine (such as espeak) to
;; pronounce and highlight the word at or near point, the selected
;; region, the current line, the current paragraph, or a whole buffer.

;;; License:

;; Copyright (c) 2016 Alexander Gromnitsky
;; Copyright (c) 2018,2019,2020 D. F. Hall

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; -- ;; -- ;; -- ;; -- ;; -- ;; -- ;; -- ;; -- ;; -- ;; -- ;; -- ;;

;;; History:

;; D. F. Hall <authorfunction@hardboiledbabylon.com>
;; Oct. 23, 2018
;;    Added support for ignoring formatting tags
;;    Add option to select downcasing string to be sent to TTS
;;      to help with pronunciation.
;; Oct. 24, 2018
;;    Removed CL-isms and re-implemented as pure elisp
;;      and removed need for subr-x.el
;;    Removed interactive selection of TTS engine.  Setq should be
;;      sufficient.
;;    Added read-along-stop-at-end to allow read-along-buf
;;      to exit while leaving speach process to finish.
;;      handling the fragment it's already received.
;;    Made logging optional.
;;    Added espeak as TTS option.
;; Oct. 26, 2018
;;    Added read-along-line
;; Nov. 13, 2018
;;    Reworked read-along--grab-text to skip invisible text.
;;    Extended tag removal support.
;;    Added ability to transform text before sending to TTS.
;; Nov. 28, 2018
;;    Moved entire egine to be based around reading any given
;;      text, whether buffer or selection or sentence, in
;;      piecemeal fashion.  Now all invisible text in an area
;;      or region should be ignored properly.
;; Mar 25, 2019
;;    Added support for user-selectable skip characters
;; Aug 11, 2019
;;    Added support for read-along-string-skips
;; Dec 23, 2019
;;    Added support for clearing hl-line mode during reading
;;    Added ability to calculate overlays piecemeal so overlays
;;      don't extend beyond fill-column, which is annoying
;;      This feature is available in emacs versions >26, but
;;      this makes it backwards compatible.
;; Dec 25, 2019
;;    Added support for skipping faces with read-along-ignore-faces.
;; Feb 5, 2020
;;    Consolidate to one TTS engine variable, rather than a
;;    multi-part one.  It's less trouble just to set that than
;;    manage switching between several options.
;; Aug 17, 2020
;;    Removed read-along-ignore-tags and add read-along-nobreak instead.
;;    This allows things to be more generic.
;;    Removed tag support completely and just rely on transforms.
;;    Added read-along-nobreak-exclude.
;; Sep 5, 2020
;;    Add kill-str support.  Different systems return too many
;;    different strings after a valid interrupt is issued to the
;;    child process, so an error can incorrectly be triggered.
;; Version: 0.0.17
;; Package-Requires: ((emacs "24.4"))
;; Keywords: multimedia

;; This file is not part of GNU Emacs.

;;; Code:

(require 'dfh-str)
(require 'dfh-faces)

;; user-facing variables

;;(defvar read-along-engine '(cmd "spd-say" args ("-e" "-w") kill "spd-say -S"))
(defvar read-along-engine '(cmd "espeak" args nil))

;; This is the longest possible string of characters
;; that will be read.  Some engines support a smaller
;; or greater number of characters at a time.
(defvar read-along-max 160)

(defface read-along-text-face '((t :inverse-video t))
  "For highlighting the text that is being read")

;; These should perhaps be buffer local.
(defvar read-along-nobreak nil)
(defvar read-along-nobreak-exclude nil)
(defvar read-along-transforms nil)
(defvar read-along-ignore-faces nil)

(defvar read-along-log-events nil)
(defvar read-along-always-stop-at-end nil)
(defvar read-along-downcase nil)
(defvar read-along-redisplay-window nil)

(defvar read-along-ignore-faces nil)

;; call `read-along-update-variables' after changing these in .emacs
(defvar read-along-char-skips nil)
(defvar read-along-string-skips nil)

;; internal variables

(defconst read-along--logbufname "*Read-Along Log*")

(defconst read-along--c-pr nil)
(defconst read-along--c-buf nil)
(defconst read-along--c-bufpos nil)
(defconst read-along--c-span-beg nil)
(defconst read-along--c-span-end nil)
(defconst read-along--c-locked nil)
(defconst read-along--c-overlays nil)
(defconst read-along--c-window nil)

(defvar read-along--need-restore-hl-line nil)

;; The characters skipped at the front of a string
;; are __NOT__ exactly the same as those that provide
;; possible break points for the end of a span.
;; The characters '\- are only skipped at the __start__
;; of a possible a span.
(defconst read-along--default-char-skips
  "][,.:!;[:space:]\r\n\"‘’“”?`'…—()/\\-")

;; these are always update by read-along-update-variables
(defvar read-along--tail-splits nil)
(defvar read-along--skips nil)

(defvar read-along--stop-flip-flop nil)

(defun read-along--mk-string-skips-tail (lst)
  (let ((ret ""))
    (dolist (str lst ret)
      (setq ret (concat ret "\\|" (regexp-quote str))))))

(defun read-along-update-variables ()
  (setq read-along--tail-splits
        (concat 
         "[][,.:!;\"‘’“”?`…—()/]\\|\\(\n\\|\r\n\\)\\{2,\\}"
         (read-along--mk-string-skips-tail read-along-string-skips)))
  (setq read-along--skips
        (concat read-along--default-char-skips read-along-char-skips)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call this immediately for the initial setup
(read-along-update-variables)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-along--log (msg &optional args)
  (when read-along-log-events
    (let ((buf (get-buffer-create read-along--logbufname)))
      (with-current-buffer buf
	(goto-char (point-max))
	(insert-before-markers (format (concat msg "\n") args))))))

(defun read-along--cmd ()
  (or (plist-get read-along-engine 'cmd)
      (user-error "Failed to get the default TTS engine")))

(defun read-along--args ()
  (plist-get read-along-engine 'args))

(defun read-along--valid-str-p (str)
  (> (length (dfh-trim-str str)) 0))

(defun read-along--overlay-rm()
  (when read-along--c-overlays
    (dolist (OV read-along--c-overlays nil)
      (delete-overlay OV))
    (setq read-along--c-overlays nil)))

;; Newer emacs versions aren't supposed to highlight in the
;; dead space in the right margin, but take care of
;; that anyway for older versions.
;; Written for emacs 26.1, but should work for earlier (maybe).
;; Doesn't handle visual-line-mode.
(defun read-along--overlay-make(beg end)
  (when (and beg end)
    (save-excursion
      (goto-char beg) ;; might not be needed, we should start here below
      (let ((WHERE beg))
        (while (< WHERE end)
          (skip-chars-forward "\n " end)
          (let ((rbeg (point)) (rend nil))
            (when (> (skip-chars-forward "^\n" end) 0)
              (setq rend (point))
              (setq read-along--c-overlays
                    (append read-along--c-overlays
                            (list (make-overlay rbeg rend))))
              (setq WHERE rend)))))
      (dolist (OV read-along--c-overlays nil)
        (overlay-put OV 'face 'read-along-text-face)) )))

(defun read-along--reset()
  "Reset internal state."
  (setq read-along--c-pr nil)
  (setq read-along--c-buf nil)
  (setq read-along--c-bufpos nil)
  (setq read-along--c-locked nil)
  (setq read-along--c-span-beg nil)
  (setq read-along--c-span-end nil)
  (setq read-along--c-window nil)

  ;; maybe should restore selection
  ;; on finishing with read-along-this when 'this'
  ;; is a region

  (read-along--overlay-rm)
  (when read-along--need-restore-hl-line
    (setq read-along--need-restore-hl-line nil)
    (hl-line-mode 1))
  (read-along--log "RESET"))

(defun read-along--string (str)
  "Open an async process, feed its stdin with STR."
  (when (read-along--valid-str-p str)
    (let ((process-connection-type nil)) ;; (start-process) requires this

      (setq read-along--c-locked t)
      (condition-case err
	  (setq read-along--c-pr
		(apply 'start-process "read-along" nil
		       (read-along--cmd) (read-along--args)))
	(error
	 (read-along--reset)
	 (user-error "External TTS engine failed to start: %s"
		     (error-message-string err))) )

      (set-process-sentinel read-along--c-pr 'read-along--sentinel)

      (read-along--log "Sending: `%s`" str)
      (process-send-string read-along--c-pr (concat str "\n"))
      (process-send-eof read-along--c-pr)) ))

(defun read-along--sentinel (process event)
  (let* ((EV (dfh-trim-str event))
         (KS (plist-get read-along-engine 'kill-str)))

    (unless KS (setq KS "interrupt"))
    
    (cond ( (string= EV "finished")
            (if read-along--stop-flip-flop
	        (progn
	          (read-along--reset)
	          (setq read-along--stop-flip-flop nil))
              (read-along--overlay-rm)
	      (read-along--span)) )
          ( (string= EV KS)
            (read-along--reset) )
          ( t
            (read-along--reset)
            (user-error "%s ended w/ the event: %s" process EV)))))

(defun read-along--stop-hard ()
  "Ask a TTS engine to stop."
  (interrupt-process read-along--c-pr nil)
  ;; If a tts engine has a separate step to switch itself off, use it.
  (let ((c (plist-get read-along-engine 'kill))
	(event-log-status (if read-along-log-events
			      read-along--logbufname nil)))
    (when c
      (start-process-shell-command "read-along-kill" event-log-status c)))

  (read-along--log "INTERRUPTED BY USER"))

;; This doesn't invoke read-along-kill as per above command.
;; Should it?
(defun read-along--stop-at-end ()
  "Disable read-along-buf at end of current fragment."
  (when read-along--c-locked
      (setq read-along--stop-flip-flop t)))

;;;###autoload
(defun read-along-stop ()
  (interactive)
  (if read-along-always-stop-at-end
      (read-along--stop-at-end)
    (read-along--stop-hard)))

(defun read-along--lockedp ()
  (if read-along--c-locked
      (progn
	(message "read-along: read already in progress")
	t)
    nil))

(defun read-along--wonkyp ()
  (if (or
       (> read-along--c-bufpos read-along--c-span-end)
       (< read-along--c-bufpos read-along--c-span-beg)
       (> read-along--c-span-beg read-along--c-span-end)
       (< read-along--c-span-end read-along--c-span-beg))
      t
    nil))

(defun read-along--span ()
  "Read the current span in the current buffer,
 highlighting words along the read."
  (interactive)

  (unless read-along--c-buf (setq read-along--c-buf (current-buffer)))

  ;; clear hl-mode
  (when (and (boundp 'hl-line-mode) hl-line-mode)
      (setq read-along--need-restore-hl-line t)
      (hl-line-mode 0))
  ;; redisplay or otherwise we might end up with the first
  ;; part of a line that in currently hidden not being
  ;; uncovered when it's read.
  ;; only if in window
  (unless read-along--c-bufpos
    (setq read-along--c-bufpos read-along--c-span-beg)
    (goto-char read-along--c-bufpos)
    (redisplay))

  (if (read-along--wonkyp)
      (read-along--reset)

    (unless read-along--c-window
      (setq read-along--c-window (selected-window)))
    
    (let ((tb nil))
      (with-current-buffer read-along--c-buf
        (narrow-to-region read-along--c-bufpos read-along--c-span-end)
	(setq tb (read-along--grab-text))
	(widen))

      (if (not tb)
	  (read-along--reset)
        ;; else there's something to read-along with
        (let ((pbeg (plist-get tb 'beg))
              (pend (plist-get tb 'end))
              (ptext (plist-get tb 'text)))
          
	  (goto-char pbeg)

	  (when read-along-redisplay-window
	    (unless (pos-visible-in-window-p pend read-along--c-window nil)
	      (recenter 0)))
	  
	  ;; highlight text
	  (read-along--log "overlay-start: `%d`" pbeg)
          (read-along--log "overlay-end: `%d`" pend)
          (read-along--log "text: `%s`" ptext)
	  (read-along--overlay-make pbeg pend)

	  (setq read-along--c-bufpos pend)
          (goto-char read-along--c-bufpos)

	  (read-along--string ptext))))))

;;;###autoload
(defun read-along-between (beg end)
  (unless (read-along--lockedp)
    (setq read-along--c-span-beg beg)
    (setq read-along--c-span-end end)
    (deactivate-mark)
    (read-along--span)))

;;;###autoload
(defun read-along-buf ()
  (interactive)
  (read-along-between (point) (point-max)))

(defun read-along--get-substring (beg end)
  "Get the substring from the current buffer between
BEG and END and return the coordinates of a string
in the buffer that is between skippable characters.
Returns a cons cells or nil if nothing can be excised."
  
  (let (raw-str)
    (save-match-data
      (setq raw-str (buffer-substring-no-properties beg end))

       (when read-along-nobreak
	(setq raw-str (dfh-str-nullify-unless
                       raw-str read-along-nobreak
                       ?e
                       read-along-nobreak-exclude)))

      ;; can't know if word isn't partially cut off
      (unless (or (= end (point-max)) (invisible-p end))
	(setq raw-str
	      (replace-regexp-in-string
	       "[[:space:]\r\n]+[^[:space:]]+\\'" "" raw-str)))

      (setq raw-str (dfh-trim-str-r raw-str))

      (if (> (length raw-str) 0)
	  (let ((chunks (split-string raw-str read-along--tail-splits t)))
            (if chunks
		(let* ((chunk (dfh-trim-str-r (car chunks)))
                       (lchunk (length chunk))
                       (pbeg beg)
                       (pend (+ pbeg lchunk)))

                  (if (and pbeg pend (> pend pbeg))
                      (cons pbeg pend)
		    nil))
              nil))
        nil))))

;; There is no protection from modifying the results of a
;; previous modification.
;; Be careful of applying transforms to already-transformed
;; text, unless that's what you want.
(defun read-along--apply-transforms (transform-list input)

  (let ((tmp-str input))

    (dolist (pair transform-list nil)
      (setq tmp-str (replace-regexp-in-string
		     (car pair) (cadr pair) tmp-str t)))

    (setq tmp-str (dfh-trim-str tmp-str))
    
    (if (= (length tmp-str) 0)
	nil
      tmp-str)))

(defun read-along--skip-forward (chars strings end)
  (while (or (> (skip-chars-forward chars end) 0) 
             (> (skip-strings-forward strings end) 0))))

(defun read-along--skip-backward (chars strings end)
  (while (or (< (skip-chars-backward chars end) 0)
             (< (skip-strings-backward strings end) 0))))

(defun read-along--pretty-spaces (str)
  "Remove single newlines, collapse consecutive space 
characters to one, and remove leading / trailing spaces
from STR. If the length of the resultant string is 0, 
return nil."
  (let ((ostr (dfh-trim-str
             (dfh-str-collapse-whitespace str))))
    (if (> (length ostr) 0)
        ostr
      nil)))

(defun read-along--grab-text ()
  "Return (text \"omglol\" beg 10 end 20) plist or nil on
eof. BUF & POINT are the starting location for the job.
BEG and END are the actual points in the buffer of the text
to highlight. TEXT is that same span but stripped of unnecessary
characters and with user transformations supplied in 
`read-along-apply-transforms' applied if `read-along-apply-transforms' 
is non-nil and matches specified in `read-along-nbreak' if 
`read-along-nobreak' is non-nil. Downcase the outputted speakable 
string if `read-along-downcase' is non-nil."
  (let (max visible-start visible-end zbeg zend coords ostr
            (output nil))
    
    (save-excursion
      (while (and (not output) (not (eobp)))

        (if (invisible-p (point))
	    (setq visible-start
		  (next-single-property-change (point) 'invisible))
	  (setq visible-start (point)))

        (if (not visible-start)
            ;; nothing visible all the way to end of buffer
            (goto-char (point-max))
          ;; else _something_ is visible
	  (if (< (- (point-max) visible-start) read-along-max)
	      (setq max (+ visible-start (- (point-max) visible-start)))
	    (setq max (+ read-along-max visible-start)))
	  ;; limit returned if all within range visible
	  ;; since visible-start is non-nil, visible-end *will*
	  ;; exist by definition
	  (setq visible-end (next-single-property-change
                             visible-start 'invisible nil max))

          (when read-along-ignore-faces
            (setq visible-start (next-face-in
                                 read-along-ignore-faces
                                 visible-start visible-end t))
            (setq visible-end (next-face-in
                               read-along-ignore-faces
                               visible-start visible-end nil)))

          (if (>= visible-start visible-end)
              ;; everything has an ignored face
              ;; but there might be something after this
              ;; so only skip to end and let eobp do its job
              ;; since next-face-in will not skip to eob
              ;; but next-single-property-change will
              (goto-char visible-end)
            ;; else there are characters that are visible
            ;; and don't have ignored faces
	    (goto-char visible-start)

            (read-along--skip-forward read-along--skips
                                  read-along-string-skips
                                  visible-end)

            (if (< (point) visible-end)
  	        (setq coords (read-along--get-substring
			      (point)
			      visible-end))
              (setq coords nil))

            (when coords
              (setq zbeg (car coords))
              (setq zend (cdr coords))

              (goto-char zend)

              ;; clear newlines so regexes in transforms
              ;; don't have to worry about them.
              (setq ostr (nuke-newlines
                          (buffer-substring-no-properties zbeg zend)))
              
              (when (and ostr read-along-transforms)
		(setq ostr (read-along--apply-transforms
		    	    read-along-transforms ostr)))
              
              (when (and ostr read-along-downcase)
                (setq ostr (downcase ostr)))

              (when ostr
                (setq ostr (read-along--pretty-spaces ostr)))

              (when ostr (setq output `(text ,ostr
                                             beg ,zbeg
                                             end ,zend))))))))
    ;; output is either nil or something
    ;; unless this section of the universe has gone wonky
    output))

(defun read-along--current-word ()
  "Pronounce a word under the pointer."
  (let (beg end word)
    (save-excursion
      (skip-chars-forward (concat "^" read-along--skips))
      (setq end (point))
      (skip-chars-backward (concat "^" read-along--skips))
      (setq beg (point))

      (if (> end beg)
	  (read-along-between beg end)
	(message "read-along: no word at point")))))

;;;###autoload
(defun read-along-this()
  "Pronounce either the selection or a word under the pointer."
  (interactive)
  (if (use-region-p)
      (read-along-between (region-beginning) (region-end))
    (read-along--current-word)))

;;;###autoload
(defun read-along-line ()
  "Read the current line."
  (interactive)
  (read-along-between (line-beginning-position) (line-end-position)))

;;;###autoload
(defun read-along-from-beginning-of-line ()
  "Read-Along from beginning of line to point"
  (interactive)
    (read-along-between (line-beginning-position) (point)))

;;;###autoload
(defun read-along-to-end-of-line ()
  "Read-Along from point to end of line"
  (interactive)
  (read-along-between (point) (line-end-position)))

;;;###autoload
(defun read-along-paragraph ()
  "Read-Along from point to end of line"
  (interactive)
  (mark-paragraph)
  (when (use-region-p)
    ;; check to make sure this actualy marked something
      (read-along-this)))

(provide 'read-along)
;;; read-along.el ends here
