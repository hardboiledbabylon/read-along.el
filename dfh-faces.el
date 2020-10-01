;; Copyright © 2020 by D. F. Hall <authorfunction@hardboiledbabylon.com>

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

;; a collection of various face-related functions

(defun next-face-in (faces start &optional limit invert)
  "Return the position of the first character with any one
of the faces in list FACES. If INVERT is non-nil, return the 
position of the first character with a face not in FACES. 
Search forward until end of buffer or LIMIT."

  (unless (numberp limit) (setq limit (point-max)))

  (let ((f nil) (r nil) (WHERE start))
    (while (and (not r) (< WHERE limit))
      (setq f (get-text-property WHERE 'face))
      (if invert
          (when (not (memq f faces))
            (setq r WHERE))
        (when (memq f faces)
          (setq r WHERE)))
      (setq WHERE (1+ WHERE)))
    (or r limit)))

(provide 'dfh-faces)
