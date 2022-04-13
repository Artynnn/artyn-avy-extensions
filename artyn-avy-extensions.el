;;; artyn-avy-extensions.el --- Extensions to avy for my dotemacs -*- lexical-binding: t -*-

;; Author: Artyn
;; URL: https://github.com/Artynnn/artyn-avy-extensions
;; Version: 1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This is code from the blogpost by Karthink "avy can do anything". Packaged.
;; https://karthinks.com/software/avy-can-do-anything/

;; I changed the copy command to be copy URL. Rarely do I ever need to
;; copy a word, I can type pretty fast and use spell check if it comes
;; out super bad. I also removed the dictionary comand and replaced it
;; with the tuxi / google command. My English is very good so I never
;; need to use a dictionary, only however for special weird words that
;; aren't in a dictionary.

;; code currently is omega messy

;; to add:
;; jump to hyperbole action
;; universal argument that specifies character length for things like deletion, yanking etc.
;; jump to synonym (with completion)
;; jump to definition?
;; capitalize word
;;; Code:

(require 'avy)
(require 'embark)
;; (require 'helpful)
;; also the Tuxi command line tool
;; https://github.com/Bugswriter/tuxi

(defgroup artyn-avy ()
  "Extensions for avy."
  :group 'avy)

;;;; keys to use for avy
;; These can't be the keys we will use for the avy actions.
(setq avy-keys '(?q ?e ?r ?u ?o ?p    ;; ?y
                    ?a ?s ?d ?f ?g ?h ;; ?j
                    ?k ?l ?' ?x ?c ?v ?b
                    ?n ?, ?/))

;;;; avy actions
(defun my-org-retrieve-url-from-point ()
  "Copies the URL from an org link at the point"
  (interactive)
  (let ((plain-url (url-get-url-at-point)))
    (if plain-url
        (progn
          (kill-new plain-url)
          (message (concat "Copied: " plain-url)))
      (let* ((link-info (assoc :link (org-context)))
             (text (when link-info
                     (buffer-substring-no-properties
                      (or (cadr link-info) (point-min))
                      (or (caddr link-info) (point-max))))))
        (if (not text)
            (error "Oops! Point isn't in an org link")
          (string-match org-link-bracket-re text)
          (let ((url (substring text (match-beginning 1) (match-end 1))))
            (kill-new url)
            (message (concat "Copied: " url))))))))

(defun avy-show-dispatch-help ()  
  (let* ((len (length "avy-action-"))
         (fw (frame-width))
         (raw-strings (mapcar
                       (lambda (x)
                         (format "%2s: %-19s"
                                 (propertize
                                  (char-to-string (car x))
                                  'face 'aw-key-face)
                                 (substring (symbol-name (cdr x)) len)))
                       avy-dispatch-alist))
         (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
         (strings-len (length raw-strings))
         (per-row (floor fw max-len))
         display-strings)
    (cl-loop for string in raw-strings
             for N from 1 to strings-len do
             (push (concat string " ") display-strings)
             (when (= (mod N per-row) 0) (push "\n" display-strings)))
    (message "%s" (apply #'concat (nreverse display-strings)))))

(defvar google-search-history nil
  "List of queries to google-search-string.")

(defun google-search-string (search-string)
  "Read SEARCH-STRING from the minibuffer and call the shell
command tuxi on it."
  (interactive (list (read-string "Google: " nil
                                  google-search-history
                                  (thing-at-point 'sexp))))
  (unless (executable-find "tuxi")
    (user-error "Cannot find shell command: tuxi"))
  (let ((search-output (string-trim-right
                        (shell-command-to-string
                         (concat
                          "tuxi -r "
                          (shell-quote-argument search-string))))))
    (with-current-buffer (get-buffer-create "*Tuxi Output*")
      (erase-buffer)
      (insert search-output)
      ;; (fill-region (point-min) (point-max))
      (if (<= (count-lines (point-min) (point-max)) 1)
          (message search-output)
        (goto-char (point-min))
        (display-buffer (current-buffer))
        (goto-address-mode 1)))))

(defun google-search-at-point (&optional beg end)
  "Call the shell command tuxi on the symbol at point. With an
active region use it instead."
  (interactive "r")
  (if-let ((search-string (if (use-region-p)
                              (buffer-substring-no-properties beg end)
                            (thing-at-point 'symbol))))
      (google-search-string search-string)
    ;; (message "No symbol to search for at point!")
    (call-interactively #'google-search-string)))

(defun avy-action-tuxi (pt)
  (cl-letf (((symbol-function 'keyboard-quit)
             #'abort-recursive-edit))
    (save-excursion
      (goto-char pt)
      (google-search-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun isearch-forward-other-window (prefix)
  "Function to isearch-forward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix -1 1)))
        (other-window next)
        (isearch-forward)
        (other-window (- next))))))

(defun isearch-backward-other-window (prefix)
  "Function to isearch-backward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix 1 -1)))
        (other-window next)
        (isearch-backward)
        (other-window (- next))))))

(defun avy-action-embark (pt)
  (save-excursion
    (goto-char pt)
    (embark-act))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(defun avy-action-jump-to-file (pt)
  (save-excursion
    (goto-char pt)
    ;; The problem here is how to make it not jump to the next window but to jump to the location in the same window.
    ;; https://emacs.stackexchange.com/questions/32775/compile-goto-error-be-more-consistent-in-choosing-a-window-for-display
    (compile-goto-error))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  
  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  
  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  
  (defun avy-action-teleport (pt)
    "Kill sexp starting on PT and yank into the current location."
    (avy-action-kill-stay pt)
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    (save-excursion
      (yank))
    t)
  
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)
  
  (defun avy-action-yank (pt)
    "Yank sexp starting at PT at the current point."
    (avy-action-copy pt)
    (yank)
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-copy (pt)
    "Copy sexp starting on PT."
    (save-excursion
      (let (str)
        (goto-char pt)
        (my-org-retrieve-url-from-point)))
    (let ((dat (ring-ref avy-ring 0)))
      (select-frame-set-input-focus
       (window-frame (cdr dat)))
      (select-window (cdr dat))
      (goto-char (car dat))))

  (defun avy-action-copy-url (pt)
    (save-excursion
      (goto-char pt)
      (my-org-retrieve-url-from-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-quickly-fix-word (pt)
    (save-excursion
      (goto-char pt)
      (recomplete-ispell-word pt))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-kill-stay (pt)
    "Kill sexp at PT."
    (save-excursion
      (goto-char pt)
      (avy-forward-item)
      (kill-region pt (point))
      (just-one-space))
    (message "Killed: %s" (current-kill 0))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  
  (define-key global-map (kbd "C-M-s") 'isearch-forward-other-window)
  (define-key global-map (kbd "C-M-s") 'isearch-backward-other-window)
  
  ;; ("C-M-s" . isearch-forward-other-window)
  ;; ("C-M-r" . isearch-backward-other-window))
  
  ;; for some reason I have to use `eval-last-sexp' for it work?


;;;; keybindings  
  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)
  ;; (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy-gnus-url ;; nee copy
  ;;       (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)
  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy-url ;; nee copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)
  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)
  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)
  (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell)
  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  (setf (alist-get ?= avy-dispatch-alist) 'avy-action-tuxi)
  (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-quickly-fix-word) ; nee avy-action-flyspell
  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)
  (setf (alist-get ?j avy-dispatch-alist) 'avy-action-jump-to-file))

  (defun avy-action-copy-gnus-url (pt)
    (save-excursion
      (goto-char pt)
      (shr-maybe-probe-and-copy-url URL))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-url (pt)
    (save-excursion
      (goto-char pt)
      (my-org-retrieve-url-from-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

(defun artyn-copy-article-url ()
  (interactive)
  '((gnus-article-copy-string "u" "Copy URL to kill ring")
    (push-button "\r" "Push the button")
    (push-button [mouse-2] "Push the button")))

(defun artyn-copy-url ()
  "Copy the URL under point to the kill ring.
With a prefix argument, or if there is no link under point, but
there is an image under point then copy the URL of the image
under point instead."
  (interactive (list (shr-url-at-point current-prefix-arg)))
  (if (not url)
      (message "No URL under point")
    (setq url (url-encode-url url))
    (kill-new url)
    (message "Copied %s" url)))

(provide 'artyn-avy)

;;   (defun avy-action-copy-gnus-url (pt)
;;     (save-excursion
;;       (goto-char pt)
;;       (shr-maybe-probe-and-copy-url URL))
;;     (select-window
;;      (cdr (ring-ref avy-ring 0)))
;;     t)

;;   (defun avy-action-copy-url (pt)
;;     (save-excursion
;;       (goto-char pt)
;;       (my-org-retrieve-url-from-point))
;;     (select-window
;;      (cdr (ring-ref avy-ring 0)))
;;     t)

;; (defun pxoq-copy-article-url ()
;;   (interactive)
;;   '((gnus-article-copy-string "u" "Copy URL to kill ring")
;;     (push-button "\r" "Push the button")
;;     (push-button [mouse-2] "Push the button")))

;; (shr-maybe-probe-and-copy-url URL)
;; URL
;; https://web.hypothes.is/blog/thoughtfully-adopting-edtech-ontarios-shift-to-social-annotation/
;; http://0sgzv.mjt.lu/lnk/AVEAAEClA4QAAAAAF1oAAAAyLtIAAAAACPoAAActABoyAABhjwuhpHML3wC_RQ6lJoDu2zsfYwAZSF8/5/Fh21uOr-oxCpHAqV5gH45w/aHR0cHM6Ly93ZWIuaHlwb3RoZXMuaXMvYmxvZy90aG91Z2h0ZnVsbHktYWRvcHRpbmctZWR0ZWNoLW9udGFyaW9zLXNoaWZ0LXRvLXNvY2lhbC1hbm5vdGF0aW9uLw

;; (defun pxoq-copy-url ()
;;   "Copy the URL under point to the kill ring.
;; With a prefix argument, or if there is no link under point, but
;; there is an image under point then copy the URL of the image
;; under point instead."
;;   (interactive (list (shr-url-at-point current-prefix-arg)))
;;   (if (not url)
;;       (message "No URL under point")
;;     (setq url (url-encode-url url))
;;     (kill-new url)
;;     (message "Copied %s" url)))

