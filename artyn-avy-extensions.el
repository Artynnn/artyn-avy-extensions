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

;;;; auxiliary functions
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


;; this should probably be moved to my init.el
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

(define-key global-map (kbd "C-M-s") 'isearch-forward-other-window)
(define-key global-map (kbd "C-M-s") 'isearch-backward-other-window)

;;;; avy actions
(defun avy-action-copy-url (pt)
  "Copies url, I believe this only works on Org Buffers?"
  (save-excursion
    (goto-char pt)
    (my-org-retrieve-url-from-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)


(defun avy-action-embark (pt)
  "Open up the selected text with Embark, which provides the equivalent of a right click context menu"
  (save-excursion
    (goto-char pt)
    (embark-act))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(defun avy-action-helpful (pt)
  "Open up the selected text with helpful, which many consider to be a better help buffer"
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(defun avy-action-flyspell (pt)
  "Fix misspelled word"
  (save-excursion
    (goto-char pt)
    (when (require 'flyspell nil t)
      (flyspell-auto-correct-word)))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank (pt)
  "Yank sexp starting at PT at the current point."
  (avy-action-copy pt)
  (yank)
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

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

;;;; keybindings
(setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank)
(setf (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

;; (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
;;       (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)
;; ;; (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy-gnus-url ;; nee copy
;; ;;       (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)
;; (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy-url ;; nee copy
;;       (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)
;; (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
;;       (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)
;; (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
;;       (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)
;; (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell)
;; (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)
;; (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
;; (setf (alist-get ?= avy-dispatch-alist) 'avy-action-tuxi)
;; (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-quickly-fix-word) ; nee avy-action-flyspell
;; (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
;;       (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)
;; (setf (alist-get ?j avy-dispatch-alist) 'avy-action-jump-to-file))

(provide 'artyn-avy)


