;;; package --- Unsourced Sentences Mode
;;; Commentary:
;;; This package allows you to surround text in <</>> (by default) to mark
;;; text as containing unsourced claims (or whatever else you want).
;;; Such text will then be highlighted, and M-x unsourced-sentence-list will
;;; provide a list of unsourced sentences in the current buffer.
;;; While highlighting an entry, you can press RET to jump to it, or s to search
;;; for the entry online.
;;; 
;;; Code:
(defgroup unsourced-sentences nil
  "Highlight unsourced sentences in Emacs."
  :group 'convenience)

(defface unsourced-sentence-highlight
  '((t (:background "yellow" :foreground "black")))
  "Face for highlighting unsourced sentences.")

(defvar-local unsourced-sentence-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s h") 'toggle-unsourced-sentence-highlight)
    (define-key map (kbd "C-c s n") 'goto-next-unsourced-sentence)
    (define-key map (kbd "C-c s p") 'goto-prev-unsourced-sentence)
    (define-key map (kbd "C-c s s") 'unsourced-sentence-search)
    (define-key map (kbd "C-c s l") 'unsourced-sentence-list)
    map)
  "Keymap for `unsourced-sentence-mode'.")

(defvar-local unsourced-sentence-delimiter-left "<<"
  "Left delimiter marking the start of an unsourced sentence.")

(defvar-local unsourced-sentence-delimiter-right ">>"
  "Right delimiter marking the end of an unsourced sentence.")

(defun unsourced-sentence-regex ()
  "Computes the regex for unsourced sentences."
        (format "\\(%s\\)\\([^%s]*?\\)\\(%s\\)"
                (regexp-quote unsourced-sentence-delimiter-left)
                (regexp-quote unsourced-sentence-delimiter-right)
                (regexp-quote unsourced-sentence-delimiter-right)))

(defvar-local unsourced-sentence-highlighting-active nil
  "Indicates whether unsourced sentence highlighting is active.")

(defun unsourced-sentence-toggle-highlight ()
  "Toggle highlighting of unsourced sentences."
  (interactive)
  (if unsourced-sentence-highlighting-active
      (remove-unsourced-sentence-highlight)
    (highlight-unsourced-sentences)))

(defun highlight-unsourced-sentences ()
  "Highlight unsourced sentences based on the defined delimiters."
  (interactive)
  (let ((regexp (unsourced-sentence-regex)))
    (font-lock-add-keywords nil `((,regexp
                                   (1 'unsourced-sentence-highlight t)
                                   (2 'unsourced-sentence-highlight t)
                                   (3 'unsourced-sentence-highlight t))))
    (font-lock-flush)
    (font-lock-ensure)
    (setq unsourced-sentence-highlighting-active t)))

(defun remove-unsourced-sentence-highlight ()
  "Remove highlighting of unsourced sentences."
  (interactive)
  (let ((regexp (unsourced-sentence-regex)))
    (font-lock-remove-keywords nil `((,regexp
                                      (1 'unsourced-sentence-highlight t)
                                      (2 'unsourced-sentence-highlight t)
                                      (3 'unsourced-sentence-highlight t))))
    (font-lock-flush)
    (font-lock-ensure)
    (setq unsourced-sentence-highlighting-active nil)))

(defun unsourced-sentence-goto-next ()
  "Go to the next occurrence of an unsourced sentence."
  (interactive)
  (search-forward-regexp (regexp-quote unsourced-sentence-delimiter-left)
                         nil t))

(defun unsourced-sentence-goto-prev ()
  "Go to the previous occurrence of an unsourced sentence."
  (interactive)
  (search-backward-regexp (regexp-quote unsourced-sentence-delimiter-left)
                          nil t))

(defun extract-unsourced-sentence ()
  "Extract the content of the unsourced sentence at point."
  (save-excursion
    (when (search-backward-regexp (regexp-quote unsourced-sentence-delimiter-left) nil t)
      (goto-char (- (match-beginning 0) 1))
      (when (search-forward-regexp (unsourced-sentence-regex) nil t)
        (buffer-substring-no-properties (match-beginning 2) (match-end 2))))))

(defvar-local unsourced-sentence-search-sources
  '(("Google" . google-search)
    ("DuckDuckGo" . duckduckgo-search)
    ("Google Scholar" . google-scholar-search)
    )
  "List of search engines and their corresponding search functions.
Each entry is a cons cell where the CAR is the engine name and
the CDR is the function that performs the search.")

(defun google-search (query)
  "Search Google with QUERY."
  (browse-url (concat "https://www.google.com/search?q=" (url-hexify-string query))))

(defun duckduckgo-search (query)
  "Search DuckDuckGo with QUERY."
  (browse-url (concat "https://duckduckgo.com/?q=" (url-hexify-string query))))

(defun google-scholar-search (query)
  "Search Google Scholar with QUERY."
  (browse-url (concat "https://scholar.google.com/scholar?q=" (url-hexify-string query))))

(defun choose-search-engine ()
  "Prompt to choose a search source from `unsourced-sentence-search-sources`."
  (interactive)
  (let ((choices (mapcar #'car unsourced-sentence-search-sources)))
    (cdr (assoc (completing-read "Choose search engine: " choices)
                unsourced-sentence-search-sources))))

(defun unsourced-sentence-search ()
  "Perform a web search on the content of the unsourced sentence at point.
Prompts the user for a search engine to use."
  (interactive)
  (let* ((query (extract-unsourced-sentence))
         (search-function (choose-search-engine)))
    (message "%s, %s" query search-function)
    (when (and query search-function)
      (funcall search-function query))))

(defun unsourced-sentences-in-buffer ()
  "Return a list of all unsourced sentences in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (entries)
      (while (search-forward-regexp (unsourced-sentence-regex)
                                   nil t)
        (push (cons (match-string-no-properties 0)
                    (list :pos (match-beginning 0)
                          :line (line-number-at-pos (match-beginning 0)))) entries))
      (reverse entries))))

(defun unsourced-sentence-list ()
  "Display a list of all unsourced sentences in the current buffer."
  (interactive)
  (let ((entries (unsourced-sentences-in-buffer))
        (bufname (buffer-name)))
    (if (= (length entries) 0)
        (message "No unsourced sentences found in buffer.")
      (with-current-buffer (get-buffer-create "*Unsourced Sentences*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert "Ref\tContent\n")
        (dolist (entry entries)
          (let ((text (car entry))
                (pos  (plist-get (cdr entry) :pos))
                (line (plist-get (cdr entry) :line)))
            (insert (format "%s:%d:\t%s"
                            bufname
                            line
                            text))
            (put-text-property (line-beginning-position) (line-end-position) 'entry-pos pos)
            (put-text-property (line-beginning-position) (line-end-position) 'entry-buffer bufname)
            (put-text-property (line-beginning-position) (line-end-position) 'entry-text text))
            (insert "\n"))
        (goto-char (point-min))
        (forward-line)
        (unsourced-sentence-list-mode)
        (switch-to-buffer-other-window (current-buffer))))))

(defun unsourced-sentence-list-visit ()
  "Visit the location of the unsourced sentence on the current line."
  (interactive)
  (let ((pos (get-text-property (point) 'entry-pos))
        (buf (get-text-property (point) 'entry-buffer))
        (win (get-buffer-window (get-text-property (point) 'entry-buffer))))
    (progn
      (message (format "Visiting %s:%d in window %s" buf pos win))
      (if pos
          (if win
              (progn
                (select-window win)
                (goto-char pos)
                (recenter))
          (switch-to-buffer-other-window buf))
      (error "No entry position found")))))

(defun unsourced-sentence-list-search ()
  "Search for the content of the unsourced sentence on the current line."
  (interactive)
  (let ((text (substring (get-text-property (point) 'entry-text)
                         (length unsourced-sentence-delimiter-left)
                         (- (length unsourced-sentence-delimiter-right)))))
    (if text
        (browse-url (concat "https://www.google.com/search?q=" (url-hexify-string text)))
      (error "No entry text found"))))

(defvar-local unsourced-sentence-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")   'quit-window)
    (define-key map (kbd "s")   'unsourced-sentence-list-search)
    (define-key map (kbd "RET") 'unsourced-sentence-list-visit)
    map)
  "Keymap for `unsourced-sentences-mode'.")

(define-derived-mode unsourced-sentence-list-mode special-mode "Unsourced Sentences"
  "Major mode for listing unsourced sentences."
  (setq buffer-read-only t)
  (highlight-unsourced-sentences))

(defvar unsourced-sentence-mode nil
  "Mode variable for unsourced-sentence-mode.")

(define-minor-mode unsourced-sentence-mode
  "Minor mode for highlighting and navigating unsourced sentences."
  :lighter " Unsourced"
  :keymap unsourced-sentence-mode-map
  (interactive (list (or current-prefix-arg 'toggle)))
  (let ((enable
         (if (eq arg 'toggle)
             (not unsourced-sentence-mode)
           (> (prefix-numeric-value arg) 0))))
    (if unsourced-sentence-mode
        (highlight-unsourced-sentences)
      (remove-unsourced-sentence-highlight)))

(provide 'unsourced-sentence-mode)
