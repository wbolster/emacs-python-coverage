;;; python-coverage.el --- Python coverage support -*- lexical-binding: t; -*-

;; Author: wouter bolsterlee <wouter@bolsterl.ee>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25") (dash "2.16.0") (dash-functional "1.2.0") (s "1.12.0") (xml+ "1"))
;; Keywords: languages, processes, tools
;; URL: https://github.com/wbolster/emacs-python-coverage

;;; License:

;; 3-clause "New BSD"; see readme for details.

;;; Commentary:

;; Show Python coverage results in source files.

;; todo defcustom for use-magit-faces
;; todo use 'error 'or 'diff-refine-removed face
;; todo don't (forward-line) from (point-min), maybe use (-zip-pair) for relative jumps
;; todo on-save-hook?
;; todo merge adjacent overlays
;; todo warn when file newer than coverage.xml
;; todo kill-buffer-hook to cancel timer
;; todo file-notify-add-watch
;; todo file-notify-rm-watch
;; todo https://stackoverflow.com/questions/24007822/emacs-creating-deleting-a-buffer-local-repeating-idle-timer
;; todo M-x python-coverage-overview

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'python)
(require 's)
(require 'xml)
(require 'xml+)

(defgroup python-coverage nil
  "Python coverage"
  :group 'python
  :prefix "python-coverage")

(defcustom python-coverage-default-file-name "coverage.xml"
  "Default file name to use when looking for coverage results."
  :group 'python-coverage
  :type 'string)

(defcustom python-coverage-overlay-refresh-interval 5
  "Time between automatic refreshes of the coverage overlay."
  :group 'python-coverage
  :type 'integer)

(defvar-local python-coverage--coverage-file-name nil
  "Coverage file to use for the current buffer.")

(defvar-local python-coverage--coverage-file-mtime 0.
  "Modified time of the coverage file used in the current buffer.")

(defvar-local python-coverage--overlay-timer nil
  "Timer for automatic overlay refreshing.")

(defvar-local python-coverage--overlay-watch nil
  "Timer for automatic overlay refreshing.")

;; Public interface

;;;###autoload
(defun python-coverage-select-coverage-file (&optional coverage-file-name)
  "Explicitly set the COVERAGE-FILE-NAME to use for the current buffer.

This is only needed if autodetection does not work."
  (interactive "f")
  (setq python-coverage--coverage-file-name coverage-file-name))

;;;###autoload
(define-minor-mode python-coverage-overlay-mode
  "Minor mode to show Python coverage results as overlays."
  :lighter " PyCov"
  (if python-coverage-overlay-mode
      (progn
        (python-coverage--overlay-install-timer))
    (python-coverage--overlay-cancel-timer)
    (python-coverage-overlay-remove-all)))

;;;###autoload
(defun python-coverage-overlay-refresh ()
  "Refresh the overlays."
  (interactive)
  (let ((coverage-info (python-coverage-current-buffer)))
    (when (python-coverage--coverage-file-outdated?)
      (message "Note: coverage file is potentially outdated."))
    (save-restriction
      (widen)
      (overlay-recenter (point-max))
      (python-coverage-overlay-remove-all)
      (python-coverage--overlay-make-all coverage-info))))

;;;###autoload
(defun python-coverage-overlay-remove-all ()
  "Remove all overlays."
  (interactive)
  (save-restriction
    (widen)
    (remove-overlays nil nil 'category 'python-coverage)))

;;;###autoload
(defun python-coverage-overlay-jump-next ()
  "Jump to the next overlay."
  (interactive)
  (-if-let* ((overlay (-first-item (python-coverage--overlays-in (1+ (point)) nil))))
      (goto-char (overlay-start overlay))
    (user-error "No more coverage overlays in this direction")))

;;;###autoload
(defun python-coverage-overlay-jump-previous ()
  "Jump to the previous overlay."
  (interactive)
  (-if-let* ((overlay (-last-item (python-coverage--overlays-in nil (point)))))
      (goto-char (overlay-start overlay))
    (user-error "No more coverage overlays in this direction")))

(defun python-coverage-overlay-jump-first ()
  "Jump to the first overlay."
  (interactive)
  (-if-let* ((overlay (-first-item (python-coverage--overlays-in))))
      (goto-char (overlay-start overlay))
    (user-error "No coverage overlays in buffer")))

(defun python-coverage-current-buffer ()
  "Obtain coverage info for the current buffer."
  (-when-let*
      ((coverage-file-name (python-coverage--find-coverage-file-current-buffer))
       (tree (python-coverage--parse-coverage-xml-file coverage-file-name))
       (coverage-info (python-coverage--get-missing-file-coverage tree (buffer-file-name))))
    coverage-info))

;; Internal helpers for handling files

(defun python-coverage--find-coverage-file-current-buffer ()
  "Find a coverage file for the current buffer."
  (-let [source-file-name
         (or (buffer-file-name)
             (error "Cannot detect source file name; buffer is not visiting a file"))]
    (python-coverage--find-coverage-file source-file-name)))

(defun python-coverage--find-coverage-file (source-file-name)
  "Find a coverage file for SOURCE-FILE-NAME."
  (or
   python-coverage--coverage-file-name
   (-some->
    (python-coverage--locate-dominating-file source-file-name python-coverage-default-file-name)
    (file-name-as-directory)
    (s-concat python-coverage-default-file-name))
   (error "Could not find coverage file. (Hint: use ‘M-x python-coverage-select-coverage-file’ to choose manually.)")))

(declare-function projectile-locate-dominating-file "projectile" (file name))

(defun python-coverage--locate-dominating-file (file name)
  "Like ‘locate-dominating-file’, using Projectile if available.

FILE and NAME are handled like ‘locate-dominating-file’ does."
  (if (featurep 'projectile)
      (projectile-locate-dominating-file file name)
    (locate-dominating-file file name)))

(defun python-coverage--coverage-file-outdated? (&optional file-name)
  "Return t when the coverage file for FILE-NAME is outdated."
  (unless file-name (setq file-name (buffer-file-name)))
  (let* ((coverage-file (python-coverage--find-coverage-file-current-buffer))
         (coverage-mtime (python-coverage--mtime coverage-file))
         (file-mtime (python-coverage--mtime file-name)))
    (< coverage-mtime file-mtime)))

(defun python-coverage--mtime (file-name)
  "Get the mtime of FILE-NAME as a float."
  (->> (file-attributes file-name)
       (nth 5)
       (float-time)))

;; Internal helpers for handling the coverage XML format

(defun python-coverage--parse-xml-file (name)
  "Parse an XML file NAME."
  ;; Try to use libxml, and fall back to the slower built-in function.
  (or
   (if (fboundp 'libxml-parse-xml-region)
       (with-temp-buffer
         (insert-file-contents name)
         (libxml-parse-xml-region (point-min) (point-max)))
     (-first-item (xml-parse-file name)))
   (error "Could not parse coverage file ‘%s’" name)))

(defun python-coverage--parse-coverage-xml-file (name)
  "Parse the XML file NAME."
  (-when-let* ((tree (python-coverage--parse-xml-file name)))
    (unless (eq (car tree) 'coverage)
      (error "Unknown XML file format; root element should be <coverage>"))
    tree))

(defun python-coverage--get-missing-file-coverage (tree file-name)
  "Get the missing coverage for FILE-NAME from TREE."
  (-when-let (class-node (python-coverage--find-class-node tree file-name))
    (python-coverage--extract-lines class-node)))

(defun python-coverage--find-class-node (tree file-name)
  "Find the <class> XML node in TREE for the specified FILE-NAME."
  ;; Unfortunately, the XML does not contain full file paths. Find all
  ;; <class name=...> elements for the base file name, then check if
  ;; any of them matches when combined with any of the source paths.
  (-if-let*
      ((file-name-without-directory (file-name-nondirectory file-name))
       (query `((coverage) > (packages) > (package) > (classes) >
                (class :name ,file-name-without-directory)))
       (class-node-candidates (xml+-query-all tree query))
       (source-paths (python-coverage--get-source-paths tree))
       (class-node
        (--first
         (python-coverage--class-node-matches-file-name? it file-name source-paths)
         class-node-candidates)))
      class-node
    (error "Coverage file contains no information for file ‘%s’" file-name)))

(defun python-coverage--get-source-paths (tree)
  "Get the source paths from the TREE."
  (->> (xml+-query-all tree '((coverage) > (sources) > (source)))
       (-map 'xml+-node-text)))

(defun python-coverage--class-node-matches-file-name? (class-node file-name source-paths)
  "Check whether CLASS-NODE is about FILE-NAME.

This tries all SOURCE-PATHS and compares that to FILE-NAME."
  ;; The ‘filename=...’ attribute contains a relative file name
  ;; starting at any of the source directories.
  (-let [relative-file-name
         (or (xml-get-attribute-or-nil class-node 'filename)
             (error "<class> node does not have a ‘filename’ attribute"))]
    (->> source-paths
         (-map 'file-name-as-directory)
         (--map (s-concat it relative-file-name))
         (member file-name))))

(defun python-coverage--extract-lines (class-node)
  "Extract info about lines that are not fully covered from CLASS-NODE."
  (->> (xml+-query-all class-node '((class) > (lines) > (line)))
       (-map 'python-coverage--transform-line-node)
       (-map 'python-coverage--merge-adjacent)
       (--remove (eq (plist-get it :status) 'covered))
       (-sort (-on '< (-rpartial 'plist-get :line-beg)))))

(defun python-coverage--transform-line-node (line-node)
  "Transform a LINE-NODE (‘<line ...>’) into a simple structure."
  (let* ((line
          (-> line-node
              (xml-get-attribute 'number)
              (string-to-number)))
         (missing?
          (-> line-node
              (xml-get-attribute 'hits)
              (s-equals? "0")))
         (missing-branches
          (-> line-node
              (xml-get-attribute-or-nil 'missing-branches)))
         (status
          (cond (missing? 'missing)
                (missing-branches 'partial)
                (t 'covered)))
         (result
          (append
           (list :line-beg line :line-end line :status status)
           (-some->> missing-branches (list :missing-branches)))))
    result))

(defun python-coverage--merge-adjacent (coverage-info)
  "Merge adjacent lines into blocks in COVERAGE-INFO."
  ;; todo
  coverage-info)

;; Internal helpers for overlays

(defun python-coverage--overlay-make-all (coverage-info)
  "Create all overlays for COVERAGE-INFO."
  (let ((previous-overlay))
    (--each (-zip (cons nil coverage-info) coverage-info)
      (-let* (((previous . current) it)
              (overlay (python-coverage--overlay-make current))))))
  ;; (-each coverage-info 'python-coverage--overlay-make)
  )

;; (-zip (cons nil '(1 2 3)) '(1 2 3))
;; (cdr (cons 1 2))

(defun python-coverage--overlay-make (info)
  "Make an overlay for coverage INFO."
  (save-restriction
    (widen)
    (-let* (((&plist :line-beg :status) info)
            (beg
             (save-excursion
               (goto-char (point-min))
               (--dotimes (1- line-beg)
                 (forward-line))
               (point)))
            (end
             (save-excursion
               (widen)
               (goto-char beg)
               (python-nav-end-of-statement)
               (1+ (point))))
            (face (pcase status
                    ('missing 'magit-diff-removed)
                    ('partial 'magit-diff-base)))
            (overlay
             (-doto (make-overlay beg end)
               (overlay-put 'evaporate t)
               (overlay-put 'category 'python-coverage)
               (overlay-put 'face face))))
      overlay)))

(defun python-coverage--overlays-in (&optional beg end)
  "Return all overlays between BEG and END."
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (save-restriction
    (widen)
    (->> (overlays-in (point-min) (point-max))
         (--filter (eq (overlay-get it 'category) 'python-coverage))
         (--filter (>= (overlay-start it) beg))
         (--filter (<= (overlay-end it) end))
         (-sort (-on '< 'overlay-start)))))

(defun python-coverage--overlay-install-timer ()
  "Install the timer to refresh overlays."
  (setq python-coverage--overlay-timer
        (run-at-time
         nil python-coverage-overlay-refresh-interval
         #'python-coverage-overlay-refresh)))

(defun python-coverage--overlay-cancel-timer ()
  "Cancel the timer."
  (-some-> python-coverage--overlay-timer
           (cancel-timer))
  (setq python-coverage--overlay-timer nil))

;; Internal helpers for flycheck

(declare-function flycheck-define-generic-checker "flycheck")
(declare-function flycheck-error-new-at "flycheck")
(declare-function flycheck-verification-result-new "flycheck")
(defvar flycheck-checkers)

(defun python-coverage--flycheck-predicate ()
  "Check whether the flycheck checker can be used."
  (condition-case nil
      (python-coverage--find-coverage-file-current-buffer)
    (error nil)))

(defun python-coverage--flycheck-verify (_checker)
  "Check whether the flycheck checker can be used."
  (list
   (condition-case err
       (let ((coverage-file (python-coverage--find-coverage-file-current-buffer)))
         (flycheck-verification-result-new
          :label "report"
          :message (format "Found at %s" coverage-file)
          :face 'success))
     (error
      (flycheck-verification-result-new
       :label "report"
       :message (format "%s" (error-message-string err))
       :face 'error)))))

(defun python-coverage--flycheck-error (info checker)
  "Create a flycheck error for CHECKER containing INFO ."
  (-let* (((&plist :line-beg :status :missing-branches) info)
          (column 1)
          (level
           (pcase status
             ('missing 'error)
             ('partial 'warning)))
          (message
           (pcase status
             ('missing "Not covered")
             ('partial
              (s-concat "Partially covered" (-some->> missing-branches (format " (missing branches: %s)")))))))
    (flycheck-error-new-at line-beg column level message :checker checker)))

(defun python-coverage--flycheck-start (checker callback)
  "Flycheck start function for CHECKER, invoking CALLBACK."
  (condition-case err
      (let* ((context)
             (coverage-info (python-coverage-current-buffer))
             (errors
              (--map
               (python-coverage--flycheck-error it checker)
               coverage-info)))
        (prog1 context
          (funcall callback 'finished errors)))
    (error
     (funcall callback 'errored (error-message-string err))
     (signal (car err) (cdr err)))))

(with-eval-after-load 'flycheck
  (flycheck-define-generic-checker 'python-coverage
    "A Python checker to show coverage results using a XML report."
    :modes '(python-mode)
    :start #'python-coverage--flycheck-start
    :predicate #'python-coverage--flycheck-predicate
    :verify #'python-coverage--flycheck-verify)

  (add-to-list 'flycheck-checkers 'python-coverage t))

(provide 'python-coverage)
;;; python-coverage.el ends here
