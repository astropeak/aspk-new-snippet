
(defvar pns-snippet-dir (expand-file-name "./snippets") "The directory for all snipppet files")
(setq pns-template-set (make-hash-table :test 'equal))
(setq pns-filename-template-tree-map (make-hash-table :test 'equal))
;; This hash save a template's parent and children elements. Key is a template element, value is a assoc list: ((parent . parent-element)(children . list-of-children-element))
(setq pns-template-parent-children-map (make-hash-table :test 'eq))
(setq pns-org-capture-template
      '("-" "PNS" entry
        (file+headline
         (concat pns-snippet-dir "/all-mode/inbox.org")
         "Captured")
        (function pns-create-a-snippet)
        :empty-lines-before 1
        )) 

(defun pns-assoc (key list)
  (cdr (assoc key list)))

(defun pns-set-assoc (key value list)
  "set key value to new value in assoc list, destructively"
  ;; (message "key: %S, list: %S" key list)
  (setcdr (assoc key list) value)
  )
;; (setq a '((name . "TOM") (age . 13)))
;; (pns-assoc 'name a)
;; (pns-set-assoc 'age 18 a)
(defun pns-print-template-tree (tree &optional height)
  "This print a tree in a nice format"
  (or height (setq height 0))
  (let ((msg-func (lambda (info &rest args)
                    (apply 'message (cons (format "%s%s" (make-string height ? ) info) args))))
        tmp)

    (funcall msg-func "--------------------")
    (mapc (lambda (field)
            (when (pns-assoc field tree)
              (setq tmp (pns-assoc field tree))
              (when (stringp tmp) (setq tmp (replace-regexp-in-string "\n" "\\\\n" tmp)))
              (funcall msg-func "%s: %S" field tmp)
              ))
          '(name hungry-vars var-table precondition-table text))

    (mapc (lambda (tt)
            (pns-print-template-tree tt (+ 4 height)))
          (pns-assoc 'children tree)))
  nil)

(defun pns-get-current-mode ()
  "get current src block mode name under cursor in a org mode file buffer. If not applicatable, return nil"
  (or 
   (let (info lang)
     (when (and (eq major-mode 'org-mode) (fboundp 'org-edit-src-find-region-and-lang))
       (setq info (org-edit-src-find-region-and-lang)))
     (when info
       (setq lang (or (cdr (assoc (nth 2 info) org-src-lang-modes)) (nth 2 info)))
       (setq lang (if (symbolp lang) (symbol-name lang) lang)))
     lang)
   (replace-regexp-in-string "-mode$" "" (format "%s"  major-mode))))
(defun pns-get-all-src-blocks (snippet &optional entry name)
  (pns-src-block-filter
   (pns-get-all-src-blocks-coolie snippet entry name)))

(defun pns-get-all-src-blocks-coolie (snippet &optional entry name)
  "Get all src blocks of the given snippet file name, return as a list. All src blocks should be in a entry, or there will be error. If the entry is provided, then only get that entry's src blocks. entry is regexp to match a headline. If name is given, only get blocks with NAME."
  (let (tmp rst)
    ;; create temp buffer without undo record or font lock. (more efficient)
    ;; first space in temp buff name is necessary
    (and (get-buffer " myTemp") (kill-buffer " myTemp"))
    (set-buffer (get-buffer-create " myTemp"))
    (insert-file-contents snippet nil nil nil t)

    ;; get only the entry's content
    (when entry
      (goto-char 1)
      (if (re-search-forward entry nil t)
          (let ((heading (match-string 0))
                (entry (org-get-entry)))
            (erase-buffer)
            (insert heading)
            (insert "\n")
            (insert entry))
        (error "Entry %S not found, file: %s, src block name: %s." entry snippet name)))

    ;; (write-file "aaa")
    (goto-char 1)
    (while (setq tmp (pns-get-src-block-info name))
      (push `(filename . ,snippet) tmp) ;; add file name.
      (push tmp rst))
    (prog1 (reverse rst)
      (kill-buffer " myTemp"))))
(defun pns-get-src-block-info (&optional name)
  "Get one src block after current positon in current buffer, return as a accoc list of block info. If no src blocks, return nil, and position will not be moved. If NAME is given, then only get a src block with that NAME. NAME is a regular expression that must contain a match pair \\(\\), to let the re searching get the result.
        The codes are copied form org-babel-find-named-block .
    "
  ;; Return value example: ((tag . ":not-a-snippet :noweb") (beginning . 179) (end . 228) (name . "  ") (lang . "elisp") (content . "  \"^\\\\\\\\*+[ \\t]+\"\n") (level . 1) (heading . "regexp value: org mode headline/title (** a title)"))
  (interactive)
  ;; (or name (setq name "\\(.*\\)"))
  (let* ((case-fold-search t)
         (regexp (if name (concat org-babel-src-name-regexp name "[ \t(]*[\r\n]\\(?:^#.*[\r\n]\\)*"
                                  (substring org-babel-src-block-regexp 1))
                   org-babel-src-block-regexp)))

    (when (re-search-forward regexp nil t)
      (append
       ;; position: beginning and end
       (list (cons 'beginning (match-beginning 0))
             (cons 'end (match-end 0)))

       ;; src block name, lang, tag, content
       (mapcar (lambda (elem)
                 (let ((idx (car elem)) (property-name (cdr elem)))
                   (cons property-name
                         (if (equal property-name 'content)
                             (pns-post-process-src-block-content
                              (substring-no-properties (or (match-string idx) "")))
                           (substring-no-properties (or (match-string idx) ""))))))
               (if name
                   '((1 . name) (3 . lang) (5 . tag) (6 . content))
                 '((1 . name) (2 . lang) (4 . tag) (5 . content))))

       ;; belonging heading and level
       (save-excursion
         (let ((p (point)))
           (org-back-to-heading)
           (move-beginning-of-line 1)
           (if (re-search-forward "^\\(\\*+\\)\s+\\(.*\\)" p t)
               (list (cons 'level (length (match-string 1)))
                     (cons 'heading (substring-no-properties (match-string 2)))))))))))

;; there is a bug in regexp-quote: '\\|' will be quoted to '\\\\|'

(defun pns-post-process-src-block-content (content)
  (pns-remove-leading-spaces 
   (pns-remove-src-block-last-new-line-char 
    (pns-unquote-src-block-content content)))
  )

(defun pns-unquote-src-block-content (content)
  "Remove all leading ',' in each line of content"
  (replace-regexp-in-string "^\\(\s*\\)," "\\1" content))

(defun pns-remove-src-block-last-new-line-char (content)
  "Remove the last new line char in content. Because when using it as a snippet, it always has a new line char at the end which is not needed"
  (string-remove-suffix "\n" content))

(defun pns-remove-leading-spaces (content)
  "Remove unneeded leading spaces in each line of CONTENT"
  (let ((unneeded-spaces 
         ;; (when (not (equal (string-match "^\s*" content) 0))
         ;;   (match-string 0))
         (with-temp-buffer (insert content) (goto-char 1)
                           (skip-chars-forward " \t\n")
                           (beginning-of-line)
                           (when (re-search-forward "^\s+" (save-excursion (end-of-line) (point)) t)
                             (match-string 0)))
         ))
    (if unneeded-spaces
        (replace-regexp-in-string (format "^%s" unneeded-spaces) "" content)
      content)))

(defun pns-src-block-filter (blocks)
  "Filter src blocks.  Remove the blocks whose tag contains ':not-a-snippet. This is a way to indicate a src block is not used as a snippet"
  (-filter (lambda (x) (not (s-contains? ":not-a-snippet" (pns-assoc 'tag x))))
           blocks))
(defun pns-create-template-new (file)
  "create template(which is a tree) given FILE, which is an org mode template file"
  (let ((tree (pns-convert-input-tree-to-template-tree (pns-convert-block-list-to-tree (pns-get-all-src-blocks file)))))
    tree))
(require 'aspk-tree)

(defun pns-convert-block-list-to-tree (blocks)
  (let ((pns-convert-block-list-tree (aspk/tree-create))
        (pns-tobe-converted-block-list blocks))
    (pns-convert-block-list-to-tree-coolie)
    ;; replace the head element
    (setcar pns-convert-block-list-tree '((heading . "root") (level . 0) (content . "")))
    pns-convert-block-list-tree))


(defun pns-convert-block-list-to-tree-coolie (&optional parent)
  "Use two gloabl variables. pns-tobe-converted-block-list: the src blocks; pns-convert-block-list-tree the output tree"
  (let (level  parent-level (run-p t) elem)
    (or parent (setq parent aspk/tree-head-element))
    (setq parent-level (and (listp parent) (pns-assoc 'level parent)))
    (or parent-level (setq parent-level 0))

    (while (and run-p pns-tobe-converted-block-list)
      (setq elem (car pns-tobe-converted-block-list))
      (setq level (pns-assoc 'level elem))
      ;; (message "level:%d, parent-level:%d" level parent-level)
      (cond ((> level parent-level)
             ;; (message "level: %d, heading: %s" level (pns-assoc 'heading elem))
             (pop pns-tobe-converted-block-list)
             (aspk/tree-add-element pns-convert-block-list-tree parent elem)
             (pns-convert-block-list-to-tree-coolie elem))
            (t (setq run-p nil)))
      )))


(defun pns-convert-input-tree-to-template-tree (input-tree)
  "Convert the input-tree destructively to a template tree"
  (aspk/tree-iterate
   input-tree
   (lambda (elem-subtree parent-subtree depth)
     (setcar elem-subtree
             (append
              (list (cons 'name (pns-assoc 'heading (car elem-subtree))))
              (list (cons 'mode (pns-assoc 'lang (car elem-subtree))))
              (list (cons 'beginning (pns-assoc 'beginning (car elem-subtree))))
              (list (cons 'end (pns-assoc 'end (car elem-subtree))))
              (list (cons 'filename (pns-assoc 'filename (car elem-subtree))))
              (pns-src-parser (pns-assoc 'content (car elem-subtree)))))))
  input-tree)

(defun pns-create-template-table (&optional mode)
  "Create a name to template object table(list of cons). Key is name, value is list of (template filename). this table can be used as helm candidates. MODE is a regexp to match the mode(if nil the for all modes)"
  (or mode (setq mode ".*"))
  (let (rst)
    (maphash (lambda (filename val)
               ;; Updated: don't use file mode to check it. but (DEMO VERSION!) use snippet mode
               ;; (when (and  (stringp (pns-assoc 'mode val))
               ;; (string-match mode (pns-assoc 'mode val)))
               (aspk/tree-iterate-element-value
                (pns-assoc 'template val)
                (lambda (elem)
                  (when (and (not (equal "root" (pns-assoc 'name elem)))
                             (and (stringp (pns-assoc 'mode elem))
                                  (string-match mode (pns-assoc 'mode elem))))
                    (push (cons (pns-assoc 'name elem) elem) rst)
                    )))
               ;; )
               )
             pns-template-set)
    rst))
;; src = annotation + text-src
;; text = replace `var value` syntax with `var` in text-src
(defun pns-src-parser (src)
  "Convert src, which is the src block's content, to a assoc list of some fields of templates: var-table, hungry-vars, precondition-table, text"
  ;; (message "src: %s" src)
  (let (hungry-vars hungry-vars1 var-table var-table1 precondition-table text)
    (multiple-value-setq
        ;; annotation is a list of (annotaton-name value), the same part after @.
        ;; text-src is the remaining src, where all `var value` syntax will be inspected.
        (annotations text-src)
      (pns-src-parser--consume-annotation src))

    (multiple-value-setq
        ;; hungry-vars is the vars of get annotation
        ;; precondition-table is the vars of if annotation
        (hungry-vars precondition-table var-table)
      (pns-src-parser--parse-annotation annotations))

    (multiple-value-setq
        (hungry-vars1 var-table1 text)
      (pns-src-parser--parse-text text-src))

    `((var-table . ,(append var-table var-table1))
      (hungry-vars . ,(append hungry-vars hungry-vars1))
      (precondition-table . ,precondition-table)
      (text . ,text))))

(defun pns-src-parser--consume-annotation (src)
  (let (rst text-src (last-point 1))
    (condition-case *error-info*
        (progn
          (and (get-buffer " myTemp") (kill-buffer " myTemp"))
          (set-buffer (get-buffer-create " myTemp"))
          (insert src))
      (error
       (message "Error happened: %S" *error-info*)
       (debug *error-info*)
       (list "Error: Fail to get snippet name. Error info below" *error-info*)))

    (goto-char (point-min))
    (while (re-search-forward "^\s*[^\\]?@\\((.*)\\)$" (point-max) t)
      (match-string 1)
      ;; here (match-string 1) is the matched text by first (), add processing codes here
      ;; (message "Matched string %S" (match-string 1))
      (add-to-list 'rst (read (match-string 1)) 'append)
      (setq last-point (match-end 0))
      )
    (setq text-src (buffer-substring-no-properties last-point (point-max)))
    (kill-buffer " myTemp")
    (list rst text-src)))


(defun pns-src-parser--parse-annotation (annotations)
  (let (hungry-vars precondition-table var-table)
    (mapc
     (lambda (elem)
       ;; (message "elem=%s" elem)
       (cond ((equal (car elem) 'get)
              (setq hungry-vars (append hungry-vars (cdr elem))))
             ((equal (car elem) 'if)
              (setq precondition-table (append precondition-table (cdr elem))))
             ((equal (car elem) 'vars)
              (setq var-table (append var-table (cdr elem))))
             (t (error "Unsupported annotation: %S" elem))))
     annotations
     )
    (list hungry-vars precondition-table var-table)))

(defun pns-src-parser--parse-text (text-src)
  (let (hungry-vars var-table rst text tmp
                    (random-string ;; create a number string with length 16
                     (replace-regexp-in-string " " "0" (format "%16s" (random 10000000000000000)))))
    (condition-case *error-info*
        (progn
          (and (get-buffer " myTemp") (kill-buffer " myTemp"))
          (set-buffer (get-buffer-create " myTemp"))
          (insert text-src))
      (error
       (message "Error happened: %S" *error-info*)
       (debug *error-info*)
       (list "Error: Fail to get snippet name. Error info below" *error-info*)))

    (goto-char (point-min))
    ;;ensure the randome-string not exists
    (while (re-search-forward random-string (point-max) t)
      (setq random-string (replace-regexp-in-string " " "0" (format "%16s" (random 10000000000000000)))))

    (goto-char (point-min))
    ;; replace all '\`' to random-string temporarily, after process all `` blocks, we will replace back.
    (while (re-search-forward "\\\\`" (point-max) t)
      (replace-match random-string))

    (goto-char (point-min))
    (while (re-search-forward "`\\([^`]+\\)`" (point-max) t)
      ;; here (match-string 1) is the matched text by first (), add processing codes here
      ;; (message "Matched string %S" (match-string 1))
      ;; for syntax like `(buffer-file-name)`, that is, the first non blank char is '(', then don't interated it as a hungry-var, instead, leave (DEMO VERSION!) it as it, it will be an embed elisp code that will be passed to yas directly
      (unless (string-match-p "\s*(" (match-string 1))
        (setq tmp (read (format "(%s)" (match-string 1))))
        (add-to-list 'rst tmp 'append)
        (replace-match (format "`%s`" (car tmp))))
      )

    (goto-char (point-min))
    ;; replace back
    (while (re-search-forward random-string  (point-max) t)
      (replace-match "\\\\`"))

    (setq text (buffer-substring-no-properties (point-min) (point-max)))
    (kill-buffer " myTemp")
    (setq hungry-vars (mapcar (lambda (x) (car x)) rst))
    (setq var-table (delete nil
                            (mapcar (lambda (x)
                                      (when (> (length x) 1)
                                        x)) rst)))
    (list hungry-vars var-table text)))
(defun pns-get-var-table (template-element)
  "Get the var table for the template, with parent's var table mergetd. Seem we should get all parent's var table"
  (when template-element
    (reverse
     (append (pns-get-var-table (pns-get-template-element-parent template-element))
             (pns-assoc 'var-table template-element)))))

(defun pns-get-hungry-values (template &optional filled-vars)
  "Get hungry for a template. Return a list of list of varname and value, (var-symbol value)"
  (save-excursion
    (let ((hungry-vars (pns-assoc 'hungry-vars template))
          (var-table (pns-get-var-table template))
          possiable-values)
      (mapcar (lambda (var)
                ;; change (read "Input") to (read . "Input")
                (setq possiable-values (mapcar (lambda (elem)
                                                 (if (listp elem)
                                                     (cons (nth 0 elem) (nth 1 elem))
                                                   elem))
                                               (pns-assoc var var-table)))
                ;; (message "var: %S, possiable-values: %S" var possiable-values)
                (message "## Getting value for hungry var %s" var)
                (list var
                      (cond
                       ;; first search in filled-vars
                       ((assoc var filled-vars) (nth 1 (assoc var filled-vars)))
                       ;; get from helm
                       ((> (length possiable-values) 1)
                        (helm-other-buffer
                         `(((name . ,(format "%s" var))
                            (candidates . ,possiable-values)
                            (action . (lambda (c ) c))
                            (accept-empty . t)))
                         (pns-assoc 'name template)))
                       ;; get by read-string
                       ((>= (length possiable-values) 0)
                        (save-excursion (read-string (format "%s. %s[%s]: "
                                                             (pns-assoc 'name template)
                                                             var (or (car possiable-values) "")) nil nil (car possiable-values))))
                       (t (error "Impossible to be here")))))
              hungry-vars))))


(defun pns-get-real-value-for-var (var-name display-value template)
  "Get real value for var with display-value"
  (let* ((var-table (pns-get-var-table template))
         (var-values (pns-assoc var-name var-table))
         (rst display-value))
    (mapc (lambda (value)
            (when (and (listp value) (equal display-value (car value)))
              (setq rst (nth 1 value))))
          var-values)
    (message "Get real value for var $S with display-value %S: %S" var-name display-value rst)
    rst))


(setq pns-template-file (expand-file-name "./snippets/java-mode/file-processing.org"))
(defun pns-clear-tempate ()
  (interactive)
  (setq pns-template-set (make-hash-table :test 'equal)))
(defun pns-update-template-list ()
  "Create/update template for all files under pns-snippet-dir, the result saved in pns-template-set"
  ;; iterate all file names under directory dir with an optional filter function, recursively
  (interactive)
  (require 'f)
  (let (rfile elem time elem2 tmpl mode)
    (mapc (lambda (file)
            (setq rfile (replace-regexp-in-string pns-snippet-dir "" file))
            (and (equal file rfile) (error "Impossiable"))
            (setq elem (gethash rfile pns-template-set))
            (setq time (time-to-seconds (nth 5 (file-attributes file 'string))))
            ;; (message "File: %s, time: %d" file time)
            ;; (or mode (error "Mode is nil for file %s" file))

            (unless (and elem (<= time (pns-assoc 'update-time elem)))
              (message "Updating template for file %s" rfile)
              (setq tmpl (pns-create-template-new file))
              (with-temp-buffer (insert file) (goto-char (point-min))
                                (when (re-search-forward "/\\([a-z-]*\\)-mode/" nil t)
                                  (setq mode (match-string 1))))
              (setq elem2 (list (cons 'update-time time)
                                (cons 'template tmpl)
                                (cons 'mode mode)))
              (pns-upadte-template-parent-children-map tmpl)
              (puthash rfile elem2 pns-template-set)))
          (f-files pns-snippet-dir nil t))))
(defun pns-upadte-template-parent-children-map (template)
  "update the map in pns-template-parent-children-map of template tree"
  (aspk/tree-iterate
   template
   (lambda (elem-subtree parent-subtree depth)
     (puthash (car elem-subtree)
              (list (cons 'parent (car parent-subtree))
                    (cons 'children (mapcar 'car (cdr elem-subtree))))
              pns-template-parent-children-map))))

(defun pns-get-template-element-parent (template-element)
  (let (rst)
    (setq rst (gethash template-element pns-template-parent-children-map))
    (or rst (error "value not exist for template-element %S" template-element))
    (pns-assoc 'parent rst)))

(defun pns-get-template-element-children (template-element)
  (let (rst)
    (setq rst (gethash template-element pns-template-parent-children-map))
    (or rst (error "value not exist for template-element %S" template-element))
    (pns-assoc 'children rst)))

;; (setq pns-template-set (make-hash-table :test 'equal))

;; (pns-update-template-list)

(defun pns-expand-template-by-name ()
  "Use helm select which template to be expanded, based on its name. The input is a global variable pns-template-file saving the template file name."
  (interactive)

  (save-excursion (pns-update-template-list))
  ;; (message "in 1 pns-expand-template-by-name Buffer:%S" (current-buffer))
  (helm-other-buffer
   `(((name . ,(format "Template in %s mode" (pns-get-current-mode)))
      (candidates . ,(pns-create-template-table (pns-get-current-mode)))
      (action . (("expand" . (lambda (template) (pns-expand-tempalte template)))
                 ("open" . (lambda (template)
                             (pns-open-template template)))))
      (accept-empty . t)))
   "Template"))


(defun pns-expand-tempalte (template-element &optional filled-vars caller)
  "Expand a template-element, recursively.
`caller' is a token indicate the caller of this function. Its value is 'parent or 'user. When the value is 'user, then will not epand children. When the value is 'parent, children will also be expanded. The default value is 'parent.
But I think now rename this parameter to `recursivep' is better, easier to understand the parameter's purpose.

`filled-vars' is the env variable.
"
  (interactive)
  ;; (message "in pns-expand-tempalte Buffer:%S" (current-buffer))
  (or caller
      (setq caller (if filled-vars 'parent 'user)))

  (let (hungry-vars text (check-passed t) (var-table (pns-get-var-table template-element)))
    ;; (pns-print-template-tree template-element)
    ;; (message "Filled-Vars: %S" filled-vars)
    ;; (message "Expanding %s" (pns-assoc 'name template-element))

    ;; check precondition
    (when (equal caller 'parent)
      (mapc (lambda (var-value)
              (let* ((name (car var-value))
                     (expected-values
                      (mapcar (lambda (elem)
                                ;; elem is the display value symbol for var name.
                                ;; convert from display to real
                                ;;convert symbol to string             (ref:ii)
                                (format "%s" (pns-get-real-value-for-var name elem template-element))
                                )
                              (cdr var-value)))
                     (real (car (pns-assoc name filled-vars))))
                (unless (member real expected-values)
                  (setq check-passed nil)
                  (message "Precondition checking not pass for name: %s. Expected: %s, real: %s"
                           name expected-values real))))
            (pns-assoc 'precondition-table template-element)))

    (when check-passed
      ;; (message "Template-Element: %S, buffer: %S" template-element (current-buffer))
      (setq text (pns-assoc 'text template-element))
      (setq hungry-vars (pns-get-hungry-values template-element filled-vars))
      (when (equal (pns-assoc 'mode template-element) "python")
        (setq hungry-vars (cons '(yas-indent-line 'fixed) hungry-vars)))
      ;; (message "%S, %S, %d, buffer: %S" text hungry-vars (point) (current-buffer))
      ;; expand this
      (if (region-active-p)
          (yas-expand-snippet  text (region-beginning) (region-end) hungry-vars)
        (yas-expand-snippet  text  (point) (point) hungry-vars))

      ;; expand all children
      (mapc (lambda (tmpl)
              (pns-expand-tempalte tmpl hungry-vars 'parent))
            (pns-get-template-element-children template-element))))
  ;; (pns-assoc 'children template-element))))
  )

(defun pns-open-template (template)
  "Open the TEMPLATE, which is defined in FILENAME"
  (let ((beginning (pns-assoc 'beginning template))
        (filename (pns-assoc 'filename template)))
    (find-file filename)
    (goto-char beginning)))

(defun pns-create-a-snippet ()
  (let ((lang (pns-get-current-mode))
        (region-str (if mark-active (buffer-substring-no-properties (region-beginning) (region-end)) "")))
    (format "* %%^{Title}\n  %%U\n  #+begin_src %s\n%s%%?\n  #+end_src\n  %%F\n\n"
            lang
            (if (equal region-str "")
                "  "
              ;; delete empty lines in beginning and end of region-str
              (setq region-str (replace-regexp-in-string "^[ \t\n]*\\(.*\\)[ \t\n]*$" "\\1" region-str))
              (pns-indent-src-code-string region-str lang 2)))))
(defun pns-indent-src-code-string (code-str lang nindent) 
  "CODE-STR is the src code, LANG is like emacs-lisp, NINDENT is the number of spaces be put at the begining of each line"
  (with-temp-buffer
    (insert code-str)
    (let ((indent (make-string nindent ?\ ))
          (mode-func (intern (concat lang "-mode"))))
      (funcall mode-func)
      (indent-buffer)
      (goto-char (point-min))
      (while (re-search-forward "\\(^\\).+" nil t)
        (replace-match indent nil nil nil 1)))
    (buffer-string)))
(defun pns-tool-convert-aspk-code-snippets-to-new (dir output-file mode)
  "Convert all snippets(native style) in DIR to a single org mode file OUTPUT-FILE. each file becomes an entry"
  (with-current-buffer (find-file-noselect output-file)
    (erase-buffer))
  (let (name value)
    (mapc (lambda (file)
            ;; (message "file=%s" file)
            (condition-case *error-info*
                (progn
                  (and (get-buffer " myTemp") (kill-buffer " myTemp"))
                  (set-buffer (get-buffer-create " myTemp"))
                  (insert-file-contents file nil nil nil t))
              (error
               (message "Error happened: %S" *error-info*)
               (debug *error-info*)
               (list "Error: Fail to get snippet name. Error info below" *error-info*)))

            (goto-char (point-min))
            ;; (re-search-forward "^\s*#\s*name\s*:\s*\\(.*\\)" (point-max))
            (when (re-search-forward "^\s*#\s*name\s*:\s*\\(.*\\)"(point-max) t)
              (match-string 1)
              ;; here (match-string 1) is the matched text by first (), add processing codes here
              (message "Matched string %s" (match-string 1))
              (setq name (match-string 1))
              (when (re-search-forward "^\s*#\s*--\s*\n"(point-max) t)
                (setq value (buffer-substring-no-properties (match-end 0) (point-max))))
              ;; replace ` with \`
              (setq value (replace-regexp-in-string "`" "\\`" value t t))
              (with-current-buffer (find-file-noselect output-file)
                ;; (erase-buffer)
                (goto-char (point-max))
                ;; (insert (format "\n* %s\n#+name:content\n#+begin_src java\n%s\n#+end_src" name value))
                (insert (format "\n* %s\n#+begin_src %s\n%s\n#+end_src\n" name mode value))
                (save-buffer)))
            (kill-buffer " myTemp"))
          (f-files dir nil t))))


;; test:
(defun pns-tool-convert-all ()
  (interactive)
  (mapc (lambda (mode)
          (let ((file (expand-file-name (format    "%s/%s-mode/collection_of_old_snippet.org" pns-snippet-dir mode)))
                (dir (expand-file-name (format "%s/%s-mode" pcs-snippet-dir mode))))
            ;; (when (file-exists-p file)
            ;;   (delete-file file))
            (when (file-exists-p dir)
              (pns-tool-convert-aspk-code-snippets-to-new dir file mode))))
        pcs--mode-names))

;; "/Users/astropeak/Dropbox/project/aspk-new-snippet/snippets/java-mode/file-processing.org"
;; (setq pns-template-file "/tmp/aa.org")

;; then run pns-expand-template-by-name



(add-to-list 'org-capture-templates pns-org-capture-template)

(provide 'pns)
