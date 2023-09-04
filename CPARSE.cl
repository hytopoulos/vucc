(load "tree-lib.cl")

(defun ident (name) (and (symbol-p name) (setq name (symbol-name name))) (let ((id (search-ident name nil))) (or id (progn (clerror "%T %d: Ident %T not found" (car (get-spos)) (cdr (get-spos)) name)))))

(defun putiprop (ident prop val) (setq (ident-alist ident) (cons (cons prop val) (ident-alist ident))))

(defun noregparam-fun (idns) (dolist (idn idns) (putiprop (ident idn) 'noregparam t)))
(defmacro noregparam (&rest idns) (list 'noregparam-fun (list 'quote idns)))
(defmacro asm (idn gen) (list 'putiprop (list 'ident (list 'quote idn)) ''asm (list 'quote gen)))
(setq *debug* nil)
(setq *gc-control* 200000)

(defun test-parser (ccodef ldf &optional debugger) (unless (boundp '*ld-name*) (load-ld ldf)) (let ((pcodef (file-name-change-suffix ccodef ".cpp")) (scodef (file-name-change-suffix ccodef ".scode"))) (and (zerop (system (format nil "juncpp %T %T" ccodef pcodef))) (cparse pcodef scodef debugger))))

(defun sfcp (ccodef &optional debugger) (test-parser ccodef "ld-65816.cl" debugger))

(defun vucp (ccodef &optional debugger) (test-parser ccodef "ld-v810.cl" debugger))

(defun fxcp (ccodef) (test-parser ccodef "ld-fx.cl"))

(load "pmain.cl")
