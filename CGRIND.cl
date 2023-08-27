(load "tree-lib.cl")

(defvar *verify* nil)
(defvar *debug* '(pass bind-hreg connect-hreg))
(defvar *optimize* '(auto-reg split-reg jump cse loop delete-zombi peep-stld peep-convzx816 peep-loadconst peep-merge816 v810-peephole))

(extern *cfun-name*)

(push 'confirm-move-optimize-in-fixup-hreg *debug*)

(setq *gc-control* 200000 *notify-gc* nil)
(defun cgrind-bblock (bblockh)
  (if *debug*
      (cgrind-bblock-debug bblockh)
      (progn
        (if (or *debug* *verify*)
            (format t "Compiling %T ...\n" *cfun-name*))
        (reduce-bblock-expr bblockh)
        (when (memq 'jump *optimize*)
          (reduce-jump bblockh))
        
        (when (memq 'split-reg *optimize*)
          (split-reg bblockh))
        (when (memq 'cse *optimize*)
          (reduce-cse bblockh)
          (reduce-bblock-expr bblockh))
        (arrange-for-md bblockh)
        (arrange-for-md-v810 bblockh)
        (dflow-life bblockh)
        (tiling-bblock-ccode bblockh)
        (bind-hreg bblockh)
        (scheduling-bblock-ccode bblockh)
        (connect-hreg bblockh)
        (fixup-hreg bblockh)
        (gct)
        (generate-mcode bblockh)
        (reduce-mcode bblockh)
        (generate-acode bblockh))))


(defun debug-break (key)
    (if (memq key *debug*)
        (break (symbol-name key))))

(defun cgrind-bblock-debug (bblockh)
  (format t "Compiling %T ...\n" *cfun-name*)
  (debug-break 'break-bblock)
  
  (reduce-bblock-expr bblockh)
  (debug-break 'break-reduce-bblock-expr)
  
  (when (memq 'jump *optimize*)
    (reduce-jump bblockh)
    (debug-break 'break-reduce-jump))
    
  (when (memq 'split-reg *optimize*)
    (split-reg bblockh)
    (debug-break 'break-split-reg))
    
  (when (memq 'cse *optimize*)
    (debug-break 'break-reduce-cse-before)
    (reduce-cse bblockh)
    (reduce-bblock-expr bblockh)
    (debug-break 'break-reduce-cse))
    
  (debug-break 'break-mip-optimize)
  
  (arrange-for-md bblockh)
  (arrange-for-md-v810 bblockh)
  (debug-break 'break-arrange-for-md)
  
  (dflow-life bblockh)
  (debug-break 'break-dflow-life)
  
  (tiling-bblock-ccode bblockh)
  (debug-break 'break-tiling-bblock-ccode)
  
  (bind-hreg bblockh)
  (debug-break 'break-bind-hreg)
  
  (scheduling-bblock-ccode bblockh)
  (debug-break 'break-scheduling-bblock-ccode)
  
  (connect-hreg bblockh)
  (debug-break 'break-connect-hreg)
  
  (fixup-hreg bblockh)
  (gct)
  (debug-break 'break-fixup-hreg)
  
  (generate-mcode bblockh)
  (debug-break 'break-generate-mcode)
  
  (reduce-mcode bblockh)
  (debug-break 'break-reduce-mcode)
  
  (generate-acode bblockh))

(defun disable-tiles (names)
    (dolist (name names) (put name 'disabled t)))

(defun enable-tiles (names)
    (dolist (name names) (put name 'disabled nil)))

(extern *mdl-russian-mult-tiles*)

(defun disable-russian-mult nil
    (disable-tiles *mdl-russian-mult-tiles*))

(defun enable-russian-mult nil
    (enable-tiles *mdl-russian-mult-tiles*))

(defun Nsplit-reg nil
    (setq *optimize* (delq 'split-reg *optimize*)))

(defun Nxmul nil
    (disable-russian-mult))

(defun Ncse nil
    (setq *optimize* (delq 'cse *optimize*)))

(defun Nloop nil
    (setq *optimize* (delq 'loop *optimize*)))

(defun Nzombi nil
    (setq *optimize* (delq 'delete-zombi *optimize*)))

(defun Npeep nil
    (dolist (newp '(peep-stld peep-convzx816 peep-loadconst peep-merge816)) (setq *optimize* (delq newp *optimize*))))

(defun Nall nil
    (setq *optimize* '(auto-reg stupid-auto-reg v810-peephole)))

(defun Oall nil
    (setq *optimize* '(auto-reg split-reg jump cse loop delete-zombi peep-stld peep-convzx816 peep-loadconst peep-merge816 v810-peephole)))

(defun test-cgrind (ccodef parser md &optional parseopt)
  (unless (boundp '*md-name*)
    (load-md md))
  
  (let ((pcodef (file-name-change-suffix ccodef ".cpp"))
        (scodef (file-name-change-suffix ccodef ".scode"))
        (acodef (file-name-change-suffix ccodef ".asm")))
    
    (and (zerop (system (format nil "juncpp %T %T" ccodef pcodef)))
         (zerop (system (format nil "%T %T %T %T" parser (or parseopt "") pcodef scodef)))
         
         (let ((saved-optimize (copy-list *optimize*))
               (saved-debug (copy-list *debug*)))
           
           (cgrind scodef acodef)
           
           (unless (and (equal *optimize* saved-optimize)
                        (equal *debug* saved-debug))
             (format t "*** WARNING: %s temporally changes some compile flags while compiling.\n" ccodef)
             
             (unless (equal *optimize* saved-optimize)
               (format t "*** *optimize* = %t\n" *optimize*))
             
             (unless (equal *debug* saved-debug)
               (format t "*** *debug* = %t\n" *debug*))
             
             (format t "*** These are restored to their previous values now.\n")
             (setq *optimize* saved-optimize))))))

(defun sfcc (ccodef)
    (test-cgrind ccodef "cparse-65816" "md-65816.cl" "-usefulenum"))

(defun vucc (ccodef &optional debug)
  (if debug
      (Nall))
  
  (test-cgrind ccodef "cparse-v810" "md-v810.cl"
               (format nil "-pedanticenum %s %s" (if debug "-g" "") "-gp 05008000")))

(defun fxcc (ccodef) (test-cgrind ccodef "cparse-fx" "md-fx.cl"))

(defun gbcc (ccodef) (test-cgrind ccodef "cparse-gb" "md-gb.cl" "-byteop"))

(defmacro do-bblock ((var bblockh) &rest body)
  (let ((qh (gensym)))
    (list 'let*
          (list (list qh (list 'bblock-next bblockh))
                (list var qh))
          (list* 'while
                 (list 'not (list 'eq var qh))
                 (append body (list (list 'setq var (list 'bblock-next var))))))))

(defmacro do-mcode ((var mcodeh) &rest body)
  (let ((qh (gensym)))
    (list 'let*
          (list (list qh (list 'mcodeh-next mcodeh))
                (list var qh))
          (list* 'while
                 (list 'not (list 'eq var qh))
                 (append body (list (list 'setq var (list 'mcodeh-next var))))))))

(load "gmain.cl")

(defun arrange-for-md-v810 (bblockh)
  (when *debug*
    (format t "=== arrange-for-md-v810 (bblockh)\n"))
  
  (do-bblock (b bblockh)
    (do-ccode (c (bblock-ccode b))
      (let ((expr (ccode-expr c)))
        (when expr
          (setq expr (arrange-for-md-v810-conv expr))
          (setq (ccode-expr c) expr)))))))


(defun arrange-for-md-v810-conv (expr)
  (let ((code (expr-car expr))
        (args (expr-args expr)))
    (cond ((and (eq code 'set)
                (eq (expr-car (cadr args)) 'convsi)
                (eq (expr-htype (cadr args)) 'i2))
           (setq (expr-htype (cadr args)) 'i4)
           (setq (cadr args) (make-expr 'convit 'i2 (list (cadr args))))
           expr)
          (t expr))))
