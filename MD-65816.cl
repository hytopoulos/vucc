(setq *md-name* "md-65816")
(setq *md-version* "0.1")

#|
;; 65816 CALLING CONVENTION and REGISTER ALLOCATION
;;    * First argument is passed in A, second in X, third in Y
;;    * The rest of the arguments are passed on zero-page stack (where?)
;;    * $02-$0D are general purpose registers, mainly used for direct access instructions
;;    * Functions which use the zero page push the DP register and allocate a stack frame by subtracting from it
;;    * $0E are reserved for stack frame, used to store temporary values and params
|#

#|
;; FUNCTIONS
;;     (defreg name htype conflict-list &optional argument-number)
;;        - define a hardware register and its argument number for the calling convention
;;
;;     (defcode name pattern registers code)
;;        - match scode expression, internally known as a "tile"
;;        - pattern may contain the following:
;;                *, wildcard
;;            (= x), bind a register/constant/memory to a name
;;            csp's, these are subpatterns with their own registers
;;        - (reg . . .) determines how registers are touched, not entirely clear
;;
;;     (gen hreg-list &rest code)
;;         - generate code for RTL
;;         - hreg-list binds named registers to a list of compatible hardware registers
;;         - the next arguments contain the instructions to be generated
;;         - advanced generation can be done with (progn) and (emit-mcode)
;;
|#



#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Definitions / Utilities ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#

(defun Nall nil (setq *optimize* '(auto-reg stupid-auto-reg)))
(defun Oall nil (setq *optimize* '(auto-reg jump cse loop delete-zombi)))
(setq *md-convit-optimize* t)
(setq *md-stack-align* 2)
(setq *md-no-push-pop* t)
(setq *md-expand-after-tiling* t)
(defvar *mdl-Ncse-const* nil)
(defvar *mdl-Ncse-global* nil)
(defun Ncse-const nil (setq *mdl-Ncse-const* t))
(defun Ncse-global nil (setq *mdl-Ncse-global* t))
(defun areg-p (x) (eq (substring (symbol-name x) 0 1) "a"))
(defun xreg-p (x) (eq (substring (symbol-name x) 0 1) "x"))
(defun yreg-p (x) (eq (substring (symbol-name x) 0 1) "y"))
(defun md-cse-const-p (expr &optional nrecomp)
    (cond (*mdl-Ncse-const* 'ignore) ((<= nrecomp 3) 'ignore) ((expr-zero-p expr) 'ignore)
        ((let ((val (expr-value expr)))
            (and (fixnum-p val) (<= -32768 val) (<= val 32767))) 'ignore)
            ((and (sent-p (expr-value expr)) (eq 'function (car-safe (sent-htype (expr-value expr))))) 'ignore)
            ((gpoffset-p (expr-value expr)) 'ignore)
            (t 'copy-and-replace)))
(defun md-cse-get-from-static-p (expr &optional nrecomp) (cond (*mdl-Ncse-global* 'ignore) ((<= nrecomp 1) 'ignore) (t 'copy-and-replace)))
(defvar *use-move-bit-stirng-threshold* 20)
(setq *use-move-bit-stirng-threshold* t)
(defun md-expand-setn (to from nbyte) (if (or (eq *use-move-bit-stirng-threshold* t) (< nbyte *use-move-bit-stirng-threshold*)) (let ((px (md-expand-setn-make-pointer (reduce-expr to))) (py (md-expand-setn-make-pointer (reduce-expr from))) htype offset delta) (setq offset 0) (while (< 0 nbyte) (setq htype (cond ((<= 4 nbyte) 'i4) ((<= 2 nbyte) 'i16) (t 'i8)) delta (sizeof-htype htype)) (emit-ccode 'expr (make-expr 'set htype (list (md-expand-setn-add-offset px offset) (make-expr 'get htype (list (md-expand-setn-add-offset py offset)))))) (decf nbyte delta) (incf offset delta)) t) nil))
(defun md-expand-setn-make-pointer (x) (cond ((md-expand-setn-make-pointer-noreg-p x) x) (t (let ((reg (genreg 'i4))) (emit-ccode 'expr (make-expr-set-reg reg x)) reg))))
(defun md-expand-setn-make-pointer-noreg-p (x) (or (eq (expr-car x) 'const) (and (memq (expr-car x) '(add sub)) (every 'md-expand-setn-make-pointer-noreg-p (expr-args x)))))
(defun md-expand-setn-add-offset (x offset) (cond ((sent-p x) (make-expr 'add 'i4 (list (make-expr-get-reg x) (make-expr 'const 'i4 offset)))) (t (make-expr 'add 'i4 (list (copy-expr-all x) (make-expr 'const 'i4 offset))))))
(defvar *mdl-stack-arg-offset* 14)
(defun md-expand-push-arg (arg argsp) (let* ((htype (expr-htype arg)) (expr (make-expr 'set htype (list (make-expr 'auto 'i4 (+ argsp *mdl-stack-arg-offset*)) arg)))) (cond ((and (fixnum-p htype) (md-expand-setn (car (expr-args expr)) (car (expr-args (cadr (expr-args expr)))) htype))) (t (emit-ccode 'expr expr)))))
(defun md-expand-pop-args (nbyte))

(defun loadmsg (msg &rest args) (when *debug* (apply 'format t msg args)))

(defun 2^n-p (n) (= (logcount n) 1))
(defun log2 (n) (loglength (rshu n 1)))


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Register Definitions ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "0")

#|;; Accumulator and index registers ;;|#
(defreg a.b i8 (conflict a.w) (param 0))
(defreg x.b i8 (conflict x.w) (param 1))
(defreg y.b i8 (conflict y.w) (param 2))

(defcsp B-regs nil (a.b x.b y.b))
(defcsp W-regs nil (a.w x.w y.w))
(defcsp A-regs nil (a.w))
(defcsp IDX8-regs nil (x.b y.b))
(defcsp IDX16-regs nil (x.w y.w))

#|;; Virtual registers ;;|#
(defreg $2.b i8 (conflict $2.w))
(defreg $4.b i8 (conflict $4.w))
(defreg $6.b i8 (conflict $6.w))
(defreg $8.b i8 (conflict $8.w))
(defreg $a.b i8 (conflict $a.w))
(defreg $c.b i8 (conflict $c.w))
(defreg a.w i16 (conflict a.b) (param 0))
(defreg x.w i16 (conflict x.b) (param 1))
(defreg y.w i16 (conflict y.b) (param 2))
(defreg $2.w i16 (conflict $2.b))
(defreg $4.w i16 (conflict $4.b))
(defreg $6.w i16 (conflict $6.b))
(defreg $8.w i16 (conflict $8.b))
(defreg $a.w i16 (conflict $a.b))
(defreg $c.w i16 (conflict $c.b))

(defcsp V8-regs nil ($2.b $4.b $6.b $8.b $a.b $c.b))
(defcsp V16-regs nil ($2.w $4.w $6.w $8.w $a.w $c.w))
(defcsp W-regs-for-call-general nil ())
(setq *md-callee-save-regs* nil)
(setq *md-caller-save-regs* '(a.w x.w y.w))
(defcsp caller-save-regs nil (a.w x.w y.w))
(dolist (regs '(B-regs W-regs)) (let ((W-regs (cdr (get 'W-regs 'defcsp)))) (dolist (r (cdr (get regs 'defcsp))) (put r 'W-reg (pop W-regs)))))
(dolist (regs '(V8-regs V16-regs)) (let ((V16-regs (cdr (get 'V16-regs 'defcsp)))) (dolist (r (cdr (get regs 'defcsp))) (put r 'V-reg (pop V16-regs)))))


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Addressing mode constraints and properties ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "1")

(defcsp am-sp (a) (auto i16 (= a)))
(defcsp am-const (c) (const i16 (= c * not-gpoffset-p)))
(defcsp am-gpoffset (g) (const i16 (= g * gpoffset-p)))
(defun gpoffset-p (x) (cond ((fixnum-p x) (and *gp-offset* (<= (- *gp-offset* 32767) x) (<= x (+ *gp-offset* 32767)))) ((consp x) (gpoffset-p (cadr x))) ((sent-p x) (if (eq (sent-scc x) 'extern) nil (memq (sent-segment x) '("bss")))) (t nil)))
(defun not-gpoffset-p (x) (not (gpoffset-p x)))
(defcsp am-dx (d x) (add i16 (= x) (const i16 (= d))))

(defun one-p (x) (eq x 1))
(defun notzero (x) ((and (fixnum-p x) not (zerop x))))
(defcsp co-fixnum (n) (const * (= n * fixnum-p)))
(defcsp co-fixnum-2^n-p (n) (const * (= n * 2^n-p)))
(defcsp co-zero (htype) (const htype (= z * zerop)))
(defcsp co-notzero (htype) (const htype (= z * notzero)))
(defcsp co-one (htype) (const htype (= c * one-p)))

(defun incdec-p (n) (and (< n 5) (> n 0)))
(defun not-incdec-p (n) (or (>= n 5) (<= n 0)))

(defun expr-not-reg-p (expr) (not (eq (expr-car expr) 'reg)))
(defun expr-not-const-p (expr) (not (eq (expr-car expr) 'const)))


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Top Level "Constraint" Code ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "2")

#|;; match tiling-bblock-ccode pass ;;|#
(defcode getreg
    #|;; pattern: (get i8 (reg _x_)) ;;|#
    (get i8 (= x (reg)))
    #|;; registers: out r ;;|#
    (reg r))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode getreg
    #|;; pattern: (get i16 (reg _x_)) ;;|#
    (get i16 (= x (reg)))
    #|;; registers: out r ;;|#
    (reg r))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode setreg
    #|;; pattern: (set i8 (reg _x_) (_y_)) ;;|#
    (set i8 (= x (reg)) (= y))
    #|;; registers: in y ;;|#
    (reg nil y))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode setreg
    #|;; pattern: (set i16 (reg _x_) (_y_)) ;;|#
    (set i16 (= x (reg)) (= y))
    #|;; registers: in y ;;|#
    (reg nil y))

#|;; match fixup-hreg pass ;;|#
(defcode load
    #|;; pattern: (set i8 (hreg _x_) (get i8 (auto i16 _a_)) ;;|#
    (set i8 (= x (hreg) B-regs) (get i8 am-sp))
    (gen nil
        (progn
        (cond   ((areg-p x) (emit-mcode (list 'lda (list 'sp a))))
                ((xreg-p x) (emit-mcode (list 'ldx (list 'sp a))))
                ((yreg-p x) (emit-mcode (list 'ldy (list 'sp a)))) ))))

#|;; match fixup-hreg pass ;;|#
(defcode load
    #|;; pattern: (set i16 (hreg _x_) (get i16 (auto i16 _a_)) ;;|#
    (set i16 (= x (hreg) W-regs) (get i16 am-sp))
    (gen nil
        (progn
        (cond   ((areg-p x) (emit-mcode (list 'lda (list 'sp a))))
                ((xreg-p x) (emit-mcode (list 'ldx (list 'sp a))))
                ((yreg-p x) (emit-mcode (list 'ldy (list 'sp a)))) ))))

#|;; match fixup-hreg pass ;;|#
(defcode store
    #|;; pattern: (set i8 (auto i8 _a_) (get i8 (hreg _x_)) ;;|#
    (set i8 am-sp (get i8 (= x (hreg) B-regs)))
    (gen nil
        (progn
        (cond   ((areg-p x) (emit-mcode (list 'sta (list 'sp a))))
                ((xreg-p x) (emit-mcode (list 'stx (list 'sp a))))
                ((yreg-p x) (emit-mcode (list 'sty (list 'sp a)))) ))))

#|;; match fixup-hreg pass ;;|#
(defcode store
    #|;; pattern: (set i16 (auto i16 _a_) (get i16 (hreg _x_)) ;;|#
    (set i16 am-sp (get i16 (= x (hreg) W-regs)))
    (gen nil
        (progn
        (cond   ((areg-p x) (emit-mcode (list 'sta (list 'sp a))))
                ((xreg-p x) (emit-mcode (list 'stx (list 'sp a))))
                ((yreg-p x) (emit-mcode (list 'sty (list 'sp a)))) ))))

#|;; match fixup-hreg pass ;;|#
(defcode move
    #|;; pattern: (set i8 (hreg _x_) (get i8 (hreg _y_)) ;;|#
    (set i8 (= x (hreg) B-regs) (get i8 (= y (hreg) B-regs)))
    (gen nil
        (progn
        (cond   ((and (xreg-p x) (yreg-p y)) (emit-mcode 'tyx))
                ((and (xreg-p x) (areg-p y)) (emit-mcode 'tax))
                ((and (yreg-p x) (xreg-p y)) (emit-mcode 'txy))
                ((and (yreg-p x) (areg-p y)) (emit-mcode 'tay))
                ((and (areg-p x) (xreg-p y)) (emit-mcode 'txa))
                ((and (areg-p x) (yreg-p y)) (emit-mcode 'tya)) ))))

#|;; match fixup-hreg pass ;;|#
(defcode move
    #|;; pattern: (set i8 (hreg _x_) (get i8 (hreg _y_)) ;;|#
    (set i8 (= x (hreg) B-regs) (get i8 (= y (hreg) V8-regs)))
    (gen nil
        (progn
        (cond   ((areg-p x) (emit-mcode (list 'lda y)))
                ((xreg-p x) (emit-mcode (list 'ldx y)))
                ((yreg-p x) (emit-mcode (list 'ldy y))) ))))

#|;; match fixup-hreg pass ;;|#
(defcode move
    #|;; pattern: (set i8 (hreg _x_) (get i8 (hreg _y_)) ;;|#
    (set i8 (= x (hreg) V8-regs) (get i8 (= y (hreg) B-regs)))
    (gen nil
        (progn
        (cond   ((areg-p y) (emit-mcode (list 'sta x)))
                ((xreg-p y) (emit-mcode (list 'stx x)))
                ((yreg-p y) (emit-mcode (list 'sty x))) ))))

#|;; match fixup-hreg pass ;;|#
(defcode move
    #|;; pattern: (set i16 (hreg _x_) (get i16 (hreg _y_)) ;;|#
    (set i16 (= x (hreg) W-regs) (get i16 (= y (hreg) W-regs)))
    (gen nil
        (progn
        (cond   ((and (xreg-p x) (yreg-p y)) (emit-mcode 'tyx))
                ((and (xreg-p x) (areg-p y)) (emit-mcode 'tax))
                ((and (yreg-p x) (xreg-p y)) (emit-mcode 'txy))
                ((and (yreg-p x) (areg-p y)) (emit-mcode 'tay))
                ((and (areg-p x) (xreg-p y)) (emit-mcode 'txa))
                ((and (areg-p x) (yreg-p y)) (emit-mcode 'tya)) ))))

#|;; match fixup-hreg pass ;;|#
(defcode move
    #|;; pattern: (set i16 (hreg _x_) (get i16 (hreg _y_)) ;;|#
    (set i16 (= x (hreg) W-regs) (get i16 (= y (hreg) V16-regs)))
    (gen nil
        (progn
        (cond   ((areg-p x) (emit-mcode (list 'lda y)))
                ((xreg-p x) (emit-mcode (list 'ldx y)))
                ((yreg-p x) (emit-mcode (list 'ldy y))) ))))

#|;; match fixup-hreg pass ;;|#
(defcode move
    #|;; pattern: (set i16 (hreg _x_) (get i16 (hreg _y_)) ;;|#
    (set i16 (= x (hreg) V16-regs) (get i16 (= y (hreg) W-regs)))
    (gen nil
        (progn
        (cond   ((areg-p y) (emit-mcode (list 'sta x)))
                ((xreg-p y) (emit-mcode (list 'stx x)))
                ((yreg-p y) (emit-mcode (list 'sty x))) ))))

#|;; match fixup-hreg pass ;;|#
(defcode move
    #|;; pattern: (set i16 (hreg _x_) (get i16 (hreg _y_)) ;;|#
    (set i16 (= x (hreg) V16-regs) (get i16 (= y (hreg) V16-regs)))
    #|;; registers: out r ;;|#
    (reg r)
    #|;;  out: {a x y}   in: {}  ;;|#
    (gen ((r . W-regs))
        (progn
        (cond   ((areg-p r) (emit-mcode (list 'lda y))
                            (emit-mcode (list 'sta x)))
                ((xreg-p r) (emit-mcode (list 'ldx y))
                            (emit-mcode (list 'stx x)))
                ((yreg-p r) (emit-mcode (list 'ldy y))
                            (emit-mcode (list 'sty x))) ))))

#|;; these are unused ;;|#
#|;; match fixup-hreg pass ;;|#
(defcode push (set i8 (tos) (get i8 (= x (hreg) B-regs))) (gen nil (*push-i8 'x)))
#|;; match fixup-hreg pass ;;|#
(defcode push (set i16 (tos) (get i16 (= x (hreg) W-regs))) (gen nil (*push-i16 'x)))
#|;; match fixup-hreg pass ;;|#
(defcode pop (set i8 (= x (hreg) B-regs) (get i8 (tos))) (gen nil (*pop-i8 'x)))
#|;; match fixup-hreg pass ;;|#
(defcode pop (set i16 (= x (hreg) W-regs) (get i16 (tos))) (gen nil (*pop-i16 'x)))

#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Get ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "3")

#|;; match tiling-bblock-ccode pass ;;|#
(defcode geti8-dx
    #|;; pattern: (get i16 (add i8 (_x_) (const i16 _d_))) ;;|#
    (get i8 am-dx)
    #|;; registers: out r, in x ;;|#
    (reg r x)
    #|;;  out: {a x y}   in: {x y}  ;;|#
    (gen ((r . B-regs) (x . IDX8-regs))
        (progn
        (cond
            ((areg-p r)
            (cond   ((xreg-p x) (emit-mcode (list 'lda (list 'mem8 d) 'x)))
                    ((yreg-p x) (emit-mcode (list 'lda (list 'mem8 d) 'y)))))
            ((xreg-p r)
            (cond   ((areg-p x) (emit-mcode (list 'ldx (list 'mem8 d) 'a)))
                    ((yreg-p x) (emit-mcode (list 'ldx (list 'mem8 d) 'y)))))
            ((yreg-p r)
            (cond   ((areg-p x) (emit-mcode (list 'ldy (list 'mem8 d) 'a)))
                    ((xreg-p x) (emit-mcode (list 'ldy (list 'mem8 d) 'x))))) ))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode geti8-const
    #|;; pattern: (get i8 (const i16 _c_)) ;;|#
    (get i8 am-const)
    #|;; registers: out r ;;|#
    (reg r)
    #|;;  out: {a x y}   in: {}  ;;|#
    (gen ((r . W-regs))
        (progn
        (cond   ((areg-p r) (emit-mcode (list 'lda (list 'mem8 c))))
                ((xreg-p r) (emit-mcode (list 'ldx (list 'mem8 c))))
                ((yreg-p r) (emit-mcode (list 'ldy (list 'mem8 c)))) ))))

#|;; match generate-mcode pass ;;|#
(defcode geti8-sp
    #|;; pattern: (get i8 (auto i16 _s_)) ;;|#
    (get i8 am-sp)
    #|;; registers: out r ;;|#
    (reg r)
    #|;;  out: {a x y}   in: {}  ;;|#
    (gen ((r . B-regs))
        (progn
        (cond   ((areg-p r) (emit-mcode (list 'lda (list 'sp a))))
                ((xreg-p r) (emit-mcode (list 'ldx (list 'sp a))))
                ((yreg-p r) (emit-mcode (list 'ldy (list 'sp a)))) ))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode geti16-dx
    #|;; pattern: (get i16 (add i16 (_x_) (const i16 _d_))) ;;|#
    (get i16 am-dx)
    #|;; registers: out r, in x ;;|#
    (reg r x)
    #|;;  out: {a x y}   in: {x y}  ;;|#
    (gen ((r . W-regs) (x . IDX16-regs))
        (progn
        (cond
            ((areg-p r)
            (cond   ((xreg-p x) (emit-mcode (list 'lda (list 'mem16 d) 'x)))
                    ((yreg-p x) (emit-mcode (list 'lda (list 'mem16 d) 'y)))))
            ((xreg-p r)
            (cond   ((areg-p x) (emit-mcode (list 'ldx (list 'mem16 d) 'a)))
                    ((yreg-p x) (emit-mcode (list 'ldx (list 'mem16 d) 'y)))))
            ((yreg-p r)
            (cond   ((areg-p x) (emit-mcode (list 'ldy (list 'mem16 d) 'a)))
                    ((xreg-p x) (emit-mcode (list 'ldy (list 'mem16 d) 'x))))) ))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode geti16-const
    #|;; pattern: (get i16 (const i16 _c_)) ;;|#
    (get i16 am-const)
    #|;; registers: out r ;;|#
    (reg r)
    #|;;  out: {a x y}   in: {}  ;;|#
    (gen ((r . W-regs))
        (progn
        (cond   ((areg-p r) (emit-mcode (list 'lda (list 'mem16 c))))
                ((xreg-p r) (emit-mcode (list 'ldx (list 'mem16 c))))
                ((yreg-p r) (emit-mcode (list 'ldy (list 'mem16 c)))) ))))

#|;; match generate-mcode pass ;;|#
(defcode geti16-sp
    #|;; pattern: (get i16 (auto i16 _s_)) ;;|#
    (get i16 am-sp)
    #|;; registers: out r ;;|#
    (reg r)
    #|;;  out: {a x y}   in: {}  ;;|#
    (gen ((r . W-regs))
        (progn
        (cond   ((areg-p r) (emit-mcode (list 'lda (list 'sp a))))
                ((xreg-p r) (emit-mcode (list 'ldx (list 'sp a))))
                ((yreg-p r) (emit-mcode (list 'ldy (list 'sp a)))) ))))


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Set ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "4")

#|;; match tiling-bblock-ccode pass ;;|#
(defcode seti8-dx
    #|;; pattern: (set i8 (add i8 (_x_) (const i16 _d_)) (_r_)) ;;|#
    (set i8 am-dx (= r))
    #|;;  registers: in x r  ;;|#
    (reg nil x r)
    #|;;  out: {}   in: {x y} {a}  ;;|#
    (gen ((x . IDX8-regs) (r . A-regs))
        (progn
        (cond   ((xreg-p x) (emit-mcode (list 'sta (list 'mem16 d) 'x)))
                ((yreg-p x) (emit-mcode (list 'sta (list 'mem16 d) 'y))) ))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode seti8-const
    #|;; pattern: (set i8 (const i16 _c_) (_r_)) ;;|#
    (set i8 am-const (= r))
    #|;;  registers: in r  ;;|#
    (reg nil r)
    #|;;  out: {}   in: {a x y}  ;;|#
    (gen ((r . B-regs))
        (progn
        (cond   ((areg-p r) (emit-mcode (list 'sta (list 'mem8 c))))
                ((xreg-p r) (emit-mcode (list 'stx (list 'mem8 c))))
                ((yreg-p r) (emit-mcode (list 'sty (list 'mem8 c)))) ))))

#|;; match generate-mcode pass ;;|#
(defcode seti8-sp
    #|;; pattern: (set i8 (auto i16 _s_) (_r_)) ;;|#
    (set i8 am-sp (= r))
    #|;;  registers: in r  ;;|#
    (reg nil r)
    #|;;  out: {}   in: {a x y}  ;;|#
    (gen ((r . B-regs))
        (progn
        (cond   ((areg-p r) (emit-mcode (list 'sta (list 'sp a))))
                ((xreg-p r) (emit-mcode (list 'stx (list 'sp a))))
                ((yreg-p r) (emit-mcode (list 'sty (list 'sp a)))) ))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode seti16-dx
    #|;; pattern: (set i16 (add i16 (_x_) (const i16 _d_)) (_r_)) ;;|#
    (set i16 am-dx (= r))
    #|;;  registers: in x r  ;;|#
    (reg nil x r)
    #|;;  out: {}   in: {x y} {a}  ;;|#
    (gen ((x . IDX16-regs) (r . A-regs))
        (progn
        (cond   ((xreg-p x) (emit-mcode (list 'sta (list 'mem16 d) 'x)))
                ((yreg-p x) (emit-mcode (list 'sta (list 'mem16 d) 'y))) ))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode seti16-virtual-zero
    #|;; pattern: (set i16 (* i16 _p_) (const i16 0)) ;;|#
    (set i16 (= p (* i16)) (co-zero i16))
    #|;;  registers: in p ;;|#
    (reg nil p)
    #|;;  out: {}   in: {v}  ;;|#
    (gen ((p . V16-regs))
        (stz.w 'p)))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode seti16-const
    #|;; pattern: (set i16 (const i16 _c_) (_r_)) ;;|#
    (set i16 am-const (= r))
    #|;;  registers: in r  ;;|#
    (reg nil r)
    #|;;  out: {}   in: {a x y}  ;;|#
    (gen ((r . W-regs))
        (progn
        (cond   ((areg-p r) (emit-mcode (list 'sta (list 'mem16 c))))
                ((xreg-p r) (emit-mcode (list 'stx (list 'mem16 c))))
                ((yreg-p r) (emit-mcode (list 'sty (list 'mem16 c)))) ))))

#|;; when this is put above seti16-const, it no longer has priority.. ;;|#
#|;; note the (= r) ;;|#

#|;; match tiling-bblock-ccode pass ;;|#
(defcode seti16-const-zero
    #|;; pattern: (set i16 (const i16 _c_) (const i16 0)) ;;|#
    (set i16 am-const (= r (co-zero i16)))
    (reg nil)
    (gen nil
        (stz.w (mem16 'c))))

#|;; match generate-mcode pass ;;|#
(defcode seti16-sp
    #|;; pattern: (set i16 (auto i16 _s_) (_r_)) ;;|#
    (set i16 am-sp (= r))
    #|;;  registers: in r  ;;|#
    (reg nil r)
    #|;;  out: {}   in: {a x y}  ;;|#
    (gen ((r . W-regs))
        (progn
        (cond   ((areg-p r) (emit-mcode (list 'sta (list 'sp a))))
                ((xreg-p r) (emit-mcode (list 'stx (list 'sp a))))
                ((yreg-p r) (emit-mcode (list 'sty (list 'sp a)))) ))))

#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Constant Value Loading ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "5")

#|;; match tiling-bblock-ccode pass ;;|#
(defcode consti8-general (const i8 (= c)) (reg r)
    (gen ((r . B-regs))
        (progn
        (ecase r    ('a.b (emit-mcode (list 'lda (list 'i8 c))))
                    ('x.b (emit-mcode (list 'ldx (list 'i8 c))))
                    ('y.b (emit-mcode (list 'ldy (list 'i8 c)))) ))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode consti16-general (const i16 (= c)) (reg r)
    (gen ((r . W-regs))
        (progn
        (ecase r    ('a.w (emit-mcode (list 'lda (list 'i16 c))))
                    ('x.w (emit-mcode (list 'ldx (list 'i16 c))))
                    ('y.w (emit-mcode (list 'ldy (list 'i16 c)))) ))))


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Automatic Value ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "6")

#|;; Not sure when this is ever used ;;|#
(defcode auto-general (auto i16 (= a)) (reg r) (gen ((r . W-regs)) (movea (i16 'a) sp 'r)))

#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Type Conversions ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "7")

#|;; match tiling-bblock-ccode pass ;;|#
(defcode convsxi16/i8-general (convsx i16 (= s (* i8))) (reg r s) (gen ((r . W-regs) (s . B-regs)) (mov 's 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode convzxi16/i8-general (convzx i16 (= s (* i8))) (reg r s) (gen ((r . W-regs) (s . B-regs)) (andi (i16 255) 's 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode conviti8/i16-general (convit i8 (= s (* i16))) (reg r s) (gen ((r . B-regs) (s . W-regs)) (mov 's 'r) (shl 24 'r) (sar 24 'r)))


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Type Conversions ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "8")

#|;; match tiling-bblock-ccode pass ;;|#
(defcode negi16-general (neg i16 (= r)) (reg r r) (gen ((r . W-regs)) (not 'r 'r) (add (i16 1) 'r)))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode inc (add i16 (= r) (const i16 (= c * incdec-p)))
    (reg r r)
    (gen ((r . W-regs))
        (progn
        (dotimes (i c)
            (cond   ((areg-p r) (emit-mcode 'ina))
                    ((xreg-p r) (emit-mcode 'inx))
                    ((yreg-p r) (emit-mcode 'iny)) )))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode addi16-const (add i16 (= r) (const i16 (= c * not-incdec-p)))
    (reg r r)
    (gen ((r . A-regs))
        (clc)
        (adc (i16 'c)) ))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode addi16-mem (add i16 (= r) (get i16 (const i16 (= p))))
    (reg r r)
    (gen ((r a.w))
        (clc)
        (adc (mem16 'p)) ))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode addi16-virtual (add i16 (= r) (= p (reg)))
    (reg r r p)
    (gen ((r . A-regs) (p . V16-regs))
        (adc 'p)))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode addi8-const (add i8 (= r) (const i8 (= c)))
    (reg r r)
    (gen ((r a.b))
        (progn
        (if (and (< c 5) (> c 0))
            (dotimes (i c)
                (emit-mcode 'inc r))
            (progn
                (emit-mcode (list 'clc))
                (emit-mcode (list 'adc (list 'i8 c))) )))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode addi8-general (add i8 (= r) (= p (reg)))
    (reg r r p)
    (gen ((r a.b) (p . V8-regs))
        (adc 'p)))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode subi16-virtual (sub i16 (= r) (= s))
    (reg r r s)
    (gen ((r . A-regs) (s . V16-regs))
        (sec)
        (sbc.w 's)))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode subi16-const (sub i16 (= r) (const i16 (= c)))
    (reg r)
    (gen ((r . A-regs))
        (sec)
        (sbc (i16 c)) ))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode bandi16-general (band i16 (= r) (= p)) (reg r r p) (gen ((r . W-regs) (p . W-regs)) (and 'p 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode bandi16-const (band i16 (= r) (const i16 (= c)))
    (reg r r)
    (gen ((r . A-regs))
        (and (i16 'c))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode bxori16-general (bxor i16 (= r) (= p)) (reg r r p) (gen ((r . W-regs) (p . W-regs)) (xor 'p 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode bxori16-const (bxor i16 (= x) (const i16 (= c))) (reg r x) (gen ((r . W-regs) (x . W-regs)) (progn (if (eq r x) (emit-mcode (list 'xor (list 'i4 c) r)) (emit-mcode (list 'xori (list 'i4 c) x r))))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode bori16-virtual (bor i16 (= r) (= p))
    (reg r r p)
    (gen ((r . A-regs) (p . V16-regs))
        (ora 'p)))

#|;; match generate-mcode pass ;;|#
(defcode bori16-sp (bor i16 (= r) (get i16 am-sp))
    (reg r r)
    (gen ((r . A-regs))
        (ora 's)))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode bori16-const (bor i16 (= r) (const i16 (= c)))
    (reg r r)
    (gen ((r . A-regs))
        (ora (i16 'c))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode bnoti16-general (bnot i16 (= r)) (reg r r) (gen ((r . W-regs)) (not 'r 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode muli16-general (mul i16 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (mul 's 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode muli16-const (mul i16 (= r) (co-fixnum n)) (reg r r) (gen ((r . W-regs)) (mul 'n 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode muli16-2^n (mul i16 (= r) (co-fixnum-2^n-p n))
    (reg r r)
    (gen ((r . A-regs))
        (progn
        (dotimes (i (log2 n))
            (emit-mcode 'asl)))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode divi4-general (div i4 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (div 's 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode divi4-2^n (div i4 (= r) (co-fixnum-2^n-p n)) (reg r r) (gen ((r . W-regs)) (progn (emit-mcode (list 'sar (list 'i4 (log2 n)) r)))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode divui4-general (divu i4 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (divu 's 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode divui4-2^n (divu i4 (= r) (co-fixnum-2^n-p n)) (reg r r) (gen ((r . W-regs)) (progn (emit-mcode (list 'shr (list 'i4 (log2 n)) r)))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode modi16-general (mod i16 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (div 's 'r) (mov r30W 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode modui16-general (modu i16 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (divu 's 'r) (mov r30W 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode lshi16-general (lsh i16 (= r) (= s)) (reg r r s)
    (gen ((r . W-regs) (s . W-regs))
        (progn
            (emit-mcode (list 'asl 's 'r)))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode lshi16-const (lsh i16 (= r) (co-fixnum c))
    (reg r r)
    (gen ((r . A-regs))
        (progn
        (dotimes (i c)
            (emit-mcode 'asl)))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode rshi16-general (rsh i16 (= r) (= s))
    (reg r r s)
    (gen ((r . A-regs) (s . W-regs)) (sar 's 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode rshi16-const (rsh i16 (= r) (co-fixnum c))
    (reg r r)
    (gen ((r . A-regs))
        (progn
        (dotimes (i c)
            (emit-mcode 'lsr)))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode rshui16-general (rshu i16 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (shr 's 'r)))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode rshui16-general (rshu i16 (= r) (co-fixnum c)) (reg r r) (gen ((r . W-regs)) (shr (i16 'c) 'r)))


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Jump / Control Flow  ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "9")

(defcsp jcode (jmc) (= jmc * ((tsteq jmp-eq) (tstne jmp-ne) (tstlt jmp-lt) (tstltu jmp-ltu) (tstle jmp-le) (tstleu jmp-leu) (tstgt jmp-gt) (tstgtu jmp-gtu) (tstge jmp-ge) (tstgeu jmp-geu))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode jump1 (jump1 la) (gen nil (jmp-1 'la)))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode jump2i16-virtual (jump2 ((jcode jmc) * (= r (* i16)) (= p (* i16))) la)
    (reg nil r p)
    (gen ((r . A-regs) (p . V16-regs))
        (progn
        (cond   ((areg-p r) (emit-mcode (list 'cmp p)))
                ((xreg-p r) (emit-mcode (list 'cpx p)))
                ((yreg-p r) (emit-mcode (list 'cpy p))) )
            (emit-mcode (list jmc la)))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode jump2i16-mem (jump2 ((jcode jmc) * (= r (* i16)) (get i16 am-const)) la)
    (reg nil r)
    (gen ((r . W-regs))
        (progn
        (cond   ((areg-p r) (emit-mcode (list 'cmp (list 'mem16 c))))
                ((xreg-p r) (emit-mcode (list 'cpx (list 'mem16 c))))
                ((yreg-p r) (emit-mcode (list 'cpy (list 'mem16 c)))) )
            (emit-mcode (list jmc la)))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode jump2i16-const-notzero (jump2 ((jcode jmc) * (= r (* i16)) (co-fixnum c)) la)
    (reg nil r)
    (gen ((r . W-regs))
        (progn
        #|;; hack to avoid bug in sf-peep where 'i2 generates extra 'mem16 [sub_2FD1C] ;;|#
        (let* ((type (if (zerop c) 'i2 'i8)))
            (cond   ((areg-p r) (emit-mcode (list 'cmp (list type c))))
                    ((xreg-p r) (emit-mcode (list 'cpx (list type c))))
                    ((yreg-p r) (emit-mcode (list 'cpy (list type c)))) ))
            (emit-mcode (list jmc la)))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode jumpn (jumpn (= r) calist) (reg nil r) (gen ((r . W-regs)) (progn (emit-jumpn r calist))))

(defun emit-jumpn (reg calist) (dolist (c calist) (setq (sent-scc (cadr c)) 'caselabel)) (let ((dlabel (genlabel))) (setq (sent-scc dlabel) 'caselabel) (emit-jumpn-rec reg calist dlabel) (emit-mcode (list 'def dlabel))))
(defun emit-jumpn-rec (reg calist dlabel) (cond ((emit-jumpn-by-table-p calist) (emit-jumpn-by-table reg calist dlabel)) ((emit-jumpn-by-liner-p calist) (emit-jumpn-by-liner reg calist dlabel)) (t (let ((tmpl (genlabel))) (letl (calist1 center calist2) (emit-jumpn-split-calist calist) (emit-mcode (list 'cmp (list 'i4 (car center)) reg)) (emit-mcode (list 'jmp-eq (cadr center))) (emit-mcode (list 'jmp-gt tmpl)) (emit-jumpn-rec reg calist1 dlabel) (emit-mcode (list 'def tmpl)) (emit-jumpn-rec reg calist2 dlabel))))))
(defun emit-jumpn-by-table-p (calist) (let ((min (caar calist)) (max (caar (last calist))) (len (length calist))) (and (<= 5 len) (<= (- max min) (* 3 len)))))
(defun emit-jumpn-by-liner-p (calist) (<= (length calist) 2)) #|;; emit linear jumps if <= 2 ;;|#
(defun emit-jumpn-by-liner (reg calist dlabel) (dolist (c calist) (emit-mcode (list 'cmp (list 'i4 (car c)) reg)) (emit-mcode (list 'jmp-eq (cadr c)))) (when dlabel (emit-mcode (list 'jmp-1 dlabel))))
(defun emit-jumpn-split-calist (calist) (let ((len (length calist)) x y) (unless (<= 3 len) (clerror "emit-jumpn-split-calist: unexpedted list length")) (setq y (nthcdr (/ len 2) calist) x (ldiff calist y)) (list x (car y) (cdr y))))
(defvar *mdl-jumpn-table-id* 0)
(defvar *mdl-jumpn-table-list* nil)
(defun emit-jumpn-by-table (reg calist dlabel) (let ((min (caar calist)) (max (caar (last calist))) table) (cond ((zerop min) (emit-mcode (list 'mov reg 'r30W))) (t (decf max min) (emit-mcode (list 'addi (list 'i4 (- min)) reg 'r30W)) (dolist (c calist) (decf (car c) min)))) (setq table (make-jumpn-table calist dlabel)) (emit-mcode (list 'cmp (list 'i4 max) 'r30W)) (emit-mcode (list 'jmp-gtu dlabel)) (emit-mcode '(shl (i4 2) r30W)) (emit-mcode (list 'ld.w (list 'dx table 'r30W) 'r30W)) (emit-mcode '(jmp (dx 0 r30W)))))
(defun make-jumpn-table (calist dlabel) (let ((tlabel (genlabel)) (table (make-tconc)) (n 0)) (setq (sent-asmname tlabel) (format nil "LS%d" (incf *mdl-jumpn-table-id*))) (dolist (c calist) (while (< n (car c)) (tconc table dlabel) (incf n)) (tconc table (cadr c)) (incf n)) (push (cons tlabel (tconc-list table)) *mdl-jumpn-table-list*) tlabel))
(defun emit-jumpn-tables nil (dolist (table *mdl-jumpn-table-list*) (emit-acode-changeseg "const") (emit-acode-align 4) (emit-acode-def (sent-asmname (car table))) (dolist (e (cdr table)) (emit-acode-data 'i4 e))) (setq *mdl-jumpn-table-list* nil))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode label (label la) (gen nil (def 'la)))

#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Prologue and Epilogue  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "a")

(extern *cfun-name* *cfun-args-htype* *cfun-return-htype* *cfun-ahtype* *cfun-param-list* *cfun-local-list* *cfun-local-size* *cfun-tmp-list* *cfun-tmp-size* *cfun-args-size* *cfun-is-leaf-p*)
(defvar *mdl-frame-size* 0)
(defvar *mdl-arg* 0)
(defvar *mdl-hreg-save-area* 0)
(defvar *mdl-hreg-save-area-size* 0)

#|;; match tiling-bblock-ccode pass ;;|#
(defcode prologue (prologue)
    (gen nil
    (progn
        #|;; calculate stack etc ;;|#
        (setq *mdl-arg* 0)
        (let ((frame 0))
            (incf frame *cfun-args-size*)
            (dolist (s *cfun-local-list*) (incf (sent-offset s) frame))
            (incf frame *cfun-local-size*)
            (dolist (r *cfun-hreg-list*)
                (let (res)
                    (if (get r 'V-reg)
                        (setq *mdl-arg* *mdl-stack-arg-offset*))
                    (setq r (get r 'W-reg))
                    (unless (memq r res) (push r res))
                    (setq *cfun-hreg-list* res)))
            (incf frame *mdl-arg*)
            (dolist (s *cfun-tmp-list*) (incf (sent-offset s) frame))
            (incf frame *cfun-tmp-size*)
            #|;; hardware registers used by the function ;;|#
            (setq *cfun-hreg-list* (delq 'nil *cfun-hreg-list*))
            (dolist (reg *md-caller-save-regs*) (setq *cfun-hreg-list* (delq reg *cfun-hreg-list*)))
            (setq *mdl-hreg-save-area* frame *mdl-hreg-save-area-size* (* 4 (length *cfun-hreg-list*)))
            (incf frame *mdl-hreg-save-area-size*)
            #|;; (unless *cfun-is-leaf-p* (incf frame 4)) ;;|#
            (setq *mdl-frame-size* frame)
            (dolist (s *cfun-param-list*)
                (incf (sent-offset s) (+ *mdl-frame-size* *mdl-stack-arg-offset*)))))
        #|;; print info ;;|#
        (progn
            (emit-mcode 'comment (format nil "framesize %d = %d(args) + %d(local) + %d(tmp) + %d(saveregs) + %d(savelp)" *mdl-frame-size* *cfun-args-size* *cfun-local-size* *cfun-tmp-size* *mdl-hreg-save-area-size* (if *cfun-is-leaf-p* 0 0)))
            (emit-mcode 'comment (format nil "used callee save regs = %t" *cfun-hreg-list*))
            (dolist (s *cfun-param-list*) (emit-mcode 'comment (format nil "param %T = (auto %d)" (sent-name s) (sent-offset s))))
            (dolist (s *cfun-local-list*) (emit-mcode 'comment (format nil "local %T = (auto %d)" (sent-name s) (sent-offset s))))
            (dolist (s *cfun-tmp-list*) (emit-mcode 'comment (format nil "tmp   %T = (auto %d)" (sent-name s) (sent-offset s)))))
        #|;; emit code ;;|#
        (progn
            (emit-mcode (list 'rep (list 'i8 31)))
            (unless (zerop *mdl-frame-size*)
                (emit-mcode 'phd)
                (emit-mcode 'pha)
                (emit-mcode 'tdc)
                (emit-mcode (list 'adc (list 'i8 (- *mdl-frame-size*))))
                (emit-mcode 'tcd)
                (emit-mcode 'pla) )
            (unless (zerop *mdl-hreg-save-area-size*)
                (let ((off *mdl-hreg-save-area*))
                    (dolist (reg *cfun-hreg-list*)
                        (emit-mcode (list 'st.w reg (list 'sp (posincf off 4))))))))))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode epilogue (epilogue)
    (gen nil
    (progn
        (unless (zerop *mdl-frame-size*) (emit-mcode 'pld)))
    (rtl)))

#|;; match tiling-bblock-ccode pass ;;|#
(defcode pusharg (set i8 (arg) (= x)) (reg nil x) (gen ((x . B-regs)) (progn (emit-mcode (list 'sta x (list 'sp *mdl-arg*))) (setq *mdl-arg* (+ *mdl-arg* 2)))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode pusharg (set i16 (arg) (= x)) (reg nil x) (gen ((x . W-regs)) (progn (emit-mcode (list 'sta x (list 'sp *mdl-arg*))) (setq *mdl-arg* (+ *mdl-arg* 2)))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode poparg (set (= n) (null) (get * (arg))) (gen nil (progn (setq *mdl-arg* (- *mdl-arg* n)))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode calli8-general (call i8 (= f)) (reg r f) (gen ((r a.b) (f . W-regs-for-call-general) (use . caller-save-regs)) (mov (i4 (add * 10)) lp) (jmp (dx 0 'f))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode calli16-general (call i16 (= f)) (reg r f) (gen ((r a.w) (f . W-regs-for-call-general) (use . caller-save-regs)) (mov (i4 (add * 10)) lp) (jmp (dx 0 'f))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode callvoid-general (call void (= f)) (reg r f) (gen ((r a.w) (f . W-regs-for-call-general) (use . caller-save-regs)) (mov (i4 (add * 10)) lp) (jmp (dx 0 'f))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode calli8-const (call i8 (const i16 (= f))) (reg r) (gen ((r a.b) (use . caller-save-regs)) (jsl (label 'f))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode calli16-const (call i16 (const i16 (= f))) (reg r) (gen ((r a.w) (use . caller-save-regs)) (jsl (label 'f))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode callvoid-const (call void (const i16 (= f))) (reg r) (gen ((r a.w) (use . caller-save-regs)) (jsl (label 'f))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode ret-i8 (set i8 (ret) (= r)) (reg nil r) (gen ((r a.b))))
#|;; match tiling-bblock-ccode pass ;;|#
(defcode ret-i16 (set i16 (ret) (= r)) (reg nil r) (gen ((r a.w))))


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Peephole  ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "b")

#|;; call into internal sf-peep routine ;;|#
#|;; peephole-optimize is called by reduce-mcode ;;|#
(defun peephole-optimize (bblockh)
    (when (memq 'sf-peep *optimize*)
        (when (memq 'peephole *debug*)
            (format t "=== sf-peep\n"))
        (do-bblock (b bblockh) (sf-peep bblockh))))

(setq *mcode-bra-offset* nil)


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Assembly Listing Emit Functions  ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(loadmsg "c")

(setq *md-no-underscore* nil)
(defvar *mdl-lstatic/string-id* 0)
(defun make-asm-name (name class) (ecase class ((extern global static) (if *md-no-underscore* name (format nil "_%s" name))) (lstatic (format nil "%s@%d" name (incf *mdl-lstatic/string-id*))) (string (format nil "%s@%d" name (incf *mdl-lstatic/string-id*)))))
(defun emit-acode-beginning-of-object nil
    (setq *mdl-float-literal-alist* nil *mdl-float-literal-id* 0)
    (emit-acode ";;; source = %T\n" (file-name-change-suffix (stream-name *objsm*) ".c"))
    (emit-acode ";;; cparse = (%T %T %T)\n" *cparse-version* *ld-name* *ld-version*)
    (emit-acode ";;; cgrind = (%T %T %T)\n" *cgrind-version* *md-name* *md-version*)
    (dolist (sym '(*command-line-args* *optimize* *debug*)) (let ((vals (symbol-value sym))) (when vals (emit-acode ";;; %t = (" sym) (dolist (a vals) (emit-acode "\n;;;     %t" a)) (emit-acode ")\n"))))
    (emit-acode "\n")
    (emit-acode ".memorymap\n")
    (emit-acode "defaultslot 0\n"
    (emit-acode "slotsize $4000\n")
    (emit-acode "slot 0 $0000\n"))
    (emit-acode ".endme\n")
    (emit-acode ".rombanksize $4000\n")
    (emit-acode ".rombanks 2\n")
    (when *debugger* (emit-acode-debug-info-type))
    (emit-acode "\n"))
(defun emit-acode-debug-info-type nil (emit-acode "\n;;; type define table\n") (emit-acode "DB@T_COUNT		equ	0\n") (emit-acode "DB@T_ATOM		equ	1\n") (emit-acode "DB@T_PTR		equ	2\n") (emit-acode "DB@T_VECT		equ	3\n") (emit-acode "DB@T_STRUCT		equ	4\n") (emit-acode "DB@T_UNION		equ	5\n") (emit-acode "DB@T_ENUM		equ	6\n") (emit-acode "DB@T_FUNC		equ	7\n") (emit-acode "DB@T_TYPENAME		equ	8\n") (emit-acode "DB@T_STRUCT_MEMBER	equ	0x10|DB@T_STRUCT\n") (emit-acode "DB@T_UNION_MEMBER	equ	0x10|DB@T_UNION\n") (emit-acode "DB@T_ENUM_MEMBER	equ	0x10|DB@T_ENUM\n") (emit-acode "DB@T_FUNC_ARG		equ	0x10|DB@T_FUNC\n") (emit-acode "\n;;; type table\n") (emit-acode "	dbtype	DB@T_COUNT,%d ; Number of types.\n" (length *typevector*)) (dotimes (i (length *typevector*)) (let ((type (vref *typevector* i))) (if (< i 10) (progn (emit-acode "	dbtype	DB@T_ATOM,") (emit-acode (ecase type (char "\"char\",1,1") (short "\"short\",1,2") (int "\"int\",1,4") (long "\"long\",1,4") (uchar "\"unsigned char\",0,1") (ushort "\"unsigned short\",0,2") (uint "\"unsigned int\",0,4") (ulong "\"unsigned long\",0,4") (float "\"float\",2,4") (void "\"void\",3,4"))) (emit-acode "%55i; #%d\n" i)) (progn (ecase (car type) (type (emit-acode "	dbtype	DB@T_TYPENAME,%t,%d %55i; #%d\n" (cadr type) (caddr type) i)) (pointer (emit-acode "	dbtype	DB@T_PTR,%d %55i; #%d\n" (cadr type) i)) (vector (emit-acode "	dbtype	DB@T_VECT,%d,%d %55i; #%d\n" (caddr type) (or (cadr type) 0) i)) #'(let ((argtypes (delq '&rest (copy-list (cadr type))))) (emit-acode "	dbtype	DB@T_FUNC,%d,%d %55i; #%d\n" (caddr type) (length argtypes) i) (dolist (a argtypes) (emit-acode "	dbtype	DB@T_FUNC_ARG,%d\n" a))) ((struct union) (emit-acode "	dbtype	%t,%t,%d,%d %55i; #%d\n" (ecase (car type) (struct 'DB@T_STRUCT) (union 'DB@T_UNION)) (or (cadr type) "") (length (cdddr type)) (or (caddr type) 0) i) (dolist (m (cdddr type)) (ecase (car type) (struct (emit-acode "	dbtype	DB@T_STRUCT_MEMBER,%t,%d,%d,%d\n" (car m) (cadr m) (nth 2 m) (nth 3 m))) (union (emit-acode "	dbtype	DB@T_UNION_MEMBER,%t,%d\n" (car m) (cadr m)))))) (enum (emit-acode "	dbtype	DB@T_ENUM,%t,%d %55i; #%d\n" (or (cadr type) "") (length (cddr type)) i) (dolist (m (cddr type)) (emit-acode "	dbtype	DB@T_ENUM_MEMBER,%t,%d\n" (car m) (cadr m))))))))))
(defvar *md-notify-room* nil)
(defun emit-acode-end-of-object nil (emit-acode "\n	; end\n") (when *md-notify-room* (room t)))
(defun emit-acode-beginning-of-function (name) (emit-acode "\n;;; FUNCTION %T\n" name))
(defun emit-acode-end-of-function nil (emit-jumpn-tables))
(defun emit-acode-static-variable (sent value) (if (eq (sent-segment sent) ".bss") (progn (unless (eq *current-segment-name* "bss") (emit-acode-changeseg "bss") (setq *current-segment-name* "bss")) (emit-acode "%s	%t	%d,%d\n" (sent-asmname sent) (if (eq (sent-scc sent) 'global) 'comm 'comm) (sizeof-htype (sent-htype sent)) (sent-align sent)) t) nil))
(defvar *mdl-emitted-segment-name-list* nil)
(defun emit-acode-changeseg (segname) (when (and segname (not (eq segname *current-segment-name*))) (emit-acode "; .%T\n" segname) (unless (memq segname *mdl-emitted-segment-name-list*) (push segname *mdl-emitted-segment-name-list*) (unless (memq segname '("text" "bss")) (emit-acode-align 4))) (setq *current-segment-name* segname)))
(defun emit-acode-align (align) (emit-acode "	.align	%d\n" align))
(defun emit-acode-def (name) (when (sent-p name) (setq name (sent-asmname name))) (emit-acode "%T:\n" name))
(defun emit-acode-xdef (name) (emit-acode "	.export	%T\n" name) (emit-acode "%T:\n" name))
(defun emit-acode-extern (name) (emit-acode "	; extern	%T\n" name))
(defun emit-acode-space (nbyte) (emit-acode "	.ds	%d\n" nbyte))
(defun emit-acode-zeros (nbyte) (let ((n 0)) (dotimes (b nbyte) (if (zerop (% n 15)) (emit-acode "%n	db	") (emit-acode ",")) (emit-acode "0") (incf n)) (emit-acode "\n")))
(defun emit-acode-string (bytelist) (let ((n 0)) (dolist (b bytelist) (if (zerop (% n 15)) (emit-acode "%n	db	") (emit-acode ",")) (emit-acode "%d" b) (incf n)) (emit-acode "\n")))
(defun emit-acode-data (htype data)
    (unless (fixnum-p data) (setq data (conv-asmconst-to-infix data)))
    (ecase htype
        (i8 (emit-acode "	.db	%T\n" data))
        (i16 (emit-acode "	.dh	%T\n" data))
        (i4 (emit-acode "	.dw	%T\n" data))
        (f4 (emit-acode "	.dw	%T\n" data))))

(defun emit-acode-jmp-bra (code label)
    (if *debug* (emit-acode "	%t	%T\n" code label)
    (emit-acode
        (ecase code
            (jmp-1 "	bra	%T\n")
            (jmp-eq "	beq	%T\n")
            (jmp-ne "	bne	%T\n")
            (jmp-lt "	bcc	%T\n")
            (jmp-ltu "	bl	%T\n")
            (jmp-le "	ble	%T\n")
            (jmp-leu "	bnh	%T\n")
            (jmp-gt "	bgt	%T\n")
            (jmp-gtu "	bh	%T\n")
            (jmp-ge "	bcs	%T\n")
            (jmp-geu "	bnl	%T\n"))
        label)))
(defun emit-acode-debug-info-code (info) (ecase (car info) (file (emit-acode "	dbsrc	%s\n" (cadr info))) (line (emit-acode "	dbline	%d\n" (cadr info))) (begin (emit-acode "B@%d:\n" (cadr info))) (end (emit-acode "E@%d:\n" (cadr info)))))
(defun emit-acode-debug-info-scope (sid) (if (zerop sid) (emit-acode "\n	dbscope	0,0\n") (emit-acode "	dbscope	B@%d,E@%d\n" sid sid)))
(defun emit-acode-debug-info-sent (name class typeid place) (if (eq class 'type) (let ((space (if (consp name) 9 8)) (name (if (consp name) (cadr name) name))) (emit-acode "	dbivar	%t,%d,%t,0\n" name typeid space)) (progn (emit-acode "	dbivar	%t,%d,%d," name typeid (ecase class (global 0) (static 1) (lstatic 4) (auto 5) (register 6))) (ecase class ((global static lstatic) (emit-acode "%s" place)) (auto (emit-acode "%d" place)) (register (emit-acode "%s" (substring (symbol-name place) 1 -1)))) (emit-acode "\n"))))
(defun emit-acode-mcode (mcode)
    (let ((code (mcode-code mcode)) (args (mcode-args mcode)) (first t))
        (emit-acode "	%t" code)
        (dolist (a args)
            (emit-acode (if first "	" ","))
            (setq first nil)
            (cond
                ((symbol-p a) (emit-acode "%s" (emit-acode-mcode-regname a)))
                ((fixnum-p a) (emit-acode "$%T" a))
                (t
                    (ecase (car a)
                        ((label) (emit-acode "%s" (conv-asmconst-to-infix (cadr a))))
                        ((i1 i2 i4 i8 i16 i4 f4) (emit-acode "#$%s" (conv-asmconst-to-infix (cadr a))))
                        ((mem8)
                            (if (numberp (cadr a))
                                (emit-acode "$%s.b" (format nil "%x" (cadr a)))
                                (emit-acode "%s.b" (emit-acode-mcode-offset (cadr a)))))
                        ((mem16)
                            (if (numberp (cadr a))
                                (emit-acode "$%s.w" (format nil "%x" (cadr a)))
                                (emit-acode "%s.w" (emit-acode-mcode-offset (cadr a)))))
                        ((sp gp) (emit-acode "$%s.w" (emit-acode-mcode-offset (cadr a))))
                        (dx (emit-acode "%s[%s]" (emit-acode-mcode-offset (cadr a)) (emit-acode-mcode-regname (caddr a)))) ))))
        (emit-acode "\n")))
(defun emit-acode-mcode-offset (o) (if (zerop o) "" (conv-asmconst-to-infix o)))
(defun emit-acode-mcode-regname (regsym)
    (let ((regname (symbol-name regsym)))
        #|;; (when (and (not *debug*) (not (memq regsym '(sp gp lp tp))))
            (setq regname (substring regname 0 -2))) ;;|# regname))
(defun emit-acode-comment (msg) (emit-acode ";; %T\n" msg))
(defun conv-asmconst-to-infix (expr)
    (cond
        ((sent-p expr)
            (if (eq (sent-sc expr) 'static)
                (sent-asmname expr)
                (if *debug* (sent-name expr) (format nil "%x" (sent-offset expr)))))
        ((eq expr '*) "*")
        ((symbol-p expr) (symbol-name expr))
        ((fixnum-p expr) (format nil "%T" expr))
        ((flonum-p expr) (format nil "0f%T" expr))
        ((string-p expr) expr)
        ((atom expr) (clerror "conv-asmconst-to-infix: %t is illegal argument ????" expr))
        (t
            (let* ((code (car expr)) (args (cdr expr)) (cargs (mapcar #'conv-asmconst-to-infix args)))
                (ecase code
                    ((add adda) (apply #'format nil "%T+%T" cargs))
                    ((sub suba) (apply #'format nil (if (consp (cadr args)) "%T-(%T)" "%T-%T") cargs))
                    (neg (apply #'format nil (if (consp (car args)) "-(%T)" "-%T") cargs)))))))


(loadmsg "...")
