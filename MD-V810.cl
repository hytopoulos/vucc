(setq *md-name* "md-v810")
(setq *md-version* "0.1")

#| 65816 CALLING CONVENTION
    First argument is passed in A, second in X, third in Y

    Temporaries are stored at $0E
    ; The rest of the variables are stored on the stack, starting at $0E
|#

#|

functions:

    (defreg name htype conflict-list &optional argument-number)
        define a hardware register and its argument number for the calling convention

    (defcode name pattern registers code)
        match pattern in RTL and emit code for it

        patterns:
            (= name expr)
                match expr and assign it to name

    (gen hreg-list code)
        generate code for RTL, reg-list is a list of (reg . reg-type) hard register pairs
        code is not an expression to be evaluated, but a list of "mcode"

    (reg . registers...)
        define a hardware register that will be used in the instruction

types:
    bblockh: a list of mcode

    mcode: machine code
        example -- '3:00000000:rep 31'

        use (mcode-code mcode) to get the instruction
        use (mcode-args mcode) to get the arguments

|#

(defun Nall nil (setq *optimize* '(auto-reg stupid-auto-reg v810-peephole)))
(defun Oall nil (setq *optimize* '(auto-reg jump cse loop delete-zombi v810-peephole)))
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
(defun md-expand-setn (to from nbyte) (if (or (eq *use-move-bit-stirng-threshold* t) (< nbyte *use-move-bit-stirng-threshold*)) (let ((px (md-expand-setn-make-pointer (reduce-expr to))) (py (md-expand-setn-make-pointer (reduce-expr from))) htype offset delta) (setq offset 0) (while (< 0 nbyte) (setq htype (cond ((<= 4 nbyte) 'i4) ((<= 2 nbyte) 'i2) (t 'i1)) delta (sizeof-htype htype)) (emit-ccode 'expr (make-expr 'set htype (list (md-expand-setn-add-offset px offset) (make-expr 'get htype (list (md-expand-setn-add-offset py offset)))))) (decf nbyte delta) (incf offset delta)) t) nil))
(defun md-expand-setn-make-pointer (x) (cond ((md-expand-setn-make-pointer-noreg-p x) x) (t (let ((reg (genreg 'i4))) (emit-ccode 'expr (make-expr-set-reg reg x)) reg))))
(defun md-expand-setn-make-pointer-noreg-p (x) (or (eq (expr-car x) 'const) (and (memq (expr-car x) '(add sub)) (every 'md-expand-setn-make-pointer-noreg-p (expr-args x)))))
(defun md-expand-setn-add-offset (x offset) (cond ((sent-p x) (make-expr 'add 'i4 (list (make-expr-get-reg x) (make-expr 'const 'i4 offset)))) (t (make-expr 'add 'i4 (list (copy-expr-all x) (make-expr 'const 'i4 offset))))))
(defun md-expand-get-bitfield (expr tlabel flabel voidp) (let* ((bit (car (expr-args expr))) (signedp (eq (expr-car bit) 'bit)) (beg (expr-value (cadr (expr-args bit)))) (width (expr-value (caddr (expr-args bit))))) (setq (car (expr-args expr)) (car (expr-args bit))) (expand-expr-1 (cond ((and (zerop beg) (not signedp)) (make-expr 'band 'i4 (list expr (make-expr-const 'i4 (unsigned-int -1 width))))) (t (make-expr (if signedp 'rsh 'rshu) 'i4 (list (make-expr 'lsh 'i4 (list expr (make-expr-const 'i4 (- 32 (+ beg width))))) (make-expr-const 'i4 (- 32 width)))))) tlabel flabel voidp)))
(defun md-expand-set-bitfield (expr tlabel flabel voidp) (let* ((bit (car (expr-args expr))) (y (cadr (expr-args expr))) (p (car (expr-args bit))) (beg (cadr (expr-args bit))) (width (caddr (expr-args bit))) (reg (genreg 'i4)) (mask1 (lsh (unsigned-int -1 (expr-value width)) (expr-value beg))) p0) (emit-ccode 'expr (make-expr-set-reg reg (make-expr 'band 'i4 (list (make-expr 'lsh 'i4 (list y beg)) (make-expr-const 'i4 mask1))))) (setq p0 (expand-expr-1 p)) (emit-ccode 'expr (make-expr 'set 'i4 (list p0 (make-expr 'bor 'i4 (list (make-expr 'band 'i4 (list (make-expr 'get 'i4 (list p0)) (make-expr-const 'i4 (bnot mask1)))) (make-expr-get-reg reg)))))) (make-expr-get-reg reg)))
(defvar *mdl-stack-arg-offset* 14)
(defun md-expand-push-arg (arg argsp) (let* ((htype (expr-htype arg)) (expr (make-expr 'set htype (list (make-expr 'auto 'i4 (+ argsp *mdl-stack-arg-offset*)) arg)))) (cond ((and (fixnum-p htype) (md-expand-setn (car (expr-args expr)) (car (expr-args (cadr (expr-args expr)))) htype))) (t (emit-ccode 'expr expr)))))
(defun md-expand-pop-args (nbyte))

#| prints loading progress for this file |#
(defun loadmsg (msg &rest args) (when *debug* (apply 'format t msg args)))

#| math |#
(defun 2^n-p (n) (= (logcount n) 1))
(defun log2 (n) (loglength (rshu n 1)))

#| define registers |#
(loadmsg "0")

(defreg a.b i1 (conflict a.w) (param 0))
(defreg x.b i1 (conflict x.w) (param 1))
(defreg y.b i1 (conflict y.w) (param 2))
(defreg $2.b i1 (conflict $2.w))
(defreg $4.b i1 (conflict $4.w))
(defreg $6.b i1 (conflict $6.w))
(defreg $8.b i1 (conflict $8.w))
(defreg $a.b i1 (conflict $a.w))
(defreg $c.b i1 (conflict $c.w))
(defreg a.w i2 (conflict a.b) (param 0))
(defreg x.w i2 (conflict x.b) (param 1))
(defreg y.w i2 (conflict y.b) (param 2))
(defreg $2.w i2 (conflict $2.b))
(defreg $4.w i2 (conflict $4.b))
(defreg $6.w i2 (conflict $6.b))
(defreg $8.w i2 (conflict $8.b))
(defreg $a.w i2 (conflict $a.b))
(defreg $c.w i2 (conflict $c.b))

#| CSP = constraint satisfaction problem |#
(defcsp B-regs nil (a.b x.b y.b))
(defcsp W-regs nil (a.w x.w y.w))
(defcsp A-regs nil (a.w))
(defcsp M8-regs nil ($2.b $4.b $6.b $8.b $a.b $c.b))
(defcsp M16-regs nil ($2.w $4.w $6.w $8.w $a.w $c.w))
(defcsp W-regs-for-call-general nil ())
(setq *md-callee-save-regs* nil)
#| (setq *md-callee-save-regs* '(r20B r21B r22B r23B r24B r25B r20H r21H r22H r23H r24H r25H r20W r21W r22W r23W r24W r25W r20F r21F r22F r23F r24F r25F)) |#
(setq *md-caller-save-regs* '(a.w x.w y.w))
(defcsp caller-save-regs nil (a.w x.w y.w))
(dolist (regs '(B-regs W-regs)) (let ((W-regs (cdr (get 'W-regs 'defcsp)))) (dolist (r (cdr (get regs 'defcsp))) (put r 'W-reg (pop W-regs)))))
(dolist (regs '(M8-regs M16-regs)) (let ((M16-regs (cdr (get 'M16-regs 'defcsp)))) (dolist (r (cdr (get regs 'defcsp))) (put r 'M-reg (pop M16-regs)))))

(loadmsg "1")
#| addressing mode? |#
(defcsp am-sp (a) (auto i2 (= a)))
(defcsp am-const (c) (const i2 (= c * not-gpoffset-p)))
(defcsp am-gpoffset (g) (const i2 (= g * gpoffset-p)))
(defun gpoffset-p (x) (cond ((fixnum-p x) (and *gp-offset* (<= (- *gp-offset* 32767) x) (<= x (+ *gp-offset* 32767)))) ((consp x) (gpoffset-p (cadr x))) ((sent-p x) (if (eq (sent-scc x) 'extern) nil (memq (sent-segment x) '("bss")))) (t nil)))
(defun not-gpoffset-p (x) (not (gpoffset-p x)))
(defun one-p (x) (eq x 1))
(defcsp am-dx (d x) (add i2 (= x) (const i2 (= d))))
(defcsp co-fixnum (n) (const * (= n * fixnum-p)))
(defcsp co-fixnum-2^n-p (n) (const * (= n * 2^n-p)))
(defcsp co-zero (htype) (const htype (= z * zerop)))
(defcsp co-one (htype) (const htype (= c * one-p)))
(defun expr-not-reg-p (expr) (not (eq (expr-car expr) 'reg)))

(loadmsg "2")
(defcode getreg (get i1 (= x (reg))) (reg r))
(defcode getreg (get i2 (= x (reg))) (reg r))
(defcode setreg (set i1 (= x (reg)) (= y)) (reg nil y))
(defcode setreg (set i2 (= x (reg)) (= y)) (reg nil y))
(defcode load (set i1 (= x (hreg) B-regs) (get i1 am-sp))
    (gen nil
        (progn
            (cond
                ((areg-p x) (emit-mcode (list 'lda (list 'sp a))))
                ((xreg-p x) (emit-mcode (list 'ldx (list 'sp a))))
                ((yreg-p x) (emit-mcode (list 'ldy (list 'sp a))))
                (t (emit-mcode (list 'load (list 'sp a)))) ))))
(defcode load (set i2 (= x (hreg) W-regs) (get i2 am-sp))
    (gen nil
        (progn
            (cond
                ((areg-p x) (emit-mcode (list 'lda (list 'sp a))))
                ((xreg-p x) (emit-mcode (list 'ldx (list 'sp a))))
                ((yreg-p x) (emit-mcode (list 'ldy (list 'sp a))))
                (t (emit-mcode (list 'load (list 'sp a)))) ))))
(defcode store (set i1 am-sp (get i1 (= x (hreg) B-regs)))
    (gen nil
        (progn
            (cond
                ((areg-p x) (emit-mcode (list 'sta (list 'sp a))))
                ((xreg-p x) (emit-mcode (list 'stx (list 'sp a))))
                ((yreg-p x) (emit-mcode (list 'sty (list 'sp a))))
                (t (emit-mcode (list 'store (list 'sp a)))) ))))
(defcode store (set i2 am-sp (get i2 (= x (hreg) W-regs)))
    (gen nil
        (progn
            (cond
                ((areg-p x) (emit-mcode (list 'sta (list 'sp a))))
                ((xreg-p x) (emit-mcode (list 'stx (list 'sp a))))
                ((yreg-p x) (emit-mcode (list 'sty (list 'sp a))))
                (t (emit-mcode (list 'store (list 'sp a)))) ))))

(defcode move (set i1 (= x (hreg) B-regs) (get i1 (= y (hreg) B-regs)))
    (gen nil
        (progn
            (cond
                ((and (xreg-p x) (yreg-p y)) (emit-mcode 'tyx))
                ((and (xreg-p x) (areg-p y)) (emit-mcode 'tax))
                ((and (yreg-p x) (xreg-p y)) (emit-mcode 'txy))
                ((and (yreg-p x) (areg-p y)) (emit-mcode 'tay))
                ((and (areg-p x) (xreg-p y)) (emit-mcode 'txa))
                ((and (areg-p x) (yreg-p y)) (emit-mcode 'tya))
                (t (emit-mcode (list 'move-i1 (list 'i1 x) (list 'i1 y)))) ))))
(defcode move (set i1 (= x (hreg) B-regs) (get i1 (= y (hreg) M8-regs)))
    (gen nil
        (progn
            (cond
                ((areg-p x) (emit-mcode (list 'lda y)))
                ((xreg-p x) (emit-mcode (list 'ldx y)))
                ((yreg-p x) (emit-mcode (list 'ldy y))) ))))
(defcode move (set i1 (= x (hreg) M8-regs) (get i1 (= y (hreg) B-regs)))
    (gen nil
        (progn
            (cond
                ((areg-p y) (emit-mcode (list 'sta x)))
                ((xreg-p y) (emit-mcode (list 'stx x)))
                ((yreg-p y) (emit-mcode (list 'sty x))) ))))
(defcode move (set i2 (= x (hreg) W-regs) (get i2 (= y (hreg) W-regs)))
    (gen nil
        (progn
            (cond
                ((and (xreg-p x) (yreg-p y)) (emit-mcode 'tyx))
                ((and (xreg-p x) (areg-p y)) (emit-mcode 'tax))
                ((and (yreg-p x) (xreg-p y)) (emit-mcode 'txy))
                ((and (yreg-p x) (areg-p y)) (emit-mcode 'tay))
                ((and (areg-p x) (xreg-p y)) (emit-mcode 'txa))
                ((and (areg-p x) (yreg-p y)) (emit-mcode 'tya))
                (t (emit-mcode (list 'move-i2 (list 'i2 x) (list 'i2 y)))) ))))
(defcode move (set i2 (= x (hreg) W-regs) (get i2 (= y (hreg) M16-regs)))
    (gen nil
        (progn
            (cond
                ((areg-p x) (emit-mcode (list 'lda y)))
                ((xreg-p x) (emit-mcode (list 'ldx y)))
                ((yreg-p x) (emit-mcode (list 'ldy y))) ))))
(defcode move (set i2 (= x (hreg) M16-regs) (get i2 (= y (hreg) W-regs)))
    (gen nil
        (progn
            (cond
                ((areg-p y) (emit-mcode (list 'sta x)))
                ((xreg-p y) (emit-mcode (list 'stx x)))
                ((yreg-p y) (emit-mcode (list 'sty x))) ))))
(defcode push (set i1 (tos) (get i1 (= x (hreg) B-regs))) (gen nil (*push-i1 'x)))
(defcode push (set i2 (tos) (get i2 (= x (hreg) W-regs))) (gen nil (*push-i2 'x)))
(defcode pop (set i1 (= x (hreg) B-regs) (get i1 (tos))) (gen nil (*pop-i1 'x)))
(defcode pop (set i2 (= x (hreg) W-regs) (get i2 (tos))) (gen nil (*pop-i2 'x)))

(loadmsg "3")
(defcode geti1-general (get i1 (= p (* i2))) (reg r p) (gen ((r . B-regs) (p . W-regs)) (ld (dx 0 'p) 'r)))
(defcode geti1-dx (get i1 am-dx) (reg r x) (gen ((r . B-regs) (x . W-regs)) (ld (dx 'd 'x) 'r)))
(defcode geti1-const (get i1 am-const) (reg r) (gen ((r . B-regs)) (ld (dx 'c r0W) 'r)))
(defcode geti1-gp (get i1 am-gpoffset) (reg r) (gen ((r . B-regs)) (ld (gp 'g) 'r)))
(defcode geti1-sp (get i1 am-sp)
    (reg r)
    (gen ((r . B-regs))
        (progn
            (cond
                ((areg-p r) (emit-mcode (list 'lda (list 'sp a))))
                ((xreg-p r) (emit-mcode (list 'ldx (list 'sp a))))
                ((yreg-p r) (emit-mcode (list 'ldy (list 'sp a))))
                (t (emit-mcode (list 'geti1-sp (list 'sp a)))) ))))

(defcode geti2-general (get i2 (= p (* i2))) (reg r p) (gen ((r . W-regs) (p . W-regs)) (geti2-general (dx 0 'p) 'r)))
(defcode geti2-dx (get i2 am-dx)
    (reg r x)
    (gen ((r . A-regs) (x . W-regs))
    (lda (i1 'd) 'x)))

(defcode geti2-const (get i2 am-const)
    (reg r)
    (gen ((r . W-regs))
        (progn
            (cond
                ((areg-p r) (emit-mcode (list 'lda (list 'mem16 c))))
                ((xreg-p r) (emit-mcode (list 'ldx (list 'mem16 c))))
                ((yreg-p r) (emit-mcode (list 'ldy (list 'mem16 c)))) ))))
(defcode geti2-gp (get i2 am-gpoffset) (reg r) (gen ((r . W-regs)) (ld (gp 'g) 'r)))
(defcode geti2-sp (get i2 am-sp)
    (reg r)
    (gen ((r . W-regs))
        (progn
            (cond
                ((areg-p r) (emit-mcode (list 'lda (list 'sp a))))
                ((xreg-p r) (emit-mcode (list 'ldx (list 'sp a))))
                ((yreg-p r) (emit-mcode (list 'ldy (list 'sp a))))
                (t (emit-mcode (list 'geti2-sp (list 'sp a)))) ))))

(loadmsg "4")
(defcode setn-general
    (set (= n) (= p) (get * (= q)))
    (reg nil p q)
    (gen
        ((p . W-regs) (q . W-regs))
        (mov 'p r29W)
        (mov r0W r26W)
        (mov 'q r30W)
        (mov r0W r27W)
        (progn (emit-mcode (list 'mov (list 'i4 (* n 8)) 'r28W)))
        (movbsu)))

(defcode seti1-general
    (set i1 (= p (* i2)) (= r))
    (reg nil p r)
    (gen ((p . W-regs) (r . B-regs)) (sta 'r (dx 0 'p))))

(defcode seti1-general-zero
    (set i1 (= p (* i2)) (co-zero i1))
    (reg nil p)
    (gen ((p . W-regs)) (sta r0B (dx 0 'p))))

(defcode seti1-dx
    (set i1 am-dx (= r))
    (reg nil x r)
    (gen ((x . W-regs) (r . B-regs)) (sta 'r (dx 'd 'x))))

(defcode seti1-dx-zero
    (set i1 am-dx (co-zero i1))
    (reg nil x)
    (gen ((x . W-regs)) (sta r0B (dx 'd 'x))))

(defcode seti1-const
    (set i1 am-const (= r))
    (reg nil r)
    (gen ((r . B-regs)) (sta 'r (dx 'c r0W))))

(defcode seti1-gp
    (set i1 am-gpoffset (= r))
    (reg nil r)
    (gen ((r . B-regs)) (sta 'r (gp 'g))))

(defcode seti1-gp-zero
    (set i1 am-gpoffset (co-zero i1))
    (reg)
    (gen nil (sta r0B (gp 'g))))

(defcode seti1-sp (set i1 am-sp (= r))
    (reg nil r)
    (gen ((r . B-regs))
        (progn
            (cond
                ((areg-p r) (emit-mcode (list 'sta (list 'sp a))))
                ((xreg-p r) (emit-mcode (list 'stx (list 'sp a))))
                ((yreg-p r) (emit-mcode (list 'sty (list 'sp a))))
                (t (emit-mcode (list 'seti1-sp (list 'sp a)))) ))))

(defcode seti1-sp-zero
    (set i1 am-sp (co-zero i1))
    (reg)
    (gen nil (sta r0B (sp 'a))))

(defcode seti2-general
    (set i2 (= p (* i2)) (= r))
    (reg nil p r)
    (gen ((p . W-regs) (r . W-regs)) (seti2-general 'r (dx 0 'p))))

(defcode seti2-general-convit (set i2 (= p (* i2)) (convit i2 (= r))) (reg nil p r) (gen ((p . W-regs) (r . W-regs)) (sta 'r (dx 0 'p))))
(defcode seti2-general-zero (set i2 (= p (* i2)) (co-zero i2)) (reg nil p) (gen ((p . W-regs)) (stz 'p)))
(defcode seti2-dx (set i2 am-dx (= r))
    (reg nil x r)
    (gen ((x . W-regs) (r . A-regs))
        (sta (mem16 'd) 'x)))
(defcode seti2-dx-convit (set i2 am-dx (convit i2 (= r))) (reg nil x r) (gen ((x . W-regs) (r . W-regs)) (sta 'r (dx 'd 'x))))
(defcode seti2-zero (set i2 (co-zero i2)) (reg nil x) (gen ((x . W-regs)) (stz 'x)))
(defcode seti2-const (set i2 am-const (= r))
    (reg nil r)
    (gen ((r . W-regs))
        (progn
            (cond
                ((areg-p r) (emit-mcode (list 'sta (list 'mem16 c))))
                ((xreg-p r) (emit-mcode (list 'stx (list 'mem16 c))))
                ((yreg-p r) (emit-mcode (list 'sty (list 'mem16 c))))
                (t (emit-mcode (list 'seti2-const (list 'mem16 c)))) ))))
(defcode seti2-const-zero (set i2 am-const (= r (co-zero i2)))
    (reg nil)
    (gen nil
    (stz.w (mem16 'c))))

(defcode seti2-const-convit (set i2 am-const (convit i2 (= r))) (reg nil r) (gen ((r . W-regs)) (sta 'r (dx 'c r0W))))
(defcode seti2-gp (set i2 am-gpoffset (= r)) (reg nil r) (gen ((r . W-regs)) (sta 'r (gp 'g))))
(defcode seti2-gp-convit (set i2 am-gpoffset (convit i2 (= r))) (reg nil r) (gen ((r . W-regs)) (sta 'r (gp 'g))))
(defcode seti2-gp (set i2 am-gpoffset (co-zero i2)) (reg) (gen nil (sta r0W (gp 'g))))
(defcode seti2-sp (set i2 am-sp (= r))
    (reg nil r)
    (gen ((r . W-regs))
        (progn
            (cond
                ((areg-p r) (emit-mcode (list 'sta (list 'sp a))))
                ((xreg-p r) (emit-mcode (list 'stx (list 'sp a))))
                ((yreg-p r) (emit-mcode (list 'sty (list 'sp a)))) ))))
(defcode seti2-sp-zero (set i2 am-sp (co-zero i2)) (reg) (gen nil (sta r0W (sp 'a))))

(loadmsg "5")
(defcode consti1-general (const i1 (= c)) (reg r)
    (gen ((r . B-regs))
        (progn
            (ecase r
                ('a.b (emit-mcode (list 'lda (list 'i1 c))))
                ('x.b (emit-mcode (list 'ldx (list 'i1 c))))
                ('y.b (emit-mcode (list 'ldy (list 'i1 c))))
                (t (emit-mcode (list 'consti1-general (list 'i1 c)))) ))))
(defcode consti2-general (const i2 (= c)) (reg r)
    (gen ((r . W-regs))
        (progn
            (ecase r
                ('a.w (emit-mcode (list 'lda (list 'i2 c))))
                ('x.w (emit-mcode (list 'ldx (list 'i2 c))))
                ('y.w (emit-mcode (list 'ldy (list 'i2 c))))
                (t (emit-mcode (list 'consti2-general (list 'i2 c)))) ))))
(loadmsg "6")
(defcode auto-general (auto i2 (= a)) (reg r) (gen ((r . W-regs)) (movea (i2 'a) sp 'r)))

#| type conversions |#
(loadmsg "7")
(defcode convsxi2/i1-general (convsx i2 (= s (* i1))) (reg r s) (gen ((r . W-regs) (s . B-regs)) (mov 's 'r)))
(defcode convzxi2/i1-general (convzx i2 (= s (* i1))) (reg r s) (gen ((r . W-regs) (s . B-regs)) (andi (i2 255) 's 'r)))
(defcode conviti1/i2-general (convit i1 (= s (* i2))) (reg r s) (gen ((r . B-regs) (s . W-regs)) (mov 's 'r) (shl 24 'r) (sar 24 'r)))

(loadmsg "8")

(defcode negi2-general (neg i2 (= r)) (reg r r) (gen ((r . W-regs)) (not 'r 'r) (add (i2 1) 'r)))
(defcode addi2-general (add i2 (= r) (= p (reg)))
    (reg r r p)
    (gen ((r . A-regs) (p . M16-regs))
    (adc 'p)))
(defcode addi1-const (add i1 (= r) (const i1 (= c)))
    (reg r r)
    (gen ((r a.b))
        (progn
            (if (and (< c 5) (> c 0))
                (dotimes (i c)
                    (emit-mcode 'inc))
                    (progn
                        (emit-mcode (list 'clc))
                        (emit-mcode (list 'adc (list 'i1 c))) )))))
(defcode addi1-general (add i1 (= r) (= p (reg)))
    (reg r r p)
    (gen ((r a.b) (p . M8-regs))
    (adc 'p)))
(defcode addi2-const (add i2 (= r) (const i2 (= c)))
    (reg r r)
    (gen ((r a.w))
        (progn
            (if (and (< c 5) (> c 0))
                (dotimes (i c)
                    (emit-mcode 'inc))
                    (progn
                        (emit-mcode (list 'clc))
                        (emit-mcode (list 'adc (list 'i1 c))) )))))
(defcode subi2-general (sub i2 (= r) (= s))
    (reg r r s)
    (gen ((r . A-regs) (s . M16-regs))
    (sec)
    (sbc.w 's)))

(defcode subi2-const (sub i2 (= r) (const i2 (= c)))
    (reg r)
    (gen ((r . A-regs))
    (progn
        (emit-mcode (list 'sec))
        (emit-mcode (list 'sbc (list 'i2 c))) )))

(defcode bandi2-general (band i2 (= r) (= p)) (reg r r p) (gen ((r . W-regs) (p . W-regs)) (and 'p 'r)))
(defcode bandi2-const (band i2 (= x) (const i2 (= c))) (reg r x) (gen ((r . W-regs) (x . W-regs)) (progn (if (eq r x) (emit-mcode (list 'and (list 'i4 c) r)) (emit-mcode (list 'andi (list 'i4 c) x r))))))
(defcode bxori2-general (bxor i2 (= r) (= p)) (reg r r p) (gen ((r . W-regs) (p . W-regs)) (xor 'p 'r)))
(defcode bxori2-const (bxor i2 (= x) (const i2 (= c))) (reg r x) (gen ((r . W-regs) (x . W-regs)) (progn (if (eq r x) (emit-mcode (list 'xor (list 'i4 c) r)) (emit-mcode (list 'xori (list 'i4 c) x r))))))
(defcode bori2-general (bor i2 (= r) (= p)) (reg r r p) (gen ((r . W-regs) (p . W-regs)) (or 'p 'r)))
(defcode bori2-const (bor i2 (= x) (const i4 (= c))) (reg r x) (gen ((r . W-regs) (x . W-regs)) (progn (if (eq r x) (emit-mcode (list 'or (list 'i4 c) r)) (emit-mcode (list 'ori (list 'i4 c) x r))))))
(defcode bnoti2-general (bnot i2 (= r)) (reg r r) (gen ((r . W-regs)) (not 'r 'r)))
(defcode muli2-general (mul i2 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (mul 's 'r)))
(defcode muli2-const (mul i2 (= r) (co-fixnum n)) (reg r r) (gen ((r . W-regs)) (mul 'n 'r)))
(defcode muli2-2^n (mul i2 (= r) (co-fixnum-2^n-p n))
    (reg r r)
    (gen ((r . A-regs))
        (progn
            (dotimes (i (log2 n))
                (emit-mcode 'asl)))))
(defcode mulf4-general (mul f4 (= r) (= s)) (reg r r s) (gen ((r . F-regs) (s . F-regs)) (mulf.s 's 'r)))
(defcode divi4-general (div i4 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (div 's 'r)))
(defcode divi4-2^n (div i4 (= r) (co-fixnum-2^n-p n)) (reg r r) (gen ((r . W-regs)) (progn (emit-mcode (list 'sar (list 'i4 (log2 n)) r)))))
(defcode divf4-general (div f4 (= r) (= s)) (reg r r s) (gen ((r . F-regs) (s . F-regs)) (divf.s 's 'r)))
(defcode divui4-general (divu i4 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (divu 's 'r)))
(defcode divui4-2^n (divu i4 (= r) (co-fixnum-2^n-p n)) (reg r r) (gen ((r . W-regs)) (progn (emit-mcode (list 'shr (list 'i4 (log2 n)) r)))))
(defcode modi4-general (mod i4 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (div 's 'r) (mov r30W 'r)))
(defcode modui4-general (modu i4 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (divu 's 'r) (mov r30W 'r)))
(defcode lshi2-general (lsh i2 (= r) (= s)) (reg r r s)
    (gen ((r . W-regs) (s . W-regs))
    (progn
        (emit-mcode (list 'asl 's 'r)))))
(defcode lshi2-const (lsh i2 (= r) (co-fixnum c)) (reg r r)
    (gen ((r . A-regs))
    (progn
        (dotimes (i c)
            (emit-mcode 'asl)
        ))))
(defcode rshi4-general (rsh i4 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (sar 's 'r)))
(defcode rshi4-const (rsh i4 (= r) (co-fixnum c)) (reg r r) (gen ((r . W-regs)) (sar (i4 'c) 'r)))
(defcode rshui4-general (rshu i4 (= r) (= s)) (reg r r s) (gen ((r . W-regs) (s . W-regs)) (shr 's 'r)))
(defcode rshui4-general (rshu i4 (= r) (co-fixnum c)) (reg r r) (gen ((r . W-regs)) (shr (i4 'c) 'r)))

(loadmsg "9")

(defcsp jcode (jmc) (= jmc * ((tsteq jmp-eq) (tstne jmp-ne) (tstlt jmp-lt) (tstltu jmp-ltu) (tstle jmp-le) (tstleu jmp-leu) (tstgt jmp-gt) (tstgtu jmp-gtu) (tstge jmp-ge) (tstgeu jmp-geu))))
(defcode jump1 (jump1 la) (gen nil (jmp-1 'la)))

#|
(convsx i2
    (get i1 (reg "T-0"))))

2:(jump2
     (tstge i2 
            (get i2 (add i2 (get i2 (const i2 "MEM_4DC6")) (const i2 62))) 
            (get i2 (reg "t.1")))
            "L1")
|#

(defcode jump2i2-general (jump2 ((jcode jmc) * (= r (* i2)) (= p (* i2))) la)
    (reg nil r p)
    (gen ((r . A-regs) (p . M16-regs))
        (progn
            (cond
                ((areg-p r) (emit-mcode (list 'cmp p)))
                ((xreg-p r) (emit-mcode (list 'cpx p)))
                ((yreg-p r) (emit-mcode (list 'cpy p)))
                (t (emit-mcode (list 'jump2i2-mem p))))
            (emit-mcode (list jmc la)) )))

(defcode jump2i2-mem (jump2 ((jcode jmc) * (= r (* i2)) (get i2 (const i2 (= p)))) la)
    (reg nil r)
    (gen ((r . W-regs))
        (progn
            (cond
                ((areg-p r) (emit-mcode (list 'cmp (list 'mem16 p))))
                ((xreg-p r) (emit-mcode (list 'cpx (list 'mem16 p))))
                ((yreg-p r) (emit-mcode (list 'cpy (list 'mem16 p))))
                (t (emit-mcode (list 'jump2i2-mem (list 'mem16 p)))))
            (emit-mcode (list jmc la)) )))

(defcode jump2i2-const (jump2 ((jcode jmc) * (= r (* i2)) (co-fixnum c)) la)
    (reg nil r)
    (gen ((r . W-regs))
        (progn
            (cond
                ((areg-p r) (emit-mcode (list 'cmp (list 'i2 c))))
                ((xreg-p r) (emit-mcode (list 'cpx (list 'i2 c))))
                ((yreg-p r) (emit-mcode (list 'cpy (list 'i2 c))))
                (t (emit-mcode (list 'jump2i2-const (list 'i2 c)))))
            (emit-mcode (list jmc la)) )))

#|
(defcode convsxi2/i1-general (convsx i2 (= s (* i1)))
    (reg r s)
    (gen ((r . W-regs) (s . B-regs)
        #| no op |#)))
|#

#|
(defcode getreg
    (get i1 (= x (reg)))
    (reg r))
|#

(defcode jump2f4-general (jump2 ((jcode jmc) * (= r (* f4)) (= p (* f4))) la) (reg nil r p) (gen ((r . F-regs) (p . F-regs)) (cmpf.s 'p 'r) ('jmc 'la)))
(defcode jumpn (jumpn (= r) calist) (reg nil r) (gen ((r . W-regs)) (progn (emit-jumpn r calist))))
(defun emit-jumpn (reg calist) (dolist (c calist) (setq (sent-scc (cadr c)) 'caselabel)) (let ((dlabel (genlabel))) (setq (sent-scc dlabel) 'caselabel) (emit-jumpn-rec reg calist dlabel) (emit-mcode (list 'def dlabel))))
(defun emit-jumpn-rec (reg calist dlabel) (cond ((emit-jumpn-by-table-p calist) (emit-jumpn-by-table reg calist dlabel)) ((emit-jumpn-by-liner-p calist) (emit-jumpn-by-liner reg calist dlabel)) (t (let ((tmpl (genlabel))) (letl (calist1 center calist2) (emit-jumpn-split-calist calist) (emit-mcode (list 'cmp (list 'i4 (car center)) reg)) (emit-mcode (list 'jmp-eq (cadr center))) (emit-mcode (list 'jmp-gt tmpl)) (emit-jumpn-rec reg calist1 dlabel) (emit-mcode (list 'def tmpl)) (emit-jumpn-rec reg calist2 dlabel))))))
(defun emit-jumpn-by-table-p (calist) (let ((min (caar calist)) (max (caar (last calist))) (len (length calist))) (and (<= 5 len) (<= (- max min) (* 3 len)))))
(defun emit-jumpn-by-liner-p (calist) (<= (length calist) 2)) #| emit linear jumps if <= 2 |#
(defun emit-jumpn-by-liner (reg calist dlabel) (dolist (c calist) (emit-mcode (list 'cmp (list 'i4 (car c)) reg)) (emit-mcode (list 'jmp-eq (cadr c)))) (when dlabel (emit-mcode (list 'jmp-1 dlabel))))
(defun emit-jumpn-split-calist (calist) (let ((len (length calist)) x y) (unless (<= 3 len) (clerror "emit-jumpn-split-calist: unexpedted list length")) (setq y (nthcdr (/ len 2) calist) x (ldiff calist y)) (list x (car y) (cdr y))))
(defvar *mdl-jumpn-table-id* 0)
(defvar *mdl-jumpn-table-list* nil)
(defun emit-jumpn-by-table (reg calist dlabel) (let ((min (caar calist)) (max (caar (last calist))) table) (cond ((zerop min) (emit-mcode (list 'mov reg 'r30W))) (t (decf max min) (emit-mcode (list 'addi (list 'i4 (- min)) reg 'r30W)) (dolist (c calist) (decf (car c) min)))) (setq table (make-jumpn-table calist dlabel)) (emit-mcode (list 'cmp (list 'i4 max) 'r30W)) (emit-mcode (list 'jmp-gtu dlabel)) (emit-mcode '(shl (i4 2) r30W)) (emit-mcode (list 'ld.w (list 'dx table 'r30W) 'r30W)) (emit-mcode '(jmp (dx 0 r30W)))))
(defun make-jumpn-table (calist dlabel) (let ((tlabel (genlabel)) (table (make-tconc)) (n 0)) (setq (sent-asmname tlabel) (format nil "LS%d" (incf *mdl-jumpn-table-id*))) (dolist (c calist) (while (< n (car c)) (tconc table dlabel) (incf n)) (tconc table (cadr c)) (incf n)) (push (cons tlabel (tconc-list table)) *mdl-jumpn-table-list*) tlabel))
(defun emit-jumpn-tables nil (dolist (table *mdl-jumpn-table-list*) (emit-acode-changeseg "const") (emit-acode-align 4) (emit-acode-def (sent-asmname (car table))) (dolist (e (cdr table)) (emit-acode-data 'i4 e))) (setq *mdl-jumpn-table-list* nil))
(defcode label (label la) (gen nil (def 'la)))

(loadmsg "a")
(extern *cfun-name* *cfun-args-htype* *cfun-return-htype* *cfun-ahtype* *cfun-param-list* *cfun-local-list* *cfun-local-size* *cfun-tmp-list* *cfun-tmp-size* *cfun-args-size* *cfun-is-leaf-p*)
(defvar *mdl-frame-size* 0)
(defvar *mdl-arg* 0)
(defvar *mdl-hreg-save-area* 0)
(defvar *mdl-hreg-save-area-size* 0)

(defcode prologue (prologue)
    (gen nil
    (progn
        #| calculate stack etc |#
        (setq *mdl-arg* 0)
        (let ((frame 0))
            (incf frame *cfun-args-size*)
            (dolist (s *cfun-local-list*) (incf (sent-offset s) frame))
            (incf frame *cfun-local-size*)
            (dolist (r *cfun-hreg-list*)
                (let (res)
                    (if (get r 'M-reg)
                        (setq *mdl-arg* *mdl-stack-arg-offset*))
                    (setq r (get r 'W-reg))
                    (unless (memq r res) (push r res))
                    (setq *cfun-hreg-list* res)))
            (incf frame *mdl-arg*)
            (dolist (s *cfun-tmp-list*) (incf (sent-offset s) frame))
            (incf frame *cfun-tmp-size*)
            #| hardware registers used by the function |#
            (setq *cfun-hreg-list* (delq 'nil *cfun-hreg-list*))
            (dolist (reg *md-caller-save-regs*) (setq *cfun-hreg-list* (delq reg *cfun-hreg-list*)))
            (setq *mdl-hreg-save-area* frame *mdl-hreg-save-area-size* (* 4 (length *cfun-hreg-list*)))
            (incf frame *mdl-hreg-save-area-size*)
            #| (unless *cfun-is-leaf-p* (incf frame 4)) |#
            (setq *mdl-frame-size* frame)
            (dolist (s *cfun-param-list*)
                (incf (sent-offset s) (+ *mdl-frame-size* *mdl-stack-arg-offset*)))))
        #| print info |#
        (progn
            (emit-mcode 'comment (format nil "framesize %d = %d(args) + %d(local) + %d(tmp) + %d(saveregs) + %d(savelp)" *mdl-frame-size* *cfun-args-size* *cfun-local-size* *cfun-tmp-size* *mdl-hreg-save-area-size* (if *cfun-is-leaf-p* 0 0)))
            (emit-mcode 'comment (format nil "used callee save regs = %t" *cfun-hreg-list*))
            (dolist (s *cfun-param-list*) (emit-mcode 'comment (format nil "param %T = (auto %d)" (sent-name s) (sent-offset s))))
            (dolist (s *cfun-local-list*) (emit-mcode 'comment (format nil "local %T = (auto %d)" (sent-name s) (sent-offset s))))
            (dolist (s *cfun-tmp-list*) (emit-mcode 'comment (format nil "tmp   %T = (auto %d)" (sent-name s) (sent-offset s)))))
        #| emit code |#
        (progn
            (emit-mcode (list 'rep (list 'i1 31)))
            (unless (zerop *mdl-frame-size*)
                (emit-mcode 'phd)
                (emit-mcode 'pha)
                (emit-mcode 'tdc)
                (emit-mcode (list 'adc (- *mdl-frame-size*)))
                (emit-mcode 'tcd)
                (emit-mcode 'pla)
                )
            #| (unless (zerop *mdl-frame-size*) (emit-mcode (list 'add (list 'i4 (- *mdl-frame-size*)) 'sp))) |#
            #| (unless *cfun-is-leaf-p* (emit-mcode (list 'st.w 'lp (list 'sp (list 'sub *mdl-frame-size* 4))))) |#
            (unless (zerop *mdl-hreg-save-area-size*) (let ((off *mdl-hreg-save-area*)) (dolist (reg *cfun-hreg-list*) (emit-mcode (list 'st.w reg (list 'sp (posincf off 4)))))))
            )))

(defcode epilogue (epilogue)
    (gen nil
    (progn
        #| (unless *cfun-is-leaf-p* (emit-mcode (list 'ld.w (list 'sp (list 'sub *mdl-frame-size* 4)) 'lp))) |#
        #| (unless (zerop *mdl-hreg-save-area-size*) (let ((off *mdl-hreg-save-area*)) (dolist (reg *cfun-hreg-list*) (emit-mcode (list 'ld.w (list 'sp (posincf off 4)) reg))))) |#
        #| (unless (zerop *mdl-frame-size*) (emit-mcode (list 'add (list 'i4 *mdl-frame-size*) 'sp)))) |#
        (unless (zerop *mdl-frame-size*) (emit-mcode 'pld)))
    (rtl)))

(defcode pusharg (set i1 (arg) (= x)) (reg nil x) (gen ((x . B-regs)) (progn (emit-mcode (list 'sta x (list 'sp *mdl-arg*))) (setq *mdl-arg* (+ *mdl-arg* 2)))))
(defcode pusharg (set i2 (arg) (= x)) (reg nil x) (gen ((x . W-regs)) (progn (emit-mcode (list 'sta x (list 'sp *mdl-arg*))) (setq *mdl-arg* (+ *mdl-arg* 2)))))
(defcode poparg (set (= n) (null) (get * (arg))) (gen nil (progn (setq *mdl-arg* (- *mdl-arg* n)))))
(defcode calli1-general (call i1 (= f)) (reg r f) (gen ((r a.b) (f . W-regs-for-call-general) (use . caller-save-regs)) (mov (i4 (add * 10)) lp) (jmp (dx 0 'f))))
(defcode calli2-general (call i2 (= f)) (reg r f) (gen ((r a.w) (f . W-regs-for-call-general) (use . caller-save-regs)) (mov (i4 (add * 10)) lp) (jmp (dx 0 'f))))
(defcode callvoid-general (call void (= f)) (reg r f) (gen ((r a.w) (f . W-regs-for-call-general) (use . caller-save-regs)) (mov (i4 (add * 10)) lp) (jmp (dx 0 'f))))
(defcode calli1-const (call i1 (const i2 (= f))) (reg r) (gen ((r a.b) (use . caller-save-regs)) (jsl (label 'f))))
(defcode calli2-const (call i2 (const i2 (= f))) (reg r) (gen ((r a.w) (use . caller-save-regs)) (jsl (label 'f))))
(defcode callvoid-const (call void (const i2 (= f))) (reg r) (gen ((r a.w) (use . caller-save-regs)) (jsl (label 'f))))
(defcode ret-i1 (set i1 (ret) (= r)) (reg nil r) (gen ((r a.b))))
(defcode ret-i2 (set i2 (ret) (= r)) (reg nil r) (gen ((r a.w))))

(loadmsg "b")
(defun peephole-optimize (bblockh))
(load "md-v810p.cl")
(setq *mcode-bra-offset* nil)

(loadmsg "c")
(setq *md-no-underscore* nil)
(defvar *mdl-lstatic/string-id* 0)
(defun make-asm-name (name class) (ecase class ((extern global static) (if *md-no-underscore* name (format nil "_%s" name))) (lstatic (format nil "%s@%d" name (incf *mdl-lstatic/string-id*))) (string (format nil "%s@%d" name (incf *mdl-lstatic/string-id*)))))
(defun emit-acode-beginning-of-object nil (setq *mdl-float-literal-alist* nil *mdl-float-literal-id* 0) (emit-acode ";;; source = %T\n" (file-name-change-suffix (stream-name *objsm*) ".c")) (emit-acode ";;; cparse = (%T %T %T)\n" *cparse-version* *ld-name* *ld-version*) (emit-acode ";;; cgrind = (%T %T %T)\n" *cgrind-version* *md-name* *md-version*) (dolist (sym '(*command-line-args* *optimize* *debug*)) (let ((vals (symbol-value sym))) (when vals (emit-acode ";;; %t = (" sym) (dolist (a vals) (emit-acode "\n;;;     %t" a)) (emit-acode ")\n")))) (emit-acode "\n") (emit-acode "isv810\n") (emit-acode "rsvreg1\n") (emit-acode "capsoff\n") (emit-acode "aligndefeven\n") (emit-acode "lprefix' '\n") (emit-acode "decon   branch\n") (emit-acode "oncnum\n") (if *gp-offset* (emit-acode "assumegp,$%x\n" *gp-offset*) (progn (emit-acode "externgp_offset\n") (emit-acode "assumegp,gp_offset\n"))) (when *debugger* (emit-acode-debug-info-type)) (emit-acode "\n"))
(defun emit-acode-debug-info-type nil (emit-acode "\n;;; type define table\n") (emit-acode "DB@T_COUNTequ0\n") (emit-acode "DB@T_ATOMequ1\n") (emit-acode "DB@T_PTRequ2\n") (emit-acode "DB@T_VECTequ3\n") (emit-acode "DB@T_STRUCTequ4\n") (emit-acode "DB@T_UNIONequ5\n") (emit-acode "DB@T_ENUMequ6\n") (emit-acode "DB@T_FUNCequ7\n") (emit-acode "DB@T_TYPENAMEequ8\n") (emit-acode "DB@T_STRUCT_MEMBERequ0x10|DB@T_STRUCT\n") (emit-acode "DB@T_UNION_MEMBERequ0x10|DB@T_UNION\n") (emit-acode "DB@T_ENUM_MEMBERequ0x10|DB@T_ENUM\n") (emit-acode "DB@T_FUNC_ARGequ0x10|DB@T_FUNC\n") (emit-acode "\n;;; type table\n") (emit-acode "dbtypeDB@T_COUNT,%d ; Number of types.\n" (length *typevector*)) (dotimes (i (length *typevector*)) (let ((type (vref *typevector* i))) (if (< i 10) (progn (emit-acode "dbtypeDB@T_ATOM,") (emit-acode (ecase type (char "\"char\",1,1") (short "\"short\",1,2") (int "\"int\",1,4") (long "\"long\",1,4") (uchar "\"unsigned char\",0,1") (ushort "\"unsigned short\",0,2") (uint "\"unsigned int\",0,4") (ulong "\"unsigned long\",0,4") (float "\"float\",2,4") (void "\"void\",3,4"))) (emit-acode "%55i; #%d\n" i)) (progn (ecase (car type) (type (emit-acode "dbtypeDB@T_TYPENAME,%t,%d %55i; #%d\n" (cadr type) (caddr type) i)) (pointer (emit-acode "dbtypeDB@T_PTR,%d %55i; #%d\n" (cadr type) i)) (vector (emit-acode "dbtypeDB@T_VECT,%d,%d %55i; #%d\n" (caddr type) (or (cadr type) 0) i)) #'(let ((argtypes (delq '&rest (copy-list (cadr type))))) (emit-acode "dbtypeDB@T_FUNC,%d,%d %55i; #%d\n" (caddr type) (length argtypes) i) (dolist (a argtypes) (emit-acode "dbtypeDB@T_FUNC_ARG,%d\n" a))) ((struct union) (emit-acode "dbtype%t,%t,%d,%d %55i; #%d\n" (ecase (car type) (struct 'DB@T_STRUCT) (union 'DB@T_UNION)) (or (cadr type) "") (length (cdddr type)) (or (caddr type) 0) i) (dolist (m (cdddr type)) (ecase (car type) (struct (emit-acode "dbtypeDB@T_STRUCT_MEMBER,%t,%d,%d,%d\n" (car m) (cadr m) (nth 2 m) (nth 3 m))) (union (emit-acode "dbtypeDB@T_UNION_MEMBER,%t,%d\n" (car m) (cadr m)))))) (enum (emit-acode "dbtypeDB@T_ENUM,%t,%d %55i; #%d\n" (or (cadr type) "") (length (cddr type)) i) (dolist (m (cddr type)) (emit-acode "dbtypeDB@T_ENUM_MEMBER,%t,%d\n" (car m) (cadr m))))))))))
(defvar *md-notify-room* nil)
(defun emit-acode-end-of-object nil (emit-acode "\nend\n") (when *md-notify-room* (room t)))
(defun emit-acode-beginning-of-function (name) (emit-acode "\n;;; FUNCTION %T\n" name))
(defun emit-acode-end-of-function nil (emit-jumpn-tables))
(defun emit-acode-static-variable (sent value) (if (eq (sent-segment sent) "bss") (progn (unless (eq *current-segment-name* "bss") (emit-acode-changeseg "bss") (setq *current-segment-name* "bss")) (emit-acode "%s%t%d,%d\n" (sent-asmname sent) (if (eq (sent-scc sent) 'global) 'comm 'comm) (sizeof-htype (sent-htype sent)) (sent-align sent)) t) nil))
(defvar *mdl-emitted-segment-name-list* nil)
(defun emit-acode-changeseg (segname) (when (and segname (not (eq segname *current-segment-name*))) (emit-acode "%Tgroup\n" segname) (unless (memq segname *mdl-emitted-segment-name-list*) (push segname *mdl-emitted-segment-name-list*) (unless (memq segname '("text" "bss")) (emit-acode-align 4))) (setq *current-segment-name* segname)))
(defun emit-acode-align (align) (emit-acode "align%d\n" align))
(defun emit-acode-def (name) (when (sent-p name) (setq name (sent-asmname name))) (emit-acode "%T:\n" name))
(defun emit-acode-xdef (name) (emit-acode "public%T\n" name) (emit-acode "%T:\n" name))
(defun emit-acode-extern (name) (emit-acode "extern%T\n" name))
(defun emit-acode-space (nbyte) (emit-acode "ds%d\n" nbyte))
(defun emit-acode-zeros (nbyte) (let ((n 0)) (dotimes (b nbyte) (if (zerop (% n 15)) (emit-acode "%ndb") (emit-acode ",")) (emit-acode "0") (incf n)) (emit-acode "\n")))
(defun emit-acode-string (bytelist) (let ((n 0)) (dolist (b bytelist) (if (zerop (% n 15)) (emit-acode "%ndb") (emit-acode ",")) (emit-acode "%d" b) (incf n)) (emit-acode "\n")))
(defun emit-acode-data (htype data)
    (unless (fixnum-p data) (setq data (conv-asmconst-to-infix data)))
    (ecase htype
        (i1 (emit-acode "db%T\n" data))
        (i2 (emit-acode "dh%T\n" data))
        (i4 (emit-acode "dw%T\n" data))
        (f4 (emit-acode "dw%T\n" data))))

(defun emit-acode-jmp-bra (code label)
    (if *debug* (emit-acode "%t%T\n" code label)
    (emit-acode
        (ecase code
            (jmp-1 "bra%T\n")
            (jmp-eq "beq%T\n")
            (jmp-ne "bne%T\n")
            (jmp-lt "bcc%T\n")
            (jmp-ltu "bl%T\n")
            (jmp-le "ble%T\n")
            (jmp-leu "bnh%T\n")
            (jmp-gt "bgt%T\n")
            (jmp-gtu "bh%T\n")
            (jmp-ge "bcs%T\n")
            (jmp-geu "bnl%T\n"))
        label)))
(defun emit-acode-debug-info-code (info) (ecase (car info) (file (emit-acode "dbsrc%s\n" (cadr info))) (line (emit-acode "dbline%d\n" (cadr info))) (begin (emit-acode "B@%d:\n" (cadr info))) (end (emit-acode "E@%d:\n" (cadr info)))))
(defun emit-acode-debug-info-scope (sid) (if (zerop sid) (emit-acode "\ndbscope0,0\n") (emit-acode "dbscopeB@%d,E@%d\n" sid sid)))
(defun emit-acode-debug-info-sent (name class typeid place) (if (eq class 'type) (let ((space (if (consp name) 9 8)) (name (if (consp name) (cadr name) name))) (emit-acode "dbivar%t,%d,%t,0\n" name typeid space)) (progn (emit-acode "dbivar%t,%d,%d," name typeid (ecase class (global 0) (static 1) (lstatic 4) (auto 5) (register 6))) (ecase class ((global static lstatic) (emit-acode "%s" place)) (auto (emit-acode "%d" place)) (register (emit-acode "%s" (substring (symbol-name place) 1 -1)))) (emit-acode "\n"))))
(defun emit-acode-mcode (mcode)
    (let ((code (mcode-code mcode)) (args (mcode-args mcode)) (first t))
        (emit-acode "%t" code)
        (dolist (a args)
            (emit-acode (if first "" ","))
            (setq first nil)
            (cond
                ((symbol-p a) (emit-acode "%s" (emit-acode-mcode-regname a)))
                ((fixnum-p a) (emit-acode "$%T" a))
                (t
                    (ecase (car a)
                        ((label) (emit-acode "%s" (conv-asmconst-to-infix (cadr a))))
                        ((i1 i2 i4 f4) (emit-acode "#$%s" (conv-asmconst-to-infix (cadr a))))
                        ((mem8 mem16) (emit-acode "$%s" (emit-acode-mcode-offset (cadr a))))
                        ((sp gp) (emit-acode "$%s" (emit-acode-mcode-offset (cadr a))))
                        (dx (emit-acode "%s[%s]" (emit-acode-mcode-offset (cadr a)) (emit-acode-mcode-regname (caddr a)))) ))))
        (emit-acode "\n")))
(defun emit-acode-mcode-offset (o) (if (zerop o) "" (conv-asmconst-to-infix o)))
(defun emit-acode-mcode-regname (regsym)
    (let ((regname (symbol-name regsym)))
        (when (and (not *debug*) (not (memq regsym '(sp gp lp tp))))
            (setq regname (substring regname 0 -2))) regname))
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
