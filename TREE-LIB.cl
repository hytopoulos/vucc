(defmacro extern (&rest syms))(defmacro prog1 (form1 &rest forms) (let ((tmp (gensym))) (list* 'let* (list (list tmp form1)) (append forms (list tmp)))))(defmacro push (x place) (list 'pref (list 'cons x place) 1))(defmacro pop (place) (list 'car (list 'posf (list 'cdr place))))(defmacro incf (place &optional n) (if n (list 'pref (list '+ place n)) (list 'pref (list '1+ place))))(defmacro posincf (place &optional n) (if n (list 'posf (list '+ place n)) (list 'posf (list '1+ place))))(defmacro decf (place &optional n) (if n (list 'pref (list '- place n)) (list 'pref (list '1- place))))(defmacro posdecf (place &optional n) (if n (list 'posf (list '- place n)) (list 'posf (list '1- place))))(defmacro for ((init test inc &optional ret) &rest body) (list 'let* init (list* 'while test (append body (list inc))) ret))(defvar *gensym-counter* 0)

(defun gensym nil (make-symbol (format nil "g%d" (incf *gensym-counter*))))

(defun set (sy val) (setq (symbol-value sy) val))

(defun caaar (x) (car (caar x)))

(defun caadr (x) (car (cadr x)))

(defun cadar (x) (car (cdar x)))

(defun caddr (x) (car (cddr x)))

(defun cdaar (x) (cdr (caar x)))

(defun cdadr (x) (cdr (cadr x)))

(defun cddar (x) (cdr (cdar x)))

(defun cdddr (x) (cdr (cddr x)))

(defun car-safe (x) (and (consp x) (car x)))

(defun cdr-safe (x) (and (consp x) (cdr x)))

(defun make-array (dimlist) (cond ((null dimlist) #()) ((fixnum-p dimlist) (make-vector dimlist)) (t (let ((ar (make-vector (car dimlist)))) (when (cdr dimlist) (dotimes (i (car dimlist)) (setq (vref ar i) (make-array (cdr dimlist))))) ar))))(defmacro aref (a &rest indexes) (dolist (i indexes a) (setq a (list 'vref a i))))(setq (symbol-function 'null) (symbol-function 'not))

(defun evenp (x) (zerop (% x 2)))

(defun oddp (x) (not (evenp x)))

(defun - (x &optional y) (numarith (if y 'sub 'neg) x y))

(defun / (x y) (numarith 'div x y))

(defun /u (x y) (numarith 'divu x y))

(defun % (x y) (numarith 'mod x y))

(defun %u (x y) (numarith 'modu x y))

(defun lsh (x y) (numarith 'lsh x y))

(defun lshu (x y) (numarith 'lshu x y))

(defun rsh (x y) (numarith 'rsh x y))

(defun rshu (x y) (numarith 'rshu x y))

(defun band (x y) (numarith 'band x y))

(defun bxor (x y) (numarith 'bxor x y))

(defun bor (x y) (numarith 'bor x y))

(defun bnot (x) (numarith 'bnot x))

(defun max (x y) (if (< x y) y x))

(defun min (x y) (if (> x y) y x))(defmacro with-input-file ((var file) &rest body) (with-open-file-aux var file 'make-file-input-stream body))(defmacro with-input-binfile ((var file) &rest body) (with-open-file-aux var file 'make-binfile-input-stream body))(defmacro with-io-binfile ((var file) &rest body) (with-open-file-aux var file 'make-binfile-io-stream body))(defmacro with-output-binfile ((var file) &rest body) (list 'progn (list 'delete-file file) (with-open-file-aux var file 'make-binfile-io-stream body)))(defmacro with-output-file ((var file) &rest body) (with-open-file-aux var file 'make-file-output-stream body))(defmacro with-input-string ((var string) &rest body) (list* 'let (list (list var (list 'make-string-input-stream string))) body))(defmacro with-output-string (var &rest body) (list* 'let (list (list var '(make-string-output-stream))) (append body (list (list 'get-output-stream-string var)))))

(defun with-open-file-aux (var file openfun body) (list 'let (list (list var (list openfun file))) (list 'unless var (list 'clerror '"%t can not open." file)) (list 'unwind-protect (cons 'progn body) (list 'close-stream var))))

(defun cat (&rest files) (dolist (file files) (with-input-file (s file) (let (c) (while (not (eq (setq c (read-char s)) -1)) (write-char *stdoutput* c))))))

(defun hexdump-file (file) (format t "%h" (let ((*stdoutput* (make-string-output-stream))) (cat file) (get-output-stream-string *stdoutput*))))

(defun flatsize (x) (with-output-file (nsm "/dev/null") (print-tree nsm x) (stream-col nsm)))

(defun getpid nil 1)

(defun make-tmp-file-name (&optional dir name) (setq dir (if (and dir (file-directory-p dir)) dir (or (getenv "TMP") ".")) name (or name "tmp")) (let ((pid (getpid)) nname) (while (file-exists-p (setq nname (format nil "%T\\%T.%d" dir name pid))) (incf pid)) nname))(defvar *break-level* 0)

(defun break (&optional msg) (let* ((*break-level* (1+ *break-level*)) (*top-level-prompt* (format nil "->%#.'>'c" (1+ *break-level*) 32))) (when msg (format t "break: %T" msg)) (top-level-loop) (when (= *break-level* 1) (format t "%nReturn to top level.\n"))))

(defun xerror (msg &rest args) (apply #'format *stderror* msg args) (exit 1))

(defun debug-on nil (setq *cl-debug-enable* t))

(defun debug-off nil (setq *cl-debug-enable* nil))

(defun gct nil (let ((*gc-control* t)) (gc)))(defmacro defnth (prefix (&rest names)) (list 'let '((n 0)) (list 'dolist (list 'field (list 'quote names)) (list 'eval (list* 'list ''defun (list 'intern (list 'format 'nil '"%t-%t" (list 'quote prefix) 'field)) '('(x) (list 'nth n 'x)))) '(incf n)) (list 'defun (intern (format nil "make-%t" prefix)) 'nil (list 'make-list (length names)))))

(defun autoload (func file) (eval (list 'defun func '(&rest args) (list 'autoload-hook (list 'quote func) file 'args))))

(defun autoload-p (func) (and (fboundp func) (eq (car (nth 2 (symbol-function func))) 'autoload-hook)))

(defun autoload-hook (func file args) (let ((protectfromgc (symbol-function func))) (when (autoload-p func) (load file) (when (autoload-p func) (clerror "%t has still not been unbound after loading %t" func file))) (apply func args)))(defvar *modules* nil)

(defun provide (name) (or (member name *modules*) (setq *modules* (cons name *modules*))))

(defun provided-p (name) (member name *modules*))

(defun require (name) (or (provided-p name) (progn (load (file-name-change-suffix name ".cl")) (provide name))))

(defun unbound nil (or (get 'unbound 'unbound) (put 'unbound 'unbound (symbol-value (gensym)))))

(defun doc (sym) (require "doc") (format t "%T" (get sym 'function-documentation)) (unbound))