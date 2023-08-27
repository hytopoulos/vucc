(extern *mdl-asm-name*)
(extern *debug* *notify-gc*)

(defvar *usage* "Usage: %T [options...] scode object

options:
-v		Report compiling function.
-asm asmname	Specify Assembler/Linker system name.
-Nsplit-reg	No optimize for split register life interval.
-Nxmul		No optimize for expand multiply with small const.
-Ncse		No optimize for reduce common sub expression.
-Nall		No optimize at all.
-Oall  		Do all optimize.
-no-underscore	External name is not prefixed by `_'.
-load-md mdfile Loading md file.
-load file	Universal option.
-eval form	Universal option.
")

(defun main (args)
  (setq *debug* nil
        *notify-gc* nil)
  (cond ((= (length args) 1)
         (xerror *usage* (car args)))
        (t
         (setq args (cdr args))
         (let (scodef objf a room)
           (while (setq a (pop args))
             (if (= (sref a 0) 45)
                 (case a
                   ("-v" (setq *verify* t))
                   ("-asm" (unless (car args)
                             (xerror "-asm : Missing name\n"))
                           (setq *mdl-asm-name* (intern (pop args))))
                   ("-Nauto-reg" (Nauto-reg))
                   ("-Nsplit-reg" (Nsplit-reg))
                   ("-Nxmul" (Nxmul))
                   ("-Ncse" (Ncse))
                   ("-Nloop" (Nloop))
                   ("-Nzombi" (Nzombi))
                   ("-Nall" (Nall))
                   ("-Oall" (Oall))
                   ("-no-underscore" (setq *md-no-underscore* t))
                   ("-load" (load (pop args)))
                   ("-load-md" (load-md (pop args)))
                   ("-eval" (eval (read-tree (pop args))))
                   ("-room" (setq room t))
                   (t (xerror "%T : No such option\n" a)))
               (cond ((null scodef)
                      (setq scodef a)
                      (unless (file-exists-p scodef)
                        (xerror "%s: No such file.\n" scodef)))
                     ((null objf)
                      (setq objf a)
                      (when (memq (file-name-suffix objf) '(".c" ".h" ".cl"))
                        (xerror "%s: Bad suffix.\n" objf)))
                     (t (xerror "Too many files.\n"))))))
         (unless scodef
           (xerror "Missing scode file.\n"))
         (unless objf
           (xerror "Missing object file.\n"))
         (let ((done (cgrind scodef objf)))
           (when room
             (room t))
           (unless done
             (exit 1)))))))
