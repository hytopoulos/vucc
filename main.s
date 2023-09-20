;;; source = main.c
;;; cparse = (95/02/23-18:49:40 ld-65816 1.00)
;;; cgrind = (95/03/03-20:15:55 md-65816 0.1)
;;; *command-line-args* = (
;;;     "D:\\CGRIND.EXE"
;;;     "main.sco"
;;;     "main.s"
;;;     "-load-md"
;;;     "MD-65816.cl")
;;; *optimize* = (
;;;     sf-peep
;;;     auto-reg
;;;     split-reg
;;;     jump
;;;     cse
;;;     loop
;;;     delete-zombi
;;;     peep-stld
;;;     peep-convzx816
;;;     peep-loadconst
;;;     peep-merge816)
;;; *debug* = (
;;;     confirm-move-optimize-in-fixup-hreg
;;;     pass
;;;     peep
;;;     peephole
;;;     dflow-life
;;;     bind-hreg
;;;     connect-hreg
;;;     confirm-move-optimize-in-fixup-hreg
;;;     loop-optimize
;;;     reduce-cse
;;;     reduce-bblock-expr
;;;     reduce-jump
;;;     reduce-mcode
;;;     generate-mcode
;;;     generate-acode
;;;     split-reg)

.memorymap
slotsize $4000
slot 0 $0000
defaultslot 0
.endme
.rombanksize $4000
.rombanks 2

; .text
	; extern	_label_C07B52
	; extern	_label_C05F33
	; extern	_label_C03A94
	; extern	_label_C03F1E
	; extern	_label_C068F4
	; extern	_label_C4FBBD
	; extern	_label_C03E5A
	; extern	_label_C08756
	; extern	_label_C069AF
	; extern	_label_C21628
; .bss
	; extern	_MEM_3486
	; extern	_MEM_438A
	; extern	_MEM_438C
	; extern	_MEM_5DA4
	; extern	_MEM_5DAC
	; extern	_MEM_5DAE
	; extern	_MEM_5DB4
	; extern	_MEM_5DD4
	; extern	_MEM_5DD6
	; extern	_MEM_5DDA
	; extern	_MEM_9877
	; extern	_MEM_987B
	; extern	_MEM_987F
	; extern	_MEM_9881
	; extern	_MEM_9889
	; extern	_MEM_9F6B
	; extern	_MEM_9F71
	; extern	_DATA_C200B9
	; extern	_DATA_C200C5
	; extern	_DATA_C30186
	; extern	_MEM_4DC6
; .text
	; extern	_label_C054C9

;;; FUNCTION label_C05769
	.align	2
	.export	_label_C05769
_label_C05769:
;; framesize 20 = 0(args) + 0(local) + 6(tmp) + 0(saveregs) + 0(savelp)
;; used callee save regs = nil
;; tmp   T_0 = (auto 14)
;; tmp   T_1 = (auto 16)
;; tmp   T_2 = (auto 18)
	rep	#$31
	phd
	pha
	tdc
	adc	#$-20
	tcd
	pla
	tay
	sty	$T_2.w
	lda	#$0
	sta	$4.w
	sta	$2.w
	sta	$6.w
	jmp-1	L2
L0:
	tya
	and	#$1
	jmp-eq	L1
	lda	$6.w
	asl
	sta	$T_1.w
	tax
	lda	_DATA_C200C5.w,x
	clc
	adc	_MEM_5DAE.w
	lsr
	lsr
	lsr
	tax
	stx	$T_0.w
	lda	$T_1.w
	tax
	lda	_DATA_C200B9.w,x
	clc
	adc	_MEM_5DAC.w
	lsr
	lsr
	lsr
	ldx	$T_0.w
	jsl	_label_C054C9
	sta	$T_1.w
	sta	$8.w
	lda	$4.w
	ora	$8.w
	sta	$4.w
	lda	$T_1.w
	and	#$192
	tax
	jmp-eq	L1
	lda	$2.w
	ora	#$64
	sta	$2.w
L1:
	lda	$2.w
	lsr
	sta	$2.w
	ldy	$T_2.w
	tya
	lsr
	tay
	sty	$T_2.w
	lda	$6.w
	ina
	sta	$6.w
L2:
	lda	$6.w
	cmp	#$6
	jmp-lt	L0
	lda	_MEM_5DB4.w
	cmp	#$1
	jmp-ne	L3
	lda	$4.w
	sta	_MEM_5DA4.w
L3:
	lda	$2.w
	pld
	rtl

;;; FUNCTION label_C03FA9
	.align	2
	.export	_label_C03FA9
_label_C03FA9:
;; framesize 16 = 0(args) + 0(local) + 2(tmp) + 0(saveregs) + 0(savelp)
;; used callee save regs = nil
;; tmp   T_10 = (auto 14)
	rep	#$31
	phd
	pha
	tdc
	adc	#$-16
	tcd
	pla
	sty	$2.w
	sta	_MEM_9877.w
	stx	_MEM_987B.w
	ldy	$2.w
	sty	_MEM_987F.w
	ldy	_MEM_9889.w
	jsl	_label_C05F33
	sta	_MEM_9881.w
	lda	$2.w
	jsl	_label_C03A94
	jsl	_label_C03F1E
	lda	#$0
	sta	$T_10.w
	jmp-1	L5
L4:
	asl
	tax
	lda	#$-1
	sta	_MEM_3486.w,x
	lda	$T_10.w
	ina
	sta	$T_10.w
L5:
	cmp	#$6
	jmp-lt	L4
	lda	#$-1
	sta	_MEM_9F6B.w
	stz.w	_MEM_438C.w
	stz.w	_MEM_438A.w
	lda	_DATA_C30186.w
	jsl	_label_C21628
	sta	_MEM_9F71.w
	jsl	_label_C07B52
	pld
	rtl

;;; FUNCTION label_C03E9D
	.align	2
	.export	_label_C03E9D
_label_C03E9D:
;; framesize 14 = 0(args) + 0(local) + 0(tmp) + 0(saveregs) + 0(savelp)
;; used callee save regs = nil
	rep	#$31
	phd
	pha
	tdc
	adc	#$-14
	tcd
	pla
	jsl	_label_C03E5A
	ldx	_MEM_4DC6.w
	stx	$2.w
	cmp	$2.w
	jmp-ge	L6
	clc
	adc	#$256
L6:
	sec
	sbc.w	$2.w
	pld
	rtl

;;; FUNCTION label_C03C25
	.align	2
	.export	_label_C03C25
_label_C03C25:
;; framesize 0 = 0(args) + 0(local) + 0(tmp) + 0(saveregs) + 0(savelp)
;; used callee save regs = nil
	rep	#$31
	lda	#$1
	sta	_MEM_5DDA.w
	ldx	_MEM_987B.w
	lda	_MEM_9877.w
	jsl	_label_C068F4
	lda	_MEM_5DD6.w
	cmp	_MEM_5DD4.w
	jmp-eq	L7
	jsl	_label_C08756
	jsl	_label_C069AF
L7:
	stz.w	_MEM_5DDA.w
	rtl

;;; FUNCTION label_C06A07
	.align	2
	.export	_label_C06A07
_label_C06A07:
;; framesize 0 = 0(args) + 0(local) + 0(tmp) + 0(saveregs) + 0(savelp)
;; used callee save regs = nil
	rep	#$31
	ldx	_MEM_987B.w
	lda	_MEM_9877.w
	jsl	_label_C068F4
	lda	_MEM_5DD6.w
	jsl	_label_C4FBBD
	rtl

	; end
