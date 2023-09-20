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