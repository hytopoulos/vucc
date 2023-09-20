
void label_C07B52(void);
short label_C05F33(short, short, short);
void label_C03A94(short);
void label_C03F1E(void);
void label_C068F4(short, short);
void label_C4FBBD(short);
int label_C03E5A(void);
void label_C08756(void);
void label_C069AF(void);
short label_C21628(short);

extern short MEM_3486[6];
extern short MEM_438A;
extern short MEM_438C;
extern short MEM_5DA4;
extern short MEM_5DAC;
extern short MEM_5DAE;
extern short MEM_5DB4;
extern short MEM_5DD4;
extern short MEM_5DD6;
extern short MEM_5DDA;
extern short MEM_9877;
extern short MEM_987B;
extern short MEM_987F;
extern short MEM_9881;
extern short MEM_9889;
extern short MEM_9F6B;
extern short MEM_9F71;
extern short DATA_C200B9[];
extern short DATA_C200C5[];
extern short DATA_C30186;

extern int sss;

typedef struct {
    char _0[0x3D];
    short _3d;
} Struct3D;

extern Struct3D* MEM_4DC6;


int label_C05769(int flags) {
	/* REP.b #$31 */
	/* PHD */
	/* PHA */
	/* TDC */
	/* ADC.w #$FFEA */
	/* TCD */
	/* PLA */
	/* STA.b $04 */
	/* LDY.w #$0000 */
	/* STY.b $14 */
	/* STY.b $02 */
	/* LDA.b $02 */
	/* STA.b $12 */
	/* BRA.b label_C057D2 */
    int i, _2, _12;

    for (i = _2 = _12 = 0; i < 6; i++) {
    /* label_C05782: */
        /* LDA.b $04 */
        /* AND.w #$0001 */
        /* BEQ.b label_C057C3 */
        if (flags & 1) {
            /* TYA */
            /* ASL */
            /* STA.b $10 */
            /* TAX */
            /* LDA.l DATA_C200C5,X */
            /* CLC */
            /* ADC.w $5DAE */
            /* LSR */
            /* LSR */
            /* LSR */
            /* TAX */
            /* STX.b $0E */
            /* LDA.b $10 */
            /* TAX */
            /* LDA.l DATA_C200B9,X */
            /* CLC */
            /* ADC.w $5DAC */
            /* LSR */
            /* LSR */
            /* LSR */
            /* LDX.b $0E */
            int _e = label_C054C9(DATA_C200B9[i] + MEM_5DAC >> 3, DATA_C200C5[i] + MEM_5DAE >> 3);
            /* JSR.w label_C054C9 */
            /* STA.b $0E */
            /* ORA.b $12 */
            /* STA.b $12 */
            _12 |= _e;
            /* LDA.b $0E */
            /* AND.w #$00C0 */
            /* BEQ.b label_C057C3 */
            if (_e & 0xC0) {
                /* LDA.b $02 */
                /* ORA.w #$0040 */
                /* STA.b $02 */
                _2 |= 0x40;
            }
        }

    /* label_C057C3: */
        /* LDA.b $02 */
        /* LSR */
        /* STA.b $02 */
        _2 >>= 1;
        /* LDA.b $04 */
        /* LSR */
        /* STA.b $04 */
        flags >>= 1;
        /* LDY.b $14 */
        /* INY */
        /* STY.b $14 */
    /* label_C057D2: */
        /* CPY.w #$0006 */
        /* BCC.b label_C05782 */
    }

    /* LDA.w $5DB4 */
	/* CMP.w #$0001 */
	/* BNE.b label_C057E4 */
    if (MEM_5DB4 == 1) {
        /* LDA.b $12 */
        /* STA.w $5DA4 */
        MEM_5DA4 = _12;
    }

/* label_C057E4: */
	/* LDA.b $02 */
	/* PLD */
	/* RTS */
    return _2;
}

void label_C03FA9(short arg1, short arg2, short arg3) {
    /* REP.b #$31 */
    /* PHD */
    /* PHA */
    /* TDC */
    /* ADC.w #$FFF0 */
    /* TCD */
    /* PLA */
    /* STY.b $02 */
    /* STA.b $0E */

    /* STA.w $9877 */
    MEM_9877 = arg1;
    /* STX.w $987B */
    MEM_987B = arg2;
    /* LDA.b $02 */
    /* STA.w $987F */
    MEM_987F = arg3;
    /* LDY.w $9889 */
    /* LDA.b $0E */
    /* JSL.l label_C05F33 */
    /* STA.w $9881 */
    MEM_9881 = label_C05F33(arg1, arg2, MEM_9889);
    /* LDA.b $02 */
    /* JSL.l label_C03A94 */
    label_C03A94(arg3);
    /* JSL.l label_C03F1E */
    label_C03F1E();
    /* LDA.w #$0000 */
    /* STA.b $0E */
    /* BRA.b label_C03FEC */
    for (arg1 = 0; arg1 < 6; arg1++) {
        /* label_C03FDF: */
        /* ASL */
        /* TAX */
        /* LDA.w #$FFFF */
        /* STA.w $3486,X */
        MEM_3486[arg1] = 0xFFFF;
        /* 	LDA.b $0E */
        /* 	INC */
        /* 	STA.b $0E */
        /* label_C03FEC: */
        /* 	CMP.w #$0006 */
        /* 	BCC.b label_C03FDF */
    }
    /* LDA.w #$FFFF */
    /* STA.w $9F6B */
    MEM_9F6B = 0xFFFF;
    /* STZ.w $438C */
    MEM_438C = 0;
    /* STZ.w $438A */
    MEM_438A = 0;
    /* LDA.l DATA_C30186 */
    /* JSL.l label_C21628 */
    /* STA.w $9F71 */
    MEM_9F71 = label_C21628(DATA_C30186);
    /* JSL.l label_C07B52 */
    label_C07B52();
    /* PLD */
    /* RTL */
    return;
}

int label_C03E9D() {
	/* REP.b #$31 */
	/* PHD */
	/* PHA */
	/* TDC */
	/* ADC.w #$FFF0 */
	/* TCD */
	/* PLA */
	/* JSR.w label_C03E5A */
    int t = label_C03E5A();

	/* STA.b $0E */
	/* LDX.w $4DC6 */
	/* LDA.w $003D,X */
	/* STA.b $02 */
	/* LDA.b $0E */
	/* CMP.b $02 */
	/* BCS.b label_C03EBE */
    short v = MEM_4DC6->_3d;
    if (t < v) {
	/* CLC */
	/* ADC.w #$0100 */
        t += 0x100;
    }
/* label_C03EBE: */
	/* SEC */
	/* SBC.b $02 */
	/* PLD */
	/* RTL */
    return t - v;
}

void label_C03C25() {
	/* REP.b #$31 */
	/* LDA.w #$0001 */
	/* STA.w $5DDA */
    MEM_5DDA = 1;
	/* LDX.w $987B */
	/* LDA.w $9877 */
	/* JSL.l label_C068F4 */
    label_C068F4(MEM_9877, MEM_987B);
	/* LDA.w $5DD6 */
	/* CMP.w $5DD4 */
	/* BEQ.b label_C03C47 */
    if (MEM_5DD6 != MEM_5DD4) {
        /* JSL.l label_C08756 */
        label_C08756();
        /* JSL.l label_C069AF */
        label_C069AF();
    }
    /* label_C03C47: */
	/* STZ.w $5DDA */
    MEM_5DDA = 0;
	/* RTS */
}

void label_C06A07() {
	/* REP.b #$31 */
	/* LDX.w $987B */
	/* LDA.w $9877 */
	/* JSL.l label_C068F4 */
    label_C068F4(MEM_9877, MEM_987B);
	/* LDA.w $5DD6 */
	/* JSL.l label_C4FBBD */
    label_C4FBBD(MEM_5DD6);
	/* RTL */
}
