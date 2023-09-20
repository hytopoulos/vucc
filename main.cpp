# 1 "main.c"

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
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
    int i, _2, _12;

    for (i = _2 = _12 = 0; i < 6; i++) {
     
         
         
         
        if (flags & 1) {
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
            int _e = label_C054C9(DATA_C200B9[i] + MEM_5DAC >> 3, DATA_C200C5[i] + MEM_5DAE >> 3);
             
             
             
             
            _12 |= _e;
             
             
             
            if (_e & 0xC0) {
                 
                 
                 
                _2 |= 0x40;
            }
        }

     
         
         
         
        _2 >>= 1;
         
         
         
        flags >>= 1;
         
         
         
     
         
         
    }

     
	 
	 
    if (MEM_5DB4 == 1) {
         
         
        MEM_5DA4 = _12;
    }

 
	 
	 
	 
    return _2;
}

void label_C03FA9(short arg1, short arg2, short arg3) {
     
     
     
     
     
     
     
     
     

     
    MEM_9877 = arg1;
     
    MEM_987B = arg2;
     
     
    MEM_987F = arg3;
     
     
     
     
    MEM_9881 = label_C05F33(arg1, arg2, MEM_9889);
     
     
    label_C03A94(arg3);
     
    label_C03F1E();
     
     
     
    for (arg1 = 0; arg1 < 6; arg1++) {
         
         
         
         
         
        MEM_3486[arg1] = 0xFFFF;
         
         
         
         
         
         
    }
     
     
    MEM_9F6B = 0xFFFF;
     
    MEM_438C = 0;
     
    MEM_438A = 0;
     
     
     
    MEM_9F71 = label_C21628(DATA_C30186);
     
    label_C07B52();
     
     
    return;
}

int label_C03E9D() {
	 
	 
	 
	 
	 
	 
	 
	 
    int t = label_C03E5A();

	 
	 
	 
	 
	 
	 
	 
    short v = MEM_4DC6->_3d;
    if (t < v) {
	 
	 
        t += 0x100;
    }
 
	 
	 
	 
	 
    return t - v;
}

void label_C03C25() {
	 
	 
	 
    MEM_5DDA = 1;
	 
	 
	 
    label_C068F4(MEM_9877, MEM_987B);
	 
	 
	 
    if (MEM_5DD6 != MEM_5DD4) {
         
        label_C08756();
         
        label_C069AF();
    }
     
	 
    MEM_5DDA = 0;
	 
}

void label_C06A07() {
	 
	 
	 
	 
    label_C068F4(MEM_9877, MEM_987B);
	 
	 
    label_C4FBBD(MEM_5DD6);
	 
}
