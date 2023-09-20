0: nil->.->(7) {"T-0"} {"flags.1" "_12.1" "_2.1" "i.1"}
  0:{prologue {a.w}}
  24:{setreg {y.w} (reg "flags.1")
     31:{getreg {} (reg "T-0")}}
  1:{setreg {y.w $4.w} (reg "_12.1")
     32:{consti16-general {} 0}}
  2:{setreg {y.w $2.w $4.w} (reg "_2.1")
     33:{getreg {} (reg "_12.1")}}
  3:{setreg {y.w $2.w $4.w $6.w} (reg "i.1")
     34:{getreg {} (reg "_2.1")}}
  4:{jump1 {y.w $2.w $4.w $6.w} "L1"}
1: (7)->.->(2 5) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}
  5:{label {y.w $2.w $4.w $6.w} "L2"}
  6:{jump2i16-const-notzero {y.w $2.w $4.w $6.w} "L3" 0 jmp-eq
     35:{bandi16-const {} 1
        36:{getreg {} (reg "flags.1")}}}
2: (1)->.->(3 5) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}
  27:{setreg {a.w y.w $2.w $4.w $6.w} (reg "T-1")
     37:{muli16-2^n {} 2
        38:{getreg {} (reg "i.1")}}}
  28:{setreg {a.w x.w y.w $2.w $4.w $6.w} (reg "T-3")
     39:{rshi16-const {} 3
        40:{addi16-mem {} "MEM_5DAE"
           41:{geti16-dx {} "DATA_C200C5"
              42:{getreg {} (reg "T-1")}}}}}
  29:{setreg {a.w x.w y.w $2.w $4.w $6.w} (reg "T-2")
     43:{rshi16-const {} 3
        44:{addi16-mem {} "MEM_5DAC"
           45:{geti16-dx {} "DATA_C200B9"
              46:{getreg {} (reg "T-1")}}}}}
  30:{setreg {a.w y.w $2.w $4.w $6.w} (reg "T-4")
     47:{calli16-const {} "label_C054C9"
        48:{getreg {} (reg "T-3")}
        49:{getreg {} (reg "T-2")}}}
  7:{setreg {a.w y.w $2.w $4.w $6.w} (reg "_e.1")
     50:{getreg {} (reg "T-4")}}
  8:{setreg {a.w y.w $2.w $4.w $6.w} (reg "_12.1")
     51:{bori16-general {}
        52:{getreg {} (reg "_12.1")}
        53:{getreg {} (reg "_e.1")}}}
  9:{jump2i16-const-notzero {y.w $2.w $4.w $6.w} "L3" 0 jmp-eq
     54:{bandi16-const {} 192
        55:{getreg {} (reg "_e.1")}}}
3: (2)->.->(5) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}

  10:{setreg {y.w $2.w $4.w $6.w} (reg "_2.1")
     56:{bori16-const {} 64
        57:{getreg {} (reg "_2.1")}}}
5: (3 2 1)->.->(7) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}
  12:{label {y.w $2.w $4.w $6.w} "L3"}
  13:{setreg {y.w $2.w $4.w $6.w} (reg "_2.1")
     58:{rshi16-const {} 1
        59:{getreg {} (reg "_2.1")}}}
  14:{setreg {y.w $2.w $4.w $6.w} (reg "flags.1")
     60:{rshi16-const {} 1
        61:{getreg {} (reg "flags.1")}}}
  16:{setreg {y.w $2.w $4.w $6.w} (reg "i.1")
     62:{inc {} 1
        63:{getreg {} (reg "i.1")}}}
7: (5 0)->.->(8 1) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}
  17:{label {y.w $2.w $4.w $6.w} "L1"}
  18:{jump2i16-const-notzero {y.w $2.w $4.w $6.w} "L2" 6 jmp-lt
     64:{getreg {} (reg "i.1")}}
8: (7)->.->(9 10) {"_12.1" "_2.1"} {"_12.1" "_2.1"}
  19:{jump2i16-const-notzero {$2.w $4.w} "L6" 1 jmp-ne
     65:{geti16-const {} "MEM_5DB4"}}
9: (8)->.->(10) {"_12.1" "_2.1"} {"_2.1"}
  20:{seti16-const {$2.w} "MEM_5DA4"
     66:{getreg {} (reg "_12.1")}}
10: (9 8)->.->nil {"_2.1"} {}
  21:{label {$2.w} "L6"}
  22:{ret-i16 {}
     67:{getreg {} (reg "_2.1")}}
  26:{epilogue {}}