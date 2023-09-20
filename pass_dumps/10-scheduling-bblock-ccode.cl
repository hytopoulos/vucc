0: nil->.->(7) {"T-0"} {"flags.1" "_12.1" "_2.1" "i.1"}
  0:[prologue {}]
  31:{getreg r=a.w {a.w} (reg "T-0")}
  68:(set i16 (hreg "y.w") (get i16 (hreg "a.w")))
  24:{setreg y=y.w {} (reg "flags.1")}
  32:[consti16-general r=a.w {a.w} 0]
  69:(set i16 (hreg "$4.w") (get i16 (hreg "a.w")))
  1:{setreg y=$4.w {} (reg "_12.1")}
  33:{getreg r=$4.w {$4.w} (reg "_12.1")}
  70:(set i16 (hreg "$2.w") (get i16 (hreg "$4.w")))
  2:{setreg y=$2.w {} (reg "_2.1")}
  34:{getreg r=$2.w {$2.w} (reg "_2.1")}
  71:(set i16 (hreg "$6.w") (get i16 (hreg "$2.w")))
  3:{setreg y=$6.w {} (reg "i.1")}
  4:[jump1 {} "L1"]
1: (7)->.->(2 5) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}
  5:[label {} "L2"]
  36:{getreg r=y.w {y.w} (reg "flags.1")}
  72:(set i16 (hreg "a.w") (get i16 (hreg "y.w")))
  35:[bandi16-const r=a.w {a.w} 1]
  6:[jump2i16-const-notzero r=a.w {} "L3" 0 jmp-eq]
2: (1)->.->(3 5) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}
  38:{getreg r=$6.w {$6.w} (reg "i.1")}
  73:(set i16 (hreg "a.w") (get i16 (hreg "$6.w")))
  37:[muli16-2^n r=a.w {a.w} 2]
  27:{setreg y=a.w {} (reg "T-1")}
  42:{getreg r=a.w {a.w} (reg "T-1")}
  74:(set i16 (hreg "x.w") (get i16 (hreg "a.w")))
  41:[geti16-dx r=a.w x=x.w {a.w} "DATA_C200C5"]
  40:[addi16-mem r=a.w {a.w} "MEM_5DAE"]
  39:[rshi16-const r=a.w {a.w} 3]
  75:(set i16 (hreg "x.w") (get i16 (hreg "a.w")))
  28:{setreg y=x.w {} (reg "T-3")}
  46:{getreg r=a.w {a.w} (reg "T-1")}
  76:(set i16 (hreg "x.w") (get i16 (hreg "a.w")))
  45:[geti16-dx r=a.w x=x.w {a.w} "DATA_C200B9"]
  44:[addi16-mem r=a.w {a.w} "MEM_5DAC"]
  43:[rshi16-const r=a.w {a.w} 3]
  29:{setreg y=a.w {} (reg "T-2")}
  48:{getreg r=x.w {x.w} (reg "T-3")}
  49:{getreg r=a.w {a.w} (reg "T-2")}
  47:[calli16-const r=a.w #:R*=a.w #:R*=x.w {a.w x.w y.w} "label_C054C9"]
  30:{setreg y=a.w {} (reg "T-4")}
  50:{getreg r=a.w {a.w} (reg "T-4")}
  7:{setreg y=a.w {} (reg "_e.1")}
  52:{getreg r=$4.w {$4.w} (reg "_12.1")}
  53:{getreg r=a.w {a.w} (reg "_e.1")}
  77:(set i16 (hreg "$8.w") (get i16 (hreg "a.w")))
  78:(set i16 (hreg "a.w") (get i16 (hreg "$4.w")))
  51:[bori16-general r=a.w p=$8.w {a.w}]
  79:(set i16 (hreg "$4.w") (get i16 (hreg "a.w")))
  8:{setreg y=$4.w {} (reg "_12.1")}
  55:{getreg r=a.w {a.w} (reg "_e.1")}
  54:[bandi16-const r=a.w {a.w} 192]
  80:(set i16 (hreg "x.w") (get i16 (hreg "a.w")))
  9:[jump2i16-const-notzero r=x.w {} "L3" 0 jmp-eq]
3: (2)->.->(5) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}

  57:{getreg r=$2.w {$2.w} (reg "_2.1")}
  81:(set i16 (hreg "a.w") (get i16 (hreg "$2.w")))
  56:[bori16-const r=a.w {a.w} 64]
  82:(set i16 (hreg "$2.w") (get i16 (hreg "a.w")))
  10:{setreg y=$2.w {} (reg "_2.1")}
5: (3 2 1)->.->(7) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}
  12:[label {} "L3"]
  59:{getreg r=$2.w {$2.w} (reg "_2.1")}
  83:(set i16 (hreg "a.w") (get i16 (hreg "$2.w")))
  58:[rshi16-const r=a.w {a.w} 1]
  84:(set i16 (hreg "$2.w") (get i16 (hreg "a.w")))
  13:{setreg y=$2.w {} (reg "_2.1")}
  61:{getreg r=y.w {y.w} (reg "flags.1")}
  85:(set i16 (hreg "a.w") (get i16 (hreg "y.w")))
  60:[rshi16-const r=a.w {a.w} 1]
  86:(set i16 (hreg "y.w") (get i16 (hreg "a.w")))
  14:{setreg y=y.w {} (reg "flags.1")}
  63:{getreg r=$6.w {$6.w} (reg "i.1")}
  87:(set i16 (hreg "a.w") (get i16 (hreg "$6.w")))
  62:[inc r=a.w {a.w} 1]
  88:(set i16 (hreg "$6.w") (get i16 (hreg "a.w")))
  16:{setreg y=$6.w {} (reg "i.1")}
7: (5 0)->.->(8 1) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}
  17:[label {} "L1"]
  64:{getreg r=$6.w {$6.w} (reg "i.1")}
  89:(set i16 (hreg "a.w") (get i16 (hreg "$6.w")))
  18:[jump2i16-const-notzero r=a.w {} "L2" 6 jmp-lt]
8: (7)->.->(9 10) {"_12.1" "_2.1"} {"_12.1" "_2.1"}
  65:[geti16-const r=a.w {a.w} "MEM_5DB4"]
  19:[jump2i16-const-notzero r=a.w {} "L6" 1 jmp-ne]
9: (8)->.->(10) {"_12.1" "_2.1"} {"_2.1"}
  66:{getreg r=$4.w {$4.w} (reg "_12.1")}
  90:(set i16 (hreg "a.w") (get i16 (hreg "$4.w")))
  20:[seti16-const r=a.w {} "MEM_5DA4"]
10: (9 8)->.->nil {"_2.1"} {}
  21:[label {} "L6"]
  67:{getreg r=$2.w {$2.w} (reg "_2.1")}
  91:(set i16 (hreg "a.w") (get i16 (hreg "$2.w")))
  22:[ret-i16 r=a.w {}]
  26:[epilogue {}]