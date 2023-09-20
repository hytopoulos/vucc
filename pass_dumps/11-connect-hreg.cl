0: nil->.->(7) {} {"flags.1"}
  0:[prologue {}]
  68:(set i16 (hreg "y.w") (get i16 (hreg "a.w")))
  24:(set i16 (auto i16 "T_2") (get i16 (hreg "y.w")))
  32:[consti16-general r=a.w {a.w} 0]
  69:(set i16 (hreg "$4.w") (get i16 (hreg "a.w")))
  70:(set i16 (hreg "$2.w") (get i16 (hreg "$4.w")))
  71:(set i16 (hreg "$6.w") (get i16 (hreg "$2.w")))
  4:[jump1 {} "L1"]
1: (7)->.->(2 5) {"flags.1"} {"flags.1"}
  5:[label {} "L2"]
  72:(set i16 (hreg "a.w") (get i16 (hreg "y.w")))
  35:[bandi16-const r=a.w {a.w} 1]
  6:[jump2i16-const-notzero r=a.w {} "L3" 0 jmp-eq]
2: (1)->.->(3 5) {"flags.1"} {"flags.1"}
  73:(set i16 (hreg "a.w") (get i16 (hreg "$6.w")))
  37:[muli16-2^n r=a.w {a.w} 2]
  27:(set i16 (auto i16 "T_1") (get i16 (hreg "a.w")))
  74:(set i16 (hreg "x.w") (get i16 (hreg "a.w")))
  41:[geti16-dx r=a.w x=x.w {a.w} "DATA_C200C5"]
  40:[addi16-mem r=a.w {a.w} "MEM_5DAE"]
  39:[rshi16-const r=a.w {a.w} 3]
  75:(set i16 (hreg "x.w") (get i16 (hreg "a.w")))
  28:(set i16 (auto i16 "T_0") (get i16 (hreg "x.w")))
  46:(set i16 (hreg "a.w") (get i16 (auto i16 "T_1")))
  76:(set i16 (hreg "x.w") (get i16 (hreg "a.w")))
  45:[geti16-dx r=a.w x=x.w {a.w} "DATA_C200B9"]
  44:[addi16-mem r=a.w {a.w} "MEM_5DAC"]
  43:[rshi16-const r=a.w {a.w} 3]
  48:(set i16 (hreg "x.w") (get i16 (auto i16 "T_0")))
  47:[calli16-const r=a.w #:R*=a.w #:R*=x.w {a.w x.w y.w} "label_C054C9"]
  7:(set i16 (auto i16 "T_1") (get i16 (hreg "a.w")))
  77:(set i16 (hreg "$8.w") (get i16 (hreg "a.w")))
  78:(set i16 (hreg "a.w") (get i16 (hreg "$4.w")))
  51:[bori16-general r=a.w p=$8.w {a.w}]
  79:(set i16 (hreg "$4.w") (get i16 (hreg "a.w")))
  55:(set i16 (hreg "a.w") (get i16 (auto i16 "T_1")))
  54:[bandi16-const r=a.w {a.w} 192]
  80:(set i16 (hreg "x.w") (get i16 (hreg "a.w")))
  9:[jump2i16-const-notzero r=x.w {} "L3" 0 jmp-eq]
3: (2)->.->(5) {"flags.1"} {"flags.1"}
  81:(set i16 (hreg "a.w") (get i16 (hreg "$2.w")))
  56:[bori16-const r=a.w {a.w} 64]
  82:(set i16 (hreg "$2.w") (get i16 (hreg "a.w")))
5: (3 2 1)->.->(7) {"flags.1"} {"flags.1"}
  12:[label {} "L3"]
  83:(set i16 (hreg "a.w") (get i16 (hreg "$2.w")))
  58:[rshi16-const r=a.w {a.w} 1]
  84:(set i16 (hreg "$2.w") (get i16 (hreg "a.w")))
  61:(set i16 (hreg "y.w") (get i16 (auto i16 "T_2")))
  85:(set i16 (hreg "a.w") (get i16 (hreg "y.w")))
  60:[rshi16-const r=a.w {a.w} 1]
  86:(set i16 (hreg "y.w") (get i16 (hreg "a.w")))
  14:(set i16 (auto i16 "T_2") (get i16 (hreg "y.w")))
  87:(set i16 (hreg "a.w") (get i16 (hreg "$6.w")))
  62:[inc r=a.w {a.w} 1]
  88:(set i16 (hreg "$6.w") (get i16 (hreg "a.w")))
7: (5 0)->.->(8 1) {"flags.1"} {"flags.1"}
  17:[label {} "L1"]
  89:(set i16 (hreg "a.w") (get i16 (hreg "$6.w")))
  18:[jump2i16-const-notzero r=a.w {} "L2" 6 jmp-lt]
8: (7)->.->(9 10) {} {}
  65:[geti16-const r=a.w {a.w} "MEM_5DB4"]
  19:[jump2i16-const-notzero r=a.w {} "L6" 1 jmp-ne]
9: (8)->.->(10) {} {}
  90:(set i16 (hreg "a.w") (get i16 (hreg "$4.w")))
  20:[seti16-const r=a.w {} "MEM_5DA4"]
10: (9 8)->.->nil {} {}
  21:[label {} "L6"]
  91:(set i16 (hreg "a.w") (get i16 (hreg "$2.w")))
  22:[ret-i16 r=a.w {}]
  26:[epilogue {}]