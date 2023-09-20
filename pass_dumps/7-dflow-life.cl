0: nil->.->(7) {"T-0"} {"flags.1" "_12.1" "_2.1" "i.1"}
  0:(prologue)
  24:(set i16 (reg "flags.1") (get i16 (reg "T-0")))
  1:(set i16 (reg "_12.1") (const i16 0))
  2:(set i16 (reg "_2.1") (get i16 (reg "_12.1")))
  3:(set i16 (reg "i.1") (get i16 (reg "_2.1")))
  4:(jump1 "L1")
1: (7)->.->(2 5) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}
  5:"L2"
  6:(jump2 (tsteq i16 (band i16 (get i16 (reg "flags.1")) (const i16 1)) (const i16 0)) "L3")
2: (1)->.->(3 5) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}
  27:(set i16 (reg "T-1") (mul i16 (get i16 (reg "i.1")) (const i16 2)))
  28:(set i16 (reg "T-3") (rsh i16 (add i16 (get i16 (add i16 (get i16 (reg "T-1")) (const i16 "DATA_C200C5"))) (get i16 (const i16 "MEM_5DAE"))) (const i16 3)))
  29:(set i16 (reg "T-2") (rsh i16 (add i16 (get i16 (add i16 (get i16 (reg "T-1")) (const i16 "DATA_C200B9"))) (get i16 (const i16 "MEM_5DAC"))) (const i16 3)))
  30:(set i16 (reg "T-4") (call i16 (const i16 "label_C054C9") (get i16 (reg "T-2")) (get i16 (reg "T-3"))))
  7:(set i16 (reg "_e.1") (get i16 (reg "T-4")))
  8:(set i16 (reg "_12.1") (bor i16 (get i16 (reg "_12.1")) (get i16 (reg "_e.1"))))
  9:(jump2 (tsteq i16 (band i16 (get i16 (reg "_e.1")) (const i16 192)) (const i16 0)) "L3")
3: (2)->.->(5) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}

  10:(set i16 (reg "_2.1") (bor i16 (get i16 (reg "_2.1")) (const i16 64)))
5: (3 2 1)->.->(7) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}
  12:"L3"
  13:(set i16 (reg "_2.1") (rsh i16 (get i16 (reg "_2.1")) (const i16 1)))
  14:(set i16 (reg "flags.1") (rsh i16 (get i16 (reg "flags.1")) (const i16 1)))

  16:(set i16 (reg "i.1") (add i16 (get i16 (reg "i.1")) (const i16 1)))
7: (5 0)->.->(8 1) {"flags.1" "_12.1" "_2.1" "i.1"} {"flags.1" "_12.1" "_2.1" "i.1"}
  17:"L1"
  18:(jump2 (tstlt i16 (get i16 (reg "i.1")) (const i16 6)) "L2")
8: (7)->.->(9 10) {"_12.1" "_2.1"} {"_12.1" "_2.1"}
  19:(jump2 (tstne i16 (get i16 (const i16 "MEM_5DB4")) (const i16 1)) "L6")
9: (8)->.->(10) {"_12.1" "_2.1"} {"_2.1"}
  20:(set i16 (const i16 "MEM_5DA4") (get i16 (reg "_12.1")))
10: (9 8)->.->nil {"_2.1"} {}
  21:"L6"
  22:(set i16 (ret "ret") (get i16 (reg "_2.1")))
  26:(epilogue)