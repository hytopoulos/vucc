0: nil->.->(7)
  0:(prologue)
  24:(set i16 (reg "flags") (get i16 (reg "T-0")))
  1:(set i16 (reg "_12") (const i16 0))
  2:(set i16 (reg "_2") (get i16 (reg "_12")))
  3:(set i16 (reg "i") (get i16 (reg "_2")))
  4:(jump1 "L1")
1: (7)->.->(2 5)
  5:"L2"
  6:(jump2 (tsteq i16 (band i16 (get i16 (reg "flags")) (const i16 1)) (const i16 0)) "L3")
2: (1)->.->(3 4)
  7:(set i16 (reg "_e") (call i16 (const i16 "label_C054C9") (rsh i16 (add i16 (get i16 (add i16 (mul i16 (get i16 (reg "i")) (const i16 2)) (const i16 "DATA_C200B9"))) (get i16 (const i16 "MEM_5DAC"))) (const i16 3)) (rsh i16 (add i16 (get i16 (add i16 (mul i16 (get i16 (reg "i")) (const i16 2)) (const i16 "DATA_C200C5"))) (get i16 (const i16 "MEM_5DAE"))) (const i16 3))))
  8:(set i16 (reg "_12") (bor i16 (get i16 (reg "_12")) (get i16 (reg "_e"))))
  9:(jump2 (tsteq i16 (band i16 (get i16 (reg "_e")) (const i16 192)) (const i16 0)) "L4")
3: (2)->.->(4)
  10:(set i16 (reg "_2") (bor i16 (get i16 (reg "_2")) (const i16 64)))
4: (3 2)->.->(5)
  11:"L4"
5: (4 1)->.->(6)
  12:"L3"
  13:(set i16 (reg "_2") (rsh i16 (get i16 (reg "_2")) (const i16 1)))
  14:(set i16 (reg "flags") (rsh i16 (get i16 (reg "flags")) (const i16 1)))
6: (5)->.->(7)
  15:"L5"
  16:(set i16 (reg "i") (add i16 (get i16 (reg "i")) (const i16 1)))
7: (6 0)->.->(8 1)
  17:"L1"
  18:(jump2 (tstlt i16 (get i16 (reg "i")) (const i16 6)) "L2")
8: (7)->.->(9 10)
  19:(jump2 (tstne i16 (get i16 (const i16 "MEM_5DB4")) (const i16 1)) "L6")
9: (8)->.->(10)
  20:(set i16 (const i16 "MEM_5DA4") (get i16 (reg "_12")))
10: (9 8)->.->(11)
  21:"L6"
  22:(set i16 (ret "ret") (get i16 (reg "_2")))
  23:(jump1 "L0")
11: (10)->.->nil
  25:"L0"
  26:(epilogue)