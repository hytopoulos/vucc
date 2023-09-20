@echo on

del main.cpp
del main.sco
del main.s
set NOCRYPTLOAD=1
juncpp main.c main.cpp
cparse main.cpp main.sco -load-ld LD-65816.cl
cgrind main.sco main.s -load-md MD-65816.cl