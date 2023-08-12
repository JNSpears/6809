include ../Makefile.inc
ASMINCLUDE=-I ../..
# SRCS=abc abc1 abc2
OBJS=mpx9ii.cm1

all: $(OBJS)

clean:
	-$(CLEAN)

# ----------------------------------------------------

start.obj : start.asm 
abc1.obj : abc1.asm 
abc2.obj : abc2.asm 
mpx9ii.lib : abc1.obj abc2.obj
mpx9ii.cm1 : start.obj mpx9ii.lib 
