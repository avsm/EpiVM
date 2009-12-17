CC = gcc
CFLAGS = -Wall -g
#CFLAGS = -Wall -O3
OBJS = closure.o stdfuns.o
INSTALLDIR = ${PREFIX}/lib/evm

TARGET = libevm.a
INSTALLHDRS = closure.h stdfuns.h mainprog.c

${TARGET} : ${OBJS}
	ar r ${TARGET} ${OBJS}
	ranlib ${TARGET}

install:
	mkdir -p ${INSTALLDIR}
	install libevm.a ${INSTALLHDRS} ${INSTALLDIR}
	ranlib ${INSTALLDIR}/libevm.a

clean:
	rm -f ${OBJS} ${TARGET}

closure.o : closure.h
stdfuns.o : stdfuns.h closure.h
