CC = gcc
CFLAGS = -pg -O2 -Wall
OBJS = closure.o
INSTALLDIR = ${PREFIX}/lib/evm

TARGET = libevm.a

${TARGET} : ${OBJS}
	ar r ${TARGET} ${OBJS}
	ranlib ${TARGET}

install:
	mkdir -p ${INSTALLDIR}
	install libevm.a closure.h ${INSTALLDIR}
	ranlib ${INSTALLDIR}/libevm.a

clean:
	rm -f ${OBJS} ${TARGET}

closure.o : closure.h

