# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../common/

# the PPE KT (KOOKANTL) source directory
PPKT_SRC   = ./../src/

# options for the compiler
FFLAGS = ${I1}${BASE_COM} 

# the PPE KT (KOOKANTL) object files
PPKT_OBJS = ppdgf.o${BJ} ppregt.o${BJ} varget.o${BJ} varput.o${BJ}

ppkt:     ${PPKT_OBJS}

ppdgf.o${BJ}:  ${PPKT_SRC}ppdgf.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPKT_SRC}ppdgf.f

ppregt.o${BJ}: ${PPKT_SRC}ppregt.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPKT_SRC}ppregt.f

varget.o${BJ}: ${PPKT_SRC}varget.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}KOTCOM.F77
	${FC} -c ${FFLAGS} ${PPKT_SRC}varget.f

varput.o${BJ}: ${PPKT_SRC}varput.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}KOTCOM.F77
	${FC} -c ${FFLAGS} ${PPKT_SRC}varput.f



