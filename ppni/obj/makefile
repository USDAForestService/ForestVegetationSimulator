# Fortran compiler
include ../../bin/FC${OS}.make
 
# the Prognosis common blocks directory
BASE_COM = ../../common/

# the PPE NI (North Idaho) source directory
PPNI_SRC   = ./../src/

# options for the compiler
FFLAGS = ${I1}${BASE_COM}

# the PPE NI (North Idaho) object files
PPNI_OBJS = ppdgf.o${BJ} ppregt.o${BJ} varget.o${BJ} varput.o${BJ}

ppni:     ${PPNI_OBJS}

ppdgf.o${BJ}:  ${PPNI_SRC}ppdgf.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPNI_SRC}ppdgf.f

ppregt.o${BJ}: ${PPNI_SRC}ppregt.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPNI_SRC}ppregt.f

varget.o${BJ}: ${PPNI_SRC}varget.f ${BASE_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${PPNI_SRC}varget.f

varput.o${BJ}: ${PPNI_SRC}varput.f ${BASE_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${PPNI_SRC}varput.f



