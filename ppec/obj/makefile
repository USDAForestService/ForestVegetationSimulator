# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../common/

# the PPE EC (EAST CASCADES) source directory
PPEC_SRC   = ./../src/

# options for the compiler
FFLAGS = ${I1}${BASE_COM}

# the PPE EC (EAST CASCADES) object files
PPEC_OBJS = ppdgf.o${BJ} ppregt.o${BJ} varget.o${BJ} varput.o${BJ}

ppec:     ${PPEC_OBJS}

ppdgf.o${BJ}:  ${PPEC_SRC}ppdgf.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}ppdgf.f

ppregt.o${BJ}: ${PPEC_SRC}ppregt.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}ppregt.f

varget.o${BJ}: ${PPEC_SRC}varget.f ${BASE_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}varget.f

varput.o${BJ}: ${PPEC_SRC}varput.f ${BASE_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}varput.f



