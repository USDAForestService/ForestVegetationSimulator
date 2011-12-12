# Fortran compiler
include ../../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../../common/

# the CLIMATE model source directory
CLIM_SRC = ./../src/

# options for the compiler
FFLAGS = ${I1}${BASE_COM}

# the CLIMATE model object files
CLIM_OBJS = clauestb.o${BJ} clin.o${BJ} clinit.o${BJ} clmaxden.o${BJ} clgmult.o${BJ} clmorts.o${BJ} \
            clgmult.o${BJ} 

clim:     ${CLIM_OBJS}

clauestb.o${BJ}:  ${CLIM_SRC}clauestb.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}CLIMATE.F77
	${FC} -c ${FFLAGS} ${CLIM_SRC}clauestb.f

clgmult.o${BJ}:  ${CLIM_SRC}clgmult.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CLIMATE.F77
	${FC} -c ${FFLAGS} ${CLIM_SRC}clgmult.f

clin.o${BJ}:  ${CLIM_SRC}clin.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}CLIMATE.F77
	${FC} -c ${FFLAGS} ${CLIM_SRC}clin.f

clinit.o${BJ}:  ${CLIM_SRC}clinit.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CLIMATE.F77
	${FC} -c ${FFLAGS} ${CLIM_SRC}clinit.f

clmaxden.o${BJ}:  ${CLIM_SRC}clmaxden.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}CLIMATE.F77
	${FC} -c ${FFLAGS} ${CLIM_SRC}clmaxden.f

clmorts.o${BJ}:  ${CLIM_SRC}clmorts.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CLIMATE.F77
	${FC} -c ${FFLAGS} ${CLIM_SRC}clmorts.f

