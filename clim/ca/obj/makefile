# Fortran compiler
include ../../../bin/FC${OS}.make

# the FVS common blocks directory
BASE_CA_COM = ../../../ca/common/
BASE_COM = ../../../common/

# the Climate Extension source directory
CLIM_BASE_SRC = ../../base/src/

# options for the compiler
FFLAGS = ${I1}${BASE_CA_COM}${I2}${BASE_COM}

# the Climate Extension object files
CLIM_OBJS = clauestb.o${BJ} clin.o${BJ} clinit.o${BJ} clmaxden.o${BJ} clgmult.o${BJ} clmorts.o${BJ}

clim:     ${CLIM_OBJS}

clauestb.o${BJ}:  ${CLIM_BASE_SRC}clauestb.f ${BASE_CA_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}CLIMATE.F77
	${FC} -c ${FFLAGS} ${CLIM_BASE_SRC}clauestb.f

clgmult.o${BJ}:  ${CLIM_BASE_SRC}clgmult.f ${BASE_CA_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CLIMATE.F77
	${FC} -c ${FFLAGS} ${CLIM_BASE_SRC}clgmult.f

clin.o${BJ}:  ${CLIM_BASE_SRC}clin.f ${BASE_CA_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}CLIMATE.F77
	${FC} -c ${FFLAGS} ${CLIM_BASE_SRC}clin.f

clinit.o${BJ}:  ${CLIM_BASE_SRC}clinit.f ${BASE_CA_COM}PRGPRM.F77 ${BASE_COM}CLIMATE.F77
	${FC} -c ${FFLAGS} ${CLIM_BASE_SRC}clinit.f

clmaxden.o${BJ}:  ${CLIM_BASE_SRC}clmaxden.f ${BASE_CA_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}CLIMATE.F77
	${FC} -c ${FFLAGS} ${CLIM_BASE_SRC}clmaxden.f

clmorts.o${BJ}:  ${CLIM_BASE_SRC}clmorts.f ${BASE_CA_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CLIMATE.F77
	${FC} -c ${FFLAGS} ${CLIM_BASE_SRC}clmorts.f

