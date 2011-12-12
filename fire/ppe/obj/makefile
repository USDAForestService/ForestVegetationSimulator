# Fortran compiler
include ../../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../../common/
FIRE_BASE_COM = ../../base/common/

FIRE_SRC = ../../base/src/
FIRE_PPE_SRC = ../src/

FFLAGS = ${I1}${BASE_COM}${I2}${FIRE_BASE_COM}

# the FIRE model object files
FIRE_OBJS = fmppget.o${BJ}  \
            fmppput.o${BJ}  \
            fmpphv.o${BJ}

fire_ppe: ${FIRE_OBJS}

fmppget.o${BJ}: \
${FIRE_PPE_SRC}fmppget.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}PPEPRM.F77 \
${BASE_COM}PPCNTL.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}SVDATA.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77  \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_PPE_SRC}fmppget.f

fmppput.o${BJ}: \
${FIRE_PPE_SRC}fmppput.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}PPEPRM.F77 \
${BASE_COM}PPCNTL.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}SVDATA.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77  \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_PPE_SRC}fmppput.f

fmpphv.o${BJ}: \
${FIRE_PPE_SRC}fmpphv.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77  \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_PPE_SRC}fmpphv.f


