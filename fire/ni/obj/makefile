# Fortran compiler
include ../../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../../common/
FIRE_BASE_COM = ../../base/common/

# source directories
FIRE_NI_SRC = ../src/
FIRE_BASE_SRC = ../../base/src/

FFLAGS = ${I1}${BASE_COM}${I2}${FIRE_BASE_COM}

# the FIRE model object files
FIRE_OBJS = fmbrkt.o${BJ}  \
            fmcba.o${BJ}   \
            fmcblk.o${BJ}  \
            fmcfmd.o${BJ}  \
            fmcons.o${BJ}  \
            fmcrow.o${BJ}  \
            fmmois.o${BJ}  \
            fmsfall.o${BJ}  \
            fmvinit.o${BJ} 

fire_ni: ${FIRE_OBJS}

fmbrkt.o${BJ}: \
${FIRE_NI_SRC}fmbrkt.f \
${BASE_COM}PRGPRM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_NI_SRC}fmbrkt.f

fmcba.o${BJ}: \
${FIRE_NI_SRC}fmcba.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_NI_SRC}fmcba.f

fmcblk.o${BJ}: \
${FIRE_NI_SRC}fmcblk.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPROP.F77 \

	${FC} -c ${FFLAGS} ${FIRE_NI_SRC}fmcblk.f

fmcfmd.o${BJ}: \
${FIRE_NI_SRC}fmcfmd.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \

	${FC} -c ${FFLAGS} ${FIRE_NI_SRC}fmcfmd.f

fmcons.o${BJ}: \
${FIRE_NI_SRC}fmcons.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ESHAP.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_NI_SRC}fmcons.f

fmcrow.o${BJ}: \
${FIRE_NI_SRC}fmcrow.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \

	${FC} -c ${FFLAGS} ${FIRE_NI_SRC}fmcrow.f

fmmois.o${BJ}: \
${FIRE_NI_SRC}fmmois.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_NI_SRC}fmmois.f

fmsfall.o${BJ}: \
${FIRE_NI_SRC}fmsfall.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_NI_SRC}fmsfall.f

fmvinit.o${BJ}: \
${FIRE_NI_SRC}fmvinit.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_NI_SRC}fmvinit.f

