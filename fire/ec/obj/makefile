# Fortran compiler
include ../../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../../common/
FIRE_BASE_COM = ../../base/common/

FIRE_EC_SRC = ../src/
FIRE_BASE_SRC = ../../base/src/
FIRE_PN_SRC =   ../../pn/src/

FFLAGS = ${I1}${BASE_COM}${I2}${FIRE_BASE_COM} 

# the FIRE model object files
FIRE_OBJS = fmbrkt.o${BJ}  \
            fmcba.o${BJ}   \
            fmcblk.o${BJ}  \
            fmcfmd.o${BJ}  \
            fmcrow.o${BJ}  \
            fmsfall.o${BJ}  \
            fmvinit.o${BJ} 

fire_ec: ${FIRE_OBJS}

fmbrkt.o${BJ}: \
${FIRE_EC_SRC}fmbrkt.f \
${BASE_COM}PRGPRM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_EC_SRC}fmbrkt.f

fmcba.o${BJ}: \
${FIRE_EC_SRC}fmcba.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_EC_SRC}fmcba.f

fmcblk.o${BJ}: \
${FIRE_EC_SRC}fmcblk.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPROP.F77 \

	${FC} -c ${FFLAGS} ${FIRE_EC_SRC}fmcblk.f

fmcfmd.o${BJ}: \
${FIRE_EC_SRC}fmcfmd.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}SSTGMC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_EC_SRC}fmcfmd.f

fmcrow.o${BJ}: \
${FIRE_EC_SRC}fmcrow.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \

	${FC} -c ${FFLAGS} ${FIRE_EC_SRC}fmcrow.f

fmsfall.o${BJ}: \
${FIRE_PN_SRC}fmsfall.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_PN_SRC}fmsfall.f

fmvinit.o${BJ}: \
${FIRE_EC_SRC}fmvinit.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_EC_SRC}fmvinit.f

