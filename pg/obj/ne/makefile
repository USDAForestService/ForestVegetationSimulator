# Fortran compiler
include ../../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../../common/

# the ie variant common blocks directory
NE_COM = ../../../ne/common/

# the Fuels and Fire Effects model source directory
FIRE_BASE_COM = ../../../fire/base/common/

# the dwarf mistletoe interim model source directory
MIST_SRC = ../../../mistoe/src/

# the pg base source directory
PGBASE_SRC   = ../../src/

# options for the compiler
FFLAGS = ${I1}${NE_COM}${I2}${BASE_COM}${I2}${MIST_SRC}${I2}${FIRE_BASE_COM}

# the PG object files
PG_OBJS = chget.o${BJ} ecnget.o${BJ} fmppget.o${BJ} \
          fmpphv.o${BJ} fmppput.o${BJ} putgetsubs.o${BJ} putstd.o${BJ} \
          chput.o${BJ}  ecnput.o${BJ} \
          getstd.o${BJ} stash.o${BJ}

pg: ${PG_OBJS}

chget.o${BJ}:  ${PGBASE_SRC}chget.f ${NE_COM}PRGPRM.F77   \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}CALDEN.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}DBSTK.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${PGBASE_SRC}chget.f

chput.o${BJ}:  ${PGBASE_SRC}chput.f ${NE_COM}PRGPRM.F77   \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}CALDEN.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}DBSTK.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${PGBASE_SRC}chput.f

ecnget.o${BJ}:  ${PGBASE_SRC}ecnget.f ${NE_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${PGBASE_SRC}ecnget.f

ecnput.o${BJ}:  ${PGBASE_SRC}ecnput.f ${NE_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${PGBASE_SRC}ecnput.f

varget.o${BJ}: ${PPNE_COM}varget.f ${NE_COM}PRGPRM.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${PPNE_COM}varget.f

varput.o${BJ}: ${PPNE_COM}varput.f ${NE_COM}PRGPRM.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${PPNE_COM}varput.f

fmppget.o${BJ}: \
${PGBASE_SRC}fmppget.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}SVDATA.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77  \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \

	${FC} -c ${FFLAGS} ${PGBASE_SRC}fmppget.f

fmppput.o${BJ}: \
${PGBASE_SRC}fmppput.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}SVDATA.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77  \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \

	${FC} -c ${FFLAGS} ${PGBASE_SRC}fmppput.f

fmpphv.o${BJ}: \
${PGBASE_SRC}fmpphv.f \
${NE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77  \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${PGBASE_SRC}fmpphv.f

putgetsubs.o${BJ}: ${PGBASE_SRC}putgetsubs.f 
	${FC} -c ${FFLAGS} ${PGBASE_SRC}putgetsubs.f

getstd.o${BJ}: ${PGBASE_SRC}getstd.f ${NE_COM}PRGPRM.F77  \
          ${BASE_COM}PPDNCM.F77 ${NE_COM}ESPARM.F77  \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}CALCOM.F77 ${BASE_COM}CALDEN.F77 ${BASE_COM}ECON.F77    \
          ${BASE_COM}ESHAP.F77  ${BASE_COM}ESHAP2.F77 ${BASE_COM}ESCOMN.F77   \
          ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESTCOR.F77 ${BASE_COM}ESTREE.F77  \
          ${BASE_COM}HTCAL.F77  ${BASE_COM}MULTCM.F77 ${BASE_COM}OPCOM.F77  \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PDEN.F77   ${BASE_COM}PLOT.F77   \
          ${BASE_COM}VOLSTD.F77 ${BASE_COM}RANCOM.F77 ${BASE_COM}ESRNCM.F77  \
          ${BASE_COM}DBSTK.F77  ${BASE_COM}VARCOM.F77 ${BASE_COM}SUMTAB.F77 \
          ${BASE_COM}CWDCOM.F77 ${BASE_COM}FVSSTDCM.F77
	${FC} -c ${FFLAGS} ${PGBASE_SRC}getstd.f

putstd.o${BJ}:  ${PGBASE_SRC}putstd.f ${NE_COM}PRGPRM.F77 \
           ${BASE_COM}PPDNCM.F77 ${NE_COM}ESPARM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}CALCOM.F77 ${BASE_COM}CALDEN.F77 ${BASE_COM}ECON.F77   \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77 ${BASE_COM}ESCOMN.F77  \
           ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESTCOR.F77 ${BASE_COM}ESTREE.F77 \
           ${BASE_COM}HTCAL.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}OPCOM.F77   \
           ${BASE_COM}OUTCOM.F77 ${BASE_COM}PDEN.F77 ${BASE_COM}PLOT.F77     \
           ${BASE_COM}VOLSTD.F77 ${BASE_COM}ESRNCM.F77 ${BASE_COM}RANCOM.F77 \
           ${BASE_COM}DBSTK.F77 ${BASE_COM}VARCOM.F77 ${BASE_COM}SUMTAB.F77 \
           ${BASE_COM}CWDCOM.F77 ${BASE_COM}FVSSTDCM.F77
	${FC} -c ${FFLAGS} ${PGBASE_SRC}putstd.f


stash.o${BJ}: ${PGBASE_SRC}stash.f ${BASE_COM}GLBLCNTL.F77 
	${FC} -c ${FFLAGS} ${PGBASE_SRC}stash.f

