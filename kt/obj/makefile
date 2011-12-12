# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../common/

# the KT (Kootenai) source directory
KT_SRC   = ./../src/

# other source directories needed for this variant
SO_SRC   = ./../../so/src/
WS_SRC   = ./../../ws/src/

# options for the compiler
FFLAGS = ${I1}${BASE_COM} 

# the KT object files
KT_OBJS  = blkdat.o${BJ} cftopk.o${BJ} cfvol.o${BJ} cratet.o${BJ} cubrds.o${BJ} dgf.o${BJ} exppnb.o${BJ}  \
           forkod.o${BJ} grinit.o${BJ} grohed.o${BJ} habtyp.o${BJ} ktfctr.o${BJ} \
           pvref1.o${BJ} regent.o${BJ} smhtrg.o${BJ} r6crwd.o${BJ} \
           r5crwd.o${BJ}

kt: ${KT_OBJS}

blkdat.o${BJ}:   ${KT_SRC}blkdat.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77   \
            ${BASE_COM}ESPARM.F77 ${BASE_COM}ESCOMN.F77 ${BASE_COM}PDEN.F77 \
            ${BASE_COM}ECON.F77 ${BASE_COM}HTCAL.F77 ${BASE_COM}CONTRL.F77  \
            ${BASE_COM}PLOT.F77 ${BASE_COM}RANCOM.F77 ${BASE_COM}SCREEN.F77 \
            ${BASE_COM}KOTCOM.F77 ${BASE_COM}FVSSTDCM.F77
	${FC} -c ${FFLAGS} ${KT_SRC}blkdat.f

cftopk.o${BJ}:   ${KT_SRC}cftopk.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77 \
            ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${KT_SRC}cftopk.f

cfvol.o${BJ}:    ${KT_SRC}cfvol.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
            ${BASE_COM}PLOT.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77  \
            ${BASE_COM}OUTCOM.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${KT_SRC}cfvol.f

cratet.o${BJ}:   ${KT_SRC}cratet.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
            ${BASE_COM}PLOT.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77  \
            ${BASE_COM}OUTCOM.F77 ${BASE_COM}HTCAL.F77
	${FC} -c ${FFLAGS} ${KT_SRC}cratet.f

cubrds.o${BJ}:   ${KT_SRC}cubrds.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${KT_SRC}cubrds.f

dgf.o${BJ}:      ${KT_SRC}dgf.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CALCOM.F77      \
            ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}PDEN.F77 \
            ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}KOTCOM.F77
	${FC} -c ${FFLAGS} ${KT_SRC}dgf.f

exppnb.o${BJ}:   ${KT_SRC}exppnb.f
	${FC} -c ${FFLAGS} ${KT_SRC}exppnb.f

forkod.o${BJ}:   ${KT_SRC}forkod.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77     \
            ${BASE_COM}CONTRL.F77 ${BASE_COM}KOTCOM.F77
	${FC} -c ${FFLAGS} ${KT_SRC}forkod.f

grinit.o${BJ}:   ${KT_SRC}grinit.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
            ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 \
            ${BASE_COM}HTCAL.F77 ${BASE_COM}ECON.F77 ${BASE_COM}MULTCM.F77  \
            ${BASE_COM}OPCOM.F77 ${BASE_COM}VOLSTD.F77 ${BASE_COM}VARCOM.F77 \
            ${BASE_COM}CWDCOM.F77 ${BASE_COM}SUMTAB.F77 ${BASE_COM}CALCOM.F77 \
            ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${KT_SRC}grinit.f

grohed.o${BJ}:   ${KT_SRC}grohed.f ${BASE_COM}includeSVN.f
	${FC} -c ${FFLAGS} ${KT_SRC}grohed.f

habtyp.o${BJ}:   ${KT_SRC}habtyp.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77     \
            ${BASE_COM}CONTRL.F77 ${BASE_COM}KOTCOM.F77
	${FC} -c ${FFLAGS} ${KT_SRC}habtyp.f

ktfctr.o${BJ}:   ${KT_SRC}ktfctr.f ${BASE_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${KT_SRC}ktfctr.f

pvref1.o${BJ}:   ${KT_SRC}pvref1.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${KT_SRC}pvref1.f

regent.o${BJ}:   ${KT_SRC}regent.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CALCOM.F77     \
            ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
            ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}PDEN.F77     \
            ${BASE_COM}HTCAL.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}ESTCOR.F77  \
            ${BASE_COM}KOTCOM.F77
	${FC} -c ${FFLAGS} ${KT_SRC}regent.f

smhtrg.o${BJ}:   ${KT_SRC}smhtrg.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
            ${BASE_COM}PLOT.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77   \
            ${BASE_COM}OUTCOM.F77 ${BASE_COM}HTCAL.F77
	${FC} -c ${FFLAGS} ${KT_SRC}smhtrg.f

r6crwd.o${BJ}: ${SO_SRC}r6crwd.f
	${FC} -c ${FFLAGS} ${SO_SRC}r6crwd.f

# compile routines from the WS source directory

r5crwd.o${BJ}: ${WS_SRC}r5crwd.f
	${FC} -c ${FFLAGS} ${WS_SRC}r5crwd.f
