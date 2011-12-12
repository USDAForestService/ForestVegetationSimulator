# Fortran compiler
include ../../bin/FC${OS}.make

# the FVS common blocks directory
BASE_COM = ../../common/

# the EC (East Cascades) source directory
EC_SRC   = ./../src/

# other source directories needed for this variant
SO_SRC   = ./../../so/src/
WS_SRC   = ./../../ws/src/

FFLAGS = ${I1}${BASE_COM}

# the EC object files
EC_OBJS  = blkdat.o${BJ} ccfcal.o${BJ} cratet.o${BJ} crown.o${BJ}  dgf.o${BJ}    dubscr.o${BJ} ecocls.o${BJ} essubh.o${BJ} \
           findag.o${BJ} forkod.o${BJ} formcl.o${BJ} grinit.o${BJ} grohed.o${BJ} habtyp.o${BJ} htcalc.o${BJ} htdbh.o${BJ}  \
           htgf.o${BJ}   morts.o${BJ}  pvref6.o${BJ} regent.o${BJ} sichg.o${BJ}  sitset.o${BJ} smhtgf.o${BJ} \
           varmrt.o${BJ} varvol.o${BJ} \
           avht40.o${BJ} r6crwd.o${BJ} \
           r5crwd.o${BJ}

ec: ${EC_OBJS}

blkdat.o${BJ}: ${EC_SRC}blkdat.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77     \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}PDEN.F77   \
          ${BASE_COM}ECON.F77 ${BASE_COM}HTCAL.F77 ${BASE_COM}CONTRL.F77    \
          ${BASE_COM}PLOT.F77 ${BASE_COM}RANCOM.F77 ${BASE_COM}SCREEN.F77   \
          ${BASE_COM}VARCOM.F77 ${BASE_COM}FVSSTDCM.F77
	${FC} -c ${FFLAGS} ${EC_SRC}blkdat.f

ccfcal.o${BJ}: ${EC_SRC}ccfcal.f ${BASE_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${EC_SRC}ccfcal.f

cratet.o${BJ}: ${EC_SRC}cratet.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}PLOT.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}HTCAL.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${EC_SRC}cratet.f

crown.o${BJ}:  ${EC_SRC}crown.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77        \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PDEN.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${EC_SRC}crown.f

dgf.o${BJ}:    ${EC_SRC}dgf.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CALCOM.F77        \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}PDEN.F77
	${FC} -c ${FFLAGS} ${EC_SRC}dgf.f

dubscr.o${BJ}: ${EC_SRC}dubscr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PDEN.F77
	${FC} -c ${FFLAGS} ${EC_SRC}dubscr.f

ecocls.o${BJ}: ${EC_SRC}ecocls.f
	${FC} -c ${FFLAGS} ${EC_SRC}ecocls.f

essubh.o${BJ}: ${EC_SRC}essubh.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77       \
          ${BASE_COM}ESPARM.F77 ${BASE_COM}ESCOMN.F77
	${FC} -c ${FFLAGS} ${EC_SRC}essubh.f

findag.o${BJ}: ${EC_SRC}findag.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77       \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${EC_SRC}findag.f

forkod.o${BJ}: ${EC_SRC}forkod.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77       \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${EC_SRC}forkod.f

formcl.o${BJ}: ${EC_SRC}formcl.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${EC_SRC}formcl.f

grinit.o${BJ}: ${EC_SRC}grinit.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}HTCAL.F77 ${BASE_COM}ECON.F77 ${BASE_COM}MULTCM.F77    \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}VOLSTD.F77 ${BASE_COM}VARCOM.F77  \
          ${BASE_COM}CWDCOM.F77 ${BASE_COM}SUMTAB.F77 ${BASE_COM}CALCOM.F77 \
          ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${EC_SRC}grinit.f

grohed.o${BJ}: ${EC_SRC}grohed.f ${BASE_COM}includeSVN.f
	${FC} -c ${FFLAGS} ${EC_SRC}grohed.f

habtyp.o${BJ}: ${EC_SRC}habtyp.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${EC_SRC}habtyp.f

htcalc.o${BJ}: ${EC_SRC}htcalc.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${EC_SRC}htcalc.f

htdbh.o${BJ}: ${EC_SRC}htdbh.f
	${FC} -c ${FFLAGS} ${EC_SRC}htdbh.f

htgf.o${BJ}:   ${EC_SRC}htgf.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77       \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}HTCAL.F77  \
          ${BASE_COM}PDEN.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${EC_SRC}htgf.f

morts.o${BJ}:  ${EC_SRC}morts.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77      \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}ESTREE.F77 ${BASE_COM}MULTCM.F77 \
          ${BASE_COM}PDEN.F77   ${BASE_COM}VARCOM.F77 ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${EC_SRC}morts.f

pvref6.o${BJ}: ${EC_SRC}pvref6.f
	${FC} -c ${FFLAGS} ${EC_SRC}pvref6.f

regent.o${BJ}: ${EC_SRC}regent.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CALCOM.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}HTCAL.F77    \
          ${BASE_COM}MULTCM.F77 ${BASE_COM}ESTCOR.F77 ${BASE_COM}PDEN.F77   \
          ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${EC_SRC}regent.f

sichg.o${BJ}:  ${EC_SRC}sichg.f ${BASE_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${EC_SRC}sichg.f

sitset.o${BJ}: ${EC_SRC}sitset.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77     \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${EC_SRC}sitset.f

smhtgf.o${BJ}: ${EC_SRC}smhtgf.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${EC_SRC}smhtgf.f

varmrt.o${BJ}: ${EC_SRC}varmrt.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77			    \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}MULTCM.F77			    \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PDEN.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${EC_SRC}varmrt.f

varvol.o${BJ}: ${EC_SRC}varvol.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${EC_SRC}varvol.f

#
# SO routine compile instructions - source files from so/src
#
avht40.o${BJ}: ${SO_SRC}avht40.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77     \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${SO_SRC}avht40.f

r6crwd.o${BJ}: ${SO_SRC}r6crwd.f
	${FC} -c ${FFLAGS} ${SO_SRC}r6crwd.f

# compile routines from the WS source directory

r5crwd.o${BJ}: ${WS_SRC}r5crwd.f
	${FC} -c ${FFLAGS} ${WS_SRC}r5crwd.f


