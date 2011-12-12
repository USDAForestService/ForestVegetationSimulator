# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../common/

# the NI (northern Idaho) source directory
NI_SRC   = ./../src/

# other source directories needed for this variant
SO_SRC   = ./../../so/src/
WS_SRC   = ./../../ws/src/

#Compiler options
FFLAGS = ${I1}${BASE_COM}

# the NI object files
NI_OBJS  = bftopk.o${BJ} bfvol.o${BJ} blkdat.o${BJ} bratio.o${BJ} ccfcal.o${BJ} cftopk.o${BJ} cfvol.o${BJ}  \
           cratet.o${BJ} crown.o${BJ} cubrds.o${BJ} cutstk.o${BJ} cwcalc.o${BJ}    \
           dgbnd.o${BJ} dgdriv.o${BJ} dgf.o${BJ} dubscr.o${BJ} dunn.o${BJ} exppnb.o${BJ} findag.o${BJ} \
           forkod.o${BJ} formcl.o${BJ} \
           grinit.o${BJ} grohed.o${BJ} habtyp.o${BJ} htgf.o${BJ} logs.o${BJ} maical.o${BJ} morts.o${BJ}  \
           pvref1.o${BJ} rcon.o${BJ} regent.o${BJ} sitset.o${BJ} spctrn.o${BJ} varvol.o${BJ} vols.o${BJ} \
           r6crwd.o${BJ} \
           r5crwd.o${BJ}

ni: ${NI_OBJS}

bfvol.o${BJ}:  ${NI_SRC}bfvol.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${NI_SRC}bfvol.f

bftopk.o${BJ}: ${NI_SRC}bftopk.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77    \
          ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${NI_SRC}bftopk.f

blkdat.o${BJ}: ${NI_SRC}blkdat.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77    \
          ${BASE_COM}ESPARM.F77 ${BASE_COM}ESCOMN.F77 ${BASE_COM}PDEN.F77  \
          ${BASE_COM}ECON.F77 ${BASE_COM}HTCAL.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}RANCOM.F77 ${BASE_COM}SCREEN.F77 \
          ${BASE_COM}FVSSTDCM.F77
	${FC} -c ${FFLAGS} ${NI_SRC}blkdat.f

bratio.o${BJ}: ${NI_SRC}bratio.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77
	${FC} -c ${FFLAGS} ${NI_SRC}bratio.f

ccfcal.o${BJ}: ${NI_SRC}ccfcal.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}ccfcal.f

cfvol.o${BJ}:  ${NI_SRC}cfvol.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cfvol.f

cftopk.o${BJ}: ${NI_SRC}cftopk.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77    \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cftopk.f

cratet.o${BJ}: ${NI_SRC}cratet.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}PLOT.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}HTCAL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cratet.f

crown.o${BJ}:  ${NI_SRC}crown.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CALCOM.F77      \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}crown.f

cubrds.o${BJ}: ${NI_SRC}cubrds.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cubrds.f

cutstk.o${BJ}: ${NI_SRC}cutstk.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77      \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cutstk.f

cwcalc.o${BJ}: ${NI_SRC}cwcalc.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cwcalc.f

dgbnd.o${BJ}:  ${NI_SRC}dgbnd.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}dgbnd.f

dgdriv.o${BJ}: ${NI_SRC}dgdriv.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CALCOM.F77      \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}PLOT.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}ESTCOR.F77    \
          ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${NI_SRC}dgdriv.f

dgf.o${BJ}:    ${NI_SRC}dgf.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CALCOM.F77         \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}dgf.f

dubscr.o${BJ}: ${NI_SRC}dubscr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}dubscr.f

dunn.o${BJ}:   ${NI_SRC}dunn.f
	${FC} -c ${FFLAGS} ${NI_SRC}dunn.f

exppnb.o${BJ}: ${NI_SRC}exppnb.f
	${FC} -c ${FFLAGS} ${NI_SRC}exppnb.f

findag.o${BJ}: ${NI_SRC}forkod.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}findag.f

forkod.o${BJ}: ${NI_SRC}forkod.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77        \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}forkod.f

formcl.o${BJ}: ${NI_SRC}formcl.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}formcl.f

grinit.o${BJ}: ${NI_SRC}grinit.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77      \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}HTCAL.F77 ${BASE_COM}ECON.F77 ${BASE_COM}MULTCM.F77     \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}VOLSTD.F77 ${BASE_COM}VARCOM.F77   \
          ${BASE_COM}CWDCOM.F77 ${BASE_COM}SUMTAB.F77 ${BASE_COM}CALCOM.F77 \
          ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${NI_SRC}grinit.f

grohed.o${BJ}: ${NI_SRC}grohed.f ${BASE_COM}includeSVN.f
	${FC} -c ${FFLAGS} ${NI_SRC}grohed.f

habtyp.o${BJ}: ${NI_SRC}habtyp.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77        \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}habtyp.f

htgf.o${BJ}:   ${NI_SRC}htgf.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CALCOM.F77        \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}PLOT.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}HTCAL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}htgf.f

logs.o${BJ}:   ${NI_SRC}logs.f ${BASE_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${NI_SRC}logs.f

maical.o${BJ}: ${NI_SRC}maical.f
	${FC} -c ${FFLAGS} ${NI_SRC}maical.f

morts.o${BJ}:  ${NI_SRC}morts.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77       \
          ${BASE_COM}PLOT.F77   ${BASE_COM}CALCOM.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}ESTREE.F77 ${BASE_COM}MULTCM.F77  \
          ${BASE_COM}PDEN.F77   ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${NI_SRC}morts.f

pvref1.o${BJ}:   ${NI_SRC}pvref1.f
	${FC} -c ${FFLAGS} ${NI_SRC}pvref1.f

rcon.o${BJ}:   ${NI_SRC}rcon.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77        \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}rcon.f

regent.o${BJ}: ${NI_SRC}regent.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CALCOM.F77      \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}PDEN.F77      \
          ${BASE_COM}HTCAL.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}ESTCOR.F77
	${FC} -c ${FFLAGS} ${NI_SRC}regent.f

sitset.o${BJ}: ${NI_SRC}sitset.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}sitset.f

spctrn.o${BJ}: ${NI_SRC}spctrn.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}spctrn.f

varvol.o${BJ}: ${NI_SRC}varvol.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}VOLSTD.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}varvol.f

vols.o${BJ}:   ${NI_SRC}vols.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${NI_SRC}vols.f

# compile routines from the SO source directory

r6crwd.o${BJ}: ${SO_SRC}r6crwd.f
	${FC} -c ${FFLAGS} ${SO_SRC}r6crwd.f

# compile routines from the WS source directory

r5crwd.o${BJ}: ${WS_SRC}r5crwd.f
	${FC} -c ${FFLAGS} ${WS_SRC}r5crwd.f

