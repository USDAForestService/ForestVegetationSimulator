# Fortran compiler
include ../../bin/FC${OS}.make

# the FVS common blocks directory
BASE_COM = ./../../common/

# the NC Klamath Mountains source directory
NC_SRC  = ./../src/
NC_COM  = ./../common/

# the CR Central Rockies
CR_SRC  = ./../../cr/src/

# the SO (SORNEC) source directory
SO_SRC   = ./../../so/src/

# the WS source directory
WS_SRC   = ./../../ws/src/

# the STRP source directory
STRP_SRC = ./../../strp/src/

# options for the compiler
FFLAGS = ${I1}${NC_COM}${I2}${BASE_COM}

# The NC object files
NC_OBJS  = bfvol.o${BJ}  blkdat.o${BJ} bratio.o${BJ} ccfcal.o${BJ} cratet.o${BJ} crown.o${BJ}  \
           dgf.o${BJ}    dubscr.o${BJ} dunn.o${BJ}   ecocls.o${BJ} essubh.o${BJ} findag.o${BJ} \
           forkod.o${BJ} formcl.o${BJ} grinit.o${BJ} grohed.o${BJ} habtyp.o${BJ} htcalc.o${BJ} \
           htdbh.o${BJ}  htgf.o${BJ}   htgr5.o${BJ}  logs.o${BJ}   morts.o${BJ}  nwcmrt.o${BJ} \
           pvref5.o${BJ} pvref6.o${BJ} regent.o${BJ} sichg.o${BJ}  sitset.o${BJ} varvol.o${BJ} \
           esfltr.o${BJ} esgent.o${BJ} esin.o${BJ}   esinit.o${BJ} esprep.o${BJ} estab.o${BJ}  \
           estime.o${BJ} estump.o${BJ} esuckr.o${BJ} crdecd.o${BJ} avht40.o${BJ} r6crwd.o${BJ} \
           r5crwd.o${BJ}

nc: ${NC_OBJS}

bfvol.o${BJ}: \
${NC_SRC}bfvol.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}OUTCOM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${NC_SRC}bfvol.f

blkdat.o${BJ}: \
${NC_SRC}blkdat.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ECON.F77 \
${NC_COM}ESPARM.F77 \
${BASE_COM}ESCOMN.F77 \
${BASE_COM}HTCAL.F77 \
${BASE_COM}KEYCOM.F77 \
${BASE_COM}PDEN.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}RANCOM.F77 \
${BASE_COM}SCREEN.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}FVSSTDCM.F77
	${FC} -c ${FFLAGS} ${NC_SRC}blkdat.f

bratio.o${BJ}: \
${NC_SRC}bratio.f
	${FC} -c ${FFLAGS} ${NC_SRC}bratio.f

ccfcal.o${BJ}: \
${NC_SRC}ccfcal.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NC_SRC}ccfcal.f

cratet.o${BJ}: \
${NC_SRC}cratet.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}OUTCOM.F77 \
${BASE_COM}HTCAL.F77  \
${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${NC_SRC}cratet.f

crown.o${BJ}: \
${NC_SRC}crown.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}OUTCOM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}PDEN.F77 \
${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${NC_SRC}crown.f

dgf.o${BJ}: \
${NC_SRC}dgf.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CALCOM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}OUTCOM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}PDEN.F77 \
${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${NC_SRC}dgf.f

dubscr.o${BJ}: \
${NC_SRC}dubscr.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77   \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PDEN.F77
	${FC} -c ${FFLAGS} ${NC_SRC}dubscr.f

dunn.o${BJ}: \
${NC_SRC}dunn.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NC_SRC}dunn.f

ecocls.o${BJ}: \
${NC_SRC}ecocls.f
	${FC} -c ${FFLAGS} ${NC_SRC}ecocls.f

essubh.o${BJ}: ${NC_SRC}essubh.f ${BASE_COM}PRGPRM.F77 ${NC_COM}ESPARM.F77    \
          ${BASE_COM}ESCOMN.F77
	${FC} -c ${FFLAGS} ${NC_SRC}essubh.f

findag.o${BJ}: \
${NC_SRC}findag.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NC_SRC}findag.f

forkod.o${BJ}: \
${NC_SRC}forkod.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NC_SRC}forkod.f

formcl.o${BJ}: \
${NC_SRC}formcl.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NC_SRC}formcl.f

grinit.o${BJ}: \
${NC_SRC}grinit.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}HTCAL.F77 \
${BASE_COM}ECON.F77 \
${BASE_COM}MULTCM.F77 \
${BASE_COM}VOLSTD.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}CWDCOM.F77 \
${BASE_COM}SUMTAB.F77 \
${BASE_COM}CALCOM.F77 \
${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${NC_SRC}grinit.f

grohed.o${BJ}: \
${NC_SRC}grohed.f ${BASE_COM}includeSVN.f
	${FC} -c ${FFLAGS} ${NC_SRC}grohed.f

habtyp.o${BJ}: \
${NC_SRC}habtyp.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${NC_SRC}habtyp.f

htcalc.o${BJ}: \
${NC_SRC}htcalc.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}VARCOM.F77
	 ${FC} -c ${FFLAGS} ${NC_SRC}htcalc.f

htdbh.o${BJ}: ${NC_SRC}htdbh.f
	${FC} -c ${FFLAGS} ${NC_SRC}htdbh.f

htgf.o${BJ}: \
${NC_SRC}htgf.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}OUTCOM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}MULTCM.F77 \
${BASE_COM}HTCAL.F77 \
${BASE_COM}PDEN.F77
	${FC} -c ${FFLAGS} ${NC_SRC}htgf.f

htgr5.o${BJ}: \
${NC_SRC}htgr5.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NC_SRC}htgr5.f

logs.o${BJ}: \
${NC_SRC}logs.f
	${FC} -c ${FFLAGS} ${NC_SRC}logs.f

morts.o${BJ}: \
${NC_SRC}morts.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}OUTCOM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}ESTREE.F77 \
${BASE_COM}MULTCM.F77 \
${BASE_COM}PDEN.F77 \
${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${NC_SRC}morts.f

nwcmrt.o${BJ}: \
${NC_SRC}nwcmrt.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}OUTCOM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}ESTREE.F77 \
${BASE_COM}MULTCM.F77 \
${BASE_COM}PDEN.F77 \
${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${NC_SRC}nwcmrt.f

pvref5.o${BJ}: \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${NC_SRC}pvref5.f
	${FC} -c ${FFLAGS} ${NC_SRC}pvref5.f

pvref6.o${BJ}: \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${NC_SRC}pvref6.f 
	${FC} -c ${FFLAGS} ${NC_SRC}pvref6.f

regent.o${BJ}: \
${NC_SRC}regent.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CALCOM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}OUTCOM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}PDEN.F77 \
${BASE_COM}HTCAL.F77 \
${BASE_COM}MULTCM.F77 \
${BASE_COM}ESTCOR.F77 \
${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${NC_SRC}regent.f

sichg.o${BJ}: \
${NC_SRC}sichg.f
	${FC} -c ${FFLAGS} ${NC_SRC}sichg.f

sitset.o${BJ}: \
${NC_SRC}sitset.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77   \
${BASE_COM}VARCOM.F77
	 ${FC} -c ${FFLAGS} ${NC_SRC}sitset.f

varvol.o${BJ}: \
${NC_SRC}varvol.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${NC_SRC}varvol.f

esfltr.o${BJ}: ${STRP_SRC}esfltr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${NC_COM}ESPARM.F77    \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}PDEN.F77   \
          ${BASE_COM}CALDEN.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esfltr.f

esgent.o${BJ}: ${STRP_SRC}esgent.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${NC_COM}ESPARM.F77 ${BASE_COM}ESCOMN.F77  \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esgent.f

esin.o${BJ}: ${STRP_SRC}esin.f ${BASE_COM}PRGPRM.F77 ${NC_COM}ESPARM.F77 \
        ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}CONTRL.F77    \
        ${BASE_COM}PLOT.F77 ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esin.f

esinit.o${BJ}: ${STRP_SRC}esinit.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${NC_COM}ESPARM.F77 ${BASE_COM}ESCOMN.F77  \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esinit.f

esprep.o${BJ}: ${STRP_SRC}esprep.f ${BASE_COM}PRGPRM.F77 ${NC_COM}ESPARM.F77    \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esprep.f

estab.o${BJ}: \
${STRP_SRC}estab.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${NC_COM}ESPARM.F77 \
${BASE_COM}ESTREE.F77 \
${BASE_COM}ESHAP.F77 \
${BASE_COM}PDEN.F77 \
${BASE_COM}CALDEN.F77 \
${BASE_COM}ESCOMN.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}STDSTK.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}estab.f

estime.o${BJ}: ${STRP_SRC}estime.f ${BASE_COM}PRGPRM.F77 ${NC_COM}ESPARM.F77    \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}estime.f

estump.o${BJ}: ${STRP_SRC}estump.f ${BASE_COM}PRGPRM.F77 ${NC_COM}ESPARM.F77    \
          ${BASE_COM}ESHOOT.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESCOMN.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}estump.f

esuckr.o${BJ}: ${STRP_SRC}esuckr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${NC_COM}ESPARM.F77    \
          ${BASE_COM}ESHOOT.F77 ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESHAP.F77   \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}STDSTK.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esuckr.f

#  routines compiled from CR source directory

crdecd.o${BJ}: ${CR_SRC}crdecd.f
	${FC} -c ${FFLAGS} ${CR_SRC}crdecd.f
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
