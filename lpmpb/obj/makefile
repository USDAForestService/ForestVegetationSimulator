# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../common/

# the mountain pine beetle source directory
MPB_SRC  = ./../src/

# options for the compiler
FFLAGS = ${I1}${MPB_SRC}${I2}${BASE_COM}

# the mountain pine beetle object files
MPB_OBJS = back.o${BJ} betin.o${BJ} coldbh.o${BJ} coldrv.o${BJ} colind.o${BJ} colmod.o${BJ} colmrt.o${BJ}       \
           emerg.o${BJ} forw.o${BJ} garbel.o${BJ} grclas.o${BJ} grpsum.o${BJ} mpbcup.o${BJ} mpbdam.o${BJ}       \
           mpbdrv.o${BJ} mpber.o${BJ} mpbgam.o${BJ} mpbgo.o${BJ} mpbhed.o${BJ} mpbin.o${BJ} mpbint.o${BJ}  \
           mpblkd.o${BJ} mpbmod.o${BJ} mpbout.o${BJ} mpbtab.o${BJ} mpgr.o${BJ} mpotpr.o${BJ} mprann.o${BJ}      \
           mpsdlp.o${BJ} percnt.o${BJ} pmslp.o${BJ} pqsml.o${BJ} ptary.o${BJ} ptbno.o${BJ} ptgrp.o${BJ} ptint.o${BJ} \
           ptplt.o${BJ} ptsav.o${BJ} ptscl.o${BJ} ptsrg.o${BJ} ptssc.o${BJ} ptsym.o${BJ} surfce.o${BJ} surflp.o${BJ} \
           tafit.o${BJ}

lpmpb:    ${MPB_OBJS}

back.o${BJ}:   ${MPB_SRC}back.f ${MPB_SRC}MPBETA.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}back.f

betin.o${BJ}:  ${MPB_SRC}betin.f ${MPB_SRC}MPBETA.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}betin.f

coldbh.o${BJ}: ${MPB_SRC}coldbh.f ${BASE_COM}PRGPRM.F77 ${MPB_SRC}COLCOM.F77    \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}coldbh.f

coldrv.o${BJ}: ${MPB_SRC}coldrv.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77     \
          ${BASE_COM}CONTRL.F77 ${MPB_SRC}COLCOM.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}coldrv.f

colind.o${BJ}: ${MPB_SRC}colind.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}colind.f

colmod.o${BJ}: ${MPB_SRC}colmod.f ${BASE_COM}PRGPRM.F77 ${MPB_SRC}COLCOM.F77    \
          ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}colmod.f

colmrt.o${BJ}: ${MPB_SRC}colmrt.f ${BASE_COM}PRGPRM.F77 ${MPB_SRC}MPBCOM.F77    \
          ${MPB_SRC}COLCOM.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}colmrt.f

emerg.o${BJ}:  ${MPB_SRC}emerg.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}emerg.f

forw.o${BJ}:   ${MPB_SRC}forw.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}forw.f

garbel.o${BJ}: ${MPB_SRC}garbel.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}garbel.f

grclas.o${BJ}: ${MPB_SRC}grclas.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}grclas.f

grpsum.o${BJ}: ${MPB_SRC}grpsum.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}grpsum.f

mpbcup.o${BJ}: ${MPB_SRC}mpbcup.f ${BASE_COM}PRGPRM.F77 ${MPB_SRC}MPBCOM.F77    \
          ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbcup.f

mpbdam.o${BJ}: ${MPB_SRC}mpbdam.f ${BASE_COM}PRGPRM.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbdam.f

mpbdrv.o${BJ}: ${MPB_SRC}mpbdrv.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbdrv.f

mpber.o${BJ}:  ${MPB_SRC}mpber.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpber.f

mpbgam.o${BJ}: ${MPB_SRC}mpbgam.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbgam.f

mpbgo.o${BJ}:  ${MPB_SRC}mpbgo.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbgo.f

mpbhed.o${BJ}: ${MPB_SRC}mpbhed.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77     \
          ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbhed.f

mpbin.o${BJ}:  ${MPB_SRC}mpbin.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77 ${MPB_SRC}COLCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbin.f

mpbint.o${BJ}: ${MPB_SRC}mpbint.f ${BASE_COM}PRGPRM.F77 ${MPB_SRC}MPBCOM.F77    \
          ${MPB_SRC}COLCOM.F77 ${MPB_SRC}MPBETA.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbint.f

mpblkd.o${BJ}: ${MPB_SRC}mpblkd.f ${BASE_COM}PRGPRM.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpblkd.f

mpbmod.o${BJ}: ${MPB_SRC}mpbmod.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbmod.f

mpbout.o${BJ}: ${MPB_SRC}mpbout.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77     \
          ${BASE_COM}CONTRL.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbout.f

mpbtab.o${BJ}: ${MPB_SRC}mpbtab.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${MPB_SRC}MPBCOM.F77 ${MPB_SRC}COLCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbtab.f

mpgr.o${BJ}:   ${MPB_SRC}mpgr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CONTRL.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpgr.f

mpotpr.o${BJ}: ${MPB_SRC}mpotpr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpotpr.f

mprann.o${BJ}: ${MPB_SRC}mprann.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}mprann.f

mpsdlp.o${BJ}: ${MPB_SRC}mpsdlp.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${MPB_SRC}MPBCOM.F77 ${MPB_SRC}COLCOM.F77     \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpsdlp.f

percnt.o${BJ}: ${MPB_SRC}percnt.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}percnt.f

pmslp.o${BJ}:  ${MPB_SRC}pmslp.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}pmslp.f

pqsml.o${BJ}:  ${MPB_SRC}pqsml.f ${MPB_SRC}MPBETA.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}pqsml.f

ptary.o${BJ}:  ${MPB_SRC}ptary.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptary.f

ptbno.o${BJ}:  ${MPB_SRC}ptbno.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}ptbno.f

ptgrp.o${BJ}:  ${MPB_SRC}ptgrp.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptgrp.f

ptint.o${BJ}:  ${MPB_SRC}ptint.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}ptint.f

ptplt.o${BJ}:  ${MPB_SRC}ptplt.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptplt.f

ptsav.o${BJ}:  ${MPB_SRC}ptsav.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptsav.f

ptscl.o${BJ}:  ${MPB_SRC}ptscl.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptscl.f

ptsrg.o${BJ}:  ${MPB_SRC}ptsrg.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}ptsrg.f

ptssc.o${BJ}:  ${MPB_SRC}ptssc.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptssc.f

ptsym.o${BJ}:  ${MPB_SRC}ptsym.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptsym.f

surfce.o${BJ}: ${MPB_SRC}surfce.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}surfce.f

surflp.o${BJ}: ${MPB_SRC}surflp.f 
	${FC} -c ${FFLAGS} ${MPB_SRC}surflp.f

tafit.o${BJ}:  ${MPB_SRC}tafit.f ${BASE_COM}PRGPRM.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}tafit.f
