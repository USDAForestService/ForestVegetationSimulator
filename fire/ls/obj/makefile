# Fortran compiler
include ../../../bin/FC${OS}.make

# the FVS common blocks directory
BASE_COM = ../../../common/
BASE_LS_COM = ../../../ls/common/
FIRE_BASE_COM = ../../base/common/

# source directory
FIRE_LS_SRC = ../src/
FIRE_NI_SRC = ../../ni/src/
FIRE_BASE_SRC = ../../base/src/

FIRE_LS_OBJS = fmbrkt.o${BJ} \
               fmcba.o${BJ} \
               fmcblk.o${BJ}  \
               fmcrow.o${BJ} \
               fmcfmd.o${BJ} \
               fmfout.o${BJ}  \
               fmmois.o${BJ} \
               fmpofl.o${BJ}  \
               fmsnag.o${BJ} \
               fmlsft.o${BJ} \
               fmvinit.o${BJ} \
               fmcons.o${BJ} \
               fmburn.o${BJ}  \
               fmcadd.o${BJ}  \
               fmcbio.o${BJ}  \
               fmcfir.o${BJ}  \
               fmcfmd2.o${BJ} \
               fmchkfwt.o${BJ} \
               fmchrvout.o${BJ}  \
               fmcmpr.o${BJ}  \
               fmcrbout.o${BJ}  \
               fmcrowls.o${BJ}  \
               fmcwd.o${BJ}   \
               fmdout.o${BJ}  \
               fmdyn.o${BJ}   \
               fmeff.o${BJ}   \
               fmevmon.o${BJ} \
               fmfint.o${BJ}  \
               fmgfmv.o${BJ}  \
               fmhide.o${BJ}  \
               fmin.o${BJ}    \
               fminit.o${BJ}  \
               fmkill.o${BJ}  \
               fmmain.o${BJ}  \
               fmoldc.o${BJ}  \
               fmphotocode.o${BJ} \
               fmphotoval.o${BJ} \
               fmpocr.o${BJ}  \
               fmprun.o${BJ}  \
               fmr6sdcy.o${BJ}  \
               fmr6htls.o${BJ}  \
               fmsadd.o${BJ}  \
               fmsalv.o${BJ}  \
               fmscro.o${BJ}  \
               fmscut.o${BJ}  \
               fmsdit.o${BJ}  \
               fmsfall.o${BJ}  \
               fmsout.o${BJ}  \
               fmsngdk.o${BJ}  \
               fmsnght.o${BJ}  \
               fmsoilheat.o${BJ}  \
               fmssee.o${BJ}  \
               fmssum.o${BJ}  \
               fmsvol.o${BJ}  \
               fmsvout.o${BJ} \
               fmsvfl.o${BJ}   \
               fmsvsync.o${BJ} \
               fmsvtobj.o${BJ} \
               fmsvtree.o${BJ} \
               fmtret.o${BJ}  \
               fmtdel.o${BJ}  \
               fmtrip.o${BJ}  \
               fmusrfm.o${BJ}


# options for the compiler
FFLAGS = ${I1}${BASE_LS_COM}${I2}${BASE_COM}${I2}${FIRE_BASE_COM}

fire_ls: ${FIRE_LS_OBJS}

fmbrkt.o${BJ}: \
${FIRE_LS_SRC}fmbrkt.f \
${BASE_LS_COM}PRGPRM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_LS_SRC}fmbrkt.f

fmcba.o${BJ}: \
${FIRE_LS_SRC}fmcba.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_LS_SRC}fmcba.f

fmcblk.o${BJ}: \
${FIRE_LS_SRC}fmcblk.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPROP.F77 \

	${FC} -c ${FFLAGS} ${FIRE_LS_SRC}fmcblk.f

fmcfmd.o${BJ}: \
${FIRE_LS_SRC}fmcfmd.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_LS_SRC}fmcfmd.f

fmcrow.o${BJ}: \
${FIRE_LS_SRC}fmcrow.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}SNCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_LS_SRC}fmcrow.f

fmmois.o${BJ}: \
${FIRE_LS_SRC}fmmois.f \
${BASE_LS_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_LS_SRC}fmmois.f

fmsfall.o${BJ}: \
${FIRE_LS_SRC}fmsfall.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_LS_SRC}fmsfall.f

fmsnag.o${BJ}: \
${FIRE_BASE_SRC}fmsnag.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsnag.f

fmlsft.o${BJ}: \
${FIRE_LS_SRC}fmlsft.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_LS_SRC}fmlsft.f

fmvinit.o${BJ}: \
${FIRE_LS_SRC}fmvinit.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_LS_SRC}fmvinit.f

#  List of routines commpiled from fire/ni

fmcons.o${BJ}: \
${FIRE_NI_SRC}fmcons.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ESHAP.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_NI_SRC}fmcons.f

#  List of routines commpiled from fire/base

fmfout.o${BJ}: \
${FIRE_BASE_SRC}fmfout.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmfout.f

fmburn.o${BJ}: \
${FIRE_BASE_SRC}fmburn.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmburn.f

fmcadd.o${BJ}: \
${FIRE_BASE_SRC}fmcadd.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmcadd.f

fmcbio.o${BJ}: \
${FIRE_BASE_SRC}fmcbio.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMPROP.F77 \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmcbio.f

fmcfir.o${BJ}: \
${FIRE_BASE_SRC}fmcfir.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77   \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmcfir.f

fmcfmd2.o${BJ}: \
${FIRE_BASE_SRC}fmcfmd2.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmcfmd2.f

fmchkfwt.o${BJ}: \
${FIRE_BASE_SRC}fmchkfwt.f \
${BASE_LS_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMPARM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmchkfwt.f

fmchrvout.o${BJ}: \
${FIRE_BASE_SRC}fmchrvout.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMPROP.F77 \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmchrvout.f

fmcmpr.o${BJ}: \
${FIRE_BASE_SRC}fmcmpr.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmcmpr.f

fmcrbout.o${BJ}: \
${FIRE_BASE_SRC}fmcrbout.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmcrbout.f

fmcrowls.o${BJ}: \
${FIRE_BASE_SRC}fmcrowls.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}VARCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmcrowls.f

fmcwd.o${BJ}: \
${FIRE_BASE_SRC}fmcwd.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmcwd.f

fmdout.o${BJ}: \
${FIRE_BASE_SRC}fmdout.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmdout.f

fmdyn.o${BJ}: \
${FIRE_BASE_SRC}fmdyn.f \
${BASE_LS_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmdyn.f

fmeff.o${BJ}: \
${FIRE_BASE_SRC}fmeff.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmeff.f

fmevmon.o${BJ}: \
${FIRE_BASE_SRC}fmevmon.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmevmon.f

fmfint.o${BJ}: \
${FIRE_BASE_SRC}fmfint.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmfint.f

fmgfmv.o${BJ}: \
${FIRE_BASE_SRC}fmgfmv.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmgfmv.f

fmhide.o${BJ}: \
${FIRE_BASE_SRC}fmhide.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmhide.f

fmin.o${BJ}: \
${FIRE_BASE_SRC}fmin.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmin.f

fminit.o${BJ}: \
${FIRE_BASE_SRC}fminit.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fminit.f

fmkill.o${BJ}: \
${FIRE_BASE_SRC}fmkill.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}OUTCOM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmkill.f

fmmain.o${BJ}: \
${FIRE_BASE_SRC}fmmain.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmmain.f

fmoldc.o${BJ}: \
${FIRE_BASE_SRC}fmoldc.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmoldc.f

fmphotocode.o${BJ}: \
${FIRE_BASE_SRC}fmphotocode.f \
${BASE_LS_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmphotocode.f

fmphotoval.o${BJ}: \
${FIRE_BASE_SRC}fmphotoval.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmphotoval.f

fmpocr.o${BJ}: \
${FIRE_BASE_SRC}fmpocr.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmpocr.f

fmpofl.o${BJ}: \
${FIRE_BASE_SRC}fmpofl.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmpofl.f

fmprun.o${BJ}: \
${FIRE_BASE_SRC}fmprun.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmprun.f

fmr6sdcy.o${BJ}: \
${FIRE_BASE_SRC}fmr6sdcy.f \
${BASE_LS_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmr6sdcy.f

fmr6htls.o${BJ}: \
${FIRE_BASE_SRC}fmr6htls.f \
${BASE_LS_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmr6htls.f

fmsadd.o${BJ}: \
${FIRE_BASE_SRC}fmsadd.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsadd.f

fmsalv.o${BJ}: \
${FIRE_BASE_SRC}fmsalv.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMPROP.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsalv.f

fmscro.o${BJ}: \
${FIRE_BASE_SRC}fmscro.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmscro.f

fmscut.o${BJ}: \
${FIRE_BASE_SRC}fmscut.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMPROP.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmscut.f

fmsdit.o${BJ}: \
${FIRE_BASE_SRC}fmsdit.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsdit.f

fmsngdk.o${BJ}: \
${FIRE_BASE_SRC}fmsngdk.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsngdk.f

fmsnght.o${BJ}: \
${FIRE_BASE_SRC}fmsnght.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsnght.f

fmsoilheat.o${BJ}: \
${FIRE_BASE_SRC}fmsoilheat.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsoilheat.f

fmsout.o${BJ}: \
${FIRE_BASE_SRC}fmsout.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsout.f

fmssee.o${BJ}: \
${FIRE_BASE_SRC}fmssee.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmssee.f

fmssum.o${BJ}: \
${FIRE_BASE_SRC}fmssum.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmssum.f

fmsvfl.o${BJ}: \
${FIRE_BASE_SRC}fmsvfl.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}SVDATA.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsvfl.f

fmsvol.o${BJ}: \
${FIRE_BASE_SRC}fmsvol.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsvol.f

fmsvout.o${BJ}: \
${FIRE_BASE_SRC}fmsvout.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}SVDEAD.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsvout.f

fmsvsync.o${BJ}: \
${FIRE_BASE_SRC}fmsvsync.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}SVDEAD.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsvsync.f

fmsvtobj.o${BJ}: \
${FIRE_BASE_SRC}fmsvtobj.f \
${BASE_LS_COM}PRGPRM.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}SVDEAD.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsvtobj.f

fmsvtree.o${BJ}: \
${FIRE_BASE_SRC}fmsvtree.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}SVDEAD.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmsvtree.f

fmtdel.o${BJ}: \
${FIRE_BASE_SRC}fmtdel.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmtdel.f

fmtret.o${BJ}: \
${FIRE_BASE_SRC}fmtret.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmtret.f

fmtrip.o${BJ}: \
${FIRE_BASE_SRC}fmtrip.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmtrip.f

fmusrfm.o${BJ}: \
${FIRE_BASE_SRC}fmusrfm.f \
${BASE_LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_BASE_SRC}fmusrfm.f
