# Fortran compiler
include ../../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../../common/

FIRE_SRC = ../src/
FIRE_BASE_COM = ../common/

FFLAGS = ${I1}${FIRE_BASE_COM}${I2}${BASE_COM}

# the FIRE model object files
FIRE_OBJS = fmburn.o${BJ}  \
            fmcadd.o${BJ}  \
            fmcbio.o${BJ}  \
            fmcfir.o${BJ}  \
            fmcfmd2.o${BJ} \
            fmchkfwt.o${BJ} \
            fmchrvout.o${BJ} \
            fmcmpr.o${BJ}  \
            fmcrbout.o${BJ}  \
            fmcrowe.o${BJ}  \
            fmcroww.o${BJ}  \
            fmcwd.o${BJ}   \
            fmdout.o${BJ}  \
            fmdyn.o${BJ}   \
            fmeff.o${BJ}   \
            fmevmon.o${BJ} \
            fmfint.o${BJ}  \
            fmgfmv.o${BJ}  \
            fmfout.o${BJ}  \
            fmhide.o${BJ}  \
            fmin.o${BJ}    \
            fminit.o${BJ}  \
            fmkill.o${BJ}  \
            fmmain.o${BJ}  \
            fmoldc.o${BJ}  \
            fmphotocode.o${BJ} \
            fmphotoval.o${BJ} \
            fmpocr.o${BJ}  \
            fmpofl.o${BJ}  \
            fmprun.o${BJ}  \
            fmr6fall.o${BJ}  \
            fmr6htls.o${BJ}  \
            fmr6sdcy.o${BJ}  \
            fmsadd.o${BJ}  \
            fmsalv.o${BJ}  \
            fmscro.o${BJ}  \
            fmscut.o${BJ}  \
            fmsdit.o${BJ}  \
            fmsnag.o${BJ}  \
            fmsngdk.o${BJ}  \
            fmsnght.o${BJ}  \
            fmsout.o${BJ}  \
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

fire_base:     ${FIRE_OBJS}

fmburn.o${BJ}: \
${FIRE_SRC}fmburn.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmburn.f

fmcadd.o${BJ}: \
${FIRE_SRC}fmcadd.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmcadd.f

fmcbio.o${BJ}: \
${FIRE_SRC}fmcbio.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMPROP.F77 \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmcbio.f

fmcfir.o${BJ}: \
${FIRE_SRC}fmcfir.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77   \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmcfir.f

fmcfmd2.o${BJ}: \
${FIRE_SRC}fmcfmd2.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmcfmd2.f

fmchkfwt.o${BJ}: \
${FIRE_SRC}fmchkfwt.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMPARM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmchkfwt.f

fmchrvout.o${BJ}: \
${FIRE_SRC}fmchrvout.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMPROP.F77 \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmchrvout.f

fmcrowe.o${BJ}: \
${FIRE_SRC}fmcrowe.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}VARCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmcrowe.f

fmcroww.o${BJ}: \
${FIRE_SRC}fmcroww.f \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmcroww.f

fmcmpr.o${BJ}: \
${FIRE_SRC}fmcmpr.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmcmpr.f

fmcrbout.o${BJ}: \
${FIRE_SRC}fmcrbout.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmcrbout.f

fmcwd.o${BJ}: \
${FIRE_SRC}fmcwd.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmcwd.f

fmdout.o${BJ}: \
${FIRE_SRC}fmdout.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmdout.f

fmdyn.o${BJ}: \
${FIRE_SRC}fmdyn.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmdyn.f

fmeff.o${BJ}: \
${FIRE_SRC}fmeff.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmeff.f

fmevmon.o${BJ}: \
${FIRE_SRC}fmevmon.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmevmon.f

fmfint.o${BJ}: \
${FIRE_SRC}fmfint.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmfint.f

fmfout.o${BJ}: \
${FIRE_SRC}fmfout.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmfout.f

fmgfmv.o${BJ}: \
${FIRE_SRC}fmgfmv.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmgfmv.f

fmhide.o${BJ}: \
${FIRE_SRC}fmhide.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmhide.f

fmin.o${BJ}: \
${FIRE_SRC}fmin.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmin.f

fminit.o${BJ}: \
${FIRE_SRC}fminit.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fminit.f

fmkill.o${BJ}: \
${FIRE_SRC}fmkill.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}OUTCOM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmkill.f

fmmain.o${BJ}: \
${FIRE_SRC}fmmain.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmmain.f

fmoldc.o${BJ}: \
${FIRE_SRC}fmoldc.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmoldc.f

fmphotocode.o${BJ}: \
${FIRE_SRC}fmphotocode.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmphotocode.f

fmphotoval.o${BJ}: \
${FIRE_SRC}fmphotoval.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmphotoval.f

fmpocr.o${BJ}: \
${FIRE_SRC}fmpocr.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmpocr.f

fmpofl.o${BJ}: \
${FIRE_SRC}fmpofl.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmpofl.f

fmprun.o${BJ}: \
${FIRE_SRC}fmprun.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmprun.f

fmr6fall.o${BJ}: \
${FIRE_SRC}fmr6fall.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmr6fall.f

fmr6htls.o${BJ}: \
${FIRE_SRC}fmr6htls.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmr6htls.f

fmr6sdcy.o${BJ}: \
${FIRE_SRC}fmr6sdcy.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmr6sdcy.f

fmsadd.o${BJ}: \
${FIRE_SRC}fmsadd.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsadd.f

fmsalv.o${BJ}: \
${FIRE_SRC}fmsalv.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMPROP.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsalv.f

fmscro.o${BJ}: \
${FIRE_SRC}fmscro.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmscro.f

fmscut.o${BJ}: \
${FIRE_SRC}fmscut.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMPROP.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmscut.f

fmsdit.o${BJ}: \
${FIRE_SRC}fmsdit.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsdit.f

fmsnag.o${BJ}: \
${FIRE_SRC}fmsnag.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsnag.f

fmsngdk.o${BJ}: \
${FIRE_SRC}fmsngdk.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsngdk.f

fmsnght.o${BJ}: \
${FIRE_SRC}fmsnght.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsnght.f

fmsoilheat.o${BJ}: \
${FIRE_SRC}fmsoilheat.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsoilheat.f

fmsout.o${BJ}: \
${FIRE_SRC}fmsout.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsout.f

fmssee.o${BJ}: \
${FIRE_SRC}fmssee.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmssee.f

fmssum.o${BJ}: \
${FIRE_SRC}fmssum.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmssum.f

fmsvfl.o${BJ}: \
${FIRE_SRC}fmsvfl.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}SVDATA.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsvfl.f

fmsvol.o${BJ}: \
${FIRE_SRC}fmsvol.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsvol.f

fmsvout.o${BJ}: \
${FIRE_SRC}fmsvout.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}SVDEAD.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsvout.f

fmsvsync.o${BJ}: \
${FIRE_SRC}fmsvsync.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}SVDEAD.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsvsync.f

fmsvtobj.o${BJ}: \
${FIRE_SRC}fmsvtobj.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}SVDEAD.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsvtobj.f

fmsvtree.o${BJ}: \
${FIRE_SRC}fmsvtree.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}SVDEAD.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmsvtree.f

fmtdel.o${BJ}: \
${FIRE_SRC}fmtdel.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmtdel.f

fmtret.o${BJ}: \
${FIRE_SRC}fmtret.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmtret.f

fmtrip.o${BJ}: \
${FIRE_SRC}fmtrip.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmtrip.f

fmusrfm.o${BJ}: \
${FIRE_SRC}fmusrfm.f \
${BASE_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_SRC}fmusrfm.f


