# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../common/

# the ESTB model source directory
ESTB_SRC = ./../src/

FFLAGS = ${I1}${BASE_COM}

# the ESTB model object files
ESTB_OBJS = esaddt.o${BJ} esadvh.o${BJ} esblkd.o${BJ} escprs.o${BJ} esdlay.o${BJ} esetpr.o${BJ} esfltr.o${BJ} esgent.o${BJ}  \
            esin.o${BJ} esinit.o${BJ} esmsgs.o${BJ} esnspe.o${BJ} esnutr.o${BJ} esout.o${BJ} espadv.o${BJ}     \
            esplt1.o${BJ} esplt2.o${BJ} esprep.o${BJ} esprin.o${BJ} espsub.o${BJ} espxcs.o${BJ} esrann.o${BJ}  \
            essubh.o${BJ} estab.o${BJ} estime.o${BJ} estock.o${BJ} estpp.o${BJ} estump.o${BJ} esuckr.o${BJ}    \
            esxcsh.o${BJ}

estb:      ${ESTB_OBJS}

esaddt.o${BJ}:  ${ESTB_SRC}esaddt.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esaddt.f

esadvh.o${BJ}:  ${ESTB_SRC}esadvh.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77  \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esadvh.f

esblkd.o${BJ}:  ${ESTB_SRC}esblkd.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77  \
           ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESRNCM.F77 ${BASE_COM}ESHAP.F77 \
           ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esblkd.f

escprs.o${BJ}:  ${ESTB_SRC}escprs.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}OUTCOM.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}escprs.f

esdlay.o${BJ}:  ${ESTB_SRC}esdlay.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77  \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esdlay.f

esetpr.o${BJ}:  ${ESTB_SRC}esetpr.f
	${FC} -c ${FFLAGS} ${ESTB_SRC}esetpr.f
 
esfltr.o${BJ}:  ${ESTB_SRC}esfltr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ESPARM.F77  \
           ${BASE_COM}ESTREE.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}PDEN.F77   \
           ${BASE_COM}CALDEN.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esfltr.f

esgent.o${BJ}:  ${ESTB_SRC}esgent.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}ESPARM.F77 ${BASE_COM}ESCOMN.F77 \
           ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esgent.f

esin.o${BJ}:    ${ESTB_SRC}esin.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESHAP.F77      \
           ${BASE_COM}ESHAP2.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esin.f

esinit.o${BJ}:  ${ESTB_SRC}esinit.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77 ${BASE_COM}ESRNCM.F77  \
           ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esinit.f

esmsgs.o${BJ}:  ${ESTB_SRC}esmsgs.f
	${FC} -c ${FFLAGS} ${ESTB_SRC}esmsgs.f
 
esnspe.o${BJ}:  ${ESTB_SRC}esnspe.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esnspe.f

esnutr.o${BJ}:  ${ESTB_SRC}esnutr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esnutr.f

esout.o${BJ}:   ${ESTB_SRC}esout.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esout.f

espadv.o${BJ}:  ${ESTB_SRC}espadv.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}PDEN.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}espadv.f

esplt1.o${BJ}:  ${ESTB_SRC}esplt1.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESHAP.F77    \
           ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esplt1.f

esplt2.o${BJ}:  ${ESTB_SRC}esplt2.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
           ${BASE_COM}PLOT.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esplt2.f

esprep.o${BJ}:  ${ESTB_SRC}esprep.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esprep.f

esprin.o${BJ}:  ${ESTB_SRC}esprin.f
	${FC} -c ${FFLAGS} ${ESTB_SRC}esprin.f

espsub.o${BJ}:  ${ESTB_SRC}espsub.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}PDEN.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}espsub.f

espxcs.o${BJ}:  ${ESTB_SRC}espxcs.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}PDEN.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}espxcs.f

esrann.o${BJ}:  ${ESTB_SRC}esrann.f ${BASE_COM}ESRNCM.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esrann.f

essubh.o${BJ}:  ${ESTB_SRC}essubh.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}essubh.f

estab.o${BJ}:   ${ESTB_SRC}estab.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ESPARM.F77   \
           ${BASE_COM}ESTREE.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77  \
           ${BASE_COM}PDEN.F77 ${BASE_COM}CALDEN.F77 ${BASE_COM}ESCOMN.F77   \
           ${BASE_COM}ESCOM2.F77 ${BASE_COM}VARCOM.F77 ${BASE_COM}STDSTK.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}estab.f

estime.o${BJ}:  ${ESTB_SRC}estime.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}estime.f

estock.o${BJ}:  ${ESTB_SRC}estock.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}estock.f

estpp.o${BJ}:   ${ESTB_SRC}estpp.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77    \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}estpp.f

estump.o${BJ}:  ${ESTB_SRC}estump.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
           ${BASE_COM}ESHOOT.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77  \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}COEFFS.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}estump.f

esuckr.o${BJ}:  ${ESTB_SRC}esuckr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ESPARM.F77   \
           ${BASE_COM}ESHOOT.F77 ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77 ${BASE_COM}ESTREE.F77  \
           ${BASE_COM}STDSTK.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esuckr.f

esxcsh.o${BJ}:  ${ESTB_SRC}esxcsh.f ${BASE_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esxcsh.f
