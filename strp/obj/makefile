# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../common/

# the STRP model source directory
STRP_SRC = ./../src/

# options for the compiler
FFLAGS = ${I1}${BASE_COM}

# the STRP model object files
STRP_OBJS = esaddt.o${BJ} esblkd.o${BJ} escprs.o${BJ} esetpr.o${BJ} esfltr.o${BJ} esgent.o${BJ} esin.o${BJ} esinit.o${BJ}    \
            esmsgs.o${BJ} esnutr.o${BJ} esout.o${BJ} esplt1.o${BJ} esplt2.o${BJ} esprep.o${BJ} esprin.o${BJ}   \
            esrann.o${BJ} essubh.o${BJ} estab.o${BJ} estime.o${BJ} estump.o${BJ} esuckr.o${BJ}

strp:     ${STRP_OBJS}

esaddt.o${BJ}:  ${STRP_SRC}esaddt.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esaddt.f

esblkd.o${BJ}: ${STRP_SRC}esblkd.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESRNCM.F77   \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esblkd.f

escprs.o${BJ}: ${STRP_SRC}escprs.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}ESHAP.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}OUTCOM.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}escprs.f

esetpr.o${BJ}: ${STRP_SRC}esetpr.f
	${FC} -c ${FFLAGS} ${STRP_SRC}esetpr.f

esfltr.o${BJ}: ${STRP_SRC}esfltr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ESPARM.F77   \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}PDEN.F77    \
          ${BASE_COM}CALDEN.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esfltr.f

esgent.o${BJ}: ${STRP_SRC}esgent.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ESPARM.F77 ${BASE_COM}ESCOMN.F77 \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esgent.f

esin.o${BJ}: ${STRP_SRC}esin.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77 \
        $(BASE_COM)ESCOMN.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}CONTRL.F77 \
        ${BASE_COM}PLOT.F77 ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esin.f

esinit.o${BJ}: ${STRP_SRC}esinit.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
          ${BASE_COM}ESHAP.F77 ${BASE_COM}ESRNCM.F77 ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esinit.f

esmsgs.o${BJ}: ${STRP_SRC}esmsgs.f
	${FC} -c ${FFLAGS} ${STRP_SRC}esmsgs.f
 
esnutr.o${BJ}: ${STRP_SRC}esnutr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esnutr.f

esout.o${BJ}:  ${STRP_SRC}esout.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esout.f

esplt1.o${BJ}: ${STRP_SRC}esplt1.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esplt1.f

esplt2.o${BJ}: ${STRP_SRC}esplt2.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esplt2.f

esprep.o${BJ}: ${STRP_SRC}esprep.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esprep.f

esprin.o${BJ}: ${STRP_SRC}esprin.f 
	${FC} -c ${FFLAGS} ${STRP_SRC}esprin.f

esrann.o${BJ}: ${STRP_SRC}esrann.f ${BASE_COM}ESRNCM.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esrann.f

essubh.o${BJ}: ${STRP_SRC}essubh.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
          ${BASE_COM}ESCOMN.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}essubh.f

estab.o${BJ}:  ${STRP_SRC}estab.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ESPARM.F77   \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}PDEN.F77    \
          ${BASE_COM}CALDEN.F77 ${BASE_COM}ESCOMN.F77 ${BASE_COM}VARCOM.F77 \
          ${BASE_COM}STDSTK.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}estab.f

estime.o${BJ}: ${STRP_SRC}estime.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}estime.f

estump.o${BJ}: ${STRP_SRC}estump.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77   \
          ${BASE_COM}ESHOOT.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESCOMN.F77  \
          ${BASE_COM}COEFFS.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}estump.f

esuckr.o${BJ}: ${STRP_SRC}esuckr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ESPARM.F77   \
          ${BASE_COM}ESHOOT.F77 ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESHAP.F77  \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}STDSTK.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esuckr.f
