# Fortran compiler
include ../../bin/FC${OS}.make

# the FVS common blocks directory
BASE_COM = ./../../common/

# the Douglas-fir beetle source directory    
DFB_SRC  = ./../src/
DFB_COM  = ./../common/

FFLAGS = ${I1}${DFB_COM}${I2}${BASE_COM}

# the Douglas-fir beetle object files
DFB_OBJS = dfbdam.o${BJ} dfbdbh.o${BJ} dfbdrv.o${BJ} dfber.o${BJ} dfbgo.o${BJ} dfbhed.o${BJ} dfbin.o${BJ}   \
           dfbincr.o${BJ} dfbind.o${BJ} dfbint.o${BJ} dfbinv.o${BJ} dfblkd.o${BJ} dfblkdbm.o${BJ}      \
           dfblkdcr.o${BJ} dfblkdec.o${BJ} dfblkdtt.o${BJ} dfblkdut.o${BJ}        \
           dfbmod.o${BJ} dfbmrt.o${BJ} dfbout.o${BJ} dfbprb.o${BJ} dfbran.o${BJ} dfbsch.o${BJ}         \
           dfbtab.o${BJ} dfbwin.o${BJ}

dfb:      ${DFB_OBJS}

dfbdam.o${BJ}:   ${DFB_SRC}dfbdam.f ${BASE_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77 
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbdam.f

dfbdbh.o${BJ}:   ${DFB_SRC}dfbdbh.f ${BASE_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77   \
            ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbdbh.f

dfbdrv.o${BJ}:   ${DFB_SRC}dfbdrv.f ${BASE_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbdrv.f

dfber.o${BJ}:    ${DFB_SRC}dfber.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
            ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfber.f

dfbgo.o${BJ}:    ${DFB_SRC}dfbgo.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
            ${BASE_COM}CONTRL.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbgo.f

dfbhed.o${BJ}:   ${DFB_SRC}dfbhed.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77    \
            ${DFB_COM}DFBCOM.F77 
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbhed.f

dfbin.o${BJ}:    ${DFB_SRC}dfbin.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
            ${DFB_COM}DFBCOM.F77 
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbin.f

dfbincr.o${BJ}:  ${DFB_SRC}dfbincr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
            ${BASE_COM}PLOT.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbincr.f

dfbind.o${BJ}:   ${DFB_SRC}dfbind.f
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbind.f

dfbint.o${BJ}:   ${DFB_SRC}dfbint.f ${BASE_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbint.f

dfbinv.o${BJ}:   ${DFB_SRC}dfbinv.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbinv.f

dfblkd.o${BJ}: ${DFB_SRC}dfblkd.f ${BASE_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfblkd.f

dfblkdbm.o${BJ}: ${DFB_SRC}dfblkdbm.f ${BASE_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77 
	${FC} -c ${FFLAGS} ${DFB_SRC}dfblkdbm.f

dfblkdcr.o${BJ}: ${DFB_SRC}dfblkdcr.f ${BASE_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77 
	${FC} -c ${FFLAGS} ${DFB_SRC}dfblkdcr.f

dfblkdec.o${BJ}: ${DFB_SRC}dfblkdec.f ${BASE_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77 
	${FC} -c ${FFLAGS} ${DFB_SRC}dfblkdec.f

dfblkdtt.o${BJ}: ${DFB_SRC}dfblkdtt.f ${BASE_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfblkdtt.f

dfblkdut.o${BJ}: ${DFB_SRC}dfblkdut.f ${BASE_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfblkdut.f

dfbmod.o${BJ}:   ${DFB_SRC}dfbmod.f ${BASE_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77    \
            ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbmod.f

dfbmrt.o${BJ}:   ${DFB_SRC}dfbmrt.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
            ${BASE_COM}CONTRL.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbmrt.f

dfbout.o${BJ}:   ${DFB_SRC}dfbout.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbout.f

dfbprb.o${BJ}:   ${DFB_SRC}dfbprb.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbprb.f

dfbran.o${BJ}:   ${DFB_SRC}dfbran.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbran.f

dfbsch.o${BJ}:   ${DFB_SRC}dfbsch.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbsch.f

dfbtab.o${BJ}:   ${DFB_SRC}dfbtab.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
            ${BASE_COM}CONTRL.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbtab.f

dfbwin.o${BJ}:   ${DFB_SRC}dfbwin.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
            ${BASE_COM}CONTRL.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbwin.f

