# Fortran compiler
include ../../bin/FC${OS}.make

# the FVS common blocks directory
BASE_COM = ./../../common/

# the Douglas-fir Tussock Moth source directory    
DFTM_SRC  = ./../src/

FFLAGS = ${I1}${DFTM_SRC}${I2}${BASE_COM}

# the Douglas-fir Tussock Moth object files
DFTM_OBJS = dfole8.o${BJ} dftmgo.o${BJ} dftmin.o${BJ} dftmod.o${BJ} g0comp.o${BJ} garbel.o${BJ} gfcomp.o${BJ}   \
            grclas.o${BJ} grpsum.o${BJ} inscyc.o${BJ} redist.o${BJ} tmbchl.o${BJ} tmbmas.o${BJ} tmcoup.o${BJ}   \
            tmdam.o${BJ} tmdtls.o${BJ} tmhed.o${BJ} tminit.o${BJ} tminitec.o${BJ} tminittt.o${BJ} tmotpr.o${BJ} \
            tmout.o${BJ} tmrann.o${BJ} tmschd.o${BJ} uv1.o${BJ} uv2.o${BJ} y0comp.o${BJ} y1comp.o${BJ} z1comp.o${BJ}


dftm:     ${DFTM_OBJS}

dfole8.o${BJ}: ${DFTM_SRC}dfole8.f ${DFTM_SRC}LIMITS.F77 ${DFTM_SRC}DFOL.F77     \
          ${DFTM_SRC}ICOND.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}dfole8.f

dftmgo.o${BJ}: ${DFTM_SRC}dftmgo.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${DFTM_SRC}TMCOM1.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}dftmgo.f

dftmin.o${BJ}: ${DFTM_SRC}dftmin.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${DFTM_SRC}TMEGGS.F77 ${DFTM_SRC}TMCOM1.F77 ${DFTM_SRC}UPPER.F77  \
          ${DFTM_SRC}LOWER.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}dftmin.f

dftmod.o${BJ}: ${DFTM_SRC}dftmod.f ${DFTM_SRC}TMCOM1.F77 ${DFTM_SRC}UPPER.F77    \
          ${DFTM_SRC}LOWER.F77 ${DFTM_SRC}LIMITS.F77 ${DFTM_SRC}DFOL.F77    \
          ${DFTM_SRC}ICOND.F77 ${DFTM_SRC}GPASS.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}dftmod.f

g0comp.o${BJ}: ${DFTM_SRC}g0comp.f ${DFTM_SRC}ICOND.F77 ${DFTM_SRC}GPASS.F77     \
          ${DFTM_SRC}UPPER.F77 ${DFTM_SRC}LIMITS.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}g0comp.f

garbel.o${BJ}: ${DFTM_SRC}garbel.f 
	${FC} -c ${FFLAGS} ${DFTM_SRC}garbel.f

gfcomp.o${BJ}: ${DFTM_SRC}gfcomp.f ${DFTM_SRC}LOWER.F77 ${DFTM_SRC}LIMITS.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}gfcomp.f

grclas.o${BJ}: ${DFTM_SRC}grclas.f 
	${FC} -c ${FFLAGS} ${DFTM_SRC}grclas.f

grpsum.o${BJ}: ${DFTM_SRC}grpsum.f 
	${FC} -c ${FFLAGS} ${DFTM_SRC}grpsum.f

inscyc.o${BJ}: ${DFTM_SRC}inscyc.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}inscyc.f

redist.o${BJ}: ${DFTM_SRC}redist.f ${DFTM_SRC}ICOND.F77 ${DFTM_SRC}GPASS.F77     \
          ${DFTM_SRC}UPPER.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}redist.f

tmbchl.o${BJ}: ${DFTM_SRC}tmbchl.f 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmbchl.f

tmbmas.o${BJ}: ${DFTM_SRC}tmbmas.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${DFTM_SRC}TMCOM1.F77 ${BASE_COM}PLOT.F77   \
          ${DFTM_SRC}BIOMAS.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmbmas.f

tmcoup.o${BJ}: ${DFTM_SRC}tmcoup.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${DFTM_SRC}TMCOM1.F77   \
          ${DFTM_SRC}BIOMAS.F77 ${DFTM_SRC}TMEGGS.F77 ${DFTM_SRC}UPPER.F77  \
          ${DFTM_SRC}LOWER.F77 ${DFTM_SRC}DFOL.F77 ${DFTM_SRC}ICOND.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmcoup.f

tmdam.o${BJ}: ${DFTM_SRC}tmdam.f 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmdam.f

tmdtls.o${BJ}: ${DFTM_SRC}tmdtls.f 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmdtls.f

tmhed.o${BJ}: ${DFTM_SRC}tmhed.f 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmhed.f

tminit.o${BJ}: ${DFTM_SRC}tminit.f ${DFTM_SRC}TMCOM1.F77 ${DFTM_SRC}TMEGGS.F77   \
          ${DFTM_SRC}UPPER.F77 ${DFTM_SRC}LOWER.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tminit.f

tminitec.o${BJ}: ${DFTM_SRC}tminitec.f ${DFTM_SRC}TMCOM1.F77 ${DFTM_SRC}TMEGGS.F77 \
            ${DFTM_SRC}UPPER.F77 ${DFTM_SRC}LOWER.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tminitec.f

tminittt.o${BJ}: ${DFTM_SRC}tminittt.f ${DFTM_SRC}TMCOM1.F77 ${DFTM_SRC}TMEGGS.F77 \
            ${DFTM_SRC}UPPER.F77 ${DFTM_SRC}LOWER.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tminittt.f

tmotpr.o${BJ}: ${DFTM_SRC}tmotpr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${DFTM_SRC}TMCOM1.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmotpr.f

tmout.o${BJ}: ${DFTM_SRC}tmout.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77      \
         ${BASE_COM}PLOT.F77 ${DFTM_SRC}TMCOM1.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmout.f

tmrann.o${BJ}: ${DFTM_SRC}tmrann.f 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmrann.f

tmschd.o${BJ}: ${DFTM_SRC}tmschd.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${DFTM_SRC}TMCOM1.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmschd.f

uv1.o${BJ}: ${DFTM_SRC}uv1.f 
	${FC} -c ${FFLAGS} ${DFTM_SRC}uv1.f

uv2.o${BJ}: ${DFTM_SRC}uv2.f 
	${FC} -c ${FFLAGS} ${DFTM_SRC}uv2.f

y0comp.o${BJ}: ${DFTM_SRC}y0comp.f ${DFTM_SRC}ICOND.F77 ${DFTM_SRC}GPASS.F77     \
          ${DFTM_SRC}UPPER.F77 ${DFTM_SRC}LIMITS.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}y0comp.f

y1comp.o${BJ}: ${DFTM_SRC}y1comp.f ${DFTM_SRC}ICOND.F77 ${DFTM_SRC}LOWER.F77     \
          ${DFTM_SRC}LIMITS.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}y1comp.f

z1comp.o${BJ}: ${DFTM_SRC}z1comp.f ${DFTM_SRC}ICOND.F77 ${DFTM_SRC}GPASS.F77     \
          ${DFTM_SRC}UPPER.F77 ${DFTM_SRC}LOWER.F77 ${DFTM_SRC}LIMITS.F77 
	${FC} -c ${FFLAGS} ${DFTM_SRC}z1comp.f

