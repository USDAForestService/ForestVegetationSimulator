# Fortran compiler
include ../../../bin/FC${OS}.make

# the Prognosis common blocks directory
NE_COM = ../../../ne/common/
BASE_COM = ../../../common/
DBS_SRC = ../../src/

FFLAGS = ${I1}${DBS_SRC}${I2}${NE_COM}${I2}${BASE_COM}

DBS_OBJS = \
dbsatrtls.o${BJ} \
dbscase.o${BJ} \
dbscmpu.o${BJ} \
dbsdiags.o${BJ} \
dbsexecsql.o${BJ} \
dbsin.o${BJ} \
dbsprssql.o${BJ} \
dbsstandin.o${BJ} \
dbstrls.o${BJ} \
dbscuts.o${BJ} \
dbsfmmort.o${BJ} \
dbsfmdsnag.o${BJ} \
dbsppget.o${BJ} \
dbsppput.o${BJ} \
dbspusput.o${BJ} \
dbspusget.o${BJ} \
dbshelpers.o${BJ} \
dbsecharv.o${BJ}

dbs: ${DBS_OBJS}

dbsatrtls.o${BJ}: \
${DBS_SRC}dbsatrtls.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}ESTREE.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}WORKCM.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsatrtls.f

dbscase.o${BJ}: \
${DBS_SRC}dbscase.f \
${NE_COM}PRGPRM.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}KEYCOM.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbscase.f

dbscmpu.o${BJ}: \
${DBS_SRC}dbscmpu.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}KEYCOM.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbscmpu.f
dbsdiags.o${BJ}: \
${DBS_SRC}dbsdiags.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsdiags.f

dbsexecsql.o${BJ}: \
${DBS_SRC}dbsexecsql.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsexecsql.f

dbsin.o${BJ}: \
${DBS_SRC}dbsin.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsin.f

dbsprssql.o${BJ}: \
${DBS_SRC}dbsprssql.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}KEYCOM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsprssql.f

dbsstandin.o${BJ}: \
${DBS_SRC}dbsstandin.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}COEFFS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}OUTCOM.F77 \
${BASE_COM}HTCAL.F77 \
${BASE_COM}ECON.F77 \
${BASE_COM}KEYCOM.F77 \
${BASE_COM}MULTCM.F77 \
${BASE_COM}VOLSTD.F77 \
${BASE_COM}SCREEN.F77 \
${BASE_COM}VARCOM.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsstandin.f

dbstrls.o${BJ}: \
${DBS_SRC}dbstrls.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}ESTREE.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}WORKCM.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbstrls.f

dbscuts.o${BJ}: \
${DBS_SRC}dbscuts.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}ESTREE.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}WORKCM.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbscuts.f

dbsfmdsnag.o${BJ}: \
${DBS_SRC}dbsfmdsnag.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmdsnag.f

dbsfmmort.o${BJ}: \
${DBS_SRC}dbsfmmort.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmmort.f	

# PPE routines (compile them even if PPE is not used as there are few of them

dbsppput.o${BJ}: \
${DBS_SRC}dbsppput.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsppput.f

dbsppget.o${BJ}: \
${DBS_SRC}dbsppget.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsppget.f

dbspusput.o${BJ}: \
${DBS_SRC}dbspusput.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbspusput.f

dbspusget.o${BJ}: \
${DBS_SRC}dbspusget.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbspusget.f

# ECON routines

dbsecharv.o${BJ}: \
${DBS_SRC}dbsecharv.f \
${NE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsecharv.f

dbshelpers.o${BJ}: \
${DBS_SRC}dbshelpers.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbshelpers.f
