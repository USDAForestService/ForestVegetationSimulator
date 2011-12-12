# Fortran compiler
include ../../../../bin/FC${OS}.make

# the Prognosis common blocks directory
CR_COM = ../../../../cr/common/
BASE_COM = ../../../../common/
DBS_SRC = ../../../../dbs/src/
DBSM_SRC = ../../src/
MIST_SRC = ../../../../mistoe/src/

FFLAGS = ${I1}${DBS_SRC}${I2}${CR_COM}${I2}${BASE_COM}${I2}${MIST_SRC}

DBS_OBJS = \
dbsatrtls.o${BJ} \
dbscuts.o${BJ} \
dbsfmfuel.o${BJ} \
dbsfuels.o${BJ} \
dbsmis.o${BJ} \
dbsstandin.o${BJ} \
dbssumry.o${BJ} \
dbstrls.o${BJ} \
dbscase.o${BJ} \
dbscmpu.o${BJ} \
dbsdiags.o${BJ} \
dbsexecsql.o${BJ} \
dbsin.o${BJ} \
dbsprssql.o${BJ} \
dbsstandin.o${BJ} \
dbsfmmort.o${BJ} \
dbsfmdsnag.o${BJ} \
dbsppget.o${BJ} \
dbsppput.o${BJ} \
dbspusput.o${BJ} \
dbspusget.o${BJ} \
dbshelpers.o${BJ} \
dbsecharv.o${BJ}

dbs: ${DBS_OBJS}

# Metric versions of DBS routines

dbsatrtls.o${BJ}: \
${DBSM_SRC}dbsatrtls.f \
${CR_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}ESTREE.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}WORKCM.F77 \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBSM_SRC}dbsatrtls.f

dbscuts.o${BJ}: \
${DBSM_SRC}dbscuts.f \
${CR_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}ESTREE.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}WORKCM.F77 \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBSM_SRC}dbscuts.f

dbsfmfuel.o${BJ}: \
${DBSM_SRC}dbsfmfuel.f \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBSM_SRC}dbsfmfuel.f

dbsfuels.o${BJ}: \
${DBSM_SRC}dbsfuels.f \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBSM_SRC}dbsfuels.f

dbsmis.o${BJ}: \
${DBSM_SRC}dbsmis.f \
${DBS_SRC}DBSCOM.F77 \
${CR_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77   \
${BASE_COM}ESTREE.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}WORKCM.F77 \
${MIST_SRC}MISCOM.F77 \
${MIST_SRC}DMCOM.F77  \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${DBSM_SRC}dbsmis.f

dbsstandin.o${BJ}: \
${DBSM_SRC}dbsstandin.f \
${CR_COM}PRGPRM.F77 \
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

	${FC} -c ${FFLAGS} ${DBSM_SRC}dbsstandin.f

dbssumry.o${BJ}: \
${DBSM_SRC}dbssumry.f \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBSM_SRC}dbssumry.f

dbstrls.o${BJ}: \
${DBSM_SRC}dbstrls.f \
${CR_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}ESTREE.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}WORKCM.F77 \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBSM_SRC}dbstrls.f

#  base DBS routines

dbscase.o${BJ}: \
${DBS_SRC}dbscase.f \
${CR_COM}PRGPRM.F77 \
${DBS_SRC}DBSCOM.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}KEYCOM.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbscase.f

dbscmpu.o${BJ}: \
${DBS_SRC}dbscmpu.f \
${CR_COM}PRGPRM.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}KEYCOM.F77 \
${DBS_SRC}DBSCOM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbscmpu.f

dbsdiags.o${BJ}: \
${DBS_SRC}dbsdiags.f \
${CR_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsdiags.f

dbsexecsql.o${BJ}: \
${DBS_SRC}dbsexecsql.f \
${CR_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsexecsql.f

dbsin.o${BJ}: \
${DBS_SRC}dbsin.f \
${CR_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsin.f

dbsprssql.o${BJ}: \
${DBS_SRC}dbsprssql.f \
${CR_COM}PRGPRM.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}KEYCOM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsprssql.f


dbsfmdsnag.o${BJ}: \
${DBS_SRC}dbsfmdsnag.f \
${CR_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmdsnag.f

dbsfmmort.o${BJ}: \
${DBS_SRC}dbsfmmort.f \
${CR_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmmort.f	

# PPE routines (compile them even if PPE is not used as there are few of them

dbsppput.o${BJ}: \
${DBS_SRC}dbsppput.f \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsppput.f

dbsppget.o${BJ}: \
${DBS_SRC}dbsppget.f \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsppget.f

dbspusput.o${BJ}: \
${DBS_SRC}dbspusput.f \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbspusput.f

dbspusget.o${BJ}: \
${DBS_SRC}dbspusget.f \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbspusget.f

# ECON routines

dbsecharv.o${BJ}: \
${DBS_SRC}dbsecharv.f \
${CR_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsecharv.f

dbshelpers.o${BJ}: \
${DBS_SRC}dbshelpers.f \
${DBS_SRC}DBSCOM.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbshelpers.f
