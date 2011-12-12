# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../common/
DBS_SRC = ../src/
MIST_SRC = ../../mistoe/src/

FFLAGS = ${I1}${DBS_SRC}${I2}${BASE_COM}${I2}${BASE_COM}

DBS_OBJS = \
../../bin/libfvsSQL.so \
dbsatrtls.o${BJ} \
dbsblkd.o${BJ} \
dbsbmbkp.o${BJ} \
dbsbmmain.o${BJ} \
dbsbmtree.o${BJ} \
dbsbmvol.o${BJ} \
dbscase.o${BJ} \
dbsclose.o${BJ} \
dbscmpu.o${BJ} \
dbsdiags.o${BJ} \
dbsevm.o${BJ} \
dbsexecsql.o${BJ} \
dbsfmpf.o${BJ} \
dbsfmmort.o${BJ} \
dbsfmburn.o${BJ} \
dbsfmcrpt.o${BJ} \
dbsfmdwvol.o${BJ} \
dbsfmdwcov.o${BJ} \
dbsfmfuel.o${BJ} \
dbsfmhrpt.o${BJ} \
dbsfmssnag.o${BJ} \
dbsfmdsnag.o${BJ} \
dbsfmcanpr.o${BJ} \
dbsfmlink.o${BJ} \
dbsfuels.o${BJ} \
dbsgetid.o${BJ} \
dbsin.o${BJ} \
dbsinit.o${BJ} \
dbsmis.o${BJ} \
dbsopen.o${BJ} \
dbsprs.o${BJ} \
dbsprssql.o${BJ} \
dbsstandin.o${BJ} \
dbssumry.o${BJ} \
dbstreesin.o${BJ} \
dbstrls.o${BJ} \
dbscuts.o${BJ} \
dbsppget.o${BJ} \
dbsppput.o${BJ} \
dbspusput.o${BJ} \
dbspusget.o${BJ} \
dbsstrclass.o${BJ} \
dbshelpers.o${BJ}  \
dbsecharv.o${BJ} \
dbsecsum.o${BJ}

dbs: ${DBS_OBJS}

../../bin/libfvsSQL.so: \
${DBS_SRC}fvsSQL.c \

		$(CC) $< -shared ${PIC} -l ${ODBC} -o ../../bin/libfvsSQL.so 


dbsatrtls.o${BJ}: \
${DBS_SRC}dbsatrtls.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}ESTREE.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}WORKCM.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsatrtls.f

dbsblkd.o${BJ}: \
${DBS_SRC}dbsblkd.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsblkd.f

dbsbmmain.o${BJ}: \
${DBS_SRC}dbsbmmain.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsbmmain.f

dbsbmtree.o${BJ}: \
${DBS_SRC}dbsbmtree.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsbmtree.f

dbsbmbkp.o${BJ}: \
${DBS_SRC}dbsbmbkp.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsbmbkp.f

dbsbmvol.o${BJ}: \
${DBS_SRC}dbsbmvol.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsbmvol.f

dbscase.o${BJ}: \
${DBS_SRC}dbscase.f \
${BASE_COM}PRGPRM.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}KEYCOM.F77 \
${BASE_COM}PLOT.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbscase.f

dbsclose.o${BJ}: \
${DBS_SRC}dbsclose.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsclose.f

dbscmpu.o${BJ}: \
${DBS_SRC}dbscmpu.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}KEYCOM.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbscmpu.f

dbsdiags.o${BJ}: \
${DBS_SRC}dbsdiags.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsdiags.f

dbsevm.o${BJ}: \
${DBS_SRC}dbsevm.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsevm.f

dbsexecsql.o${BJ}: \
${DBS_SRC}dbsexecsql.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsexecsql.f


dbsgetid.o${BJ}: \
${DBS_SRC}dbsgetid.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsgetid.f

dbsin.o${BJ}: \
${DBS_SRC}dbsin.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsin.f

dbsinit.o${BJ}: \
${DBS_SRC}dbsinit.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsinit.f

dbsmis.o${BJ}: \
${DBS_SRC}dbsmis.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77   \
${BASE_COM}ESTREE.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}WORKCM.F77 \
${MIST_SRC}MISCOM.F77 \
${MIST_SRC}DMCOM.F77  \
${BASE_COM}METRIC.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsmis.f

dbsopen.o${BJ}: \
${DBS_SRC}dbsopen.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsopen.f

dbsprs.o${BJ}: \
${DBS_SRC}dbsprs.f \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsprs.f

dbsprssql.o${BJ}: \
${DBS_SRC}dbsprssql.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}OPCOM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}KEYCOM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsprssql.f

dbsstandin.o${BJ}: \
${DBS_SRC}dbsstandin.f \
${BASE_COM}PRGPRM.F77 \
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

dbssumry.o${BJ}: \
${DBS_SRC}dbssumry.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbssumry.f

dbstreesin.o${BJ}: \
${DBS_SRC}dbstreesin.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbstreesin.f

dbstrls.o${BJ}: \
${DBS_SRC}dbstrls.f \
${BASE_COM}PRGPRM.F77 \
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
${BASE_COM}PRGPRM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}ESTREE.F77 \
${BASE_COM}VARCOM.F77 \
${BASE_COM}WORKCM.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbscuts.f

dbsstrclass.o${BJ}: \
${DBS_SRC}dbsstrclass.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsstrclass.f

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

# FFE routines

dbsfmpf.o${BJ}: \
${DBS_SRC}dbsfmpf.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmpf.f

dbsfmburn.o${BJ}: \
${DBS_SRC}dbsfmburn.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmburn.f

dbsfmcrpt.o${BJ}: \
${DBS_SRC}dbsfmcrpt.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmcrpt.f

dbsfmdwvol.o${BJ}: \
${DBS_SRC}dbsfmdwvol.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmdwvol.f

dbsfmdwcov.o${BJ}: \
${DBS_SRC}dbsfmdwcov.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmdwcov.f

dbsfmfuel.o${BJ}: \
${DBS_SRC}dbsfmfuel.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmfuel.f

dbsfmhrpt.o${BJ}: \
${DBS_SRC}dbsfmhrpt.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmhrpt.f

dbsfmssnag.o${BJ}: \
${DBS_SRC}dbsfmssnag.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmssnag.f

dbsfmdsnag.o${BJ}: \
${DBS_SRC}dbsfmdsnag.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmdsnag.f

dbsfmcanpr.o${BJ}: \
${DBS_SRC}dbsfmcanpr.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmcanpr.f

dbsfmlink.o${BJ}: \
${DBS_SRC}dbsfmlink.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmlink.f

dbsfmmort.o${BJ}: \
${DBS_SRC}dbsfmmort.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfmmort.f

dbsfuels.o${BJ}: \
${DBS_SRC}dbsfuels.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsfuels.f

# ECON routines

dbsecharv.o${BJ}: \
${DBS_SRC}dbsecharv.f \
${BASE_COM}PRGPRM.F77 \
${BASE_COM}PLOT.F77 \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsecharv.f

dbsecsum.o${BJ}: \
${DBS_SRC}dbsecsum.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbsecsum.f

dbshelpers.o${BJ}: \
${DBS_SRC}dbshelpers.f \
${DBS_SRC}DBSCOM.F77 \
${DBS_SRC}DBSTYPEDEFS.F77 \

	${FC} -c ${FFLAGS} ${DBS_SRC}dbshelpers.f
