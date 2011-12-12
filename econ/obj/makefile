# Fortran compiler
include ./../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ./../../common/

# the econ model source directory
ECON_SRC = ./../src/

FFLAGS = ${I1}${ECON_SRC}${I2}${BASE_COM}

# the econ model object files
ECON_OBJS =  eccalc.o${BJ} echarv.o${BJ} ecin.o${BJ} ecinit.o${BJ} \
             ecsetp.o${BJ} ecstatus.o${BJ} ecvol.o${BJ}

econ:  ${ECON_OBJS}

eccalc.o${BJ}: ${ECON_SRC}eccalc.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}eccalc.f

echarv.o${BJ}: ${ECON_SRC}echarv.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}echarv.f

ecin.o${BJ}: ${ECON_SRC}ecin.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecin.f

ecinit.o${BJ}: ${ECON_SRC}ecinit.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecinit.f

ecsetp.o${BJ}: ${ECON_SRC}ecsetp.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecsetp.f

ecstatus.o${BJ}: ${ECON_SRC}ecstatus.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecstatus.f

ecvol.o${BJ}: ${ECON_SRC}ecvol.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecvol.f
