# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../common/
NC_COM   = ../../nc/common/
EC_COM   = ../../ec/common/

# the dwarf mistletoe interim model source directory
MIST_SRC = ../../mistoe/src/


# the PPE base source directory
PPBASE_SRC   = ../../ppbase/src/

# the PPEC source directory
PPEC_SRC   = ../../ppec/src/

# options for the compiler
FFLAGS = ${I1}${NC_COM}${I2}${BASE_COM}${I2}${MIST_SRC}

# the PPE NC (Klammath Mountains) object files
PPNC_OBJS = getstd.o${BJ} putstd.o${BJ} \
            ppdgf.o${BJ}  ppregt.o${BJ} varget.o${BJ} varput.o${BJ}

ppnc:    ${PPNC_OBJS}

# compile routines from PPBASE source directory

getstd.o${BJ}: ${PPBASE_SRC}getstd.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPDNCM.F77 ${NC_COM}ESPARM.F77  \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}CALCOM.F77 ${BASE_COM}CALDEN.F77 ${BASE_COM}ECON.F77    \
          ${BASE_COM}ESHAP.F77  ${BASE_COM}ESHAP2.F77 ${BASE_COM}ESCOMN.F77   \
          ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESTCOR.F77 ${BASE_COM}ESTREE.F77  \
          ${BASE_COM}HTCAL.F77  ${BASE_COM}MULTCM.F77 ${BASE_COM}OPCOM.F77  \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PDEN.F77   ${BASE_COM}PLOT.F77   \
          ${BASE_COM}VOLSTD.F77 ${BASE_COM}RANCOM.F77 ${BASE_COM}ESRNCM.F77  \
          ${BASE_COM}DBSTK.F77  ${BASE_COM}VARCOM.F77 ${BASE_COM}SUMTAB.F77 \
          ${BASE_COM}CWDCOM.F77 ${BASE_COM}FVSSTDCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}getstd.f

putstd.o${BJ}:  ${PPBASE_SRC}putstd.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPDNCM.F77 ${NC_COM}ESPARM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}CALCOM.F77 ${BASE_COM}CALDEN.F77 ${BASE_COM}ECON.F77   \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77 ${BASE_COM}ESCOMN.F77  \
           ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESTCOR.F77 ${BASE_COM}ESTREE.F77 \
           ${BASE_COM}HTCAL.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}OPCOM.F77   \
           ${BASE_COM}OUTCOM.F77 ${BASE_COM}PDEN.F77 ${BASE_COM}PLOT.F77     \
           ${BASE_COM}VOLSTD.F77 ${BASE_COM}ESRNCM.F77 ${BASE_COM}RANCOM.F77 \
           ${BASE_COM}DBSTK.F77 ${BASE_COM}VARCOM.F77 ${BASE_COM}SUMTAB.F77 \
           ${BASE_COM}CWDCOM.F77 ${BASE_COM}FVSSTDCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}putstd.f

# compile routines from PPEC source directory

ppdgf.o${BJ}:  ${PPEC_SRC}ppdgf.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}ppdgf.f

ppregt.o${BJ}: ${PPEC_SRC}ppregt.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}ppregt.f

varget.o${BJ}: ${PPEC_SRC}varget.f ${BASE_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}varget.f

varput.o${BJ}: ${PPEC_SRC}varput.f ${BASE_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}varput.f

