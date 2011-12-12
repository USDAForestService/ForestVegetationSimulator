# Fortran compiler
include ../../bin/FC${OS}.make

# the FVS common blocks directory
BASE_COM = ../../common/

# the Western spruce budworm General Defoliator source directory
BW_SRC   = ../src/

# the dwarf mistletoe interim model source directory
MIST_SRC = ../../mistoe/src/

# options for the compiler
FFLAGS = ${I1}${BW_SRC}${I2}${BASE_COM}

WSBWE_OBJS = bweadv.o${BJ} bweage.o${BJ} bwebet.o${BJ} bwebk.o${BJ} bwebkbm.o${BJ} \
             bwebkec.o${BJ} bwebkem.o${BJ} bwebkso.o${BJ} bwebktt.o${BJ} bwebms.o${BJ} \
             bwebmsbm.o${BJ} bwebmsec.o${BJ} bwebmsso.o${BJ} bwecrc.o${BJ} bwecup.o${BJ} \
             bwedam.o${BJ} bwedef.o${BJ} bwedie.o${BJ} bwedr.o${BJ} bweerr.o${BJ} \
             bweest.o${BJ} bwefdk.o${BJ} bwego.o${BJ} bwein.o${BJ} bweint.o${BJ} \
             bwekdp.o${BJ} bwekey.o${BJ} bwekrd.o${BJ} bwelit.o${BJ} bweob.o${BJ} \
             bweout.o${BJ} bwep1.o${BJ} bwep2.o${BJ} bwep3.o${BJ} bwep4.o${BJ} \
             bwepdm.o${BJ} bweprb.o${BJ} bweran.o${BJ} bwernp.o${BJ} bwesin.o${BJ} \
             bwesit.o${BJ} bweslp.o${BJ} bweuca.o${BJ} bweuky.o${BJ} bwewea.o${BJ} \
             fxnote.o${BJ} txnote.o${BJ} bweppatv.o${BJ} bweppgt.o${BJ} bwepppt.o${BJ}

# dependencies for these routines have not been generated.
#

gendefol : ${WSBWE_OBJS}


bweadv.o${BJ}: ${BW_SRC}bweadv.f ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bweadv.f
  
bweage.o${BJ}: ${BW_SRC}bweage.f  ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bweage.f 

bwebet.o${BJ}: ${BW_SRC}bwebet.f
	${FC} -c ${FFLAGS} ${BW_SRC}bwebet.f 

bwebk.o${BJ}: ${BW_SRC}bwebk.f ${BASE_COM}PRGPRM.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWEBOX.F77 ${BW_SRC}BWECM2.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwebk.f

bwebkbm.o${BJ}: ${BW_SRC}bwebkbm.f  ${BASE_COM}PRGPRM.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWEBOX.F77 ${BW_SRC}BWECM2.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwebkbm.f

bwebkec.o${BJ}: ${BW_SRC}bwebkec.f  ${BASE_COM}PRGPRM.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWEBOX.F77 ${BW_SRC}BWECM2.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwebkec.f

bwebkem.o${BJ}: ${BW_SRC}bwebkem.f  ${BASE_COM}PRGPRM.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWEBOX.F77 ${BW_SRC}BWECM2.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwebkem.f

bwebkso.o${BJ}: ${BW_SRC}bwebkso.f  ${BASE_COM}PRGPRM.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWEBOX.F77 ${BW_SRC}BWECM2.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwebkso.f

bwebktt.o${BJ}: ${BW_SRC}bwebktt.f  ${BASE_COM}PRGPRM.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWEBOX.F77 ${BW_SRC}BWECM2.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwebktt.f

bwebms.o${BJ}: ${BW_SRC}bwebms.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
               ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwebms.f

bwebmsbm.o${BJ}: ${BW_SRC}bwebmsbm.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
               ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwebmsbm.f

bwebmsec.o${BJ}: ${BW_SRC}bwebmsec.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
               ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwebmsec.f

bwebmsso.o${BJ}: ${BW_SRC}bwebmsso.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
               ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwebmsso.f

bwecrc.o${BJ}: ${BW_SRC}bwecrc.f
	${FC} -c ${FFLAGS} ${BW_SRC}bwecrc.f

bwecup.o${BJ}: ${BW_SRC}bwecup.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
               ${BASE_COM}CONTRL.F77 ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwecup.f

bwedam.o${BJ}: ${BW_SRC}bwedam.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 
	${FC} -c ${FFLAGS} ${BW_SRC}bwedam.f

bwedef.o${BJ}: ${BW_SRC}bwedef.f ${BASE_COM}PRGPRM.F77 ${BW_SRC}BWESTD.F77 \
               ${BW_SRC}BWECOM.F77 
	${FC} -c ${FFLAGS} ${BW_SRC}bwedef.f

bwedie.o${BJ}: ${BW_SRC}bwedie.f ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwedie.f

bwedr.o${BJ}: ${BW_SRC}bwedr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
              ${BASE_COM}OPCOM.F77 ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
              ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwedr.f

bweerr.o${BJ}: ${BW_SRC}bweerr.f
	${FC} -c ${FFLAGS} ${BW_SRC}bweerr.f

bweest.o${BJ}: ${BW_SRC}bweest.f ${BASE_COM}ESWSBW.F77 ${BW_SRC}BWESTD.F77 \
               ${BW_SRC}BWECOM.F77 
	${FC} -c ${FFLAGS} ${BW_SRC}bweest.f

bwefdk.o${BJ}: ${BW_SRC}bwefdk.f
	${FC} -c ${FFLAGS} ${BW_SRC}bwefdk.f

bwego.o${BJ}: ${BW_SRC}bwego.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
              ${BASE_COM}PLOT.F77 ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
              ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwego.f

bwein.o${BJ}: ${BW_SRC}bwein.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
              ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
              ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwein.f

bweint.o${BJ}: ${BW_SRC}bweint.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bweint.f

bwekdp.o${BJ}: ${BW_SRC}bwekdp.f
	${FC} -c ${FFLAGS} ${BW_SRC}bwekdp.f

bwekey.o${BJ}: ${BW_SRC}bwekey.f ${BW_SRC}BWECOM.F77 
	${FC} -c ${FFLAGS} ${BW_SRC}bwekey.f

bwekrd.o${BJ}: ${BW_SRC}bwekrd.f
	${FC} -c ${FFLAGS} ${BW_SRC}bwekrd.f

bwelit.o${BJ}: ${BW_SRC}bwelit.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwelit.f

bweob.o${BJ}: ${BW_SRC}bweob.f ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bweob.f

bweout.o${BJ}: ${BW_SRC}bweout.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bweout.f

bwep1.o${BJ}: ${BW_SRC}bwep1.f ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwep1.f

bwep2.o${BJ}: ${BW_SRC}bwep2.f ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwep2.f

bwep3.o${BJ}: ${BW_SRC}bwep3.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
              ${BW_SRC}BWECOM.F77 ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwep3.f

bwep4.o${BJ}: ${BW_SRC}bwep4.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
               ${BASE_COM}CONTRL.F77 ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwep4.f

bwepdm.o${BJ}: ${BW_SRC}bwepdm.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
               ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 \
               ${BW_SRC}BWECOM.F77 ${BW_SRC}BWESTD.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS}${I2}${MIST_SRC} ${BW_SRC}bwepdm.f

bweprb.o${BJ}: ${BW_SRC}bweprb.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
               ${BASE_COM}CONTRL.F77 ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 
	${FC} -c ${FFLAGS} ${BW_SRC}bweprb.f

bweran.o${BJ}: ${BW_SRC}bweran.f
	${FC} -c ${FFLAGS} ${BW_SRC}bweran.f

bwernp.o${BJ}: ${BW_SRC}bwernp.f
	${FC} -c ${FFLAGS} ${BW_SRC}bwernp.f

bwesin.o${BJ}: ${BW_SRC}bwesin.f ${BASE_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwesin.f

bwesit.o${BJ}: ${BW_SRC}bwesit.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
               ${BASE_COM}CONTRL.F77 ${BASE_COM}COEFFS.F77 ${BW_SRC}BWESTD.F77 \
               ${BW_SRC}BWECM2.F77 ${BW_SRC}BWECOM.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwesit.f

bweslp.o${BJ}: ${BW_SRC}bweslp.f
	${FC} -c ${FFLAGS} ${BW_SRC}bweslp.f

bweuca.o${BJ}: ${BW_SRC}bweuca.f
	${FC} -c ${FFLAGS} ${BW_SRC}bweuca.f

bweuky.o${BJ}: ${BW_SRC}bweuky.f
	${FC} -c ${FFLAGS} ${BW_SRC}bweuky.f

bwewea.o${BJ}: ${BW_SRC}bwewea.f ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwewea.f

fxnote.o${BJ}: ${BW_SRC}fxnote.f
	${FC} -c ${FFLAGS} ${BW_SRC}fxnote.f

txnote.o${BJ}: ${BW_SRC}txnote.f
	${FC} -c ${FFLAGS} ${BW_SRC}txnote.f

bweppatv.o${BJ}: ${BW_SRC}bweppatv.f
	${FC} -c ${FFLAGS} ${BW_SRC}bweppatv.f

bweppgt.o${BJ}: ${BW_SRC}bweppgt.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}METRIC.F77 \
               ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 \
               ${BW_SRC}BWEBOX.F77 ${BW_SRC}BWECM2.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWESTD.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bweppgt.f

bwepppt.o${BJ}: ${BW_SRC}bwepppt.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}METRIC.F77 \
               ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 \
               ${BW_SRC}BWEBOX.F77 ${BW_SRC}BWECM2.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWESTD.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwepppt.f
