# Fortran compiler
include ../../bin/FC${OS}.make

# the FVS common blocks directory
BASE_COM = ../../common/

# the Western root disease vers. 3.0 source directory
RD_SRC   = ../src/

# options for the compiler
FFLAGS = ${I1}${RD_SRC}${I2}${BASE_COM} 
 
# Some variant-specific subroutines are compiled only into their
# respective variant obj directory. Those subroutines are:
#    rdblk1cr.obj
#    rdblk1bm.obj
#    rdblk1em.obj
#    rdblk1ie.obj
#    rdblk1pn.obj
#    rdblk1so.obj
#    rdblk1ut.obj
#    rdblk1wc.obj
#    rdblk1ws.obj
#
RD_OBJS  = rdarea.o${BJ} rdatv.o${BJ} rdbb1.o${BJ} rdbb2.o${BJ} rdbb3.o${BJ} rdbb4.o${BJ} rdbbdo.o${BJ}     \
           rdblk1.o${BJ} rdblk1ec.o${BJ} rdblk1nc.o${BJ} rdblk1so.o${BJ} rdblk1tt.o${BJ} rdblk1ws.o${BJ}    \
           rdbout.o${BJ} rdcent.o${BJ} \
           rdcloc.o${BJ} rdcmpr.o${BJ} rdcntl.o${BJ} rdcsd.o${BJ} rddam.o${BJ} rddbug.o${BJ} rddout.o${BJ}  \
           rddst.o${BJ} rdend.o${BJ} rdescp.o${BJ} rdesin.o${BJ} rdestb.o${BJ} rdgrow.o${BJ} rdin.o${BJ}    \
           rdinf.o${BJ} rdinit.o${BJ} rdinoc.o${BJ} rdinsd.o${BJ} rdinup.o${BJ} rdiout.o${BJ} rdiprp.o${BJ} \
           rdjump.o${BJ} rdload.o${BJ} rdmn1.o${BJ} rdmn2.o${BJ} rdmort.o${BJ} rdmrec.o${BJ} rdoagm.o${BJ}  \
           rdowi.o${BJ} rdowin.o${BJ} rdpr.o${BJ} rdprin.o${BJ} rdpush.o${BJ} rdrani.o${BJ} rdrann.o${BJ}   \
           rdranp.o${BJ} rdrate.o${BJ} rdrdel.o${BJ} rdroot.o${BJ} rdrout.o${BJ} rdsadd.o${BJ}         \
           rdsetp.o${BJ} rdshrk.o${BJ} rdshst.o${BJ} rdslp.o${BJ} rdsout.o${BJ} rdspl1.o${BJ} rdspl2.o${BJ} \
           rdspor.o${BJ} rdsprd.o${BJ} rdspup.o${BJ} rdssiz.o${BJ} rdstp.o${BJ} rdstr.o${BJ} rdsum.o${BJ}   \
           rdtdel.o${BJ} rdtim.o${BJ} rdtout.o${BJ} rdtreg.o${BJ} rdtres.o${BJ} rdtrip.o${BJ} rdtrp.o${BJ}  \
           rdzero.o${BJ}

rd:       ${RD_OBJS}

rdarea.o${BJ}: ${RD_SRC}rdarea.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdarea.f

rdatv.o${BJ}: ${RD_SRC}rdatv.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdatv.f

rdbb1.o${BJ}: ${RD_SRC}rdbb1.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdbb1.f

rdbb2.o${BJ}: ${RD_SRC}rdbb2.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
         ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdbb2.f

rdbb3.o${BJ}: ${RD_SRC}rdbb3.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdbb3.f

rdbb4.o${BJ}: ${RD_SRC}rdbb4.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdbb4.f

rdbbdo.o${BJ}: ${RD_SRC}rdbbdo.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdbbdo.f

rdblk1.o${BJ}: ${RD_SRC}rdblk1.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}METRIC.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdblk1.f

rdblk1ec.o${BJ}: ${RD_SRC}rdblk1ec.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}METRIC.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77       \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdblk1ec.f

rdblk1nc.o${BJ}: ${RD_SRC}rdblk1nc.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}METRIC.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77       \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdblk1nc.f

rdblk1so.o${BJ}: ${RD_SRC}rdblk1so.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
            ${BASE_COM}METRIC.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77     \
            ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdblk1so.f

rdblk1tt.o${BJ}: ${RD_SRC}rdblk1tt.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}METRIC.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77       \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdblk1tt.f

rdblk1ws.o${BJ}: ${RD_SRC}rdblk1ws.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}METRIC.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77       \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdblk1ws.f

rdbout.o${BJ}: ${RD_SRC}rdbout.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDADD.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}METRIC.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdbout.f

rdcent.o${BJ}: ${RD_SRC}rdcent.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${RD_SRC}RDPARM.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdcent.f

rdcloc.o${BJ}: ${RD_SRC}rdcloc.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdcloc.f

rdcmpr.o${BJ}: ${RD_SRC}rdcmpr.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${BASE_COM}CONTRL.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdcmpr.f

rdcntl.o${BJ}: ${RD_SRC}rdcntl.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdcntl.f

rdcsd.o${BJ}: ${RD_SRC}rdcsd.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdcsd.f

rddam.o${BJ}: ${RD_SRC}rddam.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}HTCAL.F77 \
         ${BASE_COM}ESTREE.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rddam.f

rddbug.o${BJ}: ${RD_SRC}rddbug.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rddbug.f

rddout.o${BJ}: ${RD_SRC}rddout.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 \
          ${BASE_COM}METRIC.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rddout.f

rddst.o${BJ}: ${RD_SRC}rddst.f ${RD_SRC}RDPARM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rddst.f

rdend.o${BJ}: ${RD_SRC}rdend.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
         ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdend.f

rdescp.o${BJ}: ${RD_SRC}rdescp.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdescp.f

rdesin.o${BJ}: ${RD_SRC}rdesin.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}ESHAP.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdesin.f

rdestb.o${BJ}: ${RD_SRC}rdestb.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 ${RD_SRC}RDARRY.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdestb.f

rdgrow.o${BJ}: ${RD_SRC}rdgrow.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdgrow.f

rdin.o${BJ}: ${RD_SRC}rdin.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
        ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
        ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 \
        ${BASE_COM}METRIC.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdin.f

rdinf.o${BJ}: ${RD_SRC}rdinf.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
         ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdinf.f

rdinit.o${BJ}: ${RD_SRC}rdinit.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}METRIC.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}PLOT.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdinit.f

rdinoc.o${BJ}: ${RD_SRC}rdinoc.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 ${RD_SRC}RDCOM.F77 \
          ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdinoc.f

rdinsd.o${BJ}: ${RD_SRC}rdinsd.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdinsd.f

rdinup.o${BJ}: ${RD_SRC}rdinup.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdinup.f

rdiout.o${BJ}: ${RD_SRC}rdiout.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdiout.f

rdiprp.o${BJ}: ${RD_SRC}rdiprp.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdiprp.f

rdjump.o${BJ}: ${RD_SRC}rdjump.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdjump.f

rdload.o${BJ}: ${RD_SRC}rdload.f 
	${FC} -c ${FFLAGS} ${RD_SRC}rdload.f

rdmn1.o${BJ}: ${RD_SRC}rdmn1.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}OUTCOM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdmn1.f

rdmn2.o${BJ}: ${RD_SRC}rdmn2.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCRY.F77 \
         ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdmn2.f

rdmort.o${BJ}: ${RD_SRC}rdmort.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdmort.f

rdmrec.o${BJ}: ${RD_SRC}rdmrec.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdmrec.f

rdoagm.o${BJ}: ${RD_SRC}rdoagm.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDADD.F77 ${RD_SRC}RDARRY.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdoagm.f

rdowi.o${BJ}: ${RD_SRC}rdowi.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdowi.f

rdowin.o${BJ}: ${RD_SRC}rdowin.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdowin.f

rdpr.o${BJ}: ${RD_SRC}rdpr.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
        ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 \
        ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
        ${RD_SRC}RDADD.F77 ${BASE_COM}METRIC.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdpr.f

rdprin.o${BJ}: ${RD_SRC}rdprin.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdprin.f

rdpush.o${BJ}: ${RD_SRC}rdpush.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdpush.f

rdrani.o${BJ}: ${RD_SRC}rdrani.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdrani.f

rdrann.o${BJ}: ${RD_SRC}rdrann.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdrann.f

rdranp.o${BJ}: ${RD_SRC}rdranp.f 
	${FC} -c ${FFLAGS} ${RD_SRC}rdranp.f

rdrate.o${BJ}: ${RD_SRC}rdrate.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdrate.f

rdrdel.o${BJ}: ${RD_SRC}rdrdel.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}OUTCOM.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdrdel.f

rdroot.o${BJ}: ${RD_SRC}rdroot.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}PLOT.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdroot.f

rdrout.o${BJ}: ${RD_SRC}rdrout.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdrout.f

rdsadd.o${BJ}: ${RD_SRC}rdsadd.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77 ${BASE_COM}ARRAYS.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdsadd.f

rdsetp.o${BJ}: ${RD_SRC}rdsetp.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdsetp.f

rdshrk.o${BJ}: ${RD_SRC}rdshrk.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdshrk.f

rdshst.o${BJ}: ${RD_SRC}rdshst.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdshst.f

rdslp.o${BJ}: ${RD_SRC}rdslp.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdslp.f

rdsout.o${BJ}: ${RD_SRC}rdsout.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
          ${RD_SRC}RDADD.F77 ${BASE_COM}METRIC.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdsout.f

rdspl1.o${BJ}: ${RD_SRC}rdspl1.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdspl1.f

rdspl2.o${BJ}: ${RD_SRC}rdspl2.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdspl2.f

rdspor.o${BJ}: ${RD_SRC}rdspor.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdspor.f

rdsprd.o${BJ}: ${RD_SRC}rdsprd.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdsprd.f

rdspup.o${BJ}: ${RD_SRC}rdspup.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdspup.f

rdssiz.o${BJ}: ${RD_SRC}rdssiz.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdssiz.f

rdstp.o${BJ}: ${RD_SRC}rdstp.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdstp.f

rdstr.o${BJ}: ${RD_SRC}rdstr.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdstr.f

rdsum.o${BJ}: ${RD_SRC}rdsum.f ${RD_SRC}RDPARM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdsum.f

rdtdel.o${BJ}: ${RD_SRC}rdtdel.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 \
          ${RD_SRC}RDARRY.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtdel.f

rdtim.o${BJ}: ${RD_SRC}rdtim.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtim.f

rdtout.o${BJ}: ${RD_SRC}rdtout.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77 ${BASE_COM}METRIC.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtout.f

rdtreg.o${BJ}: ${RD_SRC}rdtreg.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtreg.f

rdtres.o${BJ}: ${RD_SRC}rdtres.f ${RD_SRC}RDPARM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtres.f

rdtrip.o${BJ}: ${RD_SRC}rdtrip.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 \
          ${RD_SRC}RDARRY.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtrip.f

rdtrp.o${BJ}: ${RD_SRC}rdtrp.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtrp.f

rdzero.o${BJ}: ${RD_SRC}rdzero.f ${BASE_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdzero.f

