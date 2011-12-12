# Fortran compiler
include ../../bin/FC${OS}.make

# the PN source directory
PN_SRC    = ../src/
PN_COM    = ../common/

# the FVS common blocks directory
BASE_COM  = ../../common/

# the MISTOE source directory
MIST_SRC = ../../mistoe/src/
DFB_SRC = ../../dfb/src/
DFB_COM = ../../dfb/common/
RD_SRC = ../../rd/src/
BW_SRC = ../../wsbwe/src/

# options for the compiler
FFLAGS = ${I1}${PN_COM}${I2}${BASE_COM}${I2}${MIST_SRC}${I2}${DFB_COM}

# the PN object files
PN_OBJS   = blkdat.o${BJ} bratio.o${BJ} ccfcal.o${BJ} crown.o${BJ} dgf.o${BJ} ecocls.o${BJ} forkod.o${BJ}    \
            formcl.o${BJ} grinit.o${BJ} grohed.o${BJ} habtyp.o${BJ} htcalc.o${BJ} htdbh.o${BJ} pvref6.o${BJ}  \
            sichg.o${BJ}  sitset.o${BJ} smhtgf.o${BJ} misintpn.o${BJ} varvol.o${BJ}

# Douglas-fir Beetle
DFB_OBJS = dfbdam.o${BJ} dfbdbh.o${BJ} dfbdrv.o${BJ} dfber.o${BJ} dfbgo.o${BJ} dfbhed.o${BJ} \
           dfbin.o${BJ} dfbind.o${BJ} dfbint.o${BJ} dfbinv.o${BJ} dfblkdpn.o${BJ} \
           dfbmod.o${BJ} dfbmrt.o${BJ} dfbout.o${BJ} dfbprb.o${BJ} dfbran.o${BJ} dfbsch.o${BJ} \
           dfbtab.o${BJ} dfbwin.o${BJ} 

# Western Root Disease V3.0
RD_OBJS  = rdarea.o${BJ} rdatv.o${BJ} rdbb1.o${BJ} rdbb2.o${BJ} rdbb3.o${BJ} rdbb4.o${BJ} rdbbdo.o${BJ} \
           rdblk1pn.o${BJ} rdbout.o${BJ} rdcent.o${BJ} \
           rdcloc.o${BJ} rdcmpr.o${BJ} rdcntl.o${BJ} rdcsd.o${BJ} rddam.o${BJ} rddbug.o${BJ} rddout.o${BJ} \
           rddst.o${BJ} rdend.o${BJ} rdescp.o${BJ} rdesin.o${BJ} rdestb.o${BJ} rdgrow.o${BJ} rdin.o${BJ} \
           rdinf.o${BJ} rdinit.o${BJ} rdinoc.o${BJ} rdinsd.o${BJ} rdinup.o${BJ} rdiout.o${BJ} rdiprp.o${BJ} \
           rdjump.o${BJ} rdload.o${BJ} rdmn1.o${BJ} rdmn2.o${BJ} rdmort.o${BJ} rdmrec.o${BJ} rdoagm.o${BJ} \
           rdowi.o${BJ} rdowin.o${BJ} rdpr.o${BJ} rdprin.o${BJ} rdpush.o${BJ} rdrani.o${BJ} rdrann.o${BJ} \
           rdranp.o${BJ} rdrate.o${BJ} rdrdel.o${BJ} rdroot.o${BJ} rdrout.o${BJ} rdsadd.o${BJ} \
           rdsetp.o${BJ} rdshrk.o${BJ} rdshst.o${BJ} rdslp.o${BJ} rdsout.o${BJ} rdspl1.o${BJ} rdspl2.o${BJ} \
           rdspor.o${BJ} rdsprd.o${BJ} rdspup.o${BJ} rdssiz.o${BJ} rdstp.o${BJ} rdstr.o${BJ} rdsum.o${BJ} \
           rdtdel.o${BJ} rdtim.o${BJ} rdtout.o${BJ} rdtreg.o${BJ} rdtres.o${BJ} rdtrip.o${BJ} rdtrp.o${BJ} \
           rdzero.o${BJ} txnote.o${BJ} 

pn: ${PN_OBJS} 

pnx: ${DFB_OBJS} ${RD_OBJS}

blkdat.o${BJ}: \
   ${PN_SRC}blkdat.f \
   ${PN_COM}PRGPRM.F77 \
   ${BASE_COM}COEFFS.F77 \
   ${PN_COM}ESPARM.F77 \
   ${BASE_COM}ESCOMN.F77 \
   ${BASE_COM}PDEN.F77 \
   ${BASE_COM}ECON.F77 \
   ${BASE_COM}HTCAL.F77 \
   ${BASE_COM}CONTRL.F77 \
   ${BASE_COM}PLOT.F77 \
   ${BASE_COM}RANCOM.F77 \
   ${BASE_COM}SCREEN.F77 \
   ${BASE_COM}VARCOM.F77 \
   ${BASE_COM}FVSSTDCM.F77
	${FC} -c ${FFLAGS} ${PN_SRC}blkdat.f

bratio.o${BJ}: \
   ${PN_SRC}bratio.f
	${FC} -c ${FFLAGS} ${PN_SRC}bratio.f

ccfcal.o${BJ}: \
   ${PN_SRC}ccfcal.f
	${FC} -c ${FFLAGS} ${PN_SRC}ccfcal.f

crown.o${BJ}: \
   ${PN_SRC}crown.f \
   ${PN_COM}PRGPRM.F77 \
   ${BASE_COM}PLOT.F77 \
   ${BASE_COM}ARRAYS.F77 \
   ${BASE_COM}COEFFS.F77 \
   ${BASE_COM}CONTRL.F77 \
   ${BASE_COM}OUTCOM.F77 \
   ${BASE_COM}PDEN.F77 \
   ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${PN_SRC}crown.f

dgf.o${BJ}: \
   ${PN_SRC}dgf.f \
   ${PN_COM}PRGPRM.F77 \
   ${BASE_COM}CALCOM.F77 \
   ${BASE_COM}ARRAYS.F77 \
   ${BASE_COM}COEFFS.F77 \
   ${BASE_COM}CONTRL.F77 \
   ${BASE_COM}OUTCOM.F77 \
   ${BASE_COM}PLOT.F77 \
   ${BASE_COM}PDEN.F77
	${FC} -c ${FFLAGS} ${PN_SRC}dgf.f

ecocls.o${BJ}: ${PN_SRC}ecocls.f
	${FC} -c ${FFLAGS} ${PN_SRC}ecocls.f

forkod.o${BJ}: \
   ${PN_SRC}forkod.f \
   ${PN_COM}PRGPRM.F77 \
   ${BASE_COM}PLOT.F77 \
   ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PN_SRC}forkod.f

formcl.o${BJ}: ${PN_SRC}formcl.f ${PN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PN_SRC}formcl.f

grinit.o${BJ}: \
   ${PN_SRC}grinit.f \
   ${PN_COM}PRGPRM.F77 \
   ${BASE_COM}ARRAYS.F77 \
   ${BASE_COM}COEFFS.F77 \
   ${BASE_COM}CONTRL.F77 \
   ${BASE_COM}PLOT.F77 \
   ${BASE_COM}OPCOM.F77 \
   ${BASE_COM}HTCAL.F77 \
   ${BASE_COM}ECON.F77 \
   ${BASE_COM}MULTCM.F77 \
   ${BASE_COM}VOLSTD.F77 \
   ${BASE_COM}VARCOM.F77 \
   ${BASE_COM}CWDCOM.F77 \
   ${BASE_COM}SUMTAB.F77 \
   ${BASE_COM}CALCOM.F77 \
   ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${PN_SRC}grinit.f

grohed.o${BJ}: ${PN_SRC}grohed.f ${BASE_COM}includeSVN.f
	${FC} -c ${FFLAGS} ${PN_SRC}grohed.f

habtyp.o${BJ}: ${PN_SRC}habtyp.f ${PN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${PN_SRC}habtyp.f

htcalc.o${BJ}: \
   ${PN_SRC}htcalc.f \
   ${PN_COM}PRGPRM.F77 \
   ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${PN_SRC}htcalc.f

htdbh.o${BJ}: ${PN_SRC}htdbh.f  
	${FC} -c ${FFLAGS} ${PN_SRC}htdbh.f 

pvref6.o${BJ}: ${PN_SRC}pvref6.f  
	${FC} -c ${FFLAGS} ${PN_SRC}pvref6.f 

sichg.o${BJ}: ${PN_SRC}sichg.f ${PN_COM}PRGPRM.F77 
	${FC} -c ${FFLAGS} ${PN_SRC}sichg.f 

sitset.o${BJ}: \
   ${PN_SRC}sitset.f \
   ${PN_COM}PRGPRM.F77 \
   ${BASE_COM}CONTRL.F77 \
   ${BASE_COM}PLOT.F77 \
   ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${PN_SRC}sitset.f

smhtgf.o${BJ}: \
   ${PN_SRC}smhtgf.f \
   ${PN_COM}PRGPRM.F77 \
   ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PN_SRC}smhtgf.f

misintpn.o${BJ}: ${MIST_SRC}misintpn.f ${PN_COM}PRGPRM.F77   \
            ${BASE_COM}CONTRL.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misintpn.f

varvol.o${BJ}: ${PN_SRC}varvol.f ${PN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${PN_SRC}varvol.f

# Douglas-fir Beetle

dfbdam.o${BJ}:   ${DFB_SRC}dfbdam.f ${PN_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77 
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbdam.f

dfbdbh.o${BJ}:   ${DFB_SRC}dfbdbh.f ${PN_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77   \
            ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbdbh.f

dfbdrv.o${BJ}:   ${DFB_SRC}dfbdrv.f ${PN_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbdrv.f

dfber.o${BJ}:    ${DFB_SRC}dfber.f ${PN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
            ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfber.f

dfbgo.o${BJ}:    ${DFB_SRC}dfbgo.f ${PN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
            ${BASE_COM}CONTRL.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbgo.f

dfbhed.o${BJ}:   ${DFB_SRC}dfbhed.f ${PN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77    \
            ${DFB_COM}DFBCOM.F77 
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbhed.f

dfbin.o${BJ}:    ${DFB_SRC}dfbin.f ${PN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
            ${DFB_COM}DFBCOM.F77 
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbin.f

dfbincr.o${BJ}:  ${DFB_SRC}dfbincr.f ${PN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
            ${BASE_COM}PLOT.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbincr.f

dfbind.o${BJ}:   ${DFB_SRC}dfbind.f
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbind.f

dfbint.o${BJ}:   ${DFB_SRC}dfbint.f ${PN_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbint.f

dfbinv.o${BJ}:   ${DFB_SRC}dfbinv.f ${PN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbinv.f

dfblkdpn.o${BJ}: ${DFB_SRC}dfblkdpn.f ${PN_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfblkdpn.f

dfbmod.o${BJ}:   ${DFB_SRC}dfbmod.f ${PN_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77    \
            ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbmod.f

dfbmrt.o${BJ}:   ${DFB_SRC}dfbmrt.f ${PN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
            ${BASE_COM}CONTRL.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbmrt.f

dfbout.o${BJ}:   ${DFB_SRC}dfbout.f ${PN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbout.f

dfbprb.o${BJ}:   ${DFB_SRC}dfbprb.f ${PN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbprb.f

dfbran.o${BJ}:   ${DFB_SRC}dfbran.f ${PN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbran.f

dfbsch.o${BJ}:   ${DFB_SRC}dfbsch.f ${PN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbsch.f

dfbtab.o${BJ}:   ${DFB_SRC}dfbtab.f ${PN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
            ${BASE_COM}CONTRL.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbtab.f

dfbwin.o${BJ}:   ${DFB_SRC}dfbwin.f ${PN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
            ${BASE_COM}CONTRL.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbwin.f

# Root Disease V3.0

rdarea.o${BJ}: ${RD_SRC}rdarea.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdarea.f

rdatv.o${BJ}: ${RD_SRC}rdatv.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdatv.f

rdbb1.o${BJ}: ${RD_SRC}rdbb1.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdbb1.f

rdbb2.o${BJ}: ${RD_SRC}rdbb2.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
         ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdbb2.f

rdbb3.o${BJ}: ${RD_SRC}rdbb3.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdbb3.f

rdbb4.o${BJ}: ${RD_SRC}rdbb4.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdbb4.f

rdbbdo.o${BJ}: ${RD_SRC}rdbbdo.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdbbdo.f

rdblk1pn.o${BJ}: ${RD_SRC}rdblk1pn.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}METRIC.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdblk1pn.f

rdbout.o${BJ}: ${RD_SRC}rdbout.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDADD.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}METRIC.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdbout.f

rdcent.o${BJ}: ${RD_SRC}rdcent.f ${PN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${RD_SRC}RDPARM.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdcent.f

rdcloc.o${BJ}: ${RD_SRC}rdcloc.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdcloc.f

rdcmpr.o${BJ}: ${RD_SRC}rdcmpr.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${BASE_COM}CONTRL.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdcmpr.f

rdcntl.o${BJ}: ${RD_SRC}rdcntl.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdcntl.f

rdcsd.o${BJ}: ${RD_SRC}rdcsd.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdcsd.f

rddam.o${BJ}: ${RD_SRC}rddam.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}HTCAL.F77 \
         ${BASE_COM}ESTREE.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rddam.f

rddbug.o${BJ}: ${RD_SRC}rddbug.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rddbug.f

rddout.o${BJ}: ${RD_SRC}rddout.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 \
          ${BASE_COM}METRIC.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rddout.f

rddst.o${BJ}: ${RD_SRC}rddst.f ${RD_SRC}RDPARM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rddst.f

rdend.o${BJ}: ${RD_SRC}rdend.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
         ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdend.f

rdescp.o${BJ}: ${RD_SRC}rdescp.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdescp.f

rdesin.o${BJ}: ${RD_SRC}rdesin.f ${PN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}ESHAP.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdesin.f

rdestb.o${BJ}: ${RD_SRC}rdestb.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 ${RD_SRC}RDARRY.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdestb.f

rdgrow.o${BJ}: ${RD_SRC}rdgrow.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdgrow.f

rdin.o${BJ}: ${RD_SRC}rdin.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
        ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77  \
        ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77  \
        ${BASE_COM}METRIC.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdin.f

rdinf.o${BJ}: ${RD_SRC}rdinf.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
         ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdinf.f

rdinit.o${BJ}: ${RD_SRC}rdinit.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}METRIC.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}PLOT.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdinit.f

rdinoc.o${BJ}: ${RD_SRC}rdinoc.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 ${RD_SRC}RDCOM.F77 \
          ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdinoc.f

rdinsd.o${BJ}: ${RD_SRC}rdinsd.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdinsd.f

rdinup.o${BJ}: ${RD_SRC}rdinup.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdinup.f

rdiout.o${BJ}: ${RD_SRC}rdiout.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdiout.f

rdiprp.o${BJ}: ${RD_SRC}rdiprp.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdiprp.f

rdjump.o${BJ}: ${RD_SRC}rdjump.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdjump.f

rdload.o${BJ}: ${RD_SRC}rdload.f 
	${FC} -c ${FFLAGS} ${RD_SRC}rdload.f

rdmn1.o${BJ}: ${RD_SRC}rdmn1.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}OUTCOM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdmn1.f

rdmn2.o${BJ}: ${RD_SRC}rdmn2.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCRY.F77 \
         ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdmn2.f

rdmort.o${BJ}: ${RD_SRC}rdmort.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdmort.f

rdmrec.o${BJ}: ${RD_SRC}rdmrec.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdmrec.f

rdoagm.o${BJ}: ${RD_SRC}rdoagm.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDADD.F77 ${RD_SRC}RDARRY.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdoagm.f

rdowi.o${BJ}: ${RD_SRC}rdowi.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdowi.f

rdowin.o${BJ}: ${RD_SRC}rdowin.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdowin.f

rdpr.o${BJ}: ${RD_SRC}rdpr.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
        ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 \
        ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
        ${RD_SRC}RDADD.F77 ${BASE_COM}METRIC.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdpr.f

rdprin.o${BJ}: ${RD_SRC}rdprin.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdprin.f

rdpush.o${BJ}: ${RD_SRC}rdpush.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdpush.f

rdrani.o${BJ}: ${RD_SRC}rdrani.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdrani.f

rdrann.o${BJ}: ${RD_SRC}rdrann.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdrann.f

rdranp.o${BJ}: ${RD_SRC}rdranp.f 
	${FC} -c ${FFLAGS} ${RD_SRC}rdranp.f

rdrate.o${BJ}: ${RD_SRC}rdrate.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdrate.f

rdrdel.o${BJ}: ${RD_SRC}rdrdel.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}OUTCOM.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdrdel.f

rdroot.o${BJ}: ${RD_SRC}rdroot.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}PLOT.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdroot.f

rdrout.o${BJ}: ${RD_SRC}rdrout.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdrout.f

rdsadd.o${BJ}: ${RD_SRC}rdsadd.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77 ${BASE_COM}ARRAYS.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdsadd.f

rdsetp.o${BJ}: ${RD_SRC}rdsetp.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdsetp.f

rdshrk.o${BJ}: ${RD_SRC}rdshrk.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdshrk.f

rdshst.o${BJ}: ${RD_SRC}rdshst.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdshst.f

rdslp.o${BJ}: ${RD_SRC}rdslp.f ${PN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdslp.f

rdsout.o${BJ}: ${RD_SRC}rdsout.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
          ${RD_SRC}RDADD.F77 ${BASE_COM}METRIC.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdsout.f

rdspl1.o${BJ}: ${RD_SRC}rdspl1.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdspl1.f

rdspl2.o${BJ}: ${RD_SRC}rdspl2.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdspl2.f

rdspor.o${BJ}: ${RD_SRC}rdspor.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdspor.f

rdsprd.o${BJ}: ${RD_SRC}rdsprd.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdsprd.f

rdspup.o${BJ}: ${RD_SRC}rdspup.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdspup.f

rdssiz.o${BJ}: ${RD_SRC}rdssiz.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdssiz.f

rdstp.o${BJ}: ${RD_SRC}rdstp.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdstp.f

rdstr.o${BJ}: ${RD_SRC}rdstr.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdstr.f

rdsum.o${BJ}: ${RD_SRC}rdsum.f ${RD_SRC}RDPARM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdsum.f

rdtdel.o${BJ}: ${RD_SRC}rdtdel.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 \
          ${RD_SRC}RDARRY.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtdel.f

rdtim.o${BJ}: ${RD_SRC}rdtim.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtim.f

rdtout.o${BJ}: ${RD_SRC}rdtout.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77 ${BASE_COM}METRIC.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtout.f

rdtreg.o${BJ}: ${RD_SRC}rdtreg.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtreg.f

rdtres.o${BJ}: ${RD_SRC}rdtres.f ${RD_SRC}RDPARM.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtres.f

rdtrip.o${BJ}: ${RD_SRC}rdtrip.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 \
          ${RD_SRC}RDARRY.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtrip.f

rdtrp.o${BJ}: ${RD_SRC}rdtrp.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdtrp.f

rdzero.o${BJ}: ${RD_SRC}rdzero.f ${PN_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 
	${FC} -c ${FFLAGS} ${RD_SRC}rdzero.f

# Multiple pest link notification from WSBWE source

txnote.o${BJ}: ${BW_SRC}txnote.f
	${FC} -c ${FFLAGS} ${BW_SRC}txnote.f
