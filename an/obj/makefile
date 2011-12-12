# Fortran compiler
include ../../bin/FC${OS}.make

# the AN (Alaska Northern Forest) source directory
AN_SRC   = ./../src/
AN_COM   = ./../common/

# other source directories needed for this variant
CR_SRC = ../../cr/src/
NI_SRC = ../../ni/src/
PN_SRC = ../../pn/src/
SO_SRC = ../../so/src/
WS_SRC = ../../ws/src/
ESTB_SRC = ../../estb/src/
MIST_SRC = ../../mistoe/src/
BASE_SRC = ../../base/src/
ECON_SRC = ../../econ/src/
COVR_SRC =  ../../covr/src/

# the Prognosis common blocks directory
BASE_COM = ../../common/

# the FVS FFE common blocks directory
FIRE_BASE_COM = ../../fire/base/common/

FFLAGS = ${I1}${AN_COM}${I2}${BASE_COM}${I2}${FIRE_BASE_COM}${I2}${MIST_SRC}

# the AN object files
AN_OBJS  = blkdat.o${BJ} bratio.o${BJ} ccfcal.o${BJ} cratet.o${BJ} crown.o${BJ} cubrds.o${BJ}  \
           dgdriv.o${BJ} dgf.o${BJ} dubscr.o${BJ} esadvh.o${BJ} esdlay.o${BJ} esnspe.o${BJ} esnutr.o${BJ} \
           espadv.o${BJ} esprep.o${BJ} espsub.o${BJ} espxcs.o${BJ} essubh.o${BJ} estab.o${BJ} estock.o${BJ} \
           estpp.o${BJ} esxcsh.o${BJ} findag.o${BJ} forkod.o${BJ} grinit.o${BJ} grohed.o${BJ} habtyp.o${BJ} \
           htcalc.o${BJ} htgf.o${BJ} \
           logs.o${BJ} maical.o${BJ} morts.o${BJ} oldfst.o${BJ} oldgro.o${BJ} oldsec.o${BJ} prof1.o${BJ} \
           prof2.o${BJ} regent.o${BJ} sbb.o${BJ} seamrt.o${BJ} sitset.o${BJ} top40.o${BJ} varvol.o${BJ} \
           bfvol.o${BJ} \
           bftopk.o${BJ} cftopk.o${BJ} cfvol.o${BJ} cutstk.o${BJ} cwcalc.o${BJ} dgbnd.o${BJ} dunn.o${BJ} \
           rcon.o${BJ} spctrn.o${BJ} vols.o${BJ} \
           htdbh.o${BJ} \
           adjmai.o${BJ} avht40.o${BJ} r6crwd.o${BJ} \
           exppnb.o${BJ} r5crwd.o${BJ} \
           algcmp.o${BJ} algevl.o${BJ} algexp.o${BJ} algkey.o${BJ} algslp.o${BJ} algspp.o${BJ} autcor.o${BJ} \
           bachlo.o${BJ} basdam.o${BJ} behprm.o${BJ} behre.o${BJ} ch2num.o${BJ} ch4bsr.o${BJ} ch4srt.o${BJ} \
           cmrang.o${BJ} comcup.o${BJ} comp.o${BJ} comprs.o${BJ} covolp.o${BJ} cuts.o${BJ} cwidth.o${BJ} \
           damcds.o${BJ} dampro.o${BJ} dbadd.o${BJ} dball.o${BJ} dbchk.o${BJ} dbinit.o${BJ}  \
           dbprse.o${BJ} dbscan.o${BJ} dense.o${BJ} dgscor.o${BJ} disply.o${BJ} dist.o${BJ} ecopls.o${BJ} \
           ecvols.o${BJ} eigen.o${BJ} errgro.o${BJ} evage.o${BJ} evalnk.o${BJ} evcomp.o${BJ} evif.o${BJ} \
           evkey.o${BJ} evldx.o${BJ} evmkv.o${BJ} evmon.o${BJ} evpost.o${BJ} evpred.o${BJ} evtact.o${BJ} \
           evtstv.o${BJ} evusrv.o${BJ} exbgc.o${BJ} exbm.o${BJ} exbrus.o${BJ} exbudl.o${BJ} excov.o${BJ}  \
           exdfb.o${BJ} exdbs.o${BJ} exdftm.o${BJ} execon.o${BJ} \
           exfire.o${BJ} exmpb.o${BJ} exppe.o${BJ} exrd.o${BJ} exsv.o${BJ} extree.o${BJ}  \
           ffert.o${BJ} ffin.o${BJ} filopn.o${BJ} fndkey.o${BJ} formcl.o${BJ} fortyp.o${BJ} fvsstd.o${BJ} \
           genrpt.o${BJ} getsed.o${BJ} gheads.o${BJ} gradd.o${BJ} grdtim.o${BJ} \
           grincr.o${BJ} hbdecd.o${BJ} htgstp.o${BJ} iapsrt.o${BJ} initre.o${BJ} intree.o${BJ} iqrsrt.o${BJ} \
           istfnb.o${BJ} istlnb.o${BJ} keydmp.o${BJ} keyopn.o${BJ} keyrdr.o${BJ} keywds.o${BJ} lb1mem.o${BJ} \
           lbaglr.o${BJ} lbdset.o${BJ} lbget1.o${BJ} lbintr.o${BJ} lbmemr.o${BJ} lbsplr.o${BJ} lbsplw.o${BJ} \
           lbstrd.o${BJ} lbtrim.o${BJ} lbunin.o${BJ} lnkchn.o${BJ} lnkint.o${BJ} main.o${BJ} mbacal.o${BJ} \
           meansd.o${BJ} msbmrt.o${BJ} mults.o${BJ} myopen.o${BJ} natcrz.o${BJ} notre.o${BJ} opadd.o${BJ} \
           opbisr.o${BJ} opcact.o${BJ} opcset.o${BJ} oprdat.o${BJ} \
           opcycl.o${BJ} opdon2.o${BJ} opeval.o${BJ} opexpn.o${BJ} opfind.o${BJ} opget2.o${BJ} opget3.o${BJ} \
           opinit.o${BJ} \
           oplist.o${BJ} opmerg.o${BJ} opnew.o${BJ} opnewc.o${BJ} opsame.o${BJ} opsort.o${BJ} opstus.o${BJ} \
           pctile.o${BJ} prtexm.o${BJ} prtrls.o${BJ} ptbal.o${BJ} rxdibs.o${BJ} rann.o${BJ} rcdset.o${BJ} \
           rdpsrt.o${BJ} reass.o${BJ} resage.o${BJ} revise.o${BJ} scalef.o${BJ} sdefet.o${BJ} sdefln.o${BJ} \
           sdical.o${BJ} sdichk.o${BJ} setup.o${BJ} \
           sgdecd.o${BJ} spdecd.o${BJ} spesrt.o${BJ} sstage.o${BJ} isstag.o${BJ} ksstag.o${BJ} \
           stats.o${BJ} stkval.o${BJ} sumhed.o${BJ} sumout.o${BJ} \
           svblkd.o${BJ} svcdbh.o${BJ} svcmp1.o${BJ} svcmp2.o${BJ} svcmp3.o${BJ} svcrol.o${BJ} svcuts.o${BJ} \
           svcwd.o${BJ} \
           svdfln.o${BJ} svestb.o${BJ} svgrnd.o${BJ} svgtpl.o${BJ} svgtpt.o${BJ} svhabt.o${BJ} svinit.o${BJ} \
           svkey.o${BJ} \
           svlcol.o${BJ} svlnol.o${BJ} svlsol.o${BJ} svmort.o${BJ} svntr.o${BJ} svobol.o${BJ} svonln.o${BJ} \
           svout.o${BJ} svrann.o${BJ} svrmov.o${BJ} svstart.o${BJ} svsnad.o${BJ} svsnage.o${BJ} svtdel.o${BJ}  \
           svtrip.o${BJ} svsalv.o${BJ} tredel.o${BJ} tregro.o${BJ} tremov.o${BJ} tresor.o${BJ} \
           triple.o${BJ} trnasp.o${BJ} trnslo.o${BJ} tvalue.o${BJ} unblnk.o${BJ} upcase.o${BJ} update.o${BJ} \
           upkey.o${BJ} volkey.o${BJ} \
           esblkd.o${BJ} escprs.o${BJ} esetpr.o${BJ} esfltr.o${BJ} esgent.o${BJ} esin.o${BJ} esinit.o${BJ} \
           esmsgs.o${BJ} esout.o${BJ} esplt1.o${BJ} esplt2.o${BJ} esprin.o${BJ} esrann.o${BJ} estime.o${BJ} \
           estump.o${BJ} esuckr.o${BJ} esaddt.o${BJ} \
           miscnt.o${BJ} miscpf.o${BJ} misdam.o${BJ} misdgf.o${BJ} misget.o${BJ} mishgf.o${BJ} misin.o${BJ} \
           misinf.o${BJ} misin0.o${BJ} misintak.o${BJ} mismrt.o${BJ} misprt.o${BJ} misputz.o${BJ} \
           misran.o${BJ} mistoe.o${BJ}

# the econ model object files
ECON_OBJS =  eccalc.o${BJ} echarv.o${BJ} ecin.o${BJ} ecinit.o${BJ} \
             ecsetp.o${BJ} ecstatus.o${BJ} ecvol.o${BJ}

COVR_OBJS = cvbcal.o${BJ} cvbrow.o${BJ} cvcbms.o${BJ} cvclas.o${BJ} cvcnop.o${BJ} cvcw.o${BJ} cvgo.o${BJ}   \
            cvin.o${BJ} cvinit.o${BJ} cvnohd.o${BJ} cvout.o${BJ} cvscon.o${BJ} cvshap.o${BJ} cvsum.o${BJ}

an: ${AN_OBJS}

covr: ${COVR_OBJS}

econ: ${ECON_OBJS}

# compile routines from the AN source directory

blkdat.o${BJ}: ${AN_SRC}blkdat.f ${AN_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77     \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ECON.F77 ${AN_COM}ESPARM.F77   \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}HTCAL.F77 ${BASE_COM}KEYCOM.F77  \
          ${BASE_COM}PDEN.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}RANCOM.F77     \
          ${BASE_COM}SCREEN.F77 ${BASE_COM}VARCOM.F77 ${BASE_COM}FVSSTDCM.F77
	${FC} -c ${FFLAGS} ${AN_SRC}blkdat.f

bratio.o${BJ}: ${AN_SRC}bratio.f
	${FC} -c ${FFLAGS} ${AN_SRC}bratio.f

ccfcal.o${BJ}: ${AN_SRC}ccfcal.f
	${FC} -c ${FFLAGS} ${AN_SRC}ccfcal.f

cratet.o${BJ}: ${AN_SRC}cratet.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}PLOT.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}HTCAL.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${AN_SRC}cratet.f

crown.o${BJ}:  ${AN_SRC}crown.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77      \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}PDEN.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${AN_SRC}crown.f

cubrds.o${BJ}: ${AN_SRC}cubrds.f ${AN_COM}PRGPRM.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${AN_SRC}cubrds.f

dgdriv.o${BJ}: ${AN_SRC}dgdriv.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CALCOM.F77       \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}ESTCOR.F77   \
          ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${AN_SRC}dgdriv.f

dgf.o${BJ}:    ${AN_SRC}dgf.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CALCOM.F77        \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${AN_SRC}dgf.f

dubscr.o${BJ}: ${AN_SRC}dubscr.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77       \
          ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PDEN.F77
	${FC} -c ${FFLAGS} ${AN_SRC}dubscr.f

esadvh.o${BJ}: ${AN_SRC}esadvh.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ESPARM.F77    \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${AN_SRC}esadvh.f

esdlay.o${BJ}: ${AN_SRC}esdlay.f ${AN_COM}PRGPRM.F77 ${AN_COM}ESPARM.F77    \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77
	${FC} -c ${FFLAGS} ${AN_SRC}esdlay.f

esnspe.o${BJ}: ${AN_SRC}esnspe.f
	${FC} -c ${FFLAGS} ${AN_SRC}esnspe.f

esnutr.o${BJ}: ${AN_SRC}esnutr.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${AN_SRC}esnutr.f

espadv.o${BJ}: ${AN_SRC}espadv.f ${AN_COM}PRGPRM.F77 ${AN_COM}ESPARM.F77    \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${AN_SRC}espadv.f

esprep.o${BJ}: ${AN_SRC}esprep.f
	${FC} -c ${FFLAGS} ${AN_SRC}esprep.f

espsub.o${BJ}: ${AN_SRC}espsub.f ${AN_COM}PRGPRM.F77 ${AN_COM}ESPARM.F77    \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${AN_SRC}espsub.f

espxcs.o${BJ}: ${AN_SRC}espxcs.f ${AN_COM}PRGPRM.F77 ${AN_COM}ESPARM.F77    \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${AN_SRC}espxcs.f

essubh.o${BJ}: ${AN_SRC}essubh.f ${AN_COM}PRGPRM.F77 ${AN_COM}ESPARM.F77    \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${AN_SRC}essubh.f

estab.o${BJ}:  ${AN_SRC}estab.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${AN_COM}ESPARM.F77  \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77 \
          ${BASE_COM}PDEN.F77 ${BASE_COM}CALDEN.F77 ${BASE_COM}ESCOMN.F77  \
          ${BASE_COM}ESCOM2.F77 ${BASE_COM}VARCOM.F77 ${BASE_COM}STDSTK.F77
	${FC} -c ${FFLAGS} ${AN_SRC}estab.f

estock.o${BJ}: ${AN_SRC}estock.f
	${FC} -c ${FFLAGS} ${AN_SRC}estock.f

estpp.o${BJ}:  ${AN_SRC}estpp.f ${AN_COM}PRGPRM.F77 ${AN_COM}ESPARM.F77    \
          ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77
	${FC} -c ${FFLAGS} ${AN_SRC}estpp.f

esxcsh.o${BJ}: ${AN_SRC}esxcsh.f ${AN_COM}PRGPRM.F77 ${AN_COM}ESPARM.F77   \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${AN_SRC}esxcsh.f

findag.o${BJ}: ${AN_SRC}findag.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${AN_SRC}findag.f

forkod.o${BJ}: ${AN_SRC}forkod.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77       \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${AN_SRC}forkod.f

grinit.o${BJ}: ${AN_SRC}grinit.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}HTCAL.F77 ${BASE_COM}ECON.F77 ${BASE_COM}MULTCM.F77    \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}VOLSTD.F77 ${BASE_COM}VARCOM.F77  \
          ${BASE_COM}CWDCOM.F77 ${BASE_COM}SUMTAB.F77 ${BASE_COM}CALCOM.F77 \
          ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${AN_SRC}grinit.f

grohed.o${BJ}: ${AN_SRC}grohed.f ${BASE_COM}includeSVN.f
	${FC} -c ${FFLAGS} ${AN_SRC}grohed.f

habtyp.o${BJ}: ${AN_SRC}habtyp.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${AN_SRC}habtyp.f

htcalc.o${BJ}: ${AN_SRC}htcalc.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${AN_SRC}htcalc.f

htgf.o${BJ}:   ${AN_SRC}htgf.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77       \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}HTCAL.F77    \
          ${BASE_COM}PDEN.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${AN_SRC}htgf.f

logs.o${BJ}:   ${AN_SRC}logs.f ${AN_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${AN_SRC}logs.f

maical.o${BJ}: ${AN_SRC}maical.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}PLOT.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}HTCAL.F77
	${FC} -c ${FFLAGS} ${AN_SRC}maical.f

morts.o${BJ}:  ${AN_SRC}morts.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77      \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}ESTREE.F77 ${BASE_COM}MULTCM.F77 \
          ${BASE_COM}PDEN.F77   ${BASE_COM}VARCOM.F77 ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${AN_SRC}morts.f

oldfst.o${BJ}: ${AN_SRC}oldfst.f
	${FC} -c ${FFLAGS} ${AN_SRC}oldfst.f

oldgro.o${BJ}: ${AN_SRC}oldgro.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${AN_SRC}oldgro.f

oldsec.o${BJ}: ${AN_SRC}oldsec.f
	${FC} -c ${FFLAGS} ${AN_SRC}oldsec.f

prof1.o${BJ}:  ${AN_SRC}prof1.f
	${FC} -c ${FFLAGS} ${AN_SRC}prof1.f

prof2.o${BJ}:  ${AN_SRC}prof2.f
	${FC} -c ${FFLAGS} ${AN_SRC}prof2.f

regent.o${BJ}: ${AN_SRC}regent.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CALCOM.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}HTCAL.F77    \
          ${BASE_COM}MULTCM.F77 ${BASE_COM}ESTCOR.F77 ${BASE_COM}PDEN.F77   \
          ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${AN_SRC}regent.f

sbb.o${BJ}:    ${AN_SRC}sbb.f
	${FC} -c ${FFLAGS} ${AN_SRC}sbb.f

seamrt.o${BJ}: ${AN_SRC}seamrt.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}ESTREE.F77 ${BASE_COM}MULTCM.F77 \
          ${BASE_COM}PDEN.F77
	${FC} -c ${FFLAGS} ${AN_SRC}seamrt.f

sitset.o${BJ}: ${AN_SRC}sitset.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77     \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${AN_SRC}sitset.f

top40.o${BJ}:  ${AN_SRC}top40.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${AN_SRC}top40.f

varvol.o${BJ}: ${AN_SRC}varvol.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${AN_SRC}varvol.f

# compile routines from the CR source directory

bfvol.o${BJ}:  ${CR_SRC}bfvol.f  ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${CR_SRC}bfvol.f

# compile routines from the NI source directory

bftopk.o${BJ}: ${NI_SRC}bftopk.f ${AN_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77    \
          ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${NI_SRC}bftopk.f

cftopk.o${BJ}: ${NI_SRC}cftopk.f ${AN_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77    \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cftopk.f

cfvol.o${BJ}:  ${NI_SRC}cfvol.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cfvol.f

cutstk.o${BJ}: ${NI_SRC}cutstk.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77      \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cutstk.f

cwcalc.o${BJ}: ${NI_SRC}cwcalc.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cwcalc.f

dgbnd.o${BJ}:  ${NI_SRC}dgbnd.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}dgbnd.f

dunn.o${BJ}:   ${NI_SRC}dunn.f
	${FC} -c ${FFLAGS} ${NI_SRC}dunn.f

rcon.o${BJ}:   ${NI_SRC}rcon.f ${AN_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77        \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}rcon.f

spctrn.o${BJ}:   ${NI_SRC}spctrn.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}spctrn.f

vols.o${BJ}:   ${NI_SRC}vols.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${NI_SRC}vols.f

# compile routines from the PN source directory

htdbh.o${BJ}: ${PN_SRC}htdbh.f
	${FC} -c ${FFLAGS} ${PN_SRC}htdbh.f

# compile routines from the SO source directory

adjmai.o${BJ}: ${SO_SRC}adjmai.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${SO_SRC}adjmai.f

avht40.o${BJ}: ${SO_SRC}avht40.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77     \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${SO_SRC}avht40.f

r6crwd.o${BJ}: ${SO_SRC}r6crwd.f
	${FC} -c ${FFLAGS} ${SO_SRC}r6crwd.f

# compile routines from the WS source directory

exppnb.o${BJ}: ${WS_SRC}exppnb.f
	${FC} -c ${FFLAGS} ${WS_SRC}exppnb.f

r5crwd.o${BJ}: ${WS_SRC}r5crwd.f
	${FC} -c ${FFLAGS} ${WS_SRC}r5crwd.f

# compile routines from the MISTOE source directory

miscnt.o${BJ}: ${MIST_SRC}miscnt.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}miscnt.f

miscpf.o${BJ}: ${MIST_SRC}miscpf.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}miscpf.f

misdam.o${BJ}: ${MIST_SRC}misdam.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misdam.f

misdgf.o${BJ}: ${MIST_SRC}misdgf.f ${AN_COM}PRGPRM.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misdgf.f

misget.o${BJ}: ${MIST_SRC}misget.f ${AN_COM}PRGPRM.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misget.f

mishgf.o${BJ}: ${MIST_SRC}mishgf.f ${AN_COM}PRGPRM.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}mishgf.f

misin.o${BJ}:  ${MIST_SRC}misin.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77     \
          ${MIST_SRC}MISCOM.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misin.f

misinf.o${BJ}: ${MIST_SRC}misinf.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misinf.f

misin0.o${BJ}: ${MIST_SRC}misin0.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misin0.f

misintak.o${BJ}: ${MIST_SRC}misintak.f ${AN_COM}PRGPRM.F77    \
            ${BASE_COM}CONTRL.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misintak.f


mismrt.o${BJ}: ${MIST_SRC}mismrt.f ${AN_COM}PRGPRM.F77    \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}mismrt.f

misprt.o${BJ}: ${MIST_SRC}misprt.f ${AN_COM}PRGPRM.F77    \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misprt.f

misputz.o${BJ}: ${MIST_SRC}misputz.f ${AN_COM}PRGPRM.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misputz.f

misran.o${BJ}: ${MIST_SRC}misran.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misran.f

mistoe.o${BJ}: ${MIST_SRC}mistoe.f ${AN_COM}PRGPRM.F77    \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${MIST_SRC}MISCOM.F77  \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}mistoe.f

# compile routines from the BASE source directory

algcmp.o${BJ}: ${BASE_SRC}algcmp.f
	${FC} -c ${FFLAGS} ${BASE_SRC}algcmp.f

algevl.o${BJ}: ${BASE_SRC}algevl.f
	${FC} -c ${FFLAGS} ${BASE_SRC}algevl.f

algexp.o${BJ}: ${BASE_SRC}algexp.f
	${FC} -c ${FFLAGS} ${BASE_SRC}algexp.f

algkey.o${BJ}: ${BASE_SRC}algkey.f
	${FC} -c ${FFLAGS} ${BASE_SRC}algkey.f

algslp.o${BJ}: ${BASE_SRC}algslp.f
	${FC} -c ${FFLAGS} ${BASE_SRC}algslp.f

algspp.o${BJ}: ${BASE_SRC}algspp.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}algspp.f

autcor.o${BJ}: ${BASE_SRC}autcor.f ${AN_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}PDEN.F77   \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}autcor.f

bachlo.o${BJ}: ${BASE_SRC}bachlo.f
	${FC} -c ${FFLAGS} ${BASE_SRC}bachlo.f

basdam.o${BJ}: ${BASE_SRC}basdam.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}basdam.f

behprm.o${BJ}: ${BASE_SRC}behprm.f ${AN_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}behprm.f

behre.o${BJ}:  ${BASE_SRC}behre.f ${AN_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}behre.f

ch2num.o${BJ}: ${BASE_SRC}ch2num.f
	${FC} -c ${FFLAGS} ${BASE_SRC}ch2num.f

ch4bsr.o${BJ}: ${BASE_SRC}ch4bsr.f
	${FC} -c ${FFLAGS} ${BASE_SRC}ch4bsr.f

ch4srt.o${BJ}: ${BASE_SRC}ch4srt.f
	${FC} -c ${FFLAGS} ${BASE_SRC}ch4srt.f

cmrang.o${BJ}: ${BASE_SRC}cmrang.f
	${FC} -c ${FFLAGS} ${BASE_SRC}cmrang.f

comcup.o${BJ}: ${BASE_SRC}comcup.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}comcup.f

comp.o${BJ}:   ${BASE_SRC}comp.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77     \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}comp.f

comprs.o${BJ}: ${BASE_SRC}comprs.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ESTREE.F77 \
          ${BASE_COM}STDSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}comprs.f

covolp.o${BJ}: ${BASE_SRC}covolp.f
	${FC} -c ${FFLAGS} ${BASE_SRC}covolp.f

cuts.o${BJ}:   ${BASE_SRC}cuts.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77 ${BASE_COM}ESHOOT.F77 \
          ${BASE_COM}PDEN.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}cuts.f

cwidth.o${BJ}: ${BASE_SRC}cwidth.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}CWDCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}cwidth.f

damcds.o${BJ}: ${BASE_SRC}damcds.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}damcds.f

dampro.o${BJ}: ${BASE_SRC}dampro.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dampro.f

dbadd.o${BJ}:  ${BASE_SRC}dbadd.f ${BASE_COM}DBSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dbadd.f

dball.o${BJ}:  ${BASE_SRC}dball.f ${AN_COM}PRGPRM.F77 ${BASE_COM}DBSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dball.f

dbchk.o${BJ}:  ${BASE_SRC}dbchk.f ${BASE_COM}DBSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dbchk.f

dbinit.o${BJ}: ${BASE_SRC}dbinit.f ${BASE_COM}DBSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dbinit.f

dbprse.o${BJ}: ${BASE_SRC}dbprse.f ${BASE_COM}DBSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dbprse.f

dbscan.o${BJ}: ${BASE_SRC}dbscan.f ${BASE_COM}DBSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dbscan.f

dense.o${BJ}:  ${BASE_SRC}dense.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77      \
          ${BASE_COM}CALCOM.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}HTCAL.F77 ${BASE_COM}PDEN.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dense.f

dgscor.o${BJ}: ${BASE_SRC}dgscor.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dgscor.f

disply.o${BJ}: ${BASE_SRC}disply.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}ECON.F77 ${BASE_COM}SCREEN.F77 ${BASE_COM}SUMTAB.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}disply.f

dist.o${BJ}:   ${BASE_SRC}dist.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dist.f

ecopls.o${BJ}: ${BASE_SRC}ecopls.f ${BASE_COM}ECON.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}ecopls.f

ecvols.o${BJ}: ${BASE_SRC}ecvols.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}ecvols.f

eigen.o${BJ}:  ${BASE_SRC}eigen.f
	${FC} -c ${FFLAGS} ${BASE_SRC}eigen.f

errgro.o${BJ}: ${BASE_SRC}errgro.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}errgro.f

evage.o${BJ}:  ${BASE_SRC}evage.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evage.f

evalnk.o${BJ}: ${BASE_SRC}evalnk.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evalnk.f

evcomp.o${BJ}: ${BASE_SRC}evcomp.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evcomp.f

evif.o${BJ}:   ${BASE_SRC}evif.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evif.f

evkey.o${BJ}:  ${BASE_SRC}evkey.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evkey.f

evldx.o${BJ}:  ${BASE_SRC}evldx.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77      \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}WORKCM.F77 ${BASE_COM}STDSTK.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}SSTGMC.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evldx.f

evmkv.o${BJ}:  ${BASE_SRC}evmkv.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evmkv.f

evmon.o${BJ}:  ${BASE_SRC}evmon.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evmon.f

evpost.o${BJ}: ${BASE_SRC}evpost.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evpost.f

evpred.o${BJ}:  ${BASE_SRC}evpred.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evpred.f

evtact.o${BJ}: ${BASE_SRC}evtact.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evtact.f

evtstv.o${BJ}: ${BASE_SRC}evtstv.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}SUMTAB.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evtstv.f

evusrv.o${BJ}: ${BASE_SRC}evusrv.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evusrv.f

exbgc.o${BJ}:  ${BASE_SRC}exbgc.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exbgc.f

exbm.o${BJ}:   ${BASE_SRC}exbm.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exbm.f

exbrus.o${BJ}: ${BASE_SRC}exbrus.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exbrus.f

exbudl.o${BJ}: ${BASE_SRC}exbudl.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exbudl.f

excov.o${BJ}:  ${BASE_SRC}excov.f
	${FC} -c ${FFLAGS} ${BASE_SRC}excov.f

exdbs.o${BJ}:  ${BASE_SRC}exdbs.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exdbs.f

exdfb.o${BJ}:  ${BASE_SRC}exdfb.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exdfb.f

exdftm.o${BJ}: ${BASE_SRC}exdftm.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exdftm.f

execon.o${BJ}: ${BASE_SRC}execon.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}execon.f

exfire.o${BJ}: ${BASE_SRC}exfire.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exfire.f

exmpb.o${BJ}:  ${BASE_SRC}exmpb.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exmpb.f

exppe.o${BJ}:  ${BASE_SRC}exppe.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exppe.f

exrd.o${BJ}: ${BASE_SRC}exrd.f ${AN_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}exrd.f

exsv.o${BJ}:   ${BASE_SRC}exsv.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}SVDATA.F77 \
          ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}exsv.f

extree.o${BJ}: ${BASE_SRC}extree.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}extree.f

ffert.o${BJ}:  ${BASE_SRC}ffert.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}ffert.f

ffin.o${BJ}:   ${BASE_SRC}ffin.f
	${FC} -c ${FFLAGS} ${BASE_SRC}ffin.f

filopn.o${BJ}: ${BASE_SRC}filopn.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}ECON.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}filopn.f

fndkey.o${BJ}: ${BASE_SRC}fndkey.f
	${FC} -c ${FFLAGS} ${BASE_SRC}fndkey.f

formcl.o${BJ}: ${BASE_SRC}formcl.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}formcl.f

fortyp.o${BJ}: ${BASE_SRC}fortyp.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
        ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}fortyp.f

fvsstd.o${BJ}: ${BASE_SRC}fvsstd.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ESTREE.F77  \
	  ${BASE_COM}VARCOM.F77 ${BASE_COM}WORKCM.F77 ${BASE_COM}STDSTK.F77 \
          ${BASE_COM}SUMTAB.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}ECON.F77 \
          ${BASE_COM}OPCOM.F77  ${BASE_COM}HTCAL.F77 ${BASE_COM}FVSSTDCM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}fvsstd.f

genrpt.o${BJ}: ${BASE_SRC}genrpt.f
	${FC} -c ${FFLAGS} ${BASE_SRC}genrpt.f

getsed.o${BJ}: ${BASE_SRC}getsed.f
	${FC} -c ${FFLAGS} ${BASE_SRC}getsed.f

gheads.o${BJ}: ${BASE_SRC}gheads.f
	${FC} -c ${FFLAGS} ${BASE_SRC}gheads.f

gradd.o${BJ}:  ${BASE_SRC}gradd.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77  \
          ${BASE_COM}PLOT.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}gradd.f

grdtim.o${BJ}: ${BASE_SRC}grdtim.f
	${FC} -c ${FFLAGS} ${BASE_SRC}grdtim.f

grincr.o${BJ}: ${BASE_SRC}grincr.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PDEN.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}STDSTK.F77 ${BASE_COM}ESHAP.F77 \
          ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}grincr.f

hbdecd.o${BJ}: ${BASE_SRC}hbdecd.f
	${FC} -c ${FFLAGS} ${BASE_SRC}hbdecd.f

htgstp.o${BJ}: ${BASE_SRC}htgstp.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}htgstp.f

iapsrt.o${BJ}: ${BASE_SRC}iapsrt.f
	${FC} -c ${FFLAGS} ${BASE_SRC}iapsrt.f

initre.o${BJ}: ${BASE_SRC}initre.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}HTCAL.F77 ${BASE_COM}ECON.F77    \
          ${BASE_COM}KEYCOM.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}VOLSTD.F77 \
          ${BASE_COM}SCREEN.F77 ${BASE_COM}SNCOM.F77 ${BASE_COM}VARCOM.F77  \
          ${BASE_COM}CWDCOM.F77 ${BASE_COM}CALCOM.F77 ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}initre.f

intree.o${BJ}: ${BASE_SRC}intree.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}HTCAL.F77    \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}STDSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}intree.f

iqrsrt.o${BJ}: ${BASE_SRC}iqrsrt.f
	${FC} -c ${FFLAGS} ${BASE_SRC}iqrsrt.f

isstag.o${BJ}: ${BASE_SRC}isstag.f ${AN_COM}PRGPRM.F77 ${BASE_COM}SSTGMC.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}isstag.f

istfnb.o${BJ}: ${BASE_SRC}istfnb.f
	${FC} -c ${FFLAGS} ${BASE_SRC}istfnb.f

istlnb.o${BJ}: ${BASE_SRC}istlnb.f
	${FC} -c ${FFLAGS} ${BASE_SRC}istlnb.f

keydmp.o${BJ}: ${BASE_SRC}keydmp.f
	${FC} -c ${FFLAGS} ${BASE_SRC}keydmp.f

keyopn.o${BJ}: ${BASE_SRC}keyopn.f
	${FC} -c ${FFLAGS} ${BASE_SRC}keyopn.f

keyrdr.o${BJ}: ${BASE_SRC}keyrdr.f
	${FC} -c ${FFLAGS} ${BASE_SRC}keyrdr.f

keywds.o${BJ}: ${BASE_SRC}keywds.f ${BASE_COM}KEYCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}keywds.f

ksstag.o${BJ}: ${BASE_SRC}ksstag.f ${AN_COM}PRGPRM.F77 ${BASE_COM}SSTGMC.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}ksstag.f

lb1mem.o${BJ}: ${BASE_SRC}lb1mem.f
	${FC} -c ${FFLAGS} ${BASE_SRC}lb1mem.f

lbaglr.o${BJ}: ${BASE_SRC}lbaglr.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lbaglr.f

lbdset.o${BJ}: ${BASE_SRC}lbdset.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lbdset.f

lbget1.o${BJ}: ${BASE_SRC}lbget1.f
	${FC} -c ${FFLAGS} ${BASE_SRC}lbget1.f

lbintr.o${BJ}: ${BASE_SRC}lbintr.f
	${FC} -c ${FFLAGS} ${BASE_SRC}lbintr.f

lbmemr.o${BJ}: ${BASE_SRC}lbmemr.f
	${FC} -c ${FFLAGS} ${BASE_SRC}lbmemr.f

lbsplr.o${BJ}: ${BASE_SRC}lbsplr.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lbsplr.f

lbsplw.o${BJ}: ${BASE_SRC}lbsplw.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lbsplw.f

lbstrd.o${BJ}: ${BASE_SRC}lbstrd.f
	${FC} -c ${FFLAGS} ${BASE_SRC}lbstrd.f

lbtrim.o${BJ}: ${BASE_SRC}lbtrim.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lbtrim.f

lbunin.o${BJ}: ${BASE_SRC}lbunin.f
	${FC} -c ${FFLAGS} ${BASE_SRC}lbunin.f

lnkchn.o${BJ}: ${BASE_SRC}lnkchn.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lnkchn.f

lnkint.o${BJ}: ${BASE_SRC}lnkint.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lnkint.f

main.o${BJ}:   ${BASE_SRC}main.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77  \
          ${BASE_COM}PDEN.F77 ${BASE_COM}ECON.F77 ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}main.f

mbacal.o${BJ}: ${BASE_SRC}mbacal.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77  \
          ${BASE_COM}HTCAL.F77 ${BASE_COM}PDEN.F77 ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}mbacal.f

meansd.o${BJ}: ${BASE_SRC}meansd.f
	${FC} -c ${FFLAGS} ${BASE_SRC}meansd.f

msbmrt.o${BJ}: ${BASE_SRC}msbmrt.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77        \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}msbmrt.f

mults.o${BJ}:  ${BASE_SRC}mults.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}mults.f

myopen.o${BJ}: ${BASE_SRC}myopen${PC}.f
	${FC} -c ${FFLAGS} ${BASE_SRC}myopen${PC}.f
	mv myopen${PC}.o${BJ} myopen.tmp; mv myopen.tmp myopen.o${BJ}

natcrz.o${BJ}: ${BASE_SRC}natcrz.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   ${BASE_COM}ESTREE.F77  \
	  ${BASE_COM}VARCOM.F77 ${BASE_COM}WORKCM.F77 ${BASE_COM}STDSTK.F77 \
          ${BASE_COM}SUMTAB.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}ECON.F77   \
          ${BASE_COM}OPCOM.F77  ${BASE_COM}HTCAL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}natcrz.f

notre.o${BJ}:  ${BASE_SRC}notre.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}notre.f

opadd.o${BJ}:  ${BASE_SRC}opadd.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opadd.f

opbisr.o${BJ}: ${BASE_SRC}opbisr.f
	${FC} -c ${FFLAGS} ${BASE_SRC}opbisr.f

opcact.o${BJ}: ${BASE_SRC}opcact.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77 \
        ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opcact.f

opcset.o${BJ}: ${BASE_SRC}opcset.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opcset.f

opcycl.o${BJ}: ${BASE_SRC}opcycl.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opcycl.f

opdon2.o${BJ}: ${BASE_SRC}opdon2.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opdon2.f

opeval.o${BJ}: ${BASE_SRC}opeval.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77   \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opeval.f

opexpn.o${BJ}: ${BASE_SRC}opexpn.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opexpn.f

opfind.o${BJ}: ${BASE_SRC}opfind.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opfind.f

opget2.o${BJ}: ${BASE_SRC}opget2.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opget2.f

opget3.o${BJ}: ${BASE_SRC}opget3.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opget3.f

opinit.o${BJ}: ${BASE_SRC}opinit.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opinit.f

oplist.o${BJ}: ${BASE_SRC}oplist.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}KEYCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}oplist.f

opmerg.o${BJ}: ${BASE_SRC}opmerg.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opmerg.f

opnew.o${BJ}:  ${BASE_SRC}opnew.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77    \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opnew.f

opnewc.o${BJ}: ${BASE_SRC}opnewc.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opnewc.f

oprdat.o${BJ}: ${BASE_SRC}oprdat.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77 \
        ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}oprdat.f

opsame.o${BJ}: ${BASE_SRC}opsame.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opsame.f

opsort.o${BJ}: ${BASE_SRC}opsort.f
	${FC} -c ${FFLAGS} ${BASE_SRC}opsort.f

opstus.o${BJ}: ${BASE_SRC}opstus.f ${AN_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opstus.f

pctile.o${BJ}: ${BASE_SRC}pctile.f
	${FC} -c ${FFLAGS} ${BASE_SRC}pctile.f

prtexm.o${BJ}: ${BASE_SRC}prtexm.f
	${FC} -c ${FFLAGS} ${BASE_SRC}prtexm.f

prtrls.o${BJ}: ${BASE_SRC}prtrls.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ESTREE.F77  \
	  ${BASE_COM}VARCOM.F77 ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}prtrls.f

ptbal.o${BJ}:  ${BASE_SRC}ptbal.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}ptbal.f

rxdibs.o${BJ}: ${BASE_SRC}rxdibs.f
	${FC} -c ${FFLAGS} ${BASE_SRC}rxdibs.f

rann.o${BJ}:   ${BASE_SRC}rann.f ${BASE_COM}RANCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}rann.f

rcdset.o${BJ}: ${BASE_SRC}rcdset.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}rcdset.f

rdpsrt.o${BJ}: ${BASE_SRC}rdpsrt.f
	${FC} -c ${FFLAGS} ${BASE_SRC}rdpsrt.f

reass.o${BJ}:  ${BASE_SRC}reass.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}reass.f

resage.o${BJ}: ${BASE_SRC}resage.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}resage.f

revise.o${BJ}: ${BASE_SRC}revise.f
	${FC} -c ${FFLAGS} ${BASE_SRC}revise.f

scalef.o${BJ}: ${BASE_SRC}scalef.f
	${FC} -c ${FFLAGS} ${BASE_SRC}scalef.f

sdefet.o${BJ}: ${BASE_SRC}sdefet.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sdefet.f

sdefln.o${BJ}: ${BASE_SRC}sdefln.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sdefln.f

sdical.o${BJ}: ${BASE_SRC}sdical.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sdical.f

sdichk.o${BJ}: ${BASE_SRC}sdichk.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sdichk.f

setup.o${BJ}:  ${BASE_SRC}setup.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}setup.f

sgdecd.o${BJ}: ${BASE_SRC}sgdecd.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sgdecd.f

spdecd.o${BJ}: ${BASE_SRC}spdecd.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}spdecd.f

spesrt.o${BJ}: ${BASE_SRC}spesrt.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}spesrt.f

sstage.o${BJ}: ${BASE_SRC}sstage.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}SSTGMC.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sstage.f

stats.o${BJ}:  ${BASE_SRC}stats.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}VOLSTD.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}stats.f

stkval.o${BJ}: ${BASE_SRC}stkval.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}stkval.f

sumhed.o${BJ}: ${BASE_SRC}sumhed.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}SCREEN.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sumhed.f

sumout.o${BJ}: ${BASE_SRC}sumout.f ${AN_COM}PRGPRM.F77 ${BASE_COM}SUMTAB.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sumout.f

svblkd.o${BJ}: ${BASE_SRC}svblkd.f ${AN_COM}PRGPRM.F77 ${BASE_COM}SVDATA.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svblkd.f

svcdbh.o${BJ}: ${BASE_SRC}svcdbh.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}SVDATA.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcdbh.f

svcmp1.o${BJ}: ${BASE_SRC}svcmp1.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}SVDATA.F77 ${BASE_COM}WORKCM.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcmp1.f

svcmp2.o${BJ}: ${BASE_SRC}svcmp2.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}SVDATA.F77 ${BASE_COM}WORKCM.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcmp2.f

svcmp3.o${BJ}: ${BASE_SRC}svcmp3.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}SVDATA.F77 ${BASE_COM}WORKCM.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcmp3.f

svcrol.o${BJ}: ${BASE_SRC}svcrol.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcrol.f

svcuts.o${BJ}: ${BASE_SRC}svcuts.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}SVDATA.F77 ${BASE_COM}WORKCM.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcuts.f

svcwd.o${BJ}: \
${BASE_SRC}svcwd.f \
${AN_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}SVDEAD.F77 \

	${FC} -c  ${FFLAGS} ${BASE_SRC}svcwd.f

svdfln.o${BJ}: ${BASE_SRC}svdfln.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svdfln.f

svestb.o${BJ}: ${BASE_SRC}svestb.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}SVDATA.F77 \
          ${BASE_COM}SVDEAD.F77 ${BASE_COM}WORKCM.F77 ${BASE_COM}METRIC.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svestb.f

svgrnd.o${BJ}: ${BASE_SRC}svgrnd.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}SVDATA.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svgrnd.f

svgtpl.o${BJ}: ${BASE_SRC}svgtpl.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}SVDATA.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svgtpl.f

svgtpt.o${BJ}: ${BASE_SRC}svgtpt.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svgtpt.f

svhabt.o${BJ}: ${BASE_SRC}svhabt.f ${AN_COM}PRGPRM.F77 ${BASE_COM}SVDEAD.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svhabt.f

svinit.o${BJ}: ${BASE_SRC}svinit.f ${AN_COM}PRGPRM.F77 ${BASE_COM}SVDATA.F77 \
          ${BASE_COM}SVDEAD.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svinit.f

svkey.o${BJ}:  ${BASE_SRC}svkey.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}SVDATA.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svkey.f

svlcol.o${BJ}: ${BASE_SRC}svlcol.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svlcol.f

svlnol.o${BJ}: ${BASE_SRC}svlnol.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svlnol.f

svlsol.o${BJ}: ${BASE_SRC}svlsol.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svlsol.f

svmort.o${BJ}: ${BASE_SRC}svmort.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}SVDATA.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svmort.f

svntr.o${BJ}:  ${BASE_SRC}svntr.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svntr.f

svobol.o${BJ}: ${BASE_SRC}svobol.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svobol.f

svonln.o${BJ}: ${BASE_SRC}svonln.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svonln.f

svout.o${BJ}: \
${BASE_SRC}svout.f \
${AN_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}ARRAYS.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}SVDEAD.F77 \
${BASE_COM}METRIC.F77
	${FC} -c  -qnoescape ${FFLAGS} ${BASE_SRC}svout.f

svrann.o${BJ}: ${BASE_SRC}svrann.f ${BASE_COM}SVRCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}svrann.f

svrmov.o${BJ}: ${BASE_SRC}svrmov.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}SVDATA.F77 \
          ${BASE_COM}SVDEAD.F77 ${BASE_COM}METRIC.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svrmov.f

svstart.o${BJ}: \
${BASE_SRC}svstart.f \
${AN_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}RANCOM.F77 \
${BASE_COM}SVRCOM.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svstart.f

svsnad.o${BJ}: ${BASE_SRC}svsnad.f ${AN_COM}PRGPRM.F77 ${BASE_COM}SVDEAD.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}SVDATA.F77 \
          ${BASE_COM}CONTRL.F77 
	${FC} -c  ${FFLAGS} ${BASE_SRC}svsnad.f

svsnage.o${BJ}: \
${BASE_SRC}svsnage.f \
${AN_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}SVDEAD.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svsnage.f

svtdel.o${BJ}: ${BASE_SRC}svtdel.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}SVDATA.F77 ${BASE_COM}WORKCM.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svtdel.f

svtrip.o${BJ}: ${BASE_SRC}svtrip.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}SVDATA.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svtrip.f

svsalv.o${BJ}: \
${BASE_SRC}svsalv.f \
${AN_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}SVDEAD.F77 \

	${FC} -c  ${FFLAGS} ${BASE_SRC}svsalv.f

tredel.o${BJ}: ${BASE_SRC}tredel.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ESTREE.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}tredel.f

tregro.o${BJ}: ${BASE_SRC}tregro.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}tregro.f

tremov.o${BJ}: ${BASE_SRC}tremov.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}STDSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}tremov.f

tresor.o${BJ}: ${BASE_SRC}tresor.f
	${FC} -c ${FFLAGS} ${BASE_SRC}tresor.f

triple.o${BJ}: ${BASE_SRC}triple.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ESTREE.F77 ${BASE_COM}STDSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}triple.f

trnasp.o${BJ}: ${BASE_SRC}trnasp.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}trnasp.f

trnslo.o${BJ}: ${BASE_SRC}trnslo.f ${AN_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}trnslo.f

tvalue.o${BJ}: ${BASE_SRC}tvalue.f
	${FC} -c ${FFLAGS} ${BASE_SRC}tvalue.f

unblnk.o${BJ}: ${BASE_SRC}unblnk.f
	${FC} -c ${FFLAGS} ${BASE_SRC}unblnk.f

upcase.o${BJ}: ${BASE_SRC}upcase.f
	${FC} -c ${FFLAGS} ${BASE_SRC}upcase.f

update.o${BJ}: ${BASE_SRC}update.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}update.f

upkey.o${BJ}:  ${BASE_SRC}upkey.f
	${FC} -c ${FFLAGS} ${BASE_SRC}upkey.f

volkey.o${BJ}: ${BASE_SRC}volkey.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}volkey.f

exmist.o${BJ}: ${BASE_SRC}exmist.f ${AN_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}exmist.f

# compile the ESTB source directory routines

esblkd.o${BJ}:  ${ESTB_SRC}esblkd.f ${AN_COM}PRGPRM.F77 ${AN_COM}ESPARM.F77  \
           ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESRNCM.F77 ${BASE_COM}ESHAP.F77 \
           ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esblkd.f

esaddt.o${BJ}:  ${ESTB_SRC}esaddt.f ${AN_COM}PRGPRM.F77 ${AN_COM}ESPARM.F77  \
           ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESRNCM.F77 ${BASE_COM}ESHAP.F77 \
           ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esaddt.f

escprs.o${BJ}:  ${ESTB_SRC}escprs.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}OUTCOM.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}escprs.f

esetpr.o${BJ}:  ${ESTB_SRC}esetpr.f
	${FC} -c ${FFLAGS} ${ESTB_SRC}esetpr.f

esfltr.o${BJ}:  ${ESTB_SRC}esfltr.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${AN_COM}ESPARM.F77  \
           ${BASE_COM}ESTREE.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}PDEN.F77   \
           ${BASE_COM}CALDEN.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esfltr.f

esgent.o${BJ}:  ${ESTB_SRC}esgent.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
           ${BASE_COM}CONTRL.F77 ${AN_COM}ESPARM.F77 ${BASE_COM}ESCOMN.F77 \
           ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esgent.f

esin.o${BJ}:    ${ESTB_SRC}esin.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ESHAP.F77      \
           ${BASE_COM}ESHAP2.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esin.f

esinit.o${BJ}:  ${ESTB_SRC}esinit.f ${AN_COM}PRGPRM.F77 ${AN_COM}ESPARM.F77   \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77 ${BASE_COM}ESRNCM.F77  \
           ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esinit.f

esmsgs.o${BJ}:  ${ESTB_SRC}esmsgs.f
	${FC} -c ${FFLAGS} ${ESTB_SRC}esmsgs.f

esout.o${BJ}:   ${ESTB_SRC}esout.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esout.f

esplt1.o${BJ}:  ${ESTB_SRC}esplt1.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ESHAP.F77    \
           ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esplt1.f

esplt2.o${BJ}:  ${ESTB_SRC}esplt2.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
           ${BASE_COM}PLOT.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esplt2.f

esprin.o${BJ}:  ${ESTB_SRC}esprin.f
	${FC} -c ${FFLAGS} ${ESTB_SRC}esprin.f

esrann.o${BJ}:  ${ESTB_SRC}esrann.f ${BASE_COM}ESRNCM.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esrann.f

estime.o${BJ}:  ${ESTB_SRC}estime.f ${AN_COM}PRGPRM.F77 ${AN_COM}ESPARM.F77   \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}estime.f

estump.o${BJ}:  ${ESTB_SRC}estump.f ${AN_COM}PRGPRM.F77 ${AN_COM}ESPARM.F77   \
           ${BASE_COM}ESHOOT.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77  \
           ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 ${BASE_COM}COEFFS.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}estump.f

esuckr.o${BJ}:  ${ESTB_SRC}esuckr.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${AN_COM}ESPARM.F77   \
           ${BASE_COM}ESHOOT.F77 ${BASE_COM}ESCOMN.F77 ${BASE_COM}ESCOM2.F77 \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77 ${BASE_COM}ESTREE.F77 \
           ${BASE_COM}STDSTK.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${ESTB_SRC}esuckr.f

# Econ extension compile instructions for the IE variant

eccalc.o${BJ}: ${ECON_SRC}eccalc.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}eccalc.f

echarv.o${BJ}: ${ECON_SRC}echarv.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}echarv.f

ecin.o${BJ}: ${ECON_SRC}ecin.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecin.f

ecinit.o${BJ}: ${ECON_SRC}ecinit.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecinit.f

ecsetp.o${BJ}: ${ECON_SRC}ecsetp.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecsetp.f

ecstatus.o${BJ}: ${ECON_SRC}ecstatus.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecstatus.f

ecvol.o${BJ}: ${ECON_SRC}ecvol.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecvol.f

#  Cover extension compile instructions for the AK vraiant

cvbcal.o${BJ}: ${COVR_SRC}cvbcal.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvbcal.f

cvbrow.o${BJ}: ${COVR_SRC}cvbrow.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvbrow.f

cvcbms.o${BJ}: ${COVR_SRC}cvcbms.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvcbms.f

cvclas.o${BJ}: ${COVR_SRC}cvclas.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvclas.f

cvcnop.o${BJ}: ${COVR_SRC}cvcnop.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvcnop.f

cvcw.o${BJ}:   ${COVR_SRC}cvcw.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77     \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvcw.f

cvgo.o${BJ}:   ${COVR_SRC}cvgo.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvgo.f

cvin.o${BJ}:   ${COVR_SRC}cvin.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvin.f

cvinit.o${BJ}: ${COVR_SRC}cvinit.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvinit.f

cvnohd.o${BJ}: ${COVR_SRC}cvnohd.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}CVCOM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}OUTCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvnohd.f

cvout.o${BJ}:  ${COVR_SRC}cvout.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvout.f

cvscon.o${BJ}: ${COVR_SRC}cvscon.f ${AN_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77 
	${FC} -c ${FFLAGS} ${COVR_SRC}cvscon.f

cvshap.o${BJ}: ${COVR_SRC}cvshap.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvshap.f

cvsum.o${BJ}:  ${COVR_SRC}cvsum.f ${AN_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvsum.f
