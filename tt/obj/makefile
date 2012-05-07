# Fortran compiler
include ../../bin/FC${OS}.make

# the TT (Teton) source and COMMON directories            
TT_SRC   = ./../src/
TT_COM    = ./../common/

# other source directories needed for this variant
CR_SRC = ../../cr/src/
EM_SRC = ../../em/src/
NI_SRC = ../../ni/src/
SO_SRC = ../../so/src/
UT_SRC = ../../ut/src/
WS_SRC = ../../ws/src/
STRP_SRC = ../../strp/src/
BASE_SRC = ../../base/src/
MIST_SRC = ../../mistoe/src/
COVR_SRC = ../../covr/src/
ECON_SRC = ../../econ/src/
DFB_SRC = ../../dfb/src/
DFB_COM = ../../dfb/common/
DFTM_SRC = ../../dftm/src/
MPB_SRC = ../../lpmpb/src/
RD_SRC = ../../rd/src/
BW_SRC = ../../wsbwe/src/

# the FVS common blocks directory
BASE_COM  = ../../common/

# the FVS FFE common blocks directory
FIRE_BASE_COM = ../../fire/base/common/


# options for the compiler
FFLAGS = ${I1}${TT_COM}${I2}${BASE_COM}${I2}${FIRE_BASE_COM}${I2}${MIST_SRC}${I2}${DFB_COM}

# the TT object files
TT_OBJS  = bfvol.o${BJ}  blkdat.o${BJ} bratio.o${BJ} ccfcal.o${BJ} cratet.o${BJ} crown.o${BJ}  cubrds.o${BJ} \
           dgdriv.o${BJ} dgf.o${BJ}    dubscr.o${BJ} essubh.o${BJ} findag.o${BJ} \
           forkod.o${BJ} grinit.o${BJ} grohed.o${BJ} htgf.o${BJ}   maical.o${BJ} morts.o${BJ}  regent.o${BJ} \
           sitset.o${BJ} smdgf.o${BJ}  smhtgf.o${BJ} ttmrt.o${BJ}  varvol.o${BJ} \
           badist.o${BJ} crdecd.o${BJ} \
           logs.o${BJ}   \
           bftopk.o${BJ} cftopk.o${BJ} cfvol.o${BJ} cutstk.o${BJ} cwcalc.o${BJ} dgbnd.o${BJ}   \
           dunn.o${BJ}   rcon.o${BJ}   spctrn.o${BJ} vols.o${BJ} \
           adjmai.o${BJ} avht40.o${BJ} \
           dgfasp.o${BJ} fndag.o${BJ}  habtyp.o${BJ} pvref4.o${BJ} \
           exppnb.o${BJ} r5crwd.o${BJ} \
           algcmp.o${BJ} algevl.o${BJ} algexp.o${BJ} algkey.o${BJ} algslp.o${BJ} algspp.o${BJ} autcor.o${BJ} \
           bachlo.o${BJ} basdam.o${BJ} behprm.o${BJ} behre.o${BJ}  ch2num.o${BJ} ch4bsr.o${BJ} ch4srt.o${BJ} \
           cmrang.o${BJ} comcup.o${BJ} comp.o${BJ}   comprs.o${BJ} covolp.o${BJ} cuts.o${BJ}   cwidth.o${BJ} \
           damcds.o${BJ} dampro.o${BJ} dbadd.o${BJ}  dball.o${BJ}  dbchk.o${BJ}  dbinit.o${BJ} dbprse.o${BJ} \
           dbscan.o${BJ} \
           dense.o${BJ}  dgscor.o${BJ} disply.o${BJ} dist.o${BJ}   ecopls.o${BJ} ecvols.o${BJ} eigen.o${BJ} \
           errgro.o${BJ} evage.o${BJ}  evalnk.o${BJ} evcomp.o${BJ} evif.o${BJ}   evkey.o${BJ}  evldx.o${BJ} \
           evmkv.o${BJ}  evmon.o${BJ}  evpost.o${BJ} evpred.o${BJ} evtact.o${BJ} evtstv.o${BJ} \
           evusrv.o${BJ} exbgc.o${BJ}  exbm.o${BJ}   exbrus.o${BJ} exbudl.o${BJ} excov.o${BJ}  \
           exdbs.o${BJ}  exdfb.o${BJ}  exdftm.o${BJ} execon.o${BJ} exclim.o${BJ} \
           exfire.o${BJ} exmpb.o${BJ}  exppe.o${BJ}  exrd.o${BJ}   extree.o${BJ} \
           ffert.o${BJ}  ffin.o${BJ}   filopn.o${BJ} fndkey.o${BJ} formcl.o${BJ} fortyp.o${BJ} \
           fvsstd.o${BJ} genrpt.o${BJ} \
           getsed.o${BJ} gheads.o${BJ} gradd.o${BJ}  grdtim.o${BJ} grincr.o${BJ} hbdecd.o${BJ} htgstp.o${BJ} \
           iapsrt.o${BJ} initre.o${BJ} intree.o${BJ} iqrsrt.o${BJ} istfnb.o${BJ} istlnb.o${BJ} keydmp.o${BJ} \
           keyopn.o${BJ} keyrdr.o${BJ} keywds.o${BJ} lb1mem.o${BJ} lbaglr.o${BJ} lbdset.o${BJ} lbget1.o${BJ} \
           lbintr.o${BJ} lbmemr.o${BJ} lbsplr.o${BJ} lbsplw.o${BJ} lbstrd.o${BJ} lbtrim.o${BJ} lbunin.o${BJ} \
           lnkchn.o${BJ} lnkint.o${BJ} main.o${BJ}   mbacal.o${BJ} meansd.o${BJ} msbmrt.o${BJ} mults.o${BJ} \
           myopen.o${BJ} natcrz.o${BJ} notre.o${BJ}  opadd.o${BJ}  opbisr.o${BJ} oprdat.o${BJ} \
           opcact.o${BJ} opcset.o${BJ} opcycl.o${BJ} opdon2.o${BJ} opeval.o${BJ} opexpn.o${BJ} \
           opfind.o${BJ} opget2.o${BJ} opget3.o${BJ} opinit.o${BJ} oplist.o${BJ} opmerg.o${BJ} opnew.o${BJ} \
           opnewc.o${BJ} \
           opsame.o${BJ} opsort.o${BJ} opstus.o${BJ} pctile.o${BJ} prtexm.o${BJ} prtrls.o${BJ} ptbal.o${BJ} \
           rxdibs.o${BJ} rann.o${BJ}   rcdset.o${BJ} rdpsrt.o${BJ} reass.o${BJ}  resage.o${BJ} revise.o${BJ} \
           scalef.o${BJ} sdefet.o${BJ} sdefln.o${BJ} \
           sdical.o${BJ} sdichk.o${BJ} setup.o${BJ}  sgdecd.o${BJ} spdecd.o${BJ} spesrt.o${BJ} \
           sstage.o${BJ} stkval.o${BJ} isstag.o${BJ} ksstag.o${BJ} stats.o${BJ}  sumhed.o${BJ} sumout.o${BJ} \
           svblkd.o${BJ} svcdbh.o${BJ} svcmp1.o${BJ} svcmp2.o${BJ} svcmp3.o${BJ} svcrol.o${BJ} svcuts.o${BJ} \
           svcwd.o${BJ} \
           svdfln.o${BJ} svestb.o${BJ} svgrnd.o${BJ} svgtpl.o${BJ} svgtpt.o${BJ} svhabt.o${BJ} svinit.o${BJ} \
           svkey.o${BJ}  svlcol.o${BJ} \
           svlnol.o${BJ} svlsol.o${BJ} svmort.o${BJ} svntr.o${BJ}  svobol.o${BJ} svonln.o${BJ} svout.o${BJ}  \
           svrann.o${BJ} svrmov.o${BJ} svstart.o${BJ} svsnad.o${BJ} svsnage.o${BJ} svtdel.o${BJ} svtrip.o${BJ} \
           svsalv.o${BJ} tredel.o${BJ} tregro.o${BJ} tremov.o${BJ} tresor.o${BJ} triple.o${BJ} trnasp.o${BJ} \
           trnslo.o${BJ} tvalue.o${BJ} unblnk.o${BJ} upcase.o${BJ} update.o${BJ} upkey.o${BJ}  volkey.o${BJ} \
           esaddt.o${BJ} esblkd.o${BJ} escprs.o${BJ} esetpr.o${BJ} esfltr.o${BJ} esgent.o${BJ} esin.o${BJ}   \
           esinit.o${BJ} esmsgs.o${BJ} esnutr.o${BJ} esout.o${BJ}  esplt1.o${BJ} esplt2.o${BJ} esprep.o${BJ} \
           esprin.o${BJ} esrann.o${BJ} essprt.o${BJ} estab.o${BJ}  estime.o${BJ} estump.o${BJ} esuckr.o${BJ} \
           miscnt.o${BJ} miscpf.o${BJ} misdam.o${BJ} misdgf.o${BJ} misget.o${BJ} mishgf.o${BJ} misin.o${BJ}  \
           misinf.o${BJ} misin0.o${BJ} misinttt.o${BJ} mismrt.o${BJ} misprt.o${BJ} misputz.o${BJ} misran.o${BJ} \
           mistoe.o${BJ} 

# Dwarf Mistletoe objects are included in the base object list above.

# Douglas-fir Beetle
DFB_OBJS = dfbdam.o${BJ} dfbdbh.o${BJ} dfbdrv.o${BJ} dfber.o${BJ}  dfbgo.o${BJ}  dfbhed.o${BJ} \
           dfbin.o${BJ}  dfbind.o${BJ} dfbint.o${BJ} dfbinv.o${BJ} dfblkdtt.o${BJ} \
           dfbmod.o${BJ} dfbmrt.o${BJ} dfbout.o${BJ} dfbprb.o${BJ} dfbran.o${BJ} dfbsch.o${BJ} \
           dfbtab.o${BJ} dfbwin.o${BJ}

# Douglas-fir Tussock Moth
#
# dftm objects: garbel.o${BJ} grclas.o${BJ} grpsum.o${BJ} are removed from this list
# because they are present in the lpmpb list
#
DFTM_OBJS = dfole8.o${BJ} dftmgo.o${BJ} dftmin.o${BJ} dftmod.o${BJ} g0comp.o${BJ} gfcomp.o${BJ} \
            inscyc.o${BJ} redist.o${BJ} tmbchl.o${BJ} tmbmas.o${BJ} tmcoup.o${BJ} \
            tmdam.o${BJ}  tmdtls.o${BJ} tmhed.o${BJ}  tminit.o${BJ} tmotpr.o${BJ} \
            tmout.o${BJ}  tmrann.o${BJ} tmschd.o${BJ} uv1.o${BJ}    uv2.o${BJ}    y0comp.o${BJ} \
            y1comp.o${BJ} z1comp.o${BJ}

# Lodgepole Mountain Pine Beetle
MPB_OBJS = back.o${BJ}   betin.o${BJ}  coldbh.o${BJ} coldrv.o${BJ} colind.o${BJ} colmod.o${BJ} colmrt.o${BJ} \
           emerg.o${BJ}  forw.o${BJ}   garbel.o${BJ} grclas.o${BJ} grpsum.o${BJ} mpbcup.o${BJ} mpbdam.o${BJ} \
           mpbdrv.o${BJ} mpber.o${BJ}  mpbgam.o${BJ} mpbgo.o${BJ}  mpbhed.o${BJ} mpbin.o${BJ}  mpbint.o${BJ} \
           mpblkdtt.o${BJ} mpbmod.o${BJ} mpbout.o${BJ} mpbtab.o${BJ} mpgr.o${BJ} mpotpr.o${BJ} mprann.o${BJ} \
           mpsdlp.o${BJ} percnt.o${BJ} pmslp.o${BJ}  pqsml.o${BJ}  ptary.o${BJ}  ptbno.o${BJ}  ptgrp.o${BJ} \
           ptint.o${BJ}  ptplt.o${BJ}  ptsav.o${BJ}  ptscl.o${BJ}  ptsrg.o${BJ}  ptssc.o${BJ}  ptsym.o${BJ} \
           surfce.o${BJ} surflp.o${BJ} tafit.o${BJ}

# Western Root Disease V3.0
RD_OBJS  = rdarea.o${BJ} rdatv.o${BJ}  rdbb1.o${BJ}  rdbb2.o${BJ}  rdbb3.o${BJ}  rdbb4.o${BJ}  rdbbdo.o${BJ} \
           rdblk1tt.o${BJ} rdbout.o${BJ} rdcent.o${BJ} \
           rdcloc.o${BJ} rdcmpr.o${BJ} rdcntl.o${BJ} rdcsd.o${BJ}  rddam.o${BJ}  rddbug.o${BJ} rddout.o${BJ} \
           rddst.o${BJ}  rdend.o${BJ}  rdescp.o${BJ} rdesin.o${BJ} rdestb.o${BJ} rdgrow.o${BJ} rdintt.o${BJ} \
           rdinf.o${BJ}  rdinit.o${BJ} rdinoc.o${BJ} rdinsd.o${BJ} rdinup.o${BJ} rdiout.o${BJ} rdiprp.o${BJ} \
           rdjump.o${BJ} rdload.o${BJ} rdmn1.o${BJ}  rdmn2.o${BJ}  rdmort.o${BJ} rdmrec.o${BJ} rdoagm.o${BJ} \
           rdowi.o${BJ}  rdowin.o${BJ} rdpr.o${BJ}   rdprin.o${BJ} rdpush.o${BJ} rdrani.o${BJ} rdrann.o${BJ} \
           rdranp.o${BJ} rdrate.o${BJ} rdrdel.o${BJ} rdroot.o${BJ} rdrout.o${BJ} rdsadd.o${BJ} \
           rdsetp.o${BJ} rdshrk.o${BJ} rdshst.o${BJ} rdslp.o${BJ}  rdsout.o${BJ} rdspl1.o${BJ} rdspl2.o${BJ} \
           rdspor.o${BJ} rdsprd.o${BJ} rdspup.o${BJ} rdssiz.o${BJ} rdstp.o${BJ}  rdstr.o${BJ}  rdsum.o${BJ} \
           rdtdel.o${BJ} rdtim.o${BJ}  rdtout.o${BJ} rdtreg.o${BJ} rdtres.o${BJ} rdtrip.o${BJ} rdtrp.o${BJ} \
           rdzero.o${BJ}

# Western Spruce Budworm (GenDefol)
WSBWE_OBJS = bweadv.o${BJ} bweage.o${BJ} bwebet.o${BJ} bwebktt.o${BJ} bwebmstt.o${BJ} \
             bwecrc.o${BJ} bwecup.o${BJ} \
             bwedam.o${BJ} bwedef.o${BJ} bwedie.o${BJ} bwedr.o${BJ} bweerr.o${BJ} \
             bweest.o${BJ} bwefdk.o${BJ} bwego.o${BJ} bwein.o${BJ} bweint.o${BJ} \
             bwekdp.o${BJ} bwekey.o${BJ} bwekrd.o${BJ} bwelit.o${BJ} bweob.o${BJ} \
             bweout.o${BJ} bwep1.o${BJ} bwep3.o${BJ} bwep4.o${BJ} \
             bwepdm.o${BJ} bweprb.o${BJ} bweran.o${BJ} bwernp.o${BJ} bwesin.o${BJ} \
             bwesit.o${BJ} bweslp.o${BJ} bweuca.o${BJ} bweuky.o${BJ} bwewea.o${BJ} \
             fxnote.o${BJ} txnote.o${BJ}

# White Pine Blister Rust - stub only
WPBR_OBJS  = exbrus.o${BJ}

# the econ model object files
ECON_OBJS =  eccalc.o${BJ} echarv.o${BJ}   ecin.o${BJ}   ecinit.o${BJ} \
             ecsetp.o${BJ} ecstatus.o${BJ} ecvol.o${BJ}

COVR_OBJS = cvbcal.o${BJ} cvbrow.o${BJ} cvcbms.o${BJ} cvclas.o${BJ} cvcnop.o${BJ} cvcw.o${BJ}   cvgo.o${BJ}   \
            cvin.o${BJ}   cvinit.o${BJ} cvnohd.o${BJ} cvout.o${BJ}  cvscon.o${BJ} cvshap.o${BJ} cvsum.o${BJ}

# The BGC model objects
BGC_OBJS = babsrad.o${BJ}   balloca.o${BJ}   bark.o${BJ}     bcrown.o${BJ}  bcvts.o${BJ}   bentyload.o${BJ} \
           bentyupdt.o${BJ} bgcfvs.o${BJ}    bgcgo.o${BJ}    bgcgrow.o${BJ} bgcin.o${BJ}   bgcint.o${BJ}    \
           bgrown.o${BJ}    bhtdiam.o${BJ}   binitial.o${BJ} biopen.o${BJ}  bkill.o${BJ}   bpenman.o${BJ}   \
           bsiteload.o${BJ} bstndbgcn.o${BJ} bstructn.o${BJ} btrnovr.o${BJ} bupdate.o${BJ} bwater.o${BJ}    \
           bzcomp.o${BJ}    setleafn.o${BJ}  sitocn.o${BJ}   redolai.o${BJ}

tt: ${TT_OBJS}

ttx: ${DFB_OBJS} ${DFTM_OBJS} ${MPB_OBJS} ${RD_OBJS} ${WSBWE_OBJS}

econ: ${ECON_OBJS}

covr: ${COVR_OBJS}

tt_bgc: ${BGC_OBJS}

# compile routines from TT source directory

bfvol.o${BJ}:  ${TT_SRC}bfvol.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${TT_SRC}bfvol.f

blkdat.o${BJ}: ${TT_SRC}blkdat.f ${TT_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77   \
          ${TT_COM}ESPARM.F77 ${TT_COM}ESCOMN.F77 ${BASE_COM}PDEN.F77 \
          ${BASE_COM}ECON.F77 ${BASE_COM}HTCAL.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}PLOT.F77 ${BASE_COM}RANCOM.F77 ${BASE_COM}SCREEN.F77 \
          ${BASE_COM}VARCOM.F77 ${BASE_COM}GGCOM.F77 ${BASE_COM}FVSSTDCM.F77
	${FC} -c ${FFLAGS} ${TT_SRC}blkdat.f

bratio.o${BJ}: ${TT_SRC}bratio.f
	${FC} -c ${FFLAGS} ${TT_SRC}bratio.f

ccfcal.o${BJ}: ${TT_SRC}ccfcal.f ${TT_COM}PRGPRM.F77 
	${FC} -c ${FFLAGS} ${TT_SRC}ccfcal.f

cratet.o${BJ}: ${TT_SRC}cratet.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}HTCAL.F77 ${BASE_COM}VARCOM.F77 \
          ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${TT_SRC}cratet.f

crown.o${BJ}:  ${TT_SRC}crown.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77        \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PDEN.F77 ${BASE_COM}VARCOM.F77 
	${FC} -c ${FFLAGS} ${TT_SRC}crown.f

cubrds.o${BJ}: ${TT_SRC}cubrds.f ${TT_COM}PRGPRM.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${TT_SRC}cubrds.f

dgdriv.o${BJ}: ${TT_SRC}dgdriv.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CALCOM.F77      \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}PLOT.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}ESTCOR.F77    \
          ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${TT_SRC}dgdriv.f

dgf.o${BJ}:    ${TT_SRC}dgf.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CALCOM.F77        \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}PDEN.F77 \
          ${BASE_COM}GGCOM.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${TT_SRC}dgf.f

dubscr.o${BJ}: ${TT_SRC}dubscr.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PDEN.F77
	${FC} -c ${FFLAGS} ${TT_SRC}dubscr.f

essubh.o${BJ}: ${TT_SRC}essubh.f ${TT_COM}PRGPRM.F77 ${TT_COM}ESPARM.F77 \
          ${TT_COM}ESCOMN.F77 
	${FC} -c ${FFLAGS} ${TT_SRC}essubh.f

findag.o${BJ}: ${TT_SRC}findag.f ${TT_COM}PRGPRM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${TT_SRC}findag.f

forkod.o${BJ}: ${TT_SRC}forkod.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${TT_SRC}forkod.f

grinit.o${BJ}: ${TT_SRC}grinit.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}HTCAL.F77 ${BASE_COM}ECON.F77 ${BASE_COM}MULTCM.F77  \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}VOLSTD.F77 ${BASE_COM}VARCOM.F77 \
          ${BASE_COM}CWDCOM.F77 ${BASE_COM}SUMTAB.F77 ${BASE_COM}CALCOM.F77 \
          ${BASE_COM}WORKCM.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${TT_SRC}grinit.f

grohed.o${BJ}: ${TT_SRC}grohed.f ${BASE_COM}includeSVN.f
	${FC} -c ${FFLAGS} ${TT_SRC}grohed.f

htgf.o${BJ}:   ${TT_SRC}htgf.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77       \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}HTCAL.F77    \
          ${BASE_COM}PDEN.F77 ${BASE_COM}VARCOM.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${TT_SRC}htgf.f

maical.o${BJ}: ${TT_SRC}maical.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}PLOT.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}HTCAL.F77 
	${FC} -c ${FFLAGS} ${TT_SRC}maical.f

morts.o${BJ}:  ${TT_SRC}morts.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77      \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}ESTREE.F77 ${BASE_COM}MULTCM.F77 \
          ${BASE_COM}PDEN.F77   ${BASE_COM}VARCOM.F77 ${BASE_COM}WORKCM.F77 \
          ${BASE_COM}CALCOM.F77
	${FC} -c ${FFLAGS} ${TT_SRC}morts.f

regent.o${BJ}: ${TT_SRC}regent.f ${TT_COM}PRGPRM.F77  ${BASE_COM}CALCOM.F77     \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   ${BASE_COM}HTCAL.F77    \
          ${BASE_COM}MULTCM.F77 ${BASE_COM}ESTCOR.F77 ${BASE_COM}PDEN.F77   \
          ${BASE_COM}VARCOM.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${TT_SRC}regent.f

sitset.o${BJ}: ${TT_SRC}sitset.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${TT_SRC}sitset.f

smdgf.o${BJ}: ${TT_SRC}smdgf.f 
	${FC} -c ${FFLAGS} ${TT_SRC}smdgf.f

smhtgf.o${BJ}: ${TT_SRC}smhtgf.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}PLOT.F77 ${BASE_COM}CALCOM.F77 ${BASE_COM}COEFFS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PDEN.F77  \
          ${BASE_COM}VARCOM.F77 
	${FC} -c ${FFLAGS} ${TT_SRC}smhtgf.f

ttmrt.o${BJ}: ${TT_SRC}ttmrt.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77       \
         ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77    \
         ${BASE_COM}COEFFS.F77 ${BASE_COM}ESTREE.F77 ${BASE_COM}MULTCM.F77  \
         ${BASE_COM}PDEN.F77
	${FC} -c ${FFLAGS} ${TT_SRC}ttmrt.f

varvol.o${BJ}: ${TT_SRC}varvol.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}VOLSTD.F77 ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${TT_SRC}varvol.f

# compile routines from CR source directory

badist.o${BJ}: ${CR_SRC}badist.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${CR_SRC}badist.f

crdecd.o${BJ}: ${CR_SRC}crdecd.f
	${FC} -c ${FFLAGS} ${CR_SRC}crdecd.f


# compile routines from EM source directory

logs.o${BJ}:   ${EM_SRC}logs.f
	${FC} -c ${FFLAGS} ${EM_SRC}logs.f

# compile routines from NI source directory

bftopk.o${BJ}: ${NI_SRC}bftopk.f ${TT_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77    \
          ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${NI_SRC}bftopk.f

cftopk.o${BJ}: ${NI_SRC}cftopk.f ${TT_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77    \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cftopk.f

cfvol.o${BJ}:  ${NI_SRC}cfvol.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cfvol.f

cutstk.o${BJ}: ${NI_SRC}cutstk.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77      \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cutstk.f

cwcalc.o${BJ}: ${NI_SRC}cwcalc.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}cwcalc.f

dgbnd.o${BJ}:  ${NI_SRC}dgbnd.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}dgbnd.f

dunn.o${BJ}:   ${NI_SRC}dunn.f
	${FC} -c ${FFLAGS} ${NI_SRC}dunn.f

rcon.o${BJ}:   ${NI_SRC}rcon.f ${TT_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77        \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${NI_SRC}rcon.f

spctrn.o${BJ}: ${NI_SRC}spctrn.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${NI_SRC}spctrn.f

vols.o${BJ}:   ${NI_SRC}vols.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${NI_SRC}vols.f

# compile routines from SO source directory

adjmai.o${BJ}: ${SO_SRC}adjmai.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${SO_SRC}adjmai.f

avht40.o${BJ}: ${SO_SRC}avht40.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${SO_SRC}avht40.f

# compile routines from UT source directory

dgfasp.o${BJ}: ${UT_SRC}dgfasp.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}PDEN.F77
	${FC} -c ${FFLAGS} ${UT_SRC}dgfasp.f

fndag.o${BJ}: ${UT_SRC}fndag.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77      \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${UT_SRC}fndag.f

habtyp.o${BJ}: ${UT_SRC}habtyp.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${UT_SRC}habtyp.f

pvref4.o${BJ}: ${UT_SRC}pvref4.f
	${FC} -c ${FFLAGS} ${UT_SRC}pvref4.f

# compile routines from WS source directory

exppnb.o${BJ}: ${WS_SRC}exppnb.f
	${FC} -c ${FFLAGS} ${WS_SRC}exppnb.f

r5crwd.o${BJ}: ${WS_SRC}r5crwd.f
	${FC} -c ${FFLAGS} ${WS_SRC}r5crwd.f

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

algspp.o${BJ}: ${BASE_SRC}algspp.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}algspp.f

autcor.o${BJ}: ${BASE_SRC}autcor.f ${TT_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}PDEN.F77   \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}autcor.f

bachlo.o${BJ}: ${BASE_SRC}bachlo.f
	${FC} -c ${FFLAGS} ${BASE_SRC}bachlo.f

basdam.o${BJ}: ${BASE_SRC}basdam.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}basdam.f

behprm.o${BJ}: ${BASE_SRC}behprm.f ${TT_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}behprm.f

behre.o${BJ}:  ${BASE_SRC}behre.f ${TT_COM}PRGPRM.F77 ${BASE_COM}COEFFS.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}behre.f

ch2num.o${BJ}: ${BASE_SRC}ch2num.f
	${FC} -c ${FFLAGS} ${BASE_SRC}ch2num.f

ch4bsr.o${BJ}: ${BASE_SRC}ch4bsr.f
	${FC} -c ${FFLAGS} ${BASE_SRC}ch4bsr.f

ch4srt.o${BJ}: ${BASE_SRC}ch4srt.f
	${FC} -c ${FFLAGS} ${BASE_SRC}ch4srt.f

cmrang.o${BJ}: ${BASE_SRC}cmrang.f
	${FC} -c ${FFLAGS} ${BASE_SRC}cmrang.f

comcup.o${BJ}: ${BASE_SRC}comcup.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}comcup.f

comp.o${BJ}:   ${BASE_SRC}comp.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77     \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}comp.f

comprs.o${BJ}: ${BASE_SRC}comprs.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ESTREE.F77 \
          ${BASE_COM}STDSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}comprs.f

covolp.o${BJ}: ${BASE_SRC}covolp.f
	${FC} -c ${FFLAGS} ${BASE_SRC}covolp.f

cuts.o${BJ}:   ${BASE_SRC}cuts.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77 ${BASE_COM}ESHOOT.F77 \
          ${BASE_COM}PDEN.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}cuts.f

cwidth.o${BJ}: ${BASE_SRC}cwidth.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}CWDCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}cwidth.f

damcds.o${BJ}: ${BASE_SRC}damcds.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}damcds.f

dampro.o${BJ}: ${BASE_SRC}dampro.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dampro.f

dbadd.o${BJ}:  ${BASE_SRC}dbadd.f ${BASE_COM}DBSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dbadd.f

dball.o${BJ}:  ${BASE_SRC}dball.f ${TT_COM}PRGPRM.F77 ${BASE_COM}DBSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dball.f

dbchk.o${BJ}:  ${BASE_SRC}dbchk.f ${BASE_COM}DBSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dbchk.f

dbinit.o${BJ}: ${BASE_SRC}dbinit.f ${BASE_COM}DBSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dbinit.f

dbprse.o${BJ}: ${BASE_SRC}dbprse.f ${BASE_COM}DBSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dbprse.f

dbscan.o${BJ}: ${BASE_SRC}dbscan.f ${BASE_COM}DBSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dbscan.f

dense.o${BJ}:  ${BASE_SRC}dense.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77      \
          ${BASE_COM}CALCOM.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}HTCAL.F77 ${BASE_COM}PDEN.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dense.f

dgscor.o${BJ}: ${BASE_SRC}dgscor.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dgscor.f

disply.o${BJ}: ${BASE_SRC}disply.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}ECON.F77 ${BASE_COM}SCREEN.F77 ${BASE_COM}SUMTAB.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}disply.f

dist.o${BJ}:   ${BASE_SRC}dist.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}dist.f

ecopls.o${BJ}: ${BASE_SRC}ecopls.f ${BASE_COM}ECON.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}ecopls.f

ecvols.o${BJ}: ${BASE_SRC}ecvols.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}ecvols.f

eigen.o${BJ}:  ${BASE_SRC}eigen.f
	${FC} -c ${FFLAGS} ${BASE_SRC}eigen.f

errgro.o${BJ}: ${BASE_SRC}errgro.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}errgro.f

evage.o${BJ}:  ${BASE_SRC}evage.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evage.f

evalnk.o${BJ}: ${BASE_SRC}evalnk.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evalnk.f

evcomp.o${BJ}: ${BASE_SRC}evcomp.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evcomp.f

evif.o${BJ}:   ${BASE_SRC}evif.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evif.f

evkey.o${BJ}:  ${BASE_SRC}evkey.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evkey.f

evldx.o${BJ}:  ${BASE_SRC}evldx.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77      \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}STDSTK.F77 ${BASE_COM}WORKCM.F77 \
          ${BASE_COM}SSTGMC.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evldx.f

evmkv.o${BJ}:  ${BASE_SRC}evmkv.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evmkv.f

evmon.o${BJ}:  ${BASE_SRC}evmon.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evmon.f

evpost.o${BJ}: ${BASE_SRC}evpost.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evpost.f

evpred.o${BJ}:  ${BASE_SRC}evpred.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evpred.f

evtact.o${BJ}: ${BASE_SRC}evtact.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evtact.f

evtstv.o${BJ}: ${BASE_SRC}evtstv.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}SUMTAB.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}evtstv.f

evusrv.o${BJ}: ${BASE_SRC}evusrv.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
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

execon.o${BJ}: ${BASE_SRC}execon.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}execon.f

exclim.o${BJ}: ${BASE_SRC}exclim.f ${TT_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}exclim.f

exfire.o${BJ}: ${BASE_SRC}exfire.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exfire.f

exmpb.o${BJ}:  ${BASE_SRC}exmpb.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exmpb.f

exppe.o${BJ}:  ${BASE_SRC}exppe.f
	${FC} -c ${FFLAGS} ${BASE_SRC}exppe.f

exrd.o${BJ}: ${BASE_SRC}exrd.f ${TT_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}exrd.f

exsv.o${BJ}:   ${BASE_SRC}exsv.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}SVDATA.F77 \
          ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}exsv.f

extree.o${BJ}: ${BASE_SRC}extree.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}extree.f

ffert.o${BJ}:  ${BASE_SRC}ffert.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}ffert.f

ffin.o${BJ}:   ${BASE_SRC}ffin.f
	${FC} -c ${FFLAGS} ${BASE_SRC}ffin.f

filopn.o${BJ}: ${BASE_SRC}filopn.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}ECON.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}filopn.f

fndkey.o${BJ}: ${BASE_SRC}fndkey.f
	${FC} -c ${FFLAGS} ${BASE_SRC}fndkey.f

formcl.o${BJ}: ${BASE_SRC}formcl.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}formcl.f

fortyp.o${BJ}: ${BASE_SRC}fortyp.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
        ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}fortyp.f

fvsstd.o${BJ}: ${BASE_SRC}fvsstd.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
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

gradd.o${BJ}:  ${BASE_SRC}gradd.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77  \
          ${BASE_COM}PLOT.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}gradd.f

grdtim.o${BJ}: ${BASE_SRC}grdtim.f
	${FC} -c ${FFLAGS} ${BASE_SRC}grdtim.f

grincr.o${BJ}: ${BASE_SRC}grincr.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PDEN.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}STDSTK.F77 ${BASE_COM}ESHAP.F77 \
          ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}grincr.f

hbdecd.o${BJ}: ${BASE_SRC}hbdecd.f
	${FC} -c ${FFLAGS} ${BASE_SRC}hbdecd.f

htgstp.o${BJ}: ${BASE_SRC}htgstp.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}htgstp.f

iapsrt.o${BJ}: ${BASE_SRC}iapsrt.f
	${FC} -c ${FFLAGS} ${BASE_SRC}iapsrt.f

initre.o${BJ}: ${BASE_SRC}initre.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}HTCAL.F77 ${BASE_COM}ECON.F77    \
          ${BASE_COM}KEYCOM.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}VOLSTD.F77 \
          ${BASE_COM}SCREEN.F77 ${BASE_COM}SNCOM.F77 ${BASE_COM}VARCOM.F77  \
          ${BASE_COM}CWDCOM.F77 ${BASE_COM}CALCOM.F77 ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}initre.f

intree.o${BJ}: ${BASE_SRC}intree.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}HTCAL.F77    \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}STDSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}intree.f

iqrsrt.o${BJ}: ${BASE_SRC}iqrsrt.f
	${FC} -c ${FFLAGS} ${BASE_SRC}iqrsrt.f

isstag.o${BJ}: ${BASE_SRC}isstag.f ${TT_COM}PRGPRM.F77 ${BASE_COM}SSTGMC.F77
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

ksstag.o${BJ}: ${BASE_SRC}ksstag.f ${TT_COM}PRGPRM.F77 ${BASE_COM}SSTGMC.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}ksstag.f

lb1mem.o${BJ}: ${BASE_SRC}lb1mem.f
	${FC} -c ${FFLAGS} ${BASE_SRC}lb1mem.f

lbaglr.o${BJ}: ${BASE_SRC}lbaglr.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lbaglr.f

lbdset.o${BJ}: ${BASE_SRC}lbdset.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lbdset.f

lbget1.o${BJ}: ${BASE_SRC}lbget1.f
	${FC} -c ${FFLAGS} ${BASE_SRC}lbget1.f

lbintr.o${BJ}: ${BASE_SRC}lbintr.f
	${FC} -c ${FFLAGS} ${BASE_SRC}lbintr.f

lbmemr.o${BJ}: ${BASE_SRC}lbmemr.f
	${FC} -c ${FFLAGS} ${BASE_SRC}lbmemr.f

lbsplr.o${BJ}: ${BASE_SRC}lbsplr.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lbsplr.f

lbsplw.o${BJ}: ${BASE_SRC}lbsplw.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lbsplw.f

lbstrd.o${BJ}: ${BASE_SRC}lbstrd.f
	${FC} -c ${FFLAGS} ${BASE_SRC}lbstrd.f

lbtrim.o${BJ}: ${BASE_SRC}lbtrim.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lbtrim.f

lbunin.o${BJ}: ${BASE_SRC}lbunin.f
	${FC} -c ${FFLAGS} ${BASE_SRC}lbunin.f

lnkchn.o${BJ}: ${BASE_SRC}lnkchn.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lnkchn.f

lnkint.o${BJ}: ${BASE_SRC}lnkint.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}lnkint.f

main.o${BJ}:   ${BASE_SRC}main.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77  \
          ${BASE_COM}PDEN.F77 ${BASE_COM}ECON.F77 ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}main.f

mbacal.o${BJ}: ${BASE_SRC}mbacal.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77  \
          ${BASE_COM}HTCAL.F77 ${BASE_COM}PDEN.F77 ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}mbacal.f

meansd.o${BJ}: ${BASE_SRC}meansd.f
	${FC} -c ${FFLAGS} ${BASE_SRC}meansd.f

msbmrt.o${BJ}: ${BASE_SRC}msbmrt.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}msbmrt.f

mults.o${BJ}:  ${BASE_SRC}mults.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}mults.f

myopen.o${BJ}: ${BASE_SRC}myopen${PC}.f
	${FC} -c ${FFLAGS} ${BASE_SRC}myopen${PC}.f
	mv myopen${PC}.o${BJ} myopen.tmp; mv myopen.tmp myopen.o${BJ}

natcrz.o${BJ}: ${BASE_SRC}natcrz.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   ${BASE_COM}ESTREE.F77  \
	  ${BASE_COM}VARCOM.F77 ${BASE_COM}WORKCM.F77 ${BASE_COM}STDSTK.F77 \
          ${BASE_COM}SUMTAB.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}ECON.F77   \
          ${BASE_COM}OPCOM.F77  ${BASE_COM}HTCAL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}natcrz.f

notre.o${BJ}:  ${BASE_SRC}notre.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}notre.f

opadd.o${BJ}:  ${BASE_SRC}opadd.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opadd.f

opbisr.o${BJ}: ${BASE_SRC}opbisr.f
	${FC} -c ${FFLAGS} ${BASE_SRC}opbisr.f

opcact.o${BJ}: ${BASE_SRC}opcact.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77 \
        ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opcact.f

opcset.o${BJ}: ${BASE_SRC}opcset.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opcset.f

opcycl.o${BJ}: ${BASE_SRC}opcycl.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opcycl.f

opdon2.o${BJ}: ${BASE_SRC}opdon2.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opdon2.f

opeval.o${BJ}: ${BASE_SRC}opeval.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77   \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opeval.f

opexpn.o${BJ}: ${BASE_SRC}opexpn.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opexpn.f

opfind.o${BJ}: ${BASE_SRC}opfind.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opfind.f

opget2.o${BJ}: ${BASE_SRC}opget2.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opget2.f

opget3.o${BJ}: ${BASE_SRC}opget3.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opget3.f

opinit.o${BJ}: ${BASE_SRC}opinit.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opinit.f

oplist.o${BJ}: ${BASE_SRC}oplist.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}KEYCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}oplist.f

opmerg.o${BJ}: ${BASE_SRC}opmerg.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opmerg.f

opnew.o${BJ}:  ${BASE_SRC}opnew.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77    \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opnew.f

opnewc.o${BJ}: ${BASE_SRC}opnewc.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opnewc.f

oprdat.o${BJ}: ${BASE_SRC}oprdat.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77 \
        ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}oprdat.f

opsame.o${BJ}: ${BASE_SRC}opsame.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opsame.f

opsort.o${BJ}: ${BASE_SRC}opsort.f
	${FC} -c ${FFLAGS} ${BASE_SRC}opsort.f

opstus.o${BJ}: ${BASE_SRC}opstus.f ${TT_COM}PRGPRM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}opstus.f

pctile.o${BJ}: ${BASE_SRC}pctile.f
	${FC} -c ${FFLAGS} ${BASE_SRC}pctile.f

prtexm.o${BJ}: ${BASE_SRC}prtexm.f
	${FC} -c ${FFLAGS} ${BASE_SRC}prtexm.f

prtrls.o${BJ}: ${BASE_SRC}prtrls.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ESTREE.F77  \
	  ${BASE_COM}VARCOM.F77 ${BASE_COM}WORKCM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}prtrls.f

ptbal.o${BJ}:  ${BASE_SRC}ptbal.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}ptbal.f

rxdibs.o${BJ}: ${BASE_SRC}rxdibs.f
	${FC} -c ${FFLAGS} ${BASE_SRC}rxdibs.f

rann.o${BJ}:   ${BASE_SRC}rann.f ${BASE_COM}RANCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}rann.f

rcdset.o${BJ}: ${BASE_SRC}rcdset.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}rcdset.f

rdpsrt.o${BJ}: ${BASE_SRC}rdpsrt.f
	${FC} -c ${FFLAGS} ${BASE_SRC}rdpsrt.f

reass.o${BJ}:  ${BASE_SRC}reass.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}reass.f

resage.o${BJ}: ${BASE_SRC}resage.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}resage.f

revise.o${BJ}: ${BASE_SRC}revise.f
	${FC} -c ${FFLAGS} ${BASE_SRC}revise.f

scalef.o${BJ}: ${BASE_SRC}scalef.f
	${FC} -c ${FFLAGS} ${BASE_SRC}scalef.f

sdefet.o${BJ}: ${BASE_SRC}sdefet.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sdefet.f

sdefln.o${BJ}: ${BASE_SRC}sdefln.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sdefln.f

sdical.o${BJ}: ${BASE_SRC}sdical.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sdical.f

sdichk.o${BJ}: ${BASE_SRC}sdichk.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sdichk.f

setup.o${BJ}:  ${BASE_SRC}setup.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}setup.f

sgdecd.o${BJ}: ${BASE_SRC}sgdecd.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sgdecd.f

spdecd.o${BJ}: ${BASE_SRC}spdecd.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}spdecd.f

spesrt.o${BJ}: ${BASE_SRC}spesrt.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}spesrt.f

sstage.o${BJ}: ${BASE_SRC}sstage.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}SSTGMC.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sstage.f

stats.o${BJ}:  ${BASE_SRC}stats.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}VOLSTD.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}stats.f

stkval.o${BJ}: ${BASE_SRC}stkval.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}stkval.f

sumhed.o${BJ}: ${BASE_SRC}sumhed.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}SCREEN.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sumhed.f

sumout.o${BJ}: ${BASE_SRC}sumout.f ${TT_COM}PRGPRM.F77 ${BASE_COM}SUMTAB.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}sumout.f

svblkd.o${BJ}: ${BASE_SRC}svblkd.f ${TT_COM}PRGPRM.F77 ${BASE_COM}SVDATA.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svblkd.f

svcdbh.o${BJ}: ${BASE_SRC}svcdbh.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}SVDATA.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcdbh.f

svcmp1.o${BJ}: ${BASE_SRC}svcmp1.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}SVDATA.F77 ${BASE_COM}WORKCM.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcmp1.f

svcmp2.o${BJ}: ${BASE_SRC}svcmp2.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}SVDATA.F77 ${BASE_COM}WORKCM.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcmp2.f

svcmp3.o${BJ}: ${BASE_SRC}svcmp3.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}SVDATA.F77 ${BASE_COM}WORKCM.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcmp3.f

svcrol.o${BJ}: ${BASE_SRC}svcrol.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcrol.f

svcuts.o${BJ}: ${BASE_SRC}svcuts.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}SVDATA.F77 ${BASE_COM}WORKCM.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcuts.f

svcwd.o${BJ}:  ${BASE_SRC}svcwd.f    ${TT_COM}PRGPRM.F77   ${FIRE_BASE_COM}FMPARM.F77 \
               ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${FIRE_BASE_COM}FMCOM.F77  \
               ${BASE_COM}SVDATA.F77 ${BASE_COM}SVDEAD.F77 
	${FC} -c  ${FFLAGS} ${BASE_SRC}svcwd.f

svdfln.o${BJ}: ${BASE_SRC}svdfln.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svdfln.f

svestb.o${BJ}: ${BASE_SRC}svestb.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}SVDATA.F77 \
          ${BASE_COM}SVDEAD.F77 ${BASE_COM}WORKCM.F77 ${BASE_COM}METRIC.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svestb.f

svgrnd.o${BJ}: ${BASE_SRC}svgrnd.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}SVDATA.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svgrnd.f

svgtpl.o${BJ}: ${BASE_SRC}svgtpl.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}SVDATA.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svgtpl.f

svgtpt.o${BJ}: ${BASE_SRC}svgtpt.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svgtpt.f

svhabt.o${BJ}: ${BASE_SRC}svhabt.f ${TT_COM}PRGPRM.F77 ${BASE_COM}SVDEAD.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svhabt.f

svinit.o${BJ}: ${BASE_SRC}svinit.f ${TT_COM}PRGPRM.F77 ${BASE_COM}SVDATA.F77 \
          ${BASE_COM}SVDEAD.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svinit.f

svkey.o${BJ}:  ${BASE_SRC}svkey.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}SVDATA.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svkey.f

svlcol.o${BJ}: ${BASE_SRC}svlcol.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svlcol.f

svlnol.o${BJ}: ${BASE_SRC}svlnol.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svlnol.f

svlsol.o${BJ}: ${BASE_SRC}svlsol.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svlsol.f

svmort.o${BJ}: ${BASE_SRC}svmort.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}SVDATA.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svmort.f

svntr.o${BJ}:  ${BASE_SRC}svntr.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svntr.f

svobol.o${BJ}: ${BASE_SRC}svobol.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svobol.f

svonln.o${BJ}: ${BASE_SRC}svonln.f
	${FC} -c  ${FFLAGS} ${BASE_SRC}svonln.f

svout.o${BJ}: ${BASE_SRC}svout.f    ${TT_COM}PRGPRM.F77   ${FIRE_BASE_COM}FMPARM.F77 \
              ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${FIRE_BASE_COM}FMCOM.F77 \
              ${BASE_COM}PLOT.F77   ${BASE_COM}SVDATA.F77 ${BASE_COM}SVDEAD.F77 \
              ${BASE_COM}METRIC.F77
	${FC} -c  -qnoescape ${FFLAGS} ${BASE_SRC}svout.f

svrann.o${BJ}: ${BASE_SRC}svrann.f ${BASE_COM}SVRCOM.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}svrann.f

svrmov.o${BJ}: ${BASE_SRC}svrmov.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}SVDATA.F77 \
          ${BASE_COM}SVDEAD.F77 ${BASE_COM}METRIC.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svrmov.f

svstart.o${BJ}: ${BASE_SRC}svstart.f  ${TT_COM}PRGPRM.F77   ${FIRE_BASE_COM}FMPARM.F77 \
                ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 ${FIRE_BASE_COM}FMCOM.F77 \
                ${BASE_COM}SVDATA.F77 ${BASE_COM}RANCOM.F77 ${BASE_COM}SVRCOM.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svstart.f

svsnad.o${BJ}: ${BASE_SRC}svsnad.f ${TT_COM}PRGPRM.F77 ${BASE_COM}SVDEAD.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}SVDATA.F77 \
          ${BASE_COM}CONTRL.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svsnad.f

svsnage.o${BJ}: \
${BASE_SRC}svsnage.f \
${TT_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}SVDEAD.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}PLOT.F77 \
${BASE_COM}CONTRL.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svsnage.f

svtdel.o${BJ}: ${BASE_SRC}svtdel.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}SVDATA.F77 ${BASE_COM}WORKCM.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svtdel.f

svtrip.o${BJ}: ${BASE_SRC}svtrip.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}SVDATA.F77
	${FC} -c  ${FFLAGS} ${BASE_SRC}svtrip.f

svsalv.o${BJ}: \
${BASE_SRC}svsalv.f \
${TT_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${FIRE_BASE_COM}FMCOM.F77 \
${BASE_COM}SVDATA.F77 \
${BASE_COM}SVDEAD.F77 \

	${FC} -c  ${FFLAGS} ${BASE_SRC}svsalv.f

tredel.o${BJ}: ${BASE_SRC}tredel.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ESTREE.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}tredel.f

tregro.o${BJ}: ${BASE_SRC}tregro.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}tregro.f

tremov.o${BJ}: ${BASE_SRC}tremov.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}STDSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}tremov.f

tresor.o${BJ}: ${BASE_SRC}tresor.f
	${FC} -c ${FFLAGS} ${BASE_SRC}tresor.f

triple.o${BJ}: ${BASE_SRC}triple.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ESTREE.F77 ${BASE_COM}STDSTK.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}triple.f

trnasp.o${BJ}: ${BASE_SRC}trnasp.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}trnasp.f

trnslo.o${BJ}: ${BASE_SRC}trnslo.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}trnslo.f

tvalue.o${BJ}: ${BASE_SRC}tvalue.f
	${FC} -c ${FFLAGS} ${BASE_SRC}tvalue.f

unblnk.o${BJ}: ${BASE_SRC}unblnk.f
	${FC} -c ${FFLAGS} ${BASE_SRC}unblnk.f

upcase.o${BJ}: ${BASE_SRC}upcase.f
	${FC} -c ${FFLAGS} ${BASE_SRC}upcase.f

update.o${BJ}: ${BASE_SRC}update.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}update.f

upkey.o${BJ}:  ${BASE_SRC}upkey.f
	${FC} -c ${FFLAGS} ${BASE_SRC}upkey.f

volkey.o${BJ}: ${BASE_SRC}volkey.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}VOLSTD.F77
	${FC} -c ${FFLAGS} ${BASE_SRC}volkey.f

# compile the STRP source directory routines

esaddt.o${BJ}:  ${STRP_SRC}esaddt.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esaddt.f

esblkd.o${BJ}: ${STRP_SRC}esblkd.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ESRNCM.F77   \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esblkd.f

escprs.o${BJ}: ${STRP_SRC}escprs.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}ESHAP.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}OUTCOM.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}escprs.f

esetpr.o${BJ}: ${STRP_SRC}esetpr.f
	${FC} -c ${FFLAGS} ${STRP_SRC}esetpr.f

esfltr.o${BJ}: ${STRP_SRC}esfltr.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${TT_COM}ESPARM.F77   \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}PDEN.F77    \
          ${BASE_COM}CALDEN.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esfltr.f

esgent.o${BJ}: ${STRP_SRC}esgent.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${TT_COM}ESPARM.F77 ${TT_COM}ESCOMN.F77 \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esgent.f

esin.o${BJ}: ${STRP_SRC}esin.f ${TT_COM}PRGPRM.F77 ${TT_COM}ESPARM.F77 \
        $(BASE_COM)ESCOMN.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}CONTRL.F77 \
        ${BASE_COM}PLOT.F77 ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esin.f

esinit.o${BJ}: ${STRP_SRC}esinit.f ${TT_COM}PRGPRM.F77 ${TT_COM}ESPARM.F77   \
          ${BASE_COM}ESHAP.F77 ${BASE_COM}ESRNCM.F77 ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esinit.f

esmsgs.o${BJ}: ${STRP_SRC}esmsgs.f
	${FC} -c ${FFLAGS} ${STRP_SRC}esmsgs.f

esnutr.o${BJ}: ${STRP_SRC}esnutr.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esnutr.f

esout.o${BJ}:  ${STRP_SRC}esout.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esout.f

esplt1.o${BJ}: ${STRP_SRC}esplt1.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esplt1.f

esplt2.o${BJ}: ${STRP_SRC}esplt2.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esplt2.f

esprep.o${BJ}: ${STRP_SRC}esprep.f ${TT_COM}PRGPRM.F77 ${TT_COM}ESPARM.F77   \
          ${TT_COM}ESCOMN.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esprep.f

esprin.o${BJ}: ${STRP_SRC}esprin.f
	${FC} -c ${FFLAGS} ${STRP_SRC}esprin.f

esrann.o${BJ}: ${STRP_SRC}esrann.f ${BASE_COM}ESRNCM.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esrann.f

essprt.o${BJ}:  ${STRP_SRC}essprt.f
	${FC} -c ${FFLAGS} ${STRP_SRC}essprt.f

estab.o${BJ}:  ${STRP_SRC}estab.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77       \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${TT_COM}ESPARM.F77    \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}ESHAP.F77 ${BASE_COM}PDEN.F77   \
          ${BASE_COM}CALDEN.F77 ${TT_COM}ESCOMN.F77 ${BASE_COM}VARCOM.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}estab.f

estime.o${BJ}: ${STRP_SRC}estime.f ${TT_COM}PRGPRM.F77 ${TT_COM}ESPARM.F77   \
          ${TT_COM}ESCOMN.F77 ${BASE_COM}ESWSBW.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}estime.f

estump.o${BJ}: ${STRP_SRC}estump.f ${TT_COM}PRGPRM.F77 ${TT_COM}ESPARM.F77   \
          ${BASE_COM}ESHOOT.F77 ${BASE_COM}ESHAP.F77 ${TT_COM}ESCOMN.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}estump.f

esuckr.o${BJ}: ${STRP_SRC}esuckr.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${TT_COM}ESPARM.F77   \
          ${BASE_COM}ESHOOT.F77 ${TT_COM}ESCOMN.F77 ${BASE_COM}ESHAP.F77  \
          ${BASE_COM}ESTREE.F77 ${BASE_COM}STDSTK.F77 ${BASE_COM}VARCOM.F77 \
          ${BASE_COM}COEFFS.F77
	${FC} -c ${FFLAGS} ${STRP_SRC}esuckr.f

# compile the MISTOE source routine

miscnt.o${BJ}: ${MIST_SRC}miscnt.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}miscnt.f

miscpf.o${BJ}: ${MIST_SRC}miscpf.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}miscpf.f

misdam.o${BJ}: ${MIST_SRC}misdam.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misdam.f

misdgf.o${BJ}: ${MIST_SRC}misdgf.f ${TT_COM}PRGPRM.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misdgf.f

misget.o${BJ}: ${MIST_SRC}misget.f ${TT_COM}PRGPRM.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misget.f

mishgf.o${BJ}: ${MIST_SRC}mishgf.f ${TT_COM}PRGPRM.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}mishgf.f

misin.o${BJ}:  ${MIST_SRC}misin.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77     \
          ${MIST_SRC}MISCOM.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misin.f

misinf.o${BJ}: ${MIST_SRC}misinf.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misinf.f

misin0.o${BJ}: ${MIST_SRC}misin0.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misin0.f

misinttt.o${BJ}: ${MIST_SRC}misinttt.f ${TT_COM}PRGPRM.F77    \
            ${BASE_COM}CONTRL.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misinttt.f


mismrt.o${BJ}: ${MIST_SRC}mismrt.f ${TT_COM}PRGPRM.F77    \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}mismrt.f

misprt.o${BJ}: ${MIST_SRC}misprt.f ${TT_COM}PRGPRM.F77    \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misprt.f

misputz.o${BJ}: ${MIST_SRC}misputz.f ${TT_COM}PRGPRM.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misputz.f

misran.o${BJ}: ${MIST_SRC}misran.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}misran.f

mistoe.o${BJ}: ${MIST_SRC}mistoe.f ${TT_COM}PRGPRM.F77    \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${MIST_SRC}MISCOM.F77  \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}mistoe.f


# Douglas-fir Beetle

dfbdam.o${BJ}:   ${DFB_SRC}dfbdam.f ${TT_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbdam.f

dfbdbh.o${BJ}:   ${DFB_SRC}dfbdbh.f ${TT_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77   \
            ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbdbh.f

dfbdrv.o${BJ}:   ${DFB_SRC}dfbdrv.f ${TT_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbdrv.f

dfber.o${BJ}:    ${DFB_SRC}dfber.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
            ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfber.f

dfbgo.o${BJ}:    ${DFB_SRC}dfbgo.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
            ${BASE_COM}CONTRL.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbgo.f

dfbhed.o${BJ}:   ${DFB_SRC}dfbhed.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77    \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbhed.f

dfbin.o${BJ}:    ${DFB_SRC}dfbin.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbin.f

dfbind.o${BJ}:   ${DFB_SRC}dfbind.f
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbind.f

dfbint.o${BJ}:   ${DFB_SRC}dfbint.f ${TT_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbint.f

dfbinv.o${BJ}:   ${DFB_SRC}dfbinv.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbinv.f

dfblkdtt.o${BJ}: ${DFB_SRC}dfblkdtt.f ${TT_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfblkdtt.f

dfbmod.o${BJ}:   ${DFB_SRC}dfbmod.f ${TT_COM}PRGPRM.F77 ${DFB_COM}DFBCOM.F77    \
            ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbmod.f

dfbmrt.o${BJ}:   ${DFB_SRC}dfbmrt.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
            ${BASE_COM}CONTRL.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbmrt.f

dfbout.o${BJ}:   ${DFB_SRC}dfbout.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbout.f

dfbprb.o${BJ}:   ${DFB_SRC}dfbprb.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbprb.f

dfbran.o${BJ}:   ${DFB_SRC}dfbran.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbran.f

dfbsch.o${BJ}:   ${DFB_SRC}dfbsch.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77  \
            ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbsch.f

dfbtab.o${BJ}:   ${DFB_SRC}dfbtab.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
            ${BASE_COM}CONTRL.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbtab.f

dfbwin.o${BJ}:   ${DFB_SRC}dfbwin.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
            ${BASE_COM}CONTRL.F77 ${DFB_COM}DFBCOM.F77
	${FC} -c ${FFLAGS} ${DFB_SRC}dfbwin.f

# Douglas-fir Tussock Moth

dfole8.o${BJ}: ${DFTM_SRC}dfole8.f ${DFTM_SRC}LIMITS.F77 ${DFTM_SRC}DFOL.F77     \
          ${DFTM_SRC}ICOND.F77
	${FC} -c ${FFLAGS} ${DFTM_SRC}dfole8.f

dftmgo.o${BJ}: ${DFTM_SRC}dftmgo.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${DFTM_SRC}TMCOM1.F77
	${FC} -c ${FFLAGS} ${DFTM_SRC}dftmgo.f

dftmin.o${BJ}: ${DFTM_SRC}dftmin.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
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

gfcomp.o${BJ}: ${DFTM_SRC}gfcomp.f ${DFTM_SRC}LOWER.F77 ${DFTM_SRC}LIMITS.F77
	${FC} -c ${FFLAGS} ${DFTM_SRC}gfcomp.f

# see note at dftm object list above
#garbel.o${BJ}: ${DFTM_SRC}garbel.f
# 	${FC} -c ${FFLAGS} ${DFTM_SRC}garbel.f
#
#grclas.o${BJ}: ${DFTM_SRC}grclas.f
#	${FC} -c ${FFLAGS} ${DFTM_SRC}grclas.f
#
#grpsum.o${BJ}: ${DFTM_SRC}grpsum.f
#	${FC} -c ${FFLAGS} ${DFTM_SRC}grpsum.f

inscyc.o${BJ}: ${DFTM_SRC}inscyc.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${DFTM_SRC}inscyc.f

redist.o${BJ}: ${DFTM_SRC}redist.f ${DFTM_SRC}ICOND.F77 ${DFTM_SRC}GPASS.F77     \
          ${DFTM_SRC}UPPER.F77
	${FC} -c ${FFLAGS} ${DFTM_SRC}redist.f

tmbchl.o${BJ}: ${DFTM_SRC}tmbchl.f
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmbchl.f

tmbmas.o${BJ}: ${DFTM_SRC}tmbmas.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${DFTM_SRC}TMCOM1.F77 ${BASE_COM}PLOT.F77   \
          ${DFTM_SRC}BIOMAS.F77
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmbmas.f

tmcoup.o${BJ}: ${DFTM_SRC}tmcoup.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
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

tminitbm.o${BJ}: ${DFTM_SRC}tminitbm.f ${DFTM_SRC}TMCOM1.F77 ${DFTM_SRC}TMEGGS.F77 \
            ${DFTM_SRC}UPPER.F77 ${DFTM_SRC}LOWER.F77
	${FC} -c ${FFLAGS} ${DFTM_SRC}tminitbm.f

tminitec.o${BJ}: ${DFTM_SRC}tminitec.f ${DFTM_SRC}TMCOM1.F77 ${DFTM_SRC}TMEGGS.F77 \
            ${DFTM_SRC}UPPER.F77 ${DFTM_SRC}LOWER.F77
	${FC} -c ${FFLAGS} ${DFTM_SRC}tminitec.f

tmotpr.o${BJ}: ${DFTM_SRC}tmotpr.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${DFTM_SRC}TMCOM1.F77
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmotpr.f

tmout.o${BJ}: ${DFTM_SRC}tmout.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77      \
         ${BASE_COM}PLOT.F77 ${DFTM_SRC}TMCOM1.F77
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmout.f

tmrann.o${BJ}: ${DFTM_SRC}tmrann.f
	${FC} -c ${FFLAGS} ${DFTM_SRC}tmrann.f

tmschd.o${BJ}: ${DFTM_SRC}tmschd.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
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

# Lodgepole Mountain Pine Beetle

back.o${BJ}:   ${MPB_SRC}back.f ${MPB_SRC}MPBETA.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}back.f

betin.o${BJ}:  ${MPB_SRC}betin.f ${MPB_SRC}MPBETA.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}betin.f

coldbh.o${BJ}: ${MPB_SRC}coldbh.f ${TT_COM}PRGPRM.F77 ${MPB_SRC}COLCOM.F77    \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}coldbh.f

coldrv.o${BJ}: ${MPB_SRC}coldrv.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77     \
          ${BASE_COM}CONTRL.F77 ${MPB_SRC}COLCOM.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}coldrv.f

colind.o${BJ}: ${MPB_SRC}colind.f
	${FC} -c ${FFLAGS} ${MPB_SRC}colind.f

colmod.o${BJ}: ${MPB_SRC}colmod.f ${TT_COM}PRGPRM.F77 ${MPB_SRC}COLCOM.F77    \
          ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}colmod.f

colmrt.o${BJ}: ${MPB_SRC}colmrt.f ${TT_COM}PRGPRM.F77 ${MPB_SRC}MPBCOM.F77    \
          ${MPB_SRC}COLCOM.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}colmrt.f

emerg.o${BJ}:  ${MPB_SRC}emerg.f
	${FC} -c ${FFLAGS} ${MPB_SRC}emerg.f

forw.o${BJ}:   ${MPB_SRC}forw.f
	${FC} -c ${FFLAGS} ${MPB_SRC}forw.f

garbel.o${BJ}: ${MPB_SRC}garbel.f
	${FC} -c ${FFLAGS} ${MPB_SRC}garbel.f

grclas.o${BJ}: ${MPB_SRC}grclas.f
	${FC} -c ${FFLAGS} ${MPB_SRC}grclas.f

grpsum.o${BJ}: ${MPB_SRC}grpsum.f
	${FC} -c ${FFLAGS} ${MPB_SRC}grpsum.f

mpbcup.o${BJ}: ${MPB_SRC}mpbcup.f ${TT_COM}PRGPRM.F77 ${MPB_SRC}MPBCOM.F77    \
          ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbcup.f

mpbdam.o${BJ}: ${MPB_SRC}mpbdam.f ${TT_COM}PRGPRM.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbdam.f

mpbdrv.o${BJ}: ${MPB_SRC}mpbdrv.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbdrv.f

mpber.o${BJ}:  ${MPB_SRC}mpber.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpber.f

mpbgam.o${BJ}: ${MPB_SRC}mpbgam.f
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbgam.f

mpbgo.o${BJ}:  ${MPB_SRC}mpbgo.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbgo.f

mpbhed.o${BJ}: ${MPB_SRC}mpbhed.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77     \
          ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbhed.f

mpbin.o${BJ}:  ${MPB_SRC}mpbin.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77 ${MPB_SRC}COLCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbin.f

mpbint.o${BJ}: ${MPB_SRC}mpbint.f ${TT_COM}PRGPRM.F77 ${MPB_SRC}MPBCOM.F77    \
          ${MPB_SRC}COLCOM.F77 ${MPB_SRC}MPBETA.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbint.f

mpblkdtt.o${BJ}: ${MPB_SRC}mpblkdtt.f ${TT_COM}PRGPRM.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpblkdtt.f

mpbmod.o${BJ}: ${MPB_SRC}mpbmod.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbmod.f

mpbout.o${BJ}: ${MPB_SRC}mpbout.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77     \
          ${BASE_COM}CONTRL.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbout.f

mpbtab.o${BJ}: ${MPB_SRC}mpbtab.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${MPB_SRC}MPBCOM.F77 ${MPB_SRC}COLCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpbtab.f

mpgr.o${BJ}:   ${MPB_SRC}mpgr.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CONTRL.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpgr.f

mpotpr.o${BJ}: ${MPB_SRC}mpotpr.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpotpr.f

mprann.o${BJ}: ${MPB_SRC}mprann.f
	${FC} -c ${FFLAGS} ${MPB_SRC}mprann.f

mpsdlp.o${BJ}: ${MPB_SRC}mpsdlp.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${MPB_SRC}MPBCOM.F77 ${MPB_SRC}COLCOM.F77   \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}mpsdlp.f

percnt.o${BJ}: ${MPB_SRC}percnt.f
	${FC} -c ${FFLAGS} ${MPB_SRC}percnt.f

pmslp.o${BJ}:  ${MPB_SRC}pmslp.f
	${FC} -c ${FFLAGS} ${MPB_SRC}pmslp.f

pqsml.o${BJ}:  ${MPB_SRC}pqsml.f ${MPB_SRC}MPBETA.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}pqsml.f

ptary.o${BJ}:  ${MPB_SRC}ptary.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptary.f

ptbno.o${BJ}:  ${MPB_SRC}ptbno.f
	${FC} -c ${FFLAGS} ${MPB_SRC}ptbno.f

ptgrp.o${BJ}:  ${MPB_SRC}ptgrp.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptgrp.f

ptint.o${BJ}:  ${MPB_SRC}ptint.f
	${FC} -c ${FFLAGS} ${MPB_SRC}ptint.f

ptplt.o${BJ}:  ${MPB_SRC}ptplt.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptplt.f

ptsav.o${BJ}:  ${MPB_SRC}ptsav.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptsav.f

ptscl.o${BJ}:  ${MPB_SRC}ptscl.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptscl.f

ptsrg.o${BJ}:  ${MPB_SRC}ptsrg.f
	${FC} -c ${FFLAGS} ${MPB_SRC}ptsrg.f

ptssc.o${BJ}:  ${MPB_SRC}ptssc.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptssc.f

ptsym.o${BJ}:  ${MPB_SRC}ptsym.f ${MPB_SRC}PT.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}ptsym.f

surfce.o${BJ}: ${MPB_SRC}surfce.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}surfce.f

surflp.o${BJ}: ${MPB_SRC}surflp.f
	${FC} -c ${FFLAGS} ${MPB_SRC}surflp.f

tafit.o${BJ}:  ${MPB_SRC}tafit.f ${TT_COM}PRGPRM.F77 ${MPB_SRC}MPBCOM.F77
	${FC} -c ${FFLAGS} ${MPB_SRC}tafit.f

# Root Disease V3.0

rdarea.o${BJ}: ${RD_SRC}rdarea.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdarea.f

rdatv.o${BJ}: ${RD_SRC}rdatv.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdatv.f

rdbb1.o${BJ}: ${RD_SRC}rdbb1.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdbb1.f

rdbb2.o${BJ}: ${RD_SRC}rdbb2.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
         ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdbb2.f

rdbb3.o${BJ}: ${RD_SRC}rdbb3.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdbb3.f

rdbb4.o${BJ}: ${RD_SRC}rdbb4.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdbb4.f

rdbbdo.o${BJ}: ${RD_SRC}rdbbdo.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdbbdo.f

rdblk1tt.o${BJ}: ${RD_SRC}rdblk1tt.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}METRIC.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdblk1tt.f

rdbout.o${BJ}: ${RD_SRC}rdbout.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDADD.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}METRIC.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdbout.f

rdcent.o${BJ}: ${RD_SRC}rdcent.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${RD_SRC}RDPARM.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdcent.f

rdcloc.o${BJ}: ${RD_SRC}rdcloc.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdcloc.f

rdcmpr.o${BJ}: ${RD_SRC}rdcmpr.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${BASE_COM}CONTRL.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdcmpr.f

rdcntl.o${BJ}: ${RD_SRC}rdcntl.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdcntl.f

rdcsd.o${BJ}: ${RD_SRC}rdcsd.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdcsd.f

rddam.o${BJ}: ${RD_SRC}rddam.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}HTCAL.F77 \
         ${BASE_COM}ESTREE.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rddam.f

rddbug.o${BJ}: ${RD_SRC}rddbug.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rddbug.f

rddout.o${BJ}: ${RD_SRC}rddout.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77 \
          ${BASE_COM}METRIC.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rddout.f

rddst.o${BJ}: ${RD_SRC}rddst.f ${RD_SRC}RDPARM.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rddst.f

rdend.o${BJ}: ${RD_SRC}rdend.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
         ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdend.f

rdescp.o${BJ}: ${RD_SRC}rdescp.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdescp.f

rdesin.o${BJ}: ${RD_SRC}rdesin.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
          ${BASE_COM}ESHAP.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdesin.f

rdestb.o${BJ}: ${RD_SRC}rdestb.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 ${RD_SRC}RDARRY.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdestb.f

rdgrow.o${BJ}: ${RD_SRC}rdgrow.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdgrow.f

rdintt.o${BJ}: ${RD_SRC}rdintt.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
        ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77  \
        ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77  \
        ${BASE_COM}METRIC.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdintt.f

rdinf.o${BJ}: ${RD_SRC}rdinf.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
         ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdinf.f

rdinit.o${BJ}: ${RD_SRC}rdinit.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}METRIC.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}PLOT.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdinit.f

rdinoc.o${BJ}: ${RD_SRC}rdinoc.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77 ${RD_SRC}RDCOM.F77 \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdinoc.f

rdinsd.o${BJ}: ${RD_SRC}rdinsd.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdinsd.f

rdinup.o${BJ}: ${RD_SRC}rdinup.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdinup.f

rdiout.o${BJ}: ${RD_SRC}rdiout.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
          ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdiout.f

rdiprp.o${BJ}: ${RD_SRC}rdiprp.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdiprp.f

rdjump.o${BJ}: ${RD_SRC}rdjump.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdjump.f

rdload.o${BJ}: ${RD_SRC}rdload.f
	${FC} -c ${FFLAGS} ${RD_SRC}rdload.f

rdmn1.o${BJ}: ${RD_SRC}rdmn1.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}OUTCOM.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdmn1.f

rdmn2.o${BJ}: ${RD_SRC}rdmn2.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCRY.F77 \
         ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdmn2.f

rdmort.o${BJ}: ${RD_SRC}rdmort.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdmort.f

rdmrec.o${BJ}: ${RD_SRC}rdmrec.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdmrec.f

rdoagm.o${BJ}: ${RD_SRC}rdoagm.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDADD.F77 ${RD_SRC}RDARRY.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdoagm.f

rdowi.o${BJ}: ${RD_SRC}rdowi.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdowi.f

rdowin.o${BJ}: ${RD_SRC}rdowin.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdowin.f

rdpr.o${BJ}: ${RD_SRC}rdpr.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
        ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 \
        ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
        ${RD_SRC}RDADD.F77 ${BASE_COM}METRIC.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdpr.f

rdprin.o${BJ}: ${RD_SRC}rdprin.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdprin.f

rdpush.o${BJ}: ${RD_SRC}rdpush.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdpush.f

rdrani.o${BJ}: ${RD_SRC}rdrani.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdrani.f

rdrann.o${BJ}: ${RD_SRC}rdrann.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdrann.f

rdranp.o${BJ}: ${RD_SRC}rdranp.f
	${FC} -c ${FFLAGS} ${RD_SRC}rdranp.f

rdrate.o${BJ}: ${RD_SRC}rdrate.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdrate.f

rdrdel.o${BJ}: ${RD_SRC}rdrdel.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}OUTCOM.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdrdel.f

rdroot.o${BJ}: ${RD_SRC}rdroot.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}PLOT.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdroot.f

rdrout.o${BJ}: ${RD_SRC}rdrout.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdrout.f

rdsadd.o${BJ}: ${RD_SRC}rdsadd.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdsadd.f

rdsetp.o${BJ}: ${RD_SRC}rdsetp.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdsetp.f

rdshrk.o${BJ}: ${RD_SRC}rdshrk.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdshrk.f

rdshst.o${BJ}: ${RD_SRC}rdshst.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${RD_SRC}RDCOM.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdshst.f

rdslp.o${BJ}: ${RD_SRC}rdslp.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdslp.f

rdsout.o${BJ}: ${RD_SRC}rdsout.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDCRY.F77 \
          ${RD_SRC}RDADD.F77 ${BASE_COM}METRIC.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdsout.f

rdspl1.o${BJ}: ${RD_SRC}rdspl1.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdspl1.f

rdspl2.o${BJ}: ${RD_SRC}rdspl2.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdspl2.f

rdspor.o${BJ}: ${RD_SRC}rdspor.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdspor.f

rdsprd.o${BJ}: ${RD_SRC}rdsprd.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdsprd.f

rdspup.o${BJ}: ${RD_SRC}rdspup.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdspup.f

rdssiz.o${BJ}: ${RD_SRC}rdssiz.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdssiz.f

rdstp.o${BJ}: ${RD_SRC}rdstp.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdstp.f

rdstr.o${BJ}: ${RD_SRC}rdstr.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdstr.f

rdsum.o${BJ}: ${RD_SRC}rdsum.f ${RD_SRC}RDPARM.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdsum.f

rdtdel.o${BJ}: ${RD_SRC}rdtdel.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 \
          ${RD_SRC}RDARRY.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdtdel.f

rdtim.o${BJ}: ${RD_SRC}rdtim.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 \
         ${BASE_COM}CONTRL.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdtim.f

rdtout.o${BJ}: ${RD_SRC}rdtout.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDARRY.F77 \
          ${RD_SRC}RDADD.F77 ${BASE_COM}METRIC.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdtout.f

rdtreg.o${BJ}: ${RD_SRC}rdtreg.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 \
          ${RD_SRC}RDARRY.F77 ${RD_SRC}RDCRY.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdtreg.f

rdtres.o${BJ}: ${RD_SRC}rdtres.f ${RD_SRC}RDPARM.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdtres.f

rdtrip.o${BJ}: ${RD_SRC}rdtrip.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${RD_SRC}RDCOM.F77 ${RD_SRC}RDADD.F77 \
          ${RD_SRC}RDARRY.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdtrip.f

rdtrp.o${BJ}: ${RD_SRC}rdtrp.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
         ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdtrp.f

rdzero.o${BJ}: ${RD_SRC}rdzero.f ${TT_COM}PRGPRM.F77 ${RD_SRC}RDPARM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${RD_SRC}RDCOM.F77 \
          ${RD_SRC}RDARRY.F77 ${BASE_COM}ARRAYS.F77 ${RD_SRC}RDADD.F77
	${FC} -c ${FFLAGS} ${RD_SRC}rdzero.f

# GenDefol (western spruce budworm)

bweadv.o${BJ}: ${BW_SRC}bweadv.f ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bweadv.f
  
bweage.o${BJ}: ${BW_SRC}bweage.f  ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bweage.f 

bwebet.o${BJ}: ${BW_SRC}bwebet.f
	${FC} -c ${FFLAGS} ${BW_SRC}bwebet.f 

bwebktt.o${BJ}: ${BW_SRC}bwebktt.f  ${TT_COM}PRGPRM.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWEBOX.F77 ${BW_SRC}BWECM2.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwebktt.f

bwebmstt.o${BJ}: ${BW_SRC}bwebmstt.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
               ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwebmstt.f

bwecrc.o${BJ}: ${BW_SRC}bwecrc.f
	${FC} -c ${FFLAGS} ${BW_SRC}bwecrc.f

bwecup.o${BJ}: ${BW_SRC}bwecup.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
               ${BASE_COM}CONTRL.F77 ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwecup.f

bwedam.o${BJ}: ${BW_SRC}bwedam.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 
	${FC} -c ${FFLAGS} ${BW_SRC}bwedam.f

bwedef.o${BJ}: ${BW_SRC}bwedef.f ${TT_COM}PRGPRM.F77 ${BW_SRC}BWESTD.F77 \
               ${BW_SRC}BWECOM.F77 
	${FC} -c ${FFLAGS} ${BW_SRC}bwedef.f

bwedie.o${BJ}: ${BW_SRC}bwedie.f ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwedie.f

bwedr.o${BJ}: ${BW_SRC}bwedr.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
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

bwego.o${BJ}: ${BW_SRC}bwego.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
              ${BASE_COM}PLOT.F77 ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
              ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwego.f

bwein.o${BJ}: ${BW_SRC}bwein.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
              ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
              ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwein.f

bweint.o${BJ}: ${BW_SRC}bweint.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bweint.f

bwekdp.o${BJ}: ${BW_SRC}bwekdp.f
	${FC} -c ${FFLAGS} ${BW_SRC}bwekdp.f

bwekey.o${BJ}: ${BW_SRC}bwekey.f ${BW_SRC}BWECOM.F77 
	${FC} -c ${FFLAGS} ${BW_SRC}bwekey.f

bwekrd.o${BJ}: ${BW_SRC}bwekrd.f
	${FC} -c ${FFLAGS} ${BW_SRC}bwekrd.f

bwelit.o${BJ}: ${BW_SRC}bwelit.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 \
               ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwelit.f

bweob.o${BJ}: ${BW_SRC}bweob.f ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bweob.f

bweout.o${BJ}: ${BW_SRC}bweout.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
               ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bweout.f

bwep1.o${BJ}: ${BW_SRC}bwep1.f ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwep1.f

bwep2.o${BJ}: ${BW_SRC}bwep2.f ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwep2.f

bwep3.o${BJ}: ${BW_SRC}bwep3.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
              ${BW_SRC}BWECOM.F77 ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwep3.f

bwep4.o${BJ}: ${BW_SRC}bwep4.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
               ${BASE_COM}CONTRL.F77 ${BW_SRC}BWECM2.F77 ${BW_SRC}BWEBOX.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwep4.f

bwepdm.o${BJ}: ${BW_SRC}bwepdm.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
               ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 \
               ${BW_SRC}BWECOM.F77 ${BW_SRC}BWESTD.F77 ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS}${I2}${MIST_SRC} ${BW_SRC}bwepdm.f

bweprb.o${BJ}: ${BW_SRC}bweprb.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
               ${BASE_COM}CONTRL.F77 ${BW_SRC}BWESTD.F77 ${BW_SRC}BWECOM.F77 
	${FC} -c ${FFLAGS} ${BW_SRC}bweprb.f

bweran.o${BJ}: ${BW_SRC}bweran.f
	${FC} -c ${FFLAGS} ${BW_SRC}bweran.f

bwernp.o${BJ}: ${BW_SRC}bwernp.f
	${FC} -c ${FFLAGS} ${BW_SRC}bwernp.f

bwesin.o${BJ}: ${BW_SRC}bwesin.f ${TT_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${BW_SRC}bwesin.f

bwesit.o${BJ}: ${BW_SRC}bwesit.f ${TT_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77 \
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

# Multiple pest link notification from WSBWE source

fxnote.o${BJ}: ${BW_SRC}txnote.f
	${FC} -c ${FFLAGS} ${BW_SRC}fxnote.f

txnote.o${BJ}: ${BW_SRC}txnote.f
	${FC} -c ${FFLAGS} ${BW_SRC}txnote.f

# Econ extension compile instructions for the TT variant

eccalc.o${BJ}: ${ECON_SRC}eccalc.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}eccalc.f

echarv.o${BJ}: ${ECON_SRC}echarv.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}echarv.f

ecin.o${BJ}: ${ECON_SRC}ecin.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecin.f

ecinit.o${BJ}: ${ECON_SRC}ecinit.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecinit.f

ecsetp.o${BJ}: ${ECON_SRC}ecsetp.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecsetp.f

ecstatus.o${BJ}: ${ECON_SRC}ecstatus.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecstatus.f

ecvol.o${BJ}: ${ECON_SRC}ecvol.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${ECON_SRC}ecvol.f

#  Cover extension compile instructions for the TT vraiant

cvbcal.o${BJ}: ${COVR_SRC}cvbcal.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvbcal.f

cvbrow.o${BJ}: ${COVR_SRC}cvbrow.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvbrow.f

cvcbms.o${BJ}: ${COVR_SRC}cvcbms.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvcbms.f

cvclas.o${BJ}: ${COVR_SRC}cvclas.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvclas.f

cvcnop.o${BJ}: ${COVR_SRC}cvcnop.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvcnop.f

cvcw.o${BJ}:   ${COVR_SRC}cvcw.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77     \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvcw.f

cvgo.o${BJ}:   ${COVR_SRC}cvgo.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvgo.f

cvin.o${BJ}:   ${COVR_SRC}cvin.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77     \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvin.f

cvinit.o${BJ}: ${COVR_SRC}cvinit.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvinit.f

cvnohd.o${BJ}: ${COVR_SRC}cvnohd.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}CVCOM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}OUTCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvnohd.f

cvout.o${BJ}:  ${COVR_SRC}cvout.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77    \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvout.f

cvscon.o${BJ}: ${COVR_SRC}cvscon.f ${TT_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77 
	${FC} -c ${FFLAGS} ${COVR_SRC}cvscon.f

cvshap.o${BJ}: ${COVR_SRC}cvshap.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77   \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvshap.f

cvsum.o${BJ}:  ${COVR_SRC}cvsum.f ${TT_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${COVR_SRC}cvsum.f

# compile routines from the BGC source directory

babsrad.o${BJ}: ${BGC_SRC}babsrad.f   ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}babsrad.f  

balloca.o${BJ}: ${BGC_SRC}balloca.f   ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}balloca.f   

bark.o${BJ}: ${BGC_SRC}bark.f
	${FC} -c ${FFLAGS} ${BGC_SRC}bark.f

bcrown.o${BJ}: ${BGC_SRC}bcrown.f    ${BGC_SRC}ENTITY.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bcrown.f

bentyload.o${BJ}: ${BGC_SRC}bentyload.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bentyload.f 

bentyupdt.o${BJ}: ${BGC_SRC}bentyupdt.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bentyupdt.f   

bgcfvs.o${BJ}: ${BGC_SRC}bgcfvs.f  ${BGC_SRC}ENTITY.F77 \
                                  ${BGC_SRC}BGCCOM.F77 \
                                  ${BASE_COM}ARRAYS.F77 \
                                  ${BASE_COM}CONTRL.F77 \
                                  ${TT_COM}PRGPRM.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bgcfvs.f

bgcgo.o${BJ}: ${BGC_SRC}bgcgo.f ${BGC_SRC}BGCCOM.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bgcgo.f

bgcgrow.o${BJ}: ${BGC_SRC}bgcgrow.f  ${BGC_SRC}BGCCOM.F77 \
                                    ${TT_COM}PRGPRM.F77 \
                                    ${BASE_COM}PLOT.F77 \
                                    ${BASE_COM}CONTRL.F77 \
                                    ${BASE_COM}PDEN.F77 \
                                    ${BASE_COM}OUTCOM.F77 \
                                    ${BASE_COM}ARRAYS.F77 \
                                    ${BASE_COM}CVCOM.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bgcgrow.f

bgcin.o${BJ}: ${BGC_SRC}bgcin.f  ${BGC_SRC}BGCCOM.F77 \
                                ${BGC_SRC}ENTITY.F77 \
                                ${TT_COM}PRGPRM.F77 \
                                ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bgcin.f

bgcint.o${BJ}: ${BGC_SRC}bgcint.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77 \
                                  ${BGC_SRC}BGCCOM.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bgcint.f 

bcvts.o${BJ}: ${BGC_SRC}bcvts.f
	${FC} -c ${FFLAGS} ${BGC_SRC}bcvts.f 

bgrown.o${BJ}: ${BGC_SRC}bgrown.f   ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bgrown.f

bhtdiam.o${BJ}: ${BGC_SRC}bhtdiam.f
	${FC} -c ${FFLAGS} ${BGC_SRC}bhtdiam.f

binitial.o${BJ}: ${BGC_SRC}binitial.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77 \
                                      ${BGC_SRC}BGCCOM.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}binitial.f 

biopen.o${BJ}: ${BGC_SRC}biopen.f
	${FC} -c ${FFLAGS} ${BGC_SRC}biopen.f 

bkill.o${BJ}: ${BGC_SRC}bkill.f ${BGC_SRC}ENTITY.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bkill.f

bpenman.o${BJ}: ${BGC_SRC}bpenman.f
	${FC} -c ${FFLAGS} ${BGC_SRC}bpenman.f

bsiteload.o${BJ}: ${BGC_SRC}bsiteload.f ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bsiteload.f  

bstndbgcn.o${BJ}: ${BGC_SRC}bstndbgcn.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77 \
                                      ${BGC_SRC}BGCCOM.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bstndbgcn.f

bstructn.o${BJ}: ${BGC_SRC}bstructn.f ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bstructn.f  

btrnovr.o${BJ}: ${BGC_SRC}btrnovr.f ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}btrnovr.f  

bupdate.o${BJ}: ${BGC_SRC}bupdate.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bupdate.f   

bwater.o${BJ}: ${BGC_SRC}bwater.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bwater.f 

bzcomp.o${BJ}: ${BGC_SRC}bzcomp.f ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bzcomp.f

setleafn.o${BJ}: ${BGC_SRC}setleafn.f ${BGC_SRC}ENTITY.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}setleafn.f

sitocn.o${BJ}: ${BGC_SRC}sitocn.f ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}sitocn.f 

redolai.o${BJ}: ${BGC_SRC}redolai.f ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}redolai.f 
       










