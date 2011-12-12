# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../common/
LS_COM   = ../../ls/common/
FIRE_BASE_COM = ../../fire/base/common/

# the dwarf mistletoe interim model source directory
MIST_SRC = ../../mistoe/src/

# the PPE base source directory
PPBASE_SRC   = ../../ppbase/src/

# the PPE EC (East Cascades) source directory
PPEC_SRC   = ../../ppec/src/

# the Fuels and Fire Effects model source directory
FIRE_PPE_SRC = ../../fire/ppe/src/

# options for the compiler
FFLAGS = ${I1}${LS_COM}${I2}${BASE_COM}${I2}${FIRE_BASE_COM}

# the PPE LS (Lake States) object files
PPLS_OBJS = ppdgf.o${BJ} ppregt.o${BJ} varget.o${BJ} varput.o${BJ} \
            add1.o${BJ}   alinit.o${BJ} alstd1.o${BJ} alstd2.o${BJ} bfread.o${BJ} bfwrit.o${BJ} bygrps.o${BJ} \
            c11srt.o${BJ} ch8bsr.o${BJ} ch8srt.o${BJ} chdsth.o${BJ} chget.o${BJ}  chput.o${BJ}  chread.o${BJ} \
            chstsh.o${BJ} chwrit.o${BJ} cmadds.o${BJ} cmprt2.o${BJ} dstash.o${BJ} ecnget.o${BJ}  ecnput.o${BJ} \
            evagrp.o${BJ} getprt.o${BJ} getstd.o${BJ} gpadd.o${BJ}  gpadsd.o${BJ} gpclos.o${BJ} \
            gpcset.o${BJ} gpcycl.o${BJ} gpget.o${BJ}  gpinit.o${BJ} gpload.o${BJ} gpnew.o${BJ}  grcend.o${BJ} \
            groend.o${BJ} grstnd.o${BJ} histrd.o${BJ} histwt.o${BJ} hvalid.o${BJ} hvaloc.o${BJ} hvccut.o${BJ} \
            hvcntg.o${BJ} hvdnsl.o${BJ} hvfxlb.o${BJ} hvhrv1.o${BJ} hvin.o${BJ}   hvinit.o${BJ} hvlbwt.o${BJ} \
            hvnedn.o${BJ} hvreps.o${BJ} hvproj.o${BJ}  hvsel.o${BJ}  hvstsl.o${BJ} hvthn1.o${BJ} hvupst.o${BJ} hxindx.o${BJ} \
            ifread.o${BJ} ifwrit.o${BJ} inscyc.o${BJ} instnd.o${BJ} lbrnls.o${BJ} ldarea.o${BJ} ldsum.o${BJ}  \
            lfread.o${BJ} lfwrit.o${BJ} lladd.o${BJ}  llconc.o${BJ} llcopy.o${BJ} lldele.o${BJ} lldisp.o${BJ} \
            llerrp.o${BJ} llinit.o${BJ} lllist.o${BJ} llnew.o${BJ}  llnext.o${BJ} llnull.o${BJ} llscrh.o${BJ} \
            llsetp.o${BJ} llvalu.o${BJ} mstime.o${BJ} mstrgt.o${BJ} mstrit.o${BJ} mstrpt.o${BJ} oneisn.o${BJ} \
            onerpt.o${BJ} plbwts.o${BJ} pparps.o${BJ} ppauid.o${BJ} ppause.o${BJ} ppbrph.o${BJ} ppclop.o${BJ} \
            ppcvut.o${BJ} ppeatv.o${BJ} ppeblk.o${BJ} ppecyc.o${BJ} ppeget.o${BJ} ppeopn.o${BJ} pperrp.o${BJ} \
            ppevmi.o${BJ} ppin.o${BJ}   ppinit.o${BJ} ppintr.o${BJ} ppisn.o${BJ}  ppkey.o${BJ}  pplabs.o${BJ} \
            ppldev.o${BJ} ppldx.o${BJ}  ppmain.o${BJ} ppmclk.o${BJ} pppden.o${BJ} ppweig.o${BJ} prtbrh.o${BJ} \
            prtspa.o${BJ} putstd.o${BJ} ritcol.o${BJ} rptwtr.o${BJ} sched.o${BJ} selint.o${BJ}  slcond.o${BJ} \
            sndtbs.o${BJ} spamgm.o${BJ} sparls.o${BJ} spasgn.o${BJ} spbsrx.o${BJ} spcntg.o${BJ} spextr.o${BJ} \
            splaar.o${BJ} splads.o${BJ} splaex.o${BJ} splait.o${BJ} splalo.o${BJ} splard.o${BJ} spnbbd.o${BJ} \
            spnbed.o${BJ} spnbex.o${BJ} spnbit.o${BJ} spnbrd.o${BJ} sprdis.o${BJ} sprdit.o${BJ} sprdrd.o${BJ} \
            srtisn.o${BJ} stash.o${BJ}  stoprt.o${BJ} wrthst.o${BJ} yields.o${BJ} c26srt.o${BJ} c26bsr.o${BJ} \
            fmppget.o${BJ} fmppput.o${BJ} fmpphv.o${BJ}

ppls:     ${PPLS_OBJS}

ppdgf.o${BJ}:  ${PPEC_SRC}ppdgf.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}ppdgf.f

ppregt.o${BJ}: ${PPEC_SRC}ppregt.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}ppregt.f

varget.o${BJ}: ${PPEC_SRC}varget.f ${LS_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}varget.f

varput.o${BJ}: ${PPEC_SRC}varput.f ${LS_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}varput.f

add1.o${BJ}:   ${PPBASE_SRC}add1.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}add1.f

alinit.o${BJ}: ${PPBASE_SRC}alinit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}alinit.f

alstd1.o${BJ}: ${PPBASE_SRC}alstd1.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}alstd1.f

alstd2.o${BJ}: ${PPBASE_SRC}alstd2.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}alstd2.f

bfread.o${BJ}: ${PPBASE_SRC}bfread.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}bfread.f

bfwrit.o${BJ}: ${PPBASE_SRC}bfwrit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}bfwrit.f

bygrps.o${BJ}: ${PPBASE_SRC}bygrps.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77  \
          ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}bygrps.f

c11srt.o${BJ}: ${PPBASE_SRC}c11srt.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}c11srt.f

c26bsr.o${BJ}: ${PPBASE_SRC}c26bsr.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}c26bsr.f

c26srt.o${BJ}: ${PPBASE_SRC}c26srt.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}c26srt.f

ch8bsr.o${BJ}: ${PPBASE_SRC}ch8bsr.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ch8bsr.f

ch8srt.o${BJ}: ${PPBASE_SRC}ch8srt.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ch8srt.f

chdsth.o${BJ}: ${PPBASE_SRC}chdsth.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chdsth.f

chget.o${BJ}:  ${PPBASE_SRC}chget.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}CALDEN.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}DBSTK.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chget.f

chput.o${BJ}:  ${PPBASE_SRC}chput.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}CALDEN.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}DBSTK.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chput.f

chread.o${BJ}: ${PPBASE_SRC}chread.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chread.f

chstsh.o${BJ}: ${PPBASE_SRC}chstsh.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chstsh.f

chwrit.o${BJ}: ${PPBASE_SRC}chwrit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chwrit.f

cmadds.o${BJ}: ${PPBASE_SRC}cmadds.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCMAD.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}cmadds.f

cmprt2.o${BJ}: ${PPBASE_SRC}cmprt2.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}cmprt2.f

ecnget.o${BJ}:  ${PPBASE_SRC}ecnget.f ${LS_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ecnget.f

ecnput.o${BJ}:  ${PPBASE_SRC}ecnput.f ${LS_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ecnput.f

dstash.o${BJ}: ${PPBASE_SRC}dstash.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}dstash.f

evagrp.o${BJ}: ${PPBASE_SRC}evagrp.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}evagrp.f

getprt.o${BJ}: ${PPBASE_SRC}getprt.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}getprt.f

getstd.o${BJ}: ${PPBASE_SRC}getstd.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPDNCM.F77 ${LS_COM}ESPARM.F77  \
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

gpadd.o${BJ}:  ${PPBASE_SRC}gpadd.f ${BASE_COM}PPGPCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpadd.f

gpadsd.o${BJ}: ${PPBASE_SRC}gpadsd.f ${BASE_COM}PPGPCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpadsd.f

gpclos.o${BJ}: ${PPBASE_SRC}gpclos.f ${BASE_COM}PPGPCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpclos.f

gpcset.o${BJ}: ${PPBASE_SRC}gpcset.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpcset.f

gpcycl.o${BJ}: ${PPBASE_SRC}gpcycl.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpcycl.f

gpget.o${BJ}:  ${PPBASE_SRC}gpget.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
          ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpget.f

gpinit.o${BJ}: ${PPBASE_SRC}gpinit.f ${BASE_COM}PPGPCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpinit.f

gpload.o${BJ}: ${PPBASE_SRC}gpload.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpload.f

gpnew.o${BJ}:  ${PPBASE_SRC}gpnew.f ${BASE_COM}PPGPCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpnew.f

grcend.o${BJ}: ${PPBASE_SRC}grcend.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}grcend.f

groend.o${BJ}: ${PPBASE_SRC}groend.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77      \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}groend.f

grstnd.o${BJ}: ${PPBASE_SRC}grstnd.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}grstnd.f

histrd.o${BJ}: ${PPBASE_SRC}histrd.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCISN.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}histrd.f

histwt.o${BJ}: ${PPBASE_SRC}histwt.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}histwt.f

hvalid.o${BJ}: ${PPBASE_SRC}hvalid.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvalid.f

hvaloc.o${BJ}: ${PPBASE_SRC}hvaloc.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77 ${BASE_COM}PPCNTL.F77  \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OPCOM.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}HVDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvaloc.f

hvccut.o${BJ}: ${PPBASE_SRC}hvccut.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvccut.f

hvcntg.o${BJ}: ${PPBASE_SRC}hvcntg.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvcntg.f

hvdnsl.o${BJ}: ${PPBASE_SRC}hvdnsl.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}HVDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvdnsl.f

hvfxlb.o${BJ}: ${PPBASE_SRC}hvfxlb.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvfxlb.f

hvhrv1.o${BJ}:  ${PPBASE_SRC}hvhrv1.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}OPCOM.F77 ${BASE_COM}ECON.F77 ${BASE_COM}HVDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvhrv1.f

hvin.o${BJ}:    ${PPBASE_SRC}hvin.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77 ${BASE_COM}OPCOM.F77  \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvin.f

hvinit.o${BJ}:  ${PPBASE_SRC}hvinit.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}HVDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvinit.f

hvlbwt.o${BJ}:  ${PPBASE_SRC}hvlbwt.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvlbwt.f

hvnedn.o${BJ}:  ${PPBASE_SRC}hvnedn.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77   \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}HVDNCM.F77 ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvnedn.f

hvproj.o${BJ}:  ${PPBASE_SRC}hvproj.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}HVDNCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvproj.f

hvreps.o${BJ}:  ${PPBASE_SRC}hvreps.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvreps.f

hvsel.o${BJ}:   ${PPBASE_SRC}hvsel.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvsel.f

hvstsl.o${BJ}:  ${PPBASE_SRC}hvstsl.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvstsl.f

hvthn1.o${BJ}:  ${PPBASE_SRC}hvthn1.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}HVDNCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OPCOM.F77  \
           ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvthn1.f

hvupst.o${BJ}:  ${PPBASE_SRC}hvupst.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvupst.f

hxindx.o${BJ}:  ${PPBASE_SRC}hxindx.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hxindx.f

ifread.o${BJ}:  ${PPBASE_SRC}ifread.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ifread.f

ifwrit.o${BJ}:  ${PPBASE_SRC}ifwrit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ifwrit.f

inscyc.o${BJ}:  ${PPBASE_SRC}inscyc.f ${LS_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}inscyc.f

instnd.o${BJ}:  ${PPBASE_SRC}instnd.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}instnd.f

lbrnls.o${BJ}:  ${PPBASE_SRC}lbrnls.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lbrnls.f

ldarea.o${BJ}:  ${PPBASE_SRC}ldarea.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ldarea.f

ldsum.o${BJ}:   ${PPBASE_SRC}ldsum.f ${LS_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 \
           ${BASE_COM}SUMTAB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ldsum.f

lfread.o${BJ}:  ${PPBASE_SRC}lfread.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lfread.f

lfwrit.o${BJ}:  ${PPBASE_SRC}lfwrit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lfwrit.f

lladd.o${BJ}:   ${PPBASE_SRC}lladd.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lladd.f

llconc.o${BJ}:  ${PPBASE_SRC}llconc.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llconc.f

llcopy.o${BJ}:  ${PPBASE_SRC}llcopy.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llcopy.f

lldele.o${BJ}:  ${PPBASE_SRC}lldele.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lldele.f

lldisp.o${BJ}:  ${PPBASE_SRC}lldisp.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lldisp.f

llerrp.o${BJ}:  ${PPBASE_SRC}llerrp.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llerrp.f

llinit.o${BJ}:  ${PPBASE_SRC}llinit.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llinit.f

lllist.o${BJ}:  ${PPBASE_SRC}lllist.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lllist.f

llnew.o${BJ}:   ${PPBASE_SRC}llnew.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPLLCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llnew.f

llnext.o${BJ}:  ${PPBASE_SRC}llnext.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llnext.f

llnull.o${BJ}:  ${PPBASE_SRC}llnull.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llnull.f

llscrh.o${BJ}:  ${PPBASE_SRC}llscrh.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llscrh.f

llsetp.o${BJ}:  ${PPBASE_SRC}llsetp.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llsetp.f

llvalu.o${BJ}:  ${PPBASE_SRC}llvalu.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llvalu.f

mstime.o${BJ}:  ${PPBASE_SRC}mstime.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}mstime.f

mstrgt.o${BJ}:  ${PPBASE_SRC}mstrgt.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OPCOM.F77  \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}mstrgt.f

mstrit.o${BJ}:  ${PPBASE_SRC}mstrit.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}mstrit.f

mstrpt.o${BJ}:  ${PPBASE_SRC}mstrpt.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OPCOM.F77  \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}mstrpt.f

oneisn.o${BJ}:  ${PPBASE_SRC}oneisn.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}oneisn.f

onerpt.o${BJ}:  ${PPBASE_SRC}onerpt.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}onerpt.f

plbwts.o${BJ}:  ${PPBASE_SRC}plbwts.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}plbwts.f

pparps.o${BJ}:  ${PPBASE_SRC}pparps.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}pparps.f

ppauid.o${BJ}:  ${PPBASE_SRC}ppauid.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppauid.f

ppause.o${BJ}:  ${PPBASE_SRC}ppause.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPSPRD.F77 ${BASE_COM}PPSPNB.F77 \
           ${BASE_COM}PPSPLA.F77 ${BASE_COM}PPLLCM.F77 ${BASE_COM}HVDNCM.F77 \
           ${BASE_COM}PPMDCM.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppause.f

ppbrph.o${BJ}:  ${PPBASE_SRC}ppbrph.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppbrph.f

ppclop.o${BJ}:  ${PPBASE_SRC}ppclop.f ${LS_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppclop.f

ppcvut.o${BJ}:  ${PPBASE_SRC}ppcvut.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppcvut.f

ppeatv.o${BJ}:  ${PPBASE_SRC}ppeatv.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppeatv.f

ppeblk.o${BJ}:  ${PPBASE_SRC}ppeblk.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPEXCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppeblk.f

ppecyc.o${BJ}:  ${PPBASE_SRC}ppecyc.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppecyc.f

ppeget.o${BJ}:  ${PPBASE_SRC}ppeget.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPSPRD.F77 ${BASE_COM}PPSPNB.F77 \
           ${BASE_COM}PPSPLA.F77 ${BASE_COM}PPLLCM.F77 ${BASE_COM}HVDNCM.F77 \
           ${BASE_COM}PPMDCM.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppeget.f

ppeopn.o${BJ}:  ${PPBASE_SRC}ppeopn.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppeopn.f

pperrp.o${BJ}:  ${PPBASE_SRC}pperrp.f ${LS_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}pperrp.f

ppevmi.o${BJ}:  ${PPBASE_SRC}ppevmi.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}OPCOM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppevmi.f

ppin.o${BJ}:    ${PPBASE_SRC}ppin.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
           ${BASE_COM}OPCOM.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}HVDNCM.F77  \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppin.f

ppinit.o${BJ}:  ${PPBASE_SRC}ppinit.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPDNCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppinit.f

ppintr.o${BJ}:  ${PPBASE_SRC}ppintr.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppintr.f

ppisn.o${BJ}:   ${PPBASE_SRC}ppisn.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCISN.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppisn.f

ppkey.o${BJ}:   ${PPBASE_SRC}ppkey.f ${BASE_COM}PPEXCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppkey.f

pplabs.o${BJ}:  ${PPBASE_SRC}pplabs.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}pplabs.f

ppldev.o${BJ}:  ${PPBASE_SRC}ppldev.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppldev.f

ppldx.o${BJ}:   ${PPBASE_SRC}ppldx.f ${BASE_COM}PPEXCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppldx.f

ppmain.o${BJ}:  ${PPBASE_SRC}ppmain.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppmain.f

ppmclk.o${BJ}:  ${PPBASE_SRC}ppmclk.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppmclk.f

pppden.o${BJ}:  ${PPBASE_SRC}pppden.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}PDEN.F77 ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}pppden.f

ppweig.o${BJ}:  ${PPBASE_SRC}ppweig.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppweig.f

prtbrh.o${BJ}:  ${PPBASE_SRC}prtbrh.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}prtbrh.f

prtspa.o${BJ}:  ${PPBASE_SRC}prtspa.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}prtspa.f

putstd.o${BJ}:  ${PPBASE_SRC}putstd.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPDNCM.F77 ${LS_COM}ESPARM.F77 \
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

ritcol.o${BJ}:  ${PPBASE_SRC}ritcol.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ritcol.f

rptwtr.o${BJ}:  ${PPBASE_SRC}rptwtr.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}rptwtr.f

sched.o${BJ}:   ${PPBASE_SRC}sched.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sched.f

selint.o${BJ}:  ${PPBASE_SRC}selint.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}selint.f

slcond.o${BJ}:  ${PPBASE_SRC}slcond.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}slcond.f

sndtbs.o${BJ}:  ${PPBASE_SRC}sndtbs.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCISN.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sndtbs.f

spamgm.o${BJ}:  ${PPBASE_SRC}spamgm.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spamgm.f

sparls.o${BJ}:  ${PPBASE_SRC}sparls.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sparls.f

spasgn.o${BJ}:  ${PPBASE_SRC}spasgn.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spasgn.f

spbsrx.o${BJ}:  ${PPBASE_SRC}spbsrx.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spbsrx.f

spcntg.o${BJ}:  ${PPBASE_SRC}spcntg.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spcntg.f

spextr.o${BJ}:  ${PPBASE_SRC}spextr.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spextr.f

splaar.o${BJ}:  ${PPBASE_SRC}splaar.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splaar.f

splads.o${BJ}:  ${PPBASE_SRC}splads.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splads.f

splaex.o${BJ}:  ${PPBASE_SRC}splaex.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splaex.f

splait.o${BJ}:  ${PPBASE_SRC}splait.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splait.f

splalo.o${BJ}:  ${PPBASE_SRC}splalo.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splalo.f

splard.o${BJ}:  ${PPBASE_SRC}splard.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splard.f

spnbbd.o${BJ}:  ${PPBASE_SRC}spnbbd.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbbd.f

spnbed.o${BJ}:  ${PPBASE_SRC}spnbed.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbed.f

spnbex.o${BJ}:  ${PPBASE_SRC}spnbex.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbex.f

spnbit.o${BJ}:  ${PPBASE_SRC}spnbit.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbit.f

spnbrd.o${BJ}:  ${PPBASE_SRC}spnbrd.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbrd.f

sprdis.o${BJ}:  ${PPBASE_SRC}sprdis.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPRD.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sprdis.f

sprdit.o${BJ}:  ${PPBASE_SRC}sprdit.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPRD.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sprdit.f

sprdrd.o${BJ}:  ${PPBASE_SRC}sprdrd.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPRD.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sprdrd.f

srtisn.o${BJ}:  ${PPBASE_SRC}srtisn.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}srtisn.f

stash.o${BJ}:   ${PPBASE_SRC}stash.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}stash.f

stoprt.o${BJ}:  ${PPBASE_SRC}stoprt.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}stoprt.f

wrthst.o${BJ}:  ${PPBASE_SRC}wrthst.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCISN.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}wrthst.f

yields.o${BJ}:  ${PPBASE_SRC}yields.f ${LS_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OUTCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}yields.f

#
# Fire (FFE) PPE objects

fmppget.o${BJ}: \
${FIRE_PPE_SRC}fmppget.f \
${LS_COM}PRGPRM.F77 \
${BASE_COM}PPEPRM.F77 \
${BASE_COM}PPCNTL.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}SVDATA.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77  \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_PPE_SRC}fmppget.f

fmppput.o${BJ}: \
${FIRE_PPE_SRC}fmppput.f \
${LS_COM}PRGPRM.F77 \
${BASE_COM}PPEPRM.F77 \
${BASE_COM}PPCNTL.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}SVDATA.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${FIRE_BASE_COM}FMCOM.F77  \
${FIRE_BASE_COM}FMFCOM.F77 \
${FIRE_BASE_COM}FMSVCM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_PPE_SRC}fmppput.f

fmpphv.o${BJ}: \
${FIRE_PPE_SRC}fmpphv.f \
${LS_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77  \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_PPE_SRC}fmpphv.f

