# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../common/
WC_COM   = ../../wc/common/
FIRE_BASE_COM = ../../fire/base/common/

# the Fuels and Fire Effects model source directory
FIRE_PPE_SRC = ../../fire/ppe/src/

# the dwarf mistletoe interim model source directory
MIST_SRC = ../../mistoe/src/

# Westwide Pine Beetle source directory
WWPB_SRC   = ../../wwpb/src/

# the PPE base source directory
PPBASE_SRC   = ../../ppbase/src/

# the PPE WC (West Cascades) source directory
PPWC_COM   = ./../src/

# the PPE EC (East Cascades) source directory
PPEC_SRC   = ../../ppec/src/

# options for the compiler
FFLAGS = ${I1}${WC_COM}${I2}${BASE_COM}${I2}${MIST_SRC}${I2}${FIRE_BASE_COM}

# the PPE WC (West Cascades) object files
PPWC_OBJS = ppdgf.o${BJ} ppregt.o${BJ} varget.o${BJ} varput.o${BJ} \
            add1.o${BJ}   alinit.o${BJ} alstd1.o${BJ} alstd2.o${BJ} bfread.o${BJ} bfwrit.o${BJ} bygrps.o${BJ} \
            c11srt.o${BJ} ch8bsr.o${BJ} ch8srt.o${BJ} chdsth.o${BJ} chget.o${BJ}  chput.o${BJ}  chread.o${BJ} \
            chstsh.o${BJ} chwrit.o${BJ} cmadds.o${BJ} cmprt2.o${BJ} cvget.o${BJ}  cvput.o${BJ}  dstash.o${BJ} \
            ecnget.o${BJ}  ecnput.o${BJ} evagrp.o${BJ} getprt.o${BJ} getstd.o${BJ} gpadd.o${BJ}  gpadsd.o${BJ} \
            gpclos.o${BJ} \
            gpcset.o${BJ} gpcycl.o${BJ} gpget.o${BJ}  gpinit.o${BJ} gpload.o${BJ} gpnew.o${BJ}  grcend.o${BJ} \
            groend.o${BJ} grstnd.o${BJ} histrd.o${BJ} histwt.o${BJ} hvalid.o${BJ} hvaloc.o${BJ} hvccut.o${BJ} \
            hvcntg.o${BJ} hvdnsl.o${BJ} hvfxlb.o${BJ} hvhrv1.o${BJ} hvin.o${BJ}   hvinit.o${BJ} hvlbwt.o${BJ} \
            hvnedn.o${BJ} hvreps.o${BJ} hvproj.o${BJ} hvsel.o${BJ}  hvstsl.o${BJ} hvthn1.o${BJ} hvupst.o${BJ} \
            hxindx.o${BJ} \
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
            fmppget.o${BJ} fmppput.o${BJ} fmpphv.o${BJ} \
            misact.o${BJ} msppgt.o${BJ} mspppt.o${BJ}

# WWPB (West Cascades) object files
WWPB_OBJS = bmagdw.o${BJ} bmatct.o${BJ} bmavg.o${BJ} bmblkdwc.o${BJ} \
            bmcbet.o${BJ} bmcbkp.o${BJ} bmcgrf.o${BJ} bmcnum.o${BJ} \
            bmcspt.o${BJ} bmcwin.o${BJ} bmdam.o${BJ} bmdbhc.o${BJ} bmdfol.o${BJ} \
            bmdrgt.o${BJ} bmdrv.o${BJ} bmfdbk.o${BJ} \
            bmfint.o${BJ} bmfire.o${BJ} bmfmrt.o${BJ} bmin.o${BJ} bminit.o${BJ} \
            bmips.o${BJ} bmistd.o${BJ} bmkill.o${BJ} bmlite.o${BJ} bmmort.o${BJ} \
            bmobb.o${BJ} bmout.o${BJ} bmpher.o${BJ} bmppgt.o${BJ} bmppin.o${BJ} \
            bmpppt.o${BJ} bmpstc.o${BJ} bmqmrt.o${BJ} bmrann.o${BJ} bmrrgt.o${BJ} \
            bmrrpt.o${BJ} bmrrsr.o${BJ} bmsalv.o${BJ} bmsani.o${BJ} bmsdit.o${BJ} \
            bmsetp.o${BJ} bmslsh.o${BJ} bmsmgt.o${BJ} bmtrip.o${BJ} \
            gpget2.o${BJ}

ppwc:     ${PPWC_OBJS}

ppdgf.o${BJ}:  ${PPEC_SRC}ppdgf.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}ppdgf.f

ppregt.o${BJ}: ${PPEC_SRC}ppregt.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}ppregt.f

varget.o${BJ}: ${PPEC_SRC}varget.f ${WC_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}varget.f

varput.o${BJ}: ${PPEC_SRC}varput.f ${WC_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${PPEC_SRC}varput.f

add1.o${BJ}:   ${PPBASE_SRC}add1.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}add1.f

alinit.o${BJ}: ${PPBASE_SRC}alinit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}alinit.f

alstd1.o${BJ}: ${PPBASE_SRC}alstd1.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}alstd1.f

alstd2.o${BJ}: ${PPBASE_SRC}alstd2.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}alstd2.f

bfread.o${BJ}: ${PPBASE_SRC}bfread.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}bfread.f

bfwrit.o${BJ}: ${PPBASE_SRC}bfwrit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}bfwrit.f

bygrps.o${BJ}: ${PPBASE_SRC}bygrps.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
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

chdsth.o${BJ}: ${PPBASE_SRC}chdsth.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chdsth.f

chget.o${BJ}:  ${PPBASE_SRC}chget.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}CALDEN.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}DBSTK.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chget.f

chput.o${BJ}:  ${PPBASE_SRC}chput.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}CALDEN.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}DBSTK.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chput.f

chread.o${BJ}: ${PPBASE_SRC}chread.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chread.f

chstsh.o${BJ}: ${PPBASE_SRC}chstsh.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chstsh.f

chwrit.o${BJ}: ${PPBASE_SRC}chwrit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chwrit.f

cmadds.o${BJ}: ${PPBASE_SRC}cmadds.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCMAD.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}cmadds.f

cvget.o${BJ}:  ${PPBASE_SRC}cvget.f ${WC_COM}PRGPRM.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}cvget.f

cvput.o${BJ}:  ${PPBASE_SRC}cvput.f ${WC_COM}PRGPRM.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}cvput.f

cmprt2.o${BJ}: ${PPBASE_SRC}cmprt2.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}cmprt2.f

dstash.o${BJ}: ${PPBASE_SRC}dstash.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}dstash.f

ecnget.o${BJ}:  ${PPBASE_SRC}ecnget.f ${WC_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ecnget.f

ecnput.o${BJ}:  ${PPBASE_SRC}ecnput.f ${WC_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ecnput.f

evagrp.o${BJ}: ${PPBASE_SRC}evagrp.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}evagrp.f

getprt.o${BJ}: ${PPBASE_SRC}getprt.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}getprt.f

getstd.o${BJ}: ${PPBASE_SRC}getstd.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPDNCM.F77 ${WC_COM}ESPARM.F77  \
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

gpcset.o${BJ}: ${PPBASE_SRC}gpcset.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpcset.f

gpcycl.o${BJ}: ${PPBASE_SRC}gpcycl.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpcycl.f

gpget.o${BJ}:  ${PPBASE_SRC}gpget.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
          ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpget.f

gpinit.o${BJ}: ${PPBASE_SRC}gpinit.f ${BASE_COM}PPGPCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpinit.f

gpload.o${BJ}: ${PPBASE_SRC}gpload.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpload.f

gpnew.o${BJ}:  ${PPBASE_SRC}gpnew.f ${BASE_COM}PPGPCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpnew.f

grcend.o${BJ}: ${PPBASE_SRC}grcend.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}grcend.f

groend.o${BJ}: ${PPBASE_SRC}groend.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77      \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}groend.f

grstnd.o${BJ}: ${PPBASE_SRC}grstnd.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}grstnd.f

histrd.o${BJ}: ${PPBASE_SRC}histrd.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCISN.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}histrd.f

histwt.o${BJ}: ${PPBASE_SRC}histwt.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}histwt.f

hvalid.o${BJ}: ${PPBASE_SRC}hvalid.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvalid.f

hvaloc.o${BJ}: ${PPBASE_SRC}hvaloc.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77 ${BASE_COM}PPCNTL.F77  \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OPCOM.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}HVDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvaloc.f

hvccut.o${BJ}: ${PPBASE_SRC}hvccut.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvccut.f

hvcntg.o${BJ}: ${PPBASE_SRC}hvcntg.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvcntg.f

hvdnsl.o${BJ}: ${PPBASE_SRC}hvdnsl.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}HVDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvdnsl.f

hvfxlb.o${BJ}: ${PPBASE_SRC}hvfxlb.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvfxlb.f

hvhrv1.o${BJ}:  ${PPBASE_SRC}hvhrv1.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}OPCOM.F77 ${BASE_COM}ECON.F77 ${BASE_COM}HVDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvhrv1.f

hvin.o${BJ}:    ${PPBASE_SRC}hvin.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77 ${BASE_COM}OPCOM.F77  \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvin.f

hvinit.o${BJ}:  ${PPBASE_SRC}hvinit.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}HVDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvinit.f

hvlbwt.o${BJ}:  ${PPBASE_SRC}hvlbwt.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvlbwt.f

hvnedn.o${BJ}:  ${PPBASE_SRC}hvnedn.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77   \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}HVDNCM.F77 ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvnedn.f

hvproj.o${BJ}:  ${PPBASE_SRC}hvproj.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}HVDNCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvproj.f

hvreps.o${BJ}:  ${PPBASE_SRC}hvreps.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvreps.f

hvsel.o${BJ}:   ${PPBASE_SRC}hvsel.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvsel.f

hvstsl.o${BJ}:  ${PPBASE_SRC}hvstsl.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvstsl.f

hvthn1.o${BJ}:  ${PPBASE_SRC}hvthn1.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}HVDNCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OPCOM.F77  \
           ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvthn1.f

hvupst.o${BJ}:  ${PPBASE_SRC}hvupst.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvupst.f

hxindx.o${BJ}:  ${PPBASE_SRC}hxindx.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hxindx.f

ifread.o${BJ}:  ${PPBASE_SRC}ifread.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ifread.f

ifwrit.o${BJ}:  ${PPBASE_SRC}ifwrit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ifwrit.f

inscyc.o${BJ}:  ${PPBASE_SRC}inscyc.f ${WC_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}inscyc.f

instnd.o${BJ}:  ${PPBASE_SRC}instnd.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}instnd.f

lbrnls.o${BJ}:  ${PPBASE_SRC}lbrnls.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lbrnls.f

ldarea.o${BJ}:  ${PPBASE_SRC}ldarea.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ldarea.f

ldsum.o${BJ}:   ${PPBASE_SRC}ldsum.f ${WC_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 \
           ${BASE_COM}SUMTAB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ldsum.f

lfread.o${BJ}:  ${PPBASE_SRC}lfread.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lfread.f

lfwrit.o${BJ}:  ${PPBASE_SRC}lfwrit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lfwrit.f

lladd.o${BJ}:   ${PPBASE_SRC}lladd.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lladd.f

llconc.o${BJ}:  ${PPBASE_SRC}llconc.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llconc.f

llcopy.o${BJ}:  ${PPBASE_SRC}llcopy.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llcopy.f

lldele.o${BJ}:  ${PPBASE_SRC}lldele.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lldele.f

lldisp.o${BJ}:  ${PPBASE_SRC}lldisp.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lldisp.f

llerrp.o${BJ}:  ${PPBASE_SRC}llerrp.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llerrp.f

llinit.o${BJ}:  ${PPBASE_SRC}llinit.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llinit.f

lllist.o${BJ}:  ${PPBASE_SRC}lllist.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lllist.f

llnew.o${BJ}:   ${PPBASE_SRC}llnew.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPLLCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llnew.f

llnext.o${BJ}:  ${PPBASE_SRC}llnext.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llnext.f

llnull.o${BJ}:  ${PPBASE_SRC}llnull.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llnull.f

llscrh.o${BJ}:  ${PPBASE_SRC}llscrh.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llscrh.f

llsetp.o${BJ}:  ${PPBASE_SRC}llsetp.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llsetp.f

llvalu.o${BJ}:  ${PPBASE_SRC}llvalu.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llvalu.f

mstime.o${BJ}:  ${PPBASE_SRC}mstime.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}mstime.f

mstrgt.o${BJ}:  ${PPBASE_SRC}mstrgt.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OPCOM.F77  \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}mstrgt.f

mstrit.o${BJ}:  ${PPBASE_SRC}mstrit.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}mstrit.f

mstrpt.o${BJ}:  ${PPBASE_SRC}mstrpt.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OPCOM.F77  \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}mstrpt.f

oneisn.o${BJ}:  ${PPBASE_SRC}oneisn.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}oneisn.f

onerpt.o${BJ}:  ${PPBASE_SRC}onerpt.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}onerpt.f

plbwts.o${BJ}:  ${PPBASE_SRC}plbwts.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}plbwts.f

pparps.o${BJ}:  ${PPBASE_SRC}pparps.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}pparps.f

ppauid.o${BJ}:  ${PPBASE_SRC}ppauid.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppauid.f

ppause.o${BJ}:  ${PPBASE_SRC}ppause.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPSPRD.F77 ${BASE_COM}PPSPNB.F77 \
           ${BASE_COM}PPSPLA.F77 ${BASE_COM}PPLLCM.F77 ${BASE_COM}HVDNCM.F77 \
           ${BASE_COM}PPMDCM.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppause.f

ppbrph.o${BJ}:  ${PPBASE_SRC}ppbrph.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppbrph.f

ppclop.o${BJ}:  ${PPBASE_SRC}ppclop.f ${WC_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppclop.f

ppcvut.o${BJ}:  ${PPBASE_SRC}ppcvut.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppcvut.f

ppeatv.o${BJ}:  ${PPBASE_SRC}ppeatv.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppeatv.f

ppeblk.o${BJ}:  ${PPBASE_SRC}ppeblk.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPEXCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppeblk.f

ppecyc.o${BJ}:  ${PPBASE_SRC}ppecyc.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppecyc.f

ppeget.o${BJ}:  ${PPBASE_SRC}ppeget.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPSPRD.F77 ${BASE_COM}PPSPNB.F77 \
           ${BASE_COM}PPSPLA.F77 ${BASE_COM}PPLLCM.F77 ${BASE_COM}HVDNCM.F77 \
           ${BASE_COM}PPMDCM.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppeget.f

ppeopn.o${BJ}:  ${PPBASE_SRC}ppeopn.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppeopn.f

pperrp.o${BJ}:  ${PPBASE_SRC}pperrp.f ${WC_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}pperrp.f

ppevmi.o${BJ}:  ${PPBASE_SRC}ppevmi.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}OPCOM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppevmi.f

ppin.o${BJ}:    ${PPBASE_SRC}ppin.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
           ${BASE_COM}OPCOM.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}HVDNCM.F77  \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppin.f

ppinit.o${BJ}:  ${PPBASE_SRC}ppinit.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPDNCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppinit.f

ppintr.o${BJ}:  ${PPBASE_SRC}ppintr.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppintr.f

ppisn.o${BJ}:   ${PPBASE_SRC}ppisn.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCISN.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppisn.f

ppkey.o${BJ}:   ${PPBASE_SRC}ppkey.f ${BASE_COM}PPEXCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppkey.f

pplabs.o${BJ}:  ${PPBASE_SRC}pplabs.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}pplabs.f

ppldev.o${BJ}:  ${PPBASE_SRC}ppldev.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppldev.f

ppldx.o${BJ}:   ${PPBASE_SRC}ppldx.f ${BASE_COM}PPEXCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppldx.f

ppmain.o${BJ}:  ${PPBASE_SRC}ppmain.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppmain.f

ppmclk.o${BJ}:  ${PPBASE_SRC}ppmclk.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppmclk.f

pppden.o${BJ}:  ${PPBASE_SRC}pppden.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}PDEN.F77 ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}pppden.f

ppweig.o${BJ}:  ${PPBASE_SRC}ppweig.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppweig.f

prtbrh.o${BJ}:  ${PPBASE_SRC}prtbrh.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}prtbrh.f

prtspa.o${BJ}:  ${PPBASE_SRC}prtspa.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}prtspa.f

putstd.o${BJ}:  ${PPBASE_SRC}putstd.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPDNCM.F77 ${WC_COM}ESPARM.F77 \
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

rptwtr.o${BJ}:  ${PPBASE_SRC}rptwtr.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}rptwtr.f

sched.o${BJ}:   ${PPBASE_SRC}sched.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sched.f

selint.o${BJ}:  ${PPBASE_SRC}selint.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}selint.f

slcond.o${BJ}:  ${PPBASE_SRC}slcond.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}slcond.f

sndtbs.o${BJ}:  ${PPBASE_SRC}sndtbs.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCISN.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sndtbs.f

spamgm.o${BJ}:  ${PPBASE_SRC}spamgm.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spamgm.f

sparls.o${BJ}:  ${PPBASE_SRC}sparls.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sparls.f

spasgn.o${BJ}:  ${PPBASE_SRC}spasgn.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spasgn.f

spbsrx.o${BJ}:  ${PPBASE_SRC}spbsrx.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spbsrx.f

spcntg.o${BJ}:  ${PPBASE_SRC}spcntg.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spcntg.f

spextr.o${BJ}:  ${PPBASE_SRC}spextr.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spextr.f

splaar.o${BJ}:  ${PPBASE_SRC}splaar.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splaar.f

splads.o${BJ}:  ${PPBASE_SRC}splads.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splads.f

splaex.o${BJ}:  ${PPBASE_SRC}splaex.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splaex.f

splait.o${BJ}:  ${PPBASE_SRC}splait.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splait.f

splalo.o${BJ}:  ${PPBASE_SRC}splalo.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splalo.f

splard.o${BJ}:  ${PPBASE_SRC}splard.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splard.f

spnbbd.o${BJ}:  ${PPBASE_SRC}spnbbd.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbbd.f

spnbed.o${BJ}:  ${PPBASE_SRC}spnbed.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbed.f

spnbex.o${BJ}:  ${PPBASE_SRC}spnbex.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbex.f

spnbit.o${BJ}:  ${PPBASE_SRC}spnbit.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbit.f

spnbrd.o${BJ}:  ${PPBASE_SRC}spnbrd.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbrd.f

sprdis.o${BJ}:  ${PPBASE_SRC}sprdis.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPRD.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sprdis.f

sprdit.o${BJ}:  ${PPBASE_SRC}sprdit.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPRD.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sprdit.f

sprdrd.o${BJ}:  ${PPBASE_SRC}sprdrd.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPRD.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sprdrd.f

srtisn.o${BJ}:  ${PPBASE_SRC}srtisn.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}srtisn.f

stash.o${BJ}:   ${PPBASE_SRC}stash.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}stash.f

stoprt.o${BJ}:  ${PPBASE_SRC}stoprt.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}stoprt.f

wrthst.o${BJ}:  ${PPBASE_SRC}wrthst.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCISN.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}wrthst.f

yields.o${BJ}:  ${PPBASE_SRC}yields.f ${WC_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OUTCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}yields.f

#
# Fire (FFE) PPE objects

fmppget.o${BJ}: \
${FIRE_PPE_SRC}fmppget.f \
${WC_COM}PRGPRM.F77 \
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
${WC_COM}PRGPRM.F77 \
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
${WC_COM}PRGPRM.F77 \
${FIRE_BASE_COM}FMPARM.F77 \
${BASE_COM}CONTRL.F77 \
${BASE_COM}ARRAYS.F77 \
${FIRE_BASE_COM}FMCOM.F77  \
${FIRE_BASE_COM}FMFCOM.F77 \

	${FC} -c ${FFLAGS} ${FIRE_PPE_SRC}fmpphv.f

#
# Mistletoe PPE objects

misact.o${BJ}: ${MIST_SRC}misact.f
	${FC} -c ${FFLAGS} ${MIST_SRC}misact.f

msppgt.o${BJ}: ${MIST_SRC}msppgt.f ${WC_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}msppgt.f

mspppt.o${BJ}: ${MIST_SRC}mspppt.f ${WC_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}mspppt.f


wwpb : ${WWPB_OBJS}

bmagdw.o${BJ}:   ${WWPB_SRC}bmagdw.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmagdw.f   

bmatct.o${BJ}:   ${WWPB_SRC}bmatct.f  \
                 ${BASE_COM}PRGPRM.F77  ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmatct.f   

bmavg.o${BJ}:    ${WWPB_SRC}bmavg.f   \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmavg.f    

# This West Cascades block data needs compiled only with WC code
bmblkdwc.o${BJ}: ${WWPB_SRC}bmblkdwc.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMRCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmblkdwc.f 

bmcbet.o${BJ}:   ${WWPB_SRC}bmcbet.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcbet.f   

bmcbkp.o${BJ}:   ${WWPB_SRC}bmcbkp.f  \
                 ${BASE_COM}PRGPRM.F77  ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcbkp.f   

bmcgrf.o${BJ}:   ${WWPB_SRC}bmcgrf.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcgrf.f   

bmcnum.o${BJ}:   ${WWPB_SRC}bmcnum.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcnum.f   

bmcspt.o${BJ}:   ${WWPB_SRC}bmcspt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcspt.f   

bmcwin.o${BJ}:   ${WWPB_SRC}bmcwin.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcwin.f   

bmdam.o${BJ}:    ${WWPB_SRC}bmdam.f   \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}CONTRL.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdam.f    

bmdbhc.o${BJ}:   ${WWPB_SRC}bmdbhc.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdbhc.f   

bmdfol.o${BJ}:   ${WWPB_SRC}bmdfol.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdfol.f   

bmdrgt.o${BJ}:   ${WWPB_SRC}bmdrgt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdrgt.f   

bmdrv.o${BJ}:    ${WWPB_SRC}bmdrv.f   \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdrv.f    

bmfdbk.o${BJ}:   ${WWPB_SRC}bmfdbk.f  \
                 ${WWPB_SRC}BMPRM.F77 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmfdbk.f   

bmfint.o${BJ}:   ${WWPB_SRC}bmfint.f  \
                 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMFCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmfint.f   

bmfire.o${BJ}:   ${WWPB_SRC}bmfire.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMFCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmfire.f   

bmfmrt.o${BJ}:   ${WWPB_SRC}bmfmrt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMFCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmfmrt.f   

bmin.o${BJ}:     ${WWPB_SRC}bmin.f   \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}CONTRL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmin.f    

bminit.o${BJ}:   ${WWPB_SRC}bminit.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMRRCM.F77 \
                 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bminit.f   

bmips.o${BJ}:    ${WWPB_SRC}bmips.f   \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmips.f    

bmistd.o${BJ}:   ${WWPB_SRC}bmistd.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmistd.f   

bmkill.o${BJ}:   ${WWPB_SRC}bmkill.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}OUTCOM.F77 \
                 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmkill.f   

bmlite.o${BJ}:   ${WWPB_SRC}bmlite.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmlite.f   

bmmort.o${BJ}:   ${WWPB_SRC}bmmort.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmmort.f   

bmobb.o${BJ}:    ${WWPB_SRC}bmobb.f   \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmobb.f    

bmout.o${BJ}:    ${WWPB_SRC}bmout.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmout.f   

bmpher.o${BJ}:   ${WWPB_SRC}bmpher.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmpher.f   

bmppgt.o${BJ}:   ${WWPB_SRC}bmppgt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmppgt.f   

bmppin.o${BJ}:   ${WWPB_SRC}bmppin.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77 \
                 ${WWPB_SRC}BMRRCM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmppin.f   

bmpppt.o${BJ}:   ${WWPB_SRC}bmpppt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmpppt.f   

bmpstc.o${BJ}:   ${WWPB_SRC}bmpstc.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmpstc.f   

bmqmrt.o${BJ}:   ${WWPB_SRC}bmqmrt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmqmrt.f   

bmrann.o${BJ}:   ${WWPB_SRC}bmrann.f ${WWPB_SRC}BMRCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmrann.f   

bmrrgt.o${BJ}:   ${WWPB_SRC}bmrrgt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMRRCM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmrrgt.f   

bmrrpt.o${BJ}:   ${WWPB_SRC}bmrrpt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMRRCM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmrrpt.f   

bmrrsr.o${BJ}:   ${WWPB_SRC}bmrrsr.f  \
                 ${BASE_COM}PRGPRM.F77 ${WWPB_SRC}BMRRCM.F77 ${BASE_COM}CONTRL.F77 \
                 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmrrsr.f   

bmsalv.o${BJ}:   ${WWPB_SRC}bmsalv.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsalv.f   

bmsani.o${BJ}:   ${WWPB_SRC}bmsani.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsani.f   

bmsdit.o${BJ}:   ${WWPB_SRC}bmsdit.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}OUTCOM.F77 \
                 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MIST_SRC}MISCOM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMFCOM.F77 ${WWPB_SRC}BMRRCM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsdit.f   

bmsetp.o${BJ}:   ${WWPB_SRC}bmsetp.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}ARRAYS.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsetp.f   

bmslsh.o${BJ}:   ${WWPB_SRC}bmslsh.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmslsh.f   

bmsmgt.o${BJ}:   ${WWPB_SRC}bmsmgt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsmgt.f   

bmtrip.o${BJ}:   ${WWPB_SRC}bmtrip.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmtrip.f   

gpget2.o${BJ}:  ${PPBASE_SRC}gpget2.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
                ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpget2.f

