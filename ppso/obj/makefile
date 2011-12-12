# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ../../common/
SO_COM   = ../../so/common/
FIRE_BASE_COM = ../../fire/base/common/

# the Fuels and Fire Effects model source directory
FIRE_PPE_SRC = ../../fire/ppe/src/

# the dwarf mistletoe interim model source directory
MIST_SRC = ../../mistoe/src/

# Westwide Pine Beetle source directory
WWPB_SRC   = ../../wwpb/src/

# the PPE base source directory
PPBASE_SRC   = ../../ppbase/src/

# the PPE SO (SORNEC) source directory
PPSO_SRC   = ./../src/

# the PPE NI (Northern Idaho) source directory
PPNI_SRC   = ../../ppni/src/

# options for the compiler
FFLAGS = ${I1}${SO_COM}${I2}${BASE_COM}${I2}${MIST_SRC}${I2}${FIRE_BASE_COM}

# the PPE SO (South Central Oregon/Northeast California)
PPSO_OBJS = ppdgf.o${BJ} ppregt.o${BJ} varget.o${BJ} varput.o${BJ} \
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

# WWPB (South Central Oregon/Northeast California) object files
WWPB_OBJS = bmagdw.o${BJ} bmatct.o${BJ} bmavg.o${BJ} bmblkdso.o${BJ} \
            bmcbet.o${BJ} bmcbkp.o${BJ} bmcgrf.o${BJ} bmcnum.o${BJ} \
            bmcspt.o${BJ} bmcwin.o${BJ} bmdam.o${BJ} bmdbhc.o${BJ} bmdfol.o${BJ} \
            bmdrgt.o${BJ} bmdrv.o${BJ} bmfdbk.o${BJ}\
             bmfint.o${BJ} bmfire.o${BJ} bmfmrt.o${BJ} bmin.o${BJ} bminit.o${BJ} \
            bmips.o${BJ} bmistd.o${BJ} bmkill.o${BJ} bmlite.o${BJ} bmmort.o${BJ} \
            bmobb.o${BJ} bmout.o${BJ} bmpher.o${BJ} bmppgt.o${BJ} bmppin.o${BJ} \
            bmpppt.o${BJ} bmpstc.o${BJ} bmqmrt.o${BJ} bmrann.o${BJ} bmrrgt.o${BJ} \
            bmrrpt.o${BJ} bmrrsr.o${BJ} bmsalv.o${BJ} bmsani.o${BJ} bmsdit.o${BJ} \
            bmsetp.o${BJ} bmslsh.o${BJ} bmsmgt.o${BJ} bmtrip.o${BJ} \
            gpget2.o${BJ}

ppso:  ${PPSO_OBJS}

ppdgf.o${BJ}:  ${PPSO_SRC}ppdgf.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPSO_SRC}ppdgf.f

ppregt.o${BJ}: ${PPSO_SRC}ppregt.f ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPSO_SRC}ppregt.f

varget.o${BJ}: ${PPNI_SRC}varget.f ${SO_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${PPNI_SRC}varget.f

varput.o${BJ}: ${PPNI_SRC}varput.f ${SO_COM}PRGPRM.F77
	${FC} -c ${FFLAGS} ${PPNI_SRC}varput.f

add1.o${BJ}:   ${PPBASE_SRC}add1.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}add1.f

alinit.o${BJ}: ${PPBASE_SRC}alinit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}alinit.f

alstd1.o${BJ}: ${PPBASE_SRC}alstd1.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}alstd1.f

alstd2.o${BJ}: ${PPBASE_SRC}alstd2.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}alstd2.f

bfread.o${BJ}: ${PPBASE_SRC}bfread.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}bfread.f

bfwrit.o${BJ}: ${PPBASE_SRC}bfwrit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}bfwrit.f

bygrps.o${BJ}: ${PPBASE_SRC}bygrps.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
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

chdsth.o${BJ}: ${PPBASE_SRC}chdsth.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chdsth.f

chget.o${BJ}:  ${PPBASE_SRC}chget.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}CALDEN.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}DBSTK.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chget.f

chput.o${BJ}:  ${PPBASE_SRC}chput.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}CALDEN.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}OUTCOM.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}DBSTK.F77 ${BASE_COM}GGCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chput.f

chread.o${BJ}: ${PPBASE_SRC}chread.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chread.f

chstsh.o${BJ}: ${PPBASE_SRC}chstsh.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chstsh.f

chwrit.o${BJ}: ${PPBASE_SRC}chwrit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}chwrit.f

cmadds.o${BJ}: ${PPBASE_SRC}cmadds.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCMAD.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}cmadds.f

cvget.o${BJ}:  ${PPBASE_SRC}cvget.f ${SO_COM}PRGPRM.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}cvget.f

cvput.o${BJ}:  ${PPBASE_SRC}cvput.f ${SO_COM}PRGPRM.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}cvput.f

cmprt2.o${BJ}: ${PPBASE_SRC}cmprt2.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}cmprt2.f

dstash.o${BJ}: ${PPBASE_SRC}dstash.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}dstash.f

ecnget.o${BJ}:  ${PPBASE_SRC}ecnget.f ${SO_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ecnget.f

ecnput.o${BJ}:  ${PPBASE_SRC}ecnput.f ${SO_COM}PRGPRM.F77 ${BASE_COM}ECNCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ecnput.f

evagrp.o${BJ}: ${PPBASE_SRC}evagrp.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77   \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}evagrp.f

getprt.o${BJ}: ${PPBASE_SRC}getprt.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77    \
          ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}getprt.f

getstd.o${BJ}: ${PPBASE_SRC}getstd.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPDNCM.F77 ${SO_COM}ESPARM.F77  \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77  \
          ${BASE_COM}CALCOM.F77 ${BASE_COM}CALDEN.F77 ${BASE_COM}ECON.F77    \
          ${BASE_COM}ESHAP.F77  ${BASE_COM}ESHAP2.F77 ${BASE_COM}ESCOMN.F77   \
          ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESTCOR.F77 ${BASE_COM}ESTREE.F77  \
          ${BASE_COM}HTCAL.F77  ${BASE_COM}MULTCM.F77 ${BASE_COM}OPCOM.F77  \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PDEN.F77   ${BASE_COM}PLOT.F77   \
          ${BASE_COM}VOLSTD.F77 ${BASE_COM}RANCOM.F77 ${BASE_COM}ESRNCM.F77  \
          ${BASE_COM}DBSTK.F77  ${BASE_COM}VARCOM.F77 ${BASE_COM}SUMTAB.F77 \
          ${BASE_COM}CWDCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}getstd.f

gpadd.o${BJ}:  ${PPBASE_SRC}gpadd.f ${BASE_COM}PPGPCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpadd.f

gpadsd.o${BJ}: ${PPBASE_SRC}gpadsd.f ${BASE_COM}PPGPCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpadsd.f

gpclos.o${BJ}: ${PPBASE_SRC}gpclos.f ${BASE_COM}PPGPCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpclos.f

gpcset.o${BJ}: ${PPBASE_SRC}gpcset.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpcset.f

gpcycl.o${BJ}: ${PPBASE_SRC}gpcycl.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpcycl.f

gpget.o${BJ}:  ${PPBASE_SRC}gpget.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
          ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpget.f

gpinit.o${BJ}: ${PPBASE_SRC}gpinit.f ${BASE_COM}PPGPCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpinit.f

gpload.o${BJ}: ${PPBASE_SRC}gpload.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpload.f

gpnew.o${BJ}:  ${PPBASE_SRC}gpnew.f ${BASE_COM}PPGPCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpnew.f

grcend.o${BJ}: ${PPBASE_SRC}grcend.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}grcend.f

groend.o${BJ}: ${PPBASE_SRC}groend.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77      \
          ${BASE_COM}OUTCOM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}groend.f

grstnd.o${BJ}: ${PPBASE_SRC}grstnd.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}grstnd.f

histrd.o${BJ}: ${PPBASE_SRC}histrd.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCISN.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}histrd.f

histwt.o${BJ}: ${PPBASE_SRC}histwt.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}histwt.f

hvalid.o${BJ}: ${PPBASE_SRC}hvalid.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvalid.f

hvaloc.o${BJ}: ${PPBASE_SRC}hvaloc.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77 ${BASE_COM}PPCNTL.F77  \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OPCOM.F77   \
          ${BASE_COM}PLOT.F77 ${BASE_COM}HVDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvaloc.f

hvccut.o${BJ}: ${PPBASE_SRC}hvccut.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}OPCOM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvccut.f

hvcntg.o${BJ}: ${PPBASE_SRC}hvcntg.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvcntg.f

hvdnsl.o${BJ}: ${PPBASE_SRC}hvdnsl.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}HVDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvdnsl.f

hvfxlb.o${BJ}: ${PPBASE_SRC}hvfxlb.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
          ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvfxlb.f

hvhrv1.o${BJ}:  ${PPBASE_SRC}hvhrv1.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}OPCOM.F77 ${BASE_COM}ECON.F77 ${BASE_COM}HVDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvhrv1.f

hvin.o${BJ}:    ${PPBASE_SRC}hvin.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77 ${BASE_COM}OPCOM.F77  \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvin.f

hvinit.o${BJ}:  ${PPBASE_SRC}hvinit.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}HVDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvinit.f

hvlbwt.o${BJ}:  ${PPBASE_SRC}hvlbwt.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvlbwt.f

hvnedn.o${BJ}:  ${PPBASE_SRC}hvnedn.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77   \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}HVDNCM.F77 ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvnedn.f

hvproj.o${BJ}:  ${PPBASE_SRC}hvproj.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}HVDNCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvproj.f

hvreps.o${BJ}:  ${PPBASE_SRC}hvreps.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvreps.f

hvsel.o${BJ}:   ${PPBASE_SRC}hvsel.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvsel.f

hvstsl.o${BJ}:  ${PPBASE_SRC}hvstsl.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvstsl.f

hvthn1.o${BJ}:  ${PPBASE_SRC}hvthn1.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}HVDNCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OPCOM.F77  \
           ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvthn1.f

hvupst.o${BJ}:  ${PPBASE_SRC}hvupst.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPHVCM.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hvupst.f

hxindx.o${BJ}:  ${PPBASE_SRC}hxindx.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}hxindx.f

ifread.o${BJ}:  ${PPBASE_SRC}ifread.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ifread.f

ifwrit.o${BJ}:  ${PPBASE_SRC}ifwrit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ifwrit.f

inscyc.o${BJ}:  ${PPBASE_SRC}inscyc.f ${SO_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}inscyc.f

instnd.o${BJ}:  ${PPBASE_SRC}instnd.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}instnd.f

lbrnls.o${BJ}:  ${PPBASE_SRC}lbrnls.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lbrnls.f

ldarea.o${BJ}:  ${PPBASE_SRC}ldarea.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ldarea.f

ldsum.o${BJ}:   ${PPBASE_SRC}ldsum.f ${SO_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77  \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77 \
           ${BASE_COM}SUMTAB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ldsum.f

lfread.o${BJ}:  ${PPBASE_SRC}lfread.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lfread.f

lfwrit.o${BJ}:  ${PPBASE_SRC}lfwrit.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lfwrit.f

lladd.o${BJ}:   ${PPBASE_SRC}lladd.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lladd.f

llconc.o${BJ}:  ${PPBASE_SRC}llconc.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llconc.f

llcopy.o${BJ}:  ${PPBASE_SRC}llcopy.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llcopy.f

lldele.o${BJ}:  ${PPBASE_SRC}lldele.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lldele.f

lldisp.o${BJ}:  ${PPBASE_SRC}lldisp.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lldisp.f

llerrp.o${BJ}:  ${PPBASE_SRC}llerrp.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llerrp.f

llinit.o${BJ}:  ${PPBASE_SRC}llinit.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llinit.f

lllist.o${BJ}:  ${PPBASE_SRC}lllist.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}lllist.f

llnew.o${BJ}:   ${PPBASE_SRC}llnew.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPLLCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llnew.f

llnext.o${BJ}:  ${PPBASE_SRC}llnext.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llnext.f

llnull.o${BJ}:  ${PPBASE_SRC}llnull.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llnull.f

llscrh.o${BJ}:  ${PPBASE_SRC}llscrh.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llscrh.f

llsetp.o${BJ}:  ${PPBASE_SRC}llsetp.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llsetp.f

llvalu.o${BJ}:  ${PPBASE_SRC}llvalu.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPLLCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}llvalu.f

mstime.o${BJ}:  ${PPBASE_SRC}mstime.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}mstime.f

mstrgt.o${BJ}:  ${PPBASE_SRC}mstrgt.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OPCOM.F77  \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}mstrgt.f

mstrit.o${BJ}:  ${PPBASE_SRC}mstrit.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}mstrit.f

mstrpt.o${BJ}:  ${PPBASE_SRC}mstrpt.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OPCOM.F77  \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}mstrpt.f

oneisn.o${BJ}:  ${PPBASE_SRC}oneisn.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}oneisn.f

onerpt.o${BJ}:  ${PPBASE_SRC}onerpt.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}onerpt.f

plbwts.o${BJ}:  ${PPBASE_SRC}plbwts.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}plbwts.f

pparps.o${BJ}:  ${PPBASE_SRC}pparps.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}pparps.f

ppauid.o${BJ}:  ${PPBASE_SRC}ppauid.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppauid.f

ppause.o${BJ}:  ${PPBASE_SRC}ppause.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPSPRD.F77 ${BASE_COM}PPSPNB.F77 \
           ${BASE_COM}PPSPLA.F77 ${BASE_COM}PPLLCM.F77 ${BASE_COM}HVDNCM.F77 \
           ${BASE_COM}PPMDCM.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppause.f

ppbrph.o${BJ}:  ${PPBASE_SRC}ppbrph.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppbrph.f

ppclop.o${BJ}:  ${PPBASE_SRC}ppclop.f ${SO_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppclop.f

ppcvut.o${BJ}:  ${PPBASE_SRC}ppcvut.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}CVCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppcvut.f

ppeatv.o${BJ}:  ${PPBASE_SRC}ppeatv.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppeatv.f

ppeblk.o${BJ}:  ${PPBASE_SRC}ppeblk.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPEXCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppeblk.f

ppecyc.o${BJ}:  ${PPBASE_SRC}ppecyc.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppecyc.f

ppeget.o${BJ}:  ${PPBASE_SRC}ppeget.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPSPRD.F77 ${BASE_COM}PPSPNB.F77 \
           ${BASE_COM}PPSPLA.F77 ${BASE_COM}PPLLCM.F77 ${BASE_COM}HVDNCM.F77 \
           ${BASE_COM}PPMDCM.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppeget.f

ppeopn.o${BJ}:  ${PPBASE_SRC}ppeopn.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppeopn.f

pperrp.o${BJ}:  ${PPBASE_SRC}pperrp.f ${SO_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}pperrp.f

ppevmi.o${BJ}:  ${PPBASE_SRC}ppevmi.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}OPCOM.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppevmi.f

ppin.o${BJ}:    ${PPBASE_SRC}ppin.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
           ${BASE_COM}OPCOM.F77 ${BASE_COM}PPHVCM.F77 ${BASE_COM}HVDNCM.F77  \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppin.f

ppinit.o${BJ}:  ${PPBASE_SRC}ppinit.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPDNCM.F77 ${BASE_COM}PPEXCM.F77 \
           ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppinit.f

ppintr.o${BJ}:  ${PPBASE_SRC}ppintr.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppintr.f

ppisn.o${BJ}:   ${PPBASE_SRC}ppisn.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCISN.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppisn.f

ppkey.o${BJ}:   ${PPBASE_SRC}ppkey.f ${BASE_COM}PPEXCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppkey.f

pplabs.o${BJ}:  ${PPBASE_SRC}pplabs.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}pplabs.f

ppldev.o${BJ}:  ${PPBASE_SRC}ppldev.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppldev.f

ppldx.o${BJ}:   ${PPBASE_SRC}ppldx.f ${BASE_COM}PPEXCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppldx.f

ppmain.o${BJ}:  ${PPBASE_SRC}ppmain.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 \
           ${BASE_COM}PLOT.F77 ${BASE_COM}ECON.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppmain.f

ppmclk.o${BJ}:  ${PPBASE_SRC}ppmclk.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppmclk.f

pppden.o${BJ}:  ${PPBASE_SRC}pppden.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}PDEN.F77 ${BASE_COM}PPDNCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}pppden.f

ppweig.o${BJ}:  ${PPBASE_SRC}ppweig.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ppweig.f

prtbrh.o${BJ}:  ${PPBASE_SRC}prtbrh.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}prtbrh.f

prtspa.o${BJ}:  ${PPBASE_SRC}prtspa.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}prtspa.f

putstd.o${BJ}:  ${PPBASE_SRC}putstd.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPDNCM.F77 ${SO_COM}ESPARM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}COEFFS.F77 ${BASE_COM}CONTRL.F77 \
           ${BASE_COM}CALCOM.F77 ${BASE_COM}CALDEN.F77 ${BASE_COM}ECON.F77   \
           ${BASE_COM}ESHAP.F77 ${BASE_COM}ESHAP2.F77 ${BASE_COM}ESCOMN.F77  \
           ${BASE_COM}ESCOM2.F77 ${BASE_COM}ESTCOR.F77 ${BASE_COM}ESTREE.F77 \
           ${BASE_COM}HTCAL.F77 ${BASE_COM}MULTCM.F77 ${BASE_COM}OPCOM.F77   \
           ${BASE_COM}OUTCOM.F77 ${BASE_COM}PDEN.F77 ${BASE_COM}PLOT.F77     \
           ${BASE_COM}VOLSTD.F77 ${BASE_COM}ESRNCM.F77 ${BASE_COM}RANCOM.F77 \
           ${BASE_COM}DBSTK.F77 ${BASE_COM}VARCOM.F77 ${BASE_COM}SUMTAB.F77 \
           ${BASE_COM}CWDCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}putstd.f

ritcol.o${BJ}:  ${PPBASE_SRC}ritcol.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}ritcol.f

rptwtr.o${BJ}:  ${PPBASE_SRC}rptwtr.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}rptwtr.f

sched.o${BJ}:   ${PPBASE_SRC}sched.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sched.f

selint.o${BJ}:  ${PPBASE_SRC}selint.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}selint.f

slcond.o${BJ}:  ${PPBASE_SRC}slcond.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OPCOM.F77 ${BASE_COM}PPHVCM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}slcond.f

sndtbs.o${BJ}:  ${PPBASE_SRC}sndtbs.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCISN.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sndtbs.f

spamgm.o${BJ}:  ${PPBASE_SRC}spamgm.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spamgm.f

sparls.o${BJ}:  ${PPBASE_SRC}sparls.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sparls.f

spasgn.o${BJ}:  ${PPBASE_SRC}spasgn.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spasgn.f

spbsrx.o${BJ}:  ${PPBASE_SRC}spbsrx.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spbsrx.f

spcntg.o${BJ}:  ${PPBASE_SRC}spcntg.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spcntg.f

spextr.o${BJ}:  ${PPBASE_SRC}spextr.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spextr.f

splaar.o${BJ}:  ${PPBASE_SRC}splaar.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splaar.f

splads.o${BJ}:  ${PPBASE_SRC}splads.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splads.f

splaex.o${BJ}:  ${PPBASE_SRC}splaex.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splaex.f

splait.o${BJ}:  ${PPBASE_SRC}splait.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splait.f

splalo.o${BJ}:  ${PPBASE_SRC}splalo.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splalo.f

splard.o${BJ}:  ${PPBASE_SRC}splard.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPLA.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}splard.f

spnbbd.o${BJ}:  ${PPBASE_SRC}spnbbd.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbbd.f

spnbed.o${BJ}:  ${PPBASE_SRC}spnbed.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbed.f

spnbex.o${BJ}:  ${PPBASE_SRC}spnbex.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbex.f

spnbit.o${BJ}:  ${PPBASE_SRC}spnbit.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbit.f

spnbrd.o${BJ}:  ${PPBASE_SRC}spnbrd.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPNB.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}spnbrd.f

sprdis.o${BJ}:  ${PPBASE_SRC}sprdis.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPRD.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sprdis.f

sprdit.o${BJ}:  ${PPBASE_SRC}sprdit.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPRD.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sprdit.f

sprdrd.o${BJ}:  ${PPBASE_SRC}sprdrd.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}PPSPRD.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}sprdrd.f

srtisn.o${BJ}:  ${PPBASE_SRC}srtisn.f
	${FC} -c ${FFLAGS} ${PPBASE_SRC}srtisn.f

stash.o${BJ}:   ${PPBASE_SRC}stash.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77  \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}stash.f

stoprt.o${BJ}:  ${PPBASE_SRC}stoprt.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}CONTRL.F77 ${BASE_COM}OUTCOM.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}stoprt.f

wrthst.o${BJ}:  ${PPBASE_SRC}wrthst.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PPCISN.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}wrthst.f

yields.o${BJ}:  ${PPBASE_SRC}yields.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
           ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77   \
           ${BASE_COM}PPCNTL.F77 ${BASE_COM}OUTCOM.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}yields.f

#
# Fire (FFE) PPE objects

fmppget.o${BJ}: \
${FIRE_PPE_SRC}fmppget.f \
${SO_COM}PRGPRM.F77 \
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
${SO_COM}PRGPRM.F77 \
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
${SO_COM}PRGPRM.F77 \
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

msppgt.o${BJ}: ${MIST_SRC}msppgt.f ${SO_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}msppgt.f

mspppt.o${BJ}: ${MIST_SRC}mspppt.f ${SO_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
          ${MIST_SRC}MISCOM.F77
	${FC} -c ${FFLAGS} ${MIST_SRC}mspppt.f

#
# Westwide Pine Beetle Objects

wwpb : ${WWPB_OBJS}

bmagdw.o${BJ}:   ${WWPB_SRC}bmagdw.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmagdw.f   

bmatct.o${BJ}:   ${WWPB_SRC}bmatct.f  \
                 ${SO_COM}PRGPRM.F77  ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmatct.f   

bmavg.o${BJ}:    ${WWPB_SRC}bmavg.f   \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmavg.f    

# This Inland California/Southern Cascades block data needs compiled only with CA code
bmblkdso.o${BJ}: ${WWPB_SRC}bmblkdso.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMRCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmblkdso.f 

bmcbet.o${BJ}:   ${WWPB_SRC}bmcbet.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcbet.f   

bmcbkp.o${BJ}:   ${WWPB_SRC}bmcbkp.f  \
                 ${SO_COM}PRGPRM.F77  ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcbkp.f   

bmcgrf.o${BJ}:   ${WWPB_SRC}bmcgrf.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcgrf.f   

bmcnum.o${BJ}:   ${WWPB_SRC}bmcnum.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcnum.f   

bmcspt.o${BJ}:   ${WWPB_SRC}bmcspt.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcspt.f   

bmcwin.o${BJ}:   ${WWPB_SRC}bmcwin.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcwin.f   

bmdam.o${BJ}:    ${WWPB_SRC}bmdam.f   \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}CONTRL.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdam.f    

bmdbhc.o${BJ}:   ${WWPB_SRC}bmdbhc.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdbhc.f   

bmdfol.o${BJ}:   ${WWPB_SRC}bmdfol.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdfol.f   

bmdrgt.o${BJ}:   ${WWPB_SRC}bmdrgt.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdrgt.f   

bmdrv.o${BJ}:    ${WWPB_SRC}bmdrv.f   \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdrv.f    

bmfdbk.o${BJ}:   ${WWPB_SRC}bmfdbk.f  \
                 ${WWPB_SRC}BMPRM.F77 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmfdbk.f   

bmfint.o${BJ}:   ${WWPB_SRC}bmfint.f  \
                 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMFCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmfint.f   

bmfire.o${BJ}:   ${WWPB_SRC}bmfire.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMFCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmfire.f   

bmfmrt.o${BJ}:   ${WWPB_SRC}bmfmrt.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMFCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmfmrt.f   

bmin.o${BJ}:     ${WWPB_SRC}bmin.f   \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}CONTRL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmin.f    

bminit.o${BJ}:   ${WWPB_SRC}bminit.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMRRCM.F77 \
                 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bminit.f   

bmips.o${BJ}:    ${WWPB_SRC}bmips.f   \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmips.f    

bmistd.o${BJ}:   ${WWPB_SRC}bmistd.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmistd.f   

bmkill.o${BJ}:   ${WWPB_SRC}bmkill.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}OUTCOM.F77 \
                 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmkill.f   

bmlite.o${BJ}:   ${WWPB_SRC}bmlite.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmlite.f   

bmmort.o${BJ}:   ${WWPB_SRC}bmmort.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmmort.f   

bmobb.o${BJ}:    ${WWPB_SRC}bmobb.f   \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmobb.f    

bmout.o${BJ}:    ${WWPB_SRC}bmout.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmout.f   

bmpher.o${BJ}:   ${WWPB_SRC}bmpher.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmpher.f   

bmppgt.o${BJ}:   ${WWPB_SRC}bmppgt.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmppgt.f   

bmppin.o${BJ}:   ${WWPB_SRC}bmppin.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77 \
                 ${WWPB_SRC}BMRRCM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmppin.f   

bmpppt.o${BJ}:   ${WWPB_SRC}bmpppt.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmpppt.f   

bmpstc.o${BJ}:   ${WWPB_SRC}bmpstc.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmpstc.f   

bmqmrt.o${BJ}:   ${WWPB_SRC}bmqmrt.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmqmrt.f   

bmrann.o${BJ}:   ${WWPB_SRC}bmrann.f ${WWPB_SRC}BMRCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmrann.f   

bmrrgt.o${BJ}:   ${WWPB_SRC}bmrrgt.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMRRCM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmrrgt.f   

bmrrpt.o${BJ}:   ${WWPB_SRC}bmrrpt.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMRRCM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmrrpt.f   

bmrrsr.o${BJ}:   ${WWPB_SRC}bmrrsr.f  \
                 ${SO_COM}PRGPRM.F77 ${WWPB_SRC}BMRRCM.F77 ${BASE_COM}CONTRL.F77 \
                 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmrrsr.f   

bmsalv.o${BJ}:   ${WWPB_SRC}bmsalv.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsalv.f   

bmsani.o${BJ}:   ${WWPB_SRC}bmsani.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsani.f   

bmsdit.o${BJ}:   ${WWPB_SRC}bmsdit.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}OUTCOM.F77 \
                 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MIST_SRC}MISCOM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMFCOM.F77 ${WWPB_SRC}BMRRCM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsdit.f   

bmsetp.o${BJ}:   ${WWPB_SRC}bmsetp.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}ARRAYS.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsetp.f   

bmslsh.o${BJ}:   ${WWPB_SRC}bmslsh.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmslsh.f   

bmsmgt.o${BJ}:   ${WWPB_SRC}bmsmgt.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsmgt.f   

bmtrip.o${BJ}:   ${WWPB_SRC}bmtrip.f  \
                 ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmtrip.f   

gpget2.o${BJ}:  ${PPBASE_SRC}gpget2.f ${SO_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77   \
                ${BASE_COM}PPGPCM.F77 ${BASE_COM}PPCNTL.F77
	${FC} -c ${FFLAGS} ${PPBASE_SRC}gpget2.f
