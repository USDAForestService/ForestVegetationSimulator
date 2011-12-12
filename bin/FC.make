
ODBC = odbc

ifeq (${as.shared},1)
 SHARED = -shared 
 OUTSUFX = .so
 MAIN =
 PIC = -fPIC
else
 SHARED =
 OUTSUFX =
 MAIN = ../base/obj/main.o${BJ}
 PIC = 
endif



FC = gfortran ${PIC}
FCL = gfortran ${SHARED} -o
CC = gcc 

PC =
BJ =
I1 = -I
I2 =`echo " "-I`


  DBS_LINK =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsatrtls.o${BJ}    \
          ../dbs/obj/dbscase.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbscmpu.o${BJ}    \
          ../dbs/obj/dbsdiags.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsexecsql.o${BJ} \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsin.o${BJ}      \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbsprssql.o${BJ}  \
          ../dbs/obj/dbsstandin.o${BJ} \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbstrls.o${BJ}    \
          ../dbs/obj/dbscuts.o${BJ}    \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmdsnag.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmmort.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}  \
          ../dbs/obj/dbsecharv.o${BJ} \
          ../dbs/obj/dbshelpers.o${BJ}

  DBS_LINK_LS =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/ls/dbsatrtls.o${BJ}    \
          ../dbs/obj/ls/dbsstandin.o${BJ} \
          ../dbs/obj/ls/dbstrls.o${BJ}    \
          ../dbs/obj/ls/dbscuts.o${BJ}    \
          ../dbs/obj/ls/dbscase.o${BJ}    \
          ../dbs/obj/ls/dbscmpu.o${BJ}    \
          ../dbs/obj/ls/dbsdiags.o${BJ}   \
          ../dbs/obj/ls/dbsexecsql.o${BJ} \
          ../dbs/obj/ls/dbsin.o${BJ}      \
          ../dbs/obj/ls/dbsprssql.o${BJ}  \
          ../dbs/obj/ls/dbsfmdsnag.o${BJ} \
          ../dbs/obj/ls/dbsfmmort.o${BJ}  \
          ../dbs/obj/ls/dbsecharv.o${BJ}  \
          ../dbs/obj/ls/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ} 

  DBS_LINK_IE = libfvsSQL.so    \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/ie/dbsatrtls.o${BJ}    \
          ../dbs/obj/ie/dbsstandin.o${BJ} \
          ../dbs/obj/ie/dbstrls.o${BJ}    \
          ../dbs/obj/ie/dbscuts.o${BJ}    \
          ../dbs/obj/ie/dbscase.o${BJ}    \
          ../dbs/obj/ie/dbscmpu.o${BJ}    \
          ../dbs/obj/ie/dbsdiags.o${BJ}   \
          ../dbs/obj/ie/dbsexecsql.o${BJ} \
          ../dbs/obj/ie/dbsin.o${BJ}      \
          ../dbs/obj/ie/dbsprssql.o${BJ}  \
          ../dbs/obj/ie/dbsfmdsnag.o${BJ} \
          ../dbs/obj/ie/dbsfmmort.o${BJ}  \
          ../dbs/obj/ie/dbsecharv.o${BJ}  \
          ../dbs/obj/ie/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_IE = libfvsSQL.so    \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/acd/dbsatrtls.o${BJ}    \
          ../dbs/obj/acd/dbsstandin.o${BJ} \
          ../dbs/obj/acd/dbstrls.o${BJ}    \
          ../dbs/obj/acd/dbscuts.o${BJ}    \
          ../dbs/obj/acd/dbscase.o${BJ}    \
          ../dbs/obj/acd/dbscmpu.o${BJ}    \
          ../dbs/obj/acd/dbsdiags.o${BJ}   \
          ../dbs/obj/acd/dbsexecsql.o${BJ} \
          ../dbs/obj/acd/dbsin.o${BJ}      \
          ../dbs/obj/acd/dbsprssql.o${BJ}  \
          ../dbs/obj/acd/dbsfmdsnag.o${BJ} \
          ../dbs/obj/acd/dbsfmmort.o${BJ}  \
          ../dbs/obj/acd/dbsecharv.o${BJ}  \
          ../dbs/obj/acd/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_AK =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/ak/dbsatrtls.o${BJ}    \
          ../dbs/obj/ak/dbsstandin.o${BJ} \
          ../dbs/obj/ak/dbstrls.o${BJ}    \
          ../dbs/obj/ak/dbscuts.o${BJ}    \
          ../dbs/obj/ak/dbscase.o${BJ}    \
          ../dbs/obj/ak/dbscmpu.o${BJ}    \
          ../dbs/obj/ak/dbsdiags.o${BJ}   \
          ../dbs/obj/ak/dbsexecsql.o${BJ} \
          ../dbs/obj/ak/dbsin.o${BJ}      \
          ../dbs/obj/ak/dbsprssql.o${BJ}  \
          ../dbs/obj/ak/dbsfmdsnag.o${BJ} \
          ../dbs/obj/ak/dbsfmmort.o${BJ}  \
          ../dbs/obj/ak/dbsecharv.o${BJ}  \
          ../dbs/obj/ak/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_AN =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/an/dbsatrtls.o${BJ}    \
          ../dbs/obj/an/dbsstandin.o${BJ} \
          ../dbs/obj/an/dbstrls.o${BJ}    \
          ../dbs/obj/an/dbscuts.o${BJ}    \
          ../dbs/obj/an/dbscase.o${BJ}    \
          ../dbs/obj/an/dbscmpu.o${BJ}    \
          ../dbs/obj/an/dbsdiags.o${BJ}   \
          ../dbs/obj/an/dbsexecsql.o${BJ} \
          ../dbs/obj/an/dbsin.o${BJ}      \
          ../dbs/obj/an/dbsprssql.o${BJ}  \
          ../dbs/obj/an/dbsfmdsnag.o${BJ} \
          ../dbs/obj/an/dbsfmmort.o${BJ}  \
          ../dbs/obj/an/dbsecharv.o${BJ}  \
          ../dbs/obj/an/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_BM =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/bm/dbsatrtls.o${BJ}    \
          ../dbs/obj/bm/dbsstandin.o${BJ} \
          ../dbs/obj/bm/dbstrls.o${BJ}    \
          ../dbs/obj/bm/dbscuts.o${BJ}    \
          ../dbs/obj/bm/dbscase.o${BJ}    \
          ../dbs/obj/bm/dbscmpu.o${BJ}    \
          ../dbs/obj/bm/dbsdiags.o${BJ}   \
          ../dbs/obj/bm/dbsexecsql.o${BJ} \
          ../dbs/obj/bm/dbsin.o${BJ}      \
          ../dbs/obj/bm/dbsprssql.o${BJ}  \
          ../dbs/obj/bm/dbsfmdsnag.o${BJ} \
          ../dbs/obj/bm/dbsfmmort.o${BJ}  \
          ../dbs/obj/bm/dbsecharv.o${BJ}  \
          ../dbs/obj/bm/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_CA =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/ca/dbsatrtls.o${BJ}    \
          ../dbs/obj/ca/dbsstandin.o${BJ} \
          ../dbs/obj/ca/dbstrls.o${BJ}    \
          ../dbs/obj/ca/dbscuts.o${BJ}    \
          ../dbs/obj/ca/dbscase.o${BJ}    \
          ../dbs/obj/ca/dbscmpu.o${BJ}    \
          ../dbs/obj/ca/dbsdiags.o${BJ}   \
          ../dbs/obj/ca/dbsexecsql.o${BJ} \
          ../dbs/obj/ca/dbsin.o${BJ}      \
          ../dbs/obj/ca/dbsprssql.o${BJ}  \
          ../dbs/obj/ca/dbsfmdsnag.o${BJ} \
          ../dbs/obj/ca/dbsfmmort.o${BJ}  \
          ../dbs/obj/ca/dbsecharv.o${BJ}  \
          ../dbs/obj/ca/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_CR =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/cr/dbsatrtls.o${BJ}    \
          ../dbs/obj/cr/dbsstandin.o${BJ} \
          ../dbs/obj/cr/dbstrls.o${BJ}    \
          ../dbs/obj/cr/dbscuts.o${BJ}    \
          ../dbs/obj/cr/dbscase.o${BJ}    \
          ../dbs/obj/cr/dbscmpu.o${BJ}    \
          ../dbs/obj/cr/dbsdiags.o${BJ}   \
          ../dbs/obj/cr/dbsexecsql.o${BJ} \
          ../dbs/obj/cr/dbsin.o${BJ}      \
          ../dbs/obj/cr/dbsprssql.o${BJ}  \
          ../dbs/obj/cr/dbsfmdsnag.o${BJ} \
          ../dbs/obj/cr/dbsfmmort.o${BJ}  \
          ../dbs/obj/cr/dbsecharv.o${BJ}  \
          ../dbs/obj/cr/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_CRM =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/cr/dbscase.o${BJ}    \
          ../dbs/obj/cr/dbscmpu.o${BJ}    \
          ../dbs/obj/cr/dbsdiags.o${BJ}   \
          ../dbs/obj/cr/dbsexecsql.o${BJ} \
          ../dbs/obj/cr/dbsin.o${BJ}      \
          ../dbs/obj/cr/dbsprssql.o${BJ}  \
          ../dbs/obj/cr/dbsfmdsnag.o${BJ} \
          ../dbs/obj/cr/dbsfmmort.o${BJ}  \
          ../dbs/obj/cr/dbsecharv.o${BJ}  \
          ../dbs/obj/cr/dbshelpers.o${BJ} \
          ../metric/dbs/obj/cr/dbsatrtls.o${BJ}    \
          ../metric/dbs/obj/cr/dbscuts.o${BJ}    \
          ../metric/dbs/obj/cr/dbsfmfuel.o${BJ}  \
          ../metric/dbs/obj/cr/dbsfuels.o${BJ}   \
          ../metric/dbs/obj/cr/dbsmis.o${BJ}    \
          ../metric/dbs/obj/cr/dbsstandin.o${BJ} \
          ../metric/dbs/obj/cr/dbssumry.o${BJ}   \
          ../metric/dbs/obj/cr/dbstrls.o${BJ}    \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_CS =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/cs/dbsatrtls.o${BJ}    \
          ../dbs/obj/cs/dbsstandin.o${BJ} \
          ../dbs/obj/cs/dbstrls.o${BJ}    \
          ../dbs/obj/cs/dbscuts.o${BJ}    \
          ../dbs/obj/cs/dbscase.o${BJ}    \
          ../dbs/obj/cs/dbscmpu.o${BJ}    \
          ../dbs/obj/cs/dbsdiags.o${BJ}   \
          ../dbs/obj/cs/dbsexecsql.o${BJ} \
          ../dbs/obj/cs/dbsin.o${BJ}      \
          ../dbs/obj/cs/dbsprssql.o${BJ}  \
          ../dbs/obj/cs/dbsfmdsnag.o${BJ} \
          ../dbs/obj/cs/dbsfmmort.o${BJ}  \
          ../dbs/obj/cs/dbsecharv.o${BJ}  \
          ../dbs/obj/cs/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_EM =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/em/dbsatrtls.o${BJ}    \
          ../dbs/obj/em/dbsstandin.o${BJ} \
          ../dbs/obj/em/dbstrls.o${BJ}    \
          ../dbs/obj/em/dbscuts.o${BJ}    \
          ../dbs/obj/em/dbscase.o${BJ}    \
          ../dbs/obj/em/dbscmpu.o${BJ}    \
          ../dbs/obj/em/dbsdiags.o${BJ}   \
          ../dbs/obj/em/dbsexecsql.o${BJ} \
          ../dbs/obj/em/dbsin.o${BJ}      \
          ../dbs/obj/em/dbsprssql.o${BJ}  \
          ../dbs/obj/em/dbsfmdsnag.o${BJ} \
          ../dbs/obj/em/dbsfmmort.o${BJ}  \
          ../dbs/obj/em/dbsecharv.o${BJ}  \
          ../dbs/obj/em/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_NE =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/ne/dbsatrtls.o${BJ}    \
          ../dbs/obj/ne/dbsstandin.o${BJ} \
          ../dbs/obj/ne/dbstrls.o${BJ}    \
          ../dbs/obj/ne/dbscuts.o${BJ}    \
          ../dbs/obj/ne/dbscase.o${BJ}    \
          ../dbs/obj/ne/dbscmpu.o${BJ}    \
          ../dbs/obj/ne/dbsdiags.o${BJ}   \
          ../dbs/obj/ne/dbsexecsql.o${BJ} \
          ../dbs/obj/ne/dbsin.o${BJ}      \
          ../dbs/obj/ne/dbsprssql.o${BJ}  \
          ../dbs/obj/ne/dbsfmdsnag.o${BJ} \
          ../dbs/obj/ne/dbsfmmort.o${BJ}  \
          ../dbs/obj/ne/dbsecharv.o${BJ}  \
          ../dbs/obj/ne/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_PN =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/pn/dbsatrtls.o${BJ}    \
          ../dbs/obj/pn/dbsstandin.o${BJ} \
          ../dbs/obj/pn/dbstrls.o${BJ}    \
          ../dbs/obj/pn/dbscuts.o${BJ}    \
          ../dbs/obj/pn/dbscase.o${BJ}    \
          ../dbs/obj/pn/dbscmpu.o${BJ}    \
          ../dbs/obj/pn/dbsdiags.o${BJ}   \
          ../dbs/obj/pn/dbsexecsql.o${BJ} \
          ../dbs/obj/pn/dbsin.o${BJ}      \
          ../dbs/obj/pn/dbsprssql.o${BJ}  \
          ../dbs/obj/pn/dbsfmdsnag.o${BJ} \
          ../dbs/obj/pn/dbsfmmort.o${BJ}  \
          ../dbs/obj/pn/dbsecharv.o${BJ}  \
          ../dbs/obj/pn/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_SN =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/sn/dbsatrtls.o${BJ}    \
          ../dbs/obj/sn/dbsstandin.o${BJ} \
          ../dbs/obj/sn/dbstrls.o${BJ}    \
          ../dbs/obj/sn/dbscuts.o${BJ}    \
          ../dbs/obj/sn/dbscase.o${BJ}    \
          ../dbs/obj/sn/dbscmpu.o${BJ}    \
          ../dbs/obj/sn/dbsdiags.o${BJ}   \
          ../dbs/obj/sn/dbsexecsql.o${BJ} \
          ../dbs/obj/sn/dbsin.o${BJ}      \
          ../dbs/obj/sn/dbsprssql.o${BJ}  \
          ../dbs/obj/sn/dbsfmdsnag.o${BJ} \
          ../dbs/obj/sn/dbsfmmort.o${BJ}  \
          ../dbs/obj/sn/dbsecharv.o${BJ}  \
          ../dbs/obj/sn/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_SO =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/so/dbsatrtls.o${BJ}    \
          ../dbs/obj/so/dbsstandin.o${BJ} \
          ../dbs/obj/so/dbstrls.o${BJ}    \
          ../dbs/obj/so/dbscuts.o${BJ}    \
          ../dbs/obj/so/dbscase.o${BJ}    \
          ../dbs/obj/so/dbscmpu.o${BJ}    \
          ../dbs/obj/so/dbsdiags.o${BJ}   \
          ../dbs/obj/so/dbsexecsql.o${BJ} \
          ../dbs/obj/so/dbsin.o${BJ}      \
          ../dbs/obj/so/dbsprssql.o${BJ}  \
          ../dbs/obj/so/dbsfmdsnag.o${BJ} \
          ../dbs/obj/so/dbsfmmort.o${BJ}  \
          ../dbs/obj/so/dbsecharv.o${BJ}  \
          ../dbs/obj/so/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_TT =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/tt/dbsatrtls.o${BJ}    \
          ../dbs/obj/tt/dbsstandin.o${BJ} \
          ../dbs/obj/tt/dbstrls.o${BJ}    \
          ../dbs/obj/tt/dbscuts.o${BJ}    \
          ../dbs/obj/tt/dbscase.o${BJ}    \
          ../dbs/obj/tt/dbscmpu.o${BJ}    \
          ../dbs/obj/tt/dbsdiags.o${BJ}   \
          ../dbs/obj/tt/dbsexecsql.o${BJ} \
          ../dbs/obj/tt/dbsin.o${BJ}      \
          ../dbs/obj/tt/dbsprssql.o${BJ}  \
          ../dbs/obj/tt/dbsfmdsnag.o${BJ} \
          ../dbs/obj/tt/dbsfmmort.o${BJ}  \
          ../dbs/obj/tt/dbsecharv.o${BJ}  \
          ../dbs/obj/tt/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_UT =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/ut/dbsatrtls.o${BJ}    \
          ../dbs/obj/ut/dbsstandin.o${BJ} \
          ../dbs/obj/ut/dbstrls.o${BJ}    \
          ../dbs/obj/ut/dbscuts.o${BJ}    \
          ../dbs/obj/ut/dbscase.o${BJ}    \
          ../dbs/obj/ut/dbscmpu.o${BJ}    \
          ../dbs/obj/ut/dbsdiags.o${BJ}   \
          ../dbs/obj/ut/dbsexecsql.o${BJ} \
          ../dbs/obj/ut/dbsin.o${BJ}      \
          ../dbs/obj/ut/dbsprssql.o${BJ}  \
          ../dbs/obj/ut/dbsfmdsnag.o${BJ} \
          ../dbs/obj/ut/dbsfmmort.o${BJ}  \
          ../dbs/obj/ut/dbsecharv.o${BJ}  \
          ../dbs/obj/ut/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_WC =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/wc/dbsatrtls.o${BJ}    \
          ../dbs/obj/wc/dbsstandin.o${BJ} \
          ../dbs/obj/wc/dbstrls.o${BJ}    \
          ../dbs/obj/wc/dbscuts.o${BJ}    \
          ../dbs/obj/wc/dbscase.o${BJ}    \
          ../dbs/obj/wc/dbscmpu.o${BJ}    \
          ../dbs/obj/wc/dbsdiags.o${BJ}   \
          ../dbs/obj/wc/dbsexecsql.o${BJ} \
          ../dbs/obj/wc/dbsin.o${BJ}      \
          ../dbs/obj/wc/dbsprssql.o${BJ}  \
          ../dbs/obj/wc/dbsfmdsnag.o${BJ} \
          ../dbs/obj/wc/dbsfmmort.o${BJ}  \
          ../dbs/obj/wc/dbsecharv.o${BJ}  \
          ../dbs/obj/wc/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

DBSppLk = ../dbs/obj/dbspusput.o${BJ}  \
          ../dbs/obj/dbspusget.o${BJ}  \
          ../dbs/obj/dbsppget.o${BJ}   \
          ../dbs/obj/dbsppput.o${BJ}   \
          ../dbs/obj/dbsbmmain.o${BJ}   \
          ../dbs/obj/dbsbmtree.o${BJ}   \
          ../dbs/obj/dbsbmbkp.o${BJ}   \
          ../dbs/obj/dbsbmvol.o${BJ}

dbs_compile :
	cd ../dbs/obj ; ${MAKE} FC="${FC}" dbs

dbs_compile_acd : dbs_compile
	cd ../dbs/obj/acd ; ${MAKE} FC="${FC}" dbs

dbs_compile_ls : dbs_compile
	cd ../dbs/obj/ls ; ${MAKE} FC="${FC}" dbs

dbs_compile_ie : dbs_compile
	cd ../dbs/obj/ie ; ${MAKE} FC="${FC}" dbs

dbs_compile_ak : dbs_compile
	cd ../dbs/obj/ak ; ${MAKE} FC="${FC}" dbs

dbs_compile_an : dbs_compile
	cd ../dbs/obj/an ; ${MAKE} FC="${FC}" dbs

dbs_compile_bm : dbs_compile
	cd ../dbs/obj/bm ; ${MAKE} FC="${FC}" dbs

dbs_compile_ca : dbs_compile
	cd ../dbs/obj/ca ; ${MAKE} FC="${FC}" dbs

dbs_compile_cr : dbs_compile
	cd ../dbs/obj/cr ; ${MAKE} FC="${FC}" dbs

dbs_compile_crm : dbs_compile
	cd ../metric/dbs/obj/cr ; ${MAKE} FC="${FC}" dbs

dbs_compile_cs : dbs_compile
	cd ../dbs/obj/cs ; ${MAKE} FC="${FC}" dbs

dbs_compile_em : dbs_compile
	cd ../dbs/obj/em ; ${MAKE} FC="${FC}" dbs

dbs_compile_ne : dbs_compile
	cd ../dbs/obj/ne ; ${MAKE} FC="${FC}" dbs

dbs_compile_pn : dbs_compile
	cd ../dbs/obj/pn ; ${MAKE} FC="${FC}" dbs

dbs_compile_sn : dbs_compile
	cd ../dbs/obj/sn ; ${MAKE} FC="${FC}" dbs

dbs_compile_so : dbs_compile
	cd ../dbs/obj/so ; ${MAKE} FC="${FC}" dbs

dbs_compile_tt : dbs_compile
	cd ../dbs/obj/tt ; ${MAKE} FC="${FC}" dbs

dbs_compile_ut : dbs_compile
	cd ../dbs/obj/ut ; ${MAKE} FC="${FC}" dbs

dbs_compile_wc : dbs_compile
	cd ../dbs/obj/wc ; ${MAKE} FC="${FC}" dbs


  DBS_LINK =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsatrtls.o${BJ}    \
          ../dbs/obj/dbscase.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbscmpu.o${BJ}    \
          ../dbs/obj/dbsdiags.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsexecsql.o${BJ} \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsin.o${BJ}      \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbsprssql.o${BJ}  \
          ../dbs/obj/dbsstandin.o${BJ} \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbstrls.o${BJ}    \
          ../dbs/obj/dbscuts.o${BJ}    \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmdsnag.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmmort.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}  \
          ../dbs/obj/dbsecharv.o${BJ} \
          ../dbs/obj/dbshelpers.o${BJ}

  DBS_LINK_LS =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/ls/dbsatrtls.o${BJ}    \
          ../dbs/obj/ls/dbsstandin.o${BJ} \
          ../dbs/obj/ls/dbstrls.o${BJ}    \
          ../dbs/obj/ls/dbscuts.o${BJ}    \
          ../dbs/obj/ls/dbscase.o${BJ}    \
          ../dbs/obj/ls/dbscmpu.o${BJ}    \
          ../dbs/obj/ls/dbsdiags.o${BJ}   \
          ../dbs/obj/ls/dbsexecsql.o${BJ} \
          ../dbs/obj/ls/dbsin.o${BJ}      \
          ../dbs/obj/ls/dbsprssql.o${BJ}  \
          ../dbs/obj/ls/dbsfmdsnag.o${BJ} \
          ../dbs/obj/ls/dbsfmmort.o${BJ}  \
          ../dbs/obj/ls/dbsecharv.o${BJ}  \
          ../dbs/obj/ls/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ} 

  DBS_LINK_IE = libfvsSQL.so    \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/ie/dbsatrtls.o${BJ}    \
          ../dbs/obj/ie/dbsstandin.o${BJ} \
          ../dbs/obj/ie/dbstrls.o${BJ}    \
          ../dbs/obj/ie/dbscuts.o${BJ}    \
          ../dbs/obj/ie/dbscase.o${BJ}    \
          ../dbs/obj/ie/dbscmpu.o${BJ}    \
          ../dbs/obj/ie/dbsdiags.o${BJ}   \
          ../dbs/obj/ie/dbsexecsql.o${BJ} \
          ../dbs/obj/ie/dbsin.o${BJ}      \
          ../dbs/obj/ie/dbsprssql.o${BJ}  \
          ../dbs/obj/ie/dbsfmdsnag.o${BJ} \
          ../dbs/obj/ie/dbsfmmort.o${BJ}  \
          ../dbs/obj/ie/dbsecharv.o${BJ}  \
          ../dbs/obj/ie/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_ACD = libfvsSQL.so    \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/acd/dbsatrtls.o${BJ}    \
          ../dbs/obj/acd/dbsstandin.o${BJ} \
          ../dbs/obj/acd/dbstrls.o${BJ}    \
          ../dbs/obj/acd/dbscuts.o${BJ}    \
          ../dbs/obj/acd/dbscase.o${BJ}    \
          ../dbs/obj/acd/dbscmpu.o${BJ}    \
          ../dbs/obj/acd/dbsdiags.o${BJ}   \
          ../dbs/obj/acd/dbsexecsql.o${BJ} \
          ../dbs/obj/acd/dbsin.o${BJ}      \
          ../dbs/obj/acd/dbsprssql.o${BJ}  \
          ../dbs/obj/acd/dbsfmdsnag.o${BJ} \
          ../dbs/obj/acd/dbsfmmort.o${BJ}  \
          ../dbs/obj/acd/dbsecharv.o${BJ}  \
          ../dbs/obj/acd/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_AK =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/ak/dbsatrtls.o${BJ}    \
          ../dbs/obj/ak/dbsstandin.o${BJ} \
          ../dbs/obj/ak/dbstrls.o${BJ}    \
          ../dbs/obj/ak/dbscuts.o${BJ}    \
          ../dbs/obj/ak/dbscase.o${BJ}    \
          ../dbs/obj/ak/dbscmpu.o${BJ}    \
          ../dbs/obj/ak/dbsdiags.o${BJ}   \
          ../dbs/obj/ak/dbsexecsql.o${BJ} \
          ../dbs/obj/ak/dbsin.o${BJ}      \
          ../dbs/obj/ak/dbsprssql.o${BJ}  \
          ../dbs/obj/ak/dbsfmdsnag.o${BJ} \
          ../dbs/obj/ak/dbsfmmort.o${BJ}  \
          ../dbs/obj/ak/dbsecharv.o${BJ}  \
          ../dbs/obj/ak/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_AN =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/an/dbsatrtls.o${BJ}    \
          ../dbs/obj/an/dbsstandin.o${BJ} \
          ../dbs/obj/an/dbstrls.o${BJ}    \
          ../dbs/obj/an/dbscuts.o${BJ}    \
          ../dbs/obj/an/dbscase.o${BJ}    \
          ../dbs/obj/an/dbscmpu.o${BJ}    \
          ../dbs/obj/an/dbsdiags.o${BJ}   \
          ../dbs/obj/an/dbsexecsql.o${BJ} \
          ../dbs/obj/an/dbsin.o${BJ}      \
          ../dbs/obj/an/dbsprssql.o${BJ}  \
          ../dbs/obj/an/dbsfmdsnag.o${BJ} \
          ../dbs/obj/an/dbsfmmort.o${BJ}  \
          ../dbs/obj/an/dbsecharv.o${BJ}  \
          ../dbs/obj/an/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_BM =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/bm/dbsatrtls.o${BJ}    \
          ../dbs/obj/bm/dbsstandin.o${BJ} \
          ../dbs/obj/bm/dbstrls.o${BJ}    \
          ../dbs/obj/bm/dbscuts.o${BJ}    \
          ../dbs/obj/bm/dbscase.o${BJ}    \
          ../dbs/obj/bm/dbscmpu.o${BJ}    \
          ../dbs/obj/bm/dbsdiags.o${BJ}   \
          ../dbs/obj/bm/dbsexecsql.o${BJ} \
          ../dbs/obj/bm/dbsin.o${BJ}      \
          ../dbs/obj/bm/dbsprssql.o${BJ}  \
          ../dbs/obj/bm/dbsfmdsnag.o${BJ} \
          ../dbs/obj/bm/dbsfmmort.o${BJ}  \
          ../dbs/obj/bm/dbsecharv.o${BJ}  \
          ../dbs/obj/bm/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_CA =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/ca/dbsatrtls.o${BJ}    \
          ../dbs/obj/ca/dbsstandin.o${BJ} \
          ../dbs/obj/ca/dbstrls.o${BJ}    \
          ../dbs/obj/ca/dbscuts.o${BJ}    \
          ../dbs/obj/ca/dbscase.o${BJ}    \
          ../dbs/obj/ca/dbscmpu.o${BJ}    \
          ../dbs/obj/ca/dbsdiags.o${BJ}   \
          ../dbs/obj/ca/dbsexecsql.o${BJ} \
          ../dbs/obj/ca/dbsin.o${BJ}      \
          ../dbs/obj/ca/dbsprssql.o${BJ}  \
          ../dbs/obj/ca/dbsfmdsnag.o${BJ} \
          ../dbs/obj/ca/dbsfmmort.o${BJ}  \
          ../dbs/obj/ca/dbsecharv.o${BJ}  \
          ../dbs/obj/ca/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_CR =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/cr/dbsatrtls.o${BJ}    \
          ../dbs/obj/cr/dbsstandin.o${BJ} \
          ../dbs/obj/cr/dbstrls.o${BJ}    \
          ../dbs/obj/cr/dbscuts.o${BJ}    \
          ../dbs/obj/cr/dbscase.o${BJ}    \
          ../dbs/obj/cr/dbscmpu.o${BJ}    \
          ../dbs/obj/cr/dbsdiags.o${BJ}   \
          ../dbs/obj/cr/dbsexecsql.o${BJ} \
          ../dbs/obj/cr/dbsin.o${BJ}      \
          ../dbs/obj/cr/dbsprssql.o${BJ}  \
          ../dbs/obj/cr/dbsfmdsnag.o${BJ} \
          ../dbs/obj/cr/dbsfmmort.o${BJ}  \
          ../dbs/obj/cr/dbsecharv.o${BJ}  \
          ../dbs/obj/cr/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_CRM =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/cr/dbscase.o${BJ}    \
          ../dbs/obj/cr/dbscmpu.o${BJ}    \
          ../dbs/obj/cr/dbsdiags.o${BJ}   \
          ../dbs/obj/cr/dbsexecsql.o${BJ} \
          ../dbs/obj/cr/dbsin.o${BJ}      \
          ../dbs/obj/cr/dbsprssql.o${BJ}  \
          ../dbs/obj/cr/dbsfmdsnag.o${BJ} \
          ../dbs/obj/cr/dbsfmmort.o${BJ}  \
          ../dbs/obj/cr/dbsecharv.o${BJ}  \
          ../dbs/obj/cr/dbshelpers.o${BJ} \
          ../metric/dbs/obj/cr/dbsatrtls.o${BJ}    \
          ../metric/dbs/obj/cr/dbscuts.o${BJ}    \
          ../metric/dbs/obj/cr/dbsfmfuel.o${BJ}  \
          ../metric/dbs/obj/cr/dbsfuels.o${BJ}   \
          ../metric/dbs/obj/cr/dbsmis.o${BJ}    \
          ../metric/dbs/obj/cr/dbsstandin.o${BJ} \
          ../metric/dbs/obj/cr/dbssumry.o${BJ}   \
          ../metric/dbs/obj/cr/dbstrls.o${BJ}    \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_CS =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/cs/dbsatrtls.o${BJ}    \
          ../dbs/obj/cs/dbsstandin.o${BJ} \
          ../dbs/obj/cs/dbstrls.o${BJ}    \
          ../dbs/obj/cs/dbscuts.o${BJ}    \
          ../dbs/obj/cs/dbscase.o${BJ}    \
          ../dbs/obj/cs/dbscmpu.o${BJ}    \
          ../dbs/obj/cs/dbsdiags.o${BJ}   \
          ../dbs/obj/cs/dbsexecsql.o${BJ} \
          ../dbs/obj/cs/dbsin.o${BJ}      \
          ../dbs/obj/cs/dbsprssql.o${BJ}  \
          ../dbs/obj/cs/dbsfmdsnag.o${BJ} \
          ../dbs/obj/cs/dbsfmmort.o${BJ}  \
          ../dbs/obj/cs/dbsecharv.o${BJ}  \
          ../dbs/obj/cs/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_EM =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/em/dbsatrtls.o${BJ}    \
          ../dbs/obj/em/dbsstandin.o${BJ} \
          ../dbs/obj/em/dbstrls.o${BJ}    \
          ../dbs/obj/em/dbscuts.o${BJ}    \
          ../dbs/obj/em/dbscase.o${BJ}    \
          ../dbs/obj/em/dbscmpu.o${BJ}    \
          ../dbs/obj/em/dbsdiags.o${BJ}   \
          ../dbs/obj/em/dbsexecsql.o${BJ} \
          ../dbs/obj/em/dbsin.o${BJ}      \
          ../dbs/obj/em/dbsprssql.o${BJ}  \
          ../dbs/obj/em/dbsfmdsnag.o${BJ} \
          ../dbs/obj/em/dbsfmmort.o${BJ}  \
          ../dbs/obj/em/dbsecharv.o${BJ}  \
          ../dbs/obj/em/dbshelpers.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_NE =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/ne/dbsatrtls.o${BJ}    \
          ../dbs/obj/ne/dbsstandin.o${BJ} \
          ../dbs/obj/ne/dbstrls.o${BJ}    \
          ../dbs/obj/ne/dbscuts.o${BJ}    \
          ../dbs/obj/ne/dbscase.o${BJ}    \
          ../dbs/obj/ne/dbscmpu.o${BJ}    \
          ../dbs/obj/ne/dbsdiags.o${BJ}   \
          ../dbs/obj/ne/dbsexecsql.o${BJ} \
          ../dbs/obj/ne/dbsin.o${BJ}      \
          ../dbs/obj/ne/dbsprssql.o${BJ}  \
          ../dbs/obj/ne/dbsfmdsnag.o${BJ} \
          ../dbs/obj/ne/dbsfmmort.o${BJ}  \
          ../dbs/obj/ne/dbsecharv.o${BJ}  \
          ../dbs/obj/ne/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_PN =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/pn/dbsatrtls.o${BJ}    \
          ../dbs/obj/pn/dbsstandin.o${BJ} \
          ../dbs/obj/pn/dbstrls.o${BJ}    \
          ../dbs/obj/pn/dbscuts.o${BJ}    \
          ../dbs/obj/pn/dbscase.o${BJ}    \
          ../dbs/obj/pn/dbscmpu.o${BJ}    \
          ../dbs/obj/pn/dbsdiags.o${BJ}   \
          ../dbs/obj/pn/dbsexecsql.o${BJ} \
          ../dbs/obj/pn/dbsin.o${BJ}      \
          ../dbs/obj/pn/dbsprssql.o${BJ}  \
          ../dbs/obj/pn/dbsfmdsnag.o${BJ} \
          ../dbs/obj/pn/dbsfmmort.o${BJ}  \
          ../dbs/obj/pn/dbsecharv.o${BJ}  \
          ../dbs/obj/pn/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_SN =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/sn/dbsatrtls.o${BJ}    \
          ../dbs/obj/sn/dbsstandin.o${BJ} \
          ../dbs/obj/sn/dbstrls.o${BJ}    \
          ../dbs/obj/sn/dbscuts.o${BJ}    \
          ../dbs/obj/sn/dbscase.o${BJ}    \
          ../dbs/obj/sn/dbscmpu.o${BJ}    \
          ../dbs/obj/sn/dbsdiags.o${BJ}   \
          ../dbs/obj/sn/dbsexecsql.o${BJ} \
          ../dbs/obj/sn/dbsin.o${BJ}      \
          ../dbs/obj/sn/dbsprssql.o${BJ}  \
          ../dbs/obj/sn/dbsfmdsnag.o${BJ} \
          ../dbs/obj/sn/dbsfmmort.o${BJ}  \
          ../dbs/obj/sn/dbsecharv.o${BJ}  \
          ../dbs/obj/sn/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_SO =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/so/dbsatrtls.o${BJ}    \
          ../dbs/obj/so/dbsstandin.o${BJ} \
          ../dbs/obj/so/dbstrls.o${BJ}    \
          ../dbs/obj/so/dbscuts.o${BJ}    \
          ../dbs/obj/so/dbscase.o${BJ}    \
          ../dbs/obj/so/dbscmpu.o${BJ}    \
          ../dbs/obj/so/dbsdiags.o${BJ}   \
          ../dbs/obj/so/dbsexecsql.o${BJ} \
          ../dbs/obj/so/dbsin.o${BJ}      \
          ../dbs/obj/so/dbsprssql.o${BJ}  \
          ../dbs/obj/so/dbsfmdsnag.o${BJ} \
          ../dbs/obj/so/dbsfmmort.o${BJ}  \
          ../dbs/obj/so/dbsecharv.o${BJ}  \
          ../dbs/obj/so/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_TT =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/tt/dbsatrtls.o${BJ}    \
          ../dbs/obj/tt/dbsstandin.o${BJ} \
          ../dbs/obj/tt/dbstrls.o${BJ}    \
          ../dbs/obj/tt/dbscuts.o${BJ}    \
          ../dbs/obj/tt/dbscase.o${BJ}    \
          ../dbs/obj/tt/dbscmpu.o${BJ}    \
          ../dbs/obj/tt/dbsdiags.o${BJ}   \
          ../dbs/obj/tt/dbsexecsql.o${BJ} \
          ../dbs/obj/tt/dbsin.o${BJ}      \
          ../dbs/obj/tt/dbsprssql.o${BJ}  \
          ../dbs/obj/tt/dbsfmdsnag.o${BJ} \
          ../dbs/obj/tt/dbsfmmort.o${BJ}  \
          ../dbs/obj/tt/dbsecharv.o${BJ}  \
          ../dbs/obj/tt/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_UT =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/ut/dbsatrtls.o${BJ}    \
          ../dbs/obj/ut/dbsstandin.o${BJ} \
          ../dbs/obj/ut/dbstrls.o${BJ}    \
          ../dbs/obj/ut/dbscuts.o${BJ}    \
          ../dbs/obj/ut/dbscase.o${BJ}    \
          ../dbs/obj/ut/dbscmpu.o${BJ}    \
          ../dbs/obj/ut/dbsdiags.o${BJ}   \
          ../dbs/obj/ut/dbsexecsql.o${BJ} \
          ../dbs/obj/ut/dbsin.o${BJ}      \
          ../dbs/obj/ut/dbsprssql.o${BJ}  \
          ../dbs/obj/ut/dbsfmdsnag.o${BJ} \
          ../dbs/obj/ut/dbsfmmort.o${BJ}  \
          ../dbs/obj/ut/dbsecharv.o${BJ}  \
          ../dbs/obj/ut/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

  DBS_LINK_WC =  libfvsSQL.so   \
          ../dbs/obj/dbsblkd.o${BJ}    \
          ../dbs/obj/dbsclose.o${BJ}   \
          ../dbs/obj/dbsevm.o${BJ}     \
          ../dbs/obj/dbsfmpf.o${BJ}    \
          ../dbs/obj/dbsfuels.o${BJ}   \
          ../dbs/obj/dbsgetid.o${BJ}   \
          ../dbs/obj/dbsinit.o${BJ}    \
          ../dbs/obj/dbsmis.o${BJ}    \
          ../dbs/obj/dbsopen.o${BJ}    \
          ../dbs/obj/dbsprs.o${BJ}     \
          ../dbs/obj/dbssumry.o${BJ}   \
          ../dbs/obj/dbstreesin.o${BJ} \
          ../dbs/obj/dbsstrclass.o${BJ}  \
          ../dbs/obj/dbsfmburn.o${BJ}  \
          ../dbs/obj/dbsfmcrpt.o${BJ}  \
          ../dbs/obj/dbsfmdwvol.o${BJ}  \
          ../dbs/obj/dbsfmdwcov.o${BJ}  \
          ../dbs/obj/dbsfmfuel.o${BJ}  \
          ../dbs/obj/dbsfmhrpt.o${BJ}  \
          ../dbs/obj/dbsfmssnag.o${BJ} \
          ../dbs/obj/dbsfmlink.o${BJ} \
          ../dbs/obj/dbsfmcanpr.o${BJ} \
          ../dbs/obj/wc/dbsatrtls.o${BJ}    \
          ../dbs/obj/wc/dbsstandin.o${BJ} \
          ../dbs/obj/wc/dbstrls.o${BJ}    \
          ../dbs/obj/wc/dbscuts.o${BJ}    \
          ../dbs/obj/wc/dbscase.o${BJ}    \
          ../dbs/obj/wc/dbscmpu.o${BJ}    \
          ../dbs/obj/wc/dbsdiags.o${BJ}   \
          ../dbs/obj/wc/dbsexecsql.o${BJ} \
          ../dbs/obj/wc/dbsin.o${BJ}      \
          ../dbs/obj/wc/dbsprssql.o${BJ}  \
          ../dbs/obj/wc/dbsfmdsnag.o${BJ} \
          ../dbs/obj/wc/dbsfmmort.o${BJ}  \
          ../dbs/obj/wc/dbsecharv.o${BJ}  \
          ../dbs/obj/wc/dbshelpers.o${BJ} \
          ../dbs/obj/dbsppget.o${BJ} \
          ../dbs/obj/dbsppput.o${BJ} \
          ../dbs/obj/dbsecsum.o${BJ}

DBSppLk = ../dbs/obj/dbspusput.o${BJ}  \
          ../dbs/obj/dbspusget.o${BJ}  \
          ../dbs/obj/dbsppget.o${BJ}   \
          ../dbs/obj/dbsppput.o${BJ}   \
          ../dbs/obj/dbsbmmain.o${BJ}   \
          ../dbs/obj/dbsbmtree.o${BJ}   \
          ../dbs/obj/dbsbmbkp.o${BJ}   \
          ../dbs/obj/dbsbmvol.o${BJ}

dbs_compile :
	cd ../dbs/obj ; ${MAKE} FC="${FC}" dbs

dbs_compile_ls : dbs_compile
	cd ../dbs/obj/ls ; ${MAKE} FC="${FC}" dbs

dbs_compile_ie : dbs_compile
	cd ../dbs/obj/ie ; ${MAKE} FC="${FC}" dbs

dbs_compile_acd : dbs_compile
	cd ../dbs/obj/acd ; ${MAKE} FC="${FC}" dbs

dbs_compile_ak : dbs_compile
	cd ../dbs/obj/ak ; ${MAKE} FC="${FC}" dbs

dbs_compile_an : dbs_compile
	cd ../dbs/obj/an ; ${MAKE} FC="${FC}" dbs

dbs_compile_bm : dbs_compile
	cd ../dbs/obj/bm ; ${MAKE} FC="${FC}" dbs

dbs_compile_ca : dbs_compile
	cd ../dbs/obj/ca ; ${MAKE} FC="${FC}" dbs

dbs_compile_cr : dbs_compile
	cd ../dbs/obj/cr ; ${MAKE} FC="${FC}" dbs

dbs_compile_crm : dbs_compile
	cd ../metric/dbs/obj/cr ; ${MAKE} FC="${FC}" dbs

dbs_compile_cs : dbs_compile
	cd ../dbs/obj/cs ; ${MAKE} FC="${FC}" dbs

dbs_compile_em : dbs_compile
	cd ../dbs/obj/em ; ${MAKE} FC="${FC}" dbs

dbs_compile_ne : dbs_compile
	cd ../dbs/obj/ne ; ${MAKE} FC="${FC}" dbs

dbs_compile_pn : dbs_compile
	cd ../dbs/obj/pn ; ${MAKE} FC="${FC}" dbs

dbs_compile_sn : dbs_compile
	cd ../dbs/obj/sn ; ${MAKE} FC="${FC}" dbs

dbs_compile_so : dbs_compile
	cd ../dbs/obj/so ; ${MAKE} FC="${FC}" dbs

dbs_compile_tt : dbs_compile
	cd ../dbs/obj/tt ; ${MAKE} FC="${FC}" dbs

dbs_compile_ut : dbs_compile
	cd ../dbs/obj/ut ; ${MAKE} FC="${FC}" dbs

dbs_compile_wc : dbs_compile
	cd ../dbs/obj/wc ; ${MAKE} FC="${FC}" dbs


