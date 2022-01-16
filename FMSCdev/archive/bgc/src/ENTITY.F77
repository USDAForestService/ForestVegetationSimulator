C
C  THIS IS A FILE OF THE VARIABLES FOR PLANT CHARACTERISTICS, ETC.
C        -- DWC, 7/93.
C 5/27/04 ajm  adding new variable sumwp_al.  This is the sum of daily SWPs to be 
C used in allocation routine.  Now, water stress year is user-defineable (parameter 
C B1(13,1).  We zero-out SUMWP on user-defineable water stress year, and assign 
C the accumulated SUMWPs to date (for the last 365 days) to this new var SUMWP_AL
C ("al" for "allocation routine") 
C 5/28 ADDING 2 NEW VARS.  WE ARE NOW ALSO BOOKKEEPPING PSN FOR THE USER-DEFINEABLE YEAR
C (DEFINED VIA B1(13,1).  NEW VARS ARE AFPSN2 AND AFMR2.
C ADDING VARs "COUNT" & "AFCOUNT", INDEXED BY ENTITY, FOR TEST OF NEW ALLOCATION FRACTION 
C LOGIC (WHEREBY WE ACCUMULATE STRESDAYS ON NEW USER-DEFINED YEAR.  COUNT CHECKS 
C TO SEE IF # OF DAYS ACCUMULATED IS CORREECT)AJM 6/2/04
C
      INTEGER SIZE, ND, NUMYRS
      REAL AREA
      PARAMETER (SIZE=1400)
      PARAMETER (AREA=10000.0)
      PARAMETER (ND=365)
      
      CHARACTER CARD*60
      CHARACTER*1 ID(SIZE), VID(SIZE)
      CHARACTER*4 SPP(SIZE), VSP(SIZE)
      LOGICAL LEAF_ON(SIZE), FROOT_ON(SIZE)
      INTEGER TLEAFON(SIZE), FLAG, TREENO(SIZE),ITON,ITOFF,IGON,IGOFF
      INTEGER NSTR, NUM, NUM2, ISOURCE, NT, NLF,ISON,ISOFF
      INTEGER NB,NVE, NR, IB, IBLB(SIZE), BGCISP(SIZE), INDEX(SIZE*2)
      REAL D(SIZE), BD(SIZE), H(SIZE), TPH(SIZE), PCOVER(SIZE)
      REAL CR(SIZE), VCOV(SIZE), VHT(SIZE)
      REAL SUMDG(SIZE), SUMHG(SIZE), PASSDG(SIZE), PASSHG(SIZE)
      REAL BA(SIZE), TOTBA, CCF(SIZE), SUMCCF, BARE, GROUND
      REAL LEAF(SIZE), STEM(SIZE), ROOT(SIZE), STAT(SIZE)
      REAL LA(SIZE), SUMLA, OCPNCY(SIZE), MAXCRHT, MINCRHT
      REAL INTERVAL(3,3), CW(SIZE), BETA(SIZE), EXPAND(SIZE) 
      REAL CBOT(SIZE), CTOP(SIZE), ZONE(SIZE*2), LLA(SIZE,SIZE*2)
      REAL LAI(SIZE,SIZE*2), BASE(SIZE,SIZE*2), ZCOVER(SIZE*2) 
      REAL SNOW, RAIN, SNWMELT, ZONELA(SIZE*2), FOLDENS(SIZE)
      REAL RATIO(SIZE), SITELAI
      REAL A_(SIZE),YRMRLF(SIZE),YRMRST(SIZE),YRMRRT(SIZE)

      INTEGER JD
      REAL TMAX, TMIN, RH, XRAD, PPT, TR

      REAL Z1(20)

      REAL ZQ1ABS(SIZE,SIZE*2), ZQ2ABS(SIZE,SIZE*2)
      REAL TZQ1(SIZE*2), TZQ2(SIZE*2)
      REAL ZQ1(SIZE*2), ZQ2(SIZE*2), ETZQ1(SIZE), ETZQ2(SIZE)
      REAL SUMCOV
      
c      REAL SWC, GRNDH20, MAXSWC, SNOWPACK, SITEWP
c      REAL BUCKET, VOLCONT, H2OMAX 
      REAL GSFINAL, TRANSP, DAYPSN, TOTGRESP, TOTMRESP, TOTRESP
      REAL ABGRESP, SUMPSN, SUMTRANS, SUMGRESP, SUMMRESP, DAYTRANS
      REAL SUMWP(SIZE), SUMPSI(SIZE), ETYPSN(SIZE), ETYTRANS(SIZE)
      REAL ETYGRESP(SIZE), ETYMRESP(SIZE), ETYZQ1(SIZE)
      REAL ETYLFGR(SIZE), ETYSTGR(SIZE), ETYFRGR(SIZE)
      REAL AFPSN(SIZE), AFTRANS(SIZE), AFGRESP(SIZE), AFMRESP(SIZE)
      REAL YRPSN, YRTRANS, YRMRESP, YRGRESP, XPSN, TRANS
      REAL GRLEAF, GRSTEM, GRFROOTS, TSTEM

      REAL LEAFCF(SIZE), STEMCF(SIZE), FROOTCF(SIZE)
      REAL O_D(SIZE), O_BD(SIZE), O_H(SIZE), O_CR(SIZE), O_TPH(SIZE)
      REAL O_BA(SIZE), O_TOTBA, O_PCOVER(SIZE), O_CW(SIZE)
      REAL O_LEAF(SIZE), O_STEM(SIZE), O_ROOT(SIZE)
      REAL O_LA(SIZE), O_SUMLA
      REAL S_LEAF(SIZE), S_STEM(SIZE), S_ROOT(SIZE)
      REAL S_D(SIZE), S_BD(SIZE), S_H(SIZE), S_CR(SIZE), S_TPH(SIZE)
    
      
      REAL TOLEAF(SIZE), TOSTEM(SIZE), TOFROOT(SIZE), TOTALTO(SIZE)

      INTEGER IDEADFLG(SIZE), O_NB,COUNT(SIZE),AFCOUNT(SIZE)

      INTEGER nsl, sl, maxsl, bestsl, tf1, tf2				!*!
      REAL swc(SIZE), swc2(SIZE), sd(SIZE), outflow(SIZE), snowpack	!*! 
      REAL sitewp(SIZE), htlimit					!*!
      REAL bucket(SIZE), h2omax(SIZE), swp(SIZE), sumsoilwp		!*!
      REAL obucket(SIZE)
c  added sumla_sh, la_sh, trans_sh, daytrans_sh, and sumtrans_sh, below. ajm 10/00
      REAL sumla_gr, sumla_sm, sumla_lg, sumla_sh			!*!
      REAL la_gr, la_sm, la_lg, la_sh					!*!
      REAL trans_gr, trans_sm, trans_lg, trans_sh			!*!
      REAL daytrans_gr, daytrans_sm, daytrans_lg, daytrans_sh			!*!
      REAL sumtrans_gr, sumtrans_sm, sumtrans_lg, sumtrans_sh			!*!
      REAL trans_lg1, trans_lg2, sumtrans_lg1, sumtrans_lg2		!*!
      REAL daytrans_lg1, daytrans_lg2					!*!
      REAL stress(SIZE),SUMWP_AL(SIZE),AFPSN2(SIZE),AFMR2(SIZE)		!*!
C
C ADDING FOR NEW WATER BUCKET METHOD. AJM 7/03
C
      REAL SUMRT,SUMRT_GR,SUMRT_SH,SUMRT_SM,SUMRT_LG

      COMMON /BLBDAT/ TREENO, IBLB, IB, NB, NVE, NR, BGCISP, FLAG,
     +  NSTR, NUM, NUM2, VSP, VHT, VCOV, VID, ISOURCE, NT, NLF,
     +  D, BD, H, TPH, PCOVER, SUMDG, SUMHG, PASSDG, PASSHG,
     +  LEAF_ON, FROOT_ON, TLEAFON, IDEADFLG, NUMYRS,ITON,ITOFF,
     +  CR, LEAF, STEM, ROOT, STAT,IGON,IGOFF,ISON,ISOFF,
     +  BA, TOTBA, CCF, SUMCCF, BARE, GROUND,
     +  LA, SUMLA, OCPNCY, MAXCRHT, MINCRHT,
     +  INTERVAL, CW, BETA, EXPAND,
     +  CBOT, CTOP, ZONE, INDEX,
     +  LAI, BASE, LLA, ZCOVER, ZONELA, FOLDENS,
     +  RATIO, SITELAI,A_,YRMRLF,YRMRST,YRMRRT,
C     +  SWC, GRNDH20, MAXSWC, SNOWPACK, SITEWP, SUMPSI,
C     +  BUCKET, VOLCONT, H2OMAX, SUMWP,
     +  SNOWPACK, SUMWP,
     +  ZQ1, ZQ2, ETZQ1, ETZQ2, ZQ1ABS, ZQ2ABS, TZQ1, TZQ2, SUMCOV,
     +  GSFINAL, TRANSP, DAYPSN, TOTGRESP, TOTMRESP, TOTRESP,
     +  ABGRESP, SUMPSN, SUMTRANS, SUMGRESP, SUMMRESP, DAYTRANS,
     +  ETYPSN, ETYTRANS, ETYGRESP, ETYMRESP, ETYZQ1,
     +  ETYLFGR, ETYSTGR, ETYFRGR,
     +  AFPSN, AFTRANS, AFGRESP, AFMRESP, 
     +  XPSN, TRANS, GRLEAF, GRSTEM, GRFROOTS,
     +  SNOW, RAIN, SNWMELT,
     +  YRPSN, YRTRANS, YRMRESP, YRGRESP, TSTEM,

     +  nsl, sl, maxsl, bestsl,swc, swc2, sd, outflow, 
     +  tf1, tf2,
     +  sitewp, htlimit,
     +  bucket, obucket, h2omax, swp, sumsoilwp,
c  added sumla_sh, la_sh, trans_sh, daytrans_sh, sumtrans_sh, below.  ajm 10/00
     +  sumla_gr, sumla_sm, sumla_lg, sumla_sh, sumpsi,
     +  la_gr, la_sm, la_lg, la_sh,
     +  trans_gr, trans_sm, trans_lg, trans_sh,
     +  daytrans_gr, daytrans_sm, daytrans_lg, daytrans_sh,
     +  sumtrans_gr, sumtrans_sm, sumtrans_lg, sumtrans_sh,
     +  trans_lg1, trans_lg2, sumtrans_lg1, sumtrans_lg2,
     +  daytrans_lg1, daytrans_lg2,
     +  stress,SUMWP_AL,AFPSN2,AFMR2,COUNT,AFCOUNT,
     +  SUMRT,SUMRT_GR,SUMRT_SH,SUMRT_SM,SUMRT_LG

      COMMON /BLBOLD/ O_D, O_BD, O_H, O_CR, O_TPH, O_BA, O_TOTBA, 
     +  O_PCOVER, O_LEAF, O_STEM, O_ROOT, O_LA, O_SUMLA, O_NB, O_CW

      COMMON /BLBSAVE/S_D, S_BD, S_H, S_CR, S_TPH, S_LEAF, S_STEM,
     +  S_ROOT

      COMMON /BLBAF/ LEAFCF, STEMCF, FROOTCF

      COMMON /BLBTOVR/ TOLEAF, TOSTEM, TOFROOT, TOTALTO
      
      COMMON /BLBCLM/ JD, TMAX, TMIN, RH, XRAD, PPT, TR

      COMMON /BLBZVAR/ Z1
      
      COMMON /BLBCHR/ CARD, ID, SPP

