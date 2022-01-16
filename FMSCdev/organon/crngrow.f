C----------
C ORGANON $Id$
C----------
C     ORGANON GROWTH AND YIELD MODEL
C     SUBROUTINES INCLUDED:
C         CROWGRO
C         HCB_SWO
C         HCB_NWO
C         HCB_SMC
C         HCB_RAP
C         MCW_SWO
C         MCW_NWO
C         MCW_SMC
C         MCW_RAP
C         LCW_SWO
C         LCW_NWO
C         LCW_SMC
C         LCW_RAP
C         HLCW_SWO
C         HLCW_NWO
C         HLCW_SMC
C         HLCW_RAP
C         CALC_CC
C         CW_SWO
C         CW_NWO
C         CW_SMC
C         CW_RAP
C         CRNCLO
C         GET_CCFL
C         MAXHCB_SWO
C         MAXHCB_NWO
C         MAXHCB_SMC
C         MAXHCB_RAP
C**********************************************************************
      SUBROUTINE CROWGRO(VERSION,CYCLG,NTREES,IB,TDATAI,TDATAR,
     1                   SCR,GROWTH,MGEXP,DEADEXP,CCFLL1,CCFL1,CCFLL2,
     2                   CCFL2,SBA1,SBA2,SI_1,SI_2,CALIB,CCH)
C     DETERMINE 5-YR CROWN RECESSION
C**********************************************************************
      IMPLICIT NONE
      INTEGER*4 VERSION,CYCLG,NTREES,IB,TDATAI(2000,3),I,ISPGRP,J
      REAL*4 TDATAR(2000,8),SCR(2000,3),GROWTH(2000,4),MGEXP(2000),
     1       DEADEXP(2000),CCFLL1(51),CCFL1(500),CCFLL2(51),CCFL2(500),
     2       SBA1,SBA2,SI_1,SI_2,CALIB(6,18),CCH(41),OG1,OG2,PHT,PDBH,
     3       SCCFL1,HCB1,PCR1,PHCB1,HT,DBH,SCCFL2,HCB2,PCR2,PHCB2,HCBG,
     4       AHCB1,SHCB1,AHCB2,SHCB2,MAXHCB,CC2
      CALL OLDGRO(NTREES,IB,TDATAI,TDATAR,GROWTH,DEADEXP,-1.0,OG1)
      CALL OLDGRO(NTREES,IB,TDATAI,TDATAR,GROWTH,DEADEXP,0.0,OG2)
      DO I=1,NTREES
         IF(TDATAI(I,1).EQ. 0)CYCLE
         IF(CYCLG .EQ. 0)TDATAR(I,6)=TDATAR(I,3)
C
C        CALCULATE HCB START OF GROWTH
C
C
C        CALCULATE STARTING HEIGHT
C
         PHT=TDATAR(I,2)-GROWTH(I,1)
C
C        CALCULATE STARTING DBH
C
         PDBH=TDATAR(I,1)-GROWTH(I,2)
         ISPGRP=TDATAI(I,2)
         CALL GET_CCFL(PDBH,CCFLL1,CCFL1,SCCFL1)
         SELECT CASE(VERSION)
            CASE(1)
               CALL HCB_SWO(ISPGRP,PHT,PDBH,SCCFL1,SBA1,SI_1,SI_2,OG1,
     1                      HCB1)
               PCR1=1.0-HCB1/PHT
            CASE(2)
               CALL HCB_NWO(ISPGRP,PHT,PDBH,SCCFL1,SBA1,SI_1,SI_2,OG1,
     1                      HCB1)
               PCR1=CALIB(2,ISPGRP)*(1.0-HCB1/PHT)
            CASE(3)
               CALL HCB_SMC(ISPGRP,PHT,PDBH,SCCFL1,SBA1,SI_1,SI_2,OG1,
     1                      HCB1)
               PCR1=1.0-HCB1/PHT
            CASE(4)
               CALL HCB_RAP(ISPGRP,PHT,PDBH,SCCFL1,SBA1,SI_1,SI_2,OG1,
     1                      HCB1)
               PCR1=1.0-HCB1/PHT
         ENDSELECT
         PHCB1=(1.0-PCR1)*PHT
C
C        CALCULATE HCB END OF GROWTH
C
         HT=TDATAR(I,2)
         DBH=TDATAR(I,1)
         CALL GET_CCFL(DBH,CCFLL2,CCFL2,SCCFL2)
         SELECT CASE(VERSION)
            CASE(1)
               CALL HCB_SWO(ISPGRP,HT,DBH,SCCFL2,SBA2,SI_1,SI_2,OG2,
     1                      HCB2)
               CALL MAXHCB_SWO(ISPGRP,HT,SCCFL2,MAXHCB)
               PCR2=1.0-HCB2/HT
            CASE(2)
               CALL HCB_NWO(ISPGRP,HT,DBH,SCCFL2,SBA2,SI_1,SI_2,OG2,
     1                      HCB2)
               CALL MAXHCB_NWO(ISPGRP,HT,SCCFL2,MAXHCB)
               PCR2=CALIB(2,ISPGRP)*(1.0-HCB2/HT)
            CASE(3)
               CALL HCB_SMC(ISPGRP,HT,DBH,SCCFL2,SBA2,SI_1,SI_2,OG2,
     1                      HCB2)
               CALL MAXHCB_SMC(ISPGRP,HT,SCCFL2,MAXHCB)
               PCR2=1.0-HCB2/HT
            CASE(4)
               CALL HCB_RAP(ISPGRP,HT,DBH,SCCFL2,SBA2,SI_1,SI_2,OG2,
     1                      HCB2)
               CALL MAXHCB_RAP(ISPGRP,HT,SCCFL2,MAXHCB)
               PCR2=1.0-HCB2/HT
         ENDSELECT
         PHCB2=(1.0-PCR2)*HT
C
C        DETERMINE CROWN GROWTH
C
         HCBG=PHCB2-PHCB1
         IF(HCBG.LT.0.0) THEN
            HCBG=0.0
         ENDIF
         AHCB1=(1.0-TDATAR(I,3))*PHT
         SHCB1=(1.0-SCR(I,1))*PHT
         AHCB2=AHCB1+HCBG
         SHCB2=SHCB1+HCBG
         IF(AHCB1 .GT. SHCB1) THEN
            IF(AHCB1 .GT. SHCB2) THEN
               IF(SHCB2 .GE. MAXHCB) THEN
                  SCR(I,1)=1.0-MAXHCB/HT
                  TDATAR(I,3)=1.0-AHCB1/HT
               ELSE
                  SCR(I,1)=1.0-SHCB2/HT
                  TDATAR(I,3)=1.0-AHCB1/HT
               ENDIF
            ELSE
               TDATAR(I,3)=1.0-SHCB2/HT
               DO J=1,3
                  SCR(I,J)=0.0
               ENDDO
            ENDIF
         ELSE
            IF(AHCB1 .GE. MAXHCB) THEN
               TDATAR(I,3)=1.0-AHCB1/HT
            ELSEIF(AHCB2 .GE. MAXHCB) THEN
               TDATAR(I,3)=1.0-MAXHCB/HT
            ELSE
               TDATAR(I,3)=1.0-AHCB2/HT
            ENDIF
         ENDIF
      ENDDO
      CALL CRNCLO(0,0.0,VERSION,NTREES,TDATAI,TDATAR,SCR,MGEXP,
     1            CCH,CC2)
      RETURN
      END

C**********************************************************************
      SUBROUTINE HCB_SWO(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(18,7),B0,B1,B2,B3,
     1       B4,B5,B6
C
C  NEW HEIGHT TO CROWN BASE FOR UNDAMAGED TREES ONLY
C     (7 parameters - all species)
C
C     DF Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     GW Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     PP Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     SP Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     IC Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     WH Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     MD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     GC Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     TA Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     CL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     BL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     BO Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     WI Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C
      DATA HCBPAR/
     1          1.797136911,  3.451045887,  1.656364063,  3.785155749, !  DF,GW,PP,SP
     1          2.428285297,  0.0        ,  4.49102006 ,  0.0        , !  IC,WH,RC,PY
     1          2.955339267,  0.544237656,  0.833006499,  0.5376600543,!  MD,GC,TA,CL
     1          0.9411395642, 1.05786632 ,  2.60140655 ,  0.56713781  ,!  BL,WO,BO,RA
     1          0.0         , 0.0        ,                             !  PD,WI
C
     2         -0.010188791, -0.005985239, -0.002755463, -0.009012547, !  DF,GW,PP,SP
     2         -0.006882851,  0.0        ,  0.0        ,  0.0        , !  IC,WH,RC,PY
     2          0.0        , -0.020571754, -0.012984204, -0.018632397, !  MD,GC,TA,CL
     2         -0.00768402 ,  0.0        ,  0.0        , -0.010377976, !  BL,WO,BO,RA
     2          0.0        ,  0.0        ,                             !  PD,WI
C
     3         -0.003346230, -0.003211194,  0.0        , -0.003318574, !  DF,GW,PP,SP
     3         -0.002612590,  0.0        , -0.00132412 ,  0.0        , !  IC,WH,RC,PY
     3          0.0        , -0.004317523, -0.002704717,  0.0        , !  MD,GC,TA,CL
     3         -0.005476131, -0.00183283 , -0.002273616, -0.002066036, !  BL,WO,BO,RA
     3         -0.005666559, -0.005666559,                             !  PD,WI
C
     4         -0.412217810, -0.671479750, -0.568302547, -0.670270058, !  DF,GW,PP,SP
     4         -0.572782216,  0.0        , -1.01460531 ,  0.0        , !  IC,WH,RC,PY
     4         -0.798610738,  0.0        ,  0.0        ,  0.0        , !  MD,GC,TA,CL
     4          0.0        , -0.28644547 , -0.554980629,  0.0        , !  BL,WO,BO,RA
     4         -0.745540494, -0.745540494,                             !  PD,WI
C
     5          3.958656001,  3.931095518,  6.730693919,  2.758645081, !  DF,GW,PP,SP
     5          2.113378338,  4.801329946,  0.0        ,  2.030940382, !  IC,WH,RC,PY
     5          3.095269471,  3.132713612,  0.0        ,  0.0        , !  MD,GC,TA,CL
     5          0.0        ,  0.0        ,  0.0        ,  1.39796223 , !  BL,WO,BO,RA
     5          0.0        ,  0.0        ,                             !  PD,WI
C
     6          0.008526562,  0.003115567,  0.001852526,  0.0        , !  DF,GW,PP,SP
     6          0.008480754,  0.0        ,  0.01340624 ,  0.0        , !  IC,WH,RC,PY
     6          0.0        ,  0.0        ,  0.0        ,  0.0        , !  MD,GC,TA,CL
     6          0.0        ,  0.0        ,  0.0        ,  0.0        , !  BL,WO,BO,RA
     6          0.038476613,  0.038476613,                             !  PD,WI
C
     7          0.448909636,  0.516180892,  0.0        ,  0.841525071, !  DF,GW,PP,SP
     7          0.506226895,  0.0        ,  0.0        ,  0.0        , !  IC,WH,RC,PY
     7          0.700465646,  0.483748898,  0.2491242765, 0.0        , !  MD,GC,TA,CL
     7          0.0        ,  0.0        ,  0.0        ,  0.0        , !  BL,WO,BO,RA
     7          0.0        ,  0.0/                                     !  PD,WI
C
      B0=HCBPAR(ISPGRP,1)
      B1=HCBPAR(ISPGRP,2)
      B2=HCBPAR(ISPGRP,3)
      B3=HCBPAR(ISPGRP,4)
      B4=HCBPAR(ISPGRP,5)
      B5=HCBPAR(ISPGRP,6)
      B6=HCBPAR(ISPGRP,7)
      IF(ISPGRP .EQ. 3) THEN
         HCB=HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)
     1      +B5*SI_2+B6*OG**2))
      ELSE
         HCB=HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)
     1      +B5*SI_1+B6*OG**2))
      ENDIF
      RETURN
      END

C**********************************************************************
      SUBROUTINE HCB_NWO(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(11,7),B0,B1,B2,B3,
     1       B4,B5,B6
C
C  HEIGHT TO CROWN BASE (7 parameters - all species)
C
C     DF Coefficients from Zumrawi and Hann (1989) FRL Research Paper 52
C     GF Coefficients from Zumrawi and Hann (1989) FRL Research Paper 52
C     WH Coefficients from Johnson (2002) Willamette Industries Report
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     MD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     BL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     WI Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C
      DATA HCBPAR/
     1        1.94093    ,  1.04746    ,  1.92682    ,  4.49102006 ,   !  DF,GF,WH,RC
     1        0.0        ,  2.955339267,  0.9411395642, 1.05786632 ,   !  PY,MD,BL,WO
     1        0.56713781 ,  0.0        ,  0.0        ,                 !  RA,PD,WI
C
     2       -0.0065029  , -0.0066643  , -0.00280478 ,  0.0        ,   !  DF,GF,WH,RC
     2        0.0        ,  0.0        , -0.00768402 ,  0.0        ,   !  PY,MD,BL,WO
     2       -0.010377976,  0.0        ,  0.0        ,                 !  RA,PD,WI
C
     3       -0.0048737  , -0.0067129  , -0.0011939  , -0.00132412 ,   !  DF,GF,WH,RC
     3        0.0        ,  0.0        , -0.005476131, -0.00183283 ,   !  PY,MD,BL,WO
     3       -0.002066036, -0.005666559, -0.005666559,                 !  RA,PD,WI
C
     4       -0.261573   ,  0.0        , -0.513134   , -1.01460531 ,   !  DF,GF,WH,RC
     4        0.0        , -0.798610738,  0.0        , -0.28644547 ,   !  PY,MD,BL,WO
     4        0.0        , -0.745540494, -0.745540494,                 !  RA,PD,WI
C
     5        1.08785    ,  0.0        ,  3.68901    ,  0.0        ,   !  DF,GF,WH,RC
     5        2.030940382,  3.095269471,  0.0        ,  0.0        ,   !  PY,MD,BL,WO
     5        1.39796223 ,  0.0        ,  0.0        ,                 !  RA,PD,WI
C
     6        0.0        ,  0.0        ,  0.00742219 ,  0.01340624 ,   !  DF,GF,WH,RC
     6        0.0        ,  0.0        ,  0.0        ,  0.0        ,   !  PY,MD,BL,WO
     6        0.0        ,  0.038476613,  0.038476613,                 !  RA,PD,WI
C
     7        0.0        ,  0.0        ,  0.0        ,  0.0        ,   !  DF,GF,WH,RC
     7        0.0        ,  0.700465646,  0.0        ,  0.0        ,   !  PY,MD,BL,WO
     7        0.0        ,  0.0        ,  0.0/                         !  RA,PD,WI
C
      B0=HCBPAR(ISPGRP,1)
      B1=HCBPAR(ISPGRP,2)
      B2=HCBPAR(ISPGRP,3)
      B3=HCBPAR(ISPGRP,4)
      B4=HCBPAR(ISPGRP,5)
      B5=HCBPAR(ISPGRP,6)
      B6=HCBPAR(ISPGRP,7)
      IF(ISPGRP .EQ. 3) THEN
         HCB=HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)
     1      +B5*SI_2+B6*OG**2))
      ELSE
         HCB=HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)
     1      +B5*SI_1+B6*OG**2))
      ENDIF
      RETURN
      END

C**********************************************************************
      SUBROUTINE HCB_SMC(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(11,7),B0,B1,B2,B3,
     1       B4,B5,B6
C
C  HEIGHT TO CROWN BASE (7 parameters - all species)
C
C     DF Coefficients from Hann and Hanus (2004) FS 34: 1193-2003
C     GF Coefficients from Zumrawi and Hann (1989) FRL Research Paper 52
C     WH Coefficients from Johnson (2002) Willamette Industries Report
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     MD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     BL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     WI Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C
      DATA HCBPAR/                                                     !
     1        6.18464679 ,  1.04746    ,  1.92682    ,  4.49102006 ,   !  DF,GF,WH,RC
     1        0.0        ,  2.955339267,  0.9411395642, 1.05786632 ,   !  PY,MD,BL,WO
     1        0.56713781 ,  0.0        ,  0.0        ,                 !  RA,PD,WI
C
     2       -0.00328764 , -0.0066643  , -0.00280478 ,  0.0        ,   !  DF,GF,WH,RC
     2        0.0        ,  0.0        , -0.00768402 ,  0.0        ,   !  PY,MD,BL,WO
     2       -0.010377976,  0.0        ,  0.0        ,                 !  RA,PD,WI
C
     3       -0.00136555 , -0.0067129  , -0.0011939  , -0.00132412 ,   !  DF,GF,WH,RC
     3        0.0        ,  0.0        , -0.005476131, -0.00183283 ,   !  PY,MD,BL,WO
     3       -0.002066036, -0.005666559, -0.005666559,                 !  RA,PD,WI
C
     4       -1.19702220 ,  0.0        , -0.513134   , -1.01460531 ,   !  DF,GF,WH,RC
     4        0.0        , -0.798610738,  0.0        , -0.28644547 ,   !  PY,MD,BL,WO
     4        0.0        , -0.745540494, -0.745540494,                 !  RA,PD,WI
C
     5        3.17028263 ,  0.0        ,  3.68901    ,  0.0        ,   !  DF,GF,WH,RC
     5        2.030940382,  3.095269471,  0.0        ,  0.0        ,   !  PY,MD,BL,WO
     5        1.39796223 ,  0.0        ,  0.0        ,                 !  RA,PD,WI
C
     6        0.0        ,  0.0        ,  0.00742219 ,  0.01340624 ,   !  DF,GF,WH,RC
     6        0.0        ,  0.0        ,  0.0        ,  0.0        ,   !  PY,MD,BL,WO
     6        0.0        ,  0.038476613,  0.038476613,                 !  RA,PD,WI
C
     7        0.0        ,  0.0        ,  0.0        ,  0.0        ,   !  DF,GF,WH,RC
     7        0.0        ,  0.700465646,  0.0        ,  0.0        ,   !  PY,MD,BL,WO
     7        0.0        ,  0.0        ,  0.0/                         !  RA,PD,WI
C
      B0=HCBPAR(ISPGRP,1)
      B1=HCBPAR(ISPGRP,2)
      B2=HCBPAR(ISPGRP,3)
      B3=HCBPAR(ISPGRP,4)
      B4=HCBPAR(ISPGRP,5)
      B5=HCBPAR(ISPGRP,6)
      B6=HCBPAR(ISPGRP,7)
      IF(ISPGRP .EQ. 3) THEN
         HCB=HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)
     1      +B5*SI_2+B6*OG**2))
      ELSE
         HCB=HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)
     1      +B5*SI_1+B6*OG**2))
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE HCB_RAP(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(7,8),B0,B1,B2,B3,
     1       B4,B5,B6,K,SITE
C
C  HEIGHT TO CROWN BASE (7 parameters - all species)
C
C     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
C     DF Coefficients from Hann and Hanus (2004) FS 34: 1193-2003
C     WH Coefficients from Johnson (2002) Willamette Industries Report
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     BL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     PD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C     WI Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
C
      DATA HCBPAR/                                                     !
     1       3.73113020  ,  6.18464679 ,  1.92682    ,  4.49102006 ,   !  RA,DF,WH,RC
     1       0.9411395642,  0.0        ,  0.0        ,                 !  BL,PD,WI
C
     2      -0.021546486 , -0.00328764 , -0.00280478 ,  0.0        ,   !  RA,DF,WH,RC
     2      -0.00768402  ,  0.0        ,  0.0        ,                 !  BM,PD,WI
C
     3      -0.0016572840, -0.00136555 , -0.0011939  , -0.00132412 ,   !  RA,DF,WH,RC
     3      -0.005476131 , -0.005666559, -0.005666559,                 !  BL,PD,WI
C
     4      -1.0649544   , -1.19702220 , -0.513134   , -1.01460531 ,   !  RA,DF.WH,RC
     4       0.0         , -0.745540494, -0.745540494,                 !  BL,PD,WI
C
     5       7.47699601  ,  3.17028263 ,  3.68901    ,  0.0        ,   !  RA,DF,WH,RC
     5       0.0         ,  0.0        ,  0.0        ,                 !  BL,PD,WI
C
     6       0.0252953320,  0.0        ,  0.00742219 ,  0.01340624 ,   !  RA,DF,WH,RC
     6       0.0        ,   0.038476613,  0.038476613,                 !  BL,PD,WI
C
     7       0.0        ,   0.0        ,  0.0        ,  0.0        ,   !  RA,DF,WH,RC
     7       0.0        ,   0.0        ,  0.0        ,                 !  BL,PD,WI
C
     8       1.6        ,   0.0        ,  0.0        ,  0.0        ,   !  RA,DF,WH,RC
     8       0.0        ,   0.0        ,  0.0/                         !  BL,PD,WI
C
      B0=HCBPAR(ISPGRP,1)
      B1=HCBPAR(ISPGRP,2)
      B2=HCBPAR(ISPGRP,3)
      B3=HCBPAR(ISPGRP,4)
      B4=HCBPAR(ISPGRP,5)
      B5=HCBPAR(ISPGRP,6)
      B6=HCBPAR(ISPGRP,7)
      K=HCBPAR(ISPGRP,8)
      IF(ISPGRP .EQ. 1) THEN
         HCB=(HT-K)/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)
     1      +B5*SI_1+B6*OG**2))+K
      ELSE
         SITE=SI_2
         IF(ISPGRP .EQ. 3) THEN
            SITE=0.480 +( 1.110 * (SI_2+4.5))
         ENDIF
         HCB=HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)
     1      +B5*SITE+B6*OG**2))
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE MCW_SWO(ISPGRP,D,H,MCW)
C     DETERMINE MCW FOR EACH TREE
C**********************************************************************
C
C     MCW = MCW VALUE
C
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4    D,H,MCW,MCWPAR(18,4),B0,B1,B2,DBH,HT,PKDBH
C
C  MAXIMUM CROWN WIDTH (4 parameters - all species)
C
C     DF Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     GW Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     PP Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     SP Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     IC Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     WH Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     RC Coefficients from Smith (1966) Proc. 6th World Forestry Conference
C     PY Coefficients from WH
C     MD Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     GC Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     TA Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     CL Coefficients from TA
C     BL Coefficients from Ek (1974) School of Natural Res., U. Wisc., Forestry Res. Notes.
C     WO Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     BO Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     RA Coefficients from Smith (1966) Proc. 6th World Forestry Conference
C     PD Coefficients from GC
C     WI Coefficients from GC
C
      DATA MCWPAR/
     1            4.6366    ,   6.1880    ,   3.4835    ,   4.6600546 ,! DF,GW,PP,SP
     1            3.2837    ,   4.5652    ,   4.0       ,   4.5652    ,! IC,WH,RC,PY
     1            3.4298629 ,   2.9793895 ,   4.4443    ,   4.4443    ,! MD,GC,TA,CL
     1            4.0953    ,   3.0785639 ,   3.3625    ,   8.0       ,! BL,WO,BO,RA
     1            2.9793895 ,   2.9793895 ,                            !  PD,WI
C
     2            1.6078    ,   1.0069    ,   1.343     ,   1.0701859 ,! DF,GW,PP,SP
     2            1.2031    ,   1.4147    ,   1.65      ,   1.4147    ,! IC,WH,RC,PY
     2            1.3532302 ,   1.5512443 ,   1.7040    ,   1.7040    ,! MD,GC,TA,CL
     2            2.3849    ,   1.9242211 ,   2.0303    ,   1.53      ,! BL,WO,BO,RA
     2            1.5512443 ,   1.5512443 ,                            !  PD,WI
C
     3           -0.009625  ,   0.0       ,  -0.0082544 ,   0.0       ,! DF,GW,PP,SP
     3           -0.0071858 ,   0.0       ,   0.0       ,   0.0       ,! IC,WH,RC,PY
     3            0.0       ,  -0.01416129,  0.0        ,   0.0       ,! MD,GC,TA,CL
     3           -0.011630  ,   0.0       ,  -0.0073307 ,   0.0       ,! BL,WO,BO,RA
     3           -0.01416129,  -0.01416129,                            !  PD,WI
C
     4           88.52      , 999.99      ,  81.35      , 999.99      ,! DF,GW,PP,SP
     4           83.71      , 999.99      , 999.99      , 999.99      ,! IC,WH,RC,PY
     4          999.99      ,  54.77      , 999.99      , 999.99      ,! MD,GC,TA,CL
     4          102.53      , 999.99      , 138.93      , 999.99      ,! BL,WO,BO,RA
     4           54.77      ,  54.77/                                  !  PD,WI
C
      DBH=D
      HT=H
      B0=MCWPAR(ISPGRP,1)
      B1=MCWPAR(ISPGRP,2)
      B2=MCWPAR(ISPGRP,3)
      PKDBH=MCWPAR(ISPGRP,4)
      IF(DBH .GT. PKDBH) THEN
         DBH=PKDBH
      ENDIF
      IF(HT .LT. 4.501)THEN
           MCW=HT/4.5*B0
      ELSE
           MCW=B0+B1*DBH+B2*DBH**2
      ENDIF
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE MCW_NWO(ISPGRP,D,H,MCW)
C     DETERMINE MCW FOR EACH TREE
C**********************************************************************
C
C     MCW = MCW VALUE
C
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4    D,H,MCW,MCWPAR(11,4),B0,B1,B2,DBH,HT,PKDBH
C
C  MAXIMUM CROWN WIDTH (4 parameters - all species)
C
C     DF Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     GF Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     WH Coefficients from Johnson (2002) Willamette Industries Report
C     RC Coefficients from Smith (1966) Proc. 6th World Forestry Conference
C     PY Coefficients from WH of Paine and Hann (1982) FRL Research Paper 46
C     MD Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     BL Coefficients from Ek (1974) School of Natural Res., U. Wisc., Forestry Res. Notes.
C     WO Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     RA Coefficients from Smith (1966) Proc. 6th World Forestry Conference
C     PD Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46
C     WI Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46
C
      DATA MCWPAR/
     1            4.6198    ,   6.1880    ,   4.3586    ,   4.0       ,! DF,GF,WH,RC
     1            4.5652    ,   3.4298629 ,   4.0953    ,   3.0785639 ,! PY,MD,BL,WO
     1            8.0       ,   2.9793895 ,   2.9793895 ,              !  RA,PD,WI
C
     2            1.8426    ,   1.0069    ,   1.57458   ,   1.65      ,! DF,GF,WH,RC
     2            1.4147    ,   1.3532302 ,   2.3849    ,   1.9242211 ,! PY,MD,BL,WO
     2            1.53      ,   1.5512443 ,   1.5512443 ,              !  RA,PD,WI
C
     3           -0.011311  ,   0.0       ,   0.0       ,   0.0       ,! DF,GF,WH,RC
     3            0.0       ,   0.0       ,  -0.0102651 ,   0.0       ,! PY,MD,BL,WO
     3            0.0       ,  -0.01416129,  -0.01416129,              !  RA,PD,WI
C
     4           81.45      , 999.99      ,  76.70      , 999.99      ,! DF,GF,WH,RC
     4          999.99      , 999.99      , 102.53      , 999.99      ,! PY,MD,BL,WO
     4          999.99      ,  54.77      ,  54.77/                    !  RA,PD,WI
C
      DBH=D
      HT=H
      B0=MCWPAR(ISPGRP,1)
      B1=MCWPAR(ISPGRP,2)
      B2=MCWPAR(ISPGRP,3)
      PKDBH=MCWPAR(ISPGRP,4)
      IF(DBH .GT. PKDBH) THEN
         DBH=PKDBH
      ENDIF
      IF(HT .LT. 4.501)THEN
           MCW=HT/4.5*B0
      ELSE
           MCW=B0+B1*DBH+B2*DBH**2
      ENDIF
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE MCW_SMC(ISPGRP,D,H,MCW)
C     DETERMINE MCW FOR EACH TREE
C**********************************************************************
C
C     MCW = MCW VALUE
C
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4    D,H,MCW,MCWPAR(11,4),B0,B1,B2,DBH,HT,PKDBH
C
C  MAXIMUM CROWN WIDTH (4 parameters - all species)
C
C     DF Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     GF Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     WH Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     RC Coefficients from Smith (1966) Proc. 6th World Forestry Conference
C     PY Coefficients from WH of Paine and Hann (1982) FRL Research Paper 46
C     MD Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     BL Coefficients from Ek (1974) School of Natural Res., U. Wisc., Forestry Res. Notes.
C     WO Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     RA Coefficients from Smith (1966) Proc. 6th World Forestry Conference
C     PD Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46
C     WI Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46
C
      DATA MCWPAR/
     1            4.6198    ,   6.1880    ,   4.5652    ,   4.0       ,! DF,GF,WH,RC
     1            4.5652    ,   3.4298629 ,   4.0953    ,   3.0785639 ,! PY,MD,BL,WO
     1            8.0       ,   2.9793895 ,   2.9793895 ,              ! RA,PD,WI
C
     2            1.8426    ,   1.0069    ,   1.4147    ,   1.65      ,! DF,GF,WH,RC
     2            1.4147    ,   1.3532302 ,   2.3849    ,   1.9242211 ,! PY,MD,BL,WO
     2            1.53      ,   1.5512443 ,   1.5512443 ,              ! RA,PD,WI
C
     3           -0.011311  ,   0.0       ,   0.0       ,   0.0       ,! DF,GF,WH,RC
     3            0.0       ,   0.0       ,  -0.011630  ,   0.0       ,! PY,MD,BL,WO
     3            0.0       ,  -0.01416129,  -0.01416129,              ! RA,PD,WI
C
     4           81.45      , 999.99      , 999.99      , 999.99      ,! DF,GF,WH,RC
     4          999.99      , 999.99      , 102.53      , 999.99      ,! PY,MD,BL,WO
     4          999.99      ,  54.77      ,  54.77/                    ! RA,PD,WI
C
      DBH=D
      HT=H
      B0=MCWPAR(ISPGRP,1)
      B1=MCWPAR(ISPGRP,2)
      B2=MCWPAR(ISPGRP,3)
      PKDBH=MCWPAR(ISPGRP,4)
      IF(DBH .GT. PKDBH) THEN
         DBH=PKDBH
      ENDIF
      IF(HT .LT. 4.501)THEN
           MCW=HT/4.5*B0
      ELSE
           MCW=B0+B1*DBH+B2*DBH**2
      ENDIF
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE MCW_RAP(ISPGRP,D,H,MCW)
C     DETERMINE MCW FOR EACH TREE
C**********************************************************************
C
C     MCW = MCW VALUE
C
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4    D,H,MCW,MCWPAR(7,5),B0,B1,B2,K,DBH,HT,PKDBH
C
C  MAXIMUM CROWN WIDTH (4 parameters - all species)
C
C     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
C     DF Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     WH Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     RC Coefficients from Smith (1966) Proc. 6th World Forestry Conference
C     BL Coefficients from Ek (1974) School of Natural Res., U. Wisc., Forestry Res. Notes.
C     PD Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46
C     WI Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46
C
      DATA MCWPAR/
     1           2.320746348,   4.6198    ,   4.5652    ,   4.0       ,! RA,DF,WH,RC
     1           4.0953     ,   2.9793895 ,   2.9793895 ,              ! BL,PD,WI
C
     2           6.661401926,   1.8426    ,   1.4147    ,   1.65      ,! RA,DF,WH,RC
     2           2.3849     ,   1.5512443 ,   1.5512443 ,              ! BL,PD,WI
C
     3           0.0        ,  -0.011311  ,   0.0       ,   0.0       ,! RA,DF,WH,RC
     3          -0.011630   ,  -0.01416129,  -0.01416129,              ! BL,PD,WI
C
     4           0.6        ,   1.0       ,   1.0       ,   1.0       ,! RA,DF,WH,RC
     4           1.0        ,   1.0       ,   1.0       ,              ! BL,PD,WI
C
     5         999.99       ,  81.45      , 999.99      , 999.99      ,! RA,DF,WH,RC
     5         102.53       ,  54.77      ,  54.77/                    ! BL,PD,WI
C
      DBH=D
      HT=H
      B0=MCWPAR(ISPGRP,1)
      B1=MCWPAR(ISPGRP,2)
      B2=MCWPAR(ISPGRP,3)
      K=MCWPAR(ISPGRP,4)
      PKDBH=MCWPAR(ISPGRP,5)
      IF(DBH .GT. PKDBH) THEN
         DBH=PKDBH
      ENDIF
      IF(HT .LT. 4.501)THEN
           MCW=HT/4.5*B0
      ELSE
           MCW=B0+B1*DBH**K+B2*DBH**2
      ENDIF
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE LCW_SWO(ISPGRP,MCW,CR,SCR,DBH,HT,LCW)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 MCW,CR,SCR,DBH,HT,CL,LCW,LCWPAR(18,3),B1,B2,B3
C
C  LARGEST CROWN WIDTH (3 parameters - all species)
C
C     DF Coefficients from Hann (1997) FRL Research Contribution 17
C     GW Coefficients from Hann (1997) FRL Research Contribution 17
C     PP Coefficients from Hann (1997) FRL Research Contribution 17
C     SP Coefficients from Hann (1997) FRL Research Contribution 17
C     IC Coefficients from Hann (1997) FRL Research Contribution 17
C     WH Coefficients from Hann (1997) FRL Research Contribution 17
C     RC Coefficients from IC
C     PY Coefficients from WH
C     MD Coefficients from Hann (1997) FRL Research Contribution 17
C     GC Coefficients from Hann (1997) FRL Research Contribution 17
C     TA Coefficients from Hann (1997) FRL Research Contribution 17
C     CL Coefficients from Hann (1997) FRL Research Contribution 17
C     BL Coefficients from Hann (1997) FRL Research Contribution 17
C     WO Coefficients from Hann (1997) FRL Research Contribution 17
C     BO Coefficients from Hann (1997) FRL Research Contribution 17
C     RA Coefficients from Hann (1997) FRL Research Contribution 17
C     PD Coefficients from GC
C     WI Coefficients from GC
C
      DATA LCWPAR/
     1             0.0       ,  0.0       ,  0.355532  ,  0.0       ,  !  DF,GW,PP,SP
     1            -0.251389  ,  0.0       , -0.251389  ,  0.0       ,  !  IC,WH,RC,PY
     1             0.118621  ,  0.0       ,  0.0       ,  0.0       ,  !  MD,GC,TA,CL
     1             0.0       ,  0.364811  ,  0.0       ,  0.3227140 ,  !  BL,WO,BO,RA
     1             0.0       ,  0.0       ,                            !  PD,WI
C
     2             0.00371834,  0.00308402, 0.0        ,  0.00339675,  !  DF,GW,PP,SP
     2             0.00692512,  0.0       , 0.00692512 ,  0.0       ,  !  IC,WH,RC,PY
     2             0.00384872,  0.0       , 0.0111972  ,  0.0207676 ,  !  MD,GC,TA,CL
     2             0.0       ,  0.0       , 0.0        ,  0.0       ,  !  BL,WO,BO,RA
     2             0.0       ,  0.0       ,                            !  PD,WI
C
     3             0.808121  ,  0.0       , 0.0        ,  0.532418  ,  !  DF,GW,PP,SP
     3             0.985922  ,  0.0       , 0.985922   ,  0.0       ,  !  IC,WH,RC,PY
     3             0.0       ,  1.161440  , 0.0        ,  0.0       ,  !  MD,GC,TA,CL
     3             1.47018   ,  0.0       , 1.27196    ,  0.0       ,  !  BL,WO,BO,RA
     3             1.161440  ,  1.161440/                              !  PD,WI
C
      B1=LCWPAR(ISPGRP,1)
      B2=LCWPAR(ISPGRP,2)
      B3=LCWPAR(ISPGRP,3)
      IF(SCR .GT. CR) THEN
         CL=SCR*HT
         LCW=MCW*SCR**(B1+B2*CL+B3*(DBH/HT))
      ELSE
         CL=CR*HT
         LCW=MCW*CR**(B1+B2*CL+B3*(DBH/HT))
      ENDIF
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE LCW_NWO(ISPGRP,MCW,CR,SCR,DBH,HT,LCW)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 MCW,CR,SCR,DBH,HT,CL,LCW,LCWPAR(11,3),B1,B2,B3
C
C  LARGEST CROWN WIDTH (3 parameters - all species)
C
C     DF Coefficients from Hann (1997) FRL Research Contribution 17
C     GF Coefficients from Hann (1997) FRL Research Contribution 17
C     WH Coefficients from Johnson (2002) Willamette Industries Report
C     RC Coefficients from IC of Hann (1997) FRL Research Contribution 17
C     PY Coefficients from WH of Hann (1997) FRL Research Contribution 17
C     MD Coefficients from Hann (1997) FRL Research Contribution 17
C     BL Coefficients from Hann (1997) FRL Research Contribution 17
C     WO Coefficients from Hann (1997) FRL Research Contribution 17
C     RA Coefficients from Hann (1997) FRL Research Contribution 17
C     PD Coefficients from GC of Hann (1997) FRL Research Contribution 17
C     WI Coefficients from GC of Hann (1997) FRL Research Contribution 17
C
      DATA LCWPAR/
     1             0.0       ,  0.0       ,  0.105590  , -0.2513890 ,  !  DF,GF,WH,RC
     1             0.0       ,  0.118621  ,  0.0       ,  0.3648110 ,  !  PY,MD,BL,WO
     1             0.3227140 ,  0.0       ,  0.0       ,               !  RA,PD,WI
C
     2             0.00436324,  0.00308402,  0.0035662 ,  0.006925120, !  DF,GF,WH,RC
     2             0.0       ,  0.00384872,  0.0       ,  0.0       ,  !  PY,MD,BL,WO
     2             0.0       ,  0.0       ,  0.0       ,               !  RA,PD,WI
C
     3             0.6020020 ,  0.0       ,  0.0       ,  0.985922   , !  DF,GF,WH,RC
     3             0.0       ,  0.0       ,  1.470180  ,  0.0        , !  PY,MD,BL,WO
     3             0.0       ,  1.61440   ,  1.61440/                  !  RA,PD,WI
C
      B1=LCWPAR(ISPGRP,1)
      B2=LCWPAR(ISPGRP,2)
      B3=LCWPAR(ISPGRP,3)
      IF(SCR .GT. CR) THEN
         CL=SCR*HT
         LCW=MCW*SCR**(B1+B2*CL+B3*(DBH/HT))
      ELSE
         CL=CR*HT
         LCW=MCW*CR**(B1+B2*CL+B3*(DBH/HT))
      ENDIF
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE LCW_SMC(ISPGRP,MCW,CR,SCR,DBH,HT,LCW)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 MCW,CR,SCR,DBH,HT,CL,LCW,LCWPAR(11,3),B1,B2,B3
C
C  LARGEST CROWN WIDTH (3 parameters - all species)
C
C     DF Coefficients from Hann (1997) FRL Research Contribution 17
C     GF Coefficients from Hann (1997) FRL Research Contribution 17
C     WH Coefficients from Hann (1997) FRL Research Contribution 17
C     RC Coefficients from IC of Hann (1997) FRL Research Contribution 17
C     PY Coefficients from WH of Hann (1997) FRL Research Contribution 17
C     MD Coefficients from Hann (1997) FRL Research Contribution 17
C     BL Coefficients from Hann (1997) FRL Research Contribution 17
C     WO Coefficients from Hann (1997) FRL Research Contribution 17
C     RA Coefficients from Hann (1997) FRL Research Contribution 17
C     PD Coefficients from GC of Hann (1997) FRL Research Contribution 17
C     WI Coefficients from GC of Hann (1997) FRL Research Contribution 17
C
      DATA LCWPAR/
     1             0.0       ,  0.0       ,  0.0       , -0.2513890 ,  !  DF,GF,WH,RC
     1             0.0       ,  0.118621  ,  0.0       ,  0.3648110 ,  !  PY,MD,BL,WO
     1             0.3227140 ,  0.0       ,  0.0       ,               !  RA,PD,WI
C
     2             0.00436324,  0.00308402,  0.0       ,  0.006925120, !  DF,GF,WH,RC
     2             0.0       ,  0.00384872,  0.0       ,  0.0        , !  PY,MD,BL,WO
     2             0.0       ,  0.0       ,  0.0       ,               !  RA,PD,WI
C
     3             0.6020020 ,  0.0       ,  0.0       ,  0.985922   , !  DF,GF,WH,RC
     3             0.0       ,  0.0       ,  1.470180  ,  0.0        , !  PY,MD,BL,WO
     3             0.0       ,  1.61440   ,  1.61440/                  !  RA,PD,WI
C
      B1=LCWPAR(ISPGRP,1)
      B2=LCWPAR(ISPGRP,2)
      B3=LCWPAR(ISPGRP,3)
      IF(SCR .GT. CR) THEN
         CL=SCR*HT
         LCW=MCW*SCR**(B1+B2*CL+B3*(DBH/HT))
      ELSE
         CL=CR*HT
         LCW=MCW*CR**(B1+B2*CL+B3*(DBH/HT))
      ENDIF
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE LCW_RAP(ISPGRP,MCW,CR,SCR,DBH,HT,LCW)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 MCW,CR,SCR,DBH,HT,CL,LCW,LCWPAR(7,4),B0,B1,B2,B3
C
C  LARGEST CROWN WIDTH (3 parameters - all species)
C
C     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
C     DF Coefficients from Hann (1997) FRL Research Contribution 17
C     WH Coefficients from Hann (1997) FRL Research Contribution 17
C     RC Coefficients from IC of Hann (1997) FRL Research Contribution 17
C     BL Coefficients from Hann (1997) FRL Research Contribution 17
C     PD Coefficients from GC of Hann (1997) FRL Research Contribution 17
C     WI Coefficients from GC of Hann (1997) FRL Research Contribution 17
C
      DATA LCWPAR/
     1             0.78160725,  1.0       ,  1.0       ,  1.0        , !  RA,DF,WH,RC
     1             1.0       ,  1.0       ,  1.0       ,               !  BL,PD,WI
C
     2             0.44092737,  0.0       ,  0.0       , -0.2513890 ,  !  RA,DF,WH,RC
     2             0.0       ,  0.0       ,  0.0       ,               !  BM,PD,WI
C
     3             0.0       ,  0.00436324,  0.0       ,  0.006925120, !  RA,DF,WH,RC
     3             0.0       ,  0.0       ,  0.0       ,               !  BL,PD,WI
C
     4             0.0       ,  0.6020020 ,  0.0       ,  0.985922   , !  RA,DF,WH,RC
     4             1.470180  ,  1.61440   ,  1.61440/                  !  BL,PD,WI
C
      B0=LCWPAR(ISPGRP,1)
      B1=LCWPAR(ISPGRP,2)
      B2=LCWPAR(ISPGRP,3)
      B3=LCWPAR(ISPGRP,4)
      IF(SCR .GT. CR) THEN
         CL=SCR*HT
         LCW=B0*MCW*SCR**(B1+B2*CL+B3*(DBH/HT))
      ELSE
         CL=CR*HT
         LCW=B0*MCW*CR**(B1+B2*CL+B3*(DBH/HT))
      ENDIF
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE HLCW_SWO(ISPGRP,HT,CR,SCR,HLCW)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,CR,SCR,CL,HLCW,DACBPAR(18),B1
C
C  DISTANCE ABOVE CROWN BASE TO LARGEST CROWN WIDTH (1 parameter - all species)
C
C     DF Coefficients from Hann (1999) FS 45: 217-225
C     GW Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PP Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     SP Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     IC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RC Coefficients from IC
C     PY Coefficients from WH
C     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     GC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     TA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     CL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C
      DATA DACBPAR/
     1              0.062000, 0.028454, 0.05    , 0.05    , 0.20    ,  !  DF,GW,PP,SP,IC,
     1              0.209806, 0.20    , 0.209806, 0.0     , 0.0     ,  !  WH,RC,PY,MD,GC,
     1              0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,  !  TA,CL,BL,WO,BO,
     1              0.0     , 0.0     , 0.0/                           !  RA,PD,WI
C
      B1=DACBPAR(ISPGRP)
      IF(SCR .GT. CR) THEN
         CL=SCR*HT
      ELSE
         CL=CR*HT
      ENDIF
      HLCW=HT-(1.0-B1)*CL
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE HLCW_NWO(ISPGRP,HT,CR,SCR,HLCW)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,CR,SCR,CL,HLCW,DACBPAR(11),B1
C
C  DISTANCE ABOVE CROWN BASE TO LARGEST CROWN WIDTH (1 parameter - all species)
C
C     DF Coefficients from Hann (1999) FS 45: 217-225
C     GF Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WH Coefficients from Marshall, Johnson, and Hann (2003) CJFR 33: 2059-2066
C     RC Coefficients from WH of Hann and Hanus (2001) FRL Research Contribution 34
C     PY Coefficients from WH of Hann and Hanus (2001) FRL Research Contribution 34
C     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C
      DATA DACBPAR/
     1              0.062000, 0.028454, 0.355270, 0.209806, 0.209806,  !  DF,GF,WH,RC,PY
     1              0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,  !  MD,BL,WO,RA,PD
     1              0.0/                                               !  WI
C
      B1=DACBPAR(ISPGRP)
      IF(SCR .GT. CR) THEN
         CL=SCR*HT
      ELSE
         CL=CR*HT
      ENDIF
      HLCW=HT-(1.0-B1)*CL
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE HLCW_SMC(ISPGRP,HT,CR,SCR,HLCW)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,CR,SCR,CL,HLCW,DACBPAR(11),B1
C
C  DISTANCE ABOVE CROWN BASE TO LARGEST CROWN WIDTH (1 parameter - all species)
C
C     DF Coefficients from Hann (1999) FS 45: 217-225
C     GF Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RC Coefficients from WH
C     PY Coefficients from WH
C     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C
      DATA DACBPAR/
     1              0.062000, 0.028454, 0.209806, 0.209806, 0.209806,  !  DF,GF,WH,RC,PY
     1              0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,  !  MD,BL,WO,RA,PD
     1              0.0/                                               !  WI
C
      B1=DACBPAR(ISPGRP)
      IF(SCR .GT. CR) THEN
         CL=SCR*HT
      ELSE
         CL=CR*HT
      ENDIF
      HLCW=HT-(1.0-B1)*CL
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE HLCW_RAP(ISPGRP,HT,CR,SCR,HLCW)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,CR,SCR,CL,HLCW,DACBPAR(7,2),B1,B2
C
C  DISTANCE ABOVE CROWN BASE TO LARGEST CROWN WIDTH (1 parameter - all species)
C
C     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
C     DF Coefficients from Hann (1999) FS 45: 217-225
C     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RC Coefficients from WH
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C
      DATA DACBPAR/
     1          0.63619616, 0.062000, 0.209806, 0.209806, 0.0     ,  !  RA,DF,WH,RC,BL
     1          0.0       , 0.0     ,                                !  PD,WI
C
     2         -1.2180562 , 0.0     , 0.0     , 0.0     , 0.0     ,  !  RA,DF,WH,RC,BL
     2          0.0       , 0.0/                                     !  PD,WI
C
      B1=DACBPAR(ISPGRP,1)
      B2=DACBPAR(ISPGRP,2)
      IF(SCR .GT. CR) THEN
         CL=SCR*HT
      ELSE
         CL=CR*HT
      ENDIF
      HLCW=HT-(1.0-B1*EXP(B2*(1.0-HT/140.0)**3))*CL
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE CALC_CC(VERSION,ISPGRP,HLCW,LCW,HT,DBH,HCB,EXPAN,CCH)
      IMPLICIT NONE
      INTEGER*4 VERSION,ISPGRP,II,L
      REAL*4 HLCW,LCW,HT,DBH,HCB,EXPAN,CCH(41),XL,XHLCW,XLCW,CW,CA
      IF(HCB .GT. HLCW) THEN
         XHLCW=HCB
         SELECT CASE(VERSION)
            CASE(1)
               CALL CW_SWO(ISPGRP,HLCW,LCW,HT,DBH,XHLCW,XLCW)
            CASE(2)
               CALL CW_NWO(ISPGRP,HLCW,LCW,HT,DBH,XHLCW,XLCW)
            CASE(3)
               CALL CW_SMC(ISPGRP,HLCW,LCW,HT,DBH,XHLCW,XLCW)
            CASE(4)
               CALL CW_RAP(ISPGRP,HLCW,LCW,HT,DBH,XHLCW,XLCW)
         ENDSELECT
      ELSE
         XHLCW=HLCW
         XLCW=LCW
      ENDIF
      DO II=40,1,-1
         L=II-1
         XL=FLOAT(L)*(CCH(41)/40.0)
         IF(XL.LE.XHLCW) THEN
            CW=XLCW
         ELSE IF(XL .GT. XHLCW .AND. XL .LT. HT) THEN
            SELECT CASE(VERSION)
               CASE(1)
                  CALL CW_SWO(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
               CASE(2)
                  CALL CW_NWO(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
               CASE(3)
                  CALL CW_SMC(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
               CASE(4)
                  CALL CW_RAP(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
            ENDSELECT
         ELSE
            CW=0.0
         ENDIF
         CA=(CW**2)*(.001803*EXPAN)
         CCH(II)=CCH(II)+CA
      ENDDO
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE CW_SWO(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HLCW,LCW,HT,DBH,XL,CW,CWAPAR(18,3),B1,B2,B3,RP,RATIO
C
C  CROWN WIDTH ABOVE LARGEST CROWN WIDTH (3 parameters - all species)
C
C     DF Coefficients from Hann (1999) FS 45: 217-225
C     GW Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PP Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     SP Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     IC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RC Coefficients from IC
C     PY Coefficients from WH
C     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     GC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     TA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     CL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C
      DATA CWAPAR/
     1        0.929973,   0.999291,   0.755583,   0.755583,  0.629785, !  DF,GW,PP,SP,IC,
     1        0.629785,   0.629785,   0.629785,   0.5     ,  0.5     , !  WH,RC,PY,MD,GC,
     1        0.5     ,   0.5     ,   0.5     ,   0.5     ,  0.5     , !  TA,CL,BL,WO,BO,
     1        0.5     ,   0.5     ,   0.5     ,                        !  RA,PD,WI
C
     2       -0.135212,   0.0     ,   0.0     ,   0.0     ,   0.0     ,!  DF,GW,PP,SP,IC,
     2        0.0     ,   0.0     ,   0.0     ,   0.0     ,   0.0     ,!  WH,RC,PY,MD,GC,
     2        0.0     ,   0.0     ,   0.0     ,   0.0     ,   0.0     ,!  TA,CL,BL,WO,BO,
     2        0.0     ,   0.0     ,   0.0     ,                        !  RA,PD,WI
C
     3       -0.0157579, -0.0314603,   0.0    ,   0.0     ,   0.0     ,!  DF,GW,PP,SP,IC,
     3        0.0      ,  0.0      ,   0.0    ,   0.0     ,   0.0     ,!  WH,RC,PY,MD,GC,
     3        0.0      ,  0.0      ,   0.0    ,   0.0     ,   0.0     ,!  TA,CL,BL,WO,BO,
     3        0.0      ,  0.0      ,   0.0/                            !  RA,PD,WI
C
      B1=CWAPAR(ISPGRP,1)
      B2=CWAPAR(ISPGRP,2)
      B3=CWAPAR(ISPGRP,3)
      RP=(HT-XL)/(HT-HLCW)
      RATIO=HT/DBH
      IF(ISPGRP .EQ. 1) THEN
         IF(RATIO .GT. 50.0) RATIO=50.0
      ELSEIF(ISPGRP .EQ. 2) THEN
         IF(RATIO .GT. 31.0) RATIO=31.0
      ENDIF
      CW=LCW*RP**(B1+B2*SQRT(RP)+B3*(RATIO))
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE CW_NWO(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HLCW,LCW,HT,DBH,XL,CW,CWAPAR(11,3),B1,B2,B3,RP,RATIO
C
C  CROWN WIDTH ABOVE LARGEST CROWN WIDTH (3 parameters - all species)
C
C     DF Coefficients from Hann (1999) FS 45: 217-225
C     GF Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WH Coefficients from Marshall, Johnson, and Hann (2003) CJFR 33: 2059-2066
C     RC Coefficients from WH Hann and Hanus (2001) FRL Research Contribution 34
C     PY Coefficients from WH Hann and Hanus (2001) FRL Research Contribution 34
C     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C
      DATA CWAPAR/
     1              0.929973 ,  0.999291 ,  0.461782 ,   0.629785,     !  DF,GF,WH,RC
     1              0.629785,   0.5      ,  0.5      ,   0.5     ,     !  PY,MD,BL,WO
     1              0.5      ,  0.5      ,  0.5      ,                 !  RA,PD,WI
C
     2             -0.135212 ,  0.0      ,  0.552011 ,   0.0     ,     !  DF,GF,WH,RC
     2              0.0      ,  0.0      ,  0.0      ,   0.0     ,     !  PY,MD,BL,WO
     2              0.0      ,  0.0      ,  0.0      ,                 !  RA,PD,WI
C
     3             -0.0157579, -0.0314603,  0.0      ,   0.0     ,     !  DF,GF,WH,RC
     3              0.0      ,  0.0      ,  0.0      ,   0.0     ,     !  PY,MD,BL,WO
     3              0.0      ,  0.0      ,  0.0/                       !  RA,PD,WI
C
      B1=CWAPAR(ISPGRP,1)
      B2=CWAPAR(ISPGRP,2)
      B3=CWAPAR(ISPGRP,3)
      RP=(HT-XL)/(HT-HLCW)
      RATIO=HT/DBH
      IF(ISPGRP .EQ. 1) THEN
         IF(RATIO .GT. 50.0) RATIO=50.0
      ELSEIF(ISPGRP .EQ. 2) THEN
         IF(RATIO .GT. 31.0) RATIO=31.0
      ENDIF
      CW=LCW*RP**(B1+B2*SQRT(RP)+B3*(RATIO))
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE CW_SMC(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HLCW,LCW,HT,DBH,XL,CW,CWAPAR(11,3),B1,B2,B3,RP,RATIO
C
C  CROWN WIDTH ABOVE LARGEST CROWN WIDTH (3 parameters - all species)
C
C     DF Coefficients from Hann (1999) FS 45: 217-225
C     GF Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RC Coefficients from WH
C     PY Coefficients from WH
C     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C
      DATA CWAPAR/
     1              0.929973 ,  0.999291 ,  0.629785 ,   0.629785,     !  DF,GF,WH,RC
     1              0.629785,   0.5      ,  0.5      ,   0.5     ,     !  PY,MD,BL,WO
     1              0.5      ,  0.5      ,  0.5      ,                 !  RA,PD,WI
C
     2             -0.135212 ,  0.0      ,  0.0      ,   0.0     ,     !  DF,GF,WH,RC
     2              0.0      ,  0.0      ,  0.0      ,   0.0     ,     !  PY,MD,BL,WO
     2              0.0      ,  0.0      ,  0.0      ,                 !  RA,PD,WI
C
     3             -0.0157579, -0.0314603,  0.0      ,   0.0     ,     !  DF,GF,WH,RC
     3              0.0      ,  0.0      ,  0.0      ,   0.0     ,     !  PY,MD,BL,WO
     3              0.0      ,  0.0      ,  0.0/                       !  RA,PD,WI
C
      B1=CWAPAR(ISPGRP,1)
      B2=CWAPAR(ISPGRP,2)
      B3=CWAPAR(ISPGRP,3)
      RP=(HT-XL)/(HT-HLCW)
      RATIO=HT/DBH
      IF(ISPGRP .EQ. 1) THEN
         IF(RATIO .GT. 50.0) RATIO=50.0
      ELSEIF(ISPGRP .EQ. 2) THEN
         IF(RATIO .GT. 31.0) RATIO=31.0
      ENDIF
      CW=LCW*RP**(B1+B2*SQRT(RP)+B3*(RATIO))
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE CW_RAP(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HLCW,LCW,HT,DBH,XL,CW,CWAPAR(7,3),B1,B2,B3,RP,RATIO
C
C  CROWN WIDTH ABOVE LARGEST CROWN WIDTH (3 parameters - all species)
C
C     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
C     DF Coefficients from Hann (1999) FS 45: 217-225
C     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RC Coefficients from WH
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C
      DATA CWAPAR/
     1             0.63420194,  0.929973 ,  0.629785 ,   0.629785,     !  RA,DF,WH,RC
     1             0.5       ,   0.5     ,  0.5      ,                 !  BL,PD,WI
C
     2             0.17649614, -0.135212 ,  0.0      ,   0.0     ,     !  RA,DF,WH,RC
     2             0.0       ,  0.0      ,  0.0      ,                 !  BL,PD,WI
C
     3            -0.02315018, -0.0157579,  0.0      ,   0.0     ,     !  RA,DF,WH,RC
     3             0.0       ,  0.0      ,  0.0/                       !  BL,PD,WI
C
      B1=CWAPAR(ISPGRP,1)
      B2=CWAPAR(ISPGRP,2)
      B3=CWAPAR(ISPGRP,3)
      RP=(HT-XL)/(HT-HLCW)
      RATIO=HT/DBH
      IF(ISPGRP .EQ. 2) THEN
         IF(RATIO .GT. 50.0) RATIO=50.0
      ENDIF
      CW=LCW*RP**(B1+B2*SQRT(RP)+B3*(RATIO))
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE CRNCLO(IND,CTMUL,VERSION,NTREES,TDATAI,TDATAR,
     1                  SCR,MGEXP,CCH,CC)
C     DETERMINE CROWN CLOSURE
C**********************************************************************
C          IND = 0 COMPUTE CROWN CLOSURES WITHOUT SHADOW CROWN RATIOS
C              = 1 COMPUTE CROWN CLOSURES WITH SHADOW CROWN RATIOS
C          CTMUL = CUT TREE MULTIPLIER
C                =  0.0 TO NOT ADD OR SUBTRACT CUT TREES
C                =  1.0 TO ADD CUT TREES
C                = -1.0 TO SUBTRACT CUT TREES
      IMPLICIT NONE
      INTEGER*4 IND,VERSION,NTREES,TDATAI(2000,3),L,I,ISPGRP
      REAL*4    CTMUL,TDATAR(2000,8),SCR(2000,3),MGEXP(2000),CCH(41),
     1          CC,DBH,HT,CR,CL,HCB,EXPAN,SCR1,MCW,LCW,HLCW
C
      DO L=1,40
         CCH(L)=0.
      ENDDO
      CCH(41)=TDATAR(1,2)
      DO I=2,NTREES
         IF(TDATAR(I,2).GT.CCH(41)) CCH(41)=TDATAR(I,2)
      ENDDO
      DO I=1,NTREES
         ISPGRP=TDATAI(I,2)
         DBH=TDATAR(I,1)
         HT=TDATAR(I,2)
         IF(IND .EQ. 1 .AND. SCR(I,1) .GT. 0.0) THEN
            CR=SCR(I,1)
         ELSE
            CR=TDATAR(I,3)
         ENDIF
         SCR1=SCR(I,1)
         CL=CR*HT
         HCB=HT-CL
         EXPAN=TDATAR(I,4)+CTMUL*MGEXP(I)
         SELECT CASE(VERSION)
            CASE(1)
               CALL MCW_SWO(ISPGRP,DBH,HT,MCW)
               CALL LCW_SWO(ISPGRP,MCW,CR,SCR1,DBH,HT,LCW)
               CALL HLCW_SWO(ISPGRP,HT,CR,SCR1,HLCW)
               CALL CALC_CC(VERSION,ISPGRP,HLCW,LCW,HT,DBH,HCB,EXPAN,
     1                      CCH)
            CASE(2)
               CALL MCW_NWO(ISPGRP,DBH,HT,MCW)
               CALL LCW_NWO(ISPGRP,MCW,CR,SCR1,DBH,HT,LCW)
               CALL HLCW_NWO(ISPGRP,HT,CR,SCR1,HLCW)
               CALL CALC_CC(VERSION,ISPGRP,HLCW,LCW,HT,DBH,HCB,EXPAN,
     1                      CCH)
            CASE(3)
               CALL MCW_SMC(ISPGRP,DBH,HT,MCW)
               CALL LCW_SMC(ISPGRP,MCW,CR,SCR1,DBH,HT,LCW)
               CALL HLCW_SMC(ISPGRP,HT,CR,SCR1,HLCW)
               CALL CALC_CC(VERSION,ISPGRP,HLCW,LCW,HT,DBH,HCB,EXPAN,
     1                      CCH)
            CASE(4)
               CALL MCW_RAP(ISPGRP,DBH,HT,MCW)
               CALL LCW_RAP(ISPGRP,MCW,CR,SCR1,DBH,HT,LCW)
               CALL HLCW_RAP(ISPGRP,HT,CR,SCR1,HLCW)
               CALL CALC_CC(VERSION,ISPGRP,HLCW,LCW,HT,DBH,HCB,EXPAN,
     1                      CCH)
         ENDSELECT
      ENDDO
      CC=CCH(1)
      RETURN
      END

*******************************************************************************
      SUBROUTINE GET_CCFL(DBH,CCFLL1,CCFL1,CCFL)
      IMPLICIT NONE
      INTEGER*4 K
      REAL*4 DBH,CCFLL1(51),CCFL1(500),CCFL
      IF(DBH .GT. 100.0) THEN
         CCFL=0.0
      ELSEIF(DBH .GT. 50.0)THEN
         K=INT(DBH-49.0)
         CCFL=CCFLL1(K)
      ELSE
         K=INT(DBH*10.0+0.5)
         CCFL=CCFL1(K)
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE MAXHCB_SWO(ISPGRP,HT,CCFL,MAXHCB)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,CCFL,MAXHCB
      REAL*4 MAXPAR(18,5),B0,B1,B2,B3,LIMIT,MAXBR
C
C  MAXIMUM HEIGHT TO CROWN BASE
C     (5 parameters - all species)
C
      DATA MAXPAR/
     1          0.96       ,  0.96       ,  1.01       ,  1.02       , !  DF,GW,PP,SP
     1          0.97       ,  1.01       ,  0.96       ,  0.85       , !  IC,WH,RC,PY
     1          0.981      ,  1.0        ,  0.98       ,  1.0         ,!  MD,GC,TA,CL
     1          1.0         , 1.0        ,  1.0        ,  0.93        ,!  BL,WO,BO,RA
     1          1.0         , 0.985      ,                             !  PD,WI
C
     2          0.26       ,  0.31       ,  0.36       ,  0.27       , !  DF,GW,PP,SP
     2          0.22       ,  0.36       ,  0.31       ,  0.35       , !  IC,WH,RC,PY
     2          0.161      ,  0.45       ,  0.33       ,  0.45       , !  MD,GC,TA,CL
     2          0.45       ,  0.3        ,  0.2        ,  0.18       , !  BL,WO,BO,RA
     2          0.45       ,  0.285      ,                             !  PD,WI
C
     3         -0.987864873, -2.450718394, -1.041915784, -0.922718593, !  DF,GW,PP,SP
     3         -0.002612590, -0.944528054, -1.059636222, -0.922868139, !  IC,WH,RC,PY
     3         -1.73666044 , -1.219919284, -0.911341687, -0.922025464, !  MD,GC,TA,CL
     3         -1.020016685, -0.95634399 , -1.053892465, -0.928243505, !  BL,WO,BO,RA
     3         -1.020016685, -0.969750805,                             !  PD,WI
C
     4          1.0        ,  1.0        ,  0.6        ,  0.4        , !  DF,GW,PP,SP
     4          1.0        ,  0.6        ,  1.0        ,  0.8        , !  IC,WH,RC,PY
     4          1.0        ,  1.2        ,  1.0        ,  1.0        , !  MD,GC,TA,CL
     4          1.0        ,  1.1        ,  1.0        ,  1.0        , !  BL,WO,BO,RA
     4          1.0        ,  0.9        ,                             !  PD,WI
C
     5          0.95       ,  0.95       ,  0.95       ,  0.96       , !  DF,GW,PP,SP
     5          0.95       ,  0.96       ,  0.95       ,  0.80       , !  IC,WH,RC,PY
     5          0.98       ,  0.98       ,  0.97       ,  0.98       , !  MD,GC,TA,CL
     5          0.95       ,  0.98       ,  0.98       ,  0.92       , !  BL,WO,BO,RA
     5          0.95       ,  0.98/                                    !  PD,WI
C
      B0=MAXPAR(ISPGRP,1)
      B1=MAXPAR(ISPGRP,2)
      B2=MAXPAR(ISPGRP,3)
      B3=MAXPAR(ISPGRP,4)
      LIMIT=MAXPAR(ISPGRP,5)
      MAXBR=B0-B1*EXP(B2*(CCFL/100.0)**B3)
      IF(MAXBR .GT. LIMIT) MAXBR=LIMIT
      MAXHCB=MAXBR*HT
      RETURN
      END
C**********************************************************************
      SUBROUTINE MAXHCB_NWO(ISPGRP,HT,CCFL,MAXHCB)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,CCFL,MAXHCB
      REAL*4 MAXPAR(11,5),B0,B1,B2,B3,LIMIT,MAXBR
C
C  MAXIMUM HEIGHT TO CROWN BASE
C     (5 parameters - all species)
C
      DATA MAXPAR/
     1          0.96       ,  0.96       ,  1.01       ,  0.96       , !  DF,GF,WH,RC
     1          0.85       ,  0.981      ,  1.0        ,  1.0        , !  PY,MD,BL,WO
     1          0.93       ,  1.0        ,  0.985      ,               !  RA,PD,WI
C
     2          0.26       ,  0.31       ,  0.36       ,  0.31       , !  DF,GF,WH,RC
     2          0.35       ,  0.161      ,  0.45       ,  0.3        , !  PY,MD,BL,WO
     2          0.18       ,  0.45       ,  0.285      ,               !  RA,PD,WI
C
     3         -0.900721383, -2.450718394, -0.944528054, -1.059636222, !  DF,GW,WH,RC
     3         -0.922868139, -1.73666044 , -1.020016685, -0.95634399 , !  PY,MD,BL,WO
     3         -0.928243505, -1.020016685, -0.969750805,               !  RA,PD,WI
C
     4          1.0        ,  1.0        ,  0.6        ,  1.0        , !  DF,GW,WH,RC
     4          0.8        ,  1.0        ,  1.0        ,  1.1        , !  PY,MD,BL,WO
     4          1.0        ,  1.0        ,  0.9        ,               !  RA,PD,WI
C
     5          0.95       ,  0.95       ,  0.96       ,  0.95       , !  DF,GW,WH,RC
     5          0.80       ,  0.98       ,  0.95       ,  0.98       , !  PY,MD,BL,WO
     5          0.92       ,  0.95       ,  0.98/                      !  RA,PD,WI
C
      B0=MAXPAR(ISPGRP,1)
      B1=MAXPAR(ISPGRP,2)
      B2=MAXPAR(ISPGRP,3)
      B3=MAXPAR(ISPGRP,4)
      LIMIT=MAXPAR(ISPGRP,5)
      MAXBR=B0-B1*EXP(B2*(CCFL/100.0)**B3)
      IF(MAXBR .GT. LIMIT) MAXBR=LIMIT
      MAXHCB=MAXBR*HT
      RETURN
      END
C**********************************************************************
      SUBROUTINE MAXHCB_SMC(ISPGRP,HT,CCFL,MAXHCB)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,CCFL,MAXHCB
      REAL*4 MAXPAR(11,5),B0,B1,B2,B3,LIMIT,MAXBR
C
C  MAXIMUM HEIGHT TO CROWN BASE
C     (5 parameters - all species)
C
      DATA MAXPAR/
     1          0.96       ,  0.96       ,  1.01       ,  0.96       , !  DF,GF,WH,RC
     1          0.85       ,  0.981      ,  1.0        ,  1.0        , !  PY,MD,BL,WO
     1          0.93       ,  1.0        ,  0.985      ,               !  RA,PD,WI
C
     2          0.26       ,  0.31       ,  0.36       ,  0.31       , !  DF,GF,WH,RC
     2          0.35       ,  0.161      ,  0.45       ,  0.3        , !  PY,MD,BL,WO
     2          0.18       ,  0.45       ,  0.285      ,               !  RA,PD,WI
C
     3         -0.34758    , -2.450718394, -0.944528054, -1.059636222, !  DF,GW,WH,RC
     3         -0.922868139, -1.73666044 , -1.020016685, -0.95634399 , !  PY,MD,BL,WO
     3         -0.928243505, -1.020016685, -0.969750805,               !  RA,PD,WI
C
     4          1.5        ,  1.0        ,  0.6        ,  1.0        , !  DF,GW,WH,RC
     4          0.8        ,  1.0        ,  1.0        ,  1.1        , !  PY,MD,BL,WO
     4          1.0        ,  1.0        ,  0.9        ,               !  RA,PD,WI
C
     5          0.95       ,  0.95       ,  0.96       ,  0.95       , !  DF,GW,WH,RC
     5          0.80       ,  0.98       ,  0.95       ,  0.98       , !  PY,MD,BL,WO
     5          0.92       ,  0.95       ,  0.98/                      !  RA,PD,WI
C
      B0=MAXPAR(ISPGRP,1)
      B1=MAXPAR(ISPGRP,2)
      B2=MAXPAR(ISPGRP,3)
      B3=MAXPAR(ISPGRP,4)
      LIMIT=MAXPAR(ISPGRP,5)
      MAXBR=B0-B1*EXP(B2*(CCFL/100.0)**B3)
      IF(MAXBR .GT. LIMIT) MAXBR=LIMIT
      MAXHCB=MAXBR*HT
      RETURN
      END
C**********************************************************************
      SUBROUTINE MAXHCB_RAP(ISPGRP,HT,CCFL,MAXHCB)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,CCFL,MAXHCB
      REAL*4 MAXPAR(7,5),B0,B1,B2,B3,LIMIT,MAXBR
C
C  MAXIMUM HEIGHT TO CROWN BASE
C     (5 parameters - all species)
C
      DATA MAXPAR/
     1          0.93       ,  0.96       ,  1.01       ,  0.96       , !  RA,DF,WH,RC
     1          1.0        ,  1.0        ,  0.985      ,               !  BL,PD,WI
C
     2          0.18       ,  0.26       ,  0.36       ,  0.31       , !  RA,DF,WH,RC
     2          0.45       ,  0.45       ,  0.285      ,               !  BL,PD,WI
C
     3         -0.928243505, -0.34758    , -0.944528054, -1.059636222, !  RA,DF,WH,RC
     3         -1.020016685, -1.020016685, -0.969750805,               !  BL,PD,WI
C
     4          1.0        ,  1.5        ,  0.6        ,  1.0        , !  RA,DF,WH,RC
     4          1.0        ,  1.0        ,  0.9        ,               !  BL,PD,WI
C
     5          0.92       ,  0.95       ,  0.96       ,  0.95       , !  RA,DF,WH,RC
     5          0.95       ,  0.95       ,  0.98/                      !  BL,PD,WI
C
      B0=MAXPAR(ISPGRP,1)
      B1=MAXPAR(ISPGRP,2)
      B2=MAXPAR(ISPGRP,3)
      B3=MAXPAR(ISPGRP,4)
      LIMIT=MAXPAR(ISPGRP,5)
      MAXBR=B0-B1*EXP(B2*(CCFL/100.0)**B3)
      IF(MAXBR .GT. LIMIT) MAXBR=LIMIT
      MAXHCB=MAXBR*HT
      RETURN
      END
