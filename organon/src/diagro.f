C     ORGANON GROWTH AND YIELD MODEL
C     SUBROUTINES INCLUDED:
C         DIAMGRO
C         DG_SWO
C         DG_NWO
C         DG_SMC
C         DG_RAP
C         DG_THIN
C         DG_FERT
C         GET_BAL
C
C  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
C               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
C               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
C               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.
C
C  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - DIAMGRO TO DIAMGRO_RUN
C  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - GET_BAL TO GET_BAL_RUN
C
********************************************************************************
      SUBROUTINE DIAMGRO_RUN(VERSION,K,CYCLG,TDATAI,TDATAR,SI_1,SI_2,
     1           SBA1,BALL1,BAL1,CALIB,PN,YF,BABT,BART,YT,GROWTH)
      IMPLICIT NONE
C
C     CALCULATES FIVE-YEAR DIAMETER GROWTH RATE OF THE K-TH TREE
C
      INTEGER*4 VERSION,K,CYCLG,TDATAI(2000,3),I,ISP,ISPGRP
      REAL*4 TDATAR(2000,8),SI_1,SI_2,SBA1,BALL1(51),BAL1(500),
     1       CALIB(6,18),PN(5),YF(5),BABT,BART(5),YT(5),GROWTH(2000,4),
     2       DGRO,DBH,CR,DG,SBAL1,FERTADJ,THINADJ,SITE
C
C     CALCULATE BASAL AREA IN LARGER TREES
C
      DBH=TDATAR(K,1)
      CR=TDATAR(K,3)
      ISP=TDATAI(K,1)
      ISPGRP=TDATAI(K,2)
      CALL GET_BAL_RUN(DBH,BALL1,BAL1,SBAL1)
      SELECT CASE(VERSION)
         CASE(1)
            SITE=SI_1
         CASE(2,3)
            IF(ISP .EQ. 263) THEN
               SITE=SI_2
            ELSE
               SITE=SI_1
            ENDIF
         CASE(4)
            IF(ISP .EQ. 263) THEN
               SITE=SI_2
            ELSE
               SITE=SI_1
            ENDIF
      END SELECT
C
C     CALCULATE DIAMETER GROWTH RATE FOR UNTREATED TREES
C
      SELECT CASE(VERSION)
         CASE(1)
            CALL DG_SWO(ISPGRP,DBH,CR,SITE,SBAL1,SBA1,DG)
         CASE(2)
            CALL DG_NWO_RUN(ISPGRP,DBH,CR,SITE,SBAL1,SBA1,DG)
         CASE(3)
            CALL DG_SMC(ISPGRP,DBH,CR,SITE,SBAL1,SBA1,DG)
         CASE(4)
            CALL DG_RAP(ISPGRP,DBH,CR,SITE,SBAL1,SBA1,DG)
      END SELECT
C
C     CALCULATE FERTILIZER ADJUSTMENT
C
      CALL DG_FERT(ISP,VERSION,CYCLG,SI_1,PN,YF,FERTADJ)
C
C     CALCULATE THINNING ADJUSTMENT
C
      CALL DG_THIN(ISP,VERSION,CYCLG,BABT,BART,YT,THINADJ)
C
C     CALCULATE DIAMETER GROWTH RATE FOR UNTREATED OR TREATED TREES
C
      DGRO=DG*CALIB(3,ISPGRP)*FERTADJ*THINADJ
      GROWTH(K,2)=DGRO
      RETURN
      END
*******************************************************************************
      SUBROUTINE DG_SWO(ISPGRP,DBH,CR,SITE,SBAL1,SBA1,DG)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 B0,B1,B2,B3,B4,B5,B6,K1,K2,K3,K4,DBH,CR,SBAL1,SBA1,LNDG,
     1       SITE,DG,DGPAR(18,11),CRADJ,ADJ
C
C  DIAMETER GROWTH PARAMETERS FOR SOUTHWEST OREGON (11 parameters - all species)
C
C     DF Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     GW Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     PP Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     SP Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     IC Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WH Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     MD Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     GC Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     TA Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     CL Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     BL Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     BO Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WI Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C
      DATA DGPAR/
     1             -5.35558894,     -5.84904111,     -4.51958940,      !  DF,GW,PP
     1             -4.12342552,     -2.08551255,     -5.70052255,      !  SP,IC,WH
     1            -11.45456097,     -9.15835863,     -8.84531757,      !  RC,PY,MD
     1             -7.78451344,     -3.36821750,     -3.59333060,      !  GC,TA,CL
     1             -3.41449922,     -7.81267986,     -4.43438109,      !  BL,WO,BO
     1             -4.39082007,     -8.08352683,     -8.08352683,      !  RA,PD,WI
C
     2              0.840528547,     1.668196109,     0.813998712,     !  DF,GW,PP
     2              0.734988422,     0.596043703,     0.865087036,     !  SP,IC,WH
     2              0.784133664,     1.0        ,     1.5        ,     !  RC,PY,MD
     2              1.2        ,     1.2        ,     1.2        ,     !  GC,TA,CL
     2              1.0        ,     1.405616529,     0.930930363,     !  BL,WO,BO
     2              1.0        ,     1.0        ,     1.0        ,     !  RA,PD,WI
C
     3             -0.0427481848,   -0.0853271265,   -0.0493858858,    !  DF,GW,PP
     3             -0.0425469735,   -0.0215223077,   -0.0432543518,    !  SP,IC,WH
     3             -0.0261377888,   -0.00000035  ,   -0.0006      ,    !  RC,PY,MD
     3             -0.07        ,   -0.07        ,   -0.07        ,    !  GC,TA,CL
     3             -0.05        ,   -0.0603105850,   -0.0465947242,    !  BL,WO,BO
     3             -0.0945057147,   -0.00000035  ,   -0.00000035  ,    !  RA,PD,WI
C
     4              1.15950313  ,    1.21222176  ,    1.10249641  ,    !  DF,GW,PP
     4              1.05942163  ,    1.02734556  ,    1.10859727  ,    !  SP,IC,WH
     4              0.70174783  ,    1.16688474  ,    0.51225596  ,    !  RC,PY,MD
     4              0.0         ,    0.0         ,    0.51637418  ,    !  GC,TA,CL
     4              0.0         ,    0.64286007  ,    0.0         ,    !  BL,WO,BO
     4              1.06867026  ,    0.31176647  ,    0.31176647  ,    !  RA,PD,WI
C
     5              0.954711126 ,    0.679346647 ,    0.879440023 ,    !  DF,GW,PP
     5              0.808656390 ,    0.383450822 ,    0.977332597 ,    !  SP,IC,WH
     5              2.057236260 ,    0.0         ,    0.418129153 ,    !  RC,PY,MD
     5              1.01436101  ,    0.0         ,    0.0         ,    !  GC,TA,CL
     5              0.324349277 ,    1.037687142 ,    0.510717175 ,    !  BL,WO,BO
     5              0.685908029 ,    0.0         ,    0.0         ,    !  RA,PD,WI
C
     6             -0.00894779670,  -0.00809965733,  -0.0108521667,    !  DF,GW,PP
     6             -0.0107837565 ,  -0.00489046624,   0.0         ,    !  SP,IC,WH
     6             -0.00415440257,   0.0          ,  -0.00355254593,   !  RC,PY,MD
     6             -0.00834323811,   0.0          ,   0.0          ,   !  GC,TA,CL
     6              0.0          ,   0.0          ,   0.0          ,   !  BL,WO,BO
     6             -0.00586331028,   0.0          ,   0.0          ,   !  RA,PD,WI
C
     7              0.0         ,    0.0         ,   -0.0333706948,    !  DF,GW,PP
     7              0.0         ,   -0.0609024782,   -0.0526263229,    !  SP,IC,WH
     7              0.0         ,   -0.02        ,   -0.0321315389,    !  RC,PY,MD
     7              0.0         ,   -0.0339813575,   -0.02        ,    !  GC,TA,CL
     7             -0.0989519477,   -0.0787012218,   -0.0688832423,    !  BL,WO,BO
     7              0.0         ,   -0.0730788052,   -0.0730788052,    !  RA,PD,WI
C
     8              5.0         ,    5.0         ,    5.0         ,    !  DF,GW,PP
     8              5.0         ,    5.0         ,    5.0         ,    !  SP,IC,WH
     8              5.0         , 4000.0         ,  110.0         ,    !  RC,PY,MD
     8             10.0         ,   10.0         ,   10.0         ,    !  GC,TA,CL
     8             10.0         ,    5.0         ,    5.0         ,    !  BL,WO,BO
     8              5.0         , 4000.0         , 4000.0         ,    !  RA,PD,WI
C
     9              1.0         ,    1.0         ,    1.0         ,    !  DF,GW,PP
     9              1.0         ,    1.0         ,    1.0         ,    !  SP,IC,WH
     9              1.0         ,    4.0         ,    2.0         ,    !  RC,PY,MD
     9              1.0         ,    1.0         ,    1.0         ,    !  GC,TA,CL
     9              1.0         ,    1.0         ,    1.0         ,    !  BL,WO,BO
     9              1.0         ,    4.0         ,    4.0         ,    !  RA,PD,WI
C
     X              1.0         ,    1.0         ,    1.0         ,    !  DF,GW,PP
     X              1.0         ,    1.0         ,    1.0         ,    !  SP,IC,WH
     X              1.0         ,    1.0         ,    1.0         ,    !  RC,PY,MD
     X              1.0         ,    1.0         ,    1.0         ,    !  GC,TA,CL
     X              1.0         ,    1.0         ,    1.0         ,    !  BL,WO,BO
     X              1.0         ,    1.0         ,    1.0         ,    !  RA,PD,WI
C
     A              2.7         ,    2.7         ,    2.7         ,    !  DF,GW,PP
     A              2.7         ,    2.7         ,    2.7         ,    !  SP,IC,WH
     A              2.7         ,    2.7         ,    2.7         ,    !  RC,PY,MD
     A              2.7         ,    2.7         ,    2.7         ,    !  GC,TA,CL
     A              2.7         ,    2.7         ,    2.7         ,    !  BL,WO,BO
     A              2.7         ,    2.7         ,    2.7/             !  RA,PD,WI
C
      B0=DGPAR(ISPGRP,1)
      B1=DGPAR(ISPGRP,2)
      B2=DGPAR(ISPGRP,3)
      B3=DGPAR(ISPGRP,4)
      B4=DGPAR(ISPGRP,5)
      B5=DGPAR(ISPGRP,6)
      B6=DGPAR(ISPGRP,7)
      K1=DGPAR(ISPGRP,8)
      K2=DGPAR(ISPGRP,9)
      K3=DGPAR(ISPGRP,10)
      K4=DGPAR(ISPGRP,11)
      LNDG=B0
     1      +B1*LOG(DBH+K1)
     2      +B2*DBH**K2
     3      +B3*LOG((CR+0.2)/1.2)
     4      +B4*LOG(SITE)
     5      +B5*((SBAL1**K3)/LOG(DBH+K4))
     6      +B6*SQRT(SBA1)
C
C     CROWN RATIO ADJUSTMENT
C
      CRADJ = 1.0
      IF (CR.LE. 0.17) CRADJ=1.0-EXP(-(25.0*CR)**2.0)
C
C      FULL ADJUSTMENTS
C
      IF(ISPGRP .EQ. 1) THEN
         ADJ=0.8938
      ELSEIF(ISPGRP .EQ. 2) THEN
         ADJ=0.8722
      ELSEIF(ISPGRP .EQ. 4) THEN
         ADJ=0.7903
      ELSEIF(ISPGRP .EQ. 9) THEN
         ADJ=0.7928
      ELSEIF(ISPGRP .EQ. 10) THEN
         ADJ=0.7259
C      ELSEIF(ISPGRP .EQ. 14) THEN
C         ADJ=0.7608
      ELSEIF(ISPGRP .EQ. 14) THEN
         ADJ=1.0
      ELSEIF(ISPGRP .EQ. 15) THEN
         ADJ=0.7667
      ELSE
         ADJ=0.8
      ENDIF
      DG=EXP(LNDG)*CRADJ*ADJ
      RETURN
      END
*******************************************************************************
      SUBROUTINE DG_NWO_RUN(ISPGRP,DBH,CR,SITE,SBAL1,SBA1,DG)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 B0,B1,B2,B3,B4,B5,B6,K1,K2,K3,K4,DBH,CR,SBAL1,SBA1,LNDG,
     1       SITE,DG,DGPAR(11,11),CRADJ,ADJ
C
C  DIAMETER GROWTH (11 parameters - all species)
C
C     DF Coefficients from Zumrawi and Hann (1993) FRL Research Contribution 4
C     GF Coefficients from Zumrawi and Hann (1993) FRL Research Contribution 4
C     WH Coefficients from Johnson (2002) Willamette Industries Report
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     MD Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     BL Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WI Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C
      DATA DGPAR/
     1          -4.69624      ,     -2.34619      ,     -4.49867      ,! DF,GF,WH
     1         -11.45456097   ,     -9.15835863   ,     -8.84531757   ,! RC,PY,MD
     1          -3.41449922   ,     -7.81267986   ,     -4.39082007   ,! BL,WO,RA
     1          -8.08352683   ,     -8.08352683   ,                    ! PD,WI
C
     2           0.339513     ,      0.594640     ,      0.362369     ,! DF,GF,WH
     2           0.784133664  ,      1.0          ,      1.5          ,! RC,PY,MD
     2           1.0          ,      1.405616529  ,      1.0          ,! BL,WO,RA
     2           1.0          ,      1.0          ,                    ! PD,WI
C
     3          -0.000428261  ,     -0.000976092  ,     -0.00153907   ,! DF,GF,WH
     3          -0.0261377888 ,     -0.00000035   ,     -0.0006       ,! RC,PY,MD
     3          -0.05         ,     -0.0603105850 ,     -0.0945057147 ,! BL,WO,RA
     3          -0.00000035   ,     -0.00000035   ,                    ! PD,WI
C
     4           1.19952      ,      1.12712      ,      1.1557       ,! DF,GF,WH
     4           0.70174783   ,      1.16688474   ,      0.51225596   ,! RC,PY,MD
     4           0.0          ,      0.64286007   ,      1.06867026   ,! BL,WO,RA
     4           0.31176647   ,      0.31176647   ,                    ! PD,WI
C
     5           1.15612      ,      0.555333     ,      1.12154      ,! DF,GF,WH
     5           2.057236260  ,      0.0          ,      0.418129153  ,! RC,PY,MD
     5           0.324349277  ,      1.037687142  ,      0.685908029  ,! BL,WO,RA
     5           0.0          ,      0.0          ,                    ! PD,WI
C
     6          -0.0000446327 ,     -0.0000290672 ,     -0.0000201041 ,! DF,GF,WH
     6          -0.00415440257,      0.0          ,     -0.00355254593,! RC,PY,MD
     6           0.0          ,      0.0          ,     -0.00586331028,! BL,WO,RA
     6           0.0          ,      0.0          ,                    ! PD,WI
C
     7          -0.0237003    ,     -0.0470848    ,     -0.0417388    ,! DF,GF,WH
     7           0.0          ,     -0.02         ,     -0.0321315389 ,! RC,PY,MD
     7          -0.0989519477 ,     -0.0787012218 ,      0.0          ,! BL,WO,RA
     7          -0.0730788052 ,     -0.0730788052 ,                    ! PD,WI
C
     8           1.0          ,      1.0          ,      1.0          ,! DF,GF,WH
     8           5.0          ,   4000.0          ,    110.0          ,! RC,PY,MD
     8          10.0          ,      5.0          ,      5.0          ,! BL,WO,RA
     8        4000.0          ,   4000.0          ,                    ! PD,WI
C
     9           2.0          ,      2.0          ,      2.0          ,! DF,GF,WH
     9           1.0          ,      4.0          ,      2.0          ,! RC,PY,MD
     9           1.0          ,      1.0          ,      1.0          ,! BL,WO,RA
     9           4.0          ,      4.0          ,                    ! PD,WI
C
     X           2.0          ,      2.0          ,      2.0          ,! DF,GF,WH
     X           1.0          ,      1.0          ,      1.0          ,! RC,PY,MD
     X           1.0          ,      1.0          ,      1.0          ,! BL,WO,RA
     X           1.0          ,      1.0          ,                    ! PD,WI
C
     A           5.0          ,      5.0          ,      5.0          ,! DF,GF,WH
     A           2.7          ,      2.7          ,      2.7          ,! RC,PY,MD
     A           2.7          ,      2.7          ,      2.7          ,! BL,WO,RA
     A           2.7          ,      2.7/                              ! PD,WI
C
      B0=DGPAR(ISPGRP,1)
      B1=DGPAR(ISPGRP,2)
      B2=DGPAR(ISPGRP,3)
      B3=DGPAR(ISPGRP,4)
      B4=DGPAR(ISPGRP,5)
      B5=DGPAR(ISPGRP,6)
      B6=DGPAR(ISPGRP,7)
      K1=DGPAR(ISPGRP,8)
      K2=DGPAR(ISPGRP,9)
      K3=DGPAR(ISPGRP,10)
      K4=DGPAR(ISPGRP,11)
      LNDG=B0
     1      +B1*LOG(DBH+K1)
     2      +B2*DBH**K2
     3      +B3*LOG((CR+0.2)/1.2)
     4      +B4*LOG(SITE)
     5      +B5*((SBAL1**K3)/LOG(DBH+K4))
     6      +B6*SQRT(SBA1)
C
C     CROWN RATIO ADJUSTMENT
C
      CRADJ = 1.0
      IF (CR.LE. 0.17) CRADJ=1.0-EXP(-(25.0*CR)**2.0)
      
      IF(ISPGRP .EQ. 1) THEN
         ADJ=0.7011014
      ELSEIF(ISPGRP .EQ. 2) THEN
         ADJ=0.8722
      ELSEIF(ISPGRP .EQ. 3) THEN
         ADJ=0.7163
      ELSEIF(ISPGRP .EQ. 6) THEN
         ADJ=0.7928
      ELSEIF(ISPGRP .EQ. 8) THEN
         ADJ=1.0
      ELSE
         ADJ=0.8
      ENDIF
      DG=EXP(LNDG)*CRADJ*ADJ
      RETURN
      END
*******************************************************************************
      SUBROUTINE DG_SMC(ISPGRP,DBH,CR,SITE,SBAL1,SBA1,DG)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 B0,B1,B2,B3,B4,B5,B6,K1,K2,K3,K4,DBH,CR,SBAL1,SBA1,LNDG,
     1       SITE,DG,DGPAR(11,11),CRADJ,ADJ
C
C  DIAMETER GROWTH PARAMETERS (11 parameters - all species)
C
C     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution 49
C     GF Coefficients from Zumrawi and Hann (1993) FRL Research Contribution 4
C     WH Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     MD Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     BL Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WI Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C
      DATA DGPAR/
     1          -5.34253119   ,     -2.34619      ,     -4.87447412   ,! DF,GF,WH
     1         -11.45456097   ,     -9.15835863   ,     -8.84531757   ,! RC,PY,MD
     1          -3.41449922   ,     -7.81267986   ,     -4.39082007   ,! BL,WO,RA
     1          -8.08352683   ,     -8.08352683   ,                    ! PD,WI
C
     2           1.098406840  ,      0.594640     ,      0.4150723209 ,! DF,GF,WH
     2           0.784133664  ,      1.0          ,      1.5          ,! RC,PY,MD
     2           1.0          ,      1.405616529  ,      1.0          ,! BL,WO,RA
     2           1.0          ,      1.0          ,                    ! PD,WI
C
     3          -0.05218621   ,     -0.000976092  ,     -0.023744997  ,! DF,GF,WH
     3          -0.0261377888 ,     -0.00000035   ,     -0.0006       ,! RC,PY,MD
     3          -0.05         ,     -0.0603105850 ,     -0.0945057147 ,! BL,WO,RA
     3          -0.00000035   ,     -0.00000035   ,                    ! PD,WI
C
     4           1.01380810   ,      1.12712      ,      0.907837299  ,! DF,GF,WH
     4           0.70174783   ,      1.16688474   ,      0.51225596   ,! RC,PY,MD
     4           0.0          ,      0.64286007   ,      1.06867026   ,! BL,WO,RA
     4           0.31176647   ,      0.31176647   ,                    ! PD,WI
C
     5           0.91202025   ,      0.555333     ,      1.1346766989 ,! DF,GF,WH
     5           2.057236260  ,      0.0          ,      0.418129153  ,! RC,PY,MD
     5           0.324349277  ,      1.037687142  ,      0.685908029  ,! BL,WO,RA
     5           0.0          ,      0.0          ,                    ! PD,WI
C
     6          -0.01756220   ,     -0.0000290672 ,     -0.015333503  ,! DF,GF,WH
     6          -0.00415440257,      0.0          ,     -0.00355254593,! RC,PY,MD
     6           0.0          ,      0.0          ,     -0.00586331028,! BL,WO,RA
     6           0.0          ,      0.0          ,                    ! PD,WI
C
     7          -0.05168923   ,     -0.0470848    ,     -0.03309787   ,! DF,GF,WH
     7           0.0          ,     -0.02         ,     -0.0321315389 ,! RC,PY,MD
     7          -0.0989519477 ,     -0.0787012218 ,      0.0          ,! BL,WO,RA
     7          -0.0730788052 ,     -0.0730788052 ,                    ! PD,WI
C
     8           6.0          ,      1.0          ,      5.0          ,! DF,GF,WH
     8           5.0          ,   4000.0          ,    110.0          ,! RC,PY,MD
     8          10.0          ,      5.0          ,      5.0          ,! BL,WO,RA
     8        4000.0          ,   4000.0          ,                    ! PD,WI
C
     9           1.0          ,      2.0          ,      1.0          ,! DF,GF,WH
     9           1.0          ,      4.0          ,      2.0          ,! RC,PY,MD
     9           1.0          ,      1.0          ,      1.0          ,! BL,WO,RA
     9           4.0          ,      4.0          ,                    ! PD,WI
C
     X           1.0          ,      2.0          ,      1.0          ,! DF,GF,WH
     X           1.0          ,      1.0          ,      1.0          ,! RC,PY,MD
     X           1.0          ,      1.0          ,      1.0          ,! BL,WO,RA
     X           1.0          ,      1.0          ,                    ! PD,WI
C
     A           2.7          ,      5.0          ,      2.7          ,! DF,GF,WH
     A           2.7          ,      2.7          ,      2.7          ,! RC,PY,MD
     A           2.7          ,      2.7          ,      2.7          ,! BL,WO,RA
     A           2.7          ,      2.7/                              ! PD,WI
C
      B0=DGPAR(ISPGRP,1)
      B1=DGPAR(ISPGRP,2)
      B2=DGPAR(ISPGRP,3)
      B3=DGPAR(ISPGRP,4)
      B4=DGPAR(ISPGRP,5)
      B5=DGPAR(ISPGRP,6)
      B6=DGPAR(ISPGRP,7)
      K1=DGPAR(ISPGRP,8)
      K2=DGPAR(ISPGRP,9)
      K3=DGPAR(ISPGRP,10)
      K4=DGPAR(ISPGRP,11)
      LNDG=B0
     1      +B1*LOG(DBH+K1)
     2      +B2*DBH**K2
     3      +B3*LOG((CR+0.2)/1.2)
     4      +B4*LOG(SITE)
     5      +B5*((SBAL1**K3)/LOG(DBH+K4))
     6      +B6*SQRT(SBA1)
C
C     CROWN RATIO ADJUSTMENT
C
      CRADJ=1.0-EXP(-(25.0*CR)**2)
      IF(ISPGRP .EQ. 1) THEN
         ADJ=1.0
      ELSEIF(ISPGRP .EQ. 2) THEN
         ADJ=0.8722
      ELSEIF(ISPGRP .EQ. 3) THEN
         ADJ=1.0
      ELSEIF(ISPGRP .EQ. 6) THEN
         ADJ=0.7928
      ELSEIF(ISPGRP .EQ. 8) THEN
         ADJ=1.0
      ELSE
         ADJ=0.8
      ENDIF
      DG=EXP(LNDG)*CRADJ*ADJ
      RETURN
      END
*******************************************************************************
      SUBROUTINE DG_RAP(ISPGRP,DBH,CR,SITE,SBAL1,SBA1,DG)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 B0,B1,B2,B3,B4,B5,B6,K1,K2,K3,K4,DBH,CR,SBAL1,SBA1,LNDG,
     1       SITE,DG,DGPAR(7,11),CRADJ,ADJ
C
C  DIAMETER GROWTH PARAMETERS (11 parameters - all species)
C
C     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
C
C     The following species were annualized by adding ln(0.2) to the intercept terms
C
C     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution ??
C     WH Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     BL Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     PD Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WI Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C
      DATA DGPAR/
     1          -4.622849554  ,     -6.95196910   ,     -6.48391203   ,! RA,DF,WH
     1         -13.06399888   ,     -5.02393713   ,     -9.69296474   ,! RC,BL,PD
     1          -9.69296474   ,                                        ! WI
C
     2           0.5112200516 ,      1.098406840  ,      0.4150723209 ,! RA,DF,WH
     2           0.784133664  ,      1.0          ,      1.0          ,! RC,BL,PD
     2           1.0          ,                                        ! WI
C
     3          -.1040194568  ,     -0.05218621   ,     -0.023744997  ,! RA,DF,WH
     3          -0.0261377888 ,     -0.05         ,     -0.00000035   ,! RC,BL,PD
     3          -0.00000035   ,                                        ! WI
C
     4           0.9536538143 ,      1.01380810   ,      0.907837299  ,! RA,DF,WH
     4           0.70174783   ,      0.0          ,      0.31176647   ,! RC,BL,PD
     4           0.31176647   ,                                        ! WI
C
     5           1.0659344724 ,      0.91202025   ,      1.1346766989 ,! RA,DF,WH
     5           2.057236260  ,      0.324349277  ,      0.0          ,! RC,BL,PD
     5           0.0          ,                                        ! WI
C
     6          -.0193047405  ,     -0.01756220   ,     -0.015333503  ,! RA,DF,WH
     6          -0.00415440257,      0.0          ,      0.0          ,! RC,BL,PD
     6           0.0          ,                                        ! WI
C
     7          -.0773539455 ,     -0.05168923   ,     -0.03309787   ,! RA,DF,WH
     7           0.0          ,     -0.0989519477 ,     -0.0730788052 ,! RC,BL,PD
     7          -0.0730788052 ,                                        ! WI
C
     8           1.0          ,      6.0          ,      5.0          ,! RA,DF,WH
     8           5.0          ,     10.0          ,   4000.0          ,! RC,BL,PD
     8        4000.0          ,                                        ! WI
C
     9           1.0          ,      1.0          ,      1.0          ,! RA,DF,WH
     9           1.0          ,      1.0          ,      4.0          ,! RC,BL,PD
     9           4.0          ,                                        ! BL
C
     X           1.0          ,      1.0          ,      1.0          ,! RA,DF,WH
     X           1.0          ,      1.0          ,      1.0          ,! RC,BL,PD
     X           1.0          ,                                        ! WI
C
     A           1.0          ,      2.7          ,      2.7          ,! RA,DF,WH
     A           2.7          ,      2.7          ,      2.7          ,! RC,BL,PD
     A           2.7/                                                  ! WI
C
      B0=DGPAR(ISPGRP,1)
      B1=DGPAR(ISPGRP,2)
      B2=DGPAR(ISPGRP,3)
      B3=DGPAR(ISPGRP,4)
      B4=DGPAR(ISPGRP,5)
      B5=DGPAR(ISPGRP,6)
      B6=DGPAR(ISPGRP,7)
      K1=DGPAR(ISPGRP,8)
      K2=DGPAR(ISPGRP,9)
      K3=DGPAR(ISPGRP,10)
      K4=DGPAR(ISPGRP,11)
      LNDG=B0
     1      +B1*LOG(DBH+K1)
     2      +B2*DBH**K2
     3      +B3*LOG((CR+0.2)/1.2)
     4      +B4*LOG(SITE)
     5      +B5*((SBAL1**K3)/LOG(DBH+K4))
     6      +B6*SQRT(SBA1)
C
C     CROWN RATIO ADJUSTMENT
C
      CRADJ=1.0-EXP(-(25.0*CR)**2)
      IF(ISPGRP .LE. 3) THEN
         ADJ=1.0
      ELSE
         ADJ=0.8
      ENDIF
      DG=EXP(LNDG)*CRADJ*ADJ
      RETURN
      END
*******************************************************************************
      SUBROUTINE DG_THIN(ISP,VERSION,CYCLG,BABT,BART,YT,THINADJ)
C
C     CALCULATE THINNING ADJUSTMENT FOR DIAMETER GROWTH RATE FROM
C     HANN ET AL.(2003) FRL RESEARCH CONTRIBUTION 40
C
      IMPLICIT NONE
      REAL*4 BABT,BART(5),YT(5),THINADJ,PT1,PT2,PT3,XTIME,THINX1,
     1       THINX2,THINX3,PREM
      INTEGER*4 ISP,VERSION,CYCLG,I
C
C     SET PARAMETERS FOR ADJUSTMENT
C
      IF(ISP .EQ. 263)THEN
         PT1=0.723095045
         PT2=1.0
         PT3=-0.2644085320
      ELSEIF(ISP .EQ. 202)THEN
         PT1=0.6203827985
         PT2=1.0
         PT3=-0.2644085320
      ELSEIF(VERSION .EQ. 4 .AND. ISP .EQ. 351)THEN
         PT1=0.0
         PT2=1.0
         PT3=0.0
      ELSE
         PT1=0.6203827985
         PT2=1.0
         PT3=-0.2644085320
      ENDIF
      XTIME=FLOAT(CYCLG)*5.0
      THINX1=0.0
      DO I=2,5
         THINX1=THINX1+BART(I)*EXP((PT3/PT2)*(YT(1)-YT(I)))
      ENDDO
      THINX2=THINX1+BART(1)
      THINX3=THINX1+BABT
      IF(THINX3 .LE. 0.0) THEN
         PREM=0.0
      ELSE
         PREM=THINX2/THINX3
      ENDIF
      IF(PREM .GT. 0.75) PREM=0.75
      THINADJ=1.0+PT1*PREM**PT2*EXP(PT3*(XTIME-YT(1)))
      RETURN
      END
*******************************************************************************
      SUBROUTINE DG_FERT(ISP,VERSION,CYCLG,SI_1,PN,YF,FERTADJ)
C
C     CALCULATE FERTILIZER ADJUSTMENT FOR DIAMETER GROWTH RATE
C     FROM HANN ET AL.(2003) FRL RESEARCH CONTRIBUTION 40
C
      IMPLICIT NONE
      REAL*4 SI_1,PN(5),YF(5),FERTADJ,PF1,PF2,PF3,PF4,PF5,FALDWN,XTIME,
     1       FERTX1,FERTX2
      INTEGER*4 ISP,VERSION,CYCLG,I
C
C     SET PARAMETERS FOR ADJUSTMENT
C
      IF(VERSION .LE. 3) THEN
         IF(ISP .EQ. 263)THEN
            PF1=0.0
            PF2=1.0
            PF3=0.0
            PF4=0.0
            PF5=1.0
        ELSEIF(ISP .EQ. 202)THEN
            PF1=1.368661121
            PF2=0.741476964
            PF3=-0.214741684
            PF4=-0.851736558
            PF5=2.0
         ELSE
            PF1=0.0
            PF2=1.0
            PF3=0.0
            PF4=0.0
            PF5=1.0
        ENDIF
      ELSE
         PF1=0.0
         PF2=1.0
         PF3=0.0
         PF4=0.0
         PF5=1.0
      ENDIF
      FALDWN=1.0
      XTIME=FLOAT(CYCLG)*5.0
      FERTX1=0.0
      DO I=2,5
         FERTX1=FERTX1+(PN(I)/800.0)*EXP((PF3/PF2)*(YF(1)-YF(I)))
      ENDDO
      FERTX2=EXP(PF3*(XTIME-YF(1))+PF4*(SI_1/100.0)**PF5)
      FERTADJ=1.0+(PF1*((PN(1)/800.0)+FERTX1)**PF2*FERTX2)*FALDWN
      RETURN
      END
*******************************************************************************
      SUBROUTINE GET_BAL_RUN(DBH,BALL1,BAL1,BAL)
      IMPLICIT NONE
      INTEGER*4 K
      REAL*4 DBH,BALL1(51),BAL1(500),BAL
      IF(DBH .GT. 100.0) THEN
         BAL=0.0
      ELSEIF(DBH .GT. 50.0)THEN
         K=DBH-49.0
         BAL=BALL1(K)
      ELSE
         K=DBH*10.0+0.5
         BAL=BAL1(K)
      ENDIF
      RETURN
      END
