      SUBROUTINE CF(VERSION,SPP,SPGRP,DBH,HT,CR,CFTD,CFSH,VALU)
C     CALCULATE ENDING OR STARTING CF VOLUME
C
C     VALU = CF VOLUME TO BE CALCULATED
C
C
C  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
C               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
C               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
C               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.
C
C  CHANGED THE NAME OF SUBROUTINE - SCRIB TO SCRIB_VOL
c
      IMPLICIT NONE
      INTEGER*4 VERSION,SPP,SPGRP
      REAL*4  DBH,HT,CR,CFTD,CFSH,DIB,DIB1FT,VALU
C
      SELECT CASE(VERSION)
        CASE(1)                   ! Southwest Oregon
           CALL SWO_DIB(SPGRP,DBH,CR,DIB)
           CALL SWO_DIB1FT(SPGRP,DBH,CR,DIB1FT)
        CASE(2,3)                   ! Western Willamette Valley, SMC
           CALL NWO_DIB(SPGRP,DBH,CR,DIB)
           CALL NWO_DIB1FT(SPGRP,DBH,CR,DIB1FT)
        CASE(4)                     ! Red Alder Plantations
           CALL RAP_DIB(SPGRP,DBH,DIB)
           CALL RAP_DIB1FT(SPGRP,DBH,CR,DIB1FT)
      ENDSELECT
      IF(SPP .LE. 300)THEN
         SELECT CASE(VERSION)
           CASE(1)                   ! Southwest Oregon
              CALL SWO_CCFV(SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU)
           CASE(2,3)                   ! Western Willamette Valley, SMC
              CALL NWO_CCFV(SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU)
           CASE(4)                   ! Red Alder Plantations
              CALL RAP_CCFV(SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU)
         ENDSELECT
      ELSE
         SELECT CASE(VERSION)
           CASE(1)                   ! Southwest Oregon
              CALL SWO_HCFV(VERSION,SPGRP,DBH,HT,DIB,DIB1FT,CFTD,CFSH,
     1                      VALU)
           CASE(2,3)                   ! Western Willamette Valley, SMC
              CALL NWO_HCFV(VERSION,SPGRP,DBH,HT,DIB,DIB1FT,CFTD,CFSH,
     1                      VALU)
           CASE(4)                   ! Red Alder Plantations
              CALL RAP_HCFV(VERSION,SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,
     1                      CFSH,VALU)
         ENDSELECT
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE SWO_DIB(SPGRP,DBH,CR,DIB)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 DBH,CR,DIB,BTPAR(18,3),E1,E2,E3
C
C  BARK THICKNESS (3 parameters - all species)
C
      DATA BTPAR/
     1   0.92443655,  0.92162494,  0.80860026,  0.85897904,  0.87875535, !  DF,GW,PP,SP,IC,
     1   0.933707  ,  0.9497    ,  0.97      ,  0.96317   ,  0.94448   , !  WH,RC,PY,MD,GC,
     1   0.859151  ,  0.910499  ,  0.97059   ,  0.878457  ,  0.889703  , !  TA,CL,BL,WO,BO,
     1   0.947     ,  0.94448   ,  0.94448   ,                           !  RA,PD,WI
C
     2   0.98886654,  1.0       ,  1.01742589,  1.0       ,  1.0       , !  DF,GW,PP,SP,IC,
     2   1.0       ,  1.0       ,  1.0       ,  1.0       ,  0.9875170 , !  WH,RC,PY,MD,GC,
     2   1.0178109 ,  1.01475   ,  0.993585  ,  1.02393   ,  1.0104062 , !  TA,CL,BL,WO,BO,
     2   1.0       ,  0.9875170 ,  0.9875170 ,                           !  RA,PD,WI
C
     3  -0.03414550, -0.03415396,  0.0       , 0.0        , -0.07696055, !  DF,GW,PP,SP,IC,
     3   0.0       ,  0.0       ,  0.0       , 0.0        ,  0.0       , !  WH,RC,PY,MD,GC,
     3   0.0       ,  0.0       ,  0.0       , 0.0        ,  0.0       , !  TA,CL,BL,WO,BO,
     3   0.0       ,  0.0       ,  0.0       /                           !  RA,PD,WI
      E1=BTPAR(SPGRP,1)
      E2=BTPAR(SPGRP,2)
      E3=BTPAR(SPGRP,3)
      DIB=E1*(DBH**E2)*EXP(E3*(1.0-CR)**0.5)
      RETURN
      END
C**********************************************************************
      SUBROUTINE NWO_DIB(SPGRP,DBH,CR,DIB)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 DBH,CR,DIB,BTPAR(11,3),E1,E2,E3
C
C  BARK THICKNESS (3 parameters - all species)
C
      DATA BTPAR/
     1            0.971330, 0.92162494 , 0.933707, 0.9497  , 0.97    ,    !  DF,GF,WH,RC,PY
     1            0.96317 , 0.97059    , 0.878457, 0.947   , 0.94448 ,    !  MD,BL,WO,RA,PD
     1            0.94448 ,                                               !  WI
C
     2            0.966365, 1.0        , 1.0     , 1.0     , 1.0     ,    !  DF,GF,WH,RC,PY
     2            1.0     , 0.993585   , 1.02393 , 1.0     , 0.987517,    !  MD,BL,WO,RA,PD
     2            0.987517,                                               !  WI
C
     3            0.0     , -0.03415396, 0.0     , 0.0     , 0.0     ,    !  DF,GF,WH,RC,PY
     3            0.0     ,  0.0       , 0.0     , 0.0     , 0.0     ,    !  MD,BL,WO,RA,PD
     3            0.0     /                                               !  WI
      IF(SPGRP .EQ. 1) THEN
         E1=0.903563
         E2=0.989388
         E3=0.0
      ELSE
         E1=BTPAR(SPGRP,1)
         E2=BTPAR(SPGRP,2)
         E3=BTPAR(SPGRP,3)
      ENDIF
      DIB=E1*DBH**E2*EXP(E3*(1.0-CR)**0.5)
      RETURN
      END
C**********************************************************************
      SUBROUTINE RAP_DIB(SPGRP,DBH,DIB)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 DBH,DIB,BTPAR(7,2),E1,E2
C
C  BARK THICKNESS (2 parameters - all species)
C
      DATA BTPAR/
     1            0.947   , 0.971330, 0.933707, 0.9497  , 0.97059 ,    !  RA,DF,WH,RC,BL
     1            0.94448 , 0.94448 ,                                  !  PD,WI

     2            1.0     , 0.966365, 1.0     , 1.0     , 0.993585,    !  RA,DF,WH,RC,BL
     2            0.987517, 0.987517/                                  !  PD,WI
C
      E1=BTPAR(SPGRP,1)
      E2=BTPAR(SPGRP,2)
      DIB=E1*DBH**E2
      RETURN
      END
C**********************************************************************
      SUBROUTINE SWO_DIB1FT(SPGRP,DBH,CR,DIB1FT)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 DBH,CR,DIB1FT,SDPAR(18,5),G0,G1,G2,G3,G4
C
C  STUMP DIAMETER (4 parameters - all species)
C
      DATA SDPAR/
     1 0.149809111 , 0.393048214, 0.0        , 0.0       , 0.451569966 ,  !  DF,GW,PP,SP,IC,
     1 0.0         , 0.451569966, 0.451569966, 0.0       , 0.0         ,  !  WH,RC,PY,MD,GC,
     1 0.0         , 0.0        , 0.0        , 0.0       , 0.0         ,  !  TA,CL,BL,WO,BO,
     1 0.0         , 0.0        , 0.0        ,                            !  RA,PD,WI
C
     2 0.900790279 , 0.729932627, 1.0        , 1.04030514, 0.831752493 ,  !  DF,GW,PP,SP,IC,
     2 0.989819    , 0.831752493, 0.831752493, 1.0       , 1.0         ,  !  WH,RC,PY,MD,GC,
     2 1.0         , 1.0        , 1.0        , 1.0       , 1.0         ,  !  TA,CL,BL,WO,BO,
     2 1.0         , 1.0        , 1.0        ,                            !  RA,PD,WI
C
     3 0.133648456 , 0.120814754, 0.0        , 0.0       , 0.216216295 ,  !  DF,GW,PP,SP,IC,
     3 0.0         , 0.216216295, 0.216216295, 0.0       , 0.0         ,  !  WH,RC,PY,MD,GC,
     3 0.0         , 0.0        , 0.0        , 0.0       , 0.0         ,  !  TA,CL,BL,WO,BO,
     3 0.0         , 0.0        , 0.0        ,                            !  RA,PD,WI
C
     4 3.67532829  , 1.0        , 1.0        , 1.0       , 7.00446878  ,  !  DF,GW,PP,SP,IC,
     4 1.0         , 7.00446878 , 7.00446878 , 1.0       , 1.0         ,  !  WH,RC,PY,MD,GC,
     4 1.0         , 1.0        , 1.0        , 1.0       , 1.0         ,  !  TA,CL,BL,WO,BO,
     4 1.0         , 1.0        , 1.0        ,                            !  RA,PD,WI
C
     5 1.0213663112, 1.097851010, 1.0        , 1.0       , 1.0560026859,  !  DF,GW,PP,SP,IC,
     5 1.0         , 1.056002686, 1.056002686, 1.0       , 1.0         ,  !  WH,RC,PY,MD,GC,
     5 1.0         , 1.0        , 1.0        , 1.0       , 1.0         ,  !  TA,CL,BL,WO,BO,
     5 1.0         , 1.0        , 1.0      /                              !  RA,PD,WI
C
      G0=SDPAR(SPGRP,1)
      G1=SDPAR(SPGRP,2)
      G2=SDPAR(SPGRP,3)
      G3=SDPAR(SPGRP,4)
      G4=SDPAR(SPGRP,5)
      DIB1FT=G0+G1*EXP(G2*CR**G3)*DBH**G4
      RETURN
      END
C**********************************************************************
      SUBROUTINE NWO_DIB1FT(SPGRP,DBH,CR,DIB1FT)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 DBH,CR,DIB1FT,SDPAR(11,5),G0,G1,G2,G3,G4
C
C  STUMP DIAMETER (5 parameters - all species)
C
      DATA SDPAR/
     1  0.149809111 , 0.393048214, 0.0     , 0.451569966 , 0.451569966,   ! DF,GF,WH,RC,PY
     1  0.0         , 0.0        , 0.0     , 0.0         , 0.0        ,   ! MD,BL,WO,RA,PD
     1  0.0         ,                                                     ! WI
C
     2  0.900790279 , 0.729932627, 0.989819, 0.831752493 , 0.831752493,   ! DF,GF,WH,RC,PY
     2  1.0         , 1.0        , 1.0     , 1.0         , 1.0        ,   ! MD,BL,WO,RA,PD
     2  1.0         ,                                                     ! WI
C
     3  0.133648456 , 0.120814754, 0.0     , 0.216216295 , 0.216216295,   ! DF,GF,WH,RC,PY
     3  0.0         , 0.0        , 0.0     , 0.0         , 0.0        ,   ! MD,BL,WO,RA,PD
     3  0.0     ,                                                         ! WI
C
     4  3.67532829  , 1.0        , 1.0     , 7.00446878  , 7.00446878 ,   ! DF,GF,WH,RC,PY
     4  1.0         , 1.0        , 1.0     , 1.0         , 1.0        ,   ! MD,BL,WO,RA,PD
     4  1.0         ,                                                     ! WI
C
     5  1.0213663112, 1.097851010, 1.0     , 1.0560026859, 1.0560026859,  ! DF,GF,WH,RC,PY
     5  1.0         , 1.0        , 1.0     , 1.0         , 1.0         ,  ! MD,BL,WO,RA,PD
     5  1.0         /                                                     ! WI
C
      G0=SDPAR(SPGRP,1)
      G1=SDPAR(SPGRP,2)
      G2=SDPAR(SPGRP,3)
      G3=SDPAR(SPGRP,4)
      G4=SDPAR(SPGRP,5)
      DIB1FT=G0+G1*EXP(G2*CR**G3)*DBH**G4
      RETURN
      END

C**********************************************************************
      SUBROUTINE RAP_DIB1FT(SPGRP,DBH,CR,DIB1FT)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 DBH,CR,DIB1FT,SDPAR(7,5),G0,G1,G2,G3,G4
C
C  STUMP DIAMETER (5 parameters - all species)
C
      DATA SDPAR/
     1  0.0     , 0.149809111 , 0.0         , 0.451569966 , 0.0     ,  !  RA,DF,WH,RC,BL
     1  0.0     , 0.0         ,                                        !  PD,WI
C
     2  1.0     , 0.900790279 , 0.989819    , 0.831752493 , 1.0     ,  !  RA,DF,WH,RC,BL
     2  1.0     , 1.0         ,                                        !  PD,WI
C
     3  0.0     , 0.133648456 , 0.0         , 0.216216295 , 0.0     ,  !  RA,DF,WH,RC,BL
     3  0.0     , 0.0         ,                                        !  PD,WI
C
     4  1.0     , 3.67532829  , 1.0         , 7.00446878  , 1.0     ,  !  RA,DF,WH,RC,BL
     4  1.0     , 1.0         ,                                        !  PD,WI
C
     5  1.0     , 1.0213663112, 1.0         , 1.0560026859, 1.0     ,  !  RA,DF,WH,RC,BL
     5  1.0     , 1.0         /                                        !  PD,WI
C
      G0=SDPAR(SPGRP,1)
      G1=SDPAR(SPGRP,2)
      G2=SDPAR(SPGRP,3)
      G3=SDPAR(SPGRP,4)
      G4=SDPAR(SPGRP,5)
      DIB1FT=G0+G1*EXP(G2*CR**G3)*DBH**G4
      RETURN
      END
C**********************************************************************
      SUBROUTINE SWO_CCFV(SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4  DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU,CCVPAR(8,8),
     1        CMVPAR(8,6),A1,A2,A3,A4,A5,EQNO1,B1,B2,B3,B4,B5,B6,B7,
     2        EQNO2,X1,X2,X3,VABH,VBBH,PI,HCB,CRABH,RAT,DIBD,SDIB
C
C  CUBIC VOLUME - CONIFERS (8 parameters)
C
      DATA CCVPAR/
     1  1.24485628 ,  1.33397259 , 1.27677676 , 0.855844369, 0.90763281,  !  DF,GW,PP,SP,IC
     1  1.168      ,  0.887      , 0.887      ,                           !  WH,RC,PY
C
     2  0.346490193,  0.357808283, 0.162198194, 0.388366991, 0.34284673,  !  DF,GW,PP,SP,IC
     2  0.265430   ,  0.367622   , 0.367622   ,                           !  WH,RC,PY
C
     3 -0.56574969 , -0.755355039, 0.0        , 0.0     ,   -0.63865388,  !  DF,GW,PP,SP,IC
     3  0.0        ,  0.0        , 0.0        ,                           !  WH,RC,PY
C
     4  0.632239239,  0.5        , 0.0        , 0.0     ,    1.5857204 ,  !  DF,GW,PP,SP,IC
     4  0.0        ,  0.0        , 0.0        ,                           !  WH,RC,PY
C
     5 -0.152406551, -0.261766125, 0.0        , 0.0     ,    0.0       ,  !  DF,GW,PP,SP,IC
     5  0.0        ,  0.0        , 0.0        ,                           !  WH,RC,PY
C
     6  4.55802463 ,  1.0        , 1.0        , 1.0     ,    1.0       ,  !  DF,GW,PP,SP,IC
     6  1.0        ,  1.0        , 1.0        ,                           !  WH,RC,PY
C
     7 -0.051186711,  0.0        , 0.0        , 0.0     ,    0.0       ,  !  DF,GW,PP,SP,IC
     7  0.0        ,  0.0        , 0.0        ,                           !  WH,RC,PY
C
     8  1.0        ,  1.0        , 0.0        , 0.0     ,    1.0       ,  !  DF,GW,PP,SP,IC
     8  0.0        ,  0.0        , 0.0        /                           !  WH,RC,PY

C
C  CONIFER CUBIC MERCH VOLUME - CONIFERS (6 parameters)
C
      DATA CMVPAR/
     1 -3.39101798 , -0.76519904, -4.87435933, -4.87435933, -3.75729892,  !  DF,GW,PP,SP,IC
     1  0.930057   , 0.885038   , 0.885038   ,                            !  WH,RC,PY
C
     2  0.918583494, 0.25       , 1.19484691 ,  1.27588884,  1.23328561,  !  DF,GW,PP,SP,IC
     2  3.74152    , 3.29655    , 3.29655    ,                            !  WH,RC,PY
C
     3  1.3330217  , 3.80136398 , 0.634341265,  0.63434126,  1.17859869,  !  DF,GW,PP,SP,IC
     3  0.0        , 0.0        , 0.0        ,                            !  WH,RC,PY
C
     4 -0.935974246, -1.7902001 , 0.0        ,  0.0       , -0.45135743,  !  DF,GW,PP,SP,IC
     4  0.0        , 0.0        , 0.0        ,                            !  WH,RC,PY
C
     5  3.0        , 1.0        , 1.0        ,  1.0       ,  2.0       ,  !  DF,GW,PP,SP,IC
     5  0.0        , 0.0        , 0.0        ,                            !  WH,RC,PY
C
     6  1.0        , 1.0        , 1.0        ,  1.0       ,  1.0       ,  !  DF,GW,PP,SP,IC
     6  0.0        , 0.0        , 0.0        /                            !  WH,RC,PY
C
      A1=CMVPAR(SPGRP,1)
      A2=CMVPAR(SPGRP,2)
      A3=CMVPAR(SPGRP,3)
      A4=CMVPAR(SPGRP,4)
      A5=CMVPAR(SPGRP,5)
      EQNO1=CMVPAR(SPGRP,6)
      B1=CCVPAR(SPGRP,1)/1000.0
      B2=CCVPAR(SPGRP,2)
      B3=CCVPAR(SPGRP,3)
      B4=CCVPAR(SPGRP,4)
      B5=CCVPAR(SPGRP,5)
      B6=CCVPAR(SPGRP,6)
      B7=CCVPAR(SPGRP,7)
      EQNO2=CCVPAR(SPGRP,8)
      VABH=0.0
      VBBH=0.0
      PI=3.14159265
C
C     CALCULATE CROWN RATIO ABOVE BREAST HEIGHT
C
      HCB=HT-CR*HT
      IF(HCB .GT. 4.5)THEN
         CRABH=(HT-HCB)/(HT-4.5)
      ELSE
         CRABH=1.0
      ENDIF
C
C     CALCULATE VOLUME ABOVE BREAST HEIGHT
C
      IF(DIB.LE.CFTD) GO TO 20
      IF(EQNO1 .LT. 0.5) THEN
         RAT=1.0-A1*(CFTD/DIB)**A2
      ELSE
         RAT=1.0-EXP(A1*(1.0-CFTD/DIB)**A2)*(CFTD/DIB)**(A3+A4*CR**A5)
      ENDIF
      X1=((HT-4.5)/DBH)**(B2*(1.0-EXP(B3*DBH**B4))**EQNO2)
      X2=EXP(B5*CRABH**B6)
      X3=DBH**B7
      VABH=RAT*(B1*X1*X2*X3*DBH**2*(HT-4.5))
C
C     VOLUME BELOW BREAST HEIGHT
C
   20 DIBD=(DIB/DIB1FT)**(2./3.)
      SDIB=(((4.5-DIBD-CFSH*(1.0-DIBD))/3.5)**1.5)*DIB1FT
      IF(SDIB.LE.CFTD) GO TO 10
      VBBH=(.25*PI*DIB1FT**2)*((1.0/43904.)*(729.0+81.0*DIBD+
     1     297.0*(DIB/DIB1FT)**(4./3.)+265.0*(DIB/DIB1FT)**2)-
     2     (1.0/6174)*((4.5-DIBD)**3*CFSH-
     3     1.5*(4.5-DIBD)**2*(1.0-DIBD)*CFSH**2+
     4     (4.5-DIBD)*(1.0-DIBD)**2*CFSH**3-
     5     (.25)*(1.0-DIBD)**3*CFSH**4))
C
C     DETERMINE TREE'S VOLUME
C
   10 VALU=(VABH+VBBH)
      IF(VALU .LT. 0.0)VALU=0.0
      RETURN
      END
C**********************************************************************
      SUBROUTINE NWO_CCFV(SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4  DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU,CCVPAR(5,8),
     1        CMVPAR(5,6),A1,A2,A3,A4,A5,EQNO1,B1,B2,B3,B4,B5,B6,B7,
     2        EQNO2,X1,X2,X3,VABH,VBBH,PI,HCB,CRABH,RAT,DIBD,SDIB
C
C  CUBIC VOLUME - CONIFERS (8 parameters)
C
      DATA CCVPAR/
     1  1.24485628 ,  1.33397259 , 1.168   ,  0.90763281,  0.90763281, !  DF,GF,WH,RC,PY
C
     2  0.346490193,  0.357808283, 0.265430,  0.34284673,  0.34284673, !  DF,GF,WH,RC,PY
C
     3 -0.56574969 , -0.755355039, 0.0     , -0.63865388, -0.63865388, !  DF,GF,WH,RC,PY
C
     4  0.632239239,  0.5        , 0.0     ,  1.5857204 ,  1.5857204 , !  DF,GF,WH,RC,PY
C
     5 -0.152406551, -0.261766125, 0.0     ,  0.0       ,  0.0       , !  DF,GF,WH,RC,PY
C
     6  4.55802463 ,  1.0        , 1.0     ,  1.0       ,  1.0       , !  DF,GF,WH,RC,PY
C
     7 -0.051186711,  0.0        , 0.0     ,  0.0       ,  0.0       , !  DF,GF,WH,RC,PY
C
     8  1.0        ,  1.0        , 0.0     ,  1.0       ,  1.0       / !  DF,GF,WH,RC,PY
C
C  CONIFER CUBIC MERCH VOLUME - CONIFERS (6 parameters)
C
      DATA CMVPAR/
     1 -3.39101798 , -0.76519904, 0.930057, -3.75729892, -3.75729892,  !  DF,GF,WH,RC,PY
C
     2  0.918583494,  0.25      , 3.74152 ,  1.23328561,  1.23328561,  !  DF,GF,WH,RC,PY
C
     3  1.3330217  ,  3.80136398, 0.0     ,  1.17859869,  1.17859869,  !  DF,GF,WH,RC,PY
C
     4 -0.935974246, -1.7902001 , 0.0     , -0.45135743, -0.45135743,  !  DF,GF,WH,RC,PY
C
     5  3.0        ,  1.0       , 0.0     ,  2.0       ,  2.0       ,  !  DF,GF,WH,RC,PY
C
     6  1.0        ,  1.0       , 0.0     ,  1.0       ,  1.0       /  !  DF,GF,WH,RC,PY
C
      A1=CMVPAR(SPGRP,1)
      A2=CMVPAR(SPGRP,2)
      A3=CMVPAR(SPGRP,3)
      A4=CMVPAR(SPGRP,4)
      A5=CMVPAR(SPGRP,5)
      EQNO1=CMVPAR(SPGRP,6)
      B1=CCVPAR(SPGRP,1)/1000.0
      B2=CCVPAR(SPGRP,2)
      B3=CCVPAR(SPGRP,3)
      B4=CCVPAR(SPGRP,4)
      B5=CCVPAR(SPGRP,5)
      B6=CCVPAR(SPGRP,6)
      B7=CCVPAR(SPGRP,7)
      EQNO2=CCVPAR(SPGRP,8)
      VABH=0.0
      VBBH=0.0
      PI=3.14159265
C
C     CALCULATE CROWN RATIO ABOVE BREAST HEIGHT
C
      HCB=HT-CR*HT
      IF(HCB .GT. 4.5)THEN
         CRABH=(HT-HCB)/(HT-4.5)
      ELSE
         CRABH=1.0
      ENDIF
C
C     CALCULATE VOLUME ABOVE BREAST HEIGHT
C
      IF(DIB.LE.CFTD) GO TO 20
      IF(EQNO1 .LT. 0.5) THEN
         RAT=1.0-A1*(CFTD/DIB)**A2
      ELSE
         RAT=1.0-EXP(A1*(1.0-CFTD/DIB)**A2)*(CFTD/DIB)**(A3+A4*CR**A5)
      ENDIF
      X1=((HT-4.5)/DBH)**(B2*(1.0-EXP(B3*DBH**B4))**EQNO2)
      X2=EXP(B5*CRABH**B6)
      X3=DBH**B7
      VABH=RAT*(B1*X1*X2*X3*DBH**2*(HT-4.5))
C
C     VOLUME BELOW BREAST HEIGHT
C
   20 DIBD=(DIB/DIB1FT)**(2./3.)
      SDIB=(((4.5-DIBD-CFSH*(1.0-DIBD))/3.5)**1.5)*DIB1FT
      IF(SDIB.LE.CFTD) GO TO 10
      VBBH=(.25*PI*DIB1FT**2)*((1.0/43904.)*(729.0+81.0*DIBD+
     1     297.0*(DIB/DIB1FT)**(4./3.)+265.0*(DIB/DIB1FT)**2)-
     2     (1.0/6174)*((4.5-DIBD)**3*CFSH-
     3     1.5*(4.5-DIBD)**2*(1.0-DIBD)*CFSH**2+
     4     (4.5-DIBD)*(1.0-DIBD)**2*CFSH**3-
     5     (.25)*(1.0-DIBD)**3*CFSH**4))
C
C     DETERMINE TREE'S VOLUME
C
   10 VALU=(VABH+VBBH)
      IF(VALU .LT. 0.0)VALU=0.0
      RETURN
      END
C**********************************************************************
      SUBROUTINE RAP_CCFV(SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU)
      IMPLICIT NONE
      INTEGER*4 SPGRP,ISPGRP
      REAL*4  DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU,CCVPAR(3,8),
     1        CMVPAR(3,6),A1,A2,A3,A4,A5,EQNO1,B1,B2,B3,B4,B5,B6,B7,
     2        EQNO2,X1,X2,X3,VABH,VBBH,PI,HCB,CRABH,RAT,DIBD,SDIB
C
C  CUBIC VOLUME - CONIFERS (3 parameters)
C
      DATA CCVPAR/
     1  1.24485628 , 1.168   ,  0.90763281,  !  DF,WH,RC
C
     2  0.346490193, 0.265430,  0.34284673,  !  DF,WH,RC
C
     3 -0.56574969 , 0.0     , -0.63865388,  !  DF,WH,RC
C
     4  0.632239239, 0.0     ,  1.5857204 ,  !  DF,WH,RC
C
     5 -0.152406551, 0.0     ,  0.0       ,  !  DF,WH,RC
C
     6  4.55802463 , 1.0     ,  1.0       ,  !  DF,WH,RC
C
     7 -0.051186711, 0.0     ,  0.0       ,  !  DF,WH,RC
C
     8  1.0        , 0.0     ,  1.0       /  !  DF,WH,RC
C
C  CONIFER CUBIC MERCH VOLUME - CONIFERS (2 parameters)
C
      DATA CMVPAR/
     1 -3.39101798 , 0.930057, -3.75729892,  !  DF,WH,RC
C
     2  0.918583494, 3.74152 ,  1.23328561,  !  DF,WH,RC

     3  1.3330217  , 0.0     ,  1.17859869,  !  DF,WH,RC
C
     4 -0.935974246, 0.0     , -0.45135743,  !  DF,WH,RC
C
     5  3.0        , 1.0     ,  2.0       ,  !  DF,WH,RC
C
     6  1.0        , 0.0     ,  1.0       /  !  DF,WH,RC
C
      ISPGRP=SPGRP-1
      A1=CMVPAR(SPGRP,1)
      A2=CMVPAR(SPGRP,2)
      A3=CMVPAR(SPGRP,3)
      A4=CMVPAR(SPGRP,4)
      A5=CMVPAR(SPGRP,5)
      EQNO1=CMVPAR(SPGRP,6)
      B1=CCVPAR(SPGRP,1)/1000.0
      B2=CCVPAR(SPGRP,2)
      B3=CCVPAR(SPGRP,3)
      B4=CCVPAR(SPGRP,4)
      B5=CCVPAR(SPGRP,5)
      B6=CCVPAR(SPGRP,6)
      B7=CCVPAR(SPGRP,7)
      EQNO2=CCVPAR(SPGRP,8)
      VABH=0.0
      VBBH=0.0
      PI=3.14159265
C
C     CALCULATE CROWN RATIO ABOVE BREAST HEIGHT
C
      HCB=HT-CR*HT
      IF(HCB .GT. 4.5)THEN
         CRABH=(HT-HCB)/(HT-4.5)
      ELSE
         CRABH=1.0
      ENDIF
C
C     CALCULATE VOLUME ABOVE BREAST HEIGHT
C
      IF(DIB.LE.CFTD) GO TO 20
      IF(EQNO1 .LT. 0.5) THEN
         RAT=1.0-A1*(CFTD/DIB)**A2
      ELSE
         RAT=1.0-EXP(A1*(1.0-CFTD/DIB)**A2)*(CFTD/DIB)**(A3+A4*CR**A5)
      ENDIF
      X1=((HT-4.5)/DBH)**(B2*(1.0-EXP(B3*DBH**B4))**EQNO2)
      X2=EXP(B5*CRABH**B6)
      X3=DBH**B7
      VABH=RAT*(B1*X1*X2*X3*DBH**2*(HT-4.5))
C
C     VOLUME BELOW BREAST HEIGHT
C
   20 DIBD=(DIB/DIB1FT)**(2./3.)
      SDIB=(((4.5-DIBD-CFSH*(1.0-DIBD))/3.5)**1.5)*DIB1FT
      IF(SDIB.LE.CFTD) GO TO 10
      VBBH=(.25*PI*DIB1FT**2)*((1.0/43904.)*(729.0+81.0*DIBD+
     1     297.0*(DIB/DIB1FT)**(4./3.)+265.0*(DIB/DIB1FT)**2)-
     2     (1.0/6174)*((4.5-DIBD)**3*CFSH-
     3     1.5*(4.5-DIBD)**2*(1.0-DIBD)*CFSH**2+
     4     (4.5-DIBD)*(1.0-DIBD)**2*CFSH**3-
     5     (.25)*(1.0-DIBD)**3*CFSH**4))
C
C     DETERMINE TREE'S VOLUME
C
   10 VALU=(VABH+VBBH)
      IF(VALU .LT. 0.0)VALU=0.0
      RETURN
      END
C**********************************************************************
      SUBROUTINE SWO_HCFV(VERSION,SPGRP,DBH,HT,DIB,DIB1FT,CFTD,CFSH,
     1                    VALU)
      IMPLICIT NONE
      INTEGER*4 VERSION,SPGRP,INDSPG
      REAL*4  DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU,HCVPAR(10,2),
     1        HMVPAR(10,3),A1,A2,A3,B0,B1,CVTS,CVM,CVS,PI,DIBD,SDIB
C                                                                      !  WH,RC,PY
C  CUBIC VOLUME - HARDWOODS (2 parameters)
C
      DATA HCVPAR/
     1      0.09515994, 0.04969879, 0.06345756, 0.06345756, 0.06345756,!  MD,GC,TA,CL,BL
     1      0.06345756, 0.06345756, 0.04969879, 0.04969879, 0.04969879,!  WO,BO,RA,PD,WI
C
     2      0.00247940, 0.00247940, 0.00208240, 0.00208240, 0.00208240,!  MD,GC,TA,CL,BL
     2      0.00208240, 0.00208240, 0.00247940, 0.00247940, 0.00247940/!  WO,BO,RA,PD,WI
C
C  CUBIC MERCH VOLUME - HARDWODS (3 parameters)
C
      DATA HMVPAR/
     1   -0.2391, -0.3741, -0.2792, -0.3741, -0.4270, -0.3741, -0.3741,! MD,GC,TA,CL,BL,WO,BO
     1   -0.4280, -0.3741, -0.3741,                                   !  RA,PD,WI
C
     2    2.951 ,  3.642 ,  3.038 ,  3.642 ,  2.348 ,  3.642 ,  3.642 ,! MD,GC,TA,CL,BL,WO,BO
     2    3.465 ,  3.642 ,  3.642 ,                                    !  RA,PD,WI
C
     3   -2.512 , -3.406 , -2.603 , -3.406 , -2.276 , -3.406 , -3.406 ,! MD,GC,TA,CL,BL,WO,BO
     3    -3.269 , -3.406 , -3.406/                                    !  RA,PD,WI
C
C     CALCULATES CUBIC FOOT VOLUMES OF HARDWOODS USING
C          EQUATIONS DEVELOPED FROM THE DATA IN J.A. SNELL AND S.N. LITTLE.
C          1983.  PREDICTING CROWN WEIGHT AND BOLE VOLUME OF FIVE WESTERN
C          HARDWOODS.  GENERAL TECHNICAL REPORT PNW-151.
C
C
      PI=3.14159265
      INDSPG=SPGRP-8
      A1=HMVPAR(INDSPG,1)
      A2=HMVPAR(INDSPG,2)
      A3=HMVPAR(INDSPG,3)
      B0=HCVPAR(INDSPG,1)
      B1=HCVPAR(INDSPG,2)
C
C     CALCULATE DIB AND TREE TOTAL CUBIC FOOT VOLUME
C
      CVTS=B0+B1*DBH**2*HT
C
C     CALCULATE OTHER MERCH. TOPS USING THE EQUATIONS IN SNELL AND LITTLE
C
      CVM=CVTS*(1.0+A1*CFTD**A2*DBH**A3)
      IF(CVM .LT. 0.0) CVM=0.0
C
C     CALCUATE STUMP VOLUME
C
      CVS=0.0
      DIBD=(DIB/DIB1FT)**(2./3.)
      SDIB=(((4.5-DIBD-CFSH*(1.0-DIBD))/3.5)**1.5)*DIB1FT
      IF(SDIB.LE.CFTD) THEN
         VALU=0.0
         RETURN
      ENDIF
      CVS=(.25*PI*DIB1FT**2)*((1.0/6174)*((4.5-DIBD)**3*CFSH-
     1     1.5*(4.5-DIBD)**2*(1.0-DIBD)*CFSH**2+
     2     (4.5-DIBD)*(1.0-DIBD)**2*CFSH**3-
     3     (.25)*(1.0-DIBD)**3*CFSH**4))
C
C     CALCUATE TREE'S VOLUME
C
      VALU=CVM-CVS
      IF(VALU .LT. 0.0)VALU=0.0
      RETURN
      END
C**********************************************************************
      SUBROUTINE NWO_HCFV(VERSION,SPGRP,DBH,HT,DIB,DIB1FT,CFTD,CFSH,
     1                    VALU)
      IMPLICIT NONE
      INTEGER*4 VERSION,SPGRP,INDSPG
      REAL*4  DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU,HCVPAR(6,2),
     1        HMVPAR(6,3),A1,A2,A3,B0,B1,CVTS,CVM,CVS,PI,DIBD,SDIB
C
C  CUBIC VOLUME - HARDWOODS (2 parameters)
C
      DATA HCVPAR/
     1      0.09515994, 0.06345756, 0.06345756, 0.04969879, 0.04969879,!  MD,BL,WO,RA,PD
     1      0.04969879,                                                !  WI
C
     2      0.00247940, 0.00208240, 0.00208240, 0.00247940, 0.00247940,!  MD,BL,WO,RA,PD
     2      0.00247940/                                                !  WI
C
C  CUBIC MERCH VOLUME - HARDWODS (3 parameters)
C
      DATA HMVPAR/
     1     -0.2391, -0.4270, -0.3741, -0.4280, -0.3741, -0.3741,       !  MD,BL,WO,RA,PD,WI
C
     2      2.951 ,  2.348 ,  3.642 ,  3.465 ,  3.642 ,  3.642 ,       !  MD,BL,WO,RA,PD,WI
C
     3     -2.512 , -2.276 , -3.406 , -3.269 , -3.406 , -3.406/        !  MD,BL,WO,RA,PD,WI
C
C     CALCULATES CUBIC FOOT VOLUMES OF HARDWOODS USING
C          EQUATIONS DEVELOPED FROM THE DATA IN J.A. SNELL AND S.N. LITTLE.
C          1983.  PREDICTING CROWN WEIGHT AND BOLE VOLUME OF FIVE WESTERN
C          HARDWOODS.  GENERAL TECHNICAL REPORT PNW-151.
C
C
      PI=3.14159265
      INDSPG=SPGRP-5
      A1=HMVPAR(INDSPG,1)
      A2=HMVPAR(INDSPG,2)
      A3=HMVPAR(INDSPG,3)
      B0=HCVPAR(INDSPG,1)
      B1=HCVPAR(INDSPG,2)
C
C     CALCULATE DIB AND TREE TOTAL CUBIC FOOT VOLUME
C
      CVTS=B0+B1*DBH**2*HT
C
C     CALCULATE OTHER MERCH. TOPS USING THE EQUATIONS IN SNELL AND LITTLE
C
      CVM=CVTS*(1.0+A1*CFTD**A2*DBH**A3)
      IF(CVM .LT. 0.0) CVM=0.0
C
C     CALCUATE STUMP VOLUME
C
      CVS=0.0
      DIBD=(DIB/DIB1FT)**(2./3.)
      SDIB=(((4.5-DIBD-CFSH*(1.0-DIBD))/3.5)**1.5)*DIB1FT
      IF(SDIB.LE.CFTD) THEN
         VALU=0.0
         RETURN
      ENDIF
      CVS=(.25*PI*DIB1FT**2)*((1.0/6174)*((4.5-DIBD)**3*CFSH-
     1     1.5*(4.5-DIBD)**2*(1.0-DIBD)*CFSH**2+
     2     (4.5-DIBD)*(1.0-DIBD)**2*CFSH**3-
     3     (.25)*(1.0-DIBD)**3*CFSH**4))
C
C     CALCUATE TREE'S VOLUME
C
      VALU=CVM-CVS
      IF(VALU .LT. 0.0)VALU=0.0
      RETURN
      END

C**********************************************************************
      SUBROUTINE RAP_HCFV(VERSION,SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,
     1                    VALU)
      IMPLICIT NONE
      INTEGER*4 VERSION,SPGRP,INDSPG
      REAL*4  DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU,HCVPAR(3,2),
     1        HMVPAR(3,3),A1,A2,A3,B0,B1,CVTS,CVM,CVS,PI,DIBD,SDIB,MH
C
C  CUBIC VOLUME - HARDWOODS (2 parameters)
C
      DATA HCVPAR/
     1      0.06345756, 0.04969879, 0.04969879,!  BL,PD,WI
C
     2      0.00208240, 0.00247940, 0.00247940/!  BL,PD,WI
C
C  CUBIC MERCH VOLUME - HARDWODS (3 parameters)
C
      DATA HMVPAR/
     1     -0.4270, -0.3741, -0.3741,       !  BL,PD,WI
C
     2      2.348 ,  3.642 ,  3.642 ,       !  BL,PD,WI
C
     3     -2.276 , -3.406 , -3.406/        !  BL,PD,WI
C
C     CALCULATES CUBIC FOOT VOLUME OF RED ALDER USING THE TAPER EQUATION OF
C         HIBBS, BLUHM, AND GARBER (2007)
      IF(SPGRP .EQ. 1) THEN
C         CALL RA_CFV(DBH,HT,CR,MH,CFSH,CFTD,VALU)
         VALU=0.04969879+0.00247940*DBH**2*HT
         RETURN
      ENDIF
C
C     CALCULATES CUBIC FOOT VOLUMES OF HARDWOODS USING
C          EQUATIONS DEVELOPED FROM THE DATA IN J.A. SNELL AND S.N. LITTLE.
C          1983.  PREDICTING CROWN WEIGHT AND BOLE VOLUME OF FIVE WESTERN
C          HARDWOODS.  GENERAL TECHNICAL REPORT PNW-151.
C
C
      PI=3.14159265
      INDSPG=SPGRP-4
      A1=HMVPAR(INDSPG,1)
      A2=HMVPAR(INDSPG,2)
      A3=HMVPAR(INDSPG,3)
      B0=HCVPAR(INDSPG,1)
      B1=HCVPAR(INDSPG,2)
C
C     CALCULATE DIB AND TREE TOTAL CUBIC FOOT VOLUME
C
      CVTS=B0+B1*DBH**2*HT
C
C     CALCULATE OTHER MERCH. TOPS USING THE EQUATIONS IN SNELL AND LITTLE
C
      CVM=CVTS*(1.0+A1*CFTD**A2*DBH**A3)
      IF(CVM .LT. 0.0) CVM=0.0
C
C     CALCUATE STUMP VOLUME
C
      CVS=0.0
      DIBD=(DIB/DIB1FT)**(2./3.)
      SDIB=(((4.5-DIBD-CFSH*(1.0-DIBD))/3.5)**1.5)*DIB1FT
      IF(SDIB.LE.CFTD) THEN
         VALU=0.0
         RETURN
      ENDIF
      CVS=(.25*PI*DIB1FT**2)*((1.0/6174)*((4.5-DIBD)**3*CFSH-
     1     1.5*(4.5-DIBD)**2*(1.0-DIBD)*CFSH**2+
     2     (4.5-DIBD)*(1.0-DIBD)**2*CFSH**3-
     3     (.25)*(1.0-DIBD)**3*CFSH**4))
C
C     CALCUATE TREE'S VOLUME
C
      VALU=CVM-CVS
      IF(VALU .LT. 0.0)VALU=0.0
      RETURN
      END
C**********************************************************************
      SUBROUTINE SCRIB_VOL(VERSION,ISP,SPGRP,SVOL,LOGLL,LOGTD,LOGSH,
     1           LOGTA,LOGML,DOB,HT,CR,VALU)
C
C     VALUE = SCRIBNER VOLUME CALCULATED
C
C
      IMPLICIT NONE
      INTEGER*4  VERSION,ISP,SPGRP,SVOL,LOGLL
      INTEGER*4  NDI,NLOGTD,NW,I,J,II
      REAL*4  LOGTD,LOGSH,LOGTA,LOGML,DOB,HT,CR,VALU
      REAL*4  D,TLL,EX,HCB,HABH,PDIB,AA1,AA2,A3,A4,ALP,WLT,PP1,PP2,H,
     1        DI,VOLG,NL(40,4),LVOL(40,4),TOTS(2,4),A,B,C,ROOT,MH,HM1,
     2        HM2
C
      D=0.0
      TLL=0.0
      VALU=0.0
      DO I=1,2
         DO J=1,4
            TOTS(I,J)=0.
         ENDDO
      ENDDO
      IF(VERSION .EQ. 4 .AND. ISP .EQ. 351) THEN
         CALL RA_SCRIB(SVOL,LOGLL,LOGTD,LOGSH,LOGTA,LOGML,DOB,HT,CR,
     1                 VALU)
         RETURN
      ENDIF
      IF(ISP .GT. 300) RETURN
      EX=1.0
      HCB=(1.0-CR)*HT
      HABH=HT-4.5
C
      SELECT CASE(VERSION)
        CASE(1)                   ! Southwest Oregon
           CALL SWO_DIB(SPGRP,DOB,CR,PDIB)
           CALL SWO_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
        CASE(2,3)                   ! Western Willamette Valley, SMC
           CALL NWO_DIB(SPGRP,DOB,CR,PDIB)
           CALL NWO_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
        CASE(4)                   !Red Alder Plantations
           CALL RAP_DIB(SPGRP,DOB,PDIB)
           CALL RAP_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
      ENDSELECT
      WLT=(ALP*HCB-4.5)/HABH
      PP1=AA1+AA2*EXP(A3*(HABH/DOB)**2)
      PP2=A4
      IF(PDIB.LE.LOGTD) GO TO 10
C
C     COMPUTE MERCHANTABLE HEIGHT
C
      H=ALP*HCB
      CALL LOGVOL(1,SVOL,LOGLL,WLT,H,HABH,PP1,PP2,PDIB,D,TLL,EX,DI,
     1            VOLG,NL,LVOL,TOTS)
      IF(WLT .LE. 0.0)THEN
         A=-(PP1+1.0)/HABH**2
         B=PP1/HABH
         C=1.0-LOGTD/PDIB
      ELSE IF(WLT .GT. 0. .AND. DI .LE. LOGTD)THEN
         A=PP2/HABH**2
         B=PP1/HABH
         C=1.0-LOGTD/PDIB
      ELSE
         A=(PP2*WLT**2-PP1-2*PP2*WLT-1.0)/(HABH**2*(WLT-1.0)**2)
         B=((2*WLT-1.0+PP2*WLT**2+PP1*WLT**2)-(PP2*WLT**2-PP1-
     1         2*PP2*WLT-1.0))/(HABH*(WLT-1.0)**2)
         C=-(LOGTD/PDIB+(2*WLT-1.0+PP2*WLT**2+PP1*WLT**2)/
     1         (WLT-1.0)**2)
      ENDIF
      ROOT=B**2-4*A*C
      IF(ROOT .LT. 0.0)THEN
        MH=0.
        GO TO 21
      ENDIF
      HM1=(-B+SQRT(ROOT))/(2*A)
      HM2=(-B-SQRT(ROOT))/(2*A)
      IF(HM1 .GT. 0.0 .AND. HM1 .LE. HABH) THEN
         H=HM1+4.5
         CALL LOGVOL(4,SVOL,LOGLL,WLT,H,HABH,PP1,PP2,PDIB,D,TLL,EX,DI,
     1               VOLG,NL,LVOL,TOTS)
         NDI=ANINT(DI*10.0)
         NLOGTD=ANINT(LOGTD*10.0)
         IF(NDI .NE. NLOGTD) HM1=0.0
      ELSE
         HM1=0.0
      ENDIF
      IF(HM2 .GT. 0.0 .AND. HM2 .LE. HABH) THEN
         H=HM2+4.5
         CALL LOGVOL(4,SVOL,LOGLL,WLT,H,HABH,PP1,PP2,PDIB,D,TLL,EX,DI,
     1               VOLG,NL,LVOL,TOTS)
         NDI=ANINT(DI*10.0)
         NLOGTD=ANINT(LOGTD*10.0)
         IF(NDI .NE. NLOGTD) HM2=0.0
      ELSE
         HM2=0.0
      ENDIF
      IF(HM1 .LE. 0. .OR. HM1 .GT. HABH)THEN
         IF(HM2 .LE. 0. .OR. HM2 .GT. HABH)THEN
           MH=0.
           GO TO 21
         ELSE
           MH=HM2
         ENDIF
      ELSE IF(HM2 .LT. 0. .OR. HM2 .GT. HABH)THEN
         MH=HM1
      ELSE
         MH=MAX(HM1,HM2)
      ENDIF
      MH=MH+4.5
   21 CONTINUE
C
C     CALCULATE LOG VOLUMES
C
      NW=AINT((MH-LOGSH)/(FLOAT(LOGLL)+LOGTA))
      IF(NW .LT. 0) NW=0
      TLL=MH-LOGSH-FLOAT(NW)*(FLOAT(LOGLL)+LOGTA)
C
      H=LOGSH
      DO II=1,NW
         H=H+FLOAT(LOGLL)+LOGTA
         CALL LOGVOL(3,SVOL,LOGLL,WLT,H,HABH,PP1,PP2,PDIB,1.0,TLL,EX,DI,
     1               VOLG,NL,LVOL,TOTS)
         VALU=VALU+VOLG
      ENDDO
C
C     COMPUTE VOLUME OF TOP LOG
C
      IF(TLL .GE. (LOGML+LOGTA))THEN
         J=AINT(TLL-LOGTA)
         TLL=FLOAT(J)+LOGTA
         D=TLL/FLOAT(LOGLL)
         H=H+TLL
         CALL LOGVOL(3,SVOL,LOGLL,WLT,H,HABH,PP1,PP2,PDIB,D,TLL,EX,DI,
     1               VOLG,NL,LVOL,TOTS)
         VALU=VALU+VOLG
      ENDIF
   10 CONTINUE
      IF(VALU .LT. 0.)VALU=0.
      RETURN
      END
C**********************************************************************
      SUBROUTINE LOGVOL(N,SVOL,LOGLL,WLT,H,HABH,PP1,PP2,PDIB,D,TLL,EX,
     1           DI,V,NL,LVOL,TOTS)
      IMPLICIT NONE
C     SUBROUTINE LOGVOL(N,SVOL,EX,D,V)
C     ROUTINE TO CALCULATE LOG VOLUME AND ADD TO APPROPRIATE CELL
C
C     N = TYPE OF CALCULATION
C       = 1  MERCHANTABLE HEIGHT
C       = 2  LOG VOLUME
C       = 3  TREE VOLUME
C       = 4  TOP DIAMETER CHECK
C
C     SVOL = SPECIES GROUP FOR LOG REPORT
C
C     EX = TREE RESIDUAL OR CUT EXPANSION FACTOR
C
C     D = RATIO OF LOG LENGTH TO SPECIFIED LOG LENGTH
C
C
      INTEGER*4  N,SVOL,LOGLL
      INTEGER*4  DII,LEN
      REAL*4     WLT,H,HABH,PP1,PP2,PDIB,D,TLL,EX,DI,NL(40,4),
     1           LVOL(40,4),TOTS(2,4),V
      REAL*4     I1,I2,RH,JP1,JP2,A,B,C,SVTBL(80),SVTBL16(6),SVTBL32(6)
      DATA SVTBL/0., .143, .39, .676, 1.07, 1.160, 1.4, 1.501, 2.084,
     1           3.126, 3.749, 4.9, 6.043, 7.14, 8.88, 10., 11.528,
     2           13.29, 14.99, 17.499, 18.99, 20.88, 23.51, 25.218,
     3           28.677, 31.249, 34.22, 36.376, 38.04, 41.06, 44.376,
     4           45.975, 48.99, 50., 54.688, 57.66, 64.319, 66.73, 70.,
     5           75.24, 79.48, 83.91, 87.19, 92.501, 94.99, 99.075,
     6           103.501, 107.97, 112.292, 116.99, 121.65, 126.525,
     7           131.51, 136.51, 141.61, 146.912, 152.21, 157.71,
     8           163.288, 168.99, 174.85, 180.749, 186.623, 193.17,
     9           199.12, 205.685, 211.81, 218.501, 225.685, 232.499,
     X           239.317, 246.615, 254.04, 261.525, 269.04, 276.63,
     1           284.26, 292.501, 300.655,308.97/
      DATA SVTBL16/1.249,1.608,1.854,2.410,3.542,4.167/
      DATA SVTBL32/1.57,1.8,2.2,2.9,3.815,4.499/
C
C     USE TAPER EQUATION TO DETERMINE DIAMETER AT TOP OF LOG
C
      V=0.0
      IF(N .EQ. 1)THEN
         RH=WLT
      ELSE
         RH=(H-4.5)/HABH
      ENDIF
      IF(WLT .LE. 0.0)THEN
         I2=0.0
      ELSE
         I2=1.0
      ENDIF
      IF(RH .GE. 0.0 .AND. RH .LE. WLT)THEN
         I1=0.0
      ELSE
         I1=1.0
      ENDIF
      JP1=(RH-1.0)/(WLT-1.0)
      JP2=(WLT-RH)/(WLT-1.0)
      A=1.0-RH+I2*(RH+I1*(JP1*(1.0+JP2)-1.0))-(RH-1.0)*(RH-I2*RH)
      B=PP1*(I2*(RH+I1*(JP1*(RH+WLT*JP2)-RH))-(RH-1.0)*(RH-I2*RH))
      C=PP2*I2*((RH**2)+I1*(JP1*WLT*(2.0*RH-WLT+WLT*JP2)-RH**2))
      DI=PDIB*(A+B+C)
      IF(N.EQ.1.OR.N.EQ.4)RETURN
C
C     EXTRACT VOLUME FROM VOLUME TABLES
C
      IF(D .LT. 1.0 .OR. D .GT. 1.0)THEN
         LEN=AINT(TLL)
      ELSE
         LEN=LOGLL
      ENDIF
      DII=AINT(DI)
      IF(DII .GE. 6 .AND. DII .LE. 11)THEN
         IF(LEN .GE. 16 .AND. LEN .LE. 31)THEN
            V=(SVTBL16(DII-5)*FLOAT(LEN))*EX
         ELSE IF(LEN .GE. 32 .AND. LEN .LE. 40)THEN
            V=(SVTBL32(DII-5)*FLOAT(LEN))*EX
         ELSE
            V=(SVTBL(DII)*FLOAT(LEN))*EX
         ENDIF
      ELSE
         V=(SVTBL(DII)*FLOAT(LEN))*EX
      ENDIF
      IF(N .EQ. 3) RETURN
      DII=DII/2
      NL(DII,SVOL)=NL(DII,SVOL)+EX
      LVOL(DII,SVOL)=LVOL(DII,SVOL)+V
      TOTS(2,SVOL)=TOTS(2,SVOL)+V
      RETURN
      END
C**********************************************************************
      SUBROUTINE SWO_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 AA1,AA2,A3,A4,ALP,TPAR(8,5)
C
C  TAPER (5 parameters - conifer only)
C
      DATA TPAR/
     1 -0.55029801,-0.342017552, -0.595823501 ,-0.6       ,-0.596278066, !  DF,GW,PP,SP,IC
     1 -0.55029801,-0.596278066, -0.596278066 ,                          !  WH,RC,PY
C
     2 -0.69479837,-0.777574201, -1.25803662  ,-0.48435806,-0.83987883 , !  DF,GW,PP,SP,IC
     2 -0.69479837,-0.83987883 , -0.83987883  ,                          !  WH,RC,PY
C
     3 -6.13100423,-4.33569876 ,-13.867406    ,-3.3249206 ,-6.85768402 , !  DF,GW,PP,SP,IC
     3 -6.13100423,-6.85768402 , -6.85768402  ,                          !  WH,RC,PY
C
     4  0.35697451, 0.672963393,  0.0998711245, 0.10862035, 0.134178717, !  DF,GW,PP,SP,IC
     4  0.35697451, 0.134178717,  0.134178717 ,                          !  WH,RC,PY
C
     5  0.5       , 0.33       ,  0.6         , 0.74      , 0.71       , !  DF,GW,PP,SP,IC
     5  0.5       , 0.71       ,  0.71/                                  !  WH,RC,PY
C
      AA1=TPAR(SPGRP,1)
      AA2=TPAR(SPGRP,2)
      A3=TPAR(SPGRP,3)/100.0
      A4=TPAR(SPGRP,4)
      ALP=TPAR(SPGRP,5)
      RETURN
      END
C**********************************************************************
      SUBROUTINE NWO_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 AA1,AA2,A3,A4,ALP,TPAR(5,5)
C
C  TAPER (5 parameters - conifer only)
C
      DATA TPAR/
     1 -0.55029801,-0.342017552,-0.55029801,-0.596278066,-0.596278066,! DF,GF,WH,RC,PY
C
     2 -0.69479837,-0.777574201,-0.69479837,-0.83987883 ,-0.83987883 ,! DF,GF,WH,RC,PY
C
     3 -6.13100423,-4.33569876 ,-6.13100423,-6.85768402 ,-6.85768402 ,! DF,GF,WH,RC,PY
C
     4  0.35697451, 0.672963393, 0.35697451, 0.134178717, 0.134178717,! DF,GF,WH,RC,PY
C
     5  0.5       ,  0.33      ,  0.5      ,  0.71      ,  0.71      /! DF,GF,WH,RC,PY
C
      AA1=TPAR(SPGRP,1)
      AA2=TPAR(SPGRP,2)
      A3=TPAR(SPGRP,3)
      A4=TPAR(SPGRP,4)
      ALP=TPAR(SPGRP,5)
      RETURN
      END
C**********************************************************************
      SUBROUTINE RAP_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
      IMPLICIT NONE
      INTEGER*4 SPGRP,ISPGRP
      REAL*4 AA1,AA2,A3,A4,ALP,TPAR(3,5)
C
C  TAPER (5 parameters - conifer only)
C
      DATA TPAR/
     1 -0.55029801, -0.55029801, -0.596278066,! DF,WH,RC
C
     2 -0.69479837, -0.69479837, -0.83987883 ,! DF,WH,RC
C
     3 -6.13100423, -6.13100423, -6.85768402 ,! DF,WH,RC
C
     4  0.35697451,  0.35697451,  0.134178717,! DF,WH,RC
C
     5  0.5       ,  0.5       ,  0.71       /! DF,WH,RC
C
      ISPGRP=SPGRP-1
      AA1=TPAR(ISPGRP,1)
      AA2=TPAR(ISPGRP,2)
      A3=TPAR(ISPGRP,3)
      A4=TPAR(ISPGRP,4)
      ALP=TPAR(ISPGRP,5)
      RETURN
      END
C**********************************************************************
      SUBROUTINE TAPER_RA(DBH,HT,CR,HI,DI)
      IMPLICIT NONE
      REAL*4 DBH,HT,CR,HI,X,Z,P,A1,A2,A3,A4,A5,A6,A7,C,DI,D140
      A1=0.9113
      A2=1.0160
      A3=0.2623
      A4=-18.7695
      A5=3.1931
      A6=0.1631
      A7=0.4180
      D140=0.000585+0.997212*DBH
      Z=HI/HT
      P=4.5/HT
      X=(1.0-Z**0.5)/(1.0-P**0.5)
      C=A3*(1.364409*D140**0.3333333*EXP(A4*Z)
     1 +EXP(A5*CR**A6*(D140/HT)**A7*Z))
      DI=A1*D140**A2*X**C
      RETURN
      END
C**********************************************************************
      SUBROUTINE RA_MH(DBH,HT,CR,TD,MH)
      IMPLICIT NONE
      INTEGER*4 IHT,I
      REAL*4 DBH,HT,CR,TD,MH,D0,DI,HI
      IF(TD .LE. 0.0) THEN
         MH=HT
         RETURN
      ENDIF
      CALL TAPER_RA(DBH,HT,CR,0.0,D0)
      IF(D0 .LE. TD) THEN
         MH=0.0
      ELSE
         MH=0.0
         IHT=INT(ANINT(10.0*HT))-1
         DO I=1,IHT
            HI=HT-0.1*FLOAT(I)
            CALL TAPER_RA(DBH,HT,CR,HI,DI)
            IF(DI .GE. TD) THEN
               MH=HI
               EXIT
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE RA_CFV(DBH,HT,CR,MH,SH,TD,CFV)
      IMPLICIT NONE
      INTEGER*4 IMH,ISH,NSEC,NSECDIFF,NSEC2,I
      REAL*4 DBH,HT,CR,MH,SH,TD,FC,HI,D1,D2,D3,LOGV,CFV,DIFF,REMAIN,LOGL
      FC=0.005454154
      IF(TD .LE. 0.0) THEN
         MH=HT
      ELSE
         CALL RA_MH(DBH,HT,CR,TD,MH)
      ENDIF
      IF(MH .EQ. 0.0 .OR. MH .LE. SH) THEN
         CFV=0.0
         RETURN
      ENDIF
      DIFF=MH-SH
      REMAIN=MOD(DIFF,1.0)
      NSEC=INT(ANINT(DIFF-REMAIN))
      IF(TD .LE. 0.0 .AND. DIFF .LE. 0.0) THEN
         NSEC=NSEC-1
      ENDIF
      NSECDIFF=MOD(NSEC,2)
      NSEC2=(NSEC-NSECDIFF)/2
      CFV=0.0
      HI=SH
      CALL TAPER_RA(DBH,HT,CR,HI,D1)
      DO I=1,NSEC2
         HI=HI+1.0
         CALL TAPER_RA(DBH,HT,CR,HI,D2)
         HI=HI+1.0
         CALL TAPER_RA(DBH,HT,CR,HI,D3)
         LOGL=2.0
         LOGV=LOGL*FC*((D1**2+4.0*D2**2+D3**2)/6.0)
         CFV=CFV+LOGV
         D1=D3
      ENDDO
      IF(NSECDIFF .GT. 0) THEN
         HI=HI+1.0
         CALL TAPER_RA(DBH,HT,CR,HI,D2)
         LOGL=1.0
         LOGV=LOGL*FC*((D1**2+D2**2)/2.0)
         CFV=CFV+LOGV
         D1=D2
      ENDIF
      IF(TD .LE. 0.0) THEN
         IF(REMAIN .LE. 0.0) THEN
            LOGL=1.0
         ELSE
            LOGL=REMAIN
         ENDIF
         LOGV=LOGL*FC*(D1**2)/3.0
         CFV=CFV+LOGV
      ELSE
         IF(REMAIN .GT. 0.0) THEN
            CALL TAPER_RA(DBH,HT,CR,MH,D2)
            LOGL=REMAIN
            LOGV=LOGL*FC*((D1**2+D2**2)/2.0)
            CFV=CFV+LOGV
         ENDIF
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE RA_SCRIB(SVOL,LOGLL,LOGTD,LOGSH,LOGTA,LOGML,DOB,HT,CR,
     1                    VALU)
C
C     VALUE = SCRIBNER VOLUME CALCULATED
C
C
      IMPLICIT NONE
      INTEGER*4  VERSION,SVOL,LOGLL
      INTEGER*4  NDI,NLOGTD,NW,I,J,II
      REAL*4  LOGTD,LOGSH,LOGTA,LOGML,DOB,HT,CR,VALU
      REAL*4  D,TLL,EX,H,DI,VOLG,NL(40,4),LVOL(40,4),TOTS(2,4),MH
C
      D=0.0
      TLL=0.0
      VALU=0.0
      DO I=1,2
         DO J=1,4
            TOTS(I,J)=0.
         ENDDO
      ENDDO
      EX=1.0
C
C     COMPUTE MERCHANTABLE HEIGHT
C
      IF(LOGTD .LE. 0.0) THEN
         MH=HT
      ELSE
         CALL RA_MH(DOB,HT,CR,LOGTD,MH)
      ENDIF
      IF(MH .EQ. 0.0 .OR. MH .LE. LOGSH) THEN
         VALU=0.0
         RETURN
      ENDIF
C
C     CALCULATE LOG VOLUMES
C
      NW=AINT((MH-LOGSH)/(FLOAT(LOGLL)+LOGTA))
      IF(NW .LT. 0) NW=0
      TLL=MH-LOGSH-FLOAT(NW)*(FLOAT(LOGLL)+LOGTA)
      D=1.0
C
      H=LOGSH
      DO II=1,NW
         H=H+FLOAT(LOGLL)+LOGTA
         CALL RA_LOGVOL(3,DOB,HT,CR,SVOL,LOGLL,H,D,TLL,EX,DI,VOLG,NL,
     1                  LVOL,TOTS)
C         CALL RA_LOGVOL(3,DOB,HT,LOGLL,H,D,TLL,EX,DI,VOLG,NL,LVOL,TOTS)
         VALU=VALU+VOLG
      ENDDO
C
C     COMPUTE VOLUME OF TOP LOG
C
      IF(TLL .GE. (LOGML+LOGTA))THEN
         J=AINT(TLL-LOGTA)
         TLL=FLOAT(J)+LOGTA
         D=TLL/FLOAT(LOGLL)
         H=H+TLL
         CALL RA_LOGVOL(3,DOB,HT,CR,SVOL,LOGLL,H,D,TLL,EX,DI,VOLG,NL,
     1                  LVOL,TOTS)
C         CALL RA_LOGVOL(3,DOB,HT,LOGLL,H,D,TLL,EX,DI,VOLG,NL,LVOL,TOTS)
         VALU=VALU+VOLG
      ENDIF
   10 CONTINUE
      IF(VALU .LT. 0.)VALU=0.
      RETURN
      END
C**********************************************************************
      SUBROUTINE RA_LOGVOL(N,DBH,HT,CR,SVOL,LOGLL,HI,D,TLL,EX,DI,V,NL,
     1                     LVOL,TOTS)
C      SUBROUTINE RA_LOGVOL(N,DBH,HT,LOGLL,HI,D,TLL,EX,DI,V,NL,LVOL,TOTS)
      IMPLICIT NONE
C     ROUTINE TO CALCULATE LOG VOLUME AND ADD TO APPROPRIATE CELL
C
C     N = TYPE OF CALCULATION
C       = 1  MERCHANTABLE HEIGHT
C       = 2  LOG VOLUME
C       = 3  TREE VOLUME
C       = 4  TOP DIAMETER CHECK
C
C     SVOL = SPECIES GROUP FOR LOG REPORT
C
C     EX = TREE RESIDUAL OR CUT EXPANSION FACTOR
C
C     D = RATIO OF LOG LENGTH TO SPECIFIED LOG LENGTH
C
C
      INTEGER*4  N,SVOL,LOGLL
      INTEGER*4  DII,LEN
      REAL*4     DBH,HT,CR,HI,D,TLL,EX,DI,V,NL(40,4),LVOL(40,4),
     1           TOTS(2,4)
      REAL*4     I1,I2,RH,JP1,JP2,A,B,C,SVTBL(80),SVTBL16(6),SVTBL32(6)
      DATA SVTBL/0., .143, .39, .676, 1.07, 1.160, 1.4, 1.501, 2.084,
     1           3.126, 3.749, 4.9, 6.043, 7.14, 8.88, 10., 11.528,
     2           13.29, 14.99, 17.499, 18.99, 20.88, 23.51, 25.218,
     3           28.677, 31.249, 34.22, 36.376, 38.04, 41.06, 44.376,
     4           45.975, 48.99, 50., 54.688, 57.66, 64.319, 66.73, 70.,
     5           75.24, 79.48, 83.91, 87.19, 92.501, 94.99, 99.075,
     6           103.501, 107.97, 112.292, 116.99, 121.65, 126.525,
     7           131.51, 136.51, 141.61, 146.912, 152.21, 157.71,
     8           163.288, 168.99, 174.85, 180.749, 186.623, 193.17,
     9           199.12, 205.685, 211.81, 218.501, 225.685, 232.499,
     X           239.317, 246.615, 254.04, 261.525, 269.04, 276.63,
     1           284.26, 292.501, 300.655,308.97/
      DATA SVTBL16/1.249,1.608,1.854,2.410,3.542,4.167/
      DATA SVTBL32/1.57,1.8,2.2,2.9,3.815,4.499/
C
C     USE TAPER EQUATION TO DETERMINE DIAMETER AT TOP OF LOG
C
      V=0.0
      CALL TAPER_RA(DBH,HT,CR,HI,DI)

      IF(N.EQ.1 .OR. N.EQ.4)RETURN
C
C     EXTRACT VOLUME FROM VOLUME TABLES
C
      IF(D .LT. 1.0 .OR. D .GT. 1.0)THEN
         LEN=AINT(TLL)
      ELSE
         LEN=LOGLL
      ENDIF
      DII=AINT(DI)
      IF(DII .GE. 6 .AND. DII .LE. 11)THEN
         IF(LEN .GE. 16 .AND. LEN .LE. 31)THEN
            V=(SVTBL16(DII-5)*FLOAT(LEN))*EX
         ELSE IF(LEN .GE. 32 .AND. LEN .LE. 40)THEN
            V=(SVTBL32(DII-5)*FLOAT(LEN))*EX
         ELSE
            V=(SVTBL(DII)*FLOAT(LEN))*EX
         ENDIF
      ELSE
         V=(SVTBL(DII)*FLOAT(LEN))*EX
      ENDIF
      IF(N .EQ. 3) RETURN
      DII=DII/2
      NL(DII,SVOL)=NL(DII,SVOL)+EX
      LVOL(DII,SVOL)=LVOL(DII,SVOL)+V
      TOTS(2,SVOL)=TOTS(2,SVOL)+V
      RETURN
      END
