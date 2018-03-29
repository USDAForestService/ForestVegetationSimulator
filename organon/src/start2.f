C
C ORGANON $Id: start2.f 0000 2018-02-14 00:00:00Z gedixon $
C
C     ORGANON GROWTH AND YIELD MODEL
C     SUBROUTINES INCLUDED:
C        HDCALIB
C        PRDHT
C        A_HD_SWO
C        A_HD_RAP
C        CRCALIB
C        PRDCR
C        A_HCB_SWO
C        A_HCB_RAP
C        HS_H40
C        B_H40
C        F_H40
CC        NCH40
C        WHHLB_SI_UC
C        WHHLB_H40
C        SITECV_F
C        SITEF_C
C        SITEF_SI
C        DFORTY
C        SPMIX
C        HD40_SWO
C        HD40_NWO
C        HD40_SMC
C        HD40_RAP
C        GET_CCFL
C        CALTST
C
C
C  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
C               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
C               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
C               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.
C
C  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - GET_CCFL TO GET_CCFL_EDIT
C  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SITECV_F TO SITECV_F_EDIT
C  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SITEF_C TO SITEF_C_EDIT
C  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SITEF_SI TO SITEF_SI_EDIT
C  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - WHHLB_SI_UC TO WHHLB_SI_UC_EDIT
C
**********************************************************************
      SUBROUTINE HDCALIB(VERSION,IB,NSPN,NTREES,NPTS,TAGE,BHAGE,TDATAI,
     1           EVEN,SI_1,SI_2,PDEN,TDATAR,D40,HT40,PDF,PTF,PPP,PWH,
     2           PRA,ENTHT,ENTDBH,PTRHT,CALIB)
      IMPLICIT NONE
C
C     ROUTINE TO CALIBRATE HEIGHT/DIAMETER EQUATIONS
C
      INTEGER*4    TAGE,BHAGE,ENTHT(18),ENTDBH(18),IB,NSPN,NTREES,NPTS,
     1             TDATAI(2000,3),VERSION
      REAL*4       CALIB(3,18),SI_1,SI_2,PTRHT(2000),TDATAR(2000,4)
      LOGICAL*2    EVEN
      INTEGER*4 I,ISPGRP
      REAL*4    X,YXS(18),XSS(18),Y,WT,YSS(18),BETA,PDF,PTF,PPP,PWH,
     1          PRA,HT40,D40,AGE1,DBH,PDEN,SI_UC,AGE2
C      INTEGER*4 IANS,IYN
C      IF (J .EQ. 1) THEN
C         WRITE(*,*) ' GROWTH = ',GROWTH(J),' PDG = ',PDG
CC         WRITE(*,*) ' BAL1(1) = ',BAL1(1)
C         WRITE(*,1600)
C 1600    FORMAT(1X,' '\)
C         IANS = IYN(2)
C      ENDIF
C
C     DETERMINE CALIBRATION RATIO FOR SPECIES GROUPS
C
      DO I=1,18
        ENTHT(I)=0
        ENTDBH(I)=0
        CALIB(1,I)=1.0
        YXS(I)=0.0
        XSS(I)=0.0
        YSS(I)=0.0
      ENDDO
      D40=0.0
      HT40=0.0
      PDF=0.0
      PTF=0.0
      PPP=0.0
      PWH=0.0
      PRA=0.0
      IF(EVEN) THEN
         AGE1=FLOAT(BHAGE)
         AGE2=FLOAT(TAGE)
         CALL SPMIX(NTREES,NPTS,TDATAI,TDATAR,PDF,PTF,PPP,PWH,PRA)
         CALL DFORTY(VERSION,NTREES,NPTS,IB,TDATAI,TDATAR,D40)
         SELECT CASE(VERSION)
            CASE(1)
               IF(PDF .GE. 0.80) THEN
                  CALL HS_H40(HT40,1,AGE1,SI_1)
               ELSEIF(PTF .GE. 0.80) THEN
                  CALL HS_H40(HT40,1,AGE1,SI_1)
               ELSEIF(PPP .GE. 0.80) THEN
                  CALL HS_H40(HT40,2,AGE1,SI_2)
               ENDIF
            CASE(2,3)
               IF(PDF .GE. 0.80) THEN
                  CALL B_H40(HT40,AGE1,SI_1)
               ELSEIF(PWH .GE. 0.80) THEN
                  CALL F_H40(HT40,AGE1,SI_2)
               ENDIF
            CASE(4)
               IF(PRA .GE. 0.80) THEN
                  CALL WHHLB_SI_UC_EDIT(SI_1,PDEN,SI_UC)
                  CALL WHHLB_H40_EDIT(SI_UC,20.0,AGE2,HT40)
               ENDIF
         ENDSELECT
      ENDIF
      DO I=1,NTREES
         ISPGRP=TDATAI(I,2)
         DBH=TDATAR(I,1)
         ENTDBH(ISPGRP)=ENTDBH(ISPGRP)+1
         IF(TDATAR(I,2) .LE. 0.0) CYCLE
C
C        CALCULATE TREE HEIGHT
C
         SELECT CASE(VERSION)
            CASE(1)
               IF(PDF .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
                  CALL HD40_SWO(1,HT40,D40,DBH,PTRHT(I))
               ELSEIF(PTF .GE. 0.80 .AND. ISPGRP .EQ. 2) THEN
                  CALL HD40_SWO(2,HT40,D40,DBH,PTRHT(I))
               ELSEIF(PPP .GE. 0.80 .AND. ISPGRP .EQ. 3) THEN
                  CALL HD40_SWO(3,HT40,D40,DBH,PTRHT(I))
               ELSE
                  CALL A_HD_SWO(ISPGRP,DBH,PTRHT(I))
               ENDIF
            CASE(2)
               IF(PDF .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
                  CALL HD40_NWO(1,HT40,D40,DBH,PTRHT(I))
               ELSEIF(PWH .GE. 0.80 .AND. ISPGRP .EQ. 3) THEN
                  CALL HD40_NWO(2,HT40,D40,DBH,PTRHT(I))
               ELSE
                  CALL HD_NWO(ISPGRP,DBH,PTRHT(I))
               ENDIF
            CASE(3)
               IF(PDF .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
                  CALL HD40_SMC(1,HT40,D40,DBH,PTRHT(I))
               ELSEIF(PWH .GE. 0.80 .AND. ISPGRP .EQ. 3) THEN
                  CALL HD40_SMC(2,HT40,D40,DBH,PTRHT(I))
               ELSE
                  CALL HD_SMC(ISPGRP,DBH,PTRHT(I))
               ENDIF
            CASE(4)
               IF(PRA .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
                  CALL HD40_RAP(1,HT40,D40,DBH,PTRHT(I))
               ELSE
                  CALL A_HD_RAP(ISPGRP,DBH,PTRHT(I))
               ENDIF
         ENDSELECT
         Y=TDATAR(I,2)-4.5
         X=PTRHT(I)-4.5
         WT=TDATAR(I,1)
         YXS(ISPGRP)=YXS(ISPGRP)+Y*X/WT
         XSS(ISPGRP)=XSS(ISPGRP)+X*X/WT
         YSS(ISPGRP)=YSS(ISPGRP)+Y*Y/WT
         ENTHT(ISPGRP)=ENTHT(ISPGRP)+1
      ENDDO
      DO I=1, NSPN
         IF(ENTDBH(I) .EQ. 0) CYCLE
         IF(ENTHT(I) .LT. 2) CYCLE
         CALL CALTST(YXS(I),XSS(I),YSS(I),ENTHT(I),BETA)
         CALIB(1,I)=BETA
      ENDDO
      RETURN
      END
**********************************************************************
      SUBROUTINE PRDHT(VERSION,NTREES,TDATAI,ENT,TDATAR,D40,HT40,
     1                 PDF,PTF,PPP,PWH,PRA,CALIB,PTRHT)
      IMPLICIT NONE
C
C     ROUTINE TO PREDICT MISSING TREE HEIGHTS
C
      INTEGER*4    NTREES,TDATAI(2000,3),VERSION
      REAL*4       CALIB(3,18),PTRHT(2000),TDATAR(2000,4)
      INTEGER*4 ENT,I,ISPGRP
      REAL*4    PDF,PTF,PPP,PWH,PRA,HT40,D40,DBH
      ENT=0
      DO I=1,NTREES
         ISPGRP=TDATAI(I,2)
         DBH=TDATAR(I,1)
         IF(DBH .LE. 0.0) CYCLE
         IF(TDATAR(I,2) .GT. 0.0) CYCLE
C
C        CALCULATE TREE HEIGHT
C
         SELECT CASE(VERSION)
            CASE(1)
               IF(PDF .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
                  CALL HD40_SWO(1,HT40,D40,DBH,PTRHT(I))
               ELSEIF(PTF .GE. 0.80 .AND. ISPGRP .EQ. 2) THEN
                  CALL HD40_SWO(2,HT40,D40,DBH,PTRHT(I))
               ELSEIF(PPP .GE. 0.80 .AND. ISPGRP .EQ. 3) THEN
                  CALL HD40_SWO(3,HT40,D40,DBH,PTRHT(I))
               ELSE
                  CALL A_HD_SWO(ISPGRP,DBH,PTRHT(I))
               ENDIF
            CASE(2)
               IF(PDF .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
                  CALL HD40_NWO(1,HT40,D40,DBH,PTRHT(I))
               ELSEIF(PWH .GE. 0.80 .AND. ISPGRP .EQ. 3) THEN
                  CALL HD40_NWO(2,HT40,D40,DBH,PTRHT(I))
               ELSE
                  CALL HD_NWO(ISPGRP,DBH,PTRHT(I))
               ENDIF
            CASE(3)
               IF(PDF .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
                  CALL HD40_SMC(1,HT40,D40,DBH,PTRHT(I))
               ELSEIF(PWH .GE. 0.80 .AND. ISPGRP .EQ. 3) THEN
                  CALL HD40_SMC(2,HT40,D40,DBH,PTRHT(I))
               ELSE
                  CALL HD_SMC(ISPGRP,DBH,PTRHT(I))
               ENDIF
            CASE(4)
               IF(PRA .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
                  CALL HD40_RAP(1,HT40,D40,DBH,PTRHT(I))
               ELSE
                  CALL A_HD_RAP(ISPGRP,DBH,PTRHT(I))
               ENDIF
         ENDSELECT
         ENT=ENT+1
         TDATAR(I,2)=4.5+CALIB(1,ISPGRP)*(PTRHT(I)-4.5)
         IF(TDATAR(I,2) .LT. 4.6)TDATAR(I,2)=4.6
      ENDDO
      RETURN
      END
C
**********************************************************************
      SUBROUTINE A_HD_SWO(ISPGRP,DBH,PRDHT)
      IMPLICIT NONE
      REAL*4 DBH,PRDHT,HDPAR(18,3),B0,B1,B2
      INTEGER*4 ISPGRP
C
C  NEW HEIGHT/DIAMETER PARAMETERS FOR ALL (UNDAMAGED AND DAMAGED) TREES
C      (3 parameters - all species)
C
C     DF Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     GW Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     PP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     SP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     IC Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     WH Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     MD Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     GC Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     TA Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     CL Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     BL Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     BO Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     WI Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C
      DATA HDPAR/
     1             7.153156143,  6.638003799, 7.181264435, 6.345116767, !  DF,GW,PP,SP
     1             8.776627288,  6.58804    , 6.14817441 , 6.402691396, !  IC,WH,RC,PY
     1             5.42457261 ,  9.21600278 , 7.398142262, 7.762149257, !  MD,GC,TA,CL
     1             5.02002617 ,  4.69753118 , 4.907340242, 5.59759126 , !  BL,WO,BO,RA
     1             5.252315215,  3.862132151,                           !  PD,WI
C
     2            -5.36900835 , -5.44399465 ,-5.90709219 ,-5.30026188 , !  DF,GW,PP,SP
     2            -7.4383668  , -5.35325461 ,-5.40092761 ,-4.79802411 , !  IC,WH,RC,PY
     2            -3.56317104 , -7.63409138 ,-5.5099273  ,-6.04759773 , !  MD,GC,TA,CL
     2            -2.51228202 , -3.51586969 ,-3.18017969 ,-3.19942952 , !  BL,WO,BO,RA
     2            -3.13509983 , -1.5294776  ,                           !  PD,WI
C
     3            -0.25832512 , -0.33929196 ,-0.27533719 ,-0.35264183 , !  DF,GW,PP,SP
     3            -0.16906224 , -0.31897786 ,-0.38922036 ,-0.16317997 , !  IC,WH,RC,PY
     3            -0.36177689 , -0.15346440 ,-0.19080702 ,-0.16308399 , !  MD,GC,TA,CL
     3            -0.42256497 , -0.57665068 ,-0.46654227 ,-0.38783403 , !  BL,WO,BO,RA
     3            -0.26979750 , -0.62476287 /                           !  PD,WI
C
      B0=HDPAR(ISPGRP,1)
      B1=HDPAR(ISPGRP,2)
      B2=HDPAR(ISPGRP,3)
      PRDHT=4.5+EXP(B0+B1*DBH**B2)
      RETURN
      END
**********************************************************************
      SUBROUTINE A_HD_RAP(ISPGRP,DBH,PRDHT)
      IMPLICIT NONE
      REAL*4 DBH,PRDHT,HDPAR(7,3),B0,B1,B2
      INTEGER*4 ISPGRP
C
C  HEIGHT/DIAMETER PARAMETERS (UNDAMAGED AND DAMAGED) TREES
C     (3 parameters - all species)
C
C     RA Coefficients from Hann, Bluhm, and Hibbs (2011) Forest Biometrics Research Paper 1
C     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     BL Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     PD Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     WI Coefficients from Wang and Hann (1988) FRL Research Paper 51
C
      DATA HDPAR/
     1            6.76804202 ,  7.262195456,  6.555344622,  6.14817441,!  RA,DF,WH,RC
     1            5.21462    ,  4.49727    ,  4.88361    ,             !  BL,PD,WI
C
     2           -4.6370303  , -5.899759104, -5.137174162, -5.40092761,!  RA,DF,WH,RC
     2           -2.70252    , -2.07667    , -2.47605    ,             !  BL,PD,WI
C
     3           -0.23108894 , -0.287207389, -0.364550800, -0.38922036,!  RA,DF,WH,RC
     3           -0.354756   ,  -0.388650  , -0.309050  /              !  BL,PD,WI
C
      B0=HDPAR(ISPGRP,1)
      B1=HDPAR(ISPGRP,2)
      B2=HDPAR(ISPGRP,3)
      PRDHT=4.5+EXP(B0+B1*DBH**B2)
      RETURN
      END
**********************************************************************
      SUBROUTINE CRCALIB(VERSION,IB,NSPN,NTREES,NPTS,TDATAI,
     1           ENTDBH,SI_1,SI_2,TDATAR,STDATAR,BAL,BALL,CCFL,CCFLL,
     2           SBA,OG,ENTCR,PCR,CALIB)
      IMPLICIT NONE
C
C     ROUTINE TO CALIBRATE CROWN RATIOS
C
      INTEGER*4   VERSION,IB,NSPN,NTREES,NPTS,TDATAI(2000,3),
     1            ENTDBH(18),ENTCR(18)
      REAL*4      SI_1,SI_2,TDATAR(2000,4),STDATAR(2000,4),BAL(500),
     1            BALL(51),CCFL(500),CCFLL(51),SBA,OG,PCR(2000),
     2            CALIB(3,18)
      INTEGER*4   I,ISPGRP
      REAL*4      YSS(18),X,Y,XSS(18),YXS(18),DBH,HT,XSI_1,XSI_2,
     1            SCCFL,PHCB,BETA
C      OPEN(44,FILE='TEMP.DAT',STATUS='UNKNOWN')
C
      DO I=1,18
         ENTCR(I)=0
         CALIB(2,I)=1.0
         YSS(I)=0.0
         XSS(I)=0.0
         YXS(I)=0.0
      ENDDO

      CALL SSUM(2,VERSION,NPTS,NTREES,TDATAI,TDATAR,STDATAR,SBA,
     1          BAL,BALL,CCFL,CCFLL)
      CALL OLDGROWTH(NPTS,NTREES,IB,TDATAI,TDATAR,OG)
      DO I=1,NTREES
         IF(TDATAR(I,3) .LE. 0.0) CYCLE
         ISPGRP=TDATAI(I,2)
         DBH=TDATAR(I,1)
         HT=TDATAR(I,2)
         XSI_1=SI_1-4.5
         XSI_2=SI_2-4.5
         CALL GET_CCFL_EDIT(DBH,CCFLL,CCFL,SCCFL)
C         WRITE(44,2002) HT,DBH,SCCFL,SBA,SI_1
C 2002    FORMAT(F16.6,1X,F16.6,1X,F16.6,1X,F16.6,1X,F16.6)
         SELECT CASE(VERSION)
            CASE(1)
               CALL A_HCB_SWO(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,
     1                      PHCB)
               PCR(I)=1.0-PHCB/HT
            CASE(2)
               CALL HCB_NWO(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,
     1                      PHCB)
               PCR(I)=1.0-PHCB/HT
            CASE(3)
               CALL HCB_SMC(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,
     1                      PHCB)
               PCR(I)=1.0-PHCB/HT
            CASE(4)
               CALL A_HCB_RAP(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,
     1                      PHCB)
               PCR(I)=1.0-PHCB/HT
         ENDSELECT
         X=PCR(I)
         Y=TDATAR(I,3)
C         WRITE(44,2000) X,Y
C 2000    FORMAT(F8.4,1X,F8.4)
         YXS(ISPGRP)=YXS(ISPGRP)+X*Y
         XSS(ISPGRP)=XSS(ISPGRP)+X*X
         YSS(ISPGRP)=YSS(ISPGRP)+Y*Y
         ENTCR(ISPGRP)=ENTCR(ISPGRP)+1
      ENDDO
      DO I=1,NSPN
         IF(ENTDBH(I) .EQ. 0) CYCLE
         IF(ENTCR(I) .LT. 2)CYCLE
         CALL CALTST(YXS(I),XSS(I),YSS(I),ENTCR(I),BETA)
         CALIB(2,I)=BETA
      ENDDO
C      WRITE(44,2001) YXS(1),XSS(1),YSS(1),BETA
C 2001 FORMAT(F16.6,1X,F16.6,1X,F16.6,1X,F16.6)
C      CLOSE(44)
      RETURN
      END
**********************************************************************
      SUBROUTINE PRDCR(VERSION,IB,NSPN,NTREES,NPTS,TDATAI,SI_1,
     1           SI_2,TDATAR,CCFL,CCFLL,SBA,OG,ENT,PCR,CALIB)
      IMPLICIT NONE
C
C     ROUTINE TO CALCULATE MISSING CROWN RATIOS
C

      INTEGER*4   VERSION,IB,NSPN,NTREES,NPTS,TDATAI(2000,3),ENT
      REAL*4      SI_1,SI_2,TDATAR(2000,4),CCFL(500),CCFLL(51),SBA,OG,
     1            PCR(2000),CALIB(3,18)
      INTEGER*4   I,ISPGRP
      REAL*4      DBH,HT,XSI_1,XSI_2,SCCFL,PHCB
      REAL*4 DANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      DANUW = REAL(IB)
      DANUW = REAL(NPTS)
      DANUW = REAL(NSPN)
C
      ENT=0
      DO I=1,NTREES
         IF(TDATAR(I,3) .GT. 0.0) CYCLE
C
C        CALCULATE  CROWN RATIO
C
         ENT=ENT+1
         ISPGRP=TDATAI(I,2)
         DBH=TDATAR(I,1)
         IF(DBH .LE. 0.0) CYCLE
         HT=TDATAR(I,2)
         XSI_1=SI_1-4.5
         XSI_2=SI_2-4.5
         CALL GET_CCFL_EDIT(DBH,CCFLL,CCFL,SCCFL)
         SELECT CASE(VERSION)
            CASE(1)
               CALL A_HCB_SWO(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,
     1                      PHCB)
               PCR(I)=1.0-PHCB/HT
            CASE(2)
               CALL HCB_NWO(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,
     1                      PHCB)
               PCR(I)=1.0-PHCB/HT
            CASE(3)
               CALL HCB_SMC(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,
     1                      PHCB)
               PCR(I)=1.0-PHCB/HT
            CASE(4)
               CALL A_HCB_RAP(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,
     1                      PHCB)
               PCR(I)=1.0-PHCB/HT
         ENDSELECT
         TDATAR(I,3)=PCR(I)*CALIB(2,TDATAI(I,2))
         IF(TDATAR(I,3) .GT. 1.0) TDATAR(I,3)=1.0
         IF(TDATAR(I,3) .LT. 0.05) TDATAR(I,3)=0.05
      ENDDO
      RETURN
      END
C**********************************************************************
      SUBROUTINE A_HCB_SWO(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(18,7),B0,B1,B2,B3,
     1       B4,B5,B6
C
C  NEW HEIGHT TO CROWN BASE FOR ALL (UNDAMAGED AND DAMAGED) TREES
C        (7 PARameters - all species)
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
     1        1.990155033,  4.800089990,  2.024723585 ,  3.582314301,  !  DF,GW,PP,SP
     1        3.127730861,  0.0        ,  4.49102006  ,  0.0        ,  !  IC,WH,RC,PY
     1        3.271130882,  0.387912505,  0.4488479442,  1.285465907,  !  MD,GC,TA,CL
     1        1.000364090,  1.05786632 ,  2.672850866 ,  0.56713781 ,  !  BL,WO,BO,RA
     1        0.0        ,  0.0        ,                               !  PD,WI
C
     2       -0.008180786,  0.0        , -0.001953589 , -0.003256792,  !  DF,GW,PP,SP
     2       -0.004386780,  0.0        ,  0.0         ,  0.0        ,  !  IC,WH,RC,PY
     2        0.0        , -0.015000868, -0.009375810 , -0.024459278,  !  MD,GC,TA,CL
     2       -0.010636441,  0.0        ,  0.0         , -0.010377976,  !  BL,WO,BO,RA
     2        0.0        ,  0.0        ,                               !  PD,WI
C
     3       -0.004696095, -0.003268539, -0.001837480 ,  0.0        ,  !  DF,GW,PP,SP
     3       -0.003557122,  0.0        , -0.00132412  ,  0.0        ,  !  IC,WH,RC,PY
     3        0.0        , -0.004098099, -0.001822050 , -0.003992574,  !  MD,GC,TA,CL
     3       -0.005950398, -0.00183283 , -0.001400851 , -0.002066036,  !  BL,WO,BO,RA
     3       -0.004842962, -0.004842962,                               !  PD,WI
C
     4       -0.392033240, -0.858744969, -0.568909853 , -0.765250973,  !  DF,GW,PP,SP
     4       -0.637929879,  0.0        , -1.01460531  ,  0.0        ,  !  IC,WH,RC,PY
     4       -0.841331291,  0.0        ,  0.0         ,  0.0        ,  !  MD,GC,TA,CL
     4        0.0        , -0.28644547 , -0.605971926 ,  0.0        ,  !  BL,WO,BO,RA
     4       -0.567987126, -0.567987126,                               !  PD,WI
C
     5        1.945708371,  0.0        ,  4.831886553 ,  3.043845568,  !  DF,GW,PP,SP
     5        0.977816058,  3.246352823,  0.0         ,  1.225564582,  !  IC,WH,RC,PY
     5        1.791699815,  2.104871164,  0.0         ,  0.0        ,  !  MD,GC,TA,CL
     5        0.0        ,  0.0        ,  0.0         ,  1.39796223 ,  !  BL,WO,BO,RA
     5        0.0        ,  0.0        ,                               !  PD,WI
C
     6        0.007854260,  0.0        ,  0.001653030 ,  0.0        ,  !  DF,GW,PP,SP
     6        0.005850321,  0.0        ,  0.01340624  ,  0.0        ,  !  IC,WH,RC,PY
     6        0.0        ,  0.0        ,  0.0         ,  0.0        ,  !  MD,GC,TA,CL
     6        0.0        ,  0.0        ,  0.0         ,  0.0        ,  !  BL,WO,BO,RA
     6        0.0281315332, 0.0281315332,                              !  PD,WI
C
     7        0.295593583,  0.275679490,  0.0         ,  0.0        ,  !  DF,GW,PP,SP
     7        0.257070387,  0.0        ,  0.0         ,  0.0        ,  !  IC,WH,RC,PY
     7        0.927163029,  0.352773356,  0.233233237 ,  0.0        ,  !  MD,GC,TA,CL
     7        0.310672769,  0.0        ,  0.430988703 ,  0.0        ,  !  BL,WO,BO,RA
     7        0.0        ,  0.0/                                       !  PD,WI
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
      SUBROUTINE A_HCB_RAP(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(7,8),B0,B1,B2,B3,
     1       B4,B5,B6,K,SITE
C
C  HEIGHT TO CROWN BASE (UNDAMAGED AND DAMAGED) TREES
C     (7 parameters - all species)
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
     1       3.98915507  ,  6.18464679 ,  1.92682    ,  4.49102006 ,   !  RA,DF,WH,RC
     1       0.9411395642,  0.0        ,  0.0        ,                 !  BL,PD,WI
C
     2      -0.019280895 , -0.00328764 , -0.00280478 ,  0.0        ,   !  RA,DF,WH,RC
     2      -0.00768402  ,  0.0        ,  0.0        ,                 !  BM,PD,WI
C
     3      -0.0017632543, -0.00136555 , -0.0011939  , -0.00132412 ,   !  RA,DF,WH,RC
     3      -0.005476131 , -0.005666559, -0.005666559,                 !  BL,PD,WI
C
     4      -1.1178816   , -1.19702220 , -0.513134   , -1.01460531 ,   !  RA,DF.WH,RC
     4       0.0         , -0.745540494, -0.745540494,                 !  BL,PD,WI
C
     5       7.12804469  ,  3.17028263 ,  3.68901    ,  0.0        ,   !  RA,DF,WH,RC
     5       0.0         ,  0.0        ,  0.0        ,                 !  BL,PD,WI
C
     6       0.0240273988,  0.0        ,  0.00742219 ,  0.01340624 ,   !  RA,DF,WH,RC
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
            SITE=(0.480 +( 1.110 * (SI_2+4.5)))-4.5
         ENDIF
         HCB=HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)
     1      +B5*SITE+B6*OG**2))
      ENDIF
      RETURN
      END
**********************************************************************
      SUBROUTINE HS_H40(HT40,ISP,AGE,SI)
C
C----------------------------------------------------------------------
C     Hann, D.W. and J.A. Scrivani.  1987.  Dominanat-height-growth and
C     site-index equations for Douglas-fir and ponderosa pine in
C     Southwest Oregon.  Oregon State University, Forest Research
C     Laboratory.  Research Bulletin 59.  13p.   Douglas-fir
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      REAL*4 AGE,HT40,X1,X2,SI,S
      INTEGER*4 ISP
C

        S=SI-4.5
        IF(ISP .EQ. 1) THEN
           X1 = 1.0-EXP(-EXP(-6.21693 + 0.281176*ALOG(S)
     1          + 1.14354*ALOG(AGE)))
           X2 = 1.0-EXP(-EXP(-6.21693 + 0.281176*ALOG(S)
     1          + 1.14354*ALOG(50.0)))
        ELSEIF(ISP .EQ. 2) THEN
           X1 = 1.0-EXP(-EXP(-6.54707 + 0.288169*ALOG(S)
     1          + 1.21297*ALOG(AGE)))
           X2 = 1.0-EXP(-EXP(-6.54707 + 0.288169*ALOG(S)
     1          + 1.21297*ALOG(50.0)))
        ENDIF
        HT40 = 4.5 + S*(X1/X2)
C
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE B_H40(HT40,AGE,SI)
C
C----------------------------------------------------------------------
C     Bruce, D.  1981.  Consistent height-growth and growth-rate
C     estimates for remeasured plots.  Forest Science 27:711-725.
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      REAL*4 AGE,HT40,SI,A1,A2
C
        A2 = -0.447762 - 0.894427*SI/100.0 + 0.793548*(SI/100.0)**2
     1       -0.17166*(SI/100.0)**3
        A1 = LOG(4.5/SI)/((13.25 - (SI/20.0))**A2 - (63.25 -
     1       (SI/20.0))**A2)
        HT40 = SI*EXP(A1*((AGE + 13.25 - (SI/20.0))**A2
     1       - (63.25 - (SI/20.0))**A2))
C
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE F_H40(HT40,AGE,SI)
C
C----------------------------------------------------------------------
C     Top height (option 2) and site curves (option 1) for western
C     hemlock by J. Flewelling (unpublished).  Note, these are metric
C     curves.  This subroutine requires the following subroutines:
C       SITECV_F   computes top height from site and age
C       SITEF_C    computes model parameters
C       SITEF_SI   calculates an approximate psi for a given site
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      REAL*4 AGE,SI,HT40,HTOP,XSI
      REAL*4 ASPEC,AEQN,ATYPE,BAGE,MINA,MAXA,MINSI,MAXSI,REGION
C
      COMMON /INFO/ASPEC,AEQN,ATYPE,BAGE,MINA,MAXA,MINSI,MAXSI,REGION
C
        XSI = SI*0.3048
        CALL SITECV_F_EDIT(XSI,AGE,HTOP)
        HT40 = HTOP*3.2808
      RETURN
      END
C
C**********************************************************************
C      SUBROUTINE NCH40(H40,AGE,SI)
CC
CC     NIGH AND COURTIN (1998) RED ALDER
CC
C      IMPLICIT NONE
C      REAL*4 SI,AGE,H40,MSI,MH40
C      MSI=SI/3.28
C      MH40=1.3+((1.693*(MSI-1.3))/(1.0+EXP(3.600-1.240*ALOG(AGE-0.5))))
C      H40=3.28*MH40
C      RETURN
C      END
C*******************************************************************************
      SUBROUTINE WHHLB_SI_UC_EDIT(SI_C,PDEN,SI_UC)
C
C     UNCORRECTS THE DENSITY INPACT UPON THE WEISKITTEL, HANN, HIBBS, LAM, AND BLUHN
C          SITE INDEX FOR RED ALDER
C
      IMPLICIT NONE
      REAL*4 SI_C,PDEN,SI_UC
C
C     SITE INDEX UNCORRECTED FOR DENSITY EFFECT
C
      SI_UC=SI_C*(1.0-0.36979789*EXP(-0.00042264*PDEN**1.5))
      RETURN
      END
C***********************************************************************
      SUBROUTINE WHHLB_H40_EDIT(H40M,TAGEM,TAGEP,PH40P)
C
C     WEISKITTEL, HANN, HIBBS, LAM, AND BLUHM DOMINANT HEIGHT GROWTH EQUATION FOR
C     RED ALDER
C
      IMPLICIT NONE
      REAL*4 H40M,TAGEM,TAGEP,PH40P
      REAL*4 B1,B2
      B1=-4.481266
      B2=-0.658884
      PH40P=H40M*EXP(B1*(TAGEP**B2-TAGEM**B2))
      RETURN
      END
C**********************************************************************
      SUBROUTINE SITECV_F_EDIT(si,age,htop)
C
C----------------------------------------------------------------------
C     Purpose:  Implements new height-increment methods (F)
C
C     Current Date: FEB 2, 1994    J. FLEWELLING
C
C     SI    IN     R*4    Site index (m) (basis is BH AGE 50)
C     AGE   IN     R*4    Breast height age (.= 1.0)
C     HTOP  OUT    R*4    Top height (== site height) (m)
C----------------------------------------------------------------------
C
      REAL*4 SI,AGE,HTOP,OLD_SI,SI_2,yk,alpha,beta,c,b1,xk,psi,x,z
      INTEGER*4 h1
C
      SAVE OLD_SI

      common /sitefprm/xk, b1, c , beta,alpha, h1, yk, SI_2
c
c                           determine if coefficients for this SI are
c                          already in siteprm. If not, get them.

      if (SI.ne. old_si) then
             old_si = si
             call sitef_si_EDIT( si, psi)
             call sitef_C_EDIT ( psi)
           end if

c                                         apply height-age equation.
      x=age-1
      if (x.lt.xk) then
             htop  = h1 + SI_2*x +
     1            (1-b1)*SI_2*xk/(c+1) * (((xk-x)/xk)**(c+1)-1)
          else
             z = x -xk
             htop = yk
     1            +alpha*(1-exp(-beta*z))
          end if
      return
      end
C
C**********************************************************************
      SUBROUTINE SITEF_C_EDIT(psi)
C
C----------------------------------------------------------------------
C     Purpose:  For a specified psi, calculates all of the height-age
C               model parameters, and stores them in /sitefprm/
C
C     Current Date: FEB 2, 1994    J. FLEWELLING
C
C     psi     input   REAL     productivity index (m/yr)
C----------------------------------------------------------------------
C
      REAL*4 psi,fp,SI_2,yk,alpha,beta,c,b1,xk
      INTEGER*4 i,h1
      dimension fp(10)
      common /sitefprm/xk, b1, c ,  beta,alpha, h1, yk, SI_2
c                                                   R24 Coefficients
      data (fp(i),i=1,5),fp(9)  /    128.326    ,    -2.54871 ,
     &    5.33208, -9.00622 , 1.2, 52.7948 /

      SI_2=psi
      xk = fp(1) * exp( fp(2) * psi)
      b1 = 0.2 + 0.8/ ( 1 + exp(fp(3) + fp(4)*psi))
      c  =1.0 + fp(5)* psi
      alpha=fp(9)*psi
      h1 = INT(1.3 + (b1*psi)/2.)
      yk = h1 + psi * xk* ( 1.0 - (1.-b1)/(c+1))

      beta=psi/alpha

      RETURN
       end
C
C**********************************************************************
      SUBROUTINE SITEF_SI_EDIT( SI, PSI)
C
C----------------------------------------------------------------------
C     Purpose:  Calculates an approximate psi for a given site index
C               Ref 'Model Fitting: top height increment', Feb 2, 1994.
C
C     Current Date: FEB 2, 1994    J. FLEWELLING and A. ZUMRAWI
C
C          si      input  r*4    site index (top height at BH age 50)
C          psi     output r*4    site productivity parameter.
C----------------------------------------------------------------------
C
       REAL*4 SI,PSI,b,si_piv,x
       INTEGER*4 i,j
       dimension b(6,2)
       data si_piv  / 32.25953/
       data (b(i,1),i=1,6)/ .299720, .116875, .074866, .032348,
     1                      .006984, .000339 /
       data (b(i,2),i=1,6)/ .290737, .129665, -.058777,
     1                      -.000669, .006003, -.001060 /

        if (si.le. si_piv) then
            J=1
        else
            J=2
        end if

        x = (si - si_piv)/10.0

        psi = 0.75 + x*( b(1,j) + x*( b(2,j)  + x*( b(3,j)
     &             + x*( b(4,j) + x*( b(5,j)  + x*b(6,j)   )))))
        return
        end
C
C**********************************************************************
      SUBROUTINE DFORTY(VERSION,NTREES,NPTS,IB,TDATAI,TDATAR,D40)
C     DETERMINE DIAMETER OF THE FORTY LARGEST BIG-6 TREES PER ACRE
C
C
      IMPLICIT NONE
      INTEGER*4 VERSION,NTREES,NPTS,IB,TDATAI(2000,3),I,ID,IIB
      REAL*4    TDATAR(2000,4),TOTD,TOTTR,DCL(1000),TRCL(1000),D40,
     1          TRDIFF,EXPAN
C
      TOTD=0.0
      TOTTR=0.0
      DO I=1,1000
         DCL(I)=0.0
         TRCL(I)=0.0
      ENDDO
      IIB=IB
      IF(VERSION .GE. 4) IIB=1
      DO I=1,NTREES
         IF(TDATAR(I,1) .LE. 0.0) CYCLE
         IF(TDATAI(I,2) .LE. IIB) THEN
            ID=IFIX(TDATAR(I,1)*10.0)
            IF(ID.GT.1000) ID=1000
            EXPAN=TDATAR(I,4)/FLOAT(NPTS)
            DCL(ID)=DCL(ID)+TDATAR(I,1)*EXPAN
            TRCL(ID)=TRCL(ID)+EXPAN
         ENDIF
      ENDDO
      DO I=1000,1,-1
         TOTD=TOTD+DCL(I)
         TOTTR=TOTTR+TRCL(I)
         IF(TOTTR.GT.40.0) THEN
            TRDIFF=TRCL(I)-(TOTTR-40.0)
            TOTD=TOTD-DCL(I)+((DCL(I)/TRCL(I))*TRDIFF)
            TOTTR=40.0
            EXIT
         ENDIF
      ENDDO
      IF(TOTTR.GT.0.0) THEN
         D40=TOTD/TOTTR
      ELSE
         D40=0.0
      ENDIF
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE SPMIX(NTREES,NPTS,TDATAI,TDATAR,PDF,PTF,PPP,PWH,PRA)
      IMPLICIT NONE
      INTEGER*4  NTREES,NPTS,TDATAI(2000,3),I
      REAL*4     TDATAR(2000,4),BA,BATOT,BADF,BATF,BAPP,BAWH,BARA,PDF,
     1           PTF,PPP,PWH,PRA
      BATOT=0.0
      BADF=0.0
      BATF=0.0
      BAPP=0.0
      BAWH=0.0
      BARA=0.0
      PDF=0.0
      PTF=0.0
      PPP=0.0
      PWH=0.0
      PRA=0.0
      DO I=1,NTREES
         BA=(TDATAR(I,1)**2*TDATAR(I,4))*0.005454154/NPTS
         BATOT=BATOT+BA
         IF(TDATAI(I,1) .EQ. 15 .OR. TDATAI(I,1) .EQ. 17) THEN
            BATF=BATF+BA
         ELSEIF(TDATAI(I,1) .EQ. 122) THEN
            BAPP=BAPP+BA
         ELSEIF(TDATAI(I,1) .EQ. 202) THEN
            BADF=BADF+BA
         ELSEIF(TDATAI(I,1) .EQ. 263) THEN
            BAWH=BAWH+BA
         ELSEIF(TDATAI(I,1) .EQ. 351) THEN
            BARA=BARA+BA
         ENDIF
      ENDDO
      IF(BATOT .GT. 0.0) THEN
         PDF=BADF/BATOT
         PTF=BATF/BATOT
         PPP=BAPP/BATOT
         PWH=BAWH/BATOT
         PRA=BARA/BATOT
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE HD40_SWO(IEQ,HT40,D40,DBH,PTRHT)
      IMPLICIT NONE
      INTEGER*4  IEQ
      REAL*4     HT40,D40,DBH,PTRHT,B0,B1,B2,EXD,EXD40,HD40PAR(3,3)
C
C    HEIGHT/DIAMETER USING D40 (3PARameters - Douglas-fir, true firs,
C                               ponderosa pine)
C     DF Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     GW Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     PP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
CC
      DATA HD40PAR/
     1             -3.485635287, -4.376160718, -4.047994965,           !  DF,GW,PP
     2             -0.255712209, -0.231693907, -0.135864020,           !  DF,GW,PP
     3             -0.001555149, -0.001334070, -0.005647510/           !  DF,GW,PP
C
      B0=HD40PAR(IEQ,1)
      B1=HD40PAR(IEQ,2)
      B2=HD40PAR(IEQ,3)
      EXD=EXP(B0*DBH**(B1+B2*(HT40-4.5)))
      EXD40=EXP(B0*D40**(B1+B2*(HT40-4.5)))
      PTRHT=4.5+(HT40-4.5)*(EXD/EXD40)
      RETURN
      END
C**********************************************************************
      SUBROUTINE HD40_NWO(IEQ,HT40,D40,DBH,PTRHT)
      IMPLICIT NONE
      INTEGER*4  IEQ
      REAL*4     HT40,D40,DBH,PTRHT,B0,B1,B2,EXD,EXD40,HD40PAR(2,3)
C
C    HEIGHT/DIAMETER USING D40 (3PARameters - Douglas-fir and Western Hemlock)
C
C     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C
      DATA HD40PAR/
     1             -2.857232223, -2.790360488,                         !  DF,WH
     2             -0.393885195, -0.235470605,                         !  DF,WH
     3             -0.000521583, -0.002374673/                         !  DF,WH
C
      B0=HD40PAR(IEQ,1)
      B1=HD40PAR(IEQ,2)
      B2=HD40PAR(IEQ,3)
      EXD=EXP(B0*DBH**(B1+B2*(HT40-4.5)))
      EXD40=EXP(B0*D40**(B1+B2*(HT40-4.5)))
      PTRHT=4.5+(HT40-4.5)*(EXD/EXD40)
      RETURN
      END
C**********************************************************************
      SUBROUTINE HD40_SMC(IEQ,HT40,D40,DBH,PTRHT)
      IMPLICIT NONE
      INTEGER*4  IEQ
      REAL*4     HT40,D40,DBH,PTRHT,B0,B1,B2,EXD,EXD40,HD40PAR(2,3)

C
C    HEIGHT/DIAMETER USING D40 (3PARameters - Douglas-fir and western hemlock)
C
C     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C
      DATA HD40PAR/
     1             -2.857232223, -2.790360488,                         !  DF,WH
     2             -0.393885195, -0.235470605,                         !  DF,WH
     3             -0.000521583, -0.002374673/                         !  DF,WH
C
      B0=HD40PAR(IEQ,1)
      B1=HD40PAR(IEQ,2)
      B2=HD40PAR(IEQ,3)
      EXD=EXP(B0*DBH**(B1+B2*(HT40-4.5)))
      EXD40=EXP(B0*D40**(B1+B2*(HT40-4.5)))
      PTRHT=4.5+(HT40-4.5)*(EXD/EXD40)
      RETURN
      END
*******************************************************************************
      SUBROUTINE HD40_RAP(IEQ,HT40,D40,DBH,PTRHT)
      IMPLICIT NONE
      INTEGER*4  IEQ
      REAL*4     HT40,D40,DBH,PTRHT,B0,B1,B2,EXD,EXD40,HD40PAR(3,3)

C
C     HEIGHT/DIAMETER USING D40 (3PARameters - Red Alder, Douglas-fir and
C     western hemlock)
C
C     RA Coefficients from Hann, Bluhm, and Hibbs (2011) Forest Biometrics Research Paper 1
C     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C
      DATA HD40PAR/
     1             -1.7477875  , -2.857232223, -2.790360488,        !  RA,DF,WH
     2             -0.40004105 , -0.393885195, -0.235470605,        !  RA,DF,WH
     3             -0.00497111 , -0.000521583, -0.002374673/        !  RA,DF,WH
C
      B0=HD40PAR(IEQ,1)
      B1=HD40PAR(IEQ,2)
      B2=HD40PAR(IEQ,3)
      EXD=EXP(B0*DBH**(B1+B2*(HT40-4.5)))
      EXD40=EXP(B0*D40**(B1+B2*(HT40-4.5)))
      PTRHT=4.5+(HT40-4.5)*(EXD/EXD40)
      RETURN
      END
*******************************************************************************
      SUBROUTINE GET_CCFL_EDIT(DBH,CCFLL1,CCFL1,CCFL)
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
**********************************************************************
      SUBROUTINE CALTST(YXS,XSS,YSS,N,BETA)
C
C     COMPUTE CALIBRATION BETA AND DETERMINE IF IT IS SIGNIFICANT FROM
C     ONE
C
      IMPLICIT NONE
      INTEGER*4 N,IDEGF
      REAL*4 BETA,VARBETA,DEGF,MSE,TTEST,YXS,XSS,YSS,TVAL(34),CRITVAL
      DATA TVAL/63.657, 9.925, 5.841, 4.604, 4.032, 3.707, 3.499, 3.355,
     1           3.250, 3.169, 3.106, 3.055, 3.012, 2.977, 2.947, 2.921,
     2           2.898, 2.878, 2.861, 2.845, 2.831, 2.819, 2.807, 2.797,
     3           2.787, 2.779, 2.771, 2.763, 2.756, 2.750, 2.704, 2.660,
     4           2.617, 2.576/
      BETA=YXS/XSS
      IDEGF=N-1
      DEGF=FLOAT(IDEGF)
      MSE=(YSS-2.0*BETA*YXS+BETA**2*XSS)/DEGF
      VARBETA=MSE/XSS
      IF(VARBETA.GT.0.0) THEN
         TTEST=ABS(BETA-1.0)/SQRT(VARBETA)
         IF(IDEGF.LE.30) THEN
            CRITVAL=TVAL(IDEGF)
         ELSE IF(IDEGF.LE.40) THEN
            CRITVAL=TVAL(30)+(TVAL(31)-TVAL(30))*((DEGF-30.0)/10.0)
         ELSE IF(IDEGF.LE.60) THEN
            CRITVAL=TVAL(31)+(TVAL(32)-TVAL(31))*((DEGF-40.0)/20.0)
         ELSE IF(IDEGF.LE.120) THEN
            CRITVAL=TVAL(32)+(TVAL(33)-TVAL(32))*((DEGF-60.0)/60.0)
         ELSE IF(IDEGF.LE.1000) THEN
            CRITVAL=TVAL(33)+(TVAL(34)-TVAL(33))*((DEGF-120.0)/880.0)
         ELSE
            CRITVAL=TVAL(34)
         ENDIF
         IF(TTEST.LT.CRITVAL) THEN
            BETA=1.0
         ELSE
            IF(BETA .GT. 2.0)THEN
               BETA=2.0
            ELSE IF(BETA .LT. 0.5)THEN
               BETA=0.5
            ENDIF
         ENDIF
      ELSE
         IF(BETA .GT. 2.0)THEN
            BETA=2.0
         ELSE IF(BETA .LT. 0.5)THEN
            BETA=0.5
         ENDIF
      ENDIF
      RETURN
      END
