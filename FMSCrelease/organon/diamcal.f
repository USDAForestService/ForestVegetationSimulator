C----------
C ORGANON $Id$
C----------
C      ORGANON GROWTH AND YIELD MODEL
C     SUBROUTINES INCLUDED:
C        DGCALIB
C        DIAMGRO
C        PHTS
C        MORTAL
C        DIB_SWO
C        DIB_NWO
C        DIB_SMC
C        DIB_RAP
C        DOB_SWO
C        DOB_NWO
C        DOB_SMC
C        DOB_RAP
C        SSUM
C        OLDGROWTH
C        GET_BAL
C
C  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
C               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
C               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
C               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.
C
C  CHANGED THE NAME OF SUBROUTINE CALL - DG_NWO DG_NWO_RUN, DG_NWO_RUN IS
C                                        IN THE DIAGRO ROUTINE, DG_NWO WAS IN
C                                        EDITDLL\COMFILES.FOR

********************************************************************************
      SUBROUTINE DGCALIB(VERSION,IB,NSPN,NPTS,NTREES,RADIN,TDATAI,
     1                   RAD,BAL2,BALL2,OG,TDATAR,GROWTH,SI_1,SI_2,
     2                   RADGRO,SBA2,STDATAR,PDG,CALIB)
C
C     CALCULATION OF DIAMETER GROWTH CALIBRATION
C
      IMPLICIT NONE
      INTEGER*4    IB,NSPN,NTREES,NPTS,RADIN(18),TDATAI(2000,3),
     1             VERSION
C
      REAL*4       BAL2(500),BALL2(51),CALIB(3,18),SI_1,GROWTH(2000),
     1             OG,PDG(2000),SI_2,RADGRO(2000),SBA2,
     2             STDATAR(2000,4),TDATAR(2000,4)
      REAL*4    AVRAT,BETA,DG,DBH1,DBH2,DIB(2000),DIBCK,PM,RATIO(18),
     1          SDG,SPDG,SUM1,SUM2,SUM3,SUM4,YSS,POW(2000),XPM,PS
      REAL*4    XSI_1,XSI_2,PMK(2000),BAL1(500),BALL1(51),CCFL1(500),
     1          CCFLL1(51),SBA1
      INTEGER*4 I,J,ISPGRP
      LOGICAL*2 RAD
      AVRAT=0.
      SUM3=0.0
      SUM4=0.0
C
C     PRESET ALL DIAMETER CALIBRATIONS TO 1.0
C
      DO I=1,18
         CALIB(3,I)=1.0
      ENDDO
      XSI_1=SI_1-4.5
      XSI_2=SI_2-4.5
C
C     IF NO RADIAL GROWTHS -- SKIP CALIB
      IF(.NOT. RAD) RETURN
C
C     PREDICT PREVIOUS 5-YR DIAMETER
C
      DO I=1,NTREES
         CALL DIAMGRO(VERSION,I,TDATAI,TDATAR,XSI_1,XSI_2,SBA2,
     1                BALL2,BAL2,PDG)
      ENDDO
      DO I=1,18
         RATIO(I)=1.0
         IF(RADIN(I).LE.0) CYCLE
         SUM1=0.0
         SUM2=0.0
         DO J=1,NTREES
            IF(TDATAI(J,2) .NE. I) CYCLE
            DBH2=TDATAR(J,1)
            ISPGRP=TDATAI(J,2)
            SELECT CASE(VERSION)
               CASE(1)
                  CALL DIB_SWO(ISPGRP,DBH2,DIB(J))
               CASE(2)
                  CALL DIB_NWO(ISPGRP,DBH2,DIB(J))
               CASE(3)
                  CALL DIB_SMC(ISPGRP,DBH2,DIB(J))
               CASE(4)
                  CALL DIB_RAP(ISPGRP,DBH2,DIB(J))
            END SELECT
            IF(RADGRO(J) .GT. 0.)THEN
               DIBCK=DIB(J)-2.0*RADGRO(J)
               IF(DIBCK.GT.0.0) THEN
                  SELECT CASE(VERSION)
                     CASE(1)
                        CALL DOB_SWO(ISPGRP,DIBCK,DBH1)
                     CASE(2)
                        CALL DOB_NWO(ISPGRP,DIBCK,DBH1)
                     CASE(3)
                        CALL DOB_SMC(ISPGRP,DIBCK,DBH1)
                     CASE(4)
                        CALL DOB_RAP(ISPGRP,DIBCK,DBH1)
                  END SELECT
                  IF(DBH1.LT.0.1) DBH1=0.1
               ELSE
                  DBH1=0.1
               ENDIF
               DG=DBH2-DBH1
               SUM1=SUM1+DG
               SUM2=SUM2+PDG(J)
               STDATAR(J,1)=DBH2-DG
               IF(STDATAR(J,1) .LT. 0.1)STDATAR(J,1)=0.1
               GROWTH(J)=DG
            ENDIF
         ENDDO
         RATIO(I)=SUM1/SUM2
         SUM3=SUM3+SUM1
         SUM4=SUM4+SUM2
      ENDDO
      AVRAT=SUM3/SUM4
      DO I=1,18
         IF(RADIN(I) .LE. 5)THEN
            RATIO(I)=AVRAT
         ENDIF
      ENDDO
      DO J=1,NTREES
         I=TDATAI(J,2)
         IF(RADGRO(J) .LE. 0.0)THEN
            STDATAR(J,1)=TDATAR(J,1)-RATIO(I)*PDG(J)
            IF(STDATAR(J,1) .LT. 0.1)STDATAR(J,1)=0.1
            GROWTH(J)=TDATAR(J,1)-STDATAR(J,1)
         ENDIF
      ENDDO
C
C     PREDICT PREVIOUS 5-YR HEIGHTS
C
      CALL PHTS(VERSION,NTREES,TDATAI,TDATAR,STDATAR,CALIB)
C
C        PREDICT PREVIOUS 5-YR CROWN RATIO
C
      DO I=1,NTREES
         STDATAR(I,3)=(TDATAR(I,3)*TDATAR(I,2))/STDATAR(I,2)
         IF(STDATAR(I,3) .GT. 1.0) STDATAR(I,3)=1.0
      ENDDO
C
C     CALCULATE PREVIOUS 5-YR STAND AND TREE STATISTICS
C
C
C     PREDICT PREVIOUS 5-YR MORTALITY AND EXPANSION FACTOR
C
      CALL MORTAL(VERSION,NPTS,NTREES,IB,TDATAI,TDATAR,BALL2,BAL2,
     1            OG,XSI_1,XSI_2,POW,PMK)
      DO I=1,NTREES
         XPM=1.0/(1.0+EXP(-PMK(I)))
         PS=(1.0-XPM)**POW(I)
         PM=1.0-PS
         IF(PM .GT. 0.5)PM=0.5
         STDATAR(I,4)=TDATAR(I,4)/(1.0-PM)
      ENDDO
C
C     GROW TREES WITH RADIAL GROWTH INPUT GIVING PREDICTED & ACTUAL
C     TO DETERMINE CALIBRATION
C
      CALL SSUM(1,VERSION,NPTS,NTREES,TDATAI,TDATAR,STDATAR,SBA1,
     1          BAL1,BALL1,CCFL1,CCFLL1)
      DO I=1,NSPN
         SDG=0.0
         SPDG=0.0
         YSS=0.0
         DO J=1,NTREES
            IF(TDATAI(J,2) .NE. I .OR. RADGRO(J) .LE. 0.0) CYCLE
            CALL DIAMGRO(VERSION,J,TDATAI,STDATAR,XSI_1,XSI_2,
     1                   SBA1,BALL1,BAL1,PDG)
            SDG=SDG+GROWTH(J)
            SPDG=SPDG+PDG(J)
            YSS=YSS+(GROWTH(J)**2/PDG(J))
         ENDDO
         IF(RADIN(I) .LT. 2) CYCLE
         CALL CALTST(SDG,SPDG,YSS,RADIN(I),BETA)
         CALIB(3,I)=BETA
      ENDDO
      RETURN
      END
**********************************************************************
      SUBROUTINE DIAMGRO(VERSION,K,TDATAI,XTDATAR,SI_1,SI_2,SBA,
     1                   BALL,BAL,PDG)
      IMPLICIT NONE
C
C     CALCULATES FIVE-YEAR DIAMETER GROWTH RATE OF THE K-TH TREE
C
      INTEGER*4 VERSION,K,TDATAI(2000,3),ISP,ISPGRP
      REAL*4 XTDATAR(2000,4),SI_1,SI_2,SBA,BALL(51),BAL(500),
     1       PDG(2000),DBH,CR,DG,SBAL,SITE
C
C     CALCULATE BASAL AREA IN LARGER TREES
C
      DBH=XTDATAR(K,1)
      CR=XTDATAR(K,3)
      ISP=TDATAI(K,1)
      ISPGRP=TDATAI(K,2)
      CALL GET_BAL(DBH,BALL,BAL,SBAL)
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
            IF(ISP .EQ. 351) THEN
               SITE=SI_1
            ELSE
               SITE=SI_2
            ENDIF
      END SELECT
C
C     CALCULATE DIAMETER GROWTH RATE FOR UNTREATED TREES
C
      SELECT CASE(VERSION)
         CASE(1)
            CALL DG_SWO(ISPGRP,DBH,CR,SITE,SBAL,SBA,DG)
         CASE(2)
            CALL DG_NWO_RUN(ISPGRP,DBH,CR,SITE,SBAL,SBA,DG)
         CASE(3)
            CALL DG_SMC(ISPGRP,DBH,CR,SITE,SBAL,SBA,DG)
         CASE(4)
            CALL DG_RAP(ISPGRP,DBH,CR,SITE,SBAL,SBA,DG)
      END SELECT
      PDG(K)=DG
      RETURN
      END
C**********************************************************************
      SUBROUTINE PHTS(VERSION,NTREES,TDATAI,TDATAR,STDATAR,CALIB)
      IMPLICIT NONE
      INTEGER*4 VERSION,NTREES,TDATAI(2000,3),I,ISPGRP
      REAL*4 TDATAR(2000,4),STDATAR(2000,4),CALIB(3,18),PRSH,PRH,
     1       CPRSH,CPRH
      DO I=1,NTREES
         ISPGRP=TDATAI(I,2)
         SELECT CASE(VERSION)
            CASE(1)
                CALL A_HD_SWO(ISPGRP,STDATAR(I,1),PRSH)
                CALL A_HD_SWO(ISPGRP,TDATAR(I,1),PRH)
            CASE(2)
                CALL HD_NWO(ISPGRP,STDATAR(I,1),PRSH)
                CALL HD_NWO(ISPGRP,TDATAR(I,1),PRH)
            CASE(3)
                CALL HD_SMC(ISPGRP,STDATAR(I,1),PRSH)
                CALL HD_SMC(ISPGRP,TDATAR(I,1),PRH)
            CASE(4)
                CALL A_HD_RAP(ISPGRP,STDATAR(I,1),PRSH)
                CALL A_HD_RAP(ISPGRP,TDATAR(I,1),PRH)
         ENDSELECT
         CPRSH=4.5+CALIB(1,ISPGRP)*(PRSH-4.5)
         CPRH=4.5+CALIB(1,ISPGRP)*(PRH-4.5)
         STDATAR(I,2)=(CPRSH/CPRH)*TDATAR(I,2)
         IF(STDATAR(I,2) .LT. 4.5)STDATAR(I,2)=4.5
      ENDDO
      RETURN
      END
C**********************************************************************
      SUBROUTINE MORTAL(VERSION,NPTS,NTREES,IB,TDATAI,TDATAR,BALL,
     1                  BAL,OG,SI_1,SI_2,POW,PMK)
C     ROUTINE FOR SETTING TREE MORTALITY
C**********************************************************************
C
      IMPLICIT NONE
      INTEGER*4 VERSION,NPTS,NTREES,IB,TDATAI(2000,3),I,ISPGRP
      REAL*4    TDATAR(2000,4),BALL(51),OG,
     1          BAL(500),SI_1,SI_2,PMK(2000),DBH,HT,SBAL,
     2          CR,POW(2000)
      INTEGER IDANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = IB
      IDANUW = NPTS
C
      DO I=1,NTREES
         PMK(I)=0.0
      ENDDO
C
C  INDIVIDUAL TREE MORTALITY EQUATIONS
C
      DO I=1,NTREES
         IF(TDATAR(I,4) .LE. 0.) CYCLE
         ISPGRP=TDATAI(I,2)
         DBH=TDATAR(I,1)
         HT=TDATAR(I,2)
         CR=TDATAR(I,3)
         CALL GET_BAL(DBH,BALL,BAL,SBAL)
         SELECT CASE(VERSION)
           CASE(1)
              CALL PM_SWO(ISPGRP,DBH,CR,SI_1,SBAL,OG,POW(I),PMK(I))
           CASE(2)
              CALL PM_NWO(ISPGRP,DBH,CR,SI_1,SI_2,SBAL,POW(I),PMK(I))
           CASE(3)
              CALL PM_SMC(ISPGRP,DBH,CR,SI_1,SI_2,SBAL,POW(I),PMK(I))
           CASE(4)
              CALL PM_RAP(ISPGRP,DBH,CR,SI_1,SI_2,SBAL,POW(I),PMK(I))
         ENDSELECT
      ENDDO
      RETURN
      END
*******************************************************************************
      SUBROUTINE DIB_SWO(ISPGRP,DOB,DIB)
      IMPLICIT NONE
      REAL*4 DOB,DIBPAR(18,2),B0,B1,DIB
      INTEGER*4 ISPGRP
C
C  DIAMETER INSIDE BARK (2 parameters - all species)
C
      DATA DIBPAR/
     1       0.903563 , 0.904973 , 0.809427 , 0.859045 , 0.837291 ,    !  DF,GW,PP,SP,IC,
     1       0.933707 , 0.9497   , 0.97     , 0.96317  , 0.94448  ,    !  WH,RC,PY,MD,GC,
     1       0.859151 , 0.910499 , 0.97059  , 0.878457 , 0.889703 ,    !  TA,CL,BL,WO,BO,
     1       0.947    , 0.94448  , 0.94448  ,                          !  RA,PD,WI
C
     2       0.989388 , 1.0      , 1.016866 , 1.0      , 1.0      ,    !  DF,GW,PP,SP,IC,
     2       1.0      , 1.0      , 1.0      , 1.0      , 0.9875170,    !  WH,RC,PY,MD,GC,
     2       1.0178109, 1.01475  , 0.993585 , 1.02393  , 1.0104062,    !  TA,CL,BL,WO,BO,
     2       1.0      , 0.9875170, 0.9875170/                          !  RA,PD,WI
C
      B0=DIBPAR(ISPGRP,1)
      B1=DIBPAR(ISPGRP,2)
      DIB=B0*DOB**B1
      RETURN
      END
**********************************************************************
      SUBROUTINE DIB_NWO(ISPGRP,DOB,DIB)
      IMPLICIT NONE
      REAL*4 DOB,DIBPAR(11,2),B0,B1,DIB
      INTEGER*4 ISPGRP
C                              **********
C  DIAMETER INSIDE BARK (2 parameters - all species)
C
      DATA DIBPAR/
     1            0.971330, 0.904973, 0.933707, 0.9497  , 0.97    ,    !  DF,GF,WH,RC,PY
     1            0.96317 , 0.97059 , 0.878457, 0.947   , 0.94448 ,    !  MD,BL,WO,RA,PD
     1            0.94448 ,                                            !  WI
C
     2            0.966365, 1.0     , 1.0     , 1.0     , 1.0     ,    !  DF,GF,WH,RC,PY
     2            1.0     , 0.993585, 1.02393 , 1.0     , 0.987517,    !  MD,BL,WO,RA,PD
     2            0.987517/                                            !  WI
C
      B0=DIBPAR(ISPGRP,1)
      B1=DIBPAR(ISPGRP,2)
      DIB=B0*DOB**B1
      RETURN
      END

**********************************************************************
      SUBROUTINE DIB_SMC(ISPGRP,DOB,DIB)
      IMPLICIT NONE
      REAL*4 DOB,DIBPAR(11,2),B0,B1,DIB
      INTEGER*4 ISPGRP
C
C  DIAMETER INSIDE BARK (2 parameters - all species)
C
      DATA DIBPAR/
     1            0.971330, 0.904973, 0.933707, 0.9497  , 0.97    ,    !  DF,GF,WH,RC,PY
     1            0.96317 , 0.97059 , 0.878457, 0.947   , 0.94448 ,    !  MD,BL,WO,RA,PD
     1            0.94448 ,                                            !  WI
C
     2            0.966365, 1.0     , 1.0     , 1.0     , 1.0     ,    !  DF,GF,WH,RC,PY
     2            1.0     , 0.993585, 1.02393 , 1.0     , 0.987517,    !  MD,BL,WO,RA,PD
     2            0.987517/                                            !  WI
C
      B0=DIBPAR(ISPGRP,1)
      B1=DIBPAR(ISPGRP,2)
      DIB=B0*DOB**B1
      RETURN
      END
*******************************************************************************
      SUBROUTINE DIB_RAP(ISPGRP,DOB,DIB)
      IMPLICIT NONE
      REAL*4 DOB,DIBPAR(7,2),B0,B1,DIB
      INTEGER*4 ISPGRP
C
C  DIAMETER INSIDE BARK (2 parameters - all species)
C
      DATA DIBPAR/
     1            0.947   , 0.971330, 0.933707, 0.9497  , 0.97059 ,    !  RA,DF,WH,RC,BL
     1            0.94448 , 0.94448 ,                                  !  PD,WI
C
     2            1.0     , 0.966365, 1.0     , 1.0     , 0.993585,    !  RA,DF,WH,RC,BL
     2            0.987517, 0.987517/                                  !  PD,WI
C
      B0=DIBPAR(ISPGRP,1)
      B1=DIBPAR(ISPGRP,2)
      DIB=B0*DOB**B1
      RETURN
      END
*******************************************************************************
      SUBROUTINE DOB_SWO(ISPGRP,DIB,DOB)
      IMPLICIT NONE
      REAL*4 DIB,DIBPAR(18,2),B0,B1,DOB
      INTEGER*4 ISPGRP
C
C  DIAMETER INSIDE BARK (2 parameters - all species)
C
      DATA DIBPAR/
     1       0.903563 , 0.904973 , 0.809427 , 0.859045 , 0.837291 ,    !  DF,GW,PP,SP,IC,
     1       0.933707 , 0.9497   , 0.97     , 0.96317  , 0.94448  ,    !  WH,RC,PY,MD,GC,
     1       0.859151 , 0.910499 , 0.97059  , 0.878457 , 0.889703 ,    !  TA,CL,BL,WO,BO,
     1       0.947    , 0.94448  , 0.94448  ,                          !  RA,PD,WI
C
     2       0.989388 , 1.0      , 1.016866 , 1.0      , 1.0      ,    !  DF,GW,PP,SP,IC,
     2       1.0      , 1.0      , 1.0      , 1.0      , 0.9875170,    !  WH,RC,PY,MD,GC,
     2       1.0178109, 1.01475  , 0.993585 , 1.02393  , 1.0104062,    !  TA,CL,BL,WO,BO,
     2       1.0      , 0.9875170, 0.9875170/                          !  RA,PD,WI
C
      B0=DIBPAR(ISPGRP,1)
      B1=DIBPAR(ISPGRP,2)
      DOB=(DIB/B0)**(1.0/B1)
      RETURN
      END
**********************************************************************
      SUBROUTINE DOB_NWO(ISPGRP,DIB,DOB)
      IMPLICIT NONE
      REAL*4 DIB,DIBPAR(11,2),B0,B1,DOB
      INTEGER*4 ISPGRP
C                              **********
C  DIAMETER INSIDE BARK (2 parameters - all species)
C
      DATA DIBPAR/
     1            0.971330, 0.904973, 0.933707, 0.9497  , 0.97    ,    !  DF,GF,WH,RC,PY
     1            0.96317 , 0.97059 , 0.878457, 0.947   , 0.94448 ,    !  MD,BL,WO,RA,PD
     1            0.94448 ,                                            !  WI
C
     2            0.966365, 1.0     , 1.0     , 1.0     , 1.0     ,    !  DF,GF,WH,RC,PY
     2            1.0     , 0.993585, 1.02393 , 1.0     , 0.987517,    !  MD,BL,WO,RA,PD
     2            0.987517/                                            !  WI
C
      B0=DIBPAR(ISPGRP,1)
      B1=DIBPAR(ISPGRP,2)
      DOB=(DIB/B0)**(1.0/B1)
      RETURN
      END

**********************************************************************
      SUBROUTINE DOB_SMC(ISPGRP,DIB,DOB)
      IMPLICIT NONE
      REAL*4 DIB,DIBPAR(11,2),B0,B1,DOB
      INTEGER*4 ISPGRP
C
C  DIAMETER INSIDE BARK (2 parameters - all species)
C
      DATA DIBPAR/
     1            0.971330, 0.904973, 0.933707, 0.9497  , 0.97    ,    !  DF,GF,WH,RC,PY
     1            0.96317 , 0.97059 , 0.878457, 0.947   , 0.94448 ,    !  MD,BL,WO,RA,PD
     1            0.94448 ,                                            !  WI
C
     2            0.966365, 1.0     , 1.0     , 1.0     , 1.0     ,    !  DF,GF,WH,RC,PY
     2            1.0     , 0.993585, 1.02393 , 1.0     , 0.987517,    !  MD,BL,WO,RA,PD
     2            0.987517/                                            !  WI
C
      B0=DIBPAR(ISPGRP,1)
      B1=DIBPAR(ISPGRP,2)
      DOB=(DIB/B0)**(1.0/B1)
      RETURN
      END
**********************************************************************
      SUBROUTINE DOB_RAP(ISPGRP,DIB,DOB)
      IMPLICIT NONE
      REAL*4 DIB,DIBPAR(7,2),B0,B1,DOB
      INTEGER*4 ISPGRP
C
C  DIAMETER INSIDE BARK (2 parameters - all species)
C
      DATA DIBPAR/
     1            0.947   , 0.971330, 0.933707, 0.9497  , 0.97059 ,    !  RA,DF,WH,RC,BL
     1            0.94448 , 0.94448 ,                                  !  PD,WI
C
     2            1.0     , 0.966365, 1.0     , 1.0     , 0.993585,    !  DF,DF,WH,RC,BL
     2            0.987517, 0.987517/                                  !  PD,WI
C
      B0=DIBPAR(ISPGRP,1)
      B1=DIBPAR(ISPGRP,2)
      DOB=(DIB/B0)**(1.0/B1)
      RETURN
      END
**********************************************************************
      SUBROUTINE SSUM(N,VERSION,NPTS,NTREES,TDATAI,TDATAR,STDATAR,
     1                SBA,BAL,BALL,CCFL,CCFLL)
      IMPLICIT NONE
C     CALCULATE BEGINNING OR ENDING STAND STATISTICS
C
C     N = START OR ENDING STATISTICS
C       1 = START
C       2 = END
C
C
      INTEGER*4    N,VERSION,NPTS,NTREES,TDATAI(2000,3)
C
      REAL*4       BAL(500),BALL(51),SBA,CCFL(500),CCFLL(51),
     1             STDATAR(2000,4),TDATAR(2000,4)
C
      REAL*4  HT,DBH,EX,BA,CCF,MCW
      INTEGER*4 I,K,L,ISPGRP
C
C     CALCULATE BAL, SBA AND CCFL
C
      SBA=0.
      DO I=1,500
         CCFL(I)=0.
         BAL(I)=0.
      ENDDO
      DO I=1,51
         CCFLL(I)=0.
         BALL(I)=0.
      ENDDO
      DO I=1,NTREES
         ISPGRP=TDATAI(I,2)
         IF(N .EQ. 1)THEN
            HT=STDATAR(I,2)
            DBH=STDATAR(I,1)
            EX=STDATAR(I,4)/FLOAT(NPTS)
         ELSE
            HT=TDATAR(I,2)
            DBH=TDATAR(I,1)
            EX=TDATAR(I,4)/FLOAT(NPTS)
         ENDIF
         IF(EX .LE. 0.0) CYCLE
         BA=(DBH**2*EX)*0.005454154
         SBA=SBA+BA
         SELECT CASE(VERSION)
            CASE(1)
               CALL MCW_SWO(ISPGRP,DBH,HT,MCW)
            CASE(2)
               CALL MCW_NWO(ISPGRP,DBH,HT,MCW)
            CASE(3)
               CALL MCW_SMC(ISPGRP,DBH,HT,MCW)
            CASE(4)
               CALL MCW_RAP(ISPGRP,DBH,HT,MCW)
         ENDSELECT
         CCF=0.001803*MCW**2*TDATAR(I,4)/NPTS
         IF(DBH .GT. 50.0)THEN
            L=INT(DBH-49.0)
            IF(L.GT.52) L=52
            DO K=1,500
               CCFL(K)=CCFL(K)+CCF
               BAL(K)=BAL(K)+BA
            ENDDO
            DO K=1,L-1
               CCFLL(K)=CCFLL(K)+CCF
               BALL(K)=BALL(K)+BA
            ENDDO
         ELSE
            L=INT(DBH*10.+.5)
            DO K=1,L-1
               CCFL(K)=CCFL(K)+CCF
               BAL(K)=BAL(K)+BA
            ENDDO
         ENDIF
      ENDDO
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE OLDGROWTH(NPTS,NTREES,IB,TDATAI,TDATAR,OG)
C     DETERMINE THE OLD GROWTH INDICATOR "OG"
C**********************************************************************
C
      IMPLICIT NONE
      INTEGER*4    NPTS,IB,NTREES,TDATAI(2000,3)
C
      REAL*4       TDATAR(2000,4)
C
      INTEGER*4 I,ID
      REAL*4    TOTHT,TOTD,TOTTR,HTCL(100),DCL(100),TRCL(100),HT5,DBH5,
     1          TRDIFF,HT,DBH,EXPAN,OG
C
      TOTHT=0.0
      TOTD=0.0
      TOTTR=0.0
      DO I=1,100
         HTCL(I)=0.0
         DCL(I)=0.0
         TRCL(I)=0.0
      ENDDO
      DO I=1,NTREES
         IF(TDATAI(I,2).LE.IB) THEN
            HT=TDATAR(I,2)
            DBH=TDATAR(I,1)
            EXPAN=TDATAR(I,4)/FLOAT(NPTS)
            ID=IFIX(DBH)+1
            IF(ID.GT.100) ID=100
            HTCL(ID)=HTCL(ID)+HT*EXPAN
            DCL(ID)=DCL(ID)+DBH*EXPAN
            TRCL(ID)=TRCL(ID)+EXPAN
         ENDIF
      ENDDO
      DO I=100,1,-1
         TOTHT=TOTHT+HTCL(I)
         TOTD=TOTD+DCL(I)
         TOTTR=TOTTR+TRCL(I)
         IF(TOTTR .GT. 5.0) THEN
            TRDIFF=TRCL(I)-(TOTTR-5.0)
            TOTHT=TOTHT-HTCL(I)+((HTCL(I)/TRCL(I))*TRDIFF)
            TOTD=TOTD-DCL(I)+((DCL(I)/TRCL(I))*TRDIFF)
            TOTTR=5.0
            EXIT
         ENDIF
      ENDDO
      IF(TOTTR.GT.0.0) THEN
         HT5=TOTHT/TOTTR
         DBH5=TOTD/TOTTR
         OG=DBH5*HT5/10000.0
      ELSE
         OG=0.0
      ENDIF
      RETURN
      END
*******************************************************************************
      SUBROUTINE GET_BAL(DBH,BALL1,BAL1,BAL)
      IMPLICIT NONE
      INTEGER*4 K
      REAL*4 DBH,BALL1(51),BAL1(500),BAL
      IF(DBH .GT. 100.0) THEN
         BAL=0.0
      ELSEIF(DBH .GT. 50.0)THEN
         K=INT(DBH-49.0)
         BAL=BALL1(K)
      ELSE
         K=INT(DBH*10.0+0.5)
         BAL=BAL1(K)
      ENDIF
      RETURN
      END
