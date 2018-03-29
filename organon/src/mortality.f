C
C ORGANON $Id: mortality.f 0000 2018-02-14 00:00:00Z gedixon $
C
C     ORGANON GROWTH AND YIELD MODEL
C     SUBROUTINES INCLUDED:
C         MORTAL
C         FUNCTION QUAD
C         RAMORT
C         PM_FERT
C         PM_SWO
C         PM_NWO
C         PM_SMC
C         PM_RAP
C         OLDGRO
C
C  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
C               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
C               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
C               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.
C
C  CHANGED THE NAME OF SUBROUTINE - MORTAL TO MORTAL_RUN
C**********************************************************************
      SUBROUTINE MORTAL_RUN(VERSION,CYCLG,NTREES,IB,TDATAI,POST,MORT,
     1                  TDATAR,SCR,GROWTH,MGEXP,DEADEXP,BALL1,BAL1,SI_1,
     2                  SI_2,PN,YF,A1,A2,A1MAX,PA1MAX,NO,RD0,RAAGE,PDEN)
      IMPLICIT NONE
C     ROUTINE FOR SETTING TREE MORTALITY
C**********************************************************************
C
      INTEGER*4 VERSION,CYCLG,NTREES,IB,TDATAI(2000,3),I,ISPGRP,
     1          IND,KK
      REAL*4    TDATAR(2000,8),SCR(2000,3),GROWTH(2000,4),
     1          MGEXP(2000),DEADEXP(2000),BALL1(51),BAL1(500),SI_1,
     2          SI_2,PN(5),YF(5),A1,A2,RAAGE,A3,RDCC,KB,KR1,STBA,STN,
     3          PMK(2000),RAN,SQMDM,SQMDA,RD,RD0,BAA,NA,OG1,DBH,HT,
     4          FERTADJ,SBAL1,CR,PM,PS,QMDA,RDA,A1MAX,PA1MAX,XA3,NO,
     5          QMDP,CRADJ,NK,NAA,BAAA,QUAD1,QUAD2,POW(2000),XPM,PDEN
      LOGICAL*2 POST,MORT
C      INTEGER*4 IANS,IYN
      DO I=1,NTREES
         POW(I)=1.0
         IF(VERSION .EQ. 4 .AND. TDATAI(I,1) .NE. 351) THEN
            POW(I)=0.2
         ENDIF
      ENDDO
      IF(VERSION .LE. 3)THEN
         A3=14.39533971
      ELSE
         A3=3.88
      ENDIF
      IF(VERSION .LE. 3) THEN
         RDCC=0.60
      ELSE
         RDCC=0.5211
      ENDIF
      KB=0.005454154
      KR1=1.0
      STBA=0.0
      STN=0.0
      RAN=0.0
      DO I=1,NTREES
         STBA=STBA+TDATAR(I,1)**2*KB*TDATAR(I,4)
         STN=STN+TDATAR(I,4)
         IF(TDATAI(I,1) .EQ. 351 .AND. VERSION .LE. 3) THEN
            RAN=RAN+TDATAR(I,4)
         ENDIF
         IF(CYCLG.EQ.0 .AND. POST) THEN
           STBA=STBA+TDATAR(I,1)**2*KB*MGEXP(I)
           STN=STN+MGEXP(I)
         ENDIF
         PMK(I)=0.0
         DEADEXP(I)=0.0
      ENDDO
      IF(RAN .LE. 0.0001) THEN
         RAAGE=0.0
      ENDIF
      SQMDM=EXP(A1-A2*LOG(STN))
      SQMDA=SQRT(STBA/(KB*STN))
      RD=STN/EXP(A1/A2-LOG(SQMDA)/A2)
      IF(CYCLG.EQ.0) THEN
         RD0=RD
         NO=0.0
         A1MAX=A1
      ENDIF
      BAA=0.0
      NA=0.0
      CALL OLDGRO(NTREES,IB,TDATAI,TDATAR,GROWTH,DEADEXP,0.0,OG1)
C
C  INDIVIDUAL TREE MORTALITY EQUATIONS
C
      DO I=1,NTREES
         IF(TDATAR(I,4) .LE. 0.) CYCLE
         ISPGRP=TDATAI(I,2)
         DBH=TDATAR(I,1)
         HT=TDATAR(I,2)
         CALL PM_FERT(ISPGRP,VERSION,CYCLG,PN,YF,FERTADJ)
         CALL GET_BAL(DBH,BALL1,BAL1,SBAL1)
         IF(SCR(I,1) .GT. TDATAR(I,3)) THEN
            CR=SCR(I,1)
         ELSE
            CR=TDATAR(I,3)
         ENDIF

C      IF (I .EQ. 15) THEN
C         WRITE(*,*) ' SBAL1 = ',SBAL1,' BAL1(1) = ',BAL1(1)
C         WRITE(*,1600)
C 1600    FORMAT(1X,' '\)
C         IANS = IYN(2)
C      ENDIF
         SELECT CASE(VERSION)
           CASE(1)
              CALL PM_SWO(ISPGRP,DBH,CR,SI_1,SBAL1,OG1,POW(I),PMK(I))
           CASE(2)
              CALL PM_NWO(ISPGRP,DBH,CR,SI_1,SI_2,SBAL1,POW(I),PMK(I))
           CASE(3)
              CALL PM_SMC(ISPGRP,DBH,CR,SI_1,SI_2,SBAL1,POW(I),PMK(I))
           CASE(4)
              CALL PM_RAP(ISPGRP,DBH,CR,SI_1,SI_2,SBAL1,POW(I),PMK(I))
         ENDSELECT
         PMK(I)=PMK(I)+FERTADJ
      ENDDO
      IF(VERSION .LE. 3)THEN
         IF(RAAGE .GE. 55.0) THEN
            CALL RAMORT(NTREES,TDATAI,RAAGE,TDATAR,RAN,PMK)
         ENDIF
         RAAGE=RAAGE+5.0
      ENDIF
      DO I=1,NTREES
         IF(SCR(I,1) .GT. TDATAR(I,3)) THEN
            CR=SCR(I,1)
         ELSE
            CR=TDATAR(I,3)
         ENDIF
C         CRADJ=1.0-EXP(-(10.0*CR)**5.0)
         CRADJ = 1.0
         IF (CR.LE. 0.17) CRADJ=1.0-EXP(-(25.0*CR)**2.0)
         XPM=1.0/(1.0+EXP(-PMK(I)))
         PS=(1.0-XPM)**POW(I)
         PM=1.0-PS*CRADJ
         NA=NA+TDATAR(I,4)*(1.0-PM)
         BAA=BAA+KB*(TDATAR(I,1)+GROWTH(I,2))**2*TDATAR(I,4)*(1.0-PM)
      ENDDO

C
C  DETERMINE IF ADDITIONAL MORTALITY MUST BE TAKEN
C
      IF(MORT)THEN
           QMDA=SQRT(BAA/(KB*NA))
           RDA=NA/EXP(A1/A2-LOG(QMDA)/A2)
           IF(CYCLG .EQ.0) THEN
C
C  INITALIZATIONS FOR FIRST GROWTH CYCLE
C
                IF(RD .GE.1.0)THEN
                   IF(RDA .GT. RD) THEN
                      A1MAX=LOG(SQMDA)+A2*LOG(STN)
                   ELSE
                      A1MAX=LOG(QMDA)+A2*LOG(NA)
                   ENDIF
                   IND=1
                   IF(A1MAX .LT. A1) A1MAX=A1
                   PA1MAX=A1MAX
                ELSE
                     IND=0
                     IF(VERSION .LE. 3) THEN
                        IF(RD .GT. RDCC) THEN
                           XA3=-1.0/A3
                           NO=STN*(LOG(RD)/LOG(RDCC))**XA3
                        ENDIF
                     ELSE
                        NO=PDEN
                     ENDIF
                ENDIF
C
C  INITIALIZATIONS FOR SUBSEQUENT GROWTH CYCLES
C
           ELSE
                IF(RD0.GE.1.0) THEN
                     IND=1
                     A1MAX=LOG(QMDA)+A2*LOG(NA)
                     IF(A1MAX .GT. PA1MAX) A1MAX=PA1MAX
                     IF(A1MAX .LT. A1) A1MAX=A1
                     PA1MAX=A1MAX
                ELSE IF(RD .GE. 1.0 .AND. NO .LE. 0.0)THEN
                     IF(RDA .GT. RD) THEN
                        A1MAX=LOG(SQMDA)+A2*LOG(STN)
                     ELSE
                        A1MAX=LOG(QMDA)+A2*LOG(NA)
                     ENDIF
                     IND=1
                     IF(A1MAX .LT. A1) A1MAX=A1
                     PA1MAX=A1MAX
                ELSE
                     IND=0
                     IF(VERSION .LE. 3) THEN
                        IF(RD .GT. RDCC .AND. NO .LE. 0.0) THEN
                           XA3=-1.0/A3
                           NO=STN*(LOG(RD)/LOG(RDCC))**XA3
                        ENDIF
                     ELSE
                        NO=PDEN
                     ENDIF
                ENDIF
           ENDIF
C
C  COMPUTATION OF ADDITIONAL MORTALITY IF NECESSARY
C
           IF(IND .EQ. 0 .AND .NO .GT. 0.0) THEN
              IF(VERSION .LE. 3) THEN
                 QMDP=QUAD1(NA,NO,RDCC,A1)
              ELSE
                 QMDP=QUAD2(NA,NO,RDCC,A1)
              ENDIF
           ELSE
               QMDP=EXP(A1MAX-A2*LOG(NA))
           ENDIF
C
C          NO ADDITIONAL MORTALITY NECESSARY
C
           IF(RD .LE. RDCC .OR. QMDP .GT. QMDA) THEN
                DO I=1,NTREES
                   IF(SCR(I,1) .GT. TDATAR(I,3)) THEN
                      CR=SCR(I,1)
                   ELSE
                      CR=TDATAR(I,3)
                   ENDIF
C                   CRADJ=1.0-EXP(-(10.0*CR)**5.0)
                   CRADJ = 1.0
                   IF (CR.LE. 0.17) CRADJ=1.0-EXP(-(25.0*CR)**2.0)
                   XPM=1.0/(1.0+EXP(-PMK(I)))
                   PS=(1.0-XPM)**POW(I)
                   PM=1.0-PS*CRADJ
                   DEADEXP(I)=TDATAR(I,4)*PM
                   TDATAR(I,4)=TDATAR(I,4)*(1.0-PM)
                ENDDO
C
C          ADJUSTMENT TO MORTALITY NECESSARY
C
           ELSE
                KR1=0.0
                DO KK=1,7
                   NK=10.0/10.0**KK
   82              KR1=KR1+NK
                   NAA=0.0
                   BAAA=0.0
                   DO I=1,NTREES
                      IF(TDATAR(I,4).LT.0.001) CYCLE
                      IF(SCR(I,1) .GT. TDATAR(I,3)) THEN
                         CR=SCR(I,1)
                      ELSE
                         CR=TDATAR(I,3)
                      ENDIF
C                      CRADJ=1.0-EXP(-(10.0*CR)**5.0)
                      CRADJ = 1.0
                      IF (CR.LE. 0.17) CRADJ=1.0-EXP(-(25.0*CR)**2.0)
                      XPM=1.0/(1.0+EXP(-(KR1+PMK(I))))
                      PS=(1.0-XPM)**POW(I)
                      PM=1.0-PS*CRADJ
                      NAA=NAA+TDATAR(I,4)*(1.0-PM)
                      BAAA=BAAA+KB*(TDATAR(I,1)+GROWTH(I,2))**2.0*
     1                             TDATAR(I,4)*(1.0-PM)
                   ENDDO
                   QMDA=SQRT(BAAA/(KB*NAA))
                   IF (IND .EQ. 0) THEN
                      IF(VERSION .LE. 3) THEN
                         QMDP=QUAD1(NAA,NO,RDCC,A1)
                      ELSE
                         QMDP=QUAD2(NAA,NO,RDCC,A1)
                      ENDIF
                   ELSE
                       QMDP=EXP(A1MAX-A2*LOG(NAA))
                   ENDIF
                   IF(QMDP.GE.QMDA)THEN
                      KR1=KR1-NK
                   ELSE
                      GO TO 82
                   ENDIF
                ENDDO
                DO I=1,NTREES
                   IF(TDATAR(I,4).LE.0.0) THEN
                     DEADEXP(I)=0.0
                     TDATAR(I,4)=0.0
                   ELSE
                     IF(SCR(I,1) .GT. TDATAR(I,3)) THEN
                        CR=SCR(I,1)
                     ELSE
                        CR=TDATAR(I,3)
                     ENDIF
C                     CRADJ=1.0-EXP(-(10.0*CR)**5.0)
                     CRADJ = 1.0
                     IF (CR.LE. 0.17) CRADJ=1.0-EXP(-(25.0*CR)**2.0)
                     XPM=1.0/(1.0+EXP(-(KR1+PMK(I))))
                     PS=(1.0-XPM)**POW(I)
                     PM=1.0-PS*CRADJ
                     DEADEXP(I)=TDATAR(I,4)*PM
                     TDATAR(I,4)=TDATAR(I,4)*(1.0-PM)
                   ENDIF
                ENDDO
           ENDIF
      ELSE
                DO I=1,NTREES
                   IF(SCR(I,1) .GT. TDATAR(I,3)) THEN
                      CR=SCR(I,1)
                   ELSE
                      CR=TDATAR(I,3)
                   ENDIF
C                   CRADJ=1.0-EXP(-(10.0*CR)**5.0)
                   CRADJ = 1.0
                   IF (CR.LE. 0.17) CRADJ=1.0-EXP(-(25.0*CR)**2.0)
                   XPM=1.0/(1.0+EXP(-PMK(I)))
                   PS=(1.0-XPM)**POW(I)
                   PM=1.0-PS*CRADJ
                   DEADEXP(I)=TDATAR(I,4)*PM
                   TDATAR(I,4)=TDATAR(I,4)*(1.0-PM)
                ENDDO
      ENDIF
      DO I=1,NTREES
         IF(TDATAR(I,4) .LT. 0.00001)TDATAR(I,4)=0.0
      ENDDO
      RETURN
      END
C**********************************************************************
      FUNCTION QUAD1(NI,NO,RDCC,A1)
C**********************************************************************
C
      IMPLICIT NONE
      REAL*4 NI,NO,QUAD1,A1,A2,A3,A4,X,RDCC
      A2=0.62305
      A3=14.39533971
      A4=-(LOG(RDCC)*A2/A1)
      X=A1-A2*LOG(NI)-(A1*A4)*EXP(-A3*(LOG(NO)-LOG(NI)))
      QUAD1=EXP(X)
      RETURN
      END
C**********************************************************************
      FUNCTION QUAD2(NI,NO,RDCC,A1)
C**********************************************************************
C
      IMPLICIT NONE
      REAL*4 NI,NO,QUAD2,A1,A2,A3,A4,X,RDCC
      REAL*4 DANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      DANUW = RDCC
C
      A2=0.64
      A3=3.88
      A4=0.07
      X=A1-A2*LOG(NI)-(A1*A4)*EXP(-A3*(LOG(NO)-LOG(NI)))
      QUAD2=EXP(X)
      RETURN
      END
C**********************************************************************
      SUBROUTINE RAMORT(NTREES,TDATAI,RAAGE,TDATAR,RAN,PMK)
      IMPLICIT NONE
      REAL*4 RAAGE,TDATAR(2000,8),RAN,PMK(2000),PM,RAMORT1,RAMORT2,
     1       RAQMDN1,RAQMDN2,RABAN1,RABAN2,RATPAN1,RATPAN2,KB,KR1,NK
      INTEGER*4 NTREES,TDATAI(2000,3),I,KK
      KB=0.005454154
      RAMORT1=0.0
      DO I=1,NTREES
         IF(TDATAI(I,1) .EQ. 351) THEN
            PM=1.0/(1.0+EXP(-PMK(I)))
            RAMORT1=RAMORT1+TDATAR(I,4)*PM
         ENDIF
      ENDDO
      RAQMDN1=3.313+0.18769*RAAGE-0.000198*RAAGE*RAAGE
      RABAN1=-26.1467+5.31482*RAAGE-0.037466*RAAGE*RAAGE
      RAQMDN2=3.313+0.18769*(RAAGE+5.0)-0.000198*(RAAGE+5.0)**2
      RABAN2=-26.1467+5.31482*(RAAGE+5.0)-0.037466*(RAAGE+5.0)**2
      RATPAN1=RABAN1/(KB*RAQMDN1*RAQMDN1)
      RATPAN2=RABAN2/(KB*RAQMDN2*RAQMDN2)
      IF(RATPAN1 .GT. 0.0 .AND. RATPAN2 .GT. 0.0) THEN
         RAMORT2=RAN*(1.0-RATPAN2/RATPAN1)
      ELSE
          DO I=1,NTREES
             IF(TDATAI(I,1) .EQ. 351) THEN
                PMK(I)=1000.0
             ENDIF
          ENDDO
          RETURN
      ENDIF
      IF(RAMORT1 .LT. RAMORT2) THEN
         KR1=0.0
         DO KK=1,7
            NK=10.0/10.0**KK
   30       KR1=KR1+NK
            RAMORT1=0.0
            DO I=1,NTREES
               IF(TDATAI(I,1) .EQ. 351) THEN
                  PM=1.0/(1.0+EXP(-(KR1+PMK(I))))
                  RAMORT1=RAMORT1+TDATAR(I,4)*PM
               ENDIF
            ENDDO
            IF(RAMORT1 .GT. RAMORT2) THEN
               KR1=KR1-NK
             ELSE
                GO TO 30
            ENDIF
         ENDDO
         DO I=1,NTREES
            IF(TDATAI(I,1) .EQ. 351) THEN
               PMK(I)=KR1+PMK(I)
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END

C**********************************************************************
      SUBROUTINE PM_FERT(ISPGRP,VERSION,CYCLG,PN,YF,FERTADJ)
      IMPLICIT NONE
      INTEGER*4 ISPGRP,VERSION,CYCLG,II
      REAL*4 PN(5),YF(5),FERTADJ,PF1,PF2,PF3,XTIME,FERTX1,FERTX2
      IF(VERSION .LE. 3) THEN
         IF(ISPGRP .EQ. 1)THEN
            PF1=0.0000552859
            PF2=1.5
            PF3=-0.5
         ELSE
            PF1=0.0
            PF2=1.0
            PF3=0.0
         ENDIF
      ELSE
         PF1=0.0
         PF2=1.0
         PF3=0.0
      ENDIF
      XTIME=FLOAT(CYCLG)*5.0
      FERTX1=0.0
      DO II=2,5
         FERTX1=FERTX1+PN(II)*EXP((PF3/PF2)*(YF(1)-YF(II)))
      ENDDO
      FERTX2=EXP(PF3*(XTIME-YF(1)))
      FERTADJ=PF1*(PN(1)+FERTX1)**PF2*FERTX2
      RETURN
      END
C**********************************************************************
      SUBROUTINE PM_SWO(ISPGRP,DBH,CR,SI_1,BAL,OG,POW,PM)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 DBH,CR,SI_1,BAL,OG,POW,PM,B0,B1,B2,B3,B4,B5,B6,B7,
     1       MPAR(18,9)
C
C  NEW SWO MORTALITY WITH REVISED CLO PARAMETERS (8 parameters - all species)
C
C     DF Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     GW Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PP Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     SP Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
C     IC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RC Coefficients from WH of Hann, Marshall, Hanus (2003) FRL Research Contribution 40
C     PY Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     GC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     TA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     CL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     BO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RA Coefficients from Best Guess
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Best Guess
C
      DATA MPAR/
     1         -4.648483270, -2.215777201, -1.050000682, -1.531051304, !  DF,GW,PP,SP
     1         -1.922689902, -1.166211991, -0.761609   , -4.072781265, !  IC,WH,RC,PY
     1         -6.089598985, -4.317549852, -2.410756914, -2.990451960, !  MD,GC,TA,CL
     1         -2.976822456, -6.00031085 , -3.108619921, -2.0        , !  BL,WO,BO,RA
     1         -3.020345211, -1.386294361,                             !  PD,WI
C
     2         -0.266558690, -0.162895666, -0.194363402,  0.0        , !  DF,GW,PP,SP
     2         -0.136081990,  0.0        , -0.529366   , -0.176433475, !  IC,WH,RC,PY
     2         -0.245615070, -0.057696253,  0.0        ,  0.0        , !  MD,GC,TA,CL
     2          0.0        , -0.10490823 , -0.570366764, -0.5        , !  BL,WO,BO,RA
     2          0.0        ,  0.0        ,                             !  PD,WI
C
     3          0.003699110,  0.003317290,  0.003803100,  0.0        , !  DF,GW,PP,SP
     3          0.002479863,  0.0        ,  0.0        ,  0.0        , !  IC,WH,RC,PY
     3          0.0        ,  0.0        ,  0.0        ,  0.0        , !  MD,GC,TA,CL
     3          0.0        ,  0.0        ,  0.018205398,  0.015      , !  BL,WO,BO,RA
     3          0.0        ,  0.0        ,                             !  PD,WI
C
     4         -2.118026640, -3.561438261, -3.557300286,  0.0        , !  DF,GW,PP,SP
     4         -3.178123293, -4.602668157, -4.74019    , -1.729453975, !  IC,WH,RC,PY
     4         -3.208265570,  0.0        , -1.049353753,  0.0        , !  MD,GC,TA,CL
     4         -6.223250962, -0.99541909 , -4.584655216, -3.0        , !  BL,WO,BO,RA
     4         -8.467882343,  0.0        ,                             !  PD,WI
C
     5          0.025499430,  0.014644689,  0.003971638,  0.0        , !  DF,GW,PP,SP
     5          0.0        ,  0.0        ,  0.0119587  ,  0.0        , !  IC,WH,RC,PY
     5          0.033348079,  0.004861355,  0.008845583,  0.0        , !  MD,GC,TA,CL
     5          0.0        ,  0.00912739 ,  0.014926170,  0.015      , !  BL,WO,BO,RA
     5          0.013966388,  0.0        ,                             !  PD,WI
C
     6          0.003361340,  0.0        ,  0.005573601,  0.0        , !  DF,GW,PP,SP
     6          0.004684133,  0.0        ,  0.00756365 ,  0.012525642, !  IC,WH,RC,PY
     6          0.013571319,  0.00998129 ,  0.0        ,  0.002884840, !  MD,GC,TA,CL
     6          0.0        ,  0.87115652 ,  0.012419026,  0.01       , !  BL,WO,BO,RA
     6          0.009461545,  0.0        ,                             !  PD,WI
C
     7          0.013553950,  0.0        ,  0.0        ,  0.0        , !  DF,GW,PP,SP
     7          0.0        ,  0.0        ,  0.0        ,  0.0        , !  IC,WH,RC,PY
     7          0.0        ,  0.0        ,  0.0        ,  0.0        , !  MD,GC,TA,CL
     7          0.0        ,  0.0        ,  0.0        ,  0.0        , !  BL,WO,BO,RA
     7          0.0        ,  0.0        ,                             !  PD,WI
C
     8         -2.723470950,  0.0        ,  0.0        ,  0.0        , !  DF,GW,PP,SP
     8          0.0        ,  0.0        ,  0.0        ,  0.0        , !  IC,WH,RC,PY
     8          0.0        ,  0.0        ,  0.0        ,  0.0        , !  MD,GC,TA,CL
     8          0.0        ,  0.0        ,  0.0        ,  0.0        , !  BL,WO,BO,RA
     8          0.0        ,  0.0        ,                             !  PD,WI
C
     9          1.0        ,  1.0        ,  1.0        ,  1.0        , !  DF,GW,PP,SP
     9          1.0        ,  1.0        ,  1.0        ,  1.0        , !  IC,WH,RC,PY
     9          1.0        ,  1.0        ,  1.0        ,  1.0        , !  MD,GC,TA,CL
     9          1.0        ,  1.0        ,  1.0        ,  1.0        , !  BL,WO,BO,RA
     9          1.0        ,  1.0/                                     !  PD,WI
C
      B0=MPAR(ISPGRP,1)
      B1=MPAR(ISPGRP,2)
      B2=MPAR(ISPGRP,3)
      B3=MPAR(ISPGRP,4)
      B4=MPAR(ISPGRP,5)
      B5=MPAR(ISPGRP,6)
      B6=MPAR(ISPGRP,7)
      B7=MPAR(ISPGRP,8)
      POW=MPAR(ISPGRP,9)
      IF(ISPGRP .EQ. 14) THEN   ! Oregon White Oak
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*ALOG(BAL+5.0)
      ELSE
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*BAL
     1     +B6*BAL*EXP(B7*OG)
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE PM_NWO(ISPGRP,DBH,CR,SI_1,SI_2,BAL,POW,PM)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 DBH,CR,SI_1,SI_2,BAL,POW,PM,B0,B1,B2,B3,B4,B5,MPAR(11,7),
     1       SQDBH,CR25
C
C  NWO MORTALITY (6 parameters - all species)
C
C     DF Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
C     GF Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
C     WH Coefficients from Hann, Marshall, Hanus (2003) FRL Research Contribution 40
C     RC Coefficients from WH of Hann, Marshall, Hanus (2003) FRL Research Contribution 40
C     PY Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     RA Coefficients from Best Guess
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Best Guess
C
      DATA MPAR/
     1         -4.13142    , -7.60159    , -0.761609   , -0.761609   , !  DF,GF,WH,RC
     1         -4.072781265, -6.089598985, -2.976822456, -6.00031085 , !  PY,MD,BL,WO
     1         -2.0        , -3.020345211, -1.386294361,               !  RA,PD,WI
C
     2         -1.13736    , -0.200523   , -0.529366   , -0.529366   , !  DF,GF,WH,RC
     2         -0.176433475, -0.245615070,  0.0        , -0.10490823 , !  PY,MD,BL,WO
     2         -0.5        ,  0.0        ,  0.0        ,               !  RA,PD,WI
C
     3          0.0        ,  0.0        ,  0.0        ,  0.0        , !  DF,GF,WH,RC
     3          0.0        ,  0.0        ,  0.0        ,  0.0        , !  PY,MD,BL,WO
     3          0.015      ,  0.0        ,  0.0        ,               !  RA,PD,WI
C
     4         -0.823305   ,  0.0        , -4.74019    , -4.74019    , !  DF,GF,WH,RC
     4         -1.729453975, -3.208265570, -6.223250962, -0.99541909 , !  PY,MD,BL,WO
     4         -3.0        , -8.467882343,  0.0        ,               !  RA,PD,WI
C
     5          0.0307749  ,  0.0441333  ,  0.0119587  ,  0.0119587  , !  DF,GF,WH,RC
     5          0.0        ,  0.033348079,  0.0        ,  0.00912739 , !  PY,MD,BL,WO
     5          0.015      ,  0.013966388,  0.0        ,               !  RA,PD,WI
C
     6          0.00991005 ,  0.00063849 ,  0.00756365 ,  0.00756365 , !  DF,GF,WH,RC
     6          0.012525642,  0.013571319,  0.0        ,  0.87115652 , !  PY,MD,BL,WO
     6          0.01       ,  0.009461545,  0.0        ,               !  RA,PD,WI
C
     7          1.0        ,  1.0        ,  1.0        ,  1.0        , !  DF,GF,WH,RC
     7          1.0        ,  1.0        ,  1.0        ,  1.0        , !  PY,MD,BL,WO
     7          1.0        ,  1.0        ,  1.0/                       !  RA,PD,WI
C
      B0=MPAR(ISPGRP,1)
      B1=MPAR(ISPGRP,2)
      B2=MPAR(ISPGRP,3)
      B3=MPAR(ISPGRP,4)
      B4=MPAR(ISPGRP,5)
      B5=MPAR(ISPGRP,6)
      POW=MPAR(ISPGRP,7)
      SQDBH=DBH**0.5
      CR25=CR**0.25
      IF(ISPGRP .EQ. 1)THEN                          ! Douglas fir
         PM=B0+B1*SQDBH+B3*CR25+B4*(SI_1+4.5)+B5*BAL
      ELSE IF(ISPGRP .EQ. 2)THEN                     ! Grand Fir
C      IF(ISPGRP .EQ. 2)THEN                          ! Grand Fir
         PM=B0+B1*DBH+B4*(SI_1+4.5)+B5*(BAL/DBH)
      ELSEIF(ISPGRP .EQ. 3 .OR. ISPGRP .EQ. 4) THEN  ! Western Hemlock and Western Red Cedar
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_2+4.5)+B5*BAL
      ELSEIF(ISPGRP .EQ. 8) THEN   ! Oregon White Oak
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*ALOG(BAL+5.0)
      ELSE
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*BAL
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE PM_SMC(ISPGRP,DBH,CR,SI_1,SI_2,BAL,POW,PM)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 DBH,CR,SI_1,SI_2,BAL,POW,PM,B0,B1,B2,B3,B4,B5,MPAR(11,7)
C
C  SMC MORTALITY (6 parameters - all species)
C
C     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution 49
C     GF Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
C     WH Coefficients from Hann, Marshall, Hanus (2003) FRL Research Contribution 40
C     RC Coefficients from WH of Hann, Marshall, Hanus (2003) FRL Research Contribution 40
C     PY Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     RA Coefficients from Best Guess
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Best Guess
C
      DATA MPAR/
     1         -3.12161659 , -7.60159    , -0.761609   , -0.761609   , !  DF,GF,WH,RC
     1         -4.072781265, -6.089598985, -2.976822456, -6.00031085 , !  PY,MD,BL,WO
     1         -2.0        , -3.020345211, -1.386294361,               !  RA,PD,WI
C
     2         -0.44724396 , -0.200523   , -0.529366   , -0.529366   , !  DF,GF,WH,RC
     2         -0.176433475, -0.245615070,  0.0        , -0.10490823 , !  PY,MD,BL,WO
     2         -0.5        ,  0.0        ,  0.0        ,               !  RA,PD,WI
C
     3          0.0        ,  0.0        ,  0.0        ,  0.0        , !  DF,GF,WH,RC
     3          0.0        ,  0.0        ,  0.0        ,  0.0        , !  PY,MD,BL,WO
     3          0.015      ,  0.0        ,  0.0        ,               !  RA,PD,WI
C
     4         -2.48387172 ,  0.0        , -4.74019    , -4.74019    , !  DF,GF,WH,RC
     4         -1.729453975, -3.208265570, -6.223250962, -0.99541909 , !  PY,MD,BL,WO
     4         -3.0        , -8.467882343,  0.0        ,               !  RA,PD,WI
C
     5          0.01843137 ,  0.0441333  ,  0.0119587  ,  0.0119587  , !  DF,GF,WH,RC
     5          0.0        ,  0.033348079,  0.0        ,  0.00912739 , !  PY,MD,BL,WO
     5          0.015      ,  0.013966388,  0.0        ,               !  RA,PD,WI
C
     6          0.01353918 ,  0.00063849 ,  0.00756365 ,  0.00756365 , !  DF,GF,WH,RC
     6          0.012525642,  0.013571319,  0.0        ,  0.87115652 , !  PY,MD,BL,WO
     6          0.01       ,  0.009461545,  0.0        ,               !  RA,PD,WI
C
     7          1.0        ,  1.0        ,  1.0        ,  1.0        , !  DF,GF,WH,RC
     7          1.0        ,  1.0        ,  1.0        ,  1.0        , !  PY,MD,BL,WO
     7          1.0        ,  1.0        ,  1.0/                       !  RA,PD,WI
C
      B0=MPAR(ISPGRP,1)
      B1=MPAR(ISPGRP,2)
      B2=MPAR(ISPGRP,3)
      B3=MPAR(ISPGRP,4)
      B4=MPAR(ISPGRP,5)
      B5=MPAR(ISPGRP,6)
      POW=MPAR(ISPGRP,7)
      IF(ISPGRP .EQ. 2) THEN                          ! Grand Fir
         PM=B0+B1*DBH+B4*(SI_1+4.5)+B5*(BAL/DBH)
      ELSEIF(ISPGRP .EQ. 3 .OR. ISPGRP .EQ. 4) THEN   ! Western Hemlock and Western Red Cedar
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_2+4.5)+B5*BAL
      ELSEIF(ISPGRP .EQ. 8) THEN   ! Oregon White Oak
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*ALOG(BAL+5.0)
      ELSE
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*BAL
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE PM_RAP(ISPGRP,DBH,CR,SI_1,SI_2,BAL,POW,PM)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 DBH,CR,SI_1,SI_2,BAL,POW,PM,B0,B1,B2,B3,B4,B5,MPAR(7,7),
     1       SITE
C
C  RAP MORTALITY (6 parameters - all species)
C
C     RA Coefficients from Hann, Bluhm, and Hibbs New Red Alder Equation
C     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution 49
C     WH Coefficients from Hann, Marshall, Hanus (2003) FRL Research Contribution 40
C     RC Coefficients from WH of Hann, Marshall, Hanus (2003) FRL Research Contribution 40
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Best Guess
C
      DATA MPAR/
     1         -4.333150734 , -3.12161659 , -0.761609   , -0.761609   , !  RA,DF,WH,RC
     1         -2.976822456 , -3.020345211, -1.386294361,               !  BL,PD,WI
C
     2         -0.9856713799, -0.44724396 , -0.529366   , -0.529366   , !  RA,DF,WH,RC
     2          0.0         ,  0.0        ,  0.0        ,               !  BL,PD,WI,WO
C
     3          0.0         ,  0.0        ,  0.0        ,  0.0        , !  RA,DF,WH,RC
     3          0.0         ,  0.0        ,  0.0        ,               !  BL,PD,WI
C
     4         -2.583317081 , -2.48387172 , -4.74019    , -4.74019    , !  RA,DF,WH,RC
     4         -6.223250962 , -8.467882343,  0.0        ,               !  BL,PD,WI
C
     5          0.0369852164,  0.01843137 ,  0.0119587  ,  0.0119587  , !  RA,DF,WH,RC
     5          0.0         ,  0.013966388,  0.0        ,               !  BL,PD,WI
C
     6          0.0394546978,  0.01353918 ,  0.00756365 ,  0.00756365 , !  RA,DF,WH,RC
     6          0.0         ,  0.009461545,  0.0        ,               !  BL,PD,WI
C
     7          1.0         ,  0.2        ,  0.2        ,  0.2        , !  RA,DF,WH,RC
     7          0.2         ,  0.2        ,  0.2        /               !  BL,PD,WI
C
      B0=MPAR(ISPGRP,1)
      B1=MPAR(ISPGRP,2)
      B2=MPAR(ISPGRP,3)
      B3=MPAR(ISPGRP,4)
      B4=MPAR(ISPGRP,5)
      B5=MPAR(ISPGRP,6)
      POW=MPAR(ISPGRP,7)
      IF(ISPGRP .EQ. 1) THEN
         SITE=SI_1+4.5
      ELSEIF(ISPGRP .EQ. 2 .OR. ISPGRP .GT. 4)THEN
         SITE=SI_2+4.5
      ELSEIF(ISPGRP .EQ. 3 .OR. ISPGRP .EQ. 4)THEN
         SITE=-0.432 + 0.899 * (SI_2+4.5)
      ENDIF
      PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*SITE+B5*BAL
      RETURN
      END
C**********************************************************************
      SUBROUTINE OLDGRO(NTREES,IB,TDATAI,TDATAR,GROWTH,DEADEXP,
     1                  XIND,OG)
C     DETERMINE THE OLD GROWTH INDICATOR "OG"
C**********************************************************************
C          XIND =  0.0, DO NOT ADD GROWTH VALUES OR MORTALITY VALUES
C               = -1.0, SUBTRACT GROWTH VALUES AND ADD MORTALITY VALUES
C               =  1.0, ADD GROWTH VALUES AND SUBTRACT MORTALITY VALUES
      IMPLICIT NONE
      INTEGER*4 NTREES,IB,TDATAI(2000,3),I,ID
      REAL*4    TDATAR(2000,8),GROWTH(2000,4),DEADEXP(2000),TOTHT,
     1          TOTD,TOTTR,HTCL(100),DCL(100),TRCL(100),HT5,DBH5,TRDIFF,
     2          HT,DBH,EXPAN,XIND,OG
C      INTEGER*4 IANS,IYN
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
            HT=TDATAR(I,2)+XIND*GROWTH(I,1)
            DBH=TDATAR(I,1)+XIND*GROWTH(I,2)
            EXPAN=TDATAR(I,4)-XIND*DEADEXP(I)
C      WRITE(*,*) ' HG= ',GROWTH(I,1),' DG= ',GROWTH(I,2)
C      WRITE(*,*) ' DEAD= ',DEADEXP(I),' I= ',I
C      WRITE(*,1600)
C 1600 FORMAT(1X,' '\)
C      IANS = IYN(2)
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
            GO TO 20
         ENDIF
      ENDDO
   20 IF(TOTTR.GT.0.0) THEN
C      WRITE(*,*) ' TOTHT= ',TOTHT,' TOTD= ',TOTD,' TOTTR= ',TOTTR
C      WRITE(*,1600)
C 1600 FORMAT(1X,' '\)
C      IANS = IYN(2)
         HT5=TOTHT/TOTTR
         DBH5=TOTD/TOTTR
         OG=DBH5*HT5/10000.0
      ELSE
         OG=0.0
      ENDIF
      RETURN
      END
