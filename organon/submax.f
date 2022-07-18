      SUBROUTINE SUBMAX(TRIAL,VERSION,NTREES,TDATAI,TDATAR,MGEXP,
     1                  MSDI_1,MSDI_2,MSDI_3,A1,A2)
C----------
C ORGANON $Id$
C----------
C     CALCULATE THE MAXIMUM SIZE-DENISTY LINE
C**********************************************************************
C
      IMPLICIT NONE
      REAL*4  TDATAR(2000,8),MGEXP(2000),MSDI_1,MSDI_2,MSDI_3,A1,A2,
     1        KB,TEMPA1,BAGRP(18),TOTBA,PDF,PTF,PPP,PWH,A1MOD,TFMOD,
     2        OCMOD,DBH,EX1,PRA
      INTEGER*4 VERSION,NTREES,TDATAI(2000,3),ISPGRP,I
      LOGICAL*2 TRIAL
      SELECT CASE(VERSION)
        CASE(1,2,3)
C         REINEKE (1933)
           A2=0.62305
        CASE(4)
C         PUETTMANN ET AL. (1993)
           A2=0.64
      ENDSELECT
      KB=0.005454154
      IF(MSDI_1 .GT. 0.0) THEN
         TEMPA1=LOG(10.0)+A2*LOG(MSDI_1)
      ELSE
         SELECT CASE(VERSION)
           CASE(1)
C            ORIGINAL SWO-ORGANON - Max. SDI = 530.2
             TEMPA1=6.21113
           CASE(2)
C            ORIGINAL WWV-ORGANON - Max. SDI = 520.5
             TEMPA1=6.19958
           CASE(3)
C            ORIGINAL WWV-ORGANON
             TEMPA1=6.19958
           CASE(4)
C            PUETTMANN ET AL. (1993)
             TEMPA1=5.96
         ENDSELECT
      ENDIF
      DO I=1,18
        BAGRP(I)=0.0
      ENDDO
      DO I=1,NTREES
        ISPGRP=TDATAI(I,2)
        DBH=TDATAR(I,1)
        IF(TRIAL) THEN
           EX1=TDATAR(I,4)-MGEXP(I)
        ELSE
           EX1=TDATAR(I,4)
        ENDIF
        BAGRP(ISPGRP)=BAGRP(ISPGRP)+KB*DBH**2*EX1
      ENDDO
      TOTBA=0.0
      DO I=1,3
        TOTBA=TOTBA+BAGRP(I)
      ENDDO
      IF(TOTBA .GT. 0.0) THEN
         IF(VERSION .LE. 3) THEN
            PDF=BAGRP(1)/TOTBA
            PTF=BAGRP(2)/TOTBA
         ELSE
            PRA=BAGRP(1)/TOTBA
            PDF=BAGRP(2)/TOTBA
         ENDIF
      ELSE
         IF(VERSION .LE. 3) THEN
            PDF=0.0
            PTF=0.0
         ELSE
            PRA=0.0
            PDF=0.0
         ENDIF
      ENDIF
C     Modified for both 10/28/94
      SELECT CASE(VERSION)
        CASE(1)
          IF(MSDI_2 .GT. 0.0) THEN
             TFMOD=(LOG(10.0)+A2*LOG(MSDI_2))/TEMPA1
          ELSE
             TFMOD=1.03481817
          ENDIF
          IF(MSDI_3 .GT. 0.0) THEN
             OCMOD=(LOG(10.0)+A2*LOG(MSDI_3))/TEMPA1
          ELSE
             OCMOD=0.9943501
          ENDIF
          IF(TOTBA .GT. 0.0) THEN
             PPP=BAGRP(3)/TOTBA
          ELSE
             PPP=0.0
          ENDIF
          IF(PDF.GE.0.5) THEN
            A1MOD=1.0
          ELSE IF(PTF.GE.0.6666667) THEN
            A1MOD=TFMOD
          ELSE IF(PPP.GE.0.6666667) THEN
            A1MOD=OCMOD
          ELSE
            A1MOD=PDF+TFMOD*PTF+OCMOD*PPP
          ENDIF
        CASE(2,3)
          IF(MSDI_2 .GT. 0.0) THEN
             TFMOD=(LOG(10.0)+A2*LOG(MSDI_2))/TEMPA1
          ELSE
             TFMOD=1.03481817
          ENDIF
          IF(MSDI_3 .GT. 0.0) THEN
             OCMOD=(LOG(10.0)+A2*LOG(MSDI_3))/TEMPA1
          ELSE
C            Based on Johnson's (2000) analysis of Max. SDI for western hemlock
             OCMOD=1.014293245
          ENDIF
          IF(TOTBA .GT. 0.0) THEN
             PWH=BAGRP(3)/TOTBA
          ELSE
             PWH=0.0
          ENDIF
          IF(PDF.GE.0.5) THEN
            A1MOD=1.0
          ELSE IF(PWH.GE.0.5) THEN
            A1MOD=OCMOD
          ELSE IF(PTF.GE.0.6666667) THEN
            A1MOD=TFMOD
          ELSE
            A1MOD=PDF+OCMOD*PWH+TFMOD*PTF
          ENDIF
        CASE(4)
          IF(TOTBA .GT. 0.0) THEN
             PWH=BAGRP(3)/TOTBA
          ELSE
             PWH=0.0
          ENDIF
          A1MOD=1.0
      ENDSELECT
      IF(A1MOD.LE.0.0) A1MOD=1.0
      A1=TEMPA1*A1MOD
      RETURN
      END
