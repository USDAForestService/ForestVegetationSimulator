C
C ORGANON $Id: statsorg.f 0000 2018-02-14 00:00:00Z gedixon $
C
C**********************************************************************
      SUBROUTINE SSTATS(VERSION,NTREES,TDATAI,TDATAR,SBA,TPA,SCCF,
     1                  BAL,BALL,CCFL,CCFLL)
      IMPLICIT NONE
C
C     ROUTINE TO CALCULATE STAND LEVEL,BAL AND CCFL STATISTICS
C
      INTEGER*4 VERSION,NTREES,TDATAI(2000,3),I,ISPGRP,L,K
      REAL*4   TDATAR(2000,8),SBA,TPA,SCCF,BAL(500),BALL(51),CCFL(500),
     1         CCFLL(51),DBH,HT,EXPAN,BA,MCW,CCF
C
      DO I=1,500
         CCFL(I)=0.0
         BAL(I)=0.0
      ENDDO
      DO I=1,51
         CCFLL(I)=0.0
         BALL(I)=0.0
      ENDDO
      SBA=0.0
      SCCF=0.0
      TPA=0.0
      DO I=1,NTREES
         IF(TDATAR(I,4) .LT. 0.0001) CYCLE
         ISPGRP=TDATAI(I,2)
         DBH=TDATAR(I,1)
         HT=TDATAR(I,2)
         EXPAN=TDATAR(I,4)
         BA=DBH*DBH*EXPAN*0.005454154
         SBA=SBA+BA
         TPA=TPA+EXPAN
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
         CCF=0.001803*MCW**2*EXPAN
         SCCF=SCCF+CCF
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
            L=INT(DBH*10.0+.5)
            DO K=1,L-1
               CCFL(K)=CCFL(K)+CCF
               BAL(K)=BAL(K)+BA
            ENDDO
         ENDIF
      ENDDO
      RETURN
      END
C**********************************************************************
      SUBROUTINE HTFORTY(CTMUL,VERSION,IB,NTREES,TDATAI,TDATAR,MGEXP,
     1                   HT40)
      IMPLICIT NONE
C
C     DETERMINE HEIGHT OF THE FORTY LARGEST BIG-6 TREES PER ACRE
C**********************************************************************
C          CTMULT = CUT TREE MULTIPLIER
C                 =  0.0 TO NOT ADD OR SUBTRACT CUT TREES
C                 =  1.0 TO ADD CUT TREES
C                 = -1.0 TO SUBTRACT CUT TREES
      INTEGER*4 VERSION,IB,NTREES,TDATAI(2000,3),I,ID,IIB
      REAL*4    CTMUL,TDATAR(2000,8),MGEXP(2000),HT40,TOTHT,TOTTR,
     1          HTCL(100),TRCL(100),EXPAN,TRDIFF
C
      TOTHT=0.0
      TOTTR=0.0
      DO I=1,100
         HTCL(I)=0.0
         TRCL(I)=0.0
      ENDDO
      IIB=IB
      IF(VERSION .EQ. 4) IIB=1
      DO I=1,NTREES
         IF(TDATAI(I,2).LE.IIB) THEN
            ID=IFIX(TDATAR(I,1))+1
            IF(ID.GT.100) ID=100
            EXPAN=TDATAR(I,4)+CTMUL*MGEXP(I)
            HTCL(ID)=HTCL(ID)+TDATAR(I,2)*EXPAN
            TRCL(ID)=TRCL(ID)+EXPAN
         ENDIF
      ENDDO
      DO I=100,1,-1
         TOTHT=TOTHT+HTCL(I)
         TOTTR=TOTTR+TRCL(I)
         IF(TOTTR .GT. 40.0) THEN
            TRDIFF=TRCL(I)-(TOTTR-40.0)
            TOTHT=TOTHT-HTCL(I)+((HTCL(I)/TRCL(I))*TRDIFF)
            TOTTR=40.0
            GO TO 20
         ENDIF
      ENDDO
   20 IF(TOTTR.GT.0.0) THEN
         HT40=TOTHT/TOTTR
      ELSE
         HT40=0.0
      ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE RASITE(H,A,SI)
      IMPLICIT NONE
C
C     RED ALDER SITE INDEX EQUATION FROM WORTHINGTON, JOHNSON, STAEBLER
C         AND LLOYD (1960) PNW RESEARCH PAPER 36
C
      REAL*4 H,A,SI
      SI=(0.60924+19.538/A)*H
      RETURN
      END
C
C***********************************************************************
      FUNCTION CON_RASI(SI_1)
      IMPLICIT NONE
      REAL*4 CON_RASI,SI_1
      CON_RASI=9.73+0.64516*(SI_1+4.5)
      RETURN
      END
