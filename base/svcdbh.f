      SUBROUTINE SVCDBH (REMOV,IST) 
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C     STAND VISUALIZATION GENERATION
C     J. J. MARCHINEK -- RMRS MOSCOW -- APRIL 1999
C     N.L.CROOKSTON -- RMRS MOSCOW -- OCT 1999
C
C     CALCULATES SOME PLOT AND TOTAL STATISTICS USEFUL TO 
C     COMPARE THE DISTRIBUTION OF TREES ON THE OBJECT LIST
C     WITH THOSE ON THE FVS TREE LIST.
C     
C     CALLED FROM SVSTART, SVOUT, AND SVRMOV. 
C     INPUT:
C     REMOV = VECTOR OF MORTALITY OR REMOVAL TREES/ACRE
C     IST= 1 IF MORTALITY NEEDS TO BE TAKEN INTO ACCOUNT
C        = 0 IF IT SHOULD NOT BE TAKEN INTO ACCOUNT
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'SVDATA.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
C     WPP     - SUM OF REMOV PER PLOT 
C     PPP     - SUM OF PROB PER PLOT FROM FVS TREE LIST
C     IPLCNT  - NUMBER OF OBJECT TREES PER PLOT
C     CDBH    - CUM DBH*DBH PER PLOT FROM FVS TREE LIST
C     CDBHOB  - CUM DBH*DBH PER PLOT FROM SV OBJECT LIST
C     CPROB   - CUM PROB PER PLOT FROM SV OBJECT LIST
C     REMOV   - MORTALITY BY TREE RECORD (PASSED IN)
C
      REAL WPP(MAXPLT)
      REAL PPP(MAXPLT)
      INTEGER IPLCNT(MAXPLT)
      REAL CDBH(MAXPLT)
      REAL CDBHOB(MAXPLT)
      REAL CPROB(MAXPLT)
      REAL REMOV(MAXTRE)
      INTEGER IST,II,I,ICNT,ISVOBJ
      REAL TPPP,RMV,TCDBHO,TCDBH,TDIFF,DIFF
C
      TPPP = 0
      DO II=1, MAXPLT
         WPP(II) = 0.
         PPP(II) = 0.
         IPLCNT(II) = 0
         CDBH(II) = 0.
         CDBHOB(II) = 0.
         CPROB(II) = 0.
      ENDDO

      DO II=1,ITRN
         I = ITRE(II)
         IF (IST.EQ.1) THEN
            RMV = REMOV(II)
         ELSE
            RMV = 0.
         ENDIF
         WPP(I) = RMV + WPP(I)
         PPP(I) = PROB(II)-RMV + PPP(I)
         CDBH(I) =CDBH(I) +DBH(II)*DBH(II)*(PROB(II)-RMV)
      ENDDO

      ICNT= 0
      DO ISVOBJ = 1,NSVOBJ
         II = IS2F(ISVOBJ)
         IF(II.GT.0 .AND. IOBJTP(ISVOBJ).EQ.1) THEN
            I=ITRE(II)
            IF (IST.EQ.1) THEN
               RMV = REMOV(II)
            ELSE
               RMV = 0
            ENDIF
            CDBHOB(I)=CDBHOB(I) + DBH(II)*DBH(II)
            CPROB(I) = PROB(II)-RMV+CPROB(I)
            IPLCNT(I)=IPLCNT(I)+1
            ICNT=ICNT+1
         ENDIF
      ENDDO
      TCDBHO=0
      TCDBH=0
      TDIFF=0
      DO II=1, ISVINV
         IF (IPLCNT(II) .EQ. 0) THEN
            CDBHOB(II) = 0.
         ELSE
            CDBHOB(II) =CDBHOB(II)/FLOAT(IPLCNT(II))
         ENDIF
         TCDBHO = CDBHOB(II) +TCDBHO
         IF (PPP(II) .EQ. 0) THEN
            CDBH(II) = 0
         ELSE
            CDBH(II) = CDBH(II)/PPP(II)
         ENDIF
         TCDBH = CDBH(II)+TCDBH
         DIFF = CDBH(II)-CDBHOB(II)
         TDIFF=TDIFF+DIFF
         IF (IST.EQ.1) THEN
            RMV = REMOV(II)
         ELSE
            RMV = 0
         ENDIF
            WRITE (JOSTND,*) 'PLOT=',II,'WPP=',WPP(II),
     >           'PPP=',PPP(II),'IPLCNT=',IPLCNT(II),'D=',
     >           PPP(II)-IPLCNT(II),'CDBH=',CDBH(II),
     >           'CDBHOB=',CDBHOB(II),'D=',DIFF
            TPPP = PPP(II)+TPPP
      ENDDO
      WRITE (JOSTND,*) 'TPPP=',TPPP,'ICNT=',ICNT,' D=',TPPP-ICNT,
     >                 'TCDBH=', TCDBH,'TCDBHO=',TCDBHO,'TDIF=',TDIFF

      RETURN
      END





