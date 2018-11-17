      SUBROUTINE PTBAL
      IMPLICIT NONE
C----------
C VBASE $Id: ptbal.f 2438 2018-07-05 16:54:21Z gedixon $
C----------
C     
C     COMPUTES THE POINT BASAL AREA IN LARGER TREES.  THIS IS A
C     TREE ATTRIBUTE.  CALLED FROM DENSE.
C     
C     N.L.CROOKSTON, INT-MOSCOW, MARCH 1991.
C----------
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
      INCLUDE 'PLOT.F77'
C     
C     
      INCLUDE 'VARCOM.F77'
C     
C     
COMMONS
C     
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      LOGICAL DEBUG
C
      INTEGER I,II,IP,N,NP
C
      INTEGER IPNDX(MAXPLT)
C
      REAL XBALT
C
      EQUIVALENCE (WK6,IPNDX)
C     
C-----------
C     CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'PTBAL',5,ICYC)
      IF (DEBUG) WRITE (JOSTND,1) ICYC,ITRN,VARACD
    1 FORMAT(' ENTERING SUBROUTINE PTBAL CYCLE=',I5,
     >     '; ITRN=',I5,'; VARACD=',A2)
C
      SELECT CASE (VARACD)
C----------
C  EASTERN TWIGS AND CANADIAN ONTARIO VARIANTS
C  POINT BASAL AREA NOT USED; SET ARRAYS TO ZERO.
C----------
      CASE ('CS','LS','NE','ON')
        PTBALT = 0.
        PTBAA = 0.
        IF(DEBUG) WRITE(JOSTND,10)
   10   FORMAT(' EASTERN TWIGS VARIANT, PBA VALUES ZERO, LEAVING PTBAL') 
        RETURN
C----------
C  WESTERN, SOUTHERN, AND CANADIAN BRITISH COLUMBIA VARIANTS
C----------
      CASE DEFAULT
        IF (ITRN.LE.0) THEN
          PTBALT = 0.
          PTBAA = 0.
          IF(DEBUG) WRITE(JOSTND,20)
   20     FORMAT(' WESTERN/SOUTHERN VARIANT, NO TREES, PBA VALUES ',
     &    'ZERO, LEAVING PTBAL') 
          RETURN
        ENDIF
C----------
C  FIND THE MAXIMUM VALUE OF ITRE...THAT WILL BE TAKEN AS THE
C  ACTUAL POINT COUNT.
C----------
        NP=-1
        DO I=1,ITRN
          II=IND(I)
          IF (ITRE(II).GT.NP) NP=ITRE(II)
        ENDDO
        IF (DEBUG) WRITE (JOSTND,30) NP,IREC2,PI,GROSPC
   30   FORMAT(' ENTERING SUBROUTINE PTBAL NP=',I4,
     >     '; IREC2=',I5,'; PI=',F6.1,'; GROSPC=',F8.3)
C----------
C  IF THERE IS ONLY ONE POINT, THE PROBLEM IS SIMPLE...SAVE IT
C  FOR A SPECIAL CASE.
C----------
        IF (NP.GT.1) THEN
C----------
C  BUILD THE INDICES TO POINTS.  ZERO OUT IPNDX, AND LOAD WITH
C  THE NUMBER OF RECORDS ON EACH POINT.
C----------
          DO IP=1,NP
            IPNDX(IP)=0
          ENDDO
          DO I=1,ITRN
            II=IND(I)
            IPNDX(ITRE(II))=IPNDX(ITRE(II))+1
          ENDDO
C----------
C  RUN A CUMULATIVE.  THESE POINTERS "RESERVE" ROOM IN IND2.
C----------
          DO IP=2,NP
            IPNDX(IP)=IPNDX(IP)+IPNDX(IP-1)
          ENDDO
C----------
C  BUILD A LIST OF POINTERS IN SORTED ORDER. THEY ARE SORTED
C  BECAUSE IND IS SORTED OVER DBH.
C----------
          DO I=ITRN,1,-1
            II=IND(I)
            IND2(IPNDX(ITRE(II)))=II
            IPNDX(ITRE(II))=IPNDX(ITRE(II))-1
          ENDDO
        ELSE
C----------
C  BUILD THE LIST BY COPYING THE IND INTO THE IND2 LISTS.
C----------
          DO I=1,ITRN
            IND2(I)=IND(I)
          ENDDO
          IPNDX(1)=0
        ENDIF
C----------
C  COMPUTE THE BASAL AREA IN LARGER TREES WITHIN EACH POINT.
C----------
        DO IP=1,NP
          IF (IP.EQ.NP) THEN
            N=ITRN-IPNDX(IP)
          ELSE
            N=IPNDX(IP+1)-IPNDX(IP)
          ENDIF
C----------
C  LOOP OVER TREES WITHIN POINTS.
C----------
          XBALT=0.0
          DO I=IPNDX(IP)+1,IPNDX(IP)+N
            PTBALT(IND2(I))=XBALT
C----------
C  WK5 IS COMPUTED IN DENSE AS DBH(I)*DBH(I)*PROB(I)
C----------
            XBALT=XBALT+(WK5(IND2(I))*.005454154*PI/GROSPC)
          ENDDO
C----------
C  XBALT IS NOW THE POINT ESTIMATE OF STAND BASAL AREA.
C----------
          PTBAA(IP)=XBALT
        ENDDO
C----------
C  WRITE DEBUG, IF REQUESTED.
C----------
        IF (DEBUG) THEN
          DO II=1,ITRN
            I=IND2(II)
            WRITE (JOSTND,40) I,ITRE(I),DBH(I),WK5(I),PTBALT(I),PROB(I),
     >           PTBAA(ITRE(I))
   40       FORMAT (' IN PTBAL: I,ITRE,DBH,WK5,PTBALT,PROB,PTBAA=',
     >           2I5,5F10.3)
          ENDDO
        ENDIF
        IF(DEBUG) WRITE(JOSTND,50)
   50   FORMAT(' WESTERN/SOUTHERN VARIANT, PBA VALUES SET, LEAVING ',
     &         'PTBAL.') 
C
      END SELECT
C
      RETURN
      END
