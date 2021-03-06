      SUBROUTINE FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX1,HTMAX1,
     &                  HTMAX2,DEBUG)
      IMPLICIT NONE
C----------
C CI $Id$
C----------
C  THIS ROUTINE SETS EFFECTIVE TREE AGE BASED ON INPUT VARIABLE(S)
C  SUCH AS TREE HEIGHT.  IN CI, AGE IS CALCULATED FOR ASPEN ONLY
C  SUCH AS TREE HEIGHT.
C  CALLED FROM **COMCUP
C  CALLED FROM **CRATET
C  CALLED FROM **REGENT**
C  CALLED FROM **HTGF**
C----------
C  COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C  COMMONS
C----------
C  LOCAL VARIABLE DEFINITIONS:
C----------
C  INCRNG = HAS A VALUE OF 1 WHEN THE SITE CURVE HAS BEEN MONOTONICALLY
C           INCREASING IN PREVIOUS ITERATIONS.
C   OLDHG = HEIGHT GUESS FROM PREVIOUS ITERATION.
C----------
      LOGICAL DEBUG
      INTEGER I,ISPC,INCRNG
      REAL D1,D2,H,SITAGE,SITHT,AGMAX1,HTMAX1,HTMAX2
      REAL TOLER,SINDX,HGUESS,AG,DIFF
      REAL RDANUW,OLDHG
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      RDANUW = D1
      RDANUW = D2
      RDANUW = HTMAX2
C----------
      SELECT CASE (ISPC)
C
C  COMPUTE AGE FOR ASPEN. EQUATION FORMULATED BY
C  WAYNE SHEPPERD, ROCKY MTN FOREST EXP STATION, FT COLLINS, CO.
C
      CASE(13)
        SITAGE = (H*2.54*12.0/26.9825)**(1.0/1.1752)
C
C  CURLLEAF MOUNTAIN-MAHOGANY
C
      CASE(15)
        AGMAX1 = 50.
        HTMAX1 = 20.
        TOLER=2.0
        SINDX = SITEAR(ISPC)
C
C  CRATET CALLS FINDAG AT THE BEGINING OF THE SIMULATION TO
C  CALCULATE THE AGE OF INCOMMING TREES.  AT THIS POINT ABIRTH(I)=0.
C  THE AGE OF INCOMMING TREES HAVING H>=HMAX IS CALCULATED BY
C  ASSUMEING A GROWTH RATE OF 0.10FT/YEAR FOR THE INTERVAL H-HMAX.
C  TREES REACHING HMAX DURING THE SIMULATION ARE IDENTIFIED IN HTGF.
C
        IF(H .GE. HTMAX1) THEN
          SITAGE = AGMAX1 + (H - HTMAX1)/0.10
          SITHT = H
          IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,AGMAX1,H,HTMAX1= ',I,ISPC,
     $    AGMAX1,H,HTMAX1
          GO TO 29
        ENDIF
C
        AG=2.0
C
      INCRNG = 0
      HGUESS = 0.
   75 CONTINUE
      OLDHG = HGUESS
C
        HGUESS = (SINDX - 4.5) / ( 0.6192 - 5.3394/(SINDX - 4.5)
     &   + 240.29 * AG**(-1.4) +(3368.9/(SINDX - 4.5))*AG**(-1.4))
        HGUESS = HGUESS + 4.5
        IF(DEBUG)WRITE(JOSTND,91200)I,ISPC,AG,HGUESS,H
91200   FORMAT(' FINDAG,I,ISPC,AGE,HGUESS,H ',2I5,3F10.2)
C----------
C  AVOID NEGATIVE PREDICTED HEIGHTS AT SMALL AGES FROM SOME SI CURVES
C  GED 4/20/18
C----------
      IF(HGUESS .LT. 1.)GO TO 175
C
        DIFF=ABS(HGUESS-H)
        IF(DIFF .LE. TOLER .OR. H .LT. HGUESS)THEN
          SITAGE = AG
          SITHT = HGUESS
          IF(DEBUG)THEN
            WRITE(JOSTND, *)' DIFF,TOLER,H,HGUESS,AG,SITAGE,SITHT= ',
     &      DIFF,TOLER,H,HGUESS,AG,SITAGE,SITHT
          ENDIF
          GO TO 29
        END IF
C----------
C  SOME SITE CURVES DECREASE AT THE START BEFORE INCREASING. IF DECREASING,
C  KEEP GOING; IF SITE CURVE WAS INCREASING, BUT NOW HAS FLATTENED OFF, STOP 
C  THE ITERATION GED 04/19/18
C----------
      DIFF = (HGUESS-OLDHG)
      IF(OLDHG.NE.0.0 .AND. DIFF.GE.0.05) INCRNG=1
      IF(DEBUG)WRITE(JOSTND,*)' IN FINDAG OLDHG,DIFF,INCRNG= ',
     &OLDHG,DIFF,INCRNG 
      IF(INCRNG.EQ.1 .AND. DIFF .LT. 0.05)THEN
        SITAGE = AG
        SITHT = HGUESS
        IF(DEBUG)THEN
          WRITE(JOSTND, *)' SITE CURVE FLAT OLDHG,AG,HGUESS,SITAGE,',
     &    'SITHT= ',OLDHG,AG,HGUESS,SITAGE,SITHT
        ENDIF
        GO TO 29
      END IF
C
  175 CONTINUE
      AG = AG + 2.
C
      IF(AG .GT. AGMAX1) THEN
C----------
C  H IS TOO GREAT AND MAX AGE IS EXCEEDED
C----------
        SITAGE = AGMAX1
        SITHT = H
        GO TO 29
      ELSE
        GO TO 75
      ENDIF
C
C  REMAINING SPECIES FOR WHICH AGE DOESN'T MATTER.
C
      CASE DEFAULT
        SITAGE = 0.
        SITHT = H
C
      END SELECT
C
   29 CONTINUE
      IF(DEBUG)WRITE(JOSTND,50)ISPC,I,SITAGE,SITHT
   50 FORMAT(' LEAVING SUBROUTINE FINDAG  ISPC,I,SITAGE,SITHT =',
     &2I5,2F10.3)
C
      RETURN
      END
