      SUBROUTINE HTCALC(I,ISPC,XSITE,AG,HGUESS,POTHTG)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C   THIS SUBROUTINE COMPUTES THE HEIGHT INCREMENT GIVEN TREE-SPECIFIC
C   INDEPENDENT VARIABLES SUCH AS DBH, DG AGE ...
C   CALLED FROM **HTGF**
C----------
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
COMMONS
C------------
      LOGICAL DEBUG
C
      INTEGER ISPC,I
C
      REAL POTHTG,HGUESS,AG,XSITE,B3,C0,C1,C2,C3
      REAL HB,PH,TERM1
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'HTCALC',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE HTCALC  CYCLE =',I5)
C
      POTHTG = 0.
      HGUESS = 0.
C
      SELECT CASE(ISPC)
C
C----------
C  WESTERN HEMLOCK
C---------
        CASE(5)
          IF((HT(I) - 4.5) .LE. 0.0)GO TO 900
          B3 = 3.316557 * XSITE**(-0.2930032)
          C0 = (1.0 - EXP(-10.0*0.01046931)) * 6.421396**(1.0/B3)
          C1 = 0.7642128 / B3
          C2 = EXP(-10.0*0.01046931)
          C3 = 1.0 / B3
          HB = HT(I) - 4.5
          PH = 4.5 + (C0*XSITE**C1 + C2*HB**C3)**(1.0/C3) - HB
C----------
C TERM1 IS BORROWED FROM SPRUCE LOGIC FOR NOW TO GET SOME
C CROWN SENSITIVITY INTO THE HEMLOCK EQUATION.
C----------
          TERM1 = (1.0 - EXP( - 0.03563*ICR(I)) ** 0.907878)
C----------
C     NEW WH LOG-LINEAR HTG EQN FROM BILL FARR 10-22-87
C----------
          POTHTG = EXP(2.18643 + 0.2059*ALOG(DG(I)) - 7.84117E-5*(HT(I)
     &    *HT(I)) + 9.6528E-3*XSITE + 0.54334*ALOG(DBH(I)) 
     &    - 0.35425*ALOG(HT(I)))
          POTHTG=POTHTG*TERM1
          IF(POTHTG .GT. PH)POTHTG=PH
          IF(DEBUG)WRITE(JOSTND,*)
     &      ' IN HTCALC WH I,ICR,DG,HT,XSITE,DBH,PH= ',
     &      ICR(I),DG(I),HT(I),XSITE,DBH(I),PH
C
C----------
C  RED ALDER
C----------
        CASE(10)
          HGUESS = XSITE
     &           + (59.5864 + 0.7953*XSITE)*
     &             (1.0-EXP((0.00194 - 0.0007403*XSITE)*AG))**0.9198
     &           - (59.5864 + 0.7953*XSITE)*
     &             (1.0-EXP((0.00194 - 0.0007403*XSITE)*20.0))**0.9198
          IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC RA XSITE,AG,HGUESS= ',
     &    XSITE,AG,HGUESS     
C
C----------
C  BLACK COTTONWOOD
C---------
        CASE(11)
          HGUESS = (XSITE - 4.5) / ( 0.6192 - 5.3394/(XSITE - 4.5)
     &           + 240.29*AG**(-1.4) + (3368.9/(XSITE-4.5))*AG**(-1.4))
          HGUESS = HGUESS + 4.5
C
          IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC BC XSITE,AG,HGUESS= ',
     &    XSITE,AG,HGUESS     
C----------
C  ALL SPECIES OTHER THAN RED ALDER AND COTTONWOOD.
C----------
        CASE(1:4,6:9,12,13)
          IF((HT(I) - 4.5) .LE. 0.0)GO TO 900
C
          IF(DEBUG)WRITE(JOSTND,9050)I,ISP(I),DBH(I),HT(I),ICR(I),
     &    XSITE,DG(I)
 9050     FORMAT('IN HTCALC 9050 I,ISP,DBH,HT,ICR,AVH,XSITE,DG=',
     &    2I5,2F10.2,I5,5F8.3)
C----------
C SPRUCE HEIGHT EQUATION APPLIED TO ALL SPECIES EXCEPT WH, RA, AND BC.
C----------
          B3 = 2.744017 * XSITE**(-0.2095425)
          C0 = (1.0 - EXP(-10.0 * 0.01630621)) * 3.380276**(1.0/B3)
          C1 = 0.8683028 / B3
          C2 = EXP(-10.0 * 0.01630621)
          C3 = 1.0 / B3
          HB = HT(I) - 4.5
          PH = 4.5 + (C0*XSITE**C1 + C2*HB**C3)**(1.0/C3) - HB
          TERM1 = (1.0 - EXP( - 0.03563*ICR(I)) ** 0.907878)
C----------
C     SPRUCE LOG-LINEAR HEIGHT EQUATION FROM FARR 10-22-87
C----------
          POTHTG = EXP(1.5163 + 0.1429*ALOG(DG(I)) - 6.04687E-5*(HT(I)
     &    *HT(I)) + 0.0103*XSITE 
     &    + 0.20358*ALOG(90.00 ) + 0.44146*ALOG(DBH(I))
     &    - 0.36662*ALOG(HT(I)))
          POTHTG=POTHTG*TERM1
C----------
C HT GROWTH CORRECTION FOR RC AND YC FROM BILL FARR (PNW JUNEAU)
C JULY 9, 1987.  PUBLISHED BC STUFF INDICATES RC HEIGHT GROWTH SHOULD
C BE ABOUT 75 PERCENT OF SS HEIGHT GROWTH.
C----------
          IF(ISP(I).EQ.2 .OR. ISP(I).EQ.6) THEN
            POTHTG = POTHTG *
     &      (0.84875-0.03039*DBH(I)+0.00076*DBH(I)*DBH(I)+0.00313*XSITE)
          ENDIF
C
          IF(DEBUG)WRITE(JOSTND,110) B3,POTHTG
  110     FORMAT('IN HTCALC 110 B3,POTHTG=',5F9.5)
C
          IF(POTHTG .GT. PH)POTHTG=PH
C
C----------
C  SPACE FOR OTHER SPECIES
C---------
        CASE DEFAULT
          POTHTG = 0.
          HGUESS = 0.
C
      END SELECT
C
  900 CONTINUE
C
      RETURN
      END
