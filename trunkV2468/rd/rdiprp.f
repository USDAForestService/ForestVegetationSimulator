      SUBROUTINE RDIPRP(PROPN,NOPLOT,PRAN)
      IMPLICIT NONE
C----------
C  **RDIPRP      LAST REVISION:  08/29/14
C----------
C
C  Purpose :
C     This subroutine calculates multipliers for each species for
C     distributing the user-defined number of infected trees in a
C     center.  The proportion of each species is defined by the
C     infection probability and the average time to death for the
C     species.
C
C  Called By :
C     RDSETP  (SUBROUTINE)  [ROOT DISEASE]
C
C  Calls :
C     RDSLP  (FUNCTION)   [ROOT DISEASE]
C
C  Arguments :
C     PROPN  -
C
C     NOPLOT -
C
C     PRAN   -
C
C  Local Variables :
C     <incomplete>
C
C  Common Block Variables Used :
C     <incomplete>
C
C  Revision History :
C  10/16/98 (RNH) 
C    MOdificaion to more accurately calculate the number of trees
C    in each species to be inititially infected with root disease
C    with RRINIT keyword
C  02-AUG-01 Lance R. David (FHTET)
C    Changed initialization control loop from MINRR,MAXRR to 1,ITOTRR.
C  08/29/14 Lance R. David (FMSC)
C    Added implicit none and declared variables.
C------------------------------------------------------------------
C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'

C.... Argument variable declarations.

C.... Local variable declarations.

      LOGICAL  YESSP(MAXSP), FULL(MAXSP)                 
      INTEGER  I, IDI, II, INUMSP(ITOTRR), KSP, NOPLOT, NUMFUL(ITOTRR)
      REAL     ADDON, AVGYTK, HABSP, OVER, PRAN(ITOTRR),
     &         PROPN(MAXTRE), RDSLP, RELVAL(MAXSP),
     &         SSTVAL(ITOTRR), TMPPRP(MAXSP), TOTINF(ITOTRR),
     &         TOTNUM, TOTREE(ITOTRR), TOTSP(MAXSP), TOTVAL(ITOTRR),
     &         TOTYTK(MAXSP), TREENO(MAXSP)
C
C     the array SSTVAL(MAXSP) is used in the calculation of initial 
C     root disease in each species (RNH NOV98)
C
C.... Initializations.

      AVGYTK = 0.0
      OVER = 0.0

      DO 99 IDI=1,ITOTRR
         TOTVAL(IDI) = 0.0
         TOTINF(IDI) = 0.0
         TOTREE(IDI) = 0.0
         INUMSP(IDI) = 0
         SSTVAL(IDI) = 0.0
         NUMFUL(IDI) = 0
   99 CONTINUE  

      DO 100 KSP = 1, MAXSP
         YESSP(KSP) = .FALSE.
         FULL(KSP) = .FALSE.
         TOTSP(KSP) = 0.0
         TOTYTK(KSP) = 0.0
         TREENO(KSP) = 0.0
         RELVAL(KSP) = 0.0
         TMPPRP(KSP) = 0.0
  100 CONTINUE              
  
C.... Total the number of trees in center by species (TOTSP).
C.... Figure out the "TOTAL YEARS TO KILL" trees so we can get an
C.... average for the species in the next set of loops.

      IDI = MAXRR

      DO 1500 I= 1, ITRN
         IF (((IDPLOT(I) .EQ. NOPLOT) .OR. (NOPLOT .EQ. 0)) .AND.
     &        (ISP(I) .NE. 0)) THEN
            KSP = ISP(I)
            IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))
C
C     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
C     If non-host species then IDI = 0, and loop should be skipped to
C     prevent array out of bounds error
C
      IF (IDI .LE. 0) GO TO 1500
C
            TOTNUM = PROB(I) * PAREA(IDI)
            TOTSP(KSP) = TOTSP(KSP) + TOTNUM

            HABSP = HABFAC(IRTSPC(KSP),IDI,IRHAB)
            YTKILL = RDSLP(DBH(I),XXINF,YYINF,NNINF)

            IF (YTKILL .GT. XMINKL(IDI)) THEN
               YTKILL = (YTKILL - XMINKL(IDI)) * HABSP *
     &                  RRPSWT(IRTSPC(KSP)) + XMINKL(IDI)
            ENDIF

            TOTYTK(KSP) = TOTYTK(KSP) + YTKILL * TOTNUM

            IF (.NOT. YESSP(KSP) .AND. TOTSP(KSP) .GT. 0.0)
     &         YESSP(KSP) = .TRUE.
         ENDIF
 1500 CONTINUE

C.... Calculate the average year to kill of a particular species
C.... (AVGYTK).  Use the AVGTYK and the probability of infection for
C.... that species to find a relative value for the species (for
C.... calculating relative proportions of species killed)

C.... **Note that we are currently only using the defined probability
C....   and not correcting it for the number of years in a cycle.
C....   That would give slightly different results, but the cycle length
C....   should not affect the initialization (I Think).
C....   (Sarah, march 20/95)

      IDI = MAXRR

      DO 1600 KSP = 1, MAXSP
         IF (TOTSP(KSP) .GT. 0.0) THEN

            IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))

C     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
C     If non-host species then IDI = 0, and loop should be skipped to
C     prevent array out of bounds error
C
      IF (IDI .LE. 0) GO TO 1600
C
            AVGYTK = TOTYTK(KSP) / TOTSP(KSP)
            RELVAL(KSP) = PNINF(IRTSPC(KSP),IDI) * AVGYTK

            TOTVAL(IDI) = TOTVAL(IDI) + RELVAL(KSP)

            TOTREE(IDI) = TOTREE(IDI) + TOTSP(KSP)
C
            IF (YESSP(KSP)) INUMSP(IDI) = INUMSP(IDI) + 1
            
      ENDIF
 1600 CONTINUE

C.... Calculate number to be infected in each species.
C.... TOTINF is the total number of trees to be infected in each
C.... disease type.  TREENO is the total number if trees to be infected
C.... by species, based on the relative likelihoods (calculated from
C.... YTK and PROB infection)
C.... PRAN is the proportion of infection in stand or plot.

      DO 1699 IDI=MINRR,MAXRR
         TOTINF(IDI) = PRAN(IDI) * TOTREE(IDI)
C++++++
        DO 1698 KSP= 1, MAXSP

C     The following calculation was nested in this loop to for the 
C     new initial ization method used in loop 1700
C     New array (RNH NOV98)
C
      IF (TOTVAL(IDI) .GT. 0.0)
     1 SSTVAL(IDI)= SSTVAL(IDI) + RELVAL(KSP)*TOTSP(KSP)/
     2                            TOTVAL(IDI)
C
 1698   CONTINUE
C++++++
C    
 1699 CONTINUE

      IDI = MAXRR

      DO 1700 KSP = 1, MAXSP
C
      IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))
C
C     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
C     If non-host species then IDI = 0, and loop should be skipped to
C     prevent array out of bounds error
C
      IF (IDI .LE. 0) GO TO 1700            
C
C     Commented out the following 2 lines and added a new formula to 
C     calculate the total number of trees infected per species
C     (RNH Nov 98)
C
C            IF (TOTSP(KSP) .GT. 0.0 .AND. TOTVAL(IDI) .GT. 0.0)
C     &      TREENO(KSP) = TOTINF(IDI) * (RELVAL(KSP) / TOTVAL(IDI))
C
       IF (TOTSP(KSP) .GT. 0.0 .AND. TOTVAL(IDI) .GT. 0.0 .AND.
     1    (SSTVAL(IDI) .GT. 0.0))
C
     2 TREENO(KSP) = TOTINF(IDI)*TOTSP(KSP)*RELVAL(KSP)/SSTVAL(IDI)/
     3 TOTVAL(IDI)
C
C
C
 1700 CONTINUE

C.... Turn number of trees to be infected of each species into a
C.... proportion of the available trees within the center. This
C.... proportion may be greater than one if there are few trees but
C.... the probability of being infected is high.  If greater than one,
C.... then set to one and increase the others (If not also one) by
C.... dividing the remainder equally.

 1750 CONTINUE

      DO 1900 IRRSP=MINRR,MAXRR
         IF (INUMSP(IRRSP) .LE. NUMFUL(IRRSP)) GOTO 1850
         ADDON = OVER / (INUMSP(IRRSP) - NUMFUL(IRRSP))
         OVER = 0.0

         IDI = IRRSP
         DO 1800 KSP = 1, MAXSP
            IF (IRRSP .LT. 3) IDI = IDITYP(IRTSPC(KSP))
            IF (IDI .NE. IRRSP) GOTO 1800

            IF (YESSP(KSP) .AND. .NOT. FULL(KSP)) THEN
               TREENO(KSP) = TREENO(KSP) + ADDON
               TMPPRP(KSP) = TREENO(KSP) / TOTSP(KSP)

               IF (TMPPRP(KSP) .GT. 1.0) THEN
                  TMPPRP(KSP) = 1.0
                  FULL(KSP) = .TRUE.
                  NUMFUL(IRRSP) = NUMFUL(IRRSP) + 1

                  OVER = OVER + TREENO(KSP) - TOTSP(KSP)
               ENDIF
            ENDIF
 1800    CONTINUE

         IF (NUMFUL(IRRSP) .EQ. INUMSP(IRRSP)) GOTO 1850
         IF (OVER .GT. 0.0 .AND. NUMFUL(IRRSP) .LT. INUMSP(IRRSP))
     &       GOTO 1750

 1850    CONTINUE
         OVER = 0.0
 1900 CONTINUE

      DO 2000 II = 1,ITRN
         IF ((IDPLOT(II) .EQ. NOPLOT) .OR. (NOPLOT .EQ. 0)) THEN
            KSP = ISP(II)
            PROPN(II) = TMPPRP(KSP)
         ENDIF
 2000 CONTINUE

      RETURN
      END
