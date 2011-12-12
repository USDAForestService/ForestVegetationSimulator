      SUBROUTINE FMKILL(ICALL)
      IMPLICIT NONE
C----------
C  **FMKILL  FM-DATE OF LAST REVISION:  03/15/05
C----------
C     SINGLE-STAND VERSION
C
C     CONVERT THE MORTALITY ESTIMATES PRODUCED BY THE FIRE
C     MODEL INTO MODIFIED PROGNOSIS MODEL RATES.
C     Add all newly-killed trees to the snag list.
C     NOTE:  There is no information on cause-of-death at this time.
C
C     IF ICALL = 1 THEN CALLED JUST TO ENSURE FIRE MORTALITY
C                  IS ADDED TO WK2
C     IF ICALL = 2 THEN CALLED TO GET ALL EXTRA WK2 MORTALITY
C                  INTO THE SNAG LIST.
C
C     CALLED FROM:  PPMAIN [PPE]
C                   GRADD  [SINGLE-STAND]
C     CALLS:   FMSSEE
C              FMSADD
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'FMCOM.F77'
C
COMMONS
C
      INTEGER I, YEAR, ICALL, ISHAG
      LOGICAL DEBUG
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMKILL',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,LFMON
    7 FORMAT(' ENTERING FMKILL CYCLE = ',I2,' LFMON=',L2)

C     IF THE FIRE MODEL EXTENSION IS NOT ACTIVE, THEN RETURN

      IF (.NOT. LFMON) RETURN

      IF (ICALL .EQ. 1) THEN
C
C        Loop over all the trees to see if the WK(2) array needs to be
C        updated. Also activate sprouting and adjust crown ratios.
C
C        Set sprout age.  If there was a simulated fire this cycle, use
C        its year to determine sprout age.  If not, use the year of the
C        pileburn this cycle.  It is done this way because if both a
C        simulated fire and a pileburn treatment occur in the same
C        cycle, the simulated fire was probably the most severe.

         IF ((IY(ICYC+1) - BURNYR) .LE. IFINT) THEN
           ISHAG = IY(ICYC+1)-BURNYR
         ELSE
           ISHAG = IY(ICYC+1)-PBURNYR
         ENDIF

         DO I = 1, ITRN

            IF (DEBUG) WRITE(JOSTND,10) I,PROB(I),FIRKIL(I),WK2(I),
     >                               FMICR(I),ICR(I)
   10       FORMAT (' IN FMKILL(1), I=',I4,' PROB=',F10.4,' FIRKIL=',
     >        F10.4,' WK2=',F10.4,' FMICR=',I3,' ICR=',I3)

C           MAKE SURE WE ARE NOT KILL MORE TREES THAN WE HAVE!!!

            IF (FIRKIL(I).GT.PROB(I)) FIRKIL(I)=PROB(I)
C
C           ADD KILLED TREES TO LIST FOR REGENERATION SPROUTS.
C
            IF (FIRKIL(I) .GT. 0.00001) THEN
              CALL ESTUMP (ISP(I),DBH(I),FIRKIL(I),ITRE(I),ISHAG)
            ENDIF

c           If the fire model predicts higher mortality,
c           then the FVS values need to be adjusted.

            IF (FIRKIL(I) .GT. WK2(I)) WK2(I) = FIRKIL(I)

C           If the fire model has changed the crown ratios, give the
C           new ones to FVS, and make them negative to tell FVS not
C           to calculate new ones next cycle.
C
            IF (FMICR(I) .LT. 1) FMICR(I) = 1
            IF (FMICR(I) .LT. IABS(ICR(I))) THEN
               ICR(I) = -FMICR(I)
               IF (DEBUG) WRITE(JOSTND,20) I,FMICR(I),ICR(I)
   20          FORMAT (' IN FMKILL CROWN CHANGED, I=',I4,' FMICR=',I3,
     >                 ' ICR=',I3)
            ENDIF
         ENDDO

      ELSEIF (ICALL .EQ. 2) THEN
C
C        Now check WK2 again. If any of its values are higher than
C        FIRKIL, then the mortality needs to be added to the snags
C
         DO I = 1, ITRN

            IF (DEBUG) WRITE(JOSTND,30) I,PROB(I),FIRKIL(I),WK2(I),
     >                                  FMICR(I),ICR(I)
   30       FORMAT (' IN FMKILL(2), I=',I4,' PROB=',F10.4,' FIRKIL=',
     >               F10.4,' WK2=',F10.4,' FMICR=',I3,' ICR=',I3)

c           If the fire model predicts higher mortality, then the FVS
c           values were adjusted in the previous call. If WK2 is now
c           higher than the FFE values (FIRKIL), then we need to pass
c           that information to the snag model.
c           We do nothing if FIRKIL=WK2.

            IF (FIRKIL(I) .LT. WK2(I)) THEN

C              Store the FVS mortality in the snag mgmt routines for addition to
C              the snag pools.  This is only done for tree records where the
C              background rate is higher than the Fire mortality rate.  Fire
C              killed trees are placed in the snag pools via calls inside of
C              FMEFFS and FMTRET.  R&C 07/11/96
C              Subtract off the fire-killed trees first, since they have already
C              been added (SB 2/97)

               CALL FMSSEE (I,ISP(I),DBH(I),HT(I),WK2(I)-FIRKIL(I),
     >                      1,DEBUG,JOSTND)

            ENDIF

         ENDDO
C        end of tree loop.

C        Now group new snags into species-dbh-ht records,
C        and collect the canopy material of non-fire-killed snags.

Cppe     YEAR = MIY(MICYC) - 1
Csng     YEAR =  IY(ICYC+1) - 1
         YEAR =  IY(ICYC+1) - 1

         CALL FMSADD (YEAR,4)

C        Now you can zero out the fire model mortality array

         DO I = 1, ITRN
            FIRKIL(I) = 0.0
         ENDDO

Cppe     PUT THE FIRE MODEL PARAMETERS BACK IN THE DA FILE
Cppe     CALL SPARLS (ISNKEY(ISTND, 5), ISNKEY(ISTND, 6))
Cppe     CALL FMPSTD(ISTND)

C        SET FLAG TO MARK COMPLETION OF THE FIRST MASTER CYCLE.

         IF (LFMON2) LFMON2 = .FALSE.

      ENDIF

      RETURN
      END

