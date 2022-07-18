      SUBROUTINE FMSDIT
      IMPLICIT NONE
C----------
C FIRE-BASE $Id$
C----------
C
C     PART OF THE FIRE MODEL EXTENSION. THIS ROUTINE IS ENTERED
C     AT THE START OF EACH CYCLE. VALUES FOR SCCF, AND FIRKIL
C     ARE ZEROED HERE, 
C
C     Called from GRINCR
C
C  Local Variable Definitions:
C     CYCLEN: length in years of the current FVS cycle
C     NEWBOT: the height of the bottom of the crown in the current cycle
C     OLDBOT: the height of the bottom of the crown in the previous cycle
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'

      INTEGER I, J
      REAL    CYCLEN, NEWBOT, OLDBOT
      LOGICAL DEBUG
      REAL    TONREM, X

C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMSDIT',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,LFMON
    7 FORMAT(' ENTERING ROUTINE FMSDIT CYCLE = ',I2,' LFMON=',L2)

C     CHECK TO SEE IF THE FIRE MODEL IS ACTIVE

      IF (.NOT. LFMON) RETURN

C     SET IFMYR1 TO -1 TO SIGNAL THAT THE FIRE SIMULATION
C     HAS NOT STARTED...THEN SET SOME FIRE MODEL EVENT
C     MONITOR VARIABLES TO UNSET.

      IFMYR1=-1

      CALL EVUST4(22)  ! CROWNIDX
      CALL EVUST4(26)  ! CRBASEHT
      CALL EVUST4(27)  ! TORCHIDX
      CALL EVUST4(28)  ! CRBULKDN

C
C     SET INITIAL VALUES TO -1 (SEE **EVTSTV** FOR FULL LIST)
C     421 MINSOIL   PERCENTAGE OF MINERAL SOIL EXPOSURE (FM)
C     423 FIREYEAR  CALENDAR YEAR OF LAST FIRE; -1 OTHERWISE (FM)
C     420 FIRE      = 1 IF THERE WAS A FIRE THE PREVIOUS CYCLE;
C                   = 0 OTHERWISE (FM)
C
      IF (ICYC .EQ. 1) THEN
        CALL EVSET4(21, -1.0)
        CALL EVSET4(23, FLOAT(BURNYR))
        CALL EVSET4(20, 0.0)
      ENDIF

      FMKOD=KODTYP

      FMSLOP   = SLOPE
      SCCF = 0.0
      TONREM = 0.0

      CYCLEN = FLOAT( ( IY(ICYC+1)) - IY(ICYC) )

C     If we are on the first cycle, then the old crown is not
C     known for the tree records.

      DO I=1,ITRN
        FMPROB(I) = PROB(I)
        FMICR(I)  = ICR(I)
      ENDDO
      DO I=1,MAXTRE
        FIRKIL(I) = 0.0
      ENDDO
C
      TONRMS=0.0
      TONRMH=0.0
      TONRMC=0.0
C
      IF (ICYC.GT.1) THEN
        DO I = 1, ITRN

C       Also calculate the annual amount of old crown material to fall
C       as a result of crown lifting in the current FVS cycle.  See
C       documentation of FMOLDC for further explanation.

          OLDBOT = OLDHT(I) - OLDCRL(I)
          NEWBOT = HT(I) - (HT(I) * FLOAT(ICR(I)) / 100.0)

          IF ((OLDCRL(I) .GT. 0.001).AND.(NEWBOT-OLDBOT.GT.0.0)) THEN
            X = ( (NEWBOT-OLDBOT) / OLDCRL(I) ) / CYCLEN
            DO J=0,5
              OLDCRW(I,J) = X * OLDCRW(I,J)
            ENDDO
          ELSE
            DO J=0,5
              OLDCRW(I,J) = 0.0
            ENDDO
          ENDIF

         ENDDO
      ENDIF

C     Calculate the weight of crown components, which several FM
C     routines will want. This only needs to be done once each
C     cycle because it depends mostly on ht and dbh.

      CALL FMCROW

C     If this is the first year of the simulation, add in the
C     snags that are picked up during the inventory.  Set the year
C     of death equal to the mortality measurement period.  These
C     trees include "OLD" dead and "RECENT" mortlity.

      IF (LFMON2) CALL FMSADD (IY(1)-IFIX(FINTM),3)
 
      RETURN
      END

