      SUBROUTINE DMNDMR
      IMPLICIT NONE
C----------
C CANADA-NEWMIST $Id$
C----------
C **DMNDMR -- NISI  Date of last revision: 04/06/04
C----------------------------------------------------------------------
C Purpose:
C   The most common measure of infection status is Hawksworth's DMR.
C This routine transforms the projected infection levels from each
C crown third into the 0-6 Hawksworth scale. From the life history
C pools there are two components that contribute to the DMR: the
C ACTIVE (flower producing) and suprsd (formerly flower producing,
C now quiescent or suppressed). By this point, fecundity scalings
C (DMITUN, etc.) have already happened, and the pools represent DMR
C density for the crown the infection resdides in. A random process
C is used to determine what to do with the fractional parts of
C infection. For example, if the sum of the ACTIVE+SUPRSD pools is
C 1.71 in the lower crown third, there is a 71% chance of the crown
C third being given a 2 rating, and a 29% chance of being rated a 1.
C The crown thirds are then added up to give the tree rating.
C----------------------------------------------------------------------
C
C Called by:
C
C     DMTREG
C
C Other routines called:
C
C     DMRANN
C
C Argument list definitions:
C
C     [none]
C
C Local variable definitions:
C
C     INTEGER   i         Loop counter for treelist records.
C     INTEGER   j         Loop counter for crown thirds.
C     INTEGER   k         Incremental counter for DMR in each third.
C     REAL      x         Sum of the ACTIVE and SUPRSD pools.
C     REAL      Frac      Fractional remainder
C     REAL      RND       Uniform random number.
C
C Common block variables and parameters:
C
C     ITRN      CONTRL
C     DMRATE    DMCOM
C     CRTHRD    DMCOM
C     DMINF     DMCOM
C     ACTIVE    DMCOM
C     SUPRSD    DMCOM
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'DMCOM.F77'

      INTEGER   i,j,k,L
      REAL      x,Frac,RND

C WALK THROUGH TREELIST AND ASSIGN NEW DMR BASED ON RANDOM TRUNCATION OF
C PROJECTED DMR WITHIN EACH CROWN THIRD.

      DO I = 1,ITRN
        k = 0
        DMRATE(i) = 0
        DO J = 1,CRTHRD

          x = DMINF(i,j,ACTIVE) + DMINF(i,j,SUPRSD)
          DO L = 1,MAXBC
            x = x + DMINF_BC(i,j,ACTIVE,L) + DMINF_BC(i,j,SUPRSD,L)
          ENDDO
          x = x + DMINF(i,j,DEAD_BC)

          IF (x .GT. 2.0) THEN
            k = 2
          ELSE
            k = INT(x)
            Frac = x - FLOAT(k)
            CALL DMRANN(RND)
            IF (RND .LE. Frac) k = k + 1
          END IF
          DMRATE(i) = DMRATE(i) + k
        ENDDO
      ENDDO

      RETURN
      END
