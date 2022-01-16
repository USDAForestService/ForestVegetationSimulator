      SUBROUTINE BRANN(SEL)
      IMPLICIT NONE
C----------
C WPBR $Id$
C----------
C**********************************************************************
C  **BRANN        DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C
C  This random number generator was modfied from the algorithm
C  found in the IMSL Library Vol. 1 Edition 4 to 5,
C  December 1, 1975, and can be found in the following references:
C
C     Lewis, P.A.W., Goodman, A.S., and Miller, J.M. Psuedo-Random
C     Number Generator for the System/360, IBM Systems Juornal, No. 2,
C     1969.
C
C     Learmouth, G.P. and Lewis, P.A.W., Naval Postgraduate School
C     Random Number Generator Package LLRANDOM, NPS55LW73061A, Naval
C     Postgraduate School, Monterey, California, June, 1973.
C
C     Learmouth, G.P., and Lewis, P.A.W., Statistical Tests of Some
C     Widely Used and Recently Proposed Uniform Random Number
C     Generators. NPS55LW73111A, Naval Postgraduate School, Monterey
C     California, November, 1973.
C
C  The code was written by N.L. Crookston, Forestry Sciences Lab,
C  October 1982, from the algorithm published in the IMSL manual and
C  tested by comparing the first 10000 draws from this version and
C  and the generator 'GGUBS' in the 1981 edition of IMSL.
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  01-OCT-1982
C     Original source date noted.
C  08-MAY-2006 Lance R. David (FHTET)
C     Changed random number seed variable names to unique variables
C     BRS0, BRS1, BRSS.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations.

      REAL SEED, SEL
      LOGICAL LSET

      BRS1=DMOD(16807D0*BRS0,2147483647D0)
      SEL=BRS1/2147483648D0
      BRS0=BRS1
      GO TO 100

C.... You may reseed the generator by supplying an odd number of type
C.... real with LSET=TRUE; if LSET=FALSE, a call to RANSED will
C.... cause the random number generator to start over.

      ENTRY BRNSED(LSET,SEED)
      IF(.NOT.LSET) THEN
         SEED=BRSS
         BRS0=SEED
         GO TO 100
      ENDIF

      IF(AMOD(SEED,2.0).EQ.0.) SEED=SEED+1
      BRSS=SEED
      BRS0=SEED

C.... Common return.

  100 CONTINUE
      RETURN
      END
