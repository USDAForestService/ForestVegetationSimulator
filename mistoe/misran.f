      SUBROUTINE MISRAN(IARRAY,ISIZE)
***********************************************************************
*  **MISRAN--MS  Date of last revision:  06/16/00
*----------------------------------------------------------------------
*  Purpose:
*     Randomly inserts integers from 1 to ISIZE into the first ISIZE
*  slots of IARRAY (which has a maximum dimension of MAXTRE).
*
*  Algorithm was taken from "Numerical Recipes: The Art of Scientific
*  Computing"; Press, Flannery, Teukolsky, and Vetterling - a portable
*  integer random number generator bewteen given bounds, pp.196-198.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     IARRAY: (IO) Array containing random integers between 1 and ISIZE.
*     ISIZE:  (I)  Range of integers to randomize.
*
*  Local variable definitions:
*     DEBUG:  Logical flag to turn debug on or off.
*     I:      Loop counter.
*     IA:     The multiplier (constant for this random # generator).
*     IC:     The increment (constant for this random # generator).
*     IM:     The modulus (constant for this random # generator).
*     IRAN:   Integer representation of random number.
*     JRAN:   Initialized to a seed value between 0 and IM-1; used to
*                seed the equation each time through the loop.
*     TARRAY: Temporary array to keep track of integers already used.
*
*  Common block variables and parameters:
*     ICYC:   From CONTRL; cycle index number.
*     JOSTND: From CONTRL; unit number of stand output.
*     MAXTRE: From PRGPRM; maximum number tree records.
*
***********************************************************************
      IMPLICIT NONE

      INTEGER IM,IA,IC

C.... Parameter statements.

C.... These values listed in the above-mentioned documentation as
C.... "good" choices for constants to obtain pseudo-randomness and
C.... to avoid machine overflow.

      PARAMETER (IM=233280)
      PARAMETER (IA=1861)
      PARAMETER (IC=49297)

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'MISCOM.F77'

C.... Variable declarations.

      LOGICAL DEBUG
      INTEGER IARRAY(MAXTRE),TARRAY(MAXTRE)
      INTEGER I,IRAN,ISIZE

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISRAN',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,100) ICYC, JRAN
  100 FORMAT(' Begin MISRAN: Cycle = ',I5,'; Jran = ',I14)

C.... Initializations.

      DO 110 I=1,ISIZE
         TARRAY(I)=0
  110 CONTINUE

C.... Check on ISIZE boundaries.

      IF(ISIZE.GT.MAXTRE.OR.ISIZE.LT.1) THEN
         PRINT*,' *MISRAN* Error. Invalid ISIZE: ',ISIZE
         GO TO 9000
      ENDIF

C.... Top of random number loop.

      DO 300 I=1,ISIZE

C....    Generate a random integer.

  200    CONTINUE
         JRAN=MOD(JRAN*IA+IC,IM)
         IRAN=INT(1+(ISIZE*JRAN/IM))

C....    Make sure IRAN is between 1 and ISIZE (inclusive).

         IF(IRAN.GT.ISIZE.OR.IRAN.LT.1) GO TO 200

C....    TARRAY is a temporary storage array to keep track of what
C....    integers have already been taken (can't use the same one
C....    twice). Loop until an opening in TARRAY is found.

  250    CONTINUE
         IF(TARRAY(IRAN).EQ.1) THEN
            IRAN=IRAN+1
            IF(IRAN.GT.ISIZE) IRAN=1
            GO TO 250
         ENDIF

C....    An opening was found. Mark TARRAY slot as used and deposit
C....    random integer into IARRAY.

         TARRAY(IRAN)=1
         IARRAY(I)=IRAN
  300 CONTINUE

C.... Common return.

 9000 CONTINUE

      IF(DEBUG) WRITE(JOSTND,9010) ICYC
 9010 FORMAT(' End MISRAN: Cycle = ',I5)

      RETURN
      END
