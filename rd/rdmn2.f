      SUBROUTINE RDMN2 (XFINT)
      IMPLICIT NONE
C----------
C  **RDMN2       LAST REVISION:  03/01/16
C----------
C
C  SETS UP INITIAL CONDITIONS FOR THE OTHER AGENTS AND ROOT DISEASE
C  MODELS, AND UPDATES THE STUMP LISTS. NOTE THAT THIS HAPPENS BEFORE 
C  TRIPLING AND COMPRESSION
C
C  CALLED BY :
C     GRINCR  [PROGNOSIS]
C
C  CALLS     :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
C     RDSHST  (SUBROUTINE)   [ROOT DISEASE]
C
C  PARAMETERS :
C     XFINT  -
C
C  Revision History :
C   03/01/95 - Last revision date.
C   08/29/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C   03/01/2016 Lance R. David (FMSC)
C     Moved check to exit if not active, no trees or no disease area
C     to top.
C----------------------------------------------------------------------
C
COMMONS
C
C PARAMETER FILES
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
C
COMMONS
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'RDCRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDADD.F77'

      LOGICAL  DEBUG
      INTEGER  I, IDI, J
      REAL     TPAREA, XFINT

C     Exit if not active or no trees.
      IF (IROOT .EQ. 0 .OR. ITRN .EQ. 0) RETURN
C
C     SEE IF WE NEED TO DO SOME DEBUG.
C
      CALL DBCHK (DEBUG,'RDMN2',5,ICYC)

      ISTEP = ISTEP + 1
      IF (DEBUG) WRITE (JOSTND,100) ICYC, ISTEP
  100 FORMAT (' RDMN2 :  ICYC ISTEP=',2I5)
      IYEAR = IYEAR + INT(XFINT)
      TPAREA = 0.0
      DO 105 IDI=MINRR,MAXRR
         TPAREA = TPAREA + PAREA(IDI)
  105 CONTINUE       

C     Exit if no disease area.
      IF (TPAREA .EQ. 0.0) RETURN

C
C     INCREMENT THE TIME STEP COUNTERS FOR THE GENERAL ROOT DISEASE
C     MODEL.
C
C     ZERO OUT VARIABLES FOR THE SIMULATION.
C
C     PROBIN IS USED THE CARRYOVER MODEL
C
      DO 350 IDI=MINRR,MAXRR
         DO 300 I=1,2
            DO 200 J=1,5
               PROBIN(IDI,I,J) = 0.0
  200       CONTINUE
  300    CONTINUE
  350 CONTINUE

C
C     INITIALIZE RRKILL (RDKILL IS NEEDED NEXT TIMESTEP (AS PRANKL) SO WILL
C                        NOT BE ZEROED HERE)
C
      DO 400 I=1,ITRN
         RRKILL(I) = 0.0
C         RDKILL(I) = 0.0
  400 CONTINUE

C
C     SUM THE ARRAY PROBI INTO THE ACCUMULATOR ARRAY PROBIT.
C
      CALL RDSUM (ITRN,PROBIT,PROBI,ISTEP)

C
C     UPDATE THE STUMP HISTORY ARRAYS.
C
      CALL RDSHST

      RETURN
      END
