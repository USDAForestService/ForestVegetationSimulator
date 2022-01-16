      SUBROUTINE DFBDAM (II,ICODES)
      IMPLICIT NONE
C----------
C  **DFBDAM        DATE OF LAST REVISION:  06/30/10
C----------
C
C  PROCESSES THE DFB DAMAGE CODES.
C
C  CALLED BY :
C     DAMCDS  [PROGNOSIS]
C
C  CALLS :
C     NONE
C
C  PARAMETERS :
C     II     - TREE POINTER FOR LIVE AND DEAD TREES.
C     ICODES - DISEASE AND DAMAGE CODES ARRAY.
C
C  LOCAL VARIABLES :
C     I      - COUNTER FOR DAMAGE CODES.
C
C  COMMON BLOCK VARIABLES USED :
C     NDAMS  - (DFBCOM)   OUTPUT
C     IPT    - (DFBCOM)   OUTPUT
C
C  Revision History :
C  11/06/89 -
C  07/10/07 - Lance R. David (FHTET)
C    Time at which damage codes are processed is now at the end of 
C    keyword processing instead of during the reading of tree data.
C    So, tree data items that were passed as arguments are now 
C    available from the FVS common area. Original arguments were:
C    (II,ICODES,IITH,IREC2,LDELTR)
C    FVS array IMC(II) is used as replacement for input tree history
C    code (IITH).
C    No need for special handling of dead tree index (IREC2) because
C    dead trees are already at the end of the arrays. Argument II is
C    correct index value for both live and dead.
C----------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'DFBCOM.F77'
C
COMMONS
C
      LOGICAL LDELTR

      INTEGER I, II, ICODES(6), IITH, IREC2

C     IF THE DAMAGE CODE IS 3, THERE IS DFB DAMAGE...IF THE
C     CORRESPONDING SEVERITY CODE IS >= 3, SUCCESSFUL ATTACK, THEN SAVE
C     THE NUMBER OF TREES.
C
      DO 100 I = 1,5,2
         IF (ICODES(I) .EQ. 3) THEN
            IF (ICODES(I+1) .GE. 3) THEN
               NDAMS = NDAMS + 1

C              IF THE TREE IS RECENT MORTALITY (DIED WITH MORTALITY
C              OBSERVATION PERIOD, IMC (MANAGEMENT CODE) 7. IMC SET
C              BASED ON TREE HISTORY CODE OF 6 OR 7 DURING READING
C              OF INPUT TREE DATA IN SUBROUTINE INTREE.
C
               IF (IMC(II) .EQ. 7) THEN
                  IPT(NDAMS) = II
               ENDIF

               GOTO 200
            ENDIF
         ENDIF
  100 CONTINUE

  200 CONTINUE
      RETURN
      END
