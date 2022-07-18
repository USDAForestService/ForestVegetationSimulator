      SUBROUTINE BMDAM (II,ICODES)
C----------
C WWPB $Id$
C----------
C
C  THIS ROUTINE PROCESSES THE MOUNTAIN PINE BEETLE DAMAGE CODES.
C  FOR THE WESTWIDE PINE BEETLE MODEL. THE ASSUMPTION IS THAT
C  ATTACKS ARE ON LODGEPOLE PINE ONLY (THIS IS NOT CHECKED) THAT
C  TREES MUST BE ALIVE, AND THAT THE ATTACK MUST HAVE HAPPENED 
C  WITHIN THE LAST (DEFAULT MODIFIABLE BY KEYWORD BMDAMAGE) TWO
C  YEARS PRIOR TO THE FIRST PPE MASTER CYCLE.
C
C  CALLED BY :
C     DAMCDS  [PROGNOSIS]
C
C  CALLS     :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C
C  PARAMETERS :
C     II     - TREE POINTER FOR LIVE AND DEAD TREES.
C     ICODES - DISEASE AND DAMAGE CODES ARRAY.
C
C  LOCAL VARIABLES :
C
C
C  COMMON BLOCK VARIABLES USED :
C
C  Revision History :
C  07/28/00 AJM:
C    Reading of damage codes changed so that damage codes 1, 2, 5 
C    are used and recognized instead of 3.
C  07/10/07 - Lance R. David (FHTET)
C    Time at which damage codes are processed is now at the end of 
C    keyword processing instead of during the reading of tree data.
C    So, tree data items that were passed as arguments are now 
C    available from the FVS common area. Original arguments were:
C    (II,IITH,ICODES)
C    FVS array IMC(II) is used as replacement for input tree history
C    code (IITH).
C    No need for special handling of dead tree index (IREC2) because
C    dead trees are already at the end of the arrays. Argument II is
C    correct index value for both live and dead.
C----------
C
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PPCNTL.F77'

      INCLUDE 'BMPRM.F77'
      INCLUDE 'BMCOM.F77'

      LOGICAL DEBUG
      DIMENSION ICODES(6)

C     SEE IF WE NEED TO DO SOME DEBUG.

      CALL DBCHK (DEBUG,'BMDAM',5,ICYC)
      IF (DEBUG) WRITE (JOSTND,*) ' ** IN BMDAM'

      LBMDAM(II) = .FALSE.

C     SEE IF THE INVENTORY IS WANTED FOR INITIALIZING THE MODEL.
C     OTHERWISE BRANCH TO END

      IF (.NOT. LINVON) GOTO 9000

C     SEE IF THE INVENTORY YEAR IS RECENT ENOUGH TO USE THIS INVENTORY FOR
C     INITIALIZING THE MODEL. OTHERWISE BRANCH TO END.

      IF ((MIY(1) - IY(1)) .GT. DEDYRS) GOTO 9000

C     PROCESS TREE RECORD. DEAD TREES ARE EXCLUDED, AND ONLY
C     BOLE ATTACKS (OR NOTHING RECORDED) ARE VALID. LOADING VALUES
C     INTO THE ARRAY WORKS BECAUSE *ALL* INVENTORIED STANDS ARE PART
C     OF THE DISPERSAL PROCESS. THEREFORE WHEN THE DISPERSAL STANDS
C     ARE RECORDED DURING THE READING OF THE INVENTORY, THERE WILL
C     BE A PROPER CORRESPONDENCE BETWEEN 'ISTND', WHICH IS
C     INCREMENTED IN PPMAIN, AND IS USED TO ASSIGN THE STAND NUMBER
C     VIA A CALL TO GPADSD IN INSTND. THE LOGICAL FLAG IS USED IN
C     BM??? TO CONVERT A TREES/ACRE ATTACK FOR THE BMKILL ARRAY.

      IF (IMC(II) .LT. 7 .OR. IMC(II) .GT. 9) THEN
        DO 100 J=1,5,2
          K1 = ICODES(J)
          K2 = ICODES(J+1)
          IF ((K1 .EQ. 1 .OR. K1 .EQ. 2 .OR. K1 .EQ. 5 .OR.
     >         K1 .EQ. 6) .AND. (K2 .EQ. 0 .OR. K2 .EQ. 3)) THEN
            LBMDAM(II) = .TRUE.
          ENDIF
  100   CONTINUE
      ENDIF

 9000 CONTINUE

      RETURN
      END
