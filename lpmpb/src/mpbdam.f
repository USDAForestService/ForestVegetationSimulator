      SUBROUTINE MPBDAM (II,ICODES)
      IMPLICIT NONE
C----------
C  **MPBDAM        DATE OF LAST REVISION:  07/02/10
C----------
C
C     PROCESS THE MPB DAMAGE CODES.
C
C Revision History
C   11/06/89 -
C   07/10/07 - Lance R. David (FHTET)
C     Time at which damage codes are processed is now at the end of 
C     keyword processing instead of during the reading of tree data.
C     So, tree data items that were passed as arguments are now 
C     available from the FVS common area. Original arguments were:
C     (II,IDTR,ICODES,IITH,IREC2,IISPI,LDELTR)
C     FVS array IMC(II) is used as replacement for input tree history
C     code (IITH).
C     No need for special handling of dead tree index (IREC2) because
C     dead trees are already at the end of the arrays. Argument II is
C     correct index value for both live and dead.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'MPBCOM.F77'
C
COMMONS
C
      INTEGER   I, II, ICODES(6)

C     IF THE DAMAGE CODE IS 2, THERE IS MPB DAMAGE...IF THE
C     CORRESPONDING SEVERITY CODE IS 3, SUCCESSFUL ATTACK, THEN
C     SAVE THE TREE NUMBER IN A LIST OF NUMBERS.

      DO 10 I=1,5,2
         IF (ICODES(I) .EQ. 2) THEN
            IF (ICODES(I+1) .EQ. 3) THEN
               NDAMS = NDAMS + 1

C              IF THE TREE IS RECENT MORTALITY (DIED WITH MORTALITY
C              OBSERVATION PERIOD, IMC (MANAGEMENT CODE) 7. IMC SET
C              BASED ON TREE HISTORY CODE OF 6 OR 7 DURING READING
C              OF INPUT TREE DATA IN SUBROUTINE INTREE.

c               IF (IMC(II) .EQ. 7) THEN
                  IPT(NDAMS) = II
c               ENDIF
               GOTO 20
            ENDIF
         ENDIF
   10 CONTINUE

   20 CONTINUE
C     write (16,*) 'IN MPBDAM: II= ',II,' IPT(',NDAMS,')= ',IPT(NDAMS)
      RETURN
      END
