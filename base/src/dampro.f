      SUBROUTINE DAMPRO
      IMPLICIT NONE
C----------
C  **DAMPRO--BASE   DATE OF LAST REVISION:  07/23/08
C----------
C
C  THIS SUBROUTINE CONTROLS THE PROCESSING DAMAGE AND SEVERITY CODES 
C  STORED WHILE READING THE INPUT TREE RECORDS. DAMAGE CODE PROCESSING
C  ROUTINES OF EXTENSIONS THAT UTILIZE DAMAGE AND SEVERITY CODES ARE
C  CALLED HERE (PREVIOUSLY THEY WERE CALLED FROM SUBROUTINE INTRE).
C  CREATED: 07/06/2007 LANCE R. DAVID
C
C----------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'

      INTEGER I1,I2,II,I
      INTEGER ICODES(6)
 
      CONTINUE
      
C      WRITE(16,*) ' IN DAMPRO: IRECRD=',IRECRD,' ITRN=',ITRN,
C     > ' IREC1=',IREC1,' IREC2=',IREC2

C     Set looping indices to process live tree records.
C     Indices for dead trees will be set and processed next.

      I1 = 1
      I2 = IREC1

  100 CONTINUE
C      WRITE(16,*) ' IN DAMPRO: I1=',I1,' I2=',I2      

      DO II = I1, I2
        DO I = 1,6
          ICODES(I) = DAMSEV(I,II)
        END DO

C        WRITE(16,*) 'IN DAMPRO: ICODES(I) =    ',(ICODES(I),I=1,6),
C     &  ' IDTREE, ISP, DBH =',IDTREE(II),ISP(II),DBH(II),
C     &  ' IMC = ',IMC(II)

        CALL MISDAM (II,ICODES)
        CALL RDDAM  (II,ICODES)
        CALL TMDAM  (II,ICODES)
        CALL MPBDAM (II,ICODES)
        CALL DFBDAM (II,ICODES)
        CALL BRDAM  (II,ICODES)
        CALL BMDAM  (II,ICODES)
      END DO

C     If dead trees are present, set indices and process.

      IF (I1 .EQ. 1 .AND. IREC2 .NE. (MAXTRE + 1)) THEN
        I1 = IREC2
        I2 = MAXTRE
        GO TO 100
      ENDIF

C     write(16,*) 'EXIT ROUTINE DAMPRO'
      
      RETURN
      END
