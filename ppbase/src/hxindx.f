      SUBROUTINE HXINDX (NEIPTR,NEI,ISTND,NSTND)
      IMPLICIT NONE
C----------
C  **HXINDX--   DATE OF LAST REVISION:  07/31/08
C----------
C
C     COMPUTE INDICIES TO NEIGHBORING STANDS UNDER THE ASSUMPTION
C     THAT ALL STANDS ARE HAVE A SPATIAL ARRANGEMENT OF A SET OF
C     HEXAGONS.
C
C     PART OF THE PARALLEL PROCESSING EXTENSION OF PROGNOSIS SYSTEM.
C     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--NOV 1992
C
C     CALLED FROM: HVNEDN
C
C     NEIPTR=RETURNS A POINTER TO THE NEIGHBORING STAND.
C            RETURNS ZERO IF THE NEIGHBOR DOES NOT EXIST.
C     NEI   =PASSED IN AS THE NEIGHBORING HEXAGON DESIRED, WHERE
C            1=NORTH, 2=NORTHEAST, 3=SOUTHEAST, 4=SOUTH, 5=SOUTHWEST
C            6=NORTHWEST.
C     ISTND =THE REFERENCE STAND.
C     NSTND =THE NUMBER OF STANDS THAT MAKE UP THE SYSTEM OF HEXAGONS.
C
C     THE ROUTINES ASSUMES THAT A SYSTEM OF NSTND HEXAGONS ARE
C     USED AS SPATIAL UNITS WHERE THE NUMBER OF ROWS AND COLUMNS ARE
C     AS EQUAL AS POSSIBLE (AN EXTRA ROW IS USED IF NECESSARY).
C     THEN A POINTER TO A NEIGHBOR IS RETURNED.
C
C      THE SYSTEM OF HEXAGONS IS LIKE THIS:
C
C      Col:   1  2  3  4  5  6
C            -- -- -- -- -- --
C      Row 6: 1     3     5
C                2     4     6
C
C      Row 5: 1     3     5
C                2     4     6
C
C      Row 4: 1     3     5
C                2     4     6
C
C      Row 3: 1     3     5
C                2     4     6
C
C      Row 2: 1     3     5
C                2     4     6
C
C      Row 1: 1     3     5
C                2     4     6
C                                          _    _    _
C      EACH ROW IS ORIGANIZED LIKE THIS:  /1\ _/3\ _/5\ _
C                                         \_/ 2\_/ 4\ / 6\
C                                            \_/  \_/  \_/
C
C
C
      INTEGER NSTND,NEI,NEIPTR,NR,NC,IR,IC,NROW,NCOL,ISTND
C
C
C
C     IF NEI, ISTND, OR NSTND ARE ZERO, THEN SET THE RETURN VALUE
C     OF NEIPTR TO ZERO AND RETURN.
C
      IF (ISTND.EQ.0 .OR. NEI.LT.1 .OR. NEI.GT.6 .OR. NSTND.EQ.0) THEN
         NEIPTR=0
         RETURN
      ENDIF
C
C     COMPUTE THE NUMBER OF ROWS AND COLS.
C
      NR=IFIX(SQRT(FLOAT(NSTND)))
      NC=NR
      IF (NC*NR.LT.NSTND) NR=NR+1
C
C     COMPUTE THE ROW AND COL OF THE REFERENCE STAND.
C
      IR=ISTND/NC
      IF (MOD(ISTND,NC).EQ.0) THEN
         IC=NC
      ELSE
         IC=ISTND-(IR*NC)
         IR=IR+1
      ENDIF
      GOTO (10,20,30,40,50,60), NEI
   10 CONTINUE
C
C     FOR THE NORTH NEIGHBOR.
C
      NROW=IR+1
      NCOL=IC
      GOTO 100
   20 CONTINUE
C
C     FOR THE NORTHEAST NEIGHBOR.
C
      IF (MOD(IC,2).EQ.1) THEN
         NROW=IR+1
      ELSE
         NROW=IR
      ENDIF
      NCOL=IC+1
      GOTO 100
   30 CONTINUE
C
C     FOR THE SOUTHEAST NEIGHBOR.
C
      IF (MOD(IC,2).EQ.1) THEN
         NROW=IR
      ELSE
         NROW=IR-1
      ENDIF
      NCOL=IC+1
      GOTO 100
   40 CONTINUE
C
C     FOR THE SOUTH NEIGHBOR.
C
      NROW=IR-1
      NCOL=IC
      GOTO 100
   50 CONTINUE
C
C     FOR THE SOUTHWEST NEIGHBOR.
C
      IF (MOD(IC,2).EQ.1) THEN
         NROW=IR
      ELSE
         NROW=IR-1
      ENDIF
      NCOL=IC-1
      GOTO 100
   60 CONTINUE
C
C     FOR THE NORTHWEST NEIGHBOR.
C
      IF (MOD(IC,2).EQ.1) THEN
         NROW=IR+1
      ELSE
         NROW=IR
      ENDIF
      NCOL=IC-1
  100 CONTINUE
C
C     CHECK FOR BORDER CONDITIONS
C
      IF (NCOL.LE.0  .OR. NROW.LE.0  .OR.
     >    NCOL.GT.NC .OR. NROW.GT.NR) THEN
         NEIPTR=0
      ELSE
C
C        COMPUTE THE POINTER TO THE NEIGHBOR.
C
         NEIPTR=((NROW-1)*NC)+NCOL
         IF (NEIPTR.GT.NSTND) NEIPTR=0
      ENDIF
      RETURN
      END
