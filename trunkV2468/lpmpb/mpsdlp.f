      SUBROUTINE MPSDLP
      IMPLICIT NONE
C----------
C  **MPSDLP        DATE OF LAST REVISION:  06/14/13
C----------
C
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
C     COMPUTES THE SURFACE AREA OF THE DEAD LPP.
C
C Revision History
C   08/07/01 Lance R. David (FHTET)
C     Initialization of LPRT to false. It was uninitialized before.
C     (previous date of last revision was 02/08/88)
C   08/06/07 Lance R. David (FHTET)
C     Added PLOT.F77 common file. Added code for UNexpansion factor
C     on dead trees to address MPB mortality TPA being 2x the values
C     of input TPA and that reported in the cycle 0 treelist. An
C     explanation of the TPA expansion is in base\src\notre.f.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'ARRAYS.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'MPBCOM.F77'

      INCLUDE 'COLCOM.F77'
C
COMMONS
C
      LOGICAL LPRT
      INTEGER I, II, ISIZ
      REAL    P, SURFLP
      CHARACTER*5 DBHCLS(10)

      DATA DBHCLS / ' 1-3 ',' 3-5 ',' 5-7 ',' 7-9 ',' 9-11',
     &              '11-13','13-15','15-17','17-19',' 19+ ' /
     
      LPRT = .FALSE.
C
C     IF LSADLP IS TRUE THE USER SPECIFIED THE SURFACE AREA...
C
      IF (.NOT. LSADLP) THEN
C
C        IF THERE ARE DEAD TREES, SUM UP SURFACE AREA OF DEAD LPP.
C
         IF (IREC2 .LE. MAXTRE) THEN
            DO 100 I=IREC2,MAXTRE
               IF (ISP(I) .EQ. 7) SADLPP = SADLPP + SURFLP(DBH(I)) *
     &            PROB(I)
  100       CONTINUE
         ENDIF
C
C        IF THERE ARE 'NEW-HITS', ADD THEM TOO.
C
         IF (NDAMS .GT. 0) THEN
            DO 200 II=1,NDAMS
               I = IPT(II)
               SADLPP = SADLPP + SURFLP(DBH(I)) * PROB(I)
  200       CONTINUE
         ENDIF
      ENDIF
C
C     IF LINVMR IS TRUE, THEN SUM UP THE CURRENT ATTACKS
C
      IF (NDAMS .EQ. 0) LDAM = .FALSE.

      IF (LINVMR) THEN
         IF (.NOT. LPRT .AND. LDAM) THEN
            WRITE(JOMPB,310)
            WRITE(JOMPB,*)
            WRITE(JOMPB,320)
            WRITE(JOMPB,330)
            WRITE(JOMPB,340)
            LPRT = .TRUE.
  310       FORMAT (//,'MOUNTAIN PINE BEETLE DAMAGE CODE ',
     &              'OUTPUT TABLE')
  320       FORMAT (21X,'DBH',16X,'INF')
  330       FORMAT (' TREE ID     DBH    CLASS   TR/ACRE    STATUS ')
  340       FORMAT ('----------------------------------------------')
         ENDIF

         IF (NDAMS .GT. 0) THEN
            DO 400 I=1,10
               CURRMR(I) = 0.0
               GREINF(I) = 0.0
  400       CONTINUE

            DO 500 II=1,NDAMS
               I = IPT(II)
               CALL COLIND(DBH(I),ISIZ)
C
C              IF THE POINTER I, IS GE IREC2, THEN THE TREE IS DEAD,
C              OTHERWISE, THE TREE IS LIVE.
C              Dead trees need the TPA adjusted in order for the MPB tables
C              to match the input data set and FVS output treelist. See 
C              notre.f for complete explanation.
C
               IF (I .GE. IREC2) THEN
               	  P = (PROB(I) / GROSPC) / (FINT/FINTM)
                  CURRMR(ISIZ) = CURRMR(ISIZ) + P
                  IF (LDAM) WRITE (JOMPB,600) IDTREE(I), DBH(I),
     &                      DBHCLS(ISIZ), P, 'DEAD'
               ELSE
                  GREINF(ISIZ) = GREINF(ISIZ) + PROB(I)
                  IF (LDAM) WRITE (JOMPB,600) IDTREE(I), DBH(I),
     &                      DBHCLS(ISIZ), PROB(I), 'INFEST'
               ENDIF
  500       CONTINUE
            IF (LDAM) WRITE (JOMPB,*)
         ENDIF
  600    FORMAT(I8,3X,F6.1,3X,A5,4X,F6.2,4X,A6)
      ENDIF

      RETURN
      END
