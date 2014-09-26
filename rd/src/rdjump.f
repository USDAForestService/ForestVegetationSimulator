      SUBROUTINE RDJUMP
      IMPLICIT NONE
C----------
C  **RDJUMP      LAST REVISION:  08/29/14
C----------
C
C  Purpose :
C    This subroutine expands the area in root disease when the 
C    stand is clearcut.
C
C  Called By :
C    RDCNTL  [ROOT DISEASE]
C
C  Calls :
C    RDAREA  (SUBROUTINE)   [ROOT DISEASE]
C    RDINF   (SUBROUTINE)   [ROOT DISEASE]
C
C  Local Variables :
C    <incomplete>   
C
C  Common Block Variables Used :
C    <incomplete>   
C
C  Revision History
C    06/10/96 - Matthew K. Thompson
C               Changed the summing of trees in tree records.
C               Changed from an INTEGER summation to a REAL summation.
C   08/29/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
C.... Parameter include files

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files

      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'

C.... Local variable declarations

      INTEGER  I, I1, I2, IDI, J, KSP
      REAL     SPCEN, TOTCEN, TRENUM, XTRENU, XJPINC


      IF (PAREA(IRRSP) .EQ. 0.0) RETURN

C.... Jump out if a cut occurred

      IF (INFLAG .NE. 1) GOTO 650

      SPPROP(IRRSP) = 0.0
      IF (NCENTS(IRRSP) .EQ. 0) GOTO 650
      IF (TNJUMP(IRRSP) .LE. 0.0) GOTO 650

      SPCEN = 0.0
      TOTCEN = 0.0
      DO 700 I=1, NCENTS(IRRSP)
         IF (PCENTS(IRRSP,I,3) .LE. 0) GOTO 700

C....    Expand centers by the diameter of the uninfected root systems
C....    present at the time of the cut (given by 2*RRJINC) 

         PCENTS(IRRSP,I,3) = PCENTS(IRRSP,I,3) +
     &                       2 * RRJINC(IRRSP) * TNJUMP(IRRSP)
         IF (ICENSP(IRRSP,I) .NE. 0) SPCEN = SPCEN +
     &                               PCENTS(IRRSP,I,3)**2
         TOTCEN = TOTCEN + PCENTS(IRRSP,I,3) ** 2
  700 CONTINUE
      SPPROP(IRRSP) = SPCEN / (TOTCEN + 1E-6)

  710 CONTINUE

      CALL RDAREA

      AREANU(IRRSP) = PAREA(IRRSP) - OOAREA(IRRSP)
      IF (PAREA(IRRSP) .LE. 0.0) AREANU(IRRSP) = 0.0
      IF (AREANU(IRRSP) .LT. 0.0) AREANU(IRRSP) = 0.0
      OOAREA(IRRSP) = PAREA(IRRSP)

C.... Infect new trees due to the expansion of the centers

      CALL RDINF

C.... Loop over tree list if there are any infection centers.
C.... Calculate the mean jump increment to use next year.
C
C     TRENUM - Number of trees outside patch area represented by the
C              record (so will weight roots by number of trees).
C     XTRENU - Sum of number of trees outside patch area
C     XJPINC - Sum of uninfected root radii in appropriate records
C     RRJINC - Mean uninfected root radii (used to increment the 
C              radii of centers next year)

  650 CONTINUE

      RRJINC(IRRSP) = 0
      XJPINC = 0.0
      XTRENU = 0.0
      TRENUM = 0.0

      IF (ITRN .EQ. 0) RETURN

      IDI = IRRSP
      DO 500 KSP=1, MAXSP
         IF (ISCT(KSP,1) .EQ. 0) GOTO 500
         IF (IRRSP .LT. 3) IDI = IDITYP(IRTSPC(KSP))
         IF (IDI .NE. IRRSP) GOTO 500

         I1 = ISCT(KSP,1)
         I2 = ISCT(KSP,2)

         DO 400 J=I1, I2
            I = IND1(J) 
            TRENUM = FPROB(I) * (SAREA - PAREA(IDI))
            XJPINC = XJPINC + ROOTL(I) * TRENUM
            XTRENU = XTRENU + TRENUM
  400    CONTINUE
  500 CONTINUE

  600 CONTINUE
      RRJINC(IRRSP) = XJPINC / (XTRENU + 1E-6)

      RETURN
      END
