      SUBROUTINE SURFCE
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     SURFACE AREA CALCUTLATION ROUTINE
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
C     NICK CROOKSTON; PROGRAMMER
C
C      THIS ROUTINE IS CALLED FROM MPBCUP WHEN MPBMOD WILL BE CALLED
C
C      EQUATIONS ARE FROM A. STAGE
C
C          SNOHST= TOTAL NON-LPP SURFACE AREA TO 5 INCH TOPS
C          SURF  = SURFACE OF LPP BY CLASS TO 5 INCH TOPS
C          SUR   = TEMPORARY.  SURFACE TO THE TOP
C          SUR5  = TEMPORARY.  SURFACE TO 5 INCH TOP
C
C
C Revision History:
C   06/05/00  Glen Brink (FHTET)
C     Added variables IDXWP,IDXWL,IDXDF,IDXLP and IDXPP, array indices of
C       White Pine, Western Larch, Douglas Fir, Lodgepole Pine and
C       Ponderosa Pine respectively.  Added to common block in file
C       MPBCOM.F77.
C     Added array MPBSPM to govern the computations by species using an
C       IF block as opposed to the old COMPUTED GO TO, since the array 
C       allows the definition to be made in mpblkd.f, instead of always
C       having to change the COMPUTED GO TO. Added to common block in
C       file MPBCOM.F77.
C   11/10/00  Lance David (FHTET)
C     Basic cleanup of code and comments.
C     Removed old & inactive code.  
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C---------------------------------------------------------------------
C
COMMONS
C

      INCLUDE 'PRGPRM.F77'

      INCLUDE 'ARRAYS.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'MPBCOM.F77'
C
COMMONS
C
      INTEGER I, I1, I2, II, IS
      REAL SUR, SUR5, SURFLP

      SNOHST = 0.0

C     SPECIES LOOP.
C
      DO 200 IS = 1,MAXSP
        IF (IS .EQ. IDXLP) GO TO 200
        I1 = ISCT(IS,1)
        IF (I1 .EQ. 0) GO TO 200
        I2 = ISCT(IS,2)

C       TREE RECORD LOOP WITHIN SPECIES
C
        DO 100 II = I1, I2
          I = IND1(II)

          IF(MPBSPM(IS) .EQ. IDXWP) THEN
C
C           WHITE PINE
C
            SUR = -1.8673+0.86*ALOG(DBH(I))+1.07854*ALOG(HT(I))
            SUR5 = 19.6794 + 0.2345*HT(I) + 0.1363*CFV(I) + 0.8841*SUR

          ELSE IF(MPBSPM(IS) .EQ. IDXWL) THEN
C
C           WESTERN LARCH
C
            SUR = -3.0779+0.062565*ALOG(DBH(I))+1.46018*ALOG(HT(I))
            SUR5 = 31.90062 - 0.12195784*HT(I) + 0.071106*CFV(I) +
     >             0.972312*SUR

          ELSE IF(MPBSPM(IS) .EQ. IDXDF) THEN
C
C           DOUGLAS-FIR
C
            SUR = -1.73648+0.912291*ALOG(DBH(I))+1.010666*ALOG(HT(I))
            SUR5 = 19.4183 + 0.2631*HT(I) + 0.2593*CFV(I) + 0.8274*SUR

          ELSE IF(MPBSPM(IS) .EQ. IDXPP) THEN
C
C           PONDEROSA PINE
C
            SUR = -1.51556+0.99399*ALOG(DBH(I))+0.93723*ALOG(HT(I))
            SUR5 = 9.1767 + 0.4531*HT(I) + 0.3719*CFV(I) + 0.7589*SUR

          END IF

          IF (SUR5 .GE. 0.0)  SNOHST = SNOHST + SUR5 * PROB(I)

  100   CONTINUE
  200 CONTINUE
C
C     LODGEPOLE PINE   (BY CLASS)
C
      DO 250 I = 1,NACLAS
        SURF(I) = SURFLP(CLASS(I,2))
  250 CONTINUE

      IF (DEBUIN) THEN
        WRITE (JOMPB,400) SNOHST,(SURF(I),I=1,NACLAS)
  400   FORMAT (/,'SNOHST (TOTAL NON-LPP SURFACE AREA TO',
     >          ' 5 INCH TOPS) ',E14.7,//,
     >          'LPP SURFACE AREA BY CLASS . . .',//,
     >          3(10E13.3,/))
      END IF

      RETURN
      END
