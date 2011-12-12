      SUBROUTINE DGBND (ISPC,DBH,DDG)
      IMPLICIT NONE
C----------
C  **DGBND--SN  DATE OF LAST REVISION:  06/05/08
C----------
C  THIS SUBROUTINE IS INSURES THAT A MAXIMUM VALUE FOR DIAMETER
C  GROWTH IS NOT EXCEEDED. THE DIAMETER GROWTH IS ADJUSTED LINEARLY
C  BETWEEN THE A RANGE OF DBH VALUES BY SPECIES.  BELOW THE RANGE
C  THERE IS NO NET ADJUSTMENT (ADJUSTMENT FACTOR = 1.) ABOVE THE
C  RANGE THE ADJUSTMENT IS CONSTANT (ADJUSTMENT FACTOR= 0.10)
C  THE RANGE OVER WHICH THE DG ADJUSTMENT IS PERFORMED IS
C  CONTAINED IN THE DLODHI ARRAY.
C  THIS ROUTINE IS CALLED BY DGDRIV.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C----------
C  DLODHI - (I,J) I IS SPECIES AND J=1 IS THE LOOWER BOUND FOR DG
C           ADJUSTMENT AND J=2 IS THE UPPER BOUND FOR DG ADJUSTMENT.
C----------
      REAL DLODHI(90,2),DDG,DBH
      INTEGER ISPC,I,J
C
      DATA ((DLODHI(I,J),J= 1,2),I= 1,15)/
     &   12.0,     26.0,
     &   11.9,     24.1,
     &   12.0,     38.0,
     &   12.5,     18.9,
     &   15.3,     27.9,
     &   14.0,     27.4,
     &   20.7,     32.6,
     &   15.9,     24.4,
     &   13.7,     18.7,
     &   15.8,     24.2,
     &   15.3,     28.7,
     &   20.6,     32.9,
     &   17.1,     37.2,
     &   12.3,     20.0,
     &   26.0,     79.8/
      DATA ((DLODHI(I,J),J= 1,2),I= 16,30)/
     &   15.1,     45.4,
     &   20.5,     39.3,
     &   13.5,     26.1,
     &   15.7,     26.7,
     &   14.5,     35.9,
     &   24.1,     44.2,
     &   17.8,     35.2,
     &   20.5,     35.3,
     &   18.8,     38.4,
     &   13.3,     24.9,
     &    8.5,     17.3,
     &   17.2,     38.6,
     &   30.7,     46.5,
     &   18.8,     32.9,
     &    6.4,     11.3/
      DATA ((DLODHI(I,J),J= 1,2),I= 31,45)/
     &    5.2,     9.7 ,
     &   10.2,     22.4,
     &   25.5,     42.8,
     &   15.2,     30.7,
     &   18.2,     33.4,
     &   18.0,     36.0,
     &   20.6,     37.0,
     &   18.4,     33.2,
     &   12.5,     28.1,
     &   14.8,     20.5,
     &    7.4,     17.1,
     &   12.0,     30.0,
     &   16.7,     32.9,
     &   16.3,     39.6,
     &   19.5,     40.4/
      DATA ((DLODHI(I,J),J= 1,2),I= 46,60)/
     &   14.4,     32.5,
     &   17.7,     27.0,
     &   19.8,     36.5,
     &   14.4,     32.5,
     &   19.8,     36.5,
     &    9.0,     21.2,
     &   13.0,     23.6,
     &   21.4,     63.8,
     &   16.2,     30.8,
     &   15.9,     33.0,
     &    8.8,     18.6,
     &    8.1,     16.7,
     &    7.4,     19.2,
     &   23.6,     56.6,
     &   30.7,     46.5/
      DATA ((DLODHI(I,J),J= 1,2),I= 61,75)/
     &   24.0,     48.0,
     &   12.3,     26.9,
     &   18.9,     42.8,
     &   17.7,     34.5,
     &   19.2,     42.3,
     &   25.5,     46.2,
     &    8.6,     17.2,
     &   22.1,     48.1,
     &   26.7,     48.0,
     &   13.0,     22.7,
     &   25.1,     47.2,
     &   19.5,     37.2,
     &   22.9,     47.6,
     &   20.4,     38.4,
     &   21.3,     41.1/
      DATA ((DLODHI(I,J),J= 1,2),I= 76,90)/
     &   23.1,     40.6,
     &   17.6,     38.9,
     &   19.3,     40.2,
     &   31.4,     58.8,
     &   17.2,     30.8,
     &   23.8,     38.8,
     &   11.8,     25.6,
     &   19.0,     32.1,
     &   15.4,     31.4,
     &   13.4,     23.9,
     &   19.6,     46.7,
     &   17.8,     35.8,
     &   11.9,     24.1,
     &   12.3,     33.3,
     &    9.8,     20.5/
C----------
C MAX DG CHECK.
C----------
      IF (DBH .LE. DLODHI(ISPC,1)) THEN
        DDG= 1.0 * DDG
      ELSEIF (DBH .GT. DLODHI(ISPC,2)) THEN
        DDG= 0.1 * DDG
      ELSE
        DDG= DDG*(1.0 + 0.90*((DBH - DLODHI(ISPC,1))/
     &       (DLODHI(ISPC,1) - DLODHI(ISPC,2))))
      ENDIF
C----------
C CHECK FOR SIZE CAP COMPLIANCE.
C----------
      IF((DBH+DDG).GT.SIZCAP(ISPC,1) .AND. SIZCAP(ISPC,3).LT.1.5)THEN
        DDG=SIZCAP(ISPC,1)-DBH
        IF(DDG .LT. 0.01) DDG=0.01
      ENDIF
C
      RETURN
      END




