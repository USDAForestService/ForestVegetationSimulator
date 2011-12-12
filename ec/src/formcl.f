      SUBROUTINE FORMCL(ISPC,IFOR,D,FC)
      IMPLICIT NONE
C----------
C  **FORMCL--EC     DATE OF LAST REVISION:  03/24/08
C----------
C
C THIS PROGRAM CALCULATES FORM FACTORS FOR CALCULATING CUBIC AND
C BOARD FOOT VOLUMES.
C
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
C
C----------
      REAL GIFPFC(MAXSP,5),MTHDFC(MAXSP,5),OKANFC(MAXSP,5)
      REAL WENAFC(MAXSP,5),FC,D
      INTEGER IFOR,ISPC,IFCDBH
C----------
C  FOREST ORDER: (IFOR)
C  1=MOUNT HOOD(606)  2=OKANOGAN(608)  3=WENATCHEE(617)
C  4=OKANOGAN (TONASKET RD) (699) 5=GIFFORD PINCHOT(603)
C
C  SPECIES ORDER: (ISPC)
C  1=WP  2=WL  3=DF  4=SF  5=RC  6=GF  7=LP  8=ES  9=AF 10=PP 11=MH/OT
C----------
C  GIFFORD PINCHOT FORM CLASS VALUES
C----------
      DATA GIFPFC/
     & 84., 76., 82., 87., 70., 84., 82., 80., 80., 76., 82.,
     & 84., 76., 82., 87., 70., 84., 82., 80., 80., 76., 82.,
     & 84., 74., 80., 86., 68., 84., 82., 80., 80., 78., 82.,
     & 82., 74., 79., 84., 68., 84., 82., 78., 80., 80., 80.,
     & 82., 74., 78., 84., 68., 84., 82., 78., 80., 82., 80./
C----------
C  MOUNT HOOD FORM CLASS VALUES
C----------
      DATA MTHDFC/
     & 84., 86., 76., 87., 75., 76., 76., 77., 84., 79., 72.,
     & 76., 86., 82., 87., 82., 72., 68., 77., 84., 79., 72.,
     & 76., 86., 82., 86., 82., 72., 68., 77., 82., 82., 72.,
     & 76., 86., 82., 84., 82., 72., 68., 77., 80., 83., 72.,
     & 76., 87., 82., 80., 82., 72., 68., 77., 75., 82., 72./
C----------
C  OKANOGAN FORM CLASS VALUES
C----------
      DATA OKANFC/
     & 78., 78., 72., 82., 75., 76., 85., 82., 84., 78., 75.,
     & 80., 78., 72., 82., 75., 78., 85., 82., 84., 80., 78.,
     & 80., 78., 73., 82., 72., 77., 85., 83., 85., 81., 79.,
     & 82., 73., 75., 84., 68., 76., 85., 86., 85., 82., 79.,
     & 80., 73., 75., 84., 61., 76., 85., 86., 85., 84., 78./
C----------
C  WENATCHEE FORM CLASS VALUES
C----------
      DATA WENAFC/
     & 83., 77., 75., 85., 69., 78., 82., 79., 76., 77., 82.,
     & 84., 78., 76., 86., 70., 79., 82., 80., 77., 78., 82.,
     & 84., 79., 75., 84., 70., 79., 82., 80., 78., 81., 82.,
     & 85., 80., 76., 86., 68., 79., 82., 82., 76., 81., 80.,
     & 84., 80., 73., 86., 70., 80., 82., 82., 77., 80., 80./
C----------
C  FOR REGION 6 FORESTS, LOAD THE FORM CLASS USING TABLE VALUES.
C  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.
C----------
      IF(FRMCLS(ISPC).LE.0.) THEN
        IFCDBH = (D - 1.0) / 10.0 + 1.0
        IF(IFCDBH .LT. 1) IFCDBH=1
        IF(D.GT.40.9) IFCDBH=5
        IF(IFOR.EQ.1) THEN
          FC = MTHDFC(ISPC,IFCDBH)
        ELSEIF(IFOR.EQ.2 .OR. IFOR.EQ.4) THEN
          FC = OKANFC(ISPC,IFCDBH)
        ELSEIF(IFOR.EQ.5) THEN
          FC = GIFPFC(ISPC,IFCDBH)
        ELSE
          FC = WENAFC(ISPC,IFCDBH)
        ENDIF
      ELSE
        FC=FRMCLS(ISPC)
      ENDIF
C
      RETURN
      END
