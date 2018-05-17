      SUBROUTINE HTDIAM(SPEC,DIAM,THT)
C
C---------------------------------------------------------
C SUBROUTINE RETURNS HEIGHT FOR GIVEN DBH. EQUATION FIT TO
C DATA FROM WESTERN MONTANA. MILNER STEM ANALYSIS DATA.
C MODEL FORM IS LOG10(H) = B0 +B1*LOG10(DBH). ENGLISH UNITS.
C---------------------------------------------------------
C
      CHARACTER*4 SPEC
      REAL DIAM, THT
C SET COEFFICIENTS
      IF(SPEC.EQ.'PP') THEN
          B0=0.39835
          B1=1.21820
      ELSE IF(SPEC.EQ.'DF') THEN
          B0=0.80022
          B1=0.86325
      ELSE IF(SPEC.EQ.'WL') THEN
          B0=0.84146
          B1=0.93298
      ELSE
          B0=0.73723
          B1=1.01854
      ENDIF
C CALCULATE HEIGHT
      THT=10**(B0+B1*LOG10(DIAM))
      RETURN
      END

