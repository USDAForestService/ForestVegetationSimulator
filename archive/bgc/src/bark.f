      SUBROUTINE BARK(SPEC,DIAM,BRK)
C
C----------------------------------------------------------------------
C SUBROUTINE CALCULATES BARK THICKNESS. EQUATIONS FIT TO DATA FROM
C WESTERN MONTANA. MILNER STEM ANALYSIS DATA. ENGLISH UNITS.
C----------------------------------------------------------------------
C
      CHARACTER*4 SPEC
      REAL DIAM, BRK
C
C SET COEFFICIENTS BY SPECIES. DEFAULT TO LODGEPOLE PINE.
C
      IF(SPEC.EQ.'PP') THEN
          B0=0.18281
          B1=0.06495
      ELSE IF(SPEC.EQ.'DF') THEN
          B0=-0.05622
          B1=0.06698
      ELSE IF(SPEC.EQ.'WL') THEN
          B0=0.11153
          B1=0.06909
      ELSE
          B0=0.02179
          B1=0.02953
      ENDIF
C CALCULATE BARK THICKNESS (in.)
      BRK=B0 + B1*DIAM
      RETURN
      END
