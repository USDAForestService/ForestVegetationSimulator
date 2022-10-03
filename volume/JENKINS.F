      SUBROUTINE JENKINS(SPEC, DBHOB, BIOMS)
C     This subroutine calculate biomass using DBH (inches) and return dry biomass in lb.
C     08/15/2016 YW For woodland species, dismater is usually measured as DRC, Jenkins converted DRC to DBH
C     for the generilized equation.
      INCLUDE 'wdbkwtdata.inc'       !'WDBKWTDATA.INC'
      INTEGER SPEC, SPCD, SPGRPCD, FIRST, LAST, HALF, DONE
      INTEGER I0, J0
      REAL DBHOB, B0, B1, DBHIN, DBHCM
      REAL BIOMS(8), COEF(10,3)
      REAL FOL, ROOT, WOOD, BARK, STMTOT, ABT, BRANCHES, CROWN,STUMP
      REAL A0F, A1F, A0R, A1R, A0W, A1W, A0B, A1B
      REAL FOLRATIO, ROTRATIO, WDRATIO, BKRATIO, KG2LB
      REAL STUMPVIB, STUMPVOB, STUMPHT, WDEN, BDEN

C     COEF IS THE COEFFICIENT FOR ABOVE GROUND TOTAL BIOMASS FOR 10 SPECIES GROUP
C     SPECIES CLASS SPECIES GROUP                 GROUP CODE  B0         B1
C     ------------- ----------------------------- ----------  ---------- ------------
C     SOFTWOOD      CEDAR/LARCH                   1           -2.0336    2.2592
C                   DOUGLAS-FIR                   2           -2.2304    2.4435
C                   TRUE FIR/HEMLOCK              3           -2.5384    2.4814
C                   PINE                          4           -2.5356    2.4349
C                   SPRUCE                        5           -2.0773    2.3323
C     HARDWOOD      ASPEN/ALDER/COTTONWOOD/WILLOW 6           -2.2094    2.3867
C                   SOFT MAPLE/BIRCH              7           -1.9123    2.3651
C                   MIXED HARDWOOD                8           -2.4800    2.4835
C                   HARD MAPLE/OAK/HICKORY/BEECH  9           -2.0127    2.4342
C     WOODLAND      JUNIPER/OAK/MESQUITE          10          -0.7152    1.7029
C     --------------------------------------------------------------------------------
      DATA ((COEF(I0,J0),J0=1,3),I0=1,10) /
     *  1,           -2.0336,    2.2592,
     *  2,           -2.2304,    2.4435,
     *  3,           -2.5384,    2.4814,
     *  4,           -2.5356,    2.4349,
     *  5,           -2.0773,    2.3323,
     *  6,           -2.2094,    2.3867,
     *  7,           -1.9123,    2.3651,
     *  8,           -2.4800,    2.4835,
     *  9,           -2.0127,    2.4342,
     *  10,          -0.7152,    1.7029/

C    SOFTCOEF AND HARDCOEF ARE COEF FOR COMPONENT RATIO
C    SPECIES CLASS COMPONENT    COMPONENT CODE A0       A1
C    ------------- ------------ -------------- -------- ---------
C    HARDWOOD      FOLIAGE      1              -4.0813  5.8816
C                  COARSE ROOTS 2              -1.6911  0.8160
C                  STEM BARK    3              -2.0129  -1.6805
C                  STEM WOOD    4              -0.3065  -5.4240
C    SOFTWOOD      FOLIAGE      1              -2.9584  4.4766
C                  COARSE ROOTS 2              -1.5619  0.6614
C                  STEM BARK    3              -2.0980  -1.1432
C                  STEM WOOD    4              -0.3737  -1.8055
C    -------------------------------------------------------------
      SPCD = SPEC
      KG2LB = 2.20462
      STUMP = 0
      IF(SPCD.LT.10)THEN
C     The SPCD is jenkins group code
        SPCLS = 1
        IF(SPCD.LT.6) SPCLS = 0
        IF(SPCD.EQ.0) SPCD = 10
        SPGRPCD = SPCD
      ELSE
C     The SPCD is the species FIA code  
        FIRST = 1
        !LAST = 455
        LAST = TOTSPC
        DONE = 0
C     FIND THE SPECIES GROUP CODE FROM THE ARRAY      
        DO 5, WHILE (DONE.EQ.0)
          HALF = (LAST - FIRST +1)/2 + FIRST
          IF(WDBKWT(HALF,1) .EQ. SPCD)THEN
             DONE = HALF
          ELSEIF(FIRST .EQ. LAST) THEN
             DONE = -1
          ELSEIF (WDBKWT(HALF,1) .LT. SPCD) THEN
             FIRST = HALF
          ELSE
             LAST = HALF - 1
          ENDIF
  5     CONTINUE 
        IF(DONE.LT.0) DONE = TOTSPC  !455
        SPCLS = WDBKWT(DONE,2)
        SPGRPCD = WDBKWT(DONE,3) 
      ENDIF

      DBHIN = DBHOB

C    GET VOLUME FOR 1 FOOT STUMP USING RAILE
      STUMPHT = 1.0
      IF(DBHIN.GE.5.0) THEN
        CALL RAILEVOL(SPCD, DBHIN, STUMPHT, STUMPVIB, STUMPVOB)
        CALL WOODDEN(SPCD, WDEN, BDEN)
        STUMP = STUMPVIB*WDEN + (STUMPVOB-STUMPVIB)*BDEN
C       The STUMP calculated in above is in lb. Convert it to KG to calculate with Jenkins
        STUMP = STUMP/KG2LB        
      ENDIF
      
      DO I = 1, 10
        IF(COEF(I,1).EQ.SPGRPCD) THEN
          B0 = COEF(I,2)
          B1 = COEF(I,3)
          EXIT
        ENDIF
      END DO
C     CALCULATE BIOMASS FOR ABOVE GROUND TOTAL
C     First convert DBH from inch to cm because Jenkins equation using cn for DBH
      DBHCM = 2.54*DBHIN
C     For woodland species, convert DRC(DBHOB) to real DBH
C     The DRC to DBH conversion from Chojnacky etal 2014.
C      IF(SPGRPCD.EQ.10) DBHCM = EXP(-0.35031+1.03991*LOG(DBHCM))
C     The conversion from DRC to DBH should be done before call Jenkins
            
      ABT = EXP(B0 + B1 * LOG(DBHCM))
C     CALCULATE COMPONENT RATIO
      IF(SPCLS.EQ.0) THEN
        A0F = -2.9584
        A1F =  4.4766
        A0R = -1.5619
        A1R =  0.6614
        A0B = -2.0980
        A1B = -1.1432
        A0W = -0.3737
        A1W = -1.8055
      ELSEIF(SPCLS.EQ.1) THEN
        A0F = -4.0813
        A1F =  5.8816
        A0R = -1.6911
        A1R =  0.8160
        A0B = -2.0129
        A1B = -1.6805
        A0W = -0.3065
        A1W = -5.4240   
      ENDIF
      FOLRATIO = EXP(A0F + A1F/DBHCM)
      ROTRATIO = EXP(A0R + A1R/DBHCM) 
      BKRATIO = EXP(A0B + A1B/DBHCM)
      WDRATIO = EXP(A0W + A1W/DBHCM)   
      FOL = ABT*FOLRATIO
      ROOT = ABT*ROTRATIO
      IF(DBHIN.LT.5) THEN
        BARK = 0
        WOOD = 0
        STMTOT = 0
        BRANCHES = ABT - FOL
        CROWN = ABT
      ELSE
        BARK = ABT*BKRATIO
        WOOD = ABT*WDRATIO
        STMTOT = WOOD + BARK
        BRANCHES = ABT - STMTOT - FOL - STUMP
        CROWN = ABT - STMTOT - STUMP
      ENDIF
C     ADD BIOMASS COMPONENT VALUE TO RETURN VARIABLE BIOMS(8)
C     1 ABOVE GROUND TOTAL
C     2 STEM WOOD
C     3 STEM BARK
C     4 FOLIAGE
C     5 ROOTS
C     6 BRANCHES
C     7 CROWN
C     8 STEM WOOD AND BARK
C     Biomass calculated from Jenkins in Unit kg, need to convert to lb
      BIOMS(1) = ABT*KG2LB
      BIOMS(2) = WOOD*KG2LB
      BIOMS(3) = BARK*KG2LB
      BIOMS(4) = FOL*KG2LB
      BIOMS(5) = ROOT*KG2LB
      BIOMS(6) = BRANCHES*KG2LB
      BIOMS(7) = CROWN*KG2LB
      BIOMS(8) = STMTOT*KG2LB
      
      END
        