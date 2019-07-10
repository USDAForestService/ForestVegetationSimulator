      SUBROUTINE ESSPRT(VAR,ISPC,NSPT,PREM,DSTMP)
      IMPLICIT NONE
C----------
C VESTB $Id$
C----------
C  THIS SUBROUTINE CONTAINS ENTRY POINTS TO HANDLE COMPUTATIONS FOR
C  VARIOUS STUMP SPROUTING FUNCTIONS WHICH VARY BY VARIANT AND
C  SPECIES.
C
C  VARIANTS USING THE ESTB VERSION: AK, CI, EM, IE, KT, NI
C----------
C
COMMONS
C
C  NO COMMON BLOCKS ARE CURRENTLY NEEDED.
C
COMMONS
C----------
C  DEFINITIONS
C
C     PREM -- NUMBER OF SPROUTS PER SPROUT RECORD
C    DSTMP -- DIAMETER OF PARENT TREE
C----------
C----------
      CHARACTER VAR*2
      INTEGER IAG,INDXAS,ISHAG,ISPC,NMSPRC,NSPT
      REAL ASTPAR,ASBAR,HTSPRT,PREM,RSHAG,SI,SPA,TREES,DSTMP
      INTEGER IDANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = NSPT
C----------
C  VARIANT AND SPECIES SPECIFIC RULES FOR DETERMINING TPA A SPROUT
C  RECORD WILL REPRESENT. ASPEN IS HANDLED SEPERATELY IN ENTRY ASSPTN
C  PREM = TPA OF PARENT TREE OF SPROUT RECORD
C----------
      SELECT CASE (VAR)
C
      CASE('AK')
        SELECT CASE(ISPC)
          CASE(14,15)
            IF(DSTMP.LT.25.9)THEN
              PREM = PREM * ((99.9999-3.8462 * DSTMP)/100)
            ELSE
              PREM = 0.
            ENDIF
          CASE(16,17)
            IF(DSTMP.LT.6.0)THEN
              PREM = PREM * 0.92
            ELSE IF(DSTMP.GE.6.0.AND.DSTMP.LT.9.0)THEN
              PREM = PREM * (-0.13333) * DSTMP + 1.70
            ELSE
              PREM = PREM * 0.50
            ENDIF
          CASE(18,20)
            IF(DSTMP.LT.6.25)THEN
              PREM = PREM * 0.39
            ELSE IF(DSTMP.GE.6.25.AND.DSTMP.LT.8.75)THEN
              PREM = PREM * 0.32
            ELSE
              PREM = PREM * 0.25
            ENDIF
          CASE(21,22)
            PREM = PREM * 0.90
          CASE DEFAULT
            PREM = PREM * 1.
        END SELECT
C
      CASE('IE')
        SELECT CASE(ISPC)
          CASE(17)
            PREM = PREM * 0.40
          CASE(19)
            PREM = PREM * 0.90
          CASE(20,21)
            PREM = PREM * 0.70
          CASE DEFAULT
            PREM = PREM * 1.
          END SELECT
C
      CASE('CI')
        SELECT CASE(ISPC)
          CASE(12)
            PREM = PREM * 0.40
          CASE(17)
            PREM = PREM * 0.90
          CASE DEFAULT
            PREM = PREM * 1.
        END SELECT
C
      CASE('EM')
        SELECT CASE(ISPC)
          CASE(11)
            IF(DSTMP.LE.12.0)THEN
              PREM = PREM * 0.80
            ELSE
              PREM = PREM * 0.50
            ENDIF
          CASE(13,15)
            PREM = PREM * 0.90
          CASE(14)
            IF(DSTMP.LE.25.0)THEN
              PREM = PREM * 0.80
            ELSE
              PREM = PREM * 0.50
            ENDIF
          CASE(16)
            PREM = PREM * 0.80
          CASE(17)
            PREM = PREM * 0.70
        END SELECT
C----------
C  ALL OTHER VARIANTS & SPECIES
C----------
      CASE DEFAULT
        PREM = PREM * 1.
C
      END SELECT
      RETURN
C
C
      ENTRY ESASID(VAR,INDXAS)
C----------
C  THIS SUBROUTINE DETERMINES IF QUAKING ASPEN IS IN THIS VARIANT;
C  IF SO, RETURN THE FVS SPECIES INDEX FOR ASPEN IN THIS VARIANT;
C  OTHERWISE RETURN 9999.
C----------
C  DEFINITIONS
C
C   INDXAS -- QUAKING ASPEN SPECIES NUMBER
C----------
      SELECT CASE (VAR)
C
      CASE('AK')
        INDXAS = 19
C
      CASE('CI')
        INDXAS = 13
C
      CASE('EM')
        INDXAS = 12
C
      CASE('IE')
        INDXAS = 18
C
      CASE DEFAULT
        INDXAS = 9999
C
      END SELECT
      RETURN
C
C
      ENTRY NSPREC (VAR,ISPC,NMSPRC,DSTMP)
C----------
C  THIS SUBROUTINE DETERMINES THE NUMBER OF SPROUT RECORDS TO CREATE.
C----------
C  DEFINITIONS
C
C   NMSPRC -- NUMBER OF SPROUT RECORDS TO CREATE
C    DSTMP -- DIAMETER OF PARENT TREE
C----------
      SELECT CASE (VAR)
C----------
C  SET TO 1 FOR: CI - PACIFIC YEW (12)
C                IE - PACIFIC YEW (17)
C
C  OTHERWISE SET TO 2
C
C----------
      CASE('AK')
        SELECT CASE (ISPC)
        CASE(18,20)
          IF(DSTMP.LT.5.0)THEN
            NMSPRC = 1
          ELSE IF(DSTMP.GE.5.0.AND.DSTMP.LT.10.0)THEN
            NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
          ELSE
            NMSPRC = 3
          ENDIF
        CASE(19)
          NMSPRC = 2
        CASE DEFAULT
          NMSPRC = 1
        END SELECT
C
      CASE('CI')
        SELECT CASE (ISPC)
        CASE(12)
          NMSPRC = 1
        CASE(13)
          NMSPRC = 2
        CASE(17)
          IF(DSTMP.LT.5.0)THEN
            NMSPRC = 1
          ELSE IF(DSTMP.GE.5.0.AND.DSTMP.LT.10.0)THEN
            NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
          ELSE
            NMSPRC = 3
          ENDIF
        CASE DEFAULT
          NMSPRC = 1
        END SELECT
C
      CASE('EM')
        SELECT CASE (ISPC)
        CASE(12)
          NMSPRC = 2
        CASE(13:16)
          IF(DSTMP.LT.5.0)THEN
            NMSPRC = 1
          ELSE IF(DSTMP.GE.5.0.AND.DSTMP.LT.10.0)THEN
            NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
          ELSE
            NMSPRC = 3
          ENDIF
        CASE DEFAULT
          NMSPRC = 1
        END SELECT
C
      CASE('IE')
        SELECT CASE (ISPC)
        CASE(18)
          NMSPRC = 2
        CASE(19)
          IF(DSTMP.LT.5.0)THEN
            NMSPRC = 1
          ELSE IF(DSTMP.GE.5.0.AND.DSTMP.LT.10.0)THEN
            NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
          ELSE
            NMSPRC = 3
          ENDIF
        CASE DEFAULT
          NMSPRC = 1
        END SELECT
C----------
C  ALL OTHER VARIANTS & SPECIES
C----------
      CASE DEFAULT
          NMSPRC = 2
C
      END SELECT
C
      RETURN
C
C
      ENTRY ASSPTN (ISHAG,ASBAR,ASTPAR,PREM,TREES)
C----------
C  THIS SUBROUTINE DETERMINES THE NUMBER OF QUAKING ASPEN SPROUTS
C  PER ACRE (SPA) BASED ON POLYNOMIAL FORMULA FROM CROUCH DATA SET,
C  MODIFIED BY PROPORTION OF ASPEN BA CUT BY CHAD KEYSER (FMSC).
C----------
C  DEFINITIONS
C
C    RSHAG -- AGE OF SPROUT RECORD
C      SPA -- POTENTIAL TREES PER ACRE OF ASPEN SPROUTS
C    ASBAR -- ASPEN BASAL AREA HARVESTED (198 IS AVERAGE FOR STOCKED
C               STANDS IN INPUT DATASET)
C   ASTPAR -- TOTAL REMOVED ASPEN TREES PER ACRE
C    TREES -- TREES PER ACRE REPRESENTED BY SPROUT RECORD
C----------
      RSHAG=FLOAT(ISHAG)
      SPA = 40100.45 - 3574.02*RSHAG**2.0 + 554.02*RSHAG**3.0
     &      - 3.5208*RSHAG**5.0 + 0.011797*RSHAG**7.0
      IF(SPA .LT. 2608.)SPA=2608.
      IF(SPA .GT. 30125.)SPA=30125.
      SPA = SPA * ASBAR/198.
      TREES =(PREM/(ASTPAR*2.))*SPA
C
      RETURN
C
C
      ENTRY SPRTHT (VAR,ISPC,SI,IAG,HTSPRT)
C----------
C  THIS SUBROUTINE DETERMINES SPROUT HEIGHT BY VARIANT AND SPECIES.
C
C  EQUATION IS HT = (0.1 + SI/BASEAGE)*AGE
C
C  DEFAULT EQUATION, HTSPRT = 0.5 + 0.5*IAG, IS THE ORIGINAL EQUATION
C  FROM NI REGEN/ESTAB
C----------
C  DEFINITIONS
C
C    HTSPRT -- HEIGHT OF SPROUT RECORD
C----------
      SELECT CASE (VAR)
C----------
C AK: ALASKA
C     SPROUTING SPECIES: 14=AD, 15=RA, 16=PB, 17=AB, 18=BA, 19=AS,
C     20=CW, 21=WI, 22=SU
C----------
      CASE('AK')
        SELECT CASE (ISPC)
        CASE(14,15)
          HTSPRT = (0.1 + SI/100.)*IAG
        CASE(16:22)
          HTSPRT = (0.1 + SI/50.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C CI: CENTRAL IDAHO
C     SPROUTING SPECIES: 12=PY, 13=AS, 17=CW
C----------
      CASE('CI')
        SELECT CASE (ISPC)
        CASE(13)
          HTSPRT = (0.1 + SI/80.)*IAG
        CASE(12,17)
          HTSPRT = (0.1 + SI/100.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C EM: EASTERN MONTANA
C     SPROUTING SPECIES: 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC, 17=PB
C----------
      CASE('EM')
        SELECT CASE (ISPC)
        CASE(12,17)
          HTSPRT = (0.1 + SI/80.)*IAG
        CASE(11,13:16)
          HTSPRT = (0.1 + SI/100.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C IE: INLAND EMPIRE
C     SPROUTING SPECIES: 17=PY, 18=AS, 19=CO, 20=MM, 21=PB
C----------
      CASE('IE')
        SELECT CASE (ISPC)
        CASE(18)
          HTSPRT = (0.1 + SI/80.)*IAG
        CASE(17,19:21)
          HTSPRT = (0.1 + SI/100.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C KT: KOOTENAI, KANIKSU, TALLY LAKE RD OF THE FLATHEAD
C NI: NORTHERN IDAHO
C     SPROUTING SPECIES: NONE
C----------
      CASE('KT','NI')
        SELECT CASE (ISPC)
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C XX: LEFTOVERS
C----------
      CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
C
      END SELECT
C
      RETURN
C
C
      END



