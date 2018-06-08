      SUBROUTINE ESSPRT(VAR,ISPC,NSPT,PREM,DSTMP)
      IMPLICIT NONE
C----------
C ESTB $Id: essprt.f 0000 2018-02-14 00:00:00Z gedixon $
C
C  SUBROUTINE CONTAINING ENTRY POINTS TO HANDLE COMPUTATIONS FOR
C  VARIOUS STUMP SPROUTING FUNCTIONS WHICH VARY BY VARIANT AND 
C  SPECIES.
C
C  VARIANTS USING THE ESTB VERSION: AK, AC, CI, EM, IE, KT, NI
C----------
C
COMMONS
C
C  NO COMMON BLOCKS ARE CURRENTLY NEEDED.
C
COMMONS
C
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
C----------
      SELECT CASE (VAR)
C  
      CASE('AK')
        SELECT CASE(ISPC)
          CASE(10)
            IF(DSTMP.LT.25.9)THEN
              PREM = PREM * ((99.9999-3.8462 * DSTMP)/100)
            ELSE
              PREM = 0.
            ENDIF
          CASE(11)
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
C  DETERMINE IF QUAKING ASPEN IS IN THIS VARIANT; IF SO, RETURN THE
C  FVS SPECIES INDEX FOR ASPEN IN THIS VARIANT; OTHERWISE RETURN 9999.
C----------
      SELECT CASE (VAR)
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
C  DETERMINE NUMBER OF SPROUT RECORDS TO CREATE
C----------
      SELECT CASE (VAR)
C----------
C  SET TO 1 FOR: CI - PACIFIC YEW (12)
C                IE - PACIFIC YEW (17)
C
C  OTHERWISE SET TO 2
C----------
      CASE('CI')
        SELECT CASE (ISPC)
        CASE(12)
          NMSPRC = 1
        CASE(13)
          NMSPRC = 2  
        CASE(17)
          IF(DSTMP.LT.5.0)THEN
            NMSPRC = 1
          ELSE IF(DSTMP.GE.5.0.AND.DSTMP.LE.10.0)THEN
            NMSPRC = INT(-1.0 + 0.4 * DSTMP)
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
          ELSE IF(DSTMP.GE.5.0.AND.DSTMP.LE.10.0)THEN
            NMSPRC = INT(-1.0 + 0.4 * DSTMP)
          ELSE
            NMSPRC = 3 
          ENDIF
        CASE DEFAULT
          NMSPRC = 1
        END SELECT
C
      CASE('AK')
        SELECT CASE (ISPC)
        CASE(11)
          IF(DSTMP.LT.5.0)THEN
            NMSPRC = 1
          ELSE IF(DSTMP.GE.5.0.AND.DSTMP.LE.10.0)THEN
            NMSPRC = INT(-1.0 + 0.4 * DSTMP)
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
          ELSE IF(DSTMP.GE.5.0.AND.DSTMP.LE.10.0)THEN
            NMSPRC = INT(-1.0 + 0.4 * DSTMP)
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
C  DETERMINE NUMBER OF QUAKING ASPEN SPROUTS PER ACRE (SPA)
C  BASED ON POLYNOMIAL FORMULA FROM CROUCH DATA SET, MODIFIED
C  BY PROPORTION OF ASPEN BA CUT BY CHAD KEYSER (FMSC).
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
C  DETERMINE SPROUT HEIGHT BY VARIANT AND SPECIES.
C
C  EQUATION IS HT = (0.1 + SI/BASEAGE)*AGE
C
C  DEFAULT EQUATION, HTSPRT = 0.5 + 0.5*IAG, IS THE ORIGINAL EQUATION
C  FROM NI REGEN/ESTAB
C----------
      SELECT CASE (VAR)
C----------
C AK: SOUTHEAST ALASKA / COASTAL BC
C AN: INTERIOR ALASKA
C     SPROUTING SPECIES: 10=RA, 11=CW
C----------
      CASE('AK','AC')
        SELECT CASE (ISPC)
        CASE(10,11)
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



