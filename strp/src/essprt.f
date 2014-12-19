      SUBROUTINE ESSPRT(VAR,ISPC,NSPT,PREM,DSTMP)
      IMPLICIT NONE
C----------
C  **ESSPRT--STRP   DATE OF LAST REVISION:   12/16/2014
C
C  SUBROUTINE CONTAINING ENTRY POINTS TO HANDLE COMPUTATIONS FOR
C  VARIOUS STUMP SPROUTING FUNCTIONS WHICH VARY BY VARIANT, SPECIES,
C  AND PARENT-TREE DIAMETER.
C
C  VARIANTS USING THE STRP VERSION: BM, CA, CR, CS, EC, LS, NC, NE,
C  PN, SN, SO, TT, UT, WC, WS
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
      INTEGER IAG,INDXAS,ISHAG,ISPC,NASPRT,NMSPRC,NSPT
      REAL ASTPAR,ASBAR,HTSPRT,PREM,RSHAG,SI,SPA,TREES,DSTMP
C----------
C  VARIANT AND SPECIES SPECIFIC RULES FOR DETERMINING TPA A SPROUT
C  RECORD WILL REPRESENT. ASPEN IS HANDLED SEPERATELY IN ENTRY ASSPTN
C----------
      SELECT CASE (VAR)
C----------
C  CR: CENTRAL ROCKIES
C     PAPER BIRCH (28) SET AT 0.3 SPROUTS/STUMP PER SILVIC OF N. AMERICA
C     ALLIGATOR JUNIPER (29) SET AT 1 SPROUT/STUMP PER JIM YOUTZ R3
C     CHIHUAHUA PINE (36) SET AT 2 SPROUTS/STUMP PER JIM YOUTZ R3
C     OAK SPECIES SET AT 3 SPROUTS: GAMBEL OAK (23),
C     ARIZONA WHITE OAK (24), EMORY OAK (25), BUR OAK (26),
C     SILVERLEAF OAK (27)
C
C  SN: SOUTHERN 
C     BASED ON KEYSER AND LOFTIS, SHORT-TERM STUMP SPROUT DYNAMICS OF 
C     24 UPLAND HARDWOOD TREE SPECIES FOLLOWING REGENERATION HARVESTS
C     IN THE SOUTHERN APPALACHIAN MOUNTAINS, USA
C     
C----------
      CASE('CR')
        SELECT CASE(ISPC)
          CASE(23:27)
            PREM = PREM * 3.
          CASE(28)
            PREM = 0.3/FLOAT(NSPT)
          CASE(29)
            PREM = 1.0/FLOAT(NSPT)
          CASE(36)
            PREM = 2.0/FLOAT(NSPT)
        END SELECT
      CASE('SN')
        SELECT CASE(ISPC)
          CASE(5,18,19,26,30:32,41,51,52,56,82)
            PREM = PREM * 0.94
          CASE(20)
            PREM = PREM * 1 / (1 + EXP(-(4.1975 + (-0.1821 * DSTMP))))
          CASE(22)
            PREM = PREM * 0.73
          CASE(23)
            PREM = PREM * 0.96
          CASE(24,25)
            PREM = PREM * 1 / (1 + EXP(-(3.3670 + (-0.5159 * DSTMP))))
          CASE(27)
            PREM = PREM * 0.95
          CASE(33)
            PREM = PREM * 0.93
          CASE(45)
            PREM = PREM * 0.79
          CASE(46)
            PREM = PREM * 0.95
          CASE(47)
            PREM = PREM * 0.69
          CASE(54)
            PREM = PREM * 0.72
          CASE(57)
            PREM = PREM * 0.97
          CASE(63)
            PREM = PREM * 1 / (1 + EXP(-(2.4608 + (-0.3093 * DSTMP))))
          CASE(64)
            PREM = PREM * 1 / (1 + EXP(-(3.8897 + (-0.2260 * DSTMP))))
          CASE(74)
            PREM = PREM * 0.78
          CASE(75)
            PREM = PREM * 1 / (1 + EXP(-(3.2586 + (-0.1120 * DSTMP))))
          CASE(78)
            PREM = PREM * 1 / (1 + EXP(-(3.1070 + (-0.2128 * DSTMP))))
          CASE(80)
            PREM = PREM * 0.86
          CASE(83)
            PREM = PREM * 0.99
          CASE DEFAULT
            PREM = PREM * 1 / (1 + EXP(-(2.7386 + (-0.1076 * DSTMP))))
        END SELECT
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
      CASE('BM')
        INDXAS = 15
      CASE('CA')
        INDXAS = 44
      CASE('CR')
        INDXAS = 20
      CASE('CS')
        INDXAS = 76
      CASE('LS', 'ON')
        INDXAS = 41
      CASE('NE')
        INDXAS = 49
      CASE('SO')
        INDXAS = 24
      CASE('TT','UT')
        INDXAS = 6
      CASE('WC','PN')
        INDXAS = 26
      CASE('WS')
        INDXAS = 36
      CASE('BC')
        INDXAS = 12
      CASE DEFAULT
        INDXAS = 9999
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
C  SET TO 1 FOR: BM - PACIFIC YEW (13) 
C                CA - PACIFIC YEW (24) AND CALIFORNIA NUTMEG (47)
C                PN - PACIFIC YEW (33)
C                SO - PACIFIC YEW (20)
C                WC - PACIFIC YEW (33)
C                SN - DEFAULT AND SP < 7.0"
C  SET TO 6 FOR: SN - STUMP + 5 ROOT SUCKERS: AMERICAN BEECH (33),
C                     BIGTOOTH ASPEN(61), BLACK LOCUST (80), AND
C                     SASSAFRAS (82)
C
C  OTHERWISE SET TO 2
C----------
      CASE('BM')
        SELECT CASE (ISPC)
        CASE(13)
          NMSPRC = 1
        CASE DEFAULT
          NMSPRC = 2
        END SELECT
      CASE('CA')
        SELECT CASE (ISPC)
        CASE(24,47)
          NMSPRC = 1
        CASE DEFAULT
          NMSPRC = 2
        END SELECT
      CASE('PN','WC')
        SELECT CASE (ISPC)
        CASE(33)
          NMSPRC = 1
        CASE DEFAULT
          NMSPRC = 2
        END SELECT
      CASE('SO')
        SELECT CASE (ISPC)
        CASE(20)
          NMSPRC = 1
        CASE DEFAULT
          NMSPRC = 2
        END SELECT
      CASE('SN')
        SELECT CASE (ISPC)
        CASE(5)
          IF(DSTMP.LT.7.0)THEN
            NMSPRC = 1
          ELSE
            NMSPRC = 0
          ENDIF
        CASE(33,61,80,82)
          NMSPRC = 6
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
C BM: BLUE MOUNTAINS
C     SPROUTING SPECIES: 13=PY, 15=AS, 16=CW
C----------
      CASE('BM')
        SELECT CASE (ISPC)
        CASE(13,16)
          HTSPRT = (0.1 + SI/100.)*IAG
        CASE(15)
          HTSPRT = (0.1 + SI/80.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C CA: INLAND CALIFORNIA / SOUTHERN CASCADES
C     SPROUTING SPECIES:   24=PY, 26=LO, 27=CY, 28=BL, 29=EO, 30=WO,
C     31=BO, 32=VO, 33=IO, 34=BM, 35=BU, 36=RA, 37=MA, 38=GC, 39=DG,
C     40=FL, 41=WN, 42=TO, 43=SY, 44=AS, 45=CW, 46=WI, 47=CN, 48=CL
C----------
      CASE('CA')
        SELECT CASE (ISPC)
        CASE(24,26:48)
          HTSPRT = (0.1 + SI/50.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C CR: CENTRAL ROCKIES
C     SPROUTING SPECIES:   20=AS, 21=NC, 22=PW, 23=GO, 24=AW, 25=EM,
C     26=BK, 27=SO, 28=PB, 29=AJ, 36=CI
C----------
      CASE('CR')
        SELECT CASE (ISPC)
        CASE(21:29,36)
          HTSPRT = (0.1 + SI/100.)*IAG
        CASE(20)
          HTSPRT = (0.1 + SI/80.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C CS: CENTRAL STATES
C     SPROUTING SPECIES:    8=WN,  9=BN, 10=TL, 11=TS, 12=WT, 13=BG,
C     14=HS, 15=SH, 16=SL, 17=MH, 18=PH, 19=HI, 20=WH, 21=BH, 22=PE,
C     23=BI, 24=AB, 25=BA, 26=PA, 27=UA, 28=EC, 29=RM, 30=BE, 31=SV,
C     32=BC, 33=AE, 34=SG, 35=HK, 36=WE, 37=EL, 38=SI, 39=RL, 40=RE,
C     41=YP, 42=BW, 43=SM, 44=AS, 45=WA, 46=GA, 47=WO, 48=RO, 49=SK,
C     50=BO, 51=SO, 52=BJ, 53=CK, 54=SW, 55=BR, 56=SN, 57=PO, 58=DO,
C     59=CO, 60=PN, 61=CB, 62=QI, 63=OV, 64=WK, 65=NK, 66=WL, 67=QS,
C     69=SS, 70=OB, 71=CA, 72=PS, 73=HL, 74=BP, 75=BT, 76=QA, 77=BK,
C     79=SY, 80=BY, 81=RB, 82=SU, 83=WI, 84=BL, 86=AH, 87=RD, 88=DW,
C     89=HT, 90=KC, 91=OO, 92=CT, 93=MV, 94=MB, 95=HH, 96=SD
C----------
      CASE('CS')
        SELECT CASE (ISPC)
        CASE(8:67,69:77,79:84,86:96)
          HTSPRT = (0.1 + SI/50.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C EC: EAST CASCADES
C     SPROUTING SPECIES: NONE (but there will be in the 32 species version)
C----------
      CASE('EC')
        SELECT CASE (ISPC)
        CASE(99999)
          HTSPRT = (0.1 + SI/50.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C LS: LAKE STATES
C     SPROUTING SPECIES:   15=BA, 16=GA, 17=EC, 18=SV, 19=RM, 20=BC,
C     21=AE, 22=RL, 23=RE, 24=YB, 25=BW, 26=SM, 27=BM, 28=AB, 29=WA,
C     30=WO, 31=SW, 32=BR, 33=CK, 34=RO, 35=BO, 36=NP, 37=BH, 38=PH,
C     39=SH, 40=BT, 41=QA, 42=BP, 43=PB, 45=BN, 46=WN, 47=HH, 48=BK,
C     50=BE, 51=ST, 52=MM, 53=AH, 54=AC, 55=HK, 56=DW, 57=HT, 58=AP,
C     59=BG, 60=SY, 61=PR, 62=CC, 63=PL, 64=WI, 65=BL, 66=DM, 67=SS,
C     68=MA
C----------
      CASE('LS','ON')
        SELECT CASE (ISPC)
        CASE(15:43,45:48,50:68)
          HTSPRT = (0.1 + SI/50.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C NC: NORTHERN CALIFORNIA
C     SPROUTING SPECIES:    5=MA,  7=BO,  8=TO
C----------
      CASE('NC')
        SELECT CASE (ISPC)
        CASE(5,7,8)
          HTSPRT = (0.1 + SI/50.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C NE: NORTHEAST STATES
C     SPROUTING SPECIES:   26=RM, 27=SM, 28=BM, 29=SV, 30=YB, 31=SB,
C     32=RB, 33=PB, 34=GB, 35=HI, 36=PH, 37=SL, 38=SH, 39=MH, 40=AB,
C     41=AS, 42=WA, 43=BA, 44=GA, 45=PA, 46=YP, 47=SU, 48=CT, 49=QA,
C     50=BP, 51=EC, 52=BT, 53=PY, 54=BC, 55=WO, 56=BR, 57=CK, 58=PO,
C     59=OK, 60=SO, 61=QI, 62=WK, 63=PN, 64=CO, 65=SW, 66=SN, 67=RO,
C     68=SK, 69=BO, 70=CB, 72=BU, 73=YY, 74=WR, 75=HK, 76=PS, 77=HY,
C     78=BN, 79=WN, 80=OO, 81=MG, 82=MV, 83=AP, 84=WT, 85=BG, 86=SD,
C     87=PW, 88=SY, 89=WL, 90=BK, 91=BL, 92=SS, 93=BW, 94=WB, 95=EL,
C     96=AE, 97=RL, 99=BE,100=ST,101=AI,102=SE,103=AH,104=DW,105=HT,
C    106=HH,107=PL,108=PR
C----------
      CASE('NE')
        SELECT CASE (ISPC)
        CASE(26:70,72:97,99:108)
          HTSPRT = (0.1 + SI/50.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C SN: SOUTHERN STATES
C     SPROUTING SPECIES:   15=BY, 16=PC, 18=FM, 19=BE, 20=RM, 21=SV,
C     22=SM, 23=BU, 24=BB, 25=SB, 26=AH, 27=HI, 28=CA, 29=HB, 30=RD,
C     31=DW, 32=PS, 33=AB, 34=AS, 35=WA, 36=BA, 37=GA, 38=HL, 39=LB,
C     40=HA, 41=HY, 42=BN, 43=WN, 44=SU, 45=YP, 46=MG, 47=CT, 48=MS,
C     49=MV, 50=ML, 51=AP, 52=MB, 53=WT, 54=BG, 55=TS, 56=HH, 57=SD,
C     59=SY, 60=CW, 61=BT, 62=BC, 63=WO, 64=SO, 65=SK, 66=CB, 67=TO,
C     68=LK, 69=OV, 70=BJ, 71=SN, 72=CK, 73=WK, 74=CO, 75=RO, 76=QS,
C     77=PO, 78=BO, 79=LO, 80=BK, 81=WI, 82=SS, 83=BW, 84=EL, 85=WE,
C     86=AE, 87=RL
C----------
      CASE('SN')
        SELECT CASE (ISPC)
        CASE(15,16,18:57,59:87)
          HTSPRT = (0.1 + SI/50.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C SO: SOUTHERN OREGON / NORTHEASTERN CALIFORNIA
C     SPROUTING SPECIES:   20=PY, 21=WA, 22=RA, 23=BM, 24=AS, 25=CW,
C     26=CH, 27=WO, 28=WI, 29=GC, 31=MB
C----------
      CASE('SO')
        SELECT CASE (ISPC)
        CASE(20,21,23,25,26,28,29,31)
          HTSPRT = (0.1 + SI/100.)*IAG
        CASE(24)
          HTSPRT = (0.1 + SI/80.)*IAG
        CASE(27)
          HTSPRT = (0.1 + SI/50.)*IAG
        CASE(22)
          HTSPRT = (0.1 + SI/20.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C TT: TETONS
C     SPROUTING SPECIES:    6=AS, 13=BI, 14=MM, 15=NC
C----------
      CASE('TT')
        SELECT CASE (ISPC)
        CASE(13,15)
          HTSPRT = (0.1 + SI/100.)*IAG
        CASE(6,14)
          HTSPRT = (0.1 + SI/80.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C UT: UTAH
C     SPROUTING SPECIES:    6=AS, 13=GO, 18=NC, 19=FC, 21=BI, 22=BE
C----------
      CASE('UT')
        SELECT CASE (ISPC)
        CASE(13,18,19,21,22)
          HTSPRT = (0.1 + SI/100.)*IAG
        CASE(6)
          HTSPRT = (0.1 + SI/80.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C WC: WEST CASCADES
C PN: PACIFIC NORTHWEST
C     SPROUTING SPECIES:   17=RW, 21=BM, 22=RA, 23=WA, 24=PB, 25=GC,
C     26=AS, 27=CW, 28=WO, 33=PY, 34=DG, 35=HT, 36=CH, 37=WI
C----------
      CASE('WC','PN')
        SELECT CASE (ISPC)
        CASE(17,21,23:27,33:37)
          HTSPRT = (0.1 + SI/100.)*IAG
        CASE(28)
          HTSPRT = (0.1 + SI/300.)*IAG
        CASE(22)
          HTSPRT = (0.1 + SI/20.)*IAG
        CASE DEFAULT
          HTSPRT = 0.5 + 0.5*IAG
        END SELECT
C----------
C WS: WESTERN SIERRA NEVADA
C     SPROUTING SPECIES:   23=RW, 28=LO, 29=CY, 30=BL, 31=BO, 32=VO,
C     33=IO, 34=TO, 35=GC, 36=AS, 37=CL, 38=MA, 39=DG, 40=BM
C----------
      CASE('WS')
        SELECT CASE (ISPC)
        CASE(23,28:40)
          HTSPRT = (0.1 + SI/50.)*IAG
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



