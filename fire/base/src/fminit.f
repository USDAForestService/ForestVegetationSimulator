      SUBROUTINE FMINIT
      IMPLICIT NONE
C----------
C  **FMINIT  FIRE--DATE OF LAST REVISION: 07/12/10
C----------
C  PURPOSE:
C      INITIALIZE VARIABLES FOR THE FIRE MODEL
C----------
C
C  CALLED FROM: INITRE
C
C  CALL LIST DEFINITIONS:
C
C  LOCAL VARIABLE DEFINITIONS:
C
C----------
C  PARAMETER INCLUDE FILES.
C----------
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
C----------
C  COMMON INCLUDE FILES.
C----------
      INCLUDE 'CONTRL.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
C----------
C  VARIABLE DECLARATIONS.
C----------
      LOGICAL LRET
      INTEGER I, J, K, L
      REAL    FALLM2
C----------
C  VARIABLE INITIALIZATIONS.
C----------
      HARVYR = 0
      NSNAG  = 0
      IFTYR = 0
      ISALVC = 0
      ISALVS = 0
      NSNAGSALV=0
C
      DO I=1,4
        PFLAM(I)=0.
        POTKIL(I)=0.
        POTFSR(I)=0.
        IF (I.LE.2) THEN
          POTVOL(I)=0.
          POTTYP(I)=0
          POTRINT(I)=0.
        ENDIF
      ENDDO
C
      DO J=1,MXSNAG
        DENIH(J) = 0.
        DENIS(J) = 0.
        SALVSPA(J,1) = 0.
        SALVSPA(J,2) = 0.
C
        HTIHSALV(I)=0.
        HTISSALV(I)=0.
        SPSSALV(I)=0
        DBHSSALV(I)=0.
        HARDSALV(I)=.FALSE.
        HTDEADSALV(I)=0.
C        
      ENDDO
C
C     INITIALIZE ACTIVITY FUELS VARS
C
      SMALL   = 0.0
      LARGE   = 0.0
      OSMALL  = 0.0
      OLARGE  = 0.0
      SLCHNG  = 0.0
      LATFUEL = .FALSE.
C
C     INITIALIZE CA-VARIANT (WS,NC,IC) SHRUB DELAY MODEL VARS
C
      FM89YR  = -1
      LATSHRB = .FALSE.
      LPRV89  = .FALSE.
      PERCOV  = 0.0
      CCCHNG  = 0.0
      PRV8    = 0.0
      PRV9    = 0.0
C
      DO I=1,5
        DO J=1,4
          FUAREA(I,J)=0.0
        ENDDO
      ENDDO
C
      IFMYR1 =  0
      IFMYR2 =  0
      BURNYR = -1
      PBURNYR = -1
C
      FALLM2 = 0.0
      CWDCUT = 0.0
C
      JLOUT(1)=0
      JLOUT(2)=0
      JLOUT(3)=0
      JSNOUT=35
C
      PRESVL(1,1)=0.
      PRESVL(2,1)=0.
C
      LFMON  = .FALSE.
      LFMON2 = .TRUE.
      LHEAD  = .TRUE.
      LSHEAD = .TRUE.
      ISNGSM = -1
      LANHED = .TRUE.
      LDHEAD = .TRUE.
      LDYNFM = .TRUE.
C----------
C  INITIALIZE POTENTIAL FIRE SEASON, POTENTIAL FIRE PERCENT AREA BURNED
C  THE FIRE SEASON, AND SOIL TYPE
C----------
      POTPAB(1)=100.
      POTPAB(2)=100.
      POTSEAS(1)=1
      POTSEAS(2)=1
      BURNSEAS = 1
      SOILTP = 3
C----------
C  INITIALIZE VARIABLE RELATED TO CALCULATION OF CBH AND CBD
C----------
      ICBHMT = 0
      CANMHT = 6.0
      ICANSP = 0
      CBHCUT = 30.0
      FOLMC  = 100.0
C----------
C  INITIALIZE VARIABLES RELATING TO FUEL MODELS
C  MAKE SURE THAT THE USER-DEFINED FUEL WEIGHTINGS ARE
C  INITIALIZED AND TURNED OFF.
C----------
      LUSRFM = .FALSE.
      DO I = 1,4
        FMDUSR(I) = 0
        FWTUSR(I) = 0.0
      ENDDO
C----------
C  INITIALIZE ALL THE FUEL MODEL VARIABLES. THESE CAN
C  BE CHANGED BY THE KEYWORD DEFULMOD
C  FROM FAX FROM ELIZABETH REINHARDT
C----------
      DO I = 1,MXDFMD
        SURFVL(I,1,2) = 109
        SURFVL(I,1,3) = 30
        SURFVL(I,2,1) = 1500
        SURFVL(I,2,2) = 1500
C----------
C  AND INITIALIZE ALL THE VARIABLES THAT ALWAYS CHANGE WITH FMD
C----------
        FMLOAD(I,1,1) = 0.0
        FMLOAD(I,1,2) = 0.0
        FMLOAD(I,1,3) = 0.0
        FMLOAD(I,1,4) = 0.0
        FMLOAD(I,2,1) = 0.0
        FMLOAD(I,2,2) = 0.0
        FMDEP(I)      = 0.0
        MOISEX(I)     = 0.0
      ENDDO
C----------
C  SHORT GRASS - FMD 1
C----------
      I = 1
      SURFVL(I,1, 1) = 3500
      FMLOAD(I,1, 1) =    0.034
      FMDEP(I)       =    1.0
      MOISEX(I)      =    0.12
C----------
C  TIMBER (GRASS & UNDERSTORY) - FMD 2
C----------
      I = 2
      SURFVL(I,1, 1) = 3000
      FMLOAD(I,1, 1) =    0.092
      FMLOAD(I,1, 2) =    0.046
      FMLOAD(I,1, 3) =    0.023
      FMLOAD(I,2, 2) =    0.023
      FMDEP(I)       =    1.0
      MOISEX(I)      =    0.15
C----------
C  TALL GRASS (2.5 FT) - FMD 3
C----------
      I = 3
      SURFVL(I,1, 1) = 1500
      FMLOAD(I,1, 1) =    0.138
      FMDEP(I)       =    2.5
      MOISEX(I)      =    0.25
C----------
C  CHAPARRAL (6 FT) - FMD 4
C----------
      I = 4
      SURFVL(I,1, 1) = 2000
      FMLOAD(I,1, 1) =    0.230
      FMLOAD(I,1, 2) =    0.184
      FMLOAD(I,1, 3) =    0.092
      FMLOAD(I,2, 1) =    0.230
      FMDEP(I)       =    6.0
      MOISEX(I)      =    0.20
C----------
C  BRUSH (2 FT) - FMD 5
C----------
      I = 5
      SURFVL(I,1, 1) = 2000
      FMLOAD(I,1, 1) =    0.046
      FMLOAD(I,1, 2) =    0.023
      FMLOAD(I,2, 1) =    0.092
      FMDEP(I)       =    2.0
      MOISEX(I)      =    0.20
C----------
C  DORMANT BRUSH, HARDWOOD SLASH - FMD 6
C----------
      I = 6
      SURFVL(I,1, 1) = 1750
      FMLOAD(I,1, 1) =    0.069
      FMLOAD(I,1, 2) =    0.115
      FMLOAD(I,1, 3) =    0.092
      FMDEP(I)       =    2.5
      MOISEX(I)      =    0.25
C----------
C  SOUTHERN ROUGH - FMD 7
C----------
      I = 7
      SURFVL(I,1, 1) = 1750
      SURFVL(I,2, 1) = 1550
      FMLOAD(I,1, 1) =    0.052
      FMLOAD(I,1, 2) =    0.086
      FMLOAD(I,1, 3) =    0.069
      FMLOAD(I,2, 1) =    0.017
      FMDEP(I)       =    2.5
      MOISEX(I)      =    0.40
C----------
C  CLOSED TIMBER LITTER - FMD 8
C----------
      I = 8
      SURFVL(I,1, 1) = 2000
      FMLOAD(I,1, 1) =    0.069
      FMLOAD(I,1, 2) =    0.046
      FMLOAD(I,1, 3) =    0.115
      FMDEP(I)       =    0.2
      MOISEX(I)      =    0.3
C----------
C  HARDWOOD LITTER - FMD 9
C----------
      I = 9
      SURFVL(I,1, 1) = 2500
      FMLOAD(I,1, 1) =    0.134
      FMLOAD(I,1, 2) =    0.019
      FMLOAD(I,1, 3) =    0.007
      FMDEP(I)       =    0.2
      MOISEX(I)      =    0.25
C----------
C  TIMBER (LITTER & UNDERSTORY) - FMD 10
C----------
      I = 10
      SURFVL(I,1, 1) = 2000
      FMLOAD(I,1, 1) =    0.138
      FMLOAD(I,1, 2) =    0.092
      FMLOAD(I,1, 3) =    0.23
      FMLOAD(I,2, 1) =    0.092
      FMDEP(I)       =    1.0
      MOISEX(I)      =    0.25
C----------
C  LIGHT LOGGING SLASH (AN ACTIVITIES FUEL MODEL) - FMD 11
C----------
      I = 11
      SURFVL(I,1, 1) = 1500
      FMLOAD(I,1, 1) =    0.069
      FMLOAD(I,1, 2) =    0.207
      FMLOAD(I,1, 3) =    0.253
      FMDEP(I)       =    1.0
      MOISEX(I)      =    0.15
C----------
C  MEDIUM LOGGING SLASH - FMD 12
C----------
      I = 12
      SURFVL(I,1, 1) = 1500
      FMLOAD(I,1, 1) =    0.184
      FMLOAD(I,1, 2) =    0.644
      FMLOAD(I,1, 3) =    0.759
      FMDEP(I)       =    2.3
      MOISEX(I)      =    0.2
C----------
C  HEAVY LOGGING SLASH - FMD 13
C----------
      I = 13
      SURFVL(I,1, 1) = 1500
      FMLOAD(I,1, 1) =    0.322
      FMLOAD(I,1, 2) =    1.058
      FMLOAD(I,1, 3) =    1.288
      FMDEP(I)       =    3.0
      MOISEX(I)      =    0.25
C----------
C  A MODIFICATION OF 11, OHERWISE KNOWN AS 11A OR 14 OR 111
C----------
      I = 14
      SURFVL(I,1, 1) = 1500
      FMLOAD(I,1, 1) =    0.126
      FMLOAD(I,1, 2) =    0.426
      FMLOAD(I,1, 3) =    0.506
      FMDEP(I)       =    1.8
      MOISEX(I)      =    0.20
C----------
C  RAY HERMIT (R5) -
C  25  OLDER PLANTATION GREATER THAN 25 YEARS WITH SHRUB UNDERSTORY AND LOW CROWN BASES
C  26  MODIFIED MODEL 4 BRUSH MODEL REDUCED FUELBED FMDEP(I) AND LOADINGS
C----------
      I = 25
      SURFVL(I,1, 1) = 2000
      FMLOAD(I,1, 1) =    0.069
      FMLOAD(I,1, 2) =    0.069
      FMLOAD(I,1, 3) =    0.092
      FMLOAD(I,2, 1) =    0.207
      FMDEP(I)       =    3.5
      MOISEX(I)      =    0.25
C
      I = 26
      SURFVL(I,1, 1) = 2000
      FMLOAD(I,1, 1) =    0.1242
      FMLOAD(I,1, 2) =    0.1242
      FMLOAD(I,1, 3) =    0.0828
      FMLOAD(I,2, 1) =    0.1656
      FMDEP(I)       =    3.6
      MOISEX(I)      =    0.35
C----------
C  ADD IN THE 40 NEW FUEL MODELS, FOLLOWING THEIR NUMBERING SCHEME
C----------
C----------
C  GR1 (101) SHORT SPARSE DRY CLIMATE GRASS
C----------
      I = 101
      SURFVL(I,1,1) = 2200
      SURFVL(I,2,2) = 2000
      FMLOAD(I,1, 1) =    0.005
      FMLOAD(I,2, 2) =    0.014
      FMDEP(I)       =    0.4
      MOISEX(I)      =    0.15
C----------
C  GR2 (102) LOW LOAD DRY CLIMATE GRASS
C----------
      I = 102
      SURFVL(I,1,1) = 2000
      SURFVL(I,2,2) = 1800
      FMLOAD(I,1, 1) =    0.005
      FMLOAD(I,2, 2) =    0.046
      FMDEP(I)       =    1.0
      MOISEX(I)      =    0.15
C----------
C  GR3 (103) LOW LOAD VERY COARSE HUMID CLIMATE GRASS
C----------
      I = 103
      SURFVL(I,1,1) = 1500
      SURFVL(I,2,2) = 1300
      FMLOAD(I,1, 1) =    0.005
      FMLOAD(I,1, 2) =    0.018
      FMLOAD(I,2, 2) =    0.069
      FMDEP(I)       =    2.0
      MOISEX(I)      =    0.30

C----------
C  GR4 (104) MODERATE LOAD DRY CLIMATE GRASS
C----------
      I = 104
      SURFVL(I,1,1) = 2000
      SURFVL(I,2,2) = 1800
      FMLOAD(I,1, 1) =    0.011
      FMLOAD(I,2, 2) =    0.087
      FMDEP(I)       =    2.0
      MOISEX(I)      =    0.15

C----------
C  GR5 (105) LOW LOAD HUMID CLIMATE GRASS
C----------
      I = 105
      SURFVL(I,1,1) = 1800
      SURFVL(I,2,2) = 1600
      FMLOAD(I,1, 1) =    0.018
      FMLOAD(I,2, 2) =    0.115
      FMDEP(I)       =    1.5
      MOISEX(I)      =    0.40

C----------
C  GR6 (106) MODERATE LOAD HUMID CLIMATE GRASS
C----------
      I = 106
      SURFVL(I,1,1) = 2200
      SURFVL(I,2,2) = 2000
      FMLOAD(I,1, 1) =    0.005
      FMLOAD(I,2, 2) =    0.156
      FMDEP(I)       =    1.5
      MOISEX(I)      =    0.40


C----------
C  GR7 (107) HIGH LOAD DRY CLIMATE GRASS
C----------
      I = 107
      SURFVL(I,1,1) = 2000
      SURFVL(I,2,2) = 1800
      FMLOAD(I,1, 1) =    0.046
      FMLOAD(I,2, 2) =    0.248
      FMDEP(I)       =    3.0
      MOISEX(I)      =    0.15

C----------
C  GR8 (108) HIGH LOAD VERY COARSE HUMID CLIMATE GRASS
C----------
      I = 108
      SURFVL(I,1,1) = 1500
      SURFVL(I,2,2) = 1300
      FMLOAD(I,1, 1) =    0.023
      FMLOAD(I,1, 2) =    0.046
      FMLOAD(I,2, 2) =    0.335
      FMDEP(I)       =    4.0
      MOISEX(I)      =    0.30
C----------
C  GR9 (109) VERY HIGH LOAD HUMID CLIMATE GRASS
C----------
      I = 109
      SURFVL(I,1,1) = 1800
      SURFVL(I,2,2) = 1600
      FMLOAD(I,1, 1) =    0.046
      FMLOAD(I,1, 2) =    0.046
      FMLOAD(I,2, 2) =    0.413
      FMDEP(I)       =    5.0
      MOISEX(I)      =    0.40

C----------
C  GS1 (121) LOW LOAD DRY CLIMATE GRASS-SHRUB
C----------
      I = 121
      SURFVL(I,1,1) = 2000
      SURFVL(I,2,1) = 1800
      SURFVL(I,2,2) = 1800
      FMLOAD(I,1, 1) =    0.009
      FMLOAD(I,2, 1) =    0.03
      FMLOAD(I,2, 2) =    0.023
      FMDEP(I)       =    0.9
      MOISEX(I)      =    0.15
C----------
C  GS2 (122) MODERATE LOAD DRY CLIMATE GRASS-SHRUB
C----------
      I = 122
      SURFVL(I,1,1) = 2000
      SURFVL(I,2,1) = 1800
      SURFVL(I,2,2) = 1800
      FMLOAD(I,1, 1) =    0.023
      FMLOAD(I,1, 2) =    0.023
      FMLOAD(I,2, 1) =    0.046
      FMLOAD(I,2, 2) =    0.028
      FMDEP(I)       =    1.5
      MOISEX(I)      =    0.15

C----------
C  GS3 (123) MODERATE LOAD HUMID CLIMATE GRASS-SHRUB
C----------
      I = 123
      SURFVL(I,1,1) = 1800
      SURFVL(I,2,1) = 1600
      SURFVL(I,2,2) = 1600
      FMLOAD(I,1, 1) =    0.014
      FMLOAD(I,1, 2) =    0.011
      FMLOAD(I,2, 1) =    0.057
      FMLOAD(I,2, 2) =    0.067
      FMDEP(I)       =    1.8
      MOISEX(I)      =    0.40

C----------
C  GS4 (124) HIGH LOAD HUMID CLIMATE GRASS-SHRUB
C----------
      I = 124
      SURFVL(I,1,1) = 1800
      SURFVL(I,2,1) = 1600
      SURFVL(I,2,2) = 1600
      FMLOAD(I,1, 1) =    0.087
      FMLOAD(I,1, 2) =    0.014
      FMLOAD(I,1, 3) =    0.005
      FMLOAD(I,2, 1) =    0.326
      FMLOAD(I,2, 2) =    0.156
      FMDEP(I)       =    2.1
      MOISEX(I)      =    0.40
C----------
C  SH1 (141) LOW LOAD DRY CLIMATE SHRUB
C----------
      I = 141
      SURFVL(I,1,1) = 2000
      SURFVL(I,2,1) = 1600
      SURFVL(I,2,2) = 1800
      FMLOAD(I,1, 1) =    0.011
      FMLOAD(I,1, 2) =    0.011
      FMLOAD(I,2, 1) =    0.060
      FMLOAD(I,2, 2) =    0.007
      FMDEP(I)       =    1.0
      MOISEX(I)      =    0.15
C----------
C  SH2 (142) MODERATE LOAD DRY CLIMATE SHRUB
C----------
      I = 142
      SURFVL(I,1,1) = 2000
      SURFVL(I,2,1) = 1600
      FMLOAD(I,1, 1) =    0.062
      FMLOAD(I,1, 2) =    0.110
      FMLOAD(I,1, 3) =    0.034
      FMLOAD(I,2, 1) =    0.177
      FMDEP(I)       =    1.0
      MOISEX(I)      =    0.15

C----------
C  SH3 (143) MODERATE LOAD HUMID CLIMATE SHRUB
C----------
      I = 143
      SURFVL(I,1,1) = 1600
      SURFVL(I,2,1) = 1400
      FMLOAD(I,1, 1) =    0.021
      FMLOAD(I,1, 2) =    0.138
      FMLOAD(I,2, 1) =    0.285
      FMDEP(I)       =    2.4
      MOISEX(I)      =    0.40
C----------
C  SH4 (144) LOW LOAD HUMID CLIMATE TIMBER-SHRUB
C----------
      I = 144
      SURFVL(I,1,1) = 2000
      SURFVL(I,2,1) = 1600
      SURFVL(I,2,2) = 1800
      FMLOAD(I,1, 1) =    0.039
      FMLOAD(I,1, 2) =    0.053
      FMLOAD(I,1, 3) =    0.009
      FMLOAD(I,2, 1) =    0.117
      FMDEP(I)       =    3.0
      MOISEX(I)      =    0.30
C----------
C  SH5 (145) HIGH LOAD DRY CLIMATE SHRUB
C----------
      I = 145
      SURFVL(I,1,1) = 750
      SURFVL(I,2,1) = 1600
      FMLOAD(I,1, 1) =    0.165
      FMLOAD(I,1, 2) =    0.096
      FMLOAD(I,2, 1) =    0.133
      FMDEP(I)       =    6.0
      MOISEX(I)      =    0.15
C----------
C  SH6 (146) LOW LOAD HUMID CLIMATE SHRUB
C----------
      I = 146
      SURFVL(I,1,1) = 750
      SURFVL(I,2,1) = 1600
      FMLOAD(I,1, 1) =    0.133
      FMLOAD(I,1, 2) =    0.067
      FMLOAD(I,2, 1) =    0.064
      FMDEP(I)       =    2.0
      MOISEX(I)      =    0.30
C----------
C  SH7 (147) VERY HIGH LOAD DRY CLIMATE SHRUB
C----------
      I = 147
      SURFVL(I,1,1) = 750
      SURFVL(I,2,1) = 1600
      FMLOAD(I,1, 1) =    0.161
      FMLOAD(I,1, 2) =    0.243
      FMLOAD(I,1, 3) =    0.101
      FMLOAD(I,2, 1) =    0.156
      FMDEP(I)       =    6.0
      MOISEX(I)      =    0.15

C----------
C  SH8 (148) HIGH LOAD HUMID CLIMATE SHRUB
C----------
      I = 148
      SURFVL(I,1,1) = 750
      SURFVL(I,2,1) = 1600
      FMLOAD(I,1, 1) =    0.094
      FMLOAD(I,1, 2) =    0.156
      FMLOAD(I,1, 3) =    0.039
      FMLOAD(I,2, 1) =    0.200
      FMDEP(I)       =    3.0
      MOISEX(I)      =    0.40

C----------
C  SH9 (149) VERY HIGH LOAD HUMID CLIMATE SHRUB
C----------
      I = 149
      SURFVL(I,1,1) = 750
      SURFVL(I,2,1) = 1500
      SURFVL(I,2,2) = 1800
      FMLOAD(I,1, 1) =    0.207
      FMLOAD(I,1, 2) =    0.112
      FMLOAD(I,2, 1) =    0.321
      FMLOAD(I,2, 2) =    0.071
      FMDEP(I)       =    4.4
      MOISEX(I)      =    0.40
C----------
C  TU1 (161) LOW LOAD DRY CLIMATE TIMBER-GRASS-SHRUB
C----------
      I = 161
      SURFVL(I,1,1) = 2000
      SURFVL(I,2,1) = 1600
      SURFVL(I,2,2) = 1800
      FMLOAD(I,1, 1) =    0.009
      FMLOAD(I,1, 2) =    0.041
      FMLOAD(I,1, 3) =    0.069
      FMLOAD(I,2, 1) =    0.041
      FMLOAD(I,2, 2) =    0.009
      FMDEP(I)       =    0.6
      MOISEX(I)      =    0.20


C----------
C  TU2 (162) MODERATE LOAD HUMID CLIMATE TIMBER-SHRUB
C----------
      I = 162
      SURFVL(I,1,1) = 2000
      SURFVL(I,2,1) = 1600
      FMLOAD(I,1, 1) =    0.044
      FMLOAD(I,1, 2) =    0.083
      FMLOAD(I,1, 3) =    0.057
      FMLOAD(I,2, 1) =    0.009
      FMDEP(I)       =    1.0
      MOISEX(I)      =    0.30

C----------
C  TU3 (163) MODERATE LOAD HUMID CLIMATE TIMBER-GRASS-SHRUB
C----------
      I = 163
      SURFVL(I,1,1) = 1800
      SURFVL(I,2,1) = 1400
      SURFVL(I,2,2) = 1600
      FMLOAD(I,1, 1) =    0.051
      FMLOAD(I,1, 2) =    0.007
      FMLOAD(I,1, 3) =    0.011
      FMLOAD(I,2, 1) =    0.051
      FMLOAD(I,2, 2) =    0.030
      FMDEP(I)       =    1.3
      MOISEX(I)      =    0.30
C----------
C  TU4 (164) DWARF CONIFER WITH UNDERSTORY
C----------
      I = 164
      SURFVL(I,1,1) = 2300
      SURFVL(I,2,1) = 2000
      FMLOAD(I,1, 1) =    0.207
      FMLOAD(I,2, 1) =    0.092
      FMDEP(I)       =    0.5
      MOISEX(I)      =    0.12
C----------
C  TU5 (165) VERY HIGH LOAD DRY CLIMATE TIMBER-SHRUB
C----------
      I = 165
      SURFVL(I,1,1) = 1500
      SURFVL(I,2,1) = 750
      FMLOAD(I,1, 1) =    0.184
      FMLOAD(I,1, 2) =    0.184
      FMLOAD(I,1, 3) =    0.138
      FMLOAD(I,2, 1) =    0.138
      FMDEP(I)       =    1.0
      MOISEX(I)      =    0.25
C----------
C  TL1 (181) LOW LOAD COMPACT CONIFER LITTER
C----------
      I = 181
      SURFVL(I,1,1) = 2000
      FMLOAD(I,1, 1) =    0.046
      FMLOAD(I,1, 2) =    0.101
      FMLOAD(I,1, 3) =    0.165
      FMDEP(I)       =    0.2
      MOISEX(I)      =    0.30

C----------
C  TL2 (182) LOW LOAD BROADLEAF LITTER
C----------
      I = 182
      SURFVL(I,1,1) = 2000
      FMLOAD(I,1, 1) =    0.064
      FMLOAD(I,1, 2) =    0.106
      FMLOAD(I,1, 3) =    0.101
      FMDEP(I)       =    0.2
      MOISEX(I)      =    0.25

C----------
C  TL3 (183) MODERATE LOAD CONIFER LITTER
C----------
      I = 183
      SURFVL(I,1,1) = 2000
      FMLOAD(I,1, 1) =    0.023
      FMLOAD(I,1, 2) =    0.101
      FMLOAD(I,1, 3) =    0.129
      FMDEP(I)       =    0.3
      MOISEX(I)      =    0.20
C----------
C  TL4 (184) SMALL DOWNED LOGS
C----------
      I = 184
      SURFVL(I,1,1) = 2000
      FMLOAD(I,1, 1) =    0.023
      FMLOAD(I,1, 2) =    0.069
      FMLOAD(I,1, 3) =    0.193
      FMDEP(I)       =    0.4
      MOISEX(I)      =    0.25
C----------
C  TL5 (185) HIGH LOAD CONIFER LITTER
C----------
      I = 185
      SURFVL(I,1,1) = 2000
      SURFVL(I,2,1) = 1600
      FMLOAD(I,1, 1) =    0.053
      FMLOAD(I,1, 2) =    0.115
      FMLOAD(I,1, 3) =    0.202
      FMDEP(I)       =    0.6
      MOISEX(I)      =    0.25

C----------
C  TL6 (186) MODERATE LOAD BROADLEAF LITTER
C----------
      I = 186
      SURFVL(I,1,1) = 2000
      FMLOAD(I,1, 1) =    0.110
      FMLOAD(I,1, 2) =    0.055
      FMLOAD(I,1, 3) =    0.055
      FMDEP(I)       =    0.3
      MOISEX(I)      =    0.25

C----------
C  TL7 (187) LARGE DOWNED LOGS
C----------
      I = 187
      SURFVL(I,1,1) = 2000
      FMLOAD(I,1, 1) =    0.014
      FMLOAD(I,1, 2) =    0.064
      FMLOAD(I,1, 3) =    0.372
      FMDEP(I)       =    0.4
      MOISEX(I)      =    0.25

C----------
C  TL8 (188) LONG-NEEDLE LITTER
C----------
      I = 188
      SURFVL(I,1,1) = 1800
      FMLOAD(I,1, 1) =    0.266
      FMLOAD(I,1, 2) =    0.064
      FMLOAD(I,1, 3) =    0.051
      FMDEP(I)       =    0.3
      MOISEX(I)      =    0.35

C----------
C  TL9 (189) VERY HIGH LOAD BROADLEAF LITTER
C----------
      I = 189
      SURFVL(I,1,1) = 1800
      SURFVL(I,2,1) = 1600
      FMLOAD(I,1, 1) =    0.305
      FMLOAD(I,1, 2) =    0.152
      FMLOAD(I,1, 3) =    0.191
      FMDEP(I)       =    0.6
      MOISEX(I)      =    0.35
C----------
C  SB1 (201) LOW LOAD ACTIVITY FUEL
C----------
      I = 201
      SURFVL(I,1,1) = 2000
      FMLOAD(I,1, 1) =    0.069
      FMLOAD(I,1, 2) =    0.138
      FMLOAD(I,1, 3) =    0.505
      FMDEP(I)       =    1.0
      MOISEX(I)      =    0.25

C----------
C  SB2 (202) MODERATE LOAD ACTIVITY FUEL OR LOW LOAD BLOWDOWN
C----------
      I = 202
      SURFVL(I,1,1) = 2000
      FMLOAD(I,1, 1) =    0.207
      FMLOAD(I,1, 2) =    0.195
      FMLOAD(I,1, 3) =    0.184
      FMDEP(I)       =    1.0
      MOISEX(I)      =    0.25

C----------
C  SB3 (203) HIGH LOAD ACTIVITY FUEL OR MODERATE LOAD BLOWDOWN
C----------
      I = 203
      SURFVL(I,1,1) = 2000
      FMLOAD(I,1, 1) =    0.253
      FMLOAD(I,1, 2) =    0.126
      FMLOAD(I,1, 3) =    0.138
      FMDEP(I)       =    1.2
      MOISEX(I)      =    0.25
C----------
C  SB4 (204) HIGH LOAD BLOWDOWN
C----------
      I = 204
      SURFVL(I,1,1) = 2000
      FMLOAD(I,1, 1) =    0.241
      FMLOAD(I,1, 2) =    0.161
      FMLOAD(I,1, 3) =    0.241
      FMDEP(I)       =    2.7
      MOISEX(I)      =    0.25
C----------
C  INITIALIZE VARIABLES FOR NEW FIRE CALCULATION OPTIONS 
C  (NEW FUEL MODEL LOGIC AND USING MODELLED LOADS)
C----------
      IFLOGIC   = 0
      IFMSET    = 2
      USAV(1)   = 2000
      USAV(2)   = 1800
      USAV(3)   = 1500
      UBD(1)    = 0.10
      UBD(2)    = 0.75
      ULHV      = 8000
      DO I = 1,MXDFMD
        IFUELMON(I) = -1       
      ENDDO 
C----------
C  SNAG POOL INITIALIZATION
C----------
      DO I = 1, MAXSP
        DO J = 1, 19
          MAXHT(I,J)  =    0.0
          MINHT(I,J)  = 1000.0
          DSPDBH(I,J) =    0.0
        ENDDO
      ENDDO
C----------
C  DEFAULT CYCLE LENGTH FOR DECAY CALCULATIONS
C----------
      NYRS = 1
C----------      
C  SET VARIABLES FOR BURN, FUEL EFFECTS, ALL FUELS, DOWN WOOD, AND
C  MORTALITY REPORTS.
C----------
      IFMBRB = 9999
      IFMBRE = 9999
      IFMFLB = 9999
      IFMFLE = 9999
      IFMMRB = 9999
      IFMMRE = 9999
      IFLALB = 9999
      IFLALE = 9999
      IPFLMB = 9999
      IPFLME = 9999
      ISNAGB = 9999
      ISNAGE = 9999
      ISHEATB= 9999
      ISHEATE= 9999
      ICFPB  = 9999
      ICFPE  = 9999
      IDWRPB = 9999
      IDWRPE = 9999
      IDWCVB = 9999
      IDWCVE = 9999
C
      IDBRN  = 0
      IDSHEAT= 0
      IDFUL  = 0
      IDMRT  = 0
      IDFLAL = 0
      IDPFLM = 0
      IDDWRP = 0
      IDDWCV = 0
C
      IBRPAS = 0
      IFLPAS = 0
      IMRPAS = 0
      IFAPAS = 0
      IPFPAS = 0
      IDWPAS = 0
      IDCPAS = 0
C
C  INITIAL VALUES FOR CONTROL OF 2 CARBON REPORTS
C
      IDCRPT  = 0       ! MAIN CARBON REPORT
      ICRPTB  = 9999    ! BEGINNING YR
      ICRPTE  = 9999    ! ENDING YR
      ICRPAS  = 0       ! REPORT HEADER FLAG

      IDCHRV  = 0       ! HARVESTED WOOD REPORT
      ICHRVB  = 9999    ! BEGINNING YR
      ICHRVE  = 9999    ! ENDING YR
      ICHPAS  = 0       ! REPORT HEADER FLAG
C
C  INITIAL VALUES FOR CONTROL OF CARBON REPORT BEHAVIOR
C
      DO I = 1,17
        CARBVAL(I) = 0.0
      ENDDO
            
      ICMETH   = 0       ! CARBON METHOD 0 = FFE, 1 = JENKINS
      ICMETRC  = 0       ! UNITS TYPE 0 = IMPERIAL, 1 = METRIC
      ICHABT   = 1       ! DEFAULT C-REPORTING HABITAT GROUP
                         ! (FMCBA MAY ALTER: NI/IE/SO/SN)
      CRDCAY   = 0.0425  ! ROOT DECAY RATE (<0 = "NOT USED")
      CDBRK(1) =  9.0    ! DEFAULT SOFTWOOD DIAMETER BREAKPOINT (IN)
      CDBRK(2) = 11.0    ! DEFAULT HARDWOOD DIAMETER BREAKPOINT (IN)
      BIOLIVE  =  0.0    ! BIOxxx C-REPORTING GROUPS
      BIOREM(1)=  0.0
      BIOREM(2)=  0.0
      BIOSNAG  =  0.0
      BIODDW   =  0.0
      BIOFLR   =  0.0
      BIOSHRB  =  0.0
      BIOROOT  =  0.0
      BIOCON(1)=  0.0
      BIOCON(2)=  0.0        
      DO I = 1,2
        DO J = 1,2
          DO K = 1,MAXCYC
            FATE(I,J,K) = 0.0
          ENDDO
        ENDDO
      ENDDO

      DO J = 1,MXFLCL
        DO K = 1,2
          CWDNEW(K,J) = 0.0
        ENDDO
      ENDDO
C
      DO I = 1,3
        DO J = 1,MXFLCL
          DO K = 1,2
             DO L = 1,5
               CWD(I,J,K,L) = 0.0
               IF (J .LE. 10) THEN
                 CWDVOL(I,J,K,L) = 0.0
                 CWDCOV(I,J,K,L) = 0.0
               ENDIF               
             ENDDO
          ENDDO
        ENDDO
      ENDDO
C
      DO I = 1,4
        DO J = 0,5
          DO K = 1,TFMAX
            CWD2B(I,J,K)  = 0.0
            CWD2B2(I,J,K) = 0.0
          ENDDO
        ENDDO
      ENDDO
C
      DO I = 1,MAXTRE
        FMPROB(I) = 0.0
        OLDHT(I)  = 0.0
        OLDCRL(I) = 0.0
        GROW(I)   = 1
        SNGNEW(I) = 0.0
        DO K = 0,5
          CROWNW(I,K) = 0.0
          OLDCRW(I,K) = 0.0
        ENDDO
      ENDDO
C
      FLIVE(1) = 0.0
      FLIVE(2) = 0.0

C----------
C     *** CL-FFE *** stub
C     Initialize CWD decay rate sensitivy. No keyword control yet.
C     Array dimensions are MXFLCL+1 from FMPARM.F77
C     Array storage in FMCOM.F77
C     Decay rates are modified by calls to CLCWD in FMCWD and FMCRBOUT
C     Set Q10 and reference decay temperature for each of size category.
C     Ref: Kurz et al. 2009. Ecol. Mod. 220:480-504
C----------
      !DO I = 1, 9
      !  Q10CWD(I) = 2.0  ! snag stems, branches
      !ENDDO
      !Q10CWD(10)  = 2.65 ! litter - Kurz Table 4: AG very fast 
      !Q10CWD(11)  = 1.0  ! duff
      !Q10CWD(12)  = 2.0  ! roots
      !
      !DO I = 1,(MXFLCL+1)
      !  REFMATCWD(I) = 10.0
      !ENDDO
      
C----------
C  INITIALIZE FLAG IDICATING REMOVAL OF STAND BIOMASS 
C  EVENT MONITOR FUNCTION TREEBIO IN **FMEVMON**
C----------
      LREMT=.FALSE.
C----------
C  INITIALIZE THE NUMBER OF PICTURES TO DRAW IF USING SVS.
C----------
      NFMSVPX = 3
C----------
C  CALL THE VARIANT-SPECIFIC INITIALIZER.
C----------
      CALL FMVINIT
C
      RETURN
C
      ENTRY FMATV(LRET)
C----------
C  RETURN THE STATE OF LFMON, WHICH IS FALSE UNLESS AT LEAST
C  ONE FM KEYWORD FOR THE STAND IS PRESENT, THEN IT IS TRUE.
C----------
      LRET = LFMON
      RETURN
C
      ENTRY FMSATV(LRET)
C----------
C  SET THE STATE OF LFMON, THIS IS CALLED BY GETSTD. IF THE
C  VALUE IS FALSE, THEN NONE OF THE FIRE MODEL VARIABLES HAVE
C  BEEN DEFINED (THEY ARE NOT STORED BY THE PPE).
C----------
      LFMON = LRET
      RETURN
C
      ENTRY FMLNKD(LRET)
C----------
C  RETURNS TRUE IF THE FIRE MODEL IS LINKED, THE VERSION IN
C  EXFIRE RETURNS FALSE.
C----------
      LRET = .TRUE.
      RETURN
C
      END
