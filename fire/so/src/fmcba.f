      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C  **FMCBA   FIRE-SO-DATE OF LAST REVISION: 08/24/15
C----------
C     SINGLE-STAND VERSION
C     CALLED FROM: FMMAIN

C  PURPOSE:
C     FIND THE DOMINANT SPECIES (BY BASAL AREA). SET THE INITIAL LIVE
C     AND DEAD FUEL VALUES AS WELL. THE DEAD FUELS ARE ONLY INITIALIZED
C     IN THE FIRST YEAR, BUT COVTYP AND THE LIVE FUELS MUST BE DONE
C     EACH YEAR.
C----------------------------------------------------------------------

C  LOCAL VARIABLE DEFINITIONS:
C     BAMOST:  THE HIGHEST BASAL AREA IN A SINGLE SPECIES
C     COVINI:  THE SERAL COVER TYPE TO BE USED FOR INITIATING FUELS IN
C              BARE STANDS. THE DIMENSION (92) MATCHES THE DIMENSION OF THE
C              PLANT ASSOCIATION TABLE IN **HABTYP**
C     TOTBA:   THE TOTAL BASAL AREA IN THE STAND (USED IN THE FUELS CALCS)
C     OLDCOV:  THE COVER TYPE FROM THE PREVIOUS YEAR (USED IN YEARS WITH NO BA)
C     ILOGMOD: THE 'PREVIOUS LOGGING' MODEL
C     ISSX:    THE OTTMAN STRUCTURAL STAGE INDEX
C     ISWTCH:  =1 if called by SVSTART
C              =0 if called by any other subroutine (FMMAIN, FMPPHV)
C     ISPX:    THE INDEX OF THE DOMINANT BASAL AREA SPECIES; AND THE INDEX
C              TO THE COVER TYPE * STRUCTURAL STAGE DATA STRUCTURE COVRINI
C  FUELINI CONTAINS INITIAL FUELS DATA FOR SHRUBS, HERBS, AND THE
C  EIGHT FUEL TYPES (SEE *TYPE FUEL_STR* ABOVE FOR DETAILS), INDEXED
C  BY THE FCC VEGETATION INDEX. THE 43 OBJECTS OF FUELINI REPRESENT ALL
C  THE FCC INDICES PRESENT IN OTTMAR'S CROSSWALK DOCUMENT FOR FORESTED
C  COVER TYPES IN THIS VARIANT.
C  FUELINI(I,43)
C     I=1   FCC VEGETATION NUMBER
C     I=2   HERB
C     I=3   SHRUB
C     I=4   <0.25 "        1 HOUR FUELS, TONS/AC
C     I=5   0.25 - 1"     10 HOUR FUELS, TONS/AC
C     I=6   1 - 3"       100 HOUR FUELS, TONS/AC
C     I=7   3 - 9"     1,000 HOUR FUELS, TONS/AC
C     I=8   9 - 20"    10,000 HOUR FUELS, TONS/AC
C     I=9   20 - 35"    
C     I=10  35 - 50"  
C     I=11  > 50"  
C     I=12  LITTER   ALWAYS ZERO: ABSENT FROM OTTMAR
C     I=13  DUFF                         TONS/AC
C     THE COVER TYPE (1-11) * 10 + STRUCTURAL STAGE (1-7) DEFINE, IN COMBINATION,
C     THE SUITE OF APPLICABLE FCC'S. NOT ALL COMBINATIONS ARE PRESENT, SO THERE
C     MAY BE INPUTS THAT WILL GIVE NO MATCH (ESPECIALLY IF SPECIES 11 IS DOMINANT).
C  COVRINI(I,64)
C     I=1  DOMINANT (SENSU BASAL AREA) SPECIES * 10 + SS C
C     I=2-6 VECTOR OF PAST LOGGING MODELS
C     I=2  NO APPARENT LOGGING
C     I=3  REGENERATED
C     I=4  SELECTIVELY HARVESTED
C     I=5  THINNED
C     I=6  PATCH CLEARCUT
COMMONS
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'SSTGMC.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
COMMONS

C VARIABLE DECLARATIONS.

      LOGICAL DEBUG
      INTEGER COVRINI(6,218)
      INTEGER MYACT(3), IDUM(1)
      INTEGER ILOGMOD, ISSX, ISPX, IFMST
      INTEGER COVINI(92)
      INTEGER OLDCOV
      REAL    FUELINI(13,43)
      REAL    TBA(MAXSP)
      REAL    BAMOST
      REAL    STFUEL(MXFLCL,2), CW(MAXTRE)
      REAL    PRMS(12)

      INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC,IMODX,IPOSN,K
      REAL    TOTBA,PRCL,ADD,BA1,X3, FOTOVALS(9)
      REAL    DKRT(MXFLCL,4), PRDUFFT(MXFLCL,4), FOTOVAL(MXFLCL)
      INTEGER SOHMC(92),SOWMD(92), TEMP, MOIST
      REAL DKRADJ(3,3,3)

C     EACH SO HABITAT CODE (in R6) MAPS TO EITHER HOT (1), MODERATE (2)
C     OR COLD (3).  (FROM FMR6SDCY)

      DATA (SOHMC(I), I=   1,  50) /
     & 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 3,
     & 3, 1, 2, 2, 2, 2, 2, 2, 2, 3,
     & 3, 3, 1, 3, 1, 2, 2, 2, 1, 1/
      DATA (SOHMC(I), I=  51,  92) /
     & 1, 1, 1, 1, 2, 2, 1, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 1, 3, 2, 2, 2, 1,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2/

C     EACH SO HABITAT CODE MAPS TO EITHER WET (1), MESIC (2) OR DRY (3).  (FROM FMR6SDCY)

      DATA (SOWMD(I), I=   1,  50) /
     & 2, 2, 2, 1, 1, 2, 1, 1, 3, 3,
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 2,
     & 1, 2, 1, 1, 2, 1, 1, 2, 1, 3,
     & 1, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 3, 3, 2, 3, 3/
      DATA (SOWMD(I), I=  51,  92) /
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 2, 3, 3, 3, 2, 3, 2, 2,
     & 2, 2, 3, 2, 3, 1, 2, 2, 2, 3,
     & 1, 3, 3, 3, 3, 2, 3, 2, 2, 2,
     & 1, 2/

      DATA (((DKRADJ(I,J,K), K=1,3), J=1,3), I=1,3) /           
     &  1.7,    2,  1.7, 1.49, 1.91, 1.49,  0.75, 0.85,  0.75,
     & 1.35, 1.85, 1.35,    1,  1.7,    1, 0.875,  1.2, 0.875,
     & 1.21, 1.79, 1.21, 1.14, 1.76, 1.14,  0.75, 0.85,  0.75/

      DATA IDUM / 0 /           ! FOR CALL TO COVOLP

      DATA ((FUELINI(I,J),I=1,13),J=1,10) /
     &  1,0.3,0.4,0.5,0.8,1.7, 1.9,3.0,0.0,0.0,0.0,0.0, 2.3,
     &  4,0.5,0.5,0.1,1.5,2.2, 1.1,1.8,3.3,0.0,0.0,0.0, 6.0,
     &  7,0.8,0.4,0.1,0.6,1.6, 0.4,1.0,5.6,0.0,0.0,0.0, 9.8,
     &  8,0.0,0.0,0.1,1.6,4.2, 2.1,2.9,4.7,0.0,0.0,0.0, 9.8,
     & 10,0.5,2.5,0.2,1.2,2.3, 2.3,2.4,2.0,0.0,0.0,0.0,12.8,
     & 11,0.5,0.5,0.0,1.5,4.9,10.1,6.2,4.0,0.0,0.0,0.0,12.8,
     & 15,0.0,0.0,0.5,3.4,2.3, 1.9,0.0,0.0,0.0,0.0,0.0, 8.3,
     & 20,0.0,0.0,0.5,5.2,7.0, 3.9,0.0,0.0,0.0,0.0,0.0, 8.3,
     & 25,0.0,0.0,0.5,5.5,6.7,12.8,0.0,3.5,0.0,0.0,0.0, 8.3,
     & 32,0.0,0.0,0.5,2.3,4.8, 4.3,6.2,0.0,0.0,0.0,0.0, 6.0/
      DATA ((FUELINI(I,J),I=1,13),J=11,20) /            
     & 36,0.0,0.0,0.5,3.8,7.4, 7.1, 4.6,6.6,0.0,0.0,0.0, 6.0,
     & 40,0.0,0.0,0.5,2.7,5.0, 5.7, 2.0,2.0,0.0,0.0,0.0, 6.0,
     & 44,0.0,0.0,0.5,2.4,5.1,10.2, 8.6,3.0,0.0,0.0,0.0, 6.0,
     & 48,0.0,0.0,0.5,2.4,5.1,14.0,11.0,5.0,0.0,0.0,0.0, 6.0,
     & 52,0.5,0.5,0.6,2.3,1.9, 2.0, 0.0,0.0,0.0,0.0,0.0, 2.3,
     & 53,0.5,0.5,0.5,1.3,3.0, 4.5, 1.5,0.0,0.0,0.0,0.0, 2.3,
     & 56,0.5,0.5,0.5,1.3,3.0, 4.5, 1.5,0.0,0.0,0.0,0.0, 9.1,
     & 57,0.3,0.4,0.4,0.6,1.1, 8.8, 7.2,0.0,0.0,0.0,0.0, 9.1,
     & 58,0.3,0.4,0.7,1.1,1.5, 3.1, 4.7,0.0,0.0,0.0,0.0,15.9,
     & 59,0.7,0.7,0.5,1.8,3.5,12.3, 2.3,0.0,0.0,0.0,0.0,15.9/
      DATA ((FUELINI(I,J),I=1,13),J=21,30) /            
     & 61,0.3,0.4,0.5,1.2, 1.2, 2.5, 5.2, 2.0,0.0,0.0,0.0,20.4,
     & 62,0.8,0.5,0.5,2.6, 4.3, 7.0,10.5, 3.0,0.0,0.0,0.0,20.4,
     & 66,0.0,0.0,0.5,2.7, 5.5, 2.3, 0.0, 0.0,0.0,0.0,0.0, 8.3,
     & 71,0.0,0.0,0.5,5.2,13.0, 3.9, 0.0, 0.0,0.0,0.0,0.0, 8.3,
     & 76,0.0,0.0,0.5,5.5,13.7, 8.8, 0.0, 3.5,0.0,0.0,0.0, 8.3,
     & 83,0.0,0.0,0.5,3.2, 4.8,12.3, 5.4, 1.0,0.0,0.0,0.0,15.1,
     & 87,0.0,0.0,0.5,2.1, 6.3,10.3,10.8,11.6,0.0,0.0,0.0,15.1,
     & 95,0.0,0.0,0.5,3.9, 4.8,10.0, 5.5, 6.9,0.0,0.0,0.0,15.1,
     & 99,0.0,0.0,0.5,2.1, 6.3,10.3,10.8,11.6,0.0,0.0,0.0,15.1,
     &103,0.3,0.4,0.5,0.8, 1.7, 0.9, 0.0, 0.0,0.0,0.0,0.0, 2.3/
      DATA ((FUELINI(I,J),I=1,13),J=31,40) /            
     &106,0.5,0.5,0.3,0.7,4.0, 0.8,0.0,0.0,0.0,0.0,0.0,3.8,
     &107,0.5,0.5,0.4,1.2,7.4, 2.1,0.0,0.0,0.0,0.0,0.0,3.8,
     &110,0.5,0.5,0.7,2.3,5.9, 5.1,2.0,0.0,0.0,0.0,0.0,4.5,
     &112,0.3,0.4,0.2,0.9,1.7, 1.3,3.0,0.0,0.0,0.0,0.0,6.0,
     &113,0.5,0.5,0.2,1.1,3.4,14.8,3.5,0.0,0.0,0.0,0.0,6.0,
     &117,0.0,0.0,0.5,2.4,2.3, 1.9,0.0,0.0,0.0,0.0,0.0,9.8,
     &122,0.0,0.0,0.5,2.1,7.0, 5.3,0.0,0.0,0.0,0.0,0.0,9.8,
     &127,0.0,0.0,0.5,5.5,6.7,12.8,0.0,3.5,0.0,0.0,0.0,9.8,
     &134,0.0,0.0,0.5,1.7,4.1, 6.5,5.2,0.0,0.0,0.0,0.0,5.3,
     &138,0.0,0.0,0.5,2.5,6.6,15.8,1.2,0.0,0.0,0.0,0.0,5.3/
      DATA ((FUELINI(I,J),I=1,13),J=41,43) /            
     &146,0.0,0.0,0.5,1.7,4.1, 6.5,5.2,0.0,0.0,0.0,0.0,5.3,
     &150,0.0,0.0,0.5,2.5,6.6,15.8,1.2,0.0,0.0,0.0,0.0,5.3,
     &160,0.7,3.3,0.2,0.4,0.8, 0.0,0.0,0.0,0.0,0.0,0.0,2.3/
C----------
C  COVRINI CONTAINS THE APPROPRIATE FCC MODEL (FROM OTTMAN), GIVEN THE
C  LEADING COVER TYPE (COVINDX) AND STRUCTURAL STAGE (STRINDX).
C  COVER TYPE AND STRUCTURAL STAGE ARE COMBINED IN THE FIRST ELEMENT OF
C  THE STRUCTURE BY MULTIPLYING COVER TYPE * 10, THEN ADDING THE STRUCTURAL
C  TYPE. SO 53 IS COVER TYPE 5 (MOUNTAIN HEMLOCK) WITH STRUCTURAL STAGE 3
C  (STEM EXCLUSION CLOSED CANOPY). THE STRUCTURAL STAGES DO NOT ALL
C  CORRESPOND TO THE FVS DEFINITIONS. IN PARTICULAR, THERE IS NO BAREGROUND
C  STAGE, AND THE STEM EXCLUSION STAGE IS DIVIDED INTO 2 FORMS, OPEN AND
C  CLOSED CANOPY. THESE DIFFERENCES ARE HANDLED IN THE CODE. LASTLY, THE
C  COVRINI VECTOR MAPS THE LOGGING HISTORY. THERE ARE 5 KINDS OF
C  LOGGING HISTORY. GENERALLY ONLY THE FIRST IS USED. THE REST ARE INCLUDED
C  BECAUSE THEY MAY BE UNDER USER CONTROL IN THE FUTURE. THEY ARE:
C    I=2    NO LOGGING APPARENT
C    I=3    REGENERATED
C    I=4    SELECTIVELY HARVESTED
C    I=5    THINNED
C    I=6    PATCH CLEARCUT
C----------
      DATA ((COVRINI(I,J),I=1,6),J=1,20) /
     &  11,  52,  52,  52,  66,  52,
     &  12,  53,  53,  71,  71,  76,
     &  13,  56,  56,  71,  71,  76,
     &  14,  58,  58,  83,  83,  87,
     &  15,  58,  58,  83,  83,  87,
     &  16,  57,  57,  95,  95,  99,
     &  17,  61,  61,  95,  95,  99,
     &  31,  52,  52,  52,  66,  52,
     &  32,  53,  53,  71,  71,  76,
     &  33,  56,  56,  71,  71,  76,
     &  34,  58,  58,  83,  83,  87,
     &  35,  58,  58,  83,  83,  87,
     &  36,  62,  62,  95,  95,  99,
     &  37,  62,  62,  95,  95,  99,
     &  41,  52,  52,  52,  66,  52,
     &  42,  53,  53,  71,  71,  76,
     &  43,  56,  56,  71,  71,  76,
     &  44,  58,  58,  83,  83,  87,
     &  45,  58,  58,  83,  83,  87,
     &  46,  62,  62,  95,  95,  99/
      DATA ((COVRINI(I,J),I=1,6),J=21,40) /
     &  47,  62,  62,  95,  95,  99,
     &  51,  52,  52,  52,  66,  52,
     &  52,  53,  53,  71,  71,  76,
     &  53,  56,  56,  71,  71,  76,
     &  54,  58,  58,  83,  83,  87,
     &  55,  58,  58,  83,  83,  87,
     &  56,  62,  62,  95,  95,  99,
     &  57,  62,  62,  95,  95,  99,
     &  61,  52,  52,  52,  66,  52,
     &  62,  53,  53,  71,  71,  76,
     &  63,  56,  56,  71,  71,  76,
     &  64,  58,  58,  83,  83,  87,
     &  65,  58,  58,  83,  83,  87,
     &  66,  62,  62,  95,  95,  99,
     &  67,  62,  62,  95,  95,  99,
     &  71, 103, 103, 103, 117, 127,
     &  72, 106, 106, 122, 122, 127,
     &  73, 107, 107, 122, 122, 127,
     &  74, 110, 110, 134, 134, 138,
     &  75, 110, 110, 134, 134, 138/
      DATA ((COVRINI(I,J),I=1,6),J=41,64) /
     &  76, 112, 112, 146, 146, 150,
     &  77, 113, 113, 146, 146, 150,
     &  81,  52,  52,  52,  66,  52,
     &  82,  53,  53,  71,  71,  76,
     &  83,  56,  56,  71,  71,  76,
     &  84,  59,  59,  83,  83,  87,
     &  85,  59,  59,  83,  83,  87,
     &  86,  61,  61,  87,  87,  87,
     &  87,  62,  62,  87,  87,  87,
     &  91,  52,  52,  52,  66,  52,
     &  92,  53,  53,  71,  71,  76,
     &  93,  56,  56,  71,  71,  76,
     &  94,  58,  58,  83,  83,  87,
     &  95,  58,  58,  83,  83,  87,
     &  96,  62,  62,  95,  95,  99,
     &  97,  62,  62,  95,  95,  99,
     & 101,   1,   4,   4,  15,   4,
     & 102,   4,   4,  15,  20,  25,
     & 103,   4,   4,  15,  20,  25,
     & 104,   8,   7,  32,  32,  36,
     & 105,   8,   7,  32,  32,  36,
     & 106,  10,  10,  40,  40,  48,
     & 107,  11,  11,  44,  44,  48,
     & 115, 160, 160, 106, 106, 107/
C      for new species, copied doug-fir
      DATA ((COVRINI(I,J),I=1,6),J=65,113) /     
     & 121,  52,  52,  52,  66,  52,
     & 122,  53,  53,  71,  71,  76,
     & 123,  56,  56,  71,  71,  76,
     & 124,  58,  58,  83,  83,  87,
     & 125,  58,  58,  83,  83,  87,
     & 126,  62,  62,  95,  95,  99,
     & 127,  62,  62,  95,  95,  99,
     & 131,  52,  52,  52,  66,  52,
     & 132,  53,  53,  71,  71,  76,
     & 133,  56,  56,  71,  71,  76,
     & 134,  58,  58,  83,  83,  87,
     & 135,  58,  58,  83,  83,  87,
     & 136,  62,  62,  95,  95,  99,
     & 137,  62,  62,  95,  95,  99, 
     & 141,  52,  52,  52,  66,  52,
     & 142,  53,  53,  71,  71,  76,
     & 143,  56,  56,  71,  71,  76,
     & 144,  58,  58,  83,  83,  87,
     & 145,  58,  58,  83,  83,  87,
     & 146,  62,  62,  95,  95,  99,
     & 147,  62,  62,  95,  95,  99,
     & 151,  52,  52,  52,  66,  52,
     & 152,  53,  53,  71,  71,  76,
     & 153,  56,  56,  71,  71,  76,
     & 154,  58,  58,  83,  83,  87,
     & 155,  58,  58,  83,  83,  87,
     & 156,  62,  62,  95,  95,  99,
     & 157,  62,  62,  95,  95,  99,
     & 161,  52,  52,  52,  66,  52,
     & 162,  53,  53,  71,  71,  76,
     & 163,  56,  56,  71,  71,  76,
     & 164,  58,  58,  83,  83,  87,
     & 165,  58,  58,  83,  83,  87,
     & 166,  62,  62,  95,  95,  99,
     & 167,  62,  62,  95,  95,  99,                  
     & 171,  52,  52,  52,  66,  52,      
     & 172,  53,  53,  71,  71,  76,      
     & 173,  56,  56,  71,  71,  76,      
     & 174,  58,  58,  83,  83,  87,      
     & 175,  58,  58,  83,  83,  87,      
     & 176,  62,  62,  95,  95,  99,      
     & 177,  62,  62,  95,  95,  99, 
     & 181,  52,  52,  52,  66,  52, 
     & 182,  53,  53,  71,  71,  76, 
     & 183,  56,  56,  71,  71,  76, 
     & 184,  58,  58,  83,  83,  87, 
     & 185,  58,  58,  83,  83,  87, 
     & 186,  62,  62,  95,  95,  99, 
     & 187,  62,  62,  95,  95,  99/ 
      DATA ((COVRINI(I,J),I=1,6),J=114,162) /  
     & 191,  52,  52,  52,  66,  52, 
     & 192,  53,  53,  71,  71,  76, 
     & 193,  56,  56,  71,  71,  76, 
     & 194,  58,  58,  83,  83,  87, 
     & 195,  58,  58,  83,  83,  87, 
     & 196,  62,  62,  95,  95,  99, 
     & 197,  62,  62,  95,  95,  99, 
     & 201,  52,  52,  52,  66,  52, 
     & 202,  53,  53,  71,  71,  76, 
     & 203,  56,  56,  71,  71,  76, 
     & 204,  58,  58,  83,  83,  87, 
     & 205,  58,  58,  83,  83,  87, 
     & 206,  62,  62,  95,  95,  99, 
     & 207,  62,  62,  95,  95,  99, 
     & 211,  52,  52,  52,  66,  52, 
     & 212,  53,  53,  71,  71,  76, 
     & 213,  56,  56,  71,  71,  76, 
     & 214,  58,  58,  83,  83,  87, 
     & 215,  58,  58,  83,  83,  87, 
     & 216,  62,  62,  95,  95,  99, 
     & 217,  62,  62,  95,  95,  99, 
     & 221,  52,  52,  52,  66,  52, 
     & 222,  53,  53,  71,  71,  76, 
     & 223,  56,  56,  71,  71,  76, 
     & 224,  58,  58,  83,  83,  87, 
     & 225,  58,  58,  83,  83,  87, 
     & 226,  62,  62,  95,  95,  99, 
     & 227,  62,  62,  95,  95,  99, 
     & 231,  52,  52,  52,  66,  52, 
     & 232,  53,  53,  71,  71,  76, 
     & 233,  56,  56,  71,  71,  76, 
     & 234,  58,  58,  83,  83,  87, 
     & 235,  58,  58,  83,  83,  87, 
     & 236,  62,  62,  95,  95,  99, 
     & 237,  62,  62,  95,  95,  99, 
     & 241,  52,  52,  52,  66,  52, 
     & 242,  53,  53,  71,  71,  76, 
     & 243,  56,  56,  71,  71,  76, 
     & 244,  58,  58,  83,  83,  87, 
     & 245,  58,  58,  83,  83,  87, 
     & 246,  62,  62,  95,  95,  99, 
     & 247,  62,  62,  95,  95,  99, 
     & 251,  52,  52,  52,  66,  52, 
     & 252,  53,  53,  71,  71,  76, 
     & 253,  56,  56,  71,  71,  76, 
     & 254,  58,  58,  83,  83,  87, 
     & 255,  58,  58,  83,  83,  87, 
     & 256,  62,  62,  95,  95,  99, 
     & 257,  62,  62,  95,  95,  99/                                   
      DATA ((COVRINI(I,J),I=1,6),J=163,211) /   
     & 261,  52,  52,  52,  66,  52, 
     & 262,  53,  53,  71,  71,  76, 
     & 263,  56,  56,  71,  71,  76, 
     & 264,  58,  58,  83,  83,  87, 
     & 265,  58,  58,  83,  83,  87, 
     & 266,  62,  62,  95,  95,  99, 
     & 267,  62,  62,  95,  95,  99,  
     & 271,  52,  52,  52,  66,  52, 
     & 272,  53,  53,  71,  71,  76, 
     & 273,  56,  56,  71,  71,  76, 
     & 274,  58,  58,  83,  83,  87, 
     & 275,  58,  58,  83,  83,  87, 
     & 276,  62,  62,  95,  95,  99, 
     & 277,  62,  62,  95,  95,  99,  
     & 281,  52,  52,  52,  66,  52, 
     & 282,  53,  53,  71,  71,  76, 
     & 283,  56,  56,  71,  71,  76, 
     & 284,  58,  58,  83,  83,  87, 
     & 285,  58,  58,  83,  83,  87, 
     & 286,  62,  62,  95,  95,  99, 
     & 287,  62,  62,  95,  95,  99,  
     & 291,  52,  52,  52,  66,  52, 
     & 292,  53,  53,  71,  71,  76, 
     & 293,  56,  56,  71,  71,  76, 
     & 294,  58,  58,  83,  83,  87, 
     & 295,  58,  58,  83,  83,  87, 
     & 296,  62,  62,  95,  95,  99, 
     & 297,  62,  62,  95,  95,  99,  
     & 301,  52,  52,  52,  66,  52, 
     & 302,  53,  53,  71,  71,  76, 
     & 303,  56,  56,  71,  71,  76, 
     & 304,  58,  58,  83,  83,  87, 
     & 305,  58,  58,  83,  83,  87, 
     & 306,  62,  62,  95,  95,  99, 
     & 307,  62,  62,  95,  95,  99,  
     & 311,  52,  52,  52,  66,  52, 
     & 312,  53,  53,  71,  71,  76, 
     & 313,  56,  56,  71,  71,  76, 
     & 314,  58,  58,  83,  83,  87, 
     & 315,  58,  58,  83,  83,  87, 
     & 316,  62,  62,  95,  95,  99, 
     & 317,  62,  62,  95,  95,  99,  
     & 321,  52,  52,  52,  66,  52, 
     & 322,  53,  53,  71,  71,  76, 
     & 323,  56,  56,  71,  71,  76, 
     & 324,  58,  58,  83,  83,  87, 
     & 325,  58,  58,  83,  83,  87, 
     & 326,  62,  62,  95,  95,  99, 
     & 327,  62,  62,  95,  95,  99/                                         
      DATA ((COVRINI(I,J),I=1,6),J=212,218) /    
     & 331,  52,  52,  52,  66,  52, 
     & 332,  53,  53,  71,  71,  76, 
     & 333,  56,  56,  71,  71,  76, 
     & 334,  58,  58,  83,  83,  87, 
     & 335,  58,  58,  83,  83,  87, 
     & 336,  62,  62,  95,  95,  99, 
     & 337,  62,  62,  95,  95,  99/         
           
C----------
C  THIS TABLE (FROM NICK CROOKSTON) SHOWS THE PLANT ASSOCIATIONS FOR
C  EACH OF THE 92 PLANT ASSOCIATIONS OF THE SORNEC VARIANT. THE
C  TREE SPECIES IS USED TO ASSIGN A DEFAULT COVER TYPE WHEN NO TREES
C  ARE PRESENT. THE NUMBER AT THE END SHOWS MY INFERENCE ABOUT THE
C  DEFAULT SPECIES

C  1:{CDS612 PSME-ABCO/SYAL/LIBO}  3
C  2:{CDS613 PSME-ABCO/SYAL/FORB}  3
C  3:{CDS614 PSME-ABCO/SYAL/CARU}  3
C  4:{CEM111 PIEN/CAEU}  8
C  5:{CEM221 PIEN/EQAR-STRO}  8
C  6:{CEM222 PIEN/CLUN}  8
C  7:{CEM311 PIEN/VAOC2-FORB}  8
C  8:{CEM312 PIEN/VAOC2/CAEU}  8
C  9:{CLC111 PICO-PIAL/PELA}  7
C 10:{CLC112 PICO-PIAL/ARCO2}  7
C 11:{CLF111 PICO/FORB}  7
C 12:{CLG311 PICO/STOC-BASIN}  7
C 13:{CLG313 PICO/STOC-LUCA-LINU}  7
C 14:{CLG314 PICO/STOC-LUCA-PUM}  7
C 15:{CLG315 PICO/FRVI/FEID}  7
C 16:{CLG411 PICO/CAPE-LUCA-PUM}  7
C 17:{CLG412 PICO/CAPE-LUCA-PEEU}  7
C 18:{CLG413 PICO/CAPE-STOC-BASIN}  7
C 19:{CLG415 PICO/SIHY-CAPE}  7
C 20:{CLH111 PICO/POTR/FRVI}  7
C 21:{CLM111 PICO/CANE-ELGL-WET}  7
C 22:{CLM112 PICO/POPR}  7
C 23:{CLM113 PICO/CAEU}  7
C 24:{CLM114 PICO/CAAQ}  7
C 25:{CLM211 PICO/ARUV-PUM}  7
C 26:{CLM311 PICO/VAOC2-PUM}  7
C 27:{CLM312 PICO/VAOC2/CAEU}  7
C 28:{CLM313 PICO/SPDO-FORB}  7
C 29:{CLM314 PICO/SPDO/CAEU}  7
C 30:{CLM411 PICO/XETE-PUM}  7
C 31:{CLM911 PICO/PIEN/ELPA2}  7
C 32:{CLS112 PICO/ARTR-RHYO}  7
C 33:{CLS211 PICO/PUTR/STOC-PUM}  7
C 34:{CLS212 PICO/PUTR/CAPE-PUM}  7
C 35:{CLS213 PICO/PUTR/FORB-PUM}  7
C 36:{CLS214 PICO/PUTR/FEID-PUM}  7
C 37:{CLS215 PICO/RICE-PUTR/STOC-PUM}  7
C 38:{CLS216 PICO/PUTR-RHYO}  7
C 39:{CLS311 PICO/ARNE}  7
C 40:{CLS412 PICO/VASC-PUM}  7
C 41:{CLS413 PICO/VASC-FORB}  7
C 42:{CLS414 PICO/VASC/CAPE}  7
C 43:{CLS911 PICO/CEVE-ARPA-PUM}  7
C 44:{CMS111 TSME/VASC-DES}  5
C 45:{CPC211 PIPO-JUOC/CELE/FEID}  10
C 46:{CPF111 PIPO/WYMO}  10
C 47:{CPG212 PIPO/CAPE-FEID-LALA2}  10
C 48:{CPH311 PIPO-POTR/PONE}  10
C 49:{CPS111 PIPO/PUTR-ARTR/FEID}  10
C 50:{CPS112 PIPO/PUTR-ARTR/SIHY}  10
C 51:{CPS121 PIPO/ARTR/PONE}  10
C 52:{CPS211 PIPO/PUTR/FEID-PUM}  10
C 53:{CPS212 PIPO/PUTR/STOC-PUM}  10
C 54:{CPS213 PIPO/PUTR-ARPA/STOC-PUM}  10
C 55:{CPS214 PIPO/PUTR-ARPA/CAPE-PUM}  10
C 56:{CPS215 PIPO/PUTR/CAPE-PUM}  10
C 57:{CPS216 PIPO/PUTR/FEID-AGSP-PUM}  10
C 58:{CPS217 PIPO/PUTR-ARPA/FEID-PUM}  10
C 59:{CPS218 PIPO/PUTR/SIHY-RHYO}  10
C 60:{CPS311 PIPO/PUTR-CEVE/STOC-PUM}  10
C 61:{CPS312 PIPO/PUTR-CEVE/CAPE-PUM}  10
C 62:{CPS314 PIPO/PUTR-CEVE/FEID}  10
C 63:{CPS511 PIPO/SYAL-FLOOD}  10
C 64:{CRG111 ABMAS/CAPE} 9
C 65:{CRS111 ABMAS/ARNE} 9
C 66:{CRS112 ABMAS-TSME/ARNE/CAPE} 9
C 67:{CRS311 ABMAS/CACH/CHUM-CAPE} 9
C 68:{CWC111 ABCO-PIPO-CADE/AMAL} 4
C 69:{CWC211 ABCO/CEVE-CACH/PTAQ} 4
C 70:{CWC212 ABCO/CEVE-CACH/CARU} 4
C 71:{CWC213 ABCO/CEVE/CAPE-PTAQ} 4
C 72:{CWC215 ABCO-PSME-CEVE/ARUV} 4
C 73:{CWC311 ABCO-PICO/STOC-CAPE} 4
C 74:{CWC411 ABCO-PIPO-PILA/RIVI} 4
C 75:{CWC412 ABCO-PIPO-PILA/ARPA} 4
C 76:{CWC911 PIEN-BOTTOMS} 8
C 77:{CWF431 ABCO/CLUN} 4
C 78:{CWH111 ABCO/CEVE-CACH} 4
C 79:{CWH112 ABCO/CACH-PAMY/CHUM} 4
C 80:{CWH211 ABCO-PIPO-POTR/CAPE} 4
C 81:{CWM111 ABCO/ALTE} 4
C 82:{CWS112 ABCO/CEVE-ARPA-PUM} 4
C 83:{CWS113 ABCO/CEVE-ARPA/CAPE-PEEU} 4
C 84:{CWS114 ABCO/CEVE-PUM} 4
C 85:{CWS115 ABCO/CEVE/CAPE} 4
C 86:{CWS116 ABCO/CEVE/CEPR-FRVI} 4
C 87:{CWS117 ABCO-PIPO/ARPA/BERE} 4
C 88:{CWS312 ABCO/SYAL/FRVI} 4
C 89:{CWS313 ABCO-PIPO/SYAL/STJA} 4
C 90:{HQM121 POTR/ELGL} 7
C 91:{HQM411 POTR-PICO/SPDO/CAEU} 7
C 92:{HQS221 POTR/SYAL/ELGL} 7
C----------
      DATA COVINI /
     & 3,  3,  3,  8,  8,  8,  8,  8,  7,  7,
     & 7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
     & 7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
     & 7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
     & 7,  7,  7,  5, 10, 10, 10, 10, 10, 10,
     &10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
     &10, 10, 10,  9,  9,  9,  9,  4,  4,  4,
     & 4,  4,  4,  4,  4,  8,  4,  4,  4,  4,
     & 4,  4,  4,  4,  4,  4,  4,  4,  4,  24,
     & 24,  24 /

      DATA MYACT / 2521, 2548, 2553 /
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMCBA',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING FMCBA CYCLE = ',I2)
C----------
C  BEGIN ROUTINE.

C  NEED TO ADD CALLS TO INSURE THAT THE STRCLASS KEYWORD
C  IS ACTIVE WITH AT LEAST SILENT RUNNING ENABLED. IF IT
C  ALREADY ACTIVE, DO NOTHING. OTHERWISE ENABLE WITH SILENT
C  RUNNING

C  ZERO OUT THE CUMMULATIVE VARIABLES
C----------
      OLDCOV = COVTYP
      COVTYP = 0
      TOTBA  = 0.0
      PERCOV = 0.0
C----------
C  LOOP THROUGH THE TREE LIST
C----------
      IF (ITRN.GT.0) THEN

        DO KSP=1,MAXSP
          TBA(KSP) = 0.0
        ENDDO

        DO I=1,ITRN
          IF (FMPROB(I) .GT. 0.0) THEN
            KSP = ISP(I)
            BA1 = 5.454153E-03 * (DBH(I))**2
            TBA(KSP) = TBA(KSP) + FMPROB(I) * 5.454153E-03 * (DBH(I))**2
          ENDIF
C         USE THIS LOOP TO ZERO THIS VARIABLE, FOR LACK OF A BETTER PLACE.
          CURKIL(I) = 0.0
        ENDDO
C----------
C  DETERMINE WHICH SPECIES HAS THE MOST BASAL AREA
C  -> THAT WILL BE THE COVER TYPE
C----------
        BAMOST = 0.0
        DO KSP=1,MAXSP
          IF (TBA(KSP) .GT. BAMOST) THEN
            BAMOST = TBA(KSP)
            COVTYP = KSP
          ENDIF
          TOTBA = TOTBA + TBA(KSP)
        ENDDO
C----------
C  FIND PERCENT COVER (PERCOV) USING ALL TREES
C----------
        DO I=1,ITRN
          CW(I)=CRWDTH(I)
          CW(I) = CW(I)*CW(I)*3.1415927/4*FMPROB(I)
        ENDDO
        CALL COVOLP(.FALSE.,JOSTND,ITRN,IDUM,CW,PERCOV,
     &   CCCOEF)

      ENDIF
C----------
C     IF WE HAVE NO TREES (COVTYP=0) THEN WE MUST USE ONE OF THREE
C     METHODS FOR DETERMINING THE DEFAULT COVER TYPE.
C        1. ASSUME THAT THE COVER IS BASED ON LAST YEAR'S COVER BECAUSE
C           IT IS THE LAST YEAR THAT WILL HAVE LEFT FUELS BEHIND, AND MAY
C           DETERMINE THE TYPE OF SHRUBS THAT WILL REGENERATE.
C        2. IF IT IS THE FIRST YEAR OF THE CYCLE, USE THE HABITAT TYPE
C           (ITYPE) TO SET THE COVER TYPE.
C        3. IF WE DON'T HAVE HABITAT TYPE (E.G., IN CA), JUST ASSUME THAT
C           IT WAS A PP STAND, AND HOPE THAT TREES ARE PLANTED REALLY
C           QUICKLY.
C----------
      IF (COVTYP .EQ. 0) THEN
        IF (IYR .GT. IY(1)) THEN
          COVTYP = OLDCOV
        ELSEIF ((ITYPE .GT. 0) .AND. 
     >   ((KODFOR.GE.600 .AND. KODFOR.LT.700) .OR. KODFOR.EQ.799)) THEN
          COVTYP = COVINI(ITYPE)
        ELSE
          COVTYP = 10
        ENDIF
      ENDIF
C----------
C     USE COVER TYPE TO FIND THE LIVE FUELS: HERBS/SHRUBS & INITIAL CWD.
C     BEGIN...
C     CALCULATE THE INDEX: DOMINANT SPECIES INDX * 10 + OTTMAR STRUCTURAL STAGE.
C     USE THAT INDEX TO FIND THE APPROPRIATE MODEL. SPECIAL CONSIDERATIONS:
C      -> COVERTYPE=2 USES COVERTYPE=1 DATA STRUCTURES
C      -> THE 0-6 STRUCTURAL CLASSES RESULTING FROM SSTAGE ARE MAPPED TO
C         THE 7 STRUCTURAL STAGES OF OTTMAN AS FOLLOWS:

C     OTTMAR  SSTAGE
C     1       0                                (BARE GROUND)
C     1       1                                 STAND INITIATION
C     2       2       PERCOV <  60              STEM EXCLUSION, OPEN CANOPY
C     3       2       PERCOV >= 60              STEM EXCLUSION, CLOSED CANOPY
C     4       3                                 UNDERSTORY REINITIATION
C     5       4                                 YOUNG FOREST, SINGLE STRATUM
C     6       5                                 OLD FOREST, SINGLE STRATUM
C     7       6                                 OLD FOREST, MULTISTRATA

C     COVER TYPE IS THE DOMINANT SPECIES, WITH 2 (SUGAR PINE) USING 1 (WHITE PINE)
C----------
      ISPX = COVTYP
      IF (ISPX .EQ. 2) ISPX = 1
C----------
C  USE THE 'NO LOGGING APPARENT' MODEL BY DEFAULT
C----------
      ILOGMOD = 1
C----------
C     RESOLVE CASES WHERE SSTAGE=0 OR SSTAGE=2
C     NOTE THAT ROGER OTTMAR SAYS IT MAY BE MORE COMPLEX THAN THIS
C     SEOC IS MOISTURE-LIMITED
C     SECC IS LIGHT LIMITED
C----------
      CALL FMSSTAGE(TPAMIN,CCMIN,PCTSMX,SAWDBH,SSDBH,GAPPCT,IFMST,X3,
     &              FMPROB,FMICR)

      ISSX = IFMST
      IF (ISSX .EQ. 0) THEN
        ISSX = 1
        ILOGMOD = 2
      ELSEIF (ISSX .EQ. 2) THEN
        IF (PERCOV .GE. 60.0) ISSX = 3
      ENDIF

      ISPX = ISPX * 10 + ISSX
C----------
C  USE THE ISPX INDEX TO FIND THE FCC MODEL APPROPRIATE TO THE COVER
C  AND STRUCTURAL STAGE. THIS CAN BE USED FOR LIVE FUELS AND FOR THE
C  INITIAL VALUES OF THE CWD COMPONENTS. NOTE THAT THE SELECTION OF
C  ILOGMOD COULD BE DYNAMIC, OR UNDER USER CONTROL.
C----------
      IMODX = 0
      DO I = 1, 218
        IF (COVRINI(1,I) .EQ. ISPX) THEN
          IMODX = COVRINI(ILOGMOD+1,I)
          GOTO 90
        ENDIF
      ENDDO
   90 CONTINUE
C----------
C  USE THE FCC MODEL TO LOAD THE APPROPRIATE DATA. IF IPOSN=0 THEN
C  NO MATCH WAS FOUND, AND THE DEFAULT - INITIAL VALUES ALL ZERO -
C  WILL TAKE EFFECT.
C----------
      IPOSN = 0
      IF (IMODX .GT. 0) THEN
        DO I = 1, 43
          IF (NINT(FUELINI(1,I)) .EQ. IMODX) THEN
            IPOSN = I
            GOTO 91
          ENDIF
        ENDDO
      ENDIF
   91 CONTINUE
C----------
C NOW USE IMODX (THE FCC MODEL NUMBER) TO COPY THE RELEVANT FUEL DATA
C----------
      IF (IPOSN .EQ. 0) THEN
        FLIVE(1) = 0.
        FLIVE(2) = 0.
        DO I=1,MXFLCL
          STFUEL(I,2) = 0.
        ENDDO
      ELSE
C----------
C     FIDDLING WITH INITIALIZATION OF FUEL CLASSES 4-6 IS THE RESULT OF
C     THE DIFFERENT DIAMETER BREAKPOINTS FOR THIS MODEL COMPARED TO THE
C     DEFINITIONS USED FOR THE INITIALIZATION DATA.
C----------
        FLIVE(1)  = FUELINI(2,IPOSN)
        FLIVE(2)  = FUELINI(3,IPOSN)
        STFUEL(1,2) = FUELINI(4,IPOSN)
        STFUEL(2,2) = FUELINI(5,IPOSN)
        STFUEL(3,2) = FUELINI(6,IPOSN)
        STFUEL(4,2) = FUELINI(7,IPOSN) * (2./ 3.)
        STFUEL(5,2) = FUELINI(7,IPOSN) * (1./ 3.) +
     &              FUELINI(8,IPOSN) * (3./11.)
        STFUEL(6,2) = FUELINI(8,IPOSN) * (8./11.) +
     &              FUELINI(9,IPOSN) !gt 20" material from the FCCS is thrown
c                                     into the 12 - 20" class (the old gt 12" class)
        STFUEL(7,2) = 0  ! this is zero instead of FUELINI(9,IPOSN) so that 
c                        folks with old databases with the fuel_gt_12 column
c                        filled in won't get extra large fuel added in when 
c                        the fuels are initialized.
        STFUEL(8,2) = FUELINI(10,IPOSN)
        STFUEL(9,2) = FUELINI(11,IPOSN)
        STFUEL(10,2) = FUELINI(12,IPOSN)        
        STFUEL(11,2) = FUELINI(13,IPOSN)              
      ENDIF
      DO I=1,MXFLCL
          STFUEL(I,1) = 0.
      ENDDO
C----------
      IF (DEBUG) WRITE(JOSTND,8) COVTYP,ISSX,PERCOV,IMODX,FLIVE
    8 FORMAT(' IN FMCBA, COVTYP=',I3,' STRCLS=',I3,
     &       ' PERCOV=',F6.3,' FCC=',I3,' FLIVE=',2F6.3)
C----------
C     INITIALIZE THE DEAD FUELS ONLY FOR THE FIRST YEAR OF THE SIMULATION
C----------
      IF (IYR .NE. IY(1)) RETURN

C       *** SPECIAL CODE FOR SO-FFE DECAY RATES ***
C       *** NORMALLY IN *FMVINIT*               ***

C       MODIFY DECAY RATES IF THE FOREST CODE INDICATES A
C       CALIFORNIA SETTING; OTHERWISE THE DEFAULT OREGON
C       RATE WILL BE USED

        IF ((KODFOR .GE. 500 .AND. KODFOR .LT. 600)
     >    .OR. KODFOR .EQ. 701) THEN
          DKRT(1,1) = 0.025   ! < 0.25" - California
          DKRT(2,1) = 0.025   ! 0.25 - 1"
          DKRT(3,1) = 0.025   ! 1 - 3"
          DKRT(4,1) = 0.0125  ! 3 - 6"
          DKRT(5,1) = 0.0125  ! 6 - 12"
          DKRT(6,1) = 0.0125  ! 12 - 20"
          DKRT(7,1) = 0.0125  ! 20 - 35"
          DKRT(8,1) = 0.0125  ! 35 - 50"
          DKRT(9,1) = 0.0125  ! > 50"

          DO I = 1,9
            DO J = 2,4
              DKRT(I,J) = DKRT(I,1)  ! map to all 4 classes
            ENDDO
          ENDDO

        ELSE ! Oregon 
        
C     DECAY RATES BASED ON WORKSHOP RESULTS FOR KIM MELLEN-MCLEAN'S CWD MODEL
C     FIRST BASE RATES ARE SET (BY DECAY RATE CLASS) AND THEN THEY ARE ADJUSTED
C     BASED ON HABITAT TYPE (TEMPERATURE AND MOISTURE CATEGORY)

          DKRT(1,1) = 0.076 ! < 0.25"
          DKRT(2,1) = 0.076 ! 0.25 - 1"
          DKRT(3,1) = 0.076 ! 1 - 3"
          DKRT(4,1) = 0.019 ! 3 - 6"
          DKRT(5,1) = 0.019 ! 6 - 12"
          DKRT(6,1) = 0.019  ! 12 - 20"
          DKRT(7,1) = 0.019  ! 20 - 35"
          DKRT(8,1) = 0.019  ! 35 - 50"
          DKRT(9,1) = 0.019  !  > 50"
          
          DKRT(1,2) = 0.081 ! < 0.25"
          DKRT(2,2) = 0.081 ! 0.25 - 1"
          DKRT(3,2) = 0.081 ! 1 - 3"
          DKRT(4,2) = 0.025 ! 3 - 6"
          DKRT(5,2) = 0.025 ! 6 - 12"
          DKRT(6,2) = 0.025  ! 12 - 20"
          DKRT(7,2) = 0.025  ! 20 - 35"
          DKRT(8,2) = 0.025  ! 35 - 50"
          DKRT(9,2) = 0.025  !  > 50"
          
          DKRT(1,3) = 0.090 ! < 0.25"
          DKRT(2,3) = 0.090 ! 0.25 - 1"
          DKRT(3,3) = 0.090 ! 1 - 3"
          DKRT(4,3) = 0.033 ! 3 - 6"
          DKRT(5,3) = 0.033 ! 6 - 12"
          DKRT(6,3) = 0.033  ! 12 - 20"
          DKRT(7,3) = 0.033  ! 20 - 35"
          DKRT(8,3) = 0.033  ! 35 - 50"
          DKRT(9,3) = 0.033  !  > 50"      
          
          DKRT(1,4) = 0.113 ! < 0.25"
          DKRT(2,4) = 0.113 ! 0.25 - 1"
          DKRT(3,4) = 0.113 ! 1 - 3"
          DKRT(4,4) = 0.058 ! 3 - 6"
          DKRT(5,4) = 0.058 ! 6 - 12"
          DKRT(6,4) = 0.058  ! 12 - 20"
          DKRT(7,4) = 0.058  ! 20 - 35"
          DKRT(8,4) = 0.058  ! 35 - 50"
          DKRT(9,4) = 0.058  !  > 50"
          
          TEMP = SOHMC(ITYPE)
          MOIST = SOWMD(ITYPE)
          
          DO I = 1,9
            DO J = 1,4
              IF (I .LE. 3) THEN
                K = 1
              ELSEIF (I .LE. 5) THEN 
                K = 2
              ELSE 
                K = 3
              ENDIF
              DKRT(I,J) = DKRT(I,J)*DKRADJ(TEMP,MOIST,K)
              IF (DKRT(I,J) .GT. 1.0) DKRT(I,J) = 1.0
            ENDDO
          ENDDO

C       adjust the decay rates if smaller wood is decaying more slowly than larger wood.
C       in this case, bump up the decay rate of the smaller wood to that of the larger wood.

          DO I = 9,2,-1
            DO J = 1,4
              IF ((DKRT(I,J)-DKRT(I-1,J)) .GT. 0) THEN 
                  DKRT(I-1,J) = DKRT(I,J)                         
              ENDIF
            ENDDO
          ENDDO

        ENDIF


C     THESE RATES ARE THE SAME FOR CA AND OR

        DO J = 1,4
          DKRT(10,J) = 0.5    ! litter loss/yr
          DKRT(11,J) = 0.002  !   duff loss/yr
          DO I = 1,10
            PRDUFFT(I,J) = 0.02
          ENDDO
        ENDDO

C     COPY TEMPORARY VALUES INTO WORKING ARRAYS THAT
C     HAVEN'T BEEN MODIFIED BY KEYWORDS
C     ONLY DO THIS IF DURING THE NORMAL CALL, NOT FROM SVSTART
C     ALSO CHECK THE VALUE OF DKR.  IF THE DECAY RATES HAVE NOT BEEN
C     SET BY THE USER WITH FUELDCAY, THEY SHOULD BE SET TO -1.  UNLESS THE
C     FUELMULT KEYWORD WAS USED - IN THIS CASE DKR IS -1*THE MULTIPLIER ENTERED.
C     IF FUELMULT WAS USED, USE THE MULTIPLIER WITH THE DEFAULT RATES.

        IF ( ISWTCH .NE. 1 ) THEN
          DO I = 1,MXFLCL
            DO J = 1,4
              IF (DKR(I,J) .LT. 0.0) THEN
                IF ((DKR(I,J).GT.-1.01) .AND. (DKR(I,J).LT.-0.99)) THEN
                  DKR(I,J) = DKRT(I,J)
                ELSE
                  DKR(I,J) = -1*DKR(I,J)*DKRT(I,J)
                ENDIF  
              ENDIF
            ENDDO
          ENDDO
          DO I = 1,10
            DO J = 1,4
              IF (PRDUFF(I,J) .LT. 0.0) PRDUFF(I,J) = PRDUFFT(I,J) 
              TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
            ENDDO
          ENDDO
        ENDIF

        ENTRY SNGCOE

C       ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C       IS NOT ACTIVE. CALLED FROM SVSTART.

C       *** SPECIAL CODE FOR SO-FFE SNAG DYNAMICS ***
C       *** NORMALLY IN *FMVINIT*                 ***


C     DEFAULT SNAG RATE PARAMETERS ARE SET RELATIVE TO THE VALUES FOR
C     PP (SPECIES 10), WHICH WAS ORIGINALLY IN THE 'MEDIUM' SPECIES
C     GROUP.

C     SO DOC SAYS IC (6) SHOULD INCLUDE SNOW BREAKAGE; NOT IN YET
C     SO DOC SAYS EASTSIDE PINE DOESN'T BREAK. ASSUME HERE THAT IS PP
C     AND THAT THE BREAKAGE RATE IS SUCH THAT IT WILL TAKE 100 YEARS TO
C     GET TO 50% HEIGHT LOSS. ALL SNAGS WILL HAVE FALLEN IN THAT TIME
C     ANYWAYS.

        IF ((KODFOR .GE. 500 .AND. KODFOR .LT. 600)
     >    .OR. KODFOR .EQ. 701) THEN
          DO I = 1,MAXSP                        ! CALIFORNIA
            SELECT CASE (I)

              ! white, sugar, lodgepole & ponderosa pine
              ! mountain hemlock, whitebark pine, western hemlock
              ! all hardwoods
              CASE (1,2,7,10,5,16,19,21:31,33)
                IF (ALLDWN(I) .LT. 0.0)  ALLDWN(I) = 100.0
                IF (FALLX(I)  .LT. 0.0)  FALLX(I) =    1.235

              ! Douglas-fir, white fir,grand fir, pacific silver fir
              ! Engelmann spruce, red fir,subalpine fir, noble fir
              ! western larch, other softwoods
              CASE (3,4,8,9,12:15,17,32)
                IF (ALLDWN(I) .LT. 0.0)  ALLDWN(I) = 100.0
                IF (FALLX(I)  .LT. 0.0)  FALLX(I)  =   0.882

              ! incense cedar, western redcedar, pacific yew
              CASE (6,18,20)
                IF (ALLDWN(I) .LT. 0.0)  ALLDWN(I) =  100.0
                IF (FALLX(I)  .LT. 0.0)  FALLX(I)  =    0.687

              ! juniper
              CASE (11)
                IF (ALLDWN(I) .LT. 0.0)  ALLDWN(I) =  150.0
                IF (FALLX(I)  .LT. 0.0)  FALLX(I)  =    0.687
            END SELECT

C         HARD SNAGS NEVER BECOME SOFT. HEIGHT LOSS IS THE SAME FOR ALL SPP
C         HEIGHT LOSS CEASES FOR THE LAST 50% (USED TO BE SET BY HTR2, CHANGED FEB 2002)

            IF (DECAYX(I) .LT. 0.0) DECAYX(I) = 999.0

            IF (HTX(I,1)  .LT. 0.0) HTX(I,1) = 1.0 
            IF (HTX(I,3)  .LT. 0.0) HTX(I,3) = 1.0 
            IF (HTX(I,2)  .LT. 0.0) HTX(I,2) = 0.0
            IF (HTX(I,4)  .LT. 0.0) HTX(I,4) = 0.0

          ENDDO

          IF (PBSOFT .LT. 0.0) PBSOFT = 1.0
          IF (PBSMAL .LT. 0.0) PBSMAL = 0.9
          
        ELSE

          DO I = 1,MAXSP                        ! OREGON
            SELECT CASE (I)

              ! white pine, sugar pine, Douglas-fir, whitebark pine
              ! western larch, other softwoods
              CASE (1,2,3,16,17,32)
                IF (ALLDWN(I) .LT. 0.0)  ALLDWN(I) = 110.
                IF (DECAYX(I) .LT. 0.0)  DECAYX(I) = 1.0
                IF (FALLX(I)  .LT. 0.0)  FALLX(I) =  1.0
                DO J= 1,4
                  IF (HTX(I,J) .LT. 0.0) HTX(I,J) =  1.0
                ENDDO

              ! white fir, grand fir, mountain hemlock, incense cedar
              ! lodgepole pine, Engelmann spruce, red fir, subalpine fir
              ! pacific silver fir, noble fir, western redcedar,
              ! western hemlock, pacific yew, all hardwoods
              CASE (4,5,6,7,8,9,12:15,18:31,33)
                IF (ALLDWN(I) .LT. 0.0)  ALLDWN(I) =  90.
                IF (DECAYX(I) .LT. 0.0)  DECAYX(I) = 1.0
                IF (FALLX(I)  .LT. 0.0)  FALLX(I) =  1.0
                DO J= 1,4
                  IF (HTX(I,J) .LT. 0.0) HTX(I,J) =  1.0
                ENDDO

              ! ponderosa pine
              CASE (10)
                IF (ALLDWN(I) .LT. 0.0)  ALLDWN(I) = 100.
                IF (DECAYX(I) .LT. 0.0)  DECAYX(I) = 1.0
                IF (FALLX(I)  .LT. 0.0)  FALLX(I) =  1.0
                DO J= 1,4
                  IF (HTX(I,J) .LT. 0.0) HTX(I,J) =  1.0
                ENDDO

              ! juniper
              CASE (11)
                IF (ALLDWN(I) .LT. 0.0)  ALLDWN(I) = 100.
                IF (DECAYX(I) .LT. 0.0)  DECAYX(I) = 1.0
                IF (FALLX(I)  .LT. 0.0)  FALLX(I) =  1.0
                DO J= 1,4
                  IF (HTX(I,J) .LT. 0.0) HTX(I,J) =  1.0
                ENDDO

            END SELECT
          ENDDO
  
          IF (PBSOFT .LT. 0.0) PBSOFT = 0.0
          IF (PBSMAL .LT. 0.0) PBSMAL = 0.0
          
        ENDIF
        
        ! set whether each species is a softwood or not
   
        DO I = 1,MAXSP       
          SELECT CASE (I)
            CASE (1:20,32)
              LSW(I) = .TRUE.
            CASE (21:31,33)
              LSW(I) = .FALSE.
          END SELECT  
        ENDDO       

C
C       RETURN FOR SNGCOE ENTRY POINT (FFE NOT ACTIVE)

        IF ( .NOT. LFMON ) RETURN

C       *** SPECIAL CODE FOR SO-FFE DEAD CROWN DYNAMICS ***
C       *** NORMALLY IN *FMVINIT*                       ***

C       TFALL() - TIME TO FALL FOR DEAD CROWN COMPONENTS. THIS VARIABLE
C       IS NOT UNDER USER-CONTROL BUT MIGHT BE SOMEDAY.
C       [LITTERFALL AND SMALL TWIG FALL VALUES CHANGED 2/97. SB&ER]
C       TFALL INDEXING USES CROWNW() DIMENSIONS, I.E.

C       0 :  FOLIAGE
C       1 : <0.25"
C       2 :  0.25" -   1"
C       3 :  1"    -   3"
C       4 :  3"    -   6"
C       5 :  6"    -  12"

C       IF THE VALUE OF TFALL(I,-) IS LARGER THAN 20, PARAMETER TFMAX IN
C       **FMPARM.F77** MUST BE ADJUSTED TO EQUAL THE NEW VALUE, AND LOOPS
C       INVOLVING THE VALUE (SEE FMSCRO) MUST BE RE-EXAMINED TO INSURE
C       THAT THEY BEHAVE PROPERLY.

C       FINE DEAD FOLIAGE DIFFERS BETWEEN CA AND OR

        IF ((KODFOR .GE. 500 .AND. KODFOR .LT. 600)
     >    .OR. KODFOR .EQ. 701) THEN
          DO I = 1, MAXSP            !! CALIFORNIA
            TFALL(I,0) =  3.0
            TFALL(I,1) = 10.0
            TFALL(I,2) = 15.0
          ENDDO
        ELSE
          DO I = 1, MAXSP            !! OREGON
            TFALL(I,0) = 2.0
            TFALL(I,1) = 5.0
            TFALL(I,2) = 5.0
          ENDDO
        ENDIF

C       DEAD INCENSE-CEDAR FOLIAGE DROPS IN ONE YR

        TFALL(6,0)     = 1.0
        
C       DEAD LARCH AND HARDWOOD FOLIAGE DROPS IN ONE YR  
      
        TFALL(17,0) = 1.0
        DO I = 21,31
          TFALL(I,0) = 1.0
        ENDDO
        TFALL(33,0) = 1.0
        
C       LARGER DEAD FOLIAGE IS IDENTICAL FOR CA AND OR

        DO I = 1,MAXSP
          SELECT CASE (I)

          ! white pine, sugar pine, Douglas-fir
          ! white fir, mountain hemlock
          ! lodgepole pine, red fir,
          ! juniper, grand fir, subalpine fir, pacific silver fir,
          ! noble fir, whitebark pine, western larch, 
          ! western hemlock, all hardwoods, other softwoods
          CASE (1,2,3,4,5,7,9,11,12:17,19,21:33)
            TFALL(I,3) = 15.0

          ! incense-cedar, western redcedar, pacific yew
          CASE (6,18,20)
            TFALL(I,3) = 20.0

          ! Engelmann spruce, ponderosa pine
          CASE (8,10)
            TFALL(I,3) = 10.0

          END SELECT

          DO J = 4,5
            TFALL(I,J) = TFALL(I,3)
          ENDDO

C         DEAD LEAF FALL CANNOT BE > LIVE

          TFALL(I,0) = MIN(TFALL(I,0), LEAFLF(I))

C         TFALL(I,3) CANNOT BE < TFALL(I,2)

          IF (TFALL(I,2) .GT. TFALL(I,3)) THEN
            TFALL(I,2) = TFALL(I,3)
          ENDIF

        ENDDO

C       *** END OF SPECIAL SO-FFE INITIALIZATION ***
C       *** SECTION                              ***
        
C       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT

        CALL OPFIND(1,MYACT(2),J)
        IF (J .GT. 0) THEN
          CALL OPGET(J,2,JYR,IACTK,NPRM,PRMS)
          IF ((PRMS(1) .GE. 0) .AND. (PRMS(2) .GE. 0)) THEN
            CALL FMPHOTOVAL(NINT(PRMS(1)), NINT(PRMS(2)), FOTOVAL, 
     >                      FOTOVALS)

            DO I = 1, MXFLCL
              IF (FOTOVAL(I) .GE. 0) STFUEL(I,2) = FOTOVAL(I)
              IF (I .LE. 9) STFUEL(I,1) = FOTOVALS(I)
            ENDDO                 

C           IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
C           DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C           NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

            IF (FOTOVAL(1).GE.0 .AND. ISWTCH.NE.1) CALL OPDONE(J,IYR)

          ELSE
            WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',
     &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ',
     &      'REQUIRED.',/1X)")
            CALL RCDSET (2,.TRUE.)
          ENDIF
        ENDIF
 
C       ASSUME THE FUELS ARE UNPILED.
C       CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
C       FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT

        CALL OPFIND(1,MYACT(1),J)
        IF (J .GT. 0) THEN
          CALL OPGET(J,12,JYR,IACTK,NPRM,PRMS)
          IF (PRMS(2) .GE. 0) STFUEL(3,2) = PRMS(2)
          IF (PRMS(3) .GE. 0) STFUEL(4,2) = PRMS(3)
          IF (PRMS(4) .GE. 0) STFUEL(5,2) = PRMS(4)
          IF (PRMS(5) .GE. 0) STFUEL(6,2) = PRMS(5)
          IF (PRMS(6) .GE. 0) STFUEL(10,2) = PRMS(6)
          IF (PRMS(7) .GE. 0) STFUEL(11,2) = PRMS(7)
          IF (PRMS(8) .GE. 0) STFUEL(1,2) = PRMS(8)          
          IF (PRMS(9) .GE. 0) STFUEL(2,2) = PRMS(9)           
          IF (PRMS(1) .GE. 0) THEN
            IF ((PRMS(8) .LT. 0) .AND. (PRMS(9) .LT. 0)) THEN
              STFUEL(1,2) = PRMS(1) * 0.5
              STFUEL(2,2) = PRMS(1) * 0.5
            ENDIF                 
            IF ((PRMS(8) .LT. 0) .AND. (PRMS(9) .GE. 0)) THEN
              STFUEL(1,2) = MAX(PRMS(1) - PRMS(9),0.)
            ENDIF  
            IF ((PRMS(8) .GE. 0) .AND. (PRMS(9) .LT. 0)) THEN
              STFUEL(2,2) = MAX(PRMS(1) - PRMS(8),0.)
            ENDIF  
          ENDIF                
          IF (PRMS(10) .GE. 0) STFUEL(7,2) = PRMS(10) 
          IF (PRMS(11) .GE. 0) STFUEL(8,2) = PRMS(11) 
          IF (PRMS(12) .GE. 0) STFUEL(9,2) = PRMS(12)   

C         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

          IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)

        ENDIF

        CALL OPFIND(1,MYACT(3),J)
        IF (J .GT. 0) THEN
          CALL OPGET(J,9,JYR,IACTK,NPRM,PRMS)
          IF (PRMS(1) .GE. 0) STFUEL(1,1) = PRMS(1)
          IF (PRMS(2) .GE. 0) STFUEL(2,1) = PRMS(2)
          IF (PRMS(3) .GE. 0) STFUEL(3,1) = PRMS(3)
          IF (PRMS(4) .GE. 0) STFUEL(4,1) = PRMS(4)
          IF (PRMS(5) .GE. 0) STFUEL(5,1) = PRMS(5)
          IF (PRMS(6) .GE. 0) STFUEL(6,1) = PRMS(6)
          IF (PRMS(7) .GE. 0) STFUEL(7,1) = PRMS(7)          
          IF (PRMS(8) .GE. 0) STFUEL(8,1) = PRMS(8)                           
          IF (PRMS(9) .GE. 0) STFUEL(9,1) = PRMS(9)         

C         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

          IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)

        ENDIF

C----------
C     DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
C     OF BASAL AREA IN THE STAND.
C----------

        DO ISZ = 1,MXFLCL
          IF (TOTBA .GT. 0.0) THEN
            DO KSP = 1,MAXSP
              IF (TBA(KSP) .GT. 0.0) THEN
                DO J = 1,2
                  PRCL = TBA(KSP) / TOTBA
                  IDC = DKRCLS(KSP)
                  ADD = PRCL * STFUEL(ISZ,J)
                  CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + ADD
                ENDDO
              ENDIF
            ENDDO
          ELSE
            IDC = DKRCLS(COVTYP)
            DO J = 1,2
              CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + STFUEL(ISZ,J)
            ENDDO
          ENDIF
        ENDDO

C     IN FIRST YEAR, SET C-REPORTING REGION FOR FOREST CODES IN CALIFORNIA
        IF ((KODFOR .GE. 500 .AND. KODFOR .LT. 600)
     >    .OR. KODFOR .EQ. 701) ICHABT = 2

      RETURN
      END
 
