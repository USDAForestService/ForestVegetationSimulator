      SUBROUTINE MCNVRT(IKEY,JD,X)
      IMPLICIT NONE

C     MCNVRT--BASE/M    DATE OF LAST REVISION:  06/24/10
C     
C     PURPOSE: RETURNS A VECTOR 'X(NP)' OF CONVERSION FACTORS TO BE USED
C              IN CONJUNCTION WITH THE OPTION PROCESSOR'S PARAMETERS.
C              THE ACTIVITY
C              CODE 'KEY' LINKS THE ACTIVITY TO THE REQUIRED
C              DEATILS OF THE CONVERSION
C     
C     CODED BY: DON ROBINSON, ESSA TECHNOLOGIES
C
C     THIS IS A **PRELIMINARY** IMPLEMENTATION. IT COULD EVENTUALLY REPLACE
C     ALL THE CONVERSION MESSINESS FOUND IN INITRE, RDIN, ESTAB, AND OPLIST.
C
C     JD = DIRECTION OF CONVERSION
C           

      INCLUDE     'METRIC.F77'

      CHARACTER*8 KEYWRD
      INTEGER     IKEY, JD
      INTEGER     KEY
      INTEGER     I,IARY,LOC
      REAL        X(N_METRIC_PRMS)
      
      KEY = IKEY

C     SET DEFAULT VECTOR TO 1

      DO I=1,N_METRIC_PRMS
        X(I) = 1.0
      ENDDO
      
      CALL OPKEY(KEY)
      IF (KEY.LE.0) RETURN
      LOC=(KEY/100)+1
      KEY=MOD(KEY,100)
      GOTO (42,42,43,44,45,46,47,48,49,52,53,54,55,56,57,58),LOC
        
C     DETERMINE THE PARAMETER MULTIPLIERS FOR THE BASE ACTIVITIES 
      
   42 GOTO (1000,1000,1003,1000,1000,1000,1000,1000,1000,1000,
     >      1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,
     >      1000,1000,1000,1024,1025,1026,1027,1000,1029,1000,
     >      1000,1000,1000,1000,1035,1000,1037,1038,1000,1000,
     >      1000,1000,1043,1044,1000,1000,1000,1048,1000,1000,
     >      1000,1000,1000,1000,1000,1000,1000,1000,1059,1000,
     >      1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,
     >      1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,
     >      1000,1000,1000,1000,1000,1000,1000,1088,1000,1000,
     >      1000,1000,1000,1000,1000,1096,1000,1000,1000,1000,
     >      1000,1000,1000,1000,1000,1000,1000,1108,1000,1110,
     >      1111,1112,1000,1000,1115) KEY
           
 1000 CONTINUE
      GO TO 61

 1003 CONTINUE  ! Multipliers for the FIXCW (90) activity code.
      X(3)=INtoCM
      X(4)=INtoCM
      GO TO 61
C
C     IT SEEMS ODD THAT BOTH THINBTA AND THINRDEN WOULD HAVE IDENTICAL
C     MAPPINGS...THIS IS A REAL KLUDGE
C
 1024 CONTINUE
	IF (IKEY .EQ. 223) THEN   ! THINBTA (223) code.
        X(1)=1.0/ACRtoHA
        X(3)=INtoCM
        X(4)=INtoCM
        X(5)=FTtoM
        X(6)=FTtoM
      ELSEIF (IKEY .EQ. 234) THEN  ! THINRDEN (234) code.
        X(1)=FT2pACRtoM2pHA
        X(4)=INtoCM
        X(5)=INtoCM
	ENDIF
      GO TO 61

 1025 CONTINUE  ! Multipliers for the THINATA (224) activity code.
      X(1)=1.0/ACRtoHA
      X(3)=INtoCM
      X(4)=INtoCM
      X(5)=FTtoM
      X(6)=FTtoM
      GO TO 61

 1026 CONTINUE  ! Multipliers for the THINBBA (225) activity code.
      X(1)=FT2pACRtoM2pHA
      X(3)=INtoCM
      X(4)=INtoCM
      X(5)=FTtoM
      X(6)=FTtoM
      GO TO 61

 1027 CONTINUE  ! Multipliers for the THINABA (226) activity code.
      X(1)=FT2pACRtoM2pHA
      X(3)=INtoCM
      X(4)=INtoCM
      X(5)=FTtoM
      X(6)=FTtoM
      GO TO 61

 1029 CONTINUE  ! Multipliers for the THINDBH (228) activity code.
      IF (IKEY .EQ. 228) THEN
        X(1)=INtoCM
        X(2)=INtoCM
        X(5)=1.0/ACRtoHA
        X(6)=FT2pACRtoM2pHA
	ELSEIF (IKEY .EQ. 235) THEN ! THINPT (235) code
        X(3)=INtoCM
        X(4)=INtoCM
	ENDIF
      GO TO 61

 1035 CONTINUE  ! THINHT (234) code.
      X(1)=INtoCM
      X(2)=INtoCM
      X(5)=1.0/ACRtoHA
      X(6)=FT2pACRtoM2pHA
      GO TO 61

 1037 CONTINUE  ! Multipliers for the TOPKILL (111) activity code.
      X(2)=FTtoM
      X(3)=FTtoM
      GO TO 61

 1038 CONTINUE  ! Multipliers for the HTGSTOP (110) activity code.
      X(2)=FTtoM
      X(3)=FTtoM
      GO TO 61

 1043 CONTINUE  ! Multipliers for the VOLUME (217) activity code.
      X(2)=INtoCM
      X(3)=INtoCM
      X(4)=FTtoCM
      GO TO 61

 1044 CONTINUE  ! Multipliers for the BFVOLUME (218) activity code.
      X(2)=INtoCM
      X(3)=INtoCM
      X(4)=FTtoCM
      GO TO 61

 1048 CONTINUE  ! Multipliers for the MINHARV (200) activity code.
      X(1)=FT3pACRtoM3pHA
      X(3)=FT2pACRtoM2pHA
      GO TO 61

 1059 CONTINUE  ! Multipliers for the MORTMULT (94) activity code.
      X(3)=INtoCM
      X(4)=INtoCM
      GO TO 61

 1088 CONTINUE  ! Multipliers for the FIXMORT (97) activity code.
      X(3)=INtoCM
      X(4)=INtoCM
      GO TO 61

 1096 CONTINUE  ! Multipliers for the CRNMULT (81) activity code.
      X(3)=INtoCM
      X(4)=INtoCM
      GO TO 61

 1108 CONTINUE  ! Multipliers for the PRUNE (233) activity code.
      X(2)=FTtoM
      X(5)=INtoCM
      X(6)=INtoCM
      GO TO 61

 1110 CONTINUE  ! Multipliers for the FIXDG (98) activity code.
      X(3)=INtoCM
      X(4)=INtoCM
      GO TO 61

 1111 CONTINUE  ! Multipliers for the FIXHTG (99) activity code.
      X(3)=INtoCM
      X(4)=INtoCM
      GO TO 61

 1112 CONTINUE  ! Multipliers for the THINSDI (230) activity code.
      X(3)=INtoCM
      X(4)=INtoCM
      GO TO 61

 1115 CONTINUE  ! Multipliers for the THINCC (231) activity code.
      X(3)=INtoCM
      X(4)=INtoCM
      GO TO 61

C     ESTABLISHMENT MODEL ACTIVITY CODES.

   43 GOTO (2000,2020,2030,2000,2000,2000,2000,2000,2000,2000,
     >      2000,2000,2000,2000,2150,2000,2000,2000,2000,2000,
     >      2000,2000,2000,2000,2000,2260,2000,2000,2000,2000,
     >      2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,
     >      2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,
     >      2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,
     >      2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,
     >      2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,
     >      2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,
     >      2000,2000,2000,2000,2000,2000,2000,2000,2000,2000) KEY
     
 2000 CONTINUE
      GO TO 61
 2020 CONTINUE  ! Multipliers for the PLANT (302) activity code.
      X(2)=1.0/ACRtoHA
      X(5)=FTtoM
      GO TO 61
 2030 CONTINUE  ! Multipliers for the NATURAL (303) activity code.
      X(2)=1.0/ACRtoHA
      X(5)=FTtoM
      GO TO 61
 2150 CONTINUE  ! Multipliers for the HTADJ (315) activity code.
      X(2)=FTtoM
      GO TO 61 
 2260 CONTINUE  ! Multipliers for the SPROUT (450) activity code.
      X(4)=INtoCM
      X(5)=INtoCM      
      GO TO 61 
      
C     PLACEHOLDERS FOR CONVERSION OF OTHER KEYWORDS

   44 CONTINUE
c      CALL TMKEY(KEY,KEYWRD)
      GO TO 61
   45 CONTINUE
c      CALL MPKEY(KEY,KEYWRD)
      GO TO 61
   46 CONTINUE
c      CALL CVKEY(KEY,KEYWRD)
      GO TO 61
   47 CONTINUE
c      CALL BWKEY(KEY,KEYWRD)
      GO TO 61
   48 CONTINUE
C  ** free spot **
      GO TO 61
   49 CONTINUE
c      CALL RRKEY(KEY,KEYWRD)
      GO TO 61
   52 CONTINUE
c      CALL BRKEY(KEY,KEYWRD)
      GO TO 61
   53 CONTINUE
c      CALL MISKEY(KEY,KEYWRD)
      GO TO 61
   54 CONTINUE
c      CALL ANKEY(KEY,KEYWRD)
      GO TO 61

C     DFB ACTIVITY CODES

   55 GOTO (3000,3000,3000,3000,3000,3000,3000,3000,3000,3010,
     >      3000,3000,3000,3000,3000,3000,3000,3000,3000,3000) KEY
 3000 CONTINUE
      GO TO 61
 3010 CONTINUE  ! Multipliers for the WINDTHR (302) activity code.
      X(2)=1.0/ACRtoHA
      GO TO 61
      
   56 CONTINUE
c      CALL BMKEY(KEY,KEYWRD)
      GO TO 61

C  ROOT DISESASE ACTIVITY CODES
C
C  PRODUCE METRIC VALUES FOR GENERAL ROOT DISEASE MODEL. NOTE
C  THAT THE X() INDICES ARE +1 COMPARED TO THE PRMS() OR
C  PARMS() ARRAY THAT THEY ARE USED WITH. THIS IS BECAUSE OF 
C  THE DIFFERENT WAY THAT THESE ARE STORED, COMPARED TO THE
C  BASE MODEL KEYWORDS. SO IF PRMS(2) IS DBH THAT SHOULD BE 
C  CONVERTED FROM INCHES TO CM, X(3) IS LOADED WITH THE
C  CONVERSION FACTOR 'IN2CM'

   57 GOTO (5701,5700,5703,5700,5700,5700,5700,5700,5700,5700,
     >      5700,5700,5700,5714,5715,5716,5717,5700,5700,5700,
     >      5700,5700,5700,5700,5700,5700,5700,5700,5700,5730,
     >      5731,5732,5700,5700,5700,5700,5700,5700,5700,5700,
     >      5700,5700) KEY

 5700 GO TO 61

 5701 CONTINUE   ! spread
      X(2) = FTtoM
      X(3) = FTtoM
      GO TO 61
        
 5703 CONTINUE   ! pstump
      X(2) = INtoCM
      GO TO 61
        
 5714 CONTINUE   ! windthr  << may not be metric in RDIN
      X(3) = HAtoACR
      GO TO 61

 5715 CONTINUE   ! bbtype1   
      X(2) = INtoCM
      X(3) = HAtoACR
      GO TO 61

 5716 CONTINUE   ! bbtype2
      X(2) = INtoCM
      X(3) = HAtoACR
      GO TO 61

 5717 CONTINUE   ! bbtype3
      X(2) = INtoCM
      X(3) = HAtoACR
      GO TO 61

 5730 CONTINUE   ! borate
      X(2) = INtoCM
      GO TO 61

 5731 CONTINUE   ! spore
      X(2) = INtoCM
      GO TO 61

 5732 CONTINUE   ! bbtype4
      X(2) = INtoCM
      X(3) = HAtoACR
      GO TO 61

C     METRIFY THE FIRE MODEL OPTION CALLS.

   58 CALL FMKEY (KEY,KEYWRD)
      GO TO(5800,5800,5800,5800,5800,5806,5807,5800,5800,5800,
     &      5800,5800,5800,5800,5800,5800,5800,5800,5800,5820,
     &      5821,5822,5800,5800,5800,5800,5800,5800,5800,5800,
     &      5800,5800,5800,5834,5800,5800,5800,5800,5839,5800,
     &      5800,5800,5800,5800,5800,5800,5800,5800,5849,5800), KEY

 5800 GO TO 61

C After implement metrification of DEFULMOD and FIRECALC keywords (EBt#1630), I
C noted that the indexes for ??ALL?? the metric FFE keywords seem to be
C off-by-one. This is probably because some extensions (e.g. ESTAB) pass ARRAY(*)
C to OPLIST, but others pass PRMS(*). When building metrification in MCNVRT using
C FMIN/ESIN as a guide to indexes, ARRAY(i) values should be indexed using (i+1)
C and PRM(i) values should be indexed using (i). 
C
C I have changed **and not yet tested** these keywords:
C
C   SIMFIRE, FLAMEADJ, SALVAGE,
C   FUELINIT, SNAGINIT, FUELMOVE
C
C DEFULMOD, FIRECALC - both work correctly (EBt#1633)
C
C Remove this comment when confirmed. - DR 19 Jan 2009
C 

 5806 CONTINUE    !SIMFIRE
      X(1) = MItoKM
C             NOTE THAT FtoC1 ISN'T QUITE ACCURATE BECAUSE ACTUAL TRANSLATION IS: 
C              val*FtoC1 +FtoC2 BUT THIS WILL HAVE TO BE CLOSE ENOUGH
      X(3) = FtoC1
      GO TO 61

 5807 CONTINUE    !FLAMEADJ
      X(2) = FTtoM
      X(4) = FTtoM      
      GO TO 61

 5820 CONTINUE    !SALVAGE
      X(1) = INtoCM
      X(2) = INtoCM
      GO TO 61

 5821 CONTINUE    !FUELINIT
      DO IARY=1,9
        X(IARY) = TItoTM/ACRtoHA
      ENDDO
      GO TO 61

 5822 CONTINUE    !SNAGINIT
      X(2) = INtoCM
      X(3) = FTtoM
      X(4) = FTtoM
      X(6) = 1.0/ACRtoHA
      GO TO 61

 5834 CONTINUE    !FUELMOVE
      X(3) = TItoTM / ACRtoHA
      X(5) = TItoTM / ACRtoHA
      X(6) = TItoTM / ACRtoHA
      GO TO 61

 5839 CONTINUE    !DEFULMOD
      X(2)  = MtoFT
      X(3)  = MtoFT
      X(4)  = MtoFT
      X(5)  = MtoFT
      X(6)  = M2toFT2/KGtoLB
      X(7)  = M2toFT2/KGtoLB
      X(8)  = M2toFT2/KGtoLB
      X(9)  = M2toFT2/KGtoLB
      X(10) = FTtoM
      X(12) = MtoFT
      X(13) = M2toFT2/KGtoLB
      GO TO 61      

 5849 CONTINUE   ! FIRECALC      
      X(3)  = FTtoM
      X(4)  = FTtoM
      X(5)  = FTtoM
      X(6)  = LBtoKG/FT3toM3
      X(7)  = LBtoKG/FT3toM3
      X(8)  = BTUtoKJ/LBtoKG

   61 CONTINUE

C     FOR TYPE 1 CONVERSIONS, THE STORED (OR COMPUTED) PARAMETERS ARE METRIC
C     AND MUST BE CONVERTED TO IMPERIAL FOR THE CORE MODEL. SINCE THE FACTORS
C     STORED HERE ARE THE IMPERIAL->METRIC CONVERSIONS, WE USE THE INVERSE.
C     OTHERWISE A TYPE 2 CONVERSION (JD .EQ. 2) AND THE FACTORS ARE UNINVERTED

      DO I = 1,N_METRIC_PRMS
	  IF (JD .EQ. 1) THEN
          IF (X(I) .GT. 0.0) X(I) = 1.0 / X(I)
	  ENDIF
      ENDDO

      RETURN   
      END
