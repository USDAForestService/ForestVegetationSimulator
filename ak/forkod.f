      SUBROUTINE FORKOD
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C
C     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
C     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.
C----------
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
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
C----------
C  NATIONAL FORESTS:
C   1004 = CHUGACH 
C   1005 = TONGASS (DEFAULT)
C   1002 = TONGASS STIKINE AREA OLD CODE (MAPPED TO 1005 TONGASS)
C   1003 = TONGASS CHATHAM AREA OLD CODE (MAPPED TO 1005 TONGASS)
C  CANADIAN:
C   701 = BC/MAKAH COMBINED CODE (MAPPED TO 703 BC)
C   703 = BRITISH COLUMBIA 
C  BUREAU OF LAND MANAGEMENT
C   713 = BLM LAND 
C  STATE:
C   720 = STATE OF ALASKA, DEPARTMENT OF NATURAL RESOURCES
C         
C  TRIBAL CONSORTIUM
C   7400 = TANANA CHIEFS CONFERENCE (ANY LOCATION)
C          
C  RESERVATION PSUEDO CODES:
C   7401 = ARCTIC VILLAGE ANVSA 
C   7402 = AHTNA                
C   7403 = CHUGACH              
C   7404 = COOK INLET           
C   7405 = DOYON                
C   7406 = KONGIGANAK ANVSA     
C   7407 = NANWALEK ANVSA      
C   7408 = SELAWIK ANVSA        
C   8134 = ANNETTE ISLAND       
C   8135 = MAKAH INDIAN RES.    
C   8112 = QUILEUTE RES.        
C  
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      INTEGER I,NUMFOR
C
      INTEGER JFOR(17),KFOR(17)
C
C----------
C  DATA STATEMENTS:
C----------
C SEAPROG OLD LOCATIONS 1002 AND 1003 MAPPED TO 1005
C
      DATA JFOR/1004,1005,703,713,720,7400, 7401,7402,7403,7404,7405,
     & 7406,7407,7408,8134,8135,8112/
      DATA NUMFOR/17/,KFOR/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/
      LOGICAL FORFOUND/.FALSE./,USEIGL/.TRUE./
C
      SELECT CASE (KODFOR)
C       CROSSWALK FOR OLD TONGASS CODES 
        CASE (1002)
          WRITE(JOSTND,61)
   61     FORMAT(T12,'TONGASS: STIKINE AREA (1002) BEING ',
     &    'MAPPED TO TONGASS (1005) FOR FURTHER PROCESSING.')
          IFOR = 2
        CASE (1003)
          WRITE(JOSTND,62)
   62     FORMAT(T12,'TONGASS: CHATHAM AREA (1003) BEING ',
     &    'MAPPED TO TONGASS (1005) FOR FURTHER PROCESSING.')
          IFOR = 2
        CASE (701)
          WRITE(JOSTND,63)
   63     FORMAT(T12,'BC/MAKAH COMBINED CODE (701) BEING ',
     &    'MAPPED TO BRITISH COLUMBIA (703) FOR FURTHER PROCESSING.')
          IFOR = 3
C       END CROSSWALK FOR OLD TONGASS CODES

        CASE DEFAULT
        
C         CONFIRMS THAT KODFOR IS AN ACCEPTED FVS LOCATION CODE
C         FOR THIS VARIANT FOUND IN DATA ARRAY JFOR
          DO 10 I=1,NUMFOR
            IF (KODFOR .EQ. JFOR(I)) THEN
              IFOR = I
              FORFOUND = .TRUE.
              EXIT
            ENDIF
   10     CONTINUE
C     LOCATION CODE ERROR TRAP
         IF (.NOT. FORFOUND) THEN
           CALL ERRGRO (.TRUE.,3)
           WRITE(JOSTND,11) JFOR(IFOR)
   11      FORMAT(T12,'FOREST CODE USED IN THIS PROJECTION IS ',I4)
           USEIGL = .FALSE.
         ENDIF
     
      END SELECT

C     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
C     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
C     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
C     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
      IF (USEIGL) IGL = KFOR(IFOR)
      KODFOR=JFOR(IFOR)

C----------------------------------------------------------------------
C  SET DEFAULT TLAT, TLONG, AND ELEVATION VALUES, BY FOREST
C----------------------------------------------------------------------
      SELECT CASE(KODFOR)
        CASE(1004) 
          IF(TLAT.EQ.0)  TLAT  = 61.22
          IF(TLONG.EQ.0) TLONG = -149.88
          IF(ELEV.EQ.0)  ELEV  = 1.
        CASE(1005) 
          IF(TLAT.EQ.0)  TLAT  = 55.34
          IF(TLONG.EQ.0) TLONG = -131.64
          IF(ELEV.EQ.0)  ELEV  = 1.
        CASE(703) 
          IF(TLAT.EQ.0)  TLAT  = 48.53
          IF(TLONG.EQ.0) TLONG = -123.37
          IF(ELEV.EQ.0)  ELEV  = 1.
        CASE(713) 
          IF(TLAT.EQ.0)  TLAT  = 61.22
          IF(TLONG.EQ.0) TLONG = -149.89
          IF(ELEV.EQ.0)  ELEV  = 2.
        CASE(720) 
          IF(TLAT.EQ.0)  TLAT  = 61.22
          IF(TLONG.EQ.0) TLONG = -149.89
          IF(ELEV.EQ.0)  ELEV  = 2.
        CASE(7400) 
          IF(TLAT.EQ.0)  TLAT  = 64.84
          IF(TLONG.EQ.0) TLONG = -147.71
          IF(ELEV.EQ.0)  ELEV  = 5.
        CASE(7401) 
          IF(TLAT.EQ.0)  TLAT  = 68.13
          IF(TLONG.EQ.0) TLONG = -145.54
          IF(ELEV.EQ.0)  ELEV  = 10.
        CASE(7402) 
          IF(TLAT.EQ.0)  TLAT  = 62.14
          IF(TLONG.EQ.0) TLONG = -145.47
          IF(ELEV.EQ.0)  ELEV  = 15.
        CASE(7403) 
          IF(TLAT.EQ.0)  TLAT  = 61.19
          IF(TLONG.EQ.0) TLONG = -149.89
          IF(ELEV.EQ.0)  ELEV  = 1.
        CASE(7404) 
          IF(TLAT.EQ.0)  TLAT  = 61.21
          IF(TLONG.EQ.0) TLONG = -149.81
          IF(ELEV.EQ.0)  ELEV  = 1.
        CASE(7405) 
          IF(TLAT.EQ.0)  TLAT  = 64.85
          IF(TLONG.EQ.0) TLONG = -147.72
          IF(ELEV.EQ.0)  ELEV  = 4.
        CASE(7406) 
          IF(TLAT.EQ.0)  TLAT  = 59.99
          IF(TLONG.EQ.0) TLONG = -162.89
          IF(ELEV.EQ.0)  ELEV  = 1.
        CASE(7407) 
          IF(TLAT.EQ.0)  TLAT  = 59.36
          IF(TLONG.EQ.0) TLONG = -151.92
          IF(ELEV.EQ.0)  ELEV  = 2.
        CASE(7408) 
          IF(TLAT.EQ.0)  TLAT  = 66.73
          IF(TLONG.EQ.0) TLONG = -160.05
          IF(ELEV.EQ.0)  ELEV  = 1.
        CASE(8134) 
          IF(TLAT.EQ.0)  TLAT  = 48.00
          IF(TLONG.EQ.0) TLONG = -124.61
          IF(ELEV.EQ.0)  ELEV  = 1.
        CASE(8135) 
          IF(TLAT.EQ.0)  TLAT  = 55.12
          IF(TLONG.EQ.0) TLONG = -131.57
          IF(ELEV.EQ.0)  ELEV  = 1.
        CASE(8112) 
          IF(TLAT.EQ.0)  TLAT  = 48.37
          IF(TLONG.EQ.0) TLONG = -124.61
          IF(ELEV.EQ.0)  ELEV  = 3.
      END SELECT
      RETURN
      END
