      SUBROUTINE ORGTAB (JOSTND,IMODTY)
      IMPLICIT NONE
C----------
C ORGANON $Id$
C----------
C
C    WRITES TABLE OF FVS-ORGANON PARAMETER SETTINGS.
C----------
COMMONS
C
      INCLUDE 'ORGANON.F77'
C
COMMONS
C----------
C VARIABLE DECLARATIONS:
C----------
      CHARACTER VVER*7
C
      INTEGER I,J,JOSTND,IMODTY
C
C----------
C
      CALL VARVER(VVER)
C----------
C  HEADER, TREE DATA AND PARAMETER SETTING INFORMATION
C----------
      SELECT CASE (VVER(:2))
      CASE ('OC')
        WRITE(JOSTND,100)
 100    FORMAT(/,'PARAMETER SETTINGS FOR FVS-ORGANON ',
     &  'SOUTHWEST OREGON MODEL TYPE:')
      CASE DEFAULT
        IF(IMODTY .EQ. 2) THEN
          WRITE(JOSTND,110)
 110      FORMAT(/,'PARAMETER SETTINGS FOR FVS-ORGANON ',
     &    'NORTHWEST OREGON MODEL TYPE:')
        ELSE
          WRITE(JOSTND,120)
 120      FORMAT(/,'PARAMETER SETTINGS FOR FVS-ORGANON ',
     &    'STAND MANAGEMENT COOP MODEL TYPE:')
        ENDIF
      END SELECT
C
      IF(LORGPREP) THEN
        WRITE(JOSTND,600)
  600   FORMAT(/,'     TREE DATA WAS READ FROM AN ORGANON .INP FILE ',
     &  'AND WILL NOT BE EDITTED.',/,'     INITIAL PARAMETER SETTINGS ',
     &  'ARE FROM THE .INP FILE AS MODIFIED BY OTHER KEYWORDS.')
      ELSE
        WRITE(JOSTND,610)
  610   FORMAT(/,'     TREE DATA IS NOT FROM AN ORGANON .INP FILE ',
     &  'AND WILL BE EDITTED.',/,'     INITIAL PARAMETER SETTINGS ',
     &  'ARE FROM DEFAULT SETTINGS OR KEYWORDS.')
      ENDIF      
C----------
C  IMPORTANT SITE INDEX VALUES; SDI; STAND AGE
C----------
      SELECT CASE (VVER(:2))
      CASE ('OC')
        WRITE(JOSTND,200) RVARS(1),RVARS(2)                !DF, PP     
 200    FORMAT(/,'                DOUGLAS-FIR SITE INDEX: ',F6.2,/,
     &  '             PONDEROSA PINE SITE INDEX: ',F6.2)
        WRITE(JOSTND,201) RVARS(3),RVARS(4),RVARS(5)       !DF, GF/WF, PP
 201    FORMAT(/,'                   DOUGLAS-FIR MAX SDI: ',F6.2,/,
     &  '               GRAND/WHITE FIR MAX SDI: ',F6.2,/,
     &  '                PONDEROSA PINE MAX SDI: ',F6.2)
      CASE DEFAULT                                            
        WRITE(JOSTND,210) RVARS(1),RVARS(2)                !DF, WH
 210    FORMAT(/,'                DOUGLAS-FIR SITE INDEX: ',F6.2,/,
     &           '            WESTERN HEMLOCK SITE INDEX: ',F6.2)
        WRITE(JOSTND,211) RVARS(3),RVARS(4),RVARS(5)       !DF, GF, WH
 211    FORMAT(/,'                   DOUGLAS-FIR MAX SDI: ',F6.2,/,
     &           '                     GRAND FIR MAX SDI: ',F6.2,/,
     &           '               WESTERN HEMLOCK MAX SDI: ',F6.2)
      END SELECT
C----------
C  SELECTED INITIAL INDS(i) ARRAY VALUES
C----------
      IF(INDS(1) .EQ. 0)THEN
        WRITE(JOSTND,310)
  310   FORMAT(/,'                    HEIGHT CALIBRATION: NO')
      ELSE
        WRITE(JOSTND,311)
  311   FORMAT(/,'                    HEIGHT CALIBRATION: YES')
      ENDIF
C
      IF(INDS(2) .EQ. 0)THEN
        WRITE(JOSTND,320)
  320   FORMAT(1X,'     HEIGHT-TO-CROWN BASE CALIBRATION: NO')
      ELSE
        WRITE(JOSTND,321)
  321   FORMAT(1X,'     HEIGHT-TO-CROWN BASE CALIBRATION: YES')
      ENDIF
C
      IF(INDS(3) .EQ. 0)THEN
        WRITE(JOSTND,330)
  330   FORMAT(1X,'          DIAMETER GROWTH CALIBRATION: NO')
      ELSE
        WRITE(JOSTND,331)
  331   FORMAT(1X,'          DIAMETER GROWTH CALIBRATION: YES')
      ENDIF
C
      IF(INDS(4) .EQ. 0)THEN
        WRITE(JOSTND,340)
  340   FORMAT(1X,'                      EVEN-AGED STAND: NO')
      ELSE
        WRITE(JOSTND,341)
  341   FORMAT(1X,'                      EVEN-AGED STAND: YES')
      ENDIF
      IF(ITEST .EQ. 1) THEN
          WRITE(JOSTND,342)
  342     FORMAT(/'*NOTE -- STAND WAS DECLARED EVEN-AGED, BUT STAND ',
     &    'AGE WAS NOT ENTERED.',/,'STAND INDICATOR WAS RESET TO ',
     &    'UNEVEN-AGED SO ORGANON MODEL WILL RUN.',/,'HOWEVER, IF THIS',
     &    ' IS A BARE-GROUND PLANTING SCENARIO, THE STAND INDICATOR',
     &    ' WILL BE SET BACK TO EVEN-AGED.',/)
      ENDIF
C
      IF(INDS(5) .EQ. 0)THEN
        WRITE(JOSTND,350)
  350   FORMAT(1X,'                             TRIPLING: NO')
      ELSE
        WRITE(JOSTND,351)
  351   FORMAT(1X,'                             TRIPLING: YES')
      ENDIF
C
      IF(INDS(9) .EQ. 0)THEN
        WRITE(JOSTND,390)
  390   FORMAT(1X,'     MAX SDI FOR ADDITIONAL MORTALITY: NO')
      ELSE
        WRITE(JOSTND,391)
  391   FORMAT(1X,'     MAX SDI FOR ADDITIONAL MORTALITY: YES')
      ENDIF
C----------
C  INITIAL ORGANON CALIBRATION VALUES
C----------
      SELECT CASE (VVER(:2))
      CASE ('OC')
        WRITE(JOSTND,400) ((ACALIB(I,J),I=1,3),J=1,18)
 400    FORMAT(/,'     STARTING CALIBRATION RATIOS:',/,
     &  5X,'       SPECIES GROUP   HT/DBH    HTCB    DIAM',/,
     &  5X,'         DOUGLAS FIR:',3F8.2,/,
     &  5X,'     WHITE/GRAND FIR:',3F8.2,/,
     &  5X,'      PONDEROSA PINE:',3F8.2,/,
     &  5X,'          SUGAR PINE:',3F8.2,/,
     &  5X,'       INCENSE-CEDAR:',3F8.2,/,
     &  5X,'     WESTERN HEMLOCK:',3F8.2,/,
     &  5X,'    WESTERN REDCEDAR:',3F8.2,/,
     &  5X,'         PACIFIC YEW:',3F8.2,/,
     &  5X,'     PACIFIC MADRONE:',3F8.2,/,
     &  5X,'    GIANT CHINQUAPIN:',3F8.2,/,
     &  5X,'              TANOAK:',3F8.2,/,
     &  5X,'     CANYON LIVE OAK:',3F8.2,/,
     &  5X,'       BIGLEAF MAPLE:',3F8.2,/,
     &  5X,'    OREGON WHITE OAK:',3F8.2,/,
     &  5X,'CALIFORNIA BLACK OAK:',3F8.2,/,
     &  5X,'           RED ALDER:',3F8.2,/,
     &  5X,'     PACIFIC DOGWOOD:',3F8.2,/,
     &  5X,'      WILLOW SPECIES:',3F8.2)
      CASE DEFAULT
        WRITE(JOSTND,410) ((ACALIB(I,J),I=1,3),J=1,11)
 410    FORMAT(/,'        STARTING CALIBRATION RATIOS:',/,
     &  5X,'                 SPECIES GROUP   HT/DBH    HTCB    DIAM',/,
     &  5X,'                   DOUGLAS FIR:',3F8.2,/,
     &  5X,'                     GRAND FIR:',3F8.2,/,
     &  5X,'               WESTERN HEMLOCK:',3F8.2,/,
     &  5X,'              WESTERN REDCEDAR:',3F8.2,/,
     &  5X,'                   PACIFIC YEW:',3F8.2,/,
     &  5X,' WHITE ALDER / PACIFIC MADRONE:',3F8.2,/,
     &  5X,'                 BIGLEAF MAPLE:',3F8.2,/,
     &  5X,'  OREGON WHITE / CAL BLACK OAK:',3F8.2,/,
     &  5X,'                     RED ALDER:',3F8.2,/,
     &  5X,'               PACIFIC DOGWOOD:',3F8.2,/,
     &  5X,'                WILLOW SPECIES:',3F8.2)
      END SELECT
C----------
C  VOLUME SPECIFICATIONS
C----------
C  ORGANON USER'S GUIDE SHOW THESE AS DEFAULTS: 
C    CFTD=0.0
C    CFSH=0.0
C    LOGTD=6.0
C    LOGSH=0.5
C    LOGTA=8.0
C    LOGLL=32.0
C    LOGML=8.0
C  JEFF HAS THESE SET AT:
C  VOLS     VOLS     VOLS       VOLS      GRINIT GRINIT GRINIT
C  TOPD(i), STMP(i), BFTOPD(i), BFSTMP(i), 10.0,  32.0,  12.0 
C  BUT, FOR EXAMPLE, TOPD(i)=0.0 IN **GRINIT**, THEN = 4.5 OR 6.0 IN **SITSET**
C  SO THESE NEED TO BE EXAMINED AND STRAIGHTENED OUT
C
C      IF (LKECHO .AND. LORGVOLS ) THEN
C        WRITE(JOSTND,311) KEYWRD, LOGTA,LOGML,LOGLL
C  311   FORMAT (/1X,A8,'   ORGANON ',
C     >  'LOG TRIM IS ', F4.1, ' INCHES', 
C     >  '; MINIMUM LOG LENGTH IS ', F4.1, ' FEET', 
C     >  '; TARGET LOG LENGTH IS ', F4.1, ' FEET;' )
C
C        WRITE(JOSTND,312) ARRAY(4), ARRAY(5)
C  312   FORMAT ('           ',
C     >  ' BF STUMP HEIGHT IS ', F4.1, ' FEET;',
C     >  ' BF MIN TOP DIB IS ', F4.1, ' INCHES' )
C
C        WRITE(JOSTND,313) ARRAY(6), ARRAY(7)
C  313   FORMAT ('           ',
C     >  ' CF STUMP HEIGHT IS ', F4.1, ' FEET;',
C     >  ' CF MIN TOP DIB IS ', F4.1, ' INCHES' )
C
C
      RETURN
      END

