      SUBROUTINE ORGTAB
      IMPLICIT NONE
C----------
C ORGANON $Id$
C----------
C
C    WRITES TABLE OF FVS-ORGANON PARAMETER SETTINGS.
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ORGANON.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'VOLSTD.F77'
C
COMMONS
C----------
C VARIABLE DECLARATIONS:
C----------
      CHARACTER VVER*7
C
      INTEGER I,J
C
C----------
C
      CALL VARVER(VVER)
C----------
C     WRITE THE START DELIMITER TO THE ORGANON TABLE
C----------
      WRITE(JOSTND, 20)
   20 FORMAT (130('-'))
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
          WRITE(JOSTND,101)
 101      FORMAT(/,'PARAMETER SETTINGS FOR FVS-ORGANON ',
     &    'NORTHWEST OREGON MODEL TYPE:')
        ELSE
          WRITE(JOSTND,102)
 102      FORMAT(/,'PARAMETER SETTINGS FOR FVS-ORGANON ',
     &    'STAND MANAGEMENT COOP MODEL TYPE:')
        ENDIF
      END SELECT
C----------
C WRITE ORGANON TYPE HEADINGS:
C----------
      WRITE(JOSTND,120)
  120 FORMAT(/T30,'O R G A N O N',
     &/T15,'ORegon Growth ANalysis and projectiON system',
     &/T25,'Growth & Yield Model for')
      SELECT CASE (VVER(:2))
      CASE ('OC')
        WRITE(JOSTND,125)
  125   FORMAT(T17,'Southwest Oregon Mixed Conifer Forests',
     &  //T22,'SW OREGON VERSION, EDITION 9.1')
      CASE DEFAULT
        IF(IMODTY .EQ. 2) THEN
          WRITE(JOSTND,126)
  126     FORMAT(T24,'Northwest Oregon Forests',
     &    //T22,'NW OREGON VERSION, EDITION 9.1')
        ELSE
          WRITE(JOSTND,127)
  127     FORMAT(T20,'Stand Management Coop Conifer Forests',
     &    //T19,'STAND MANAGEMENT COOP VERSION, EDITION 9.1')
        ENDIF
      END SELECT
      SELECT CASE (VVER(:2))
      CASE ('OC')
          WRITE(JOSTND,130)
  130     FORMAT(T28,'by David W. Hann',
     &/T27,'and Mark Hanus',
     &/T28,'College of Forestry',
     &/T25,'Oregon State University',
     &//T24,'This model was funded by:',
     &/T11,'Forestry Intensified Research (FIR) and the USDI BLM')
      CASE DEFAULT
        IF(IMODTY .EQ. 2) THEN
          WRITE(JOSTND,131)
  131     FORMAT(T28,'by David W. Hann',
     &/T12,'Martin Ritchie, Chao-Huan Wang, and Abdel Azim Zumrawi',
     &/T28,'College of Forestry',
     &/T25,'Oregon State University',
     &//T24,'This model was funded by the',
     &/T18,'College of Forestry Research Forests')
        ELSE
          WRITE(JOSTND,132)
  132     FORMAT(T28,'by David W. Hann',
     &/T18,'David D. Marshall and Mark L. Hanus',
     &/T28,'College of Forestry',
     &/T25,'Oregon State University',
     &//T24,'This model was funded by:',
     &/T18,'The Stand Management Cooperative (SMC)')
        ENDIF
      END SELECT
C----------
C  IMPORTANT SITE INDEX VALUES; SDI; STAND AGE
C----------
      SELECT CASE (VVER(:2))
      CASE ('OC')
        WRITE(JOSTND,200) RVARS(1),RVARS(2)                !DF, PP     
 200    FORMAT(//,'                Douglas-fir Site Index: ',F6.2,/,
     &  '             Ponderosa Pine Site Index: ',F6.2)
        WRITE(JOSTND,201) RVARS(3),RVARS(4),RVARS(5)       !DF, GF/WF, PP
 201    FORMAT(/,'                   Douglas-fir Max SDI: ',F6.2,/,
     &  '               Grand/White Fir Max SDI: ',F6.2,/,
     &  '                Ponderosa Pine Max SDI: ',F6.2)
      CASE DEFAULT                                            
        WRITE(JOSTND,210) RVARS(1),RVARS(2)                !DF, WH
 210    FORMAT(//,'                Douglas-fir Site Index: ',F6.2,/,
     &           '            Western Hemlock Site Index: ',F6.2)
        WRITE(JOSTND,211) RVARS(3),RVARS(4),RVARS(5)       !DF, GF, WH
 211    FORMAT(/,'                   Douglas-fir Max SDI: ',F6.2,/,
     &           '                     Grand Fir Max SDI: ',F6.2,/,
     &           '               Western Hemlock Max SDI: ',F6.2)
      END SELECT
C----------
C  SELECTED INITIAL INDS(i) ARRAY VALUES
C----------
      IF(INDS(1) .EQ. 0)THEN
        WRITE(JOSTND,310)
  310   FORMAT(/,'                    HEIGHT CALIBRATION: No')
      ELSE
        WRITE(JOSTND,311)
  311   FORMAT(/,'                    HEIGHT CALIBRATION: Yes')
      ENDIF
C
      IF(INDS(2) .EQ. 0)THEN
        WRITE(JOSTND,320)
  320   FORMAT(1X,'     HEIGHT-TO-CROWN BASE CALIBRATION: No')
      ELSE
        WRITE(JOSTND,321)
  321   FORMAT(1X,'     HEIGHT-TO-CROWN BASE CALIBRATION: Yes')
      ENDIF
C
      IF(INDS(3) .EQ. 0)THEN
        WRITE(JOSTND,330)
  330   FORMAT(1X,'          DIAMETER GROWTH CALIBRATION: No')
      ELSE
        WRITE(JOSTND,331)
  331   FORMAT(1X,'          DIAMETER GROWTH CALIBRATION: Yes')
      ENDIF
C
      IF(INDS(4) .EQ. 0)THEN
        WRITE(JOSTND,340)
  340   FORMAT(1X,'                      EVEN-AGED STAND: No')
      ELSE
        WRITE(JOSTND,341)
  341   FORMAT(1X,'                      EVEN-AGED STAND: Yes')
      ENDIF
      IF(ITEST .EQ. 1) THEN
          WRITE(JOSTND,342)
  342     FORMAT(/'*NOTE -- Stand was declared even-aged, but stand ',
     &    'age was not entered.',/,'Stand indicator was reset to ',
     &    'uneven-aged so ORGANON model will run.',/,'However, if this',
     &    ' is a bare-ground planting scenario, the stand indicator',
     &    ' will be set back to even-aged.',/)
      ENDIF
C
      IF(INDS(5) .EQ. 0)THEN
        WRITE(JOSTND,350)
  350   FORMAT(1X,'                             TRIPLING: No')
      ELSE
        WRITE(JOSTND,351)
  351   FORMAT(1X,'                             TRIPLING: Yes')
      ENDIF
C
      IF(INDS(9) .EQ. 0)THEN
        WRITE(JOSTND,360)
  360   FORMAT(1X,'     MAX SDI FOR ADDITIONAL MORTALITY: No')
      ELSE
        WRITE(JOSTND,361)
  361   FORMAT(1X,'     MAX SDI FOR ADDITIONAL MORTALITY: Yes')
      ENDIF
C----------
C  INITIAL ORGANON CALIBRATION VALUES
C----------
      SELECT CASE (VVER(:2))
      CASE ('OC')
        WRITE(JOSTND,400) ((ACALIB(I,J),I=1,3),J=1,18)
 400    FORMAT(/T3,'CALIBRATION RATIOS USED FOR THIS RUN:',
     &  /T42,'HT/DBH    HTCB    DIAM',
     &  /T28,'DOUGLAS FIR:',3F8.2,
     &  /T24,'WHITE/GRAND FIR:',3F8.2,
     &  /T25,'PONDEROSA PINE:',3F8.2,
     &  /T29,'SUGAR PINE:',3F8.2,
     &  /T26,'INCENSE-CEDAR:',3F8.2,
     &  /T24,'WESTERN HEMLOCK:',3F8.2,
     &  /T23,'WESTERN REDCEDAR:',3F8.2,
     &  /T28,'PACIFIC YEW:',3F8.2,
     &  /T24,'PACIFIC MADRONE:',3F8.2,
     &  /T23,'GIANT CHINQUAPIN:',3F8.2,
     &  /T33,'TANOAK:',3F8.2,
     &  /T24,'CANYON LIVE OAK:',3F8.2,
     &  /T26,'BIGLEAF MAPLE:',3F8.2,
     &  /T23,'OREGON WHITE OAK:',3F8.2,
     &  /T19,'CALIFORNIA BLACK OAK:',3F8.2,
     &  /T30,'RED ALDER:',3F8.2,
     &  /T24,'PACIFIC DOGWOOD:',3F8.2,
     &  /T25,'WILLOW SPECIES:',3F8.2)
      CASE DEFAULT
        WRITE(JOSTND,410) ((ACALIB(I,J),I=1,3),J=1,11)
 410    FORMAT(/T3,'CALIBRATION RATIOS USED FOR THIS RUN:',
     &  /T42,'HT/DBH    HTCB    DIAM',
     &  /T28,'DOUGLAS FIR:',3F8.2,
     &  /T30,'GRAND FIR:',3F8.2,
     &  /T24,'WESTERN HEMLOCK:',3F8.2,
     &  /T23,'WESTERN REDCEDAR:',3F8.2,
     &  /T28,'PACIFIC YEW:',3F8.2,
     &  /T10,'WHITE ALDER / PACIFIC MADRONE:',3F8.2,
     &  /T26,'BIGLEAF MAPLE:',3F8.2,
     &  /T11,'OREGON WHITE / CAL BLACK OAK:',3F8.2,
     &  /T30,'RED ALDER:',3F8.2,
     &  /T24,'PACIFIC DOGWOOD:',3F8.2,
     &  /T25,'WILLOW SPECIES:',3F8.2)
      END SELECT
C----------
C  VOLUME SPECIFICATIONS
C  LORGVOLS .TRUE. OSU VOLUME SPECIFICATIONS
C  LORGVOLS .FALSE. NVEL VOLUME SPECIFICATIONS
C----------
      IF(LORGVOLS) THEN
C----------
C ORGANON OSU CUBIC FOOT
C----------
        WRITE(JOSTND,500)
  500   FORMAT(/T5,'VOLUME SPECIFICATIONS FOR THIS RUN:',
     &  /T23,'VOLUME EQUATIONS: ORGANON OSU')
        WRITE(JOSTND,501) CFTD,CFSH,CFTDHW
  501   FORMAT(/T19,'SOFTWOODS CF TOP DIB:',F5.1,' INCHES',
     &  /T14,'SOFTWOODS CF STUMP HEIGHT:',F5.1,' FEET',
     &  /T19,'HARDWOODS CF TOP DIB:',F5.1,' INCHES')
C----------
C ORGANON OSU BOARD FOOT
C----------
        WRITE(JOSTND,502) LOGTD,LOGTA,LOGSH,LOGLL,LOGML
  502   FORMAT(/T29,'BF TOP DIB:',F5.1,' INCHES',
     &  /T21,'LOG TRIM ALLOWANCE:',F5.1,' INCHES',
     &  /T28,'BF STUMP HT:',F5.1,' FEET',
     &  /T22,'TARGET LOG LENGTH:',F5.1,' FEET',
     &  /T21,'MINIMUM LOG LENGTH:',F5.1,' FEET')
C----------
C FVS NVEL VOLUME SPECIFICATIONS, BY SPECIES
C----------
      ELSE
        WRITE(JOSTND,510)
  510   FORMAT(/T5,'VOLUME SPECIFICATIONS FOR THIS RUN:',/T23,
     &  'VOLUME EQUATIONS: FVS NATIONAL VOLUME ESTIMATOR LIBRARY')
        WRITE(JOSTND,511)
  511   FORMAT(/T42,'----- CUBIC FEET -----  ----- BOARD FEET -----',
     &  /T32,'SPECIES    MIN       TOP   STUMP     MIN     TOP   STUMP',
     &  /T35,'CODE    DBH       DIB      HT     DBH     DIB      HT')
        DO I=1,MAXSP
          WRITE(JOSTND,512)JSP(I),DBHMIN(I),TOPD(I),STMP(I),BFMIND(I),
     &    BFTOPD(I),BFSTMP(I)
  512     FORMAT(T37,A2,':',F6.1,2X,5F8.1)
        ENDDO
      ENDIF
C----------
C     WRITE THE END DELIMITER TO THE ORGANON TABLE
C----------
      WRITE(JOSTND, 20)
      RETURN
      END

