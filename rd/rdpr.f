      SUBROUTINE RDPR
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  PRODUCES SUMMARY OUTPUT IN TABLE FORM FROM THE ROOT
C  DISEASE EXTENSION TO THE PROGNOSIS MODEL. THE SUMMARY OUTPUT
C  DETAILS INDICATORS OF THE STATE OF THE TREES IN THE ROOT DISEASE
C  AREA.  CURRENTLY, THIS OUTPUT IS *ALWAYS* PRINTED (I.E. IT IS NOT
C  KEYWORD CONTROLLED).
C
C  CALLED BY :
C     MAIN    [PROGNOSIS]
C
C  CALLS     :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C     RDDOUT  (SUBROUTINE)   [ROOT DISEASE]
C
C  PARAMETERS :
C     NONE
C
C  COMMON BLOCK VARIABLES :
C     xxxxx:   From ANCOM;
C
C
C  LOCAL VARIABLES :
C     FIRSTL:  Is first line of output yet to be printed?
C     LYAREA:  LAST YEAR'S DISEASED AREA
C
C  REVISION HISTORY:
C    18-JUN-2001 Lance R. David (FHTET)
C       Added Stand ID and Management ID line to header.
C    06-AUG-2001 Lance R. David (FHTET)
C       Initialization of LYAREA at cycle 0.
C    16-AUG-2006 Lance R. David (FHTET)
C       Change of metric conversion factors variable names to match
C       variables in new \FVS\COMMON\METRIC.F77. rd\src\metric.f77
C       will be retired. (mods courtesy of Don Robinson, ESSA)
C   08/29/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C   03/24/15 Lance R. David (FMSC)
C     Implemented General Report Writer facility.
C
C----------------------------------------------------------------------
C
C.... PARAMETER INCLUDE FILES
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
C
C.... COMMON INCLUDE FILES
C
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDCRY.F77' 
      INCLUDE 'RDADD.F77' 
      INCLUDE 'METRIC.F77'

      LOGICAL  DEBUG, FIRSTL
      INTEGER  I, I1, I2, IDI, IEND, IOAGE, IOUT, J, JYR, M,
     &         K, KSP, L
      REAL     ALLVOL, BAPA, BASTPA, CFVPA, CORE, DAREA, EXPAND,
     &         LYAREA(ITOTSP), TCLAS, TDIE, TDVOL, TIN, TOTINF,
     &         TPRBIN, TPRINF, TSTMPS, TUN
      CHARACTER*1 CHTYPE(ITOTRR)
      
      DATA CHTYPE /'P','S','A','W'/

      IF (ICYC .EQ. 0) THEN
         DO 10 I=1,ITOTRR
            LYAREA(I) = 0.0
   10    CONTINUE
      ENDIF

      FIRSTL = .TRUE.
C
C     SEE IF WE NEED TO DO SOME DEBUG.
C
      CALL DBCHK (DEBUG,'RDPR',4,ICYC)

      IF (IROOT .EQ. 0) RETURN 
      
      DO 5000 IRRSP=MINRR,MAXRR
         TDIE   = 0.0
         TDVOL  = 0.0
         TSTMPS = 0.0
         BASTPA = 0.0
         BAPA   = 0.0
         TPRINF = 0.0
         CFVPA  = 0.0
         TIN    = 0.0
         TUN    = 0.0
         TPRBIN = 0.0 
         CORE   = 0.0
         EXPAND = 0.0
         TOTINF = 0.0 
         ALLVOL = 0.0

         IF (IRDOUT .NE. 0) CALL RDDOUT

         IF (DEBUG) WRITE(JOSTND,101) ISTEP
 101     FORMAT ('IN RDPR ISTEP=',I5)

C
C        Get logical unit number and open genrpt file if it has been closed.
C
         CALL GETLUN(IRUNIT)
            
         IF (ISTEP .EQ. 1 .AND. IRRSP .EQ. MINRR) THEN
            JYR = IY(1)
            IOAGE = IAGE
C
C           get report ID.
C
            CALL GETID(IDRDOUT(1))

C
C           WRITE THE TABLE HEADERS
C
            WRITE (IRUNIT,'(2(/1X,I5))') IDRDOUT(1),IDRDOUT(1)
            WRITE (IRUNIT,1100) IDRDOUT(1),IDRDOUT(1)
            WRITE (IRUNIT,1105) IDRDOUT(1)
C
C     Write warning message RE. use of multi pest models (RNH June 98)
C
      IF (LXNOTE) THEN
      WRITE (IRUNIT,1106) IDRDOUT(1)
      LXNOTE = .FALSE.
      ENDIF
C
            WRITE (IRUNIT,1107) IDRDOUT(1)
            WRITE (IRUNIT,1110) IDRDOUT(1), NPLT, MGMID
            WRITE (IRUNIT,1115) IDRDOUT(1)
            WRITE (IRUNIT,1120) IDRDOUT(1)
            WRITE (IRUNIT,1125) IDRDOUT(1)
            WRITE (IRUNIT,1130) IDRDOUT(1)
            WRITE (IRUNIT,1135) IDRDOUT(1)
            WRITE (IRUNIT,1140) IDRDOUT(1)
            WRITE (IRUNIT,1143) IDRDOUT(1)
            WRITE (IRUNIT,1145) IDRDOUT(1)
            IF (LMTRIC) THEN
               WRITE (IRUNIT,1250) IDRDOUT(1)
               WRITE (IRUNIT,1255) IDRDOUT(1)
            ELSE
               WRITE (IRUNIT,1150) IDRDOUT(1)
               WRITE (IRUNIT,1155) IDRDOUT(1)
            ENDIF   
            WRITE (IRUNIT,1120) IDRDOUT(1)
         ELSE
            JYR = IY(ISTEP)
            IOAGE = IAGE + IY(ISTEP) - IY(1)
            IF (DEBUG) WRITE (JOSTND,102) ISTEP, ICYC, JYR
 102        FORMAT ('IN RDPR:ISTEP ICYC JYR=',3I5)
         ENDIF

         IF (PAREA(IRRSP) .LE. 0.0 .AND. LYAREA(IRRSP) .LE. 0.0) 
     &      GOTO 7300
         IF (PAREA(IRRSP) .LE. 0.0 .AND. LYAREA(IRRSP) .GT. 0.0) 
     &      GOTO 6000
        
         IEND = MAX(1,ISTEP)

         DO 702 M = 1,IEND
            DO 701 K = 1,5
               DO 700 L = 1,2
                  TSTMPS = TSTMPS + PROBDA(IRRSP,L,K,M) /
     &                     (PAREA(IRRSP) + 1.0E-9)
                  BASTPA = BASTPA + PROBDA(IRRSP,L,K,M) * (3.141593 *
     &                     (DBHDA(IRRSP,L,K,M) / 24.0)**2) /
     &                     (PAREA(IRRSP) + 1.0E-9)
  700          CONTINUE
  701       CONTINUE
  702    CONTINUE

C
C        FIND TREES IN TREE LIST
C
         IF (ITRN .LE. 0) GOTO 805
         IDI = IRRSP
         DO 800 KSP = 1,MAXSP
            IF (IRRSP .LT. 3) IDI = IDITYP(IRTSPC(KSP))
            IF ((ISCT(KSP,1) .EQ. 0) .OR. (IDI .NE. IRRSP)) GOTO 800

            I1 = ISCT(KSP,1)
            I2 = ISCT(KSP,2)

            DO 750 J = I1,I2
               I = IND1(J)
C
C              ACCUMULATE INDICATORS
C
               IF (DEBUG) WRITE (JOSTND,749) I, PAREA(IRRSP), PROBIU(I),
     &                           PROBIT(I)
  749          FORMAT ('IN RDPR :  I PAREA PROBIU PROBIT',I4,3F9.2)

               TCLAS = PROBIU(I) + PROBIT(I)
               TUN   = TUN + PROBIU(I) / (PAREA(IRRSP) + 1.0E-9)
               TIN   = TIN + PROBIT(I) / (PAREA(IRRSP) + 1.0E-9)

               TDIE  = TDIE + RDKILL(I) / (PAREA(IRRSP) + 1.0E-9)
               TDVOL = TDVOL + RDKILL(I) * CFV(I) /
     &                 (PAREA(IRRSP) + 1.0E-9)
               ALLVOL = ALLVOL + WK2(I) * CFV(I) /
     &                 (PAREA(IRRSP) + 1.0E-9)
               BAPA  = BAPA + TCLAS * (3.14159 * (DBH(I) / 24.0)**2) /
     &                 (PAREA(IRRSP) + 1.0E-9)
               CFVPA = CFVPA + TCLAS * WK1(I) / (PAREA(IRRSP) + 1.0E-9)

  750       CONTINUE
  800    CONTINUE
      
C        CHANGE NEWLY INFECTED TREES TO A PROPORTION

         IF (CORINF(IRRSP,2) .GT. 0.0) 
     &      CORE = CORINF(IRRSP,1) / CORINF(IRRSP,2)
         IF (EXPINF(IRRSP,2) .GT. 0.0)
     &      EXPAND = EXPINF(IRRSP,1) / EXPINF(IRRSP,2) 
         TOTINF = EXPINF(IRRSP,2) + CORINF(IRRSP,2)
         IF (TOTINF .GT. 0.0)
     &      TOTINF = (EXPINF(IRRSP,1) + CORINF(IRRSP,1)) / TOTINF
C
C        ACCUMULATE WEIGHTED AVE PROBABILITY OF NEW CENTER INITIATION
C
         DO 950 I = 1,2
            DO 900 J = 1,5
               IF (PROBIN(IRRSP,I,J) .GE. 0.0 .AND.
     &           PAREA(IRRSP) .GT. 0.0)
     &           TPRBIN = TPRBIN + PROBIN(IRRSP,I,J) * PROBD(IRRSP,I,J)
  900       CONTINUE
  950    CONTINUE

         TPRBIN = TPRBIN / (TSTMPS + 1.0E-9)

  805    CONTINUE
         TPRINF = PRINF(IRRSP) * 100.0

          IF (LMTRIC) THEN
            WRITE(IRUNIT,2098)
     &         IDRDOUT(1),JYR,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),
     &         PAREA(IRRSP)*ACRtoHA,RRRATE(IRRSP)*FTTOM,TSTMPS/ACRtoHA,
     &         BASTPA*FT2pACRtoM2pHA,TDIE/ACRtoHA,TDVOL*FT3pACRtoM3pHA,
     &         TUN/ACRtoHA,TIN/ACRtoHA,TPRINF,CFVPA*FT3pACRtoM3pHA,
     &         BAPA*FT2pACRtoM2pHA,CORE,EXPAND,TOTINF
C
C              Call DBS for RD Summary output to database
C
               CALL DBSRD1 (
     &         JYR,NPLT,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),
     &         PAREA(IRRSP)*ACRtoHA,RRRATE(IRRSP)*FTTOM,TSTMPS/ACRtoHA,
     &         BASTPA*FT2pACRtoM2pHA,TDIE/ACRtoHA,TDVOL*FT3pACRtoM3pHA,
     &         TUN/ACRtoHA,TIN/ACRtoHA,TPRINF,CFVPA*FT3pACRtoM3pHA,
     &         BAPA*FT2pACRtoM2pHA,CORE,EXPAND,TOTINF)
          ELSE  
            WRITE(IRUNIT,2098)
     &         IDRDOUT(1),JYR,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),
     &         PAREA(IRRSP),RRRATE(IRRSP),TSTMPS,BASTPA,TDIE,TDVOL,
     &         TUN,TIN,TPRINF,CFVPA,BAPA,CORE,EXPAND,TOTINF
C
C              Call DBS for RD Summary output to database
C
               CALL DBSRD1 (
     &         JYR,NPLT,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),
     &         PAREA(IRRSP),RRRATE(IRRSP),TSTMPS,BASTPA,TDIE,TDVOL,
     &         TUN,TIN,TPRINF,CFVPA,BAPA,CORE,EXPAND,TOTINF)
          ENDIF  
          FIRSTL = .FALSE.
          GOTO 7300

 6000    CONTINUE
 
C         OUTPUT TO PRINT IF THIS WAS THE CYCLE AFTER CENTERS DISAPPEARED

         IF (LMTRIC) THEN
            WRITE(IRUNIT,2199)
     &            IDRDOUT(1),JYR,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),
     &            PAREA(IRRSP)*ACRtoHA,RRRATE(IRRSP)*FTTOM
         ELSE
            WRITE(IRUNIT,2199)
     &            IDRDOUT(1),JYR,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),
     &            PAREA(IRRSP),RRRATE(IRRSP)
         ENDIF
         RRRATE(IRRSP) = 0.0

 7300    CONTINUE 
 
C        PRINT THE MACHINE READABLE FORM (I.E. NO TABLE HEADERS) OF THE
C        OUTPUT ON UNIT IOUT. IRGEN(7) STATES WHETHER TO PRINT THIS OUTPUT.
C        NOTE THAT THE INFORMATION IS SLIGHTLY DIFFERENT IN THIS FILE.

         DAREA = PAREA(IRRSP) - LYAREA(IRRSP)
         IOUT = IRGEN(2)
         IF (IOUT .GT. 0 .AND. IRGEN(7) .EQ. 1) THEN
          IF (LMTRIC) THEN
            WRITE(IOUT,4099) JYR,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),
     &         PAREA(IRRSP)*ACRtoHA,RRRATE(IRRSP)*FTTOM,TSTMPS/ACRtoHA,
     &         BASTPA*FT2pACRtoM2pHA,TDIE/ACRtoHA,TDVOL*FT3pACRtoM3pHA,
     &         TUN/ACRtoHA,TIN/ACRtoHA,TPRINF,
     &         DAREA*ACRtoHA,CORE,EXPAND,TOTINF
          ELSE  
            WRITE(IOUT,4099) JYR,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),
     &         PAREA(IRRSP),RRRATE(IRRSP),TSTMPS,BASTPA,TDIE,TDVOL,
     &         TUN,TIN,TPRINF,DAREA,CORE,EXPAND,TOTINF
          ENDIF
         ENDIF
      
        LYAREA(IRRSP) = PAREA(IRRSP)
        
C       NOW THAT WE'VE USED THE VARIABLES, ZERO THEM FOR FUTURE USE

        CORINF(IRRSP,1) = 0.0
        EXPINF(IRRSP,1) = 0.0
        CORINF(IRRSP,2) = 0.0
        EXPINF(IRRSP,2) = 0.0
 5000 CONTINUE
                     
      IF (.NOT. FIRSTL .AND. MINRR .NE. MAXRR)
     &  WRITE(IRUNIT,1120) IDRDOUT(1)
      RETURN

 1100 FORMAT (1X,I5,/1X,I5,1X,65('* '))
 1107 FORMAT (1X,I5,1X,65('* '))
 1105 FORMAT (1X,I5,51X,'WESTERN ROOT DISEASE MODEL')
 1110 FORMAT (1X,I5,34X,'STAND ID= ',A26,5X,'MANAGEMENT ID= ',A4)
C
C     Warning message RE. multi-pest models (RNH June 1998)
C
 1106 FORMAT (/1X,I5,1X,
     &'*=================================================',
     &'===========================*',/ 
     &'*---> Note:  The combined insect and pathogen models (in ',
     &'one executable) <---*',/
     &'*---> should NOT be used without close consultation with ',
     &'the forest''s    <---*',/ 
     &'*---> pathologist and entomologist.  Because of the ',
     &'potential for more   <---*',/
     &'*---> than one insect and/or pathogen acting on the same ',
     &'tree species,   <---*',/   
     &'*---> the interpretation of the results of the combined ',
     &'model can be     <---*',/
     &'*---> inaccurate without appropriate model knowledge and/or',
     &' experience.  <---*',/
     &'*==========================================================',
     &'==================*',/)
 
 1115 FORMAT (1X,I5,26X,'SUMMARY STATISTICS FOR ROOT DISEASE AREAS',
     &        ' (PER ACRE BASED ON DISEASED AREA ONLY)')
 1120 FORMAT (1X,I5,1X,130('-'))
 1125 FORMAT (1X,I5,17X,'DISEASE STATISTICS',
     &       9X,'DEAD TREE CHARACTERISTICS',
     &       8X,'LIVE TREE CHARACTERISTICS')
 1130 FORMAT (1X,I5,43X,'STUMPS',6X,'LOSSES FROM DISEASE')

 1135 FORMAT (1X,I5,12X,26('-'),3X,12('-'),2X,19('-'),1X,35('-'))
 1140 FORMAT (1X,I5,41X,'   ',31X,'UNINF',4X,'INFECTED',9X,'OVERALL')
 1143 FORMAT (1X,I5,56X,'NO OF',3X,'VOLUME')
 2098 FORMAT (1x,I5,1X,I4,I6,4X,A1,I4,F8.2,F8.2,4(F8.1),3X,F8.1,F8.1,
     &       F8.1,F8.0,F7.0,1X,F5.3,3X,F5.3,3X,F5.3)
 4099 FORMAT (I4,I6,4X,A1,I4,F8.2,F8.2,4(F8.1),3X,F8.1,F8.1,
     &       F8.1,F8.2,1X,F5.3,3X,F5.3,3X,F5.3)
 1145 FORMAT (1X,I5,18X,'NO',3X,'DISEASE',2X,'SPREAD',10X,'BA/',
     &       4X,'TREES',3X,'LOSSES',5X,'NO OF',3X,'NO OF',
     &       4X,'AVE',3X,'MERCH',4X,'BA/',3X,'NEWLY INFECTED')

 1150 FORMAT (1X,I5,18X,'OF',4X,'AREA',5X,'RATE',3X,'TOTAL',
     &       3X,'ACRE',4X,'KILLED',2X,'CU FT',6X,'TREES',3X,'TREES',2X,
     &       '%ROOTS',2X,'CU FT',2X,'ACRE',5X,'PROPORTION')

 1155 FORMAT (1X,I5,' YEAR',3X,'AGE',1X,'TYPE',1X,'CENTS',2X,'ACRES',
     &       3X,'FT/YR',
     &       3X,'/ACRE',2X,'SQFT',4X,'/ACRE',3X,'/ACRE',3X,
     &       2(3X,'/ACRE'),1X,'INFECTED',1X,'/ACRE',3X,'SQFT',1X,
     &       'INSIDE',2X,'EXPAND',2X,'TOTAL')
 2199 FORMAT (1X,I5,1X,I4,I6,4X,A1,I4,F8.2,F8.2)
C
C     SPECIFIC METRIC HEADERS
C
 1250 FORMAT (1X,I5,18X,'OF',4X,'AREA',5X,'RATE',3X,'TOTAL',
     &       3X,'HA  ',4X,'KILLED',2X,'CU M ',6X,'TREES',3X,'TREES',2X,
     &       '%ROOTS',2X,'CU M ',2X,' HA ',5X,'PROPORTION')

 1255 FORMAT (1X,I5,' YEAR',3X,'AGE',1X,'TYPE',1X,'CENTS',2X,' HA  ',
     &       3X,' M/YR',
     &       3X,' /HA ',2X,'SQ M',4X,' /HA ',3X,' /HA ',3X,
     &       2(3X,' /HA '),1X,'INFECTED',1X,' /HA ',3X,'SQ M',1X,
     &       'INSIDE',2X,'EXPAND',2X,'TOTAL')

      END

