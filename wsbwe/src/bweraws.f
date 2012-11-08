      SUBROUTINE BWERAWS
      IMPLICIT NONE
C-----------
C **BWERAWS                  DATE OF LAST REVISION:  01/10/12
C-----------
C Part of the General Defoliator (GenDefol) model
C
C Processes RAWS daily climate data and loads the necessary
C weather parameters for the General Defoliator model.
C
C   PARAMETERS:
C
C
C Weather data is available from Western Region Climate Center (WRCC)
C and the format used in this subroutine is the FWX format.
C Weather station data is provided in two files, one of metadata and
C one of daily weather data. Examples of the metadata and daily weather
C records of this format follows.
C
C Weather station metadata file (ex: 20877-METADATA.txt)
C+--------------+-----------+-------+-------+-------+------+--------+-------+---------+------+
C| Station Name |   FPU ID  | Ver.  | Start |  End  | WRCC |    ID  |  Lat. |  Lon.   | Elev |
C+--------------+-----------+-------+-------+-------+------+--------+-------+---------+------+
C|Elk Creek     | NW_OR_009 | Dec05 | 07-00 | 12-04 | OECK | 352126 | 44.76 | -117.97 | 6576 |
C+--------------+-----------+-------+-------+-------+------+--------+-------+---------+------+
C
C Daily weather data file (ex: OECK-unvalidated-vDec05.fwx)
C Column Name
C   1-6  Station Number (4-character code assigned by WRCC)
C   7-8  Year
C   9-10 Month
C  11-12 Day
C  13    State of Weather Code
C  14-16 Dry Bulb Temp (F)
C  17-19 Relative Humidity (%)
C  28    Wind Direction (8 point)
C  29-31 Wind Speed (MPH) 
C  39-41 Max Temperature (F)
C  42-44 Min Temperature (F)
C  45-47 Max RH (%)
C  48-50 Min RH (%)
C  51    Season Code
C  52-53 Precipitation Duration (Hrs)
C  54-57 Precipitation Amount (IN 100ths))
C  58-60 Lightning Activity Level
C  61    Relative Humidity Variable Indicator (1=Wet Bulb, 2=RH%, 3=dew point)
C
C                                       Temp F RH(%) Precip
C StanumYYMMDD Tmp RH        WSpd       MaxMinMaxMin HHIIII
C   oeck0007100 68 37        6  3        69 44 63 333 0   0   2
C   oeck0007110 75 30        8  2        75 49 64 303 0   0   2
C   oeck0007128 80 34        7  2        80 57 49 303 0   0   2
C   oeck0007130 80 21        5  3        80 57 48 203 0   0   2
C   oeck0007140 74 27        7  3        84 50 54 163 0   0   2
C   oeck0007150 65 39        7  1        75 45 79 223 0   0   2
C   oeck0007163 73 27        7  3        74 51 44  83 0   0   2
C   oeck0007172 70 32        5  4        74 58 37 253 0   0   2
C   oeck0007188 64 47        8  2        70 54 47 313 0   0   2
C   oeck0007198 71 40        2  3        74 52 75 323 2   3   2
C   oeck0007201 77 27        5  4        77 52 73 273 3  13   2
C
C Weather data is processed one year at a time with the period
C beginning Jan 1 and ending Dec 31. Climate values relevant to
C Specific budworm life stage periods is accumulated and processed.
C Dates associated with specific events are specified and captured
C using Julian date instead of month and day.
C
C Degree days for larvae are based on 42 deg F (5.5 Deg C). Degree
C days for bud flushing is based on 40 deg F (4.4 Deg C). Warm
C degree days that after adult flight in the fall is based on 75
C deg F (23.9 deg C).
C
C Budworm life stage requirements (5.5 oC deg-days):
c peak emerg: 100, L2: 102, L3: 60, L4: 85, L5: 90, L6: 141,
c adult: 112; phen. assumptions from Sheehan et al 1989 except
c adults from Thomson et al. 1983.
C
C Julian dates of interest:
C     1 - Jan 1, beginning of annual period
C    79 - March 20, start accumulating degree days for bud flush
C    ?? - Emergence of L2, begin small larvae period (100 DD)
C    ?? - Mid L4, begin large larvae period (305 DD)
C    ?? - Begin pupal stage (578 DD)
C    ?? - Adult eclosion (emergence) (764 DD)
C    ?? - (Date of 764 DD) + 4, begin accumulating warm DD at 75 F
C
C
C  Lance R. David, USDA-FS, FMSC, Fort Collins, CO (METI, Inc)
C
C   CALLED FROM: BWELIT
C
C   SUBROUTINES AND FUNCTIONS CALLED:
C
C      BWENOR - CALC. AND SCALE A NORMALLY DISTRIB. DEVIATE
C      BWEMUL - SCALE A RANDOM NUMBER TO GET A MULTIPLIER
C
C   PARAMETERS:
C
C   BWEATH - ARRAY THAT STORES WEATHER PARAMETERS FOR 1 STATION
C            (X,1)=MEAN,(X,2)=S.D., (X,3)=MINIMUM, (X,4)=MAX. FOR X=
C            1=NO. OF DAYS FROM L2 EMERGENCE TO MID-L4
C            2=NO. OF DAYS FROM MID-L4 THROUGH PUPAE
C            3=NO. OF DAYS AS PUPAE
C            4=WARM DEGREE-DAYS AFTER ADULT FLIGHT IN FALL
C            5=NO. OF DAYS FROM L2 EMERGENCE TO BUD-FLUSHING
C            6=TREE DEGREE-DAYS ACCUMULATED AT MID-L4
C            7=MEAN PPT. FOR SMALL LARVAE
C            8=MEAN PPT. FOR LARGE LARVAE
C            9=MEAN PPT. FOR PUPAE
C           10=MEAN PPT. DURING L2 EMERGENCE
C
C  WEATH - Holds one year of weather data (3,365) (TMAX, TMIN, PRECIP)
C          
C  OBYRC - Outbreak year current, year of weather data to be processed.
C   JDAY - Julian day used to cycle through 1 year of weather data.
C DDAYSF - Degree days for foliage
C DDAYSL - Degree days for larvae (budworm)
C
C
C Common files
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'BWECM2.F77'
      INCLUDE 'BWEBOX.F77'
      INCLUDE 'BWECOM.F77'

C
C Define variables
      CHARACTER CTMP*1, WSNAME*14, FPUID*9, WVER*5, WRCCID*4,
     &          WSTAID*6, STANUM*4
      INTEGER I, I2, I3, JDAY, KODE, WSTMM, WSTYY, WENDMM, WENDYY,
     &        WELEV, WYR, WMON, WDAY, DAYL2, DAYL4, DAYL7, DAYL8,
     &        DAYF
      REAL DDAYS, DDAYSF, DDAYSL, MAXTC, MAXTF, MINTC, MINTF,
     &     WLAT, WLONG, PRECIP, WEATH(3,365),
     &     PPTL2E, PRDL24, PPTL24, PRDL46, PPTL46, PRDL7, PPTL7,
     &     DDFALL, PRDE2B
     
C Load static variables



C initialize variables
      JDAY   = 0
      IYRCNT = 0
C
C Open weather file.  
C
      CALL MYOPEN (JOWE,WFNAME,3,133,1,1,1,0,KODE)

C Check if Open was successful
C
      IF (KODE.EQ.1) WRITE(JOSTND,11)
   11 FORMAT (/T13,'**********   OPEN FAILED   **********')

C Read weather file
C First read the header record and store info. Note that when FPA RAWS
C data is downloaded, there is a metadata file and the .fwx file. It is
C expected here that the user has inserted the records from the metadata
C file as the first 5 records in the .fwx file. Following is a sample.
C
C+--------------+-----------+-------+-------+-------+------+--------+-------+---------+------+
C| Station Name |   FPU ID  | Ver.  | Start |  End  | WRCC |    ID  |  Lat. |  Lon.   | Elev |
C+--------------+-----------+-------+-------+-------+------+--------+-------+---------+------+
C|Elk Creek     | NW_OR_009 | Dec05 | 07-00 | 12-04 | OECK | 352126 | 44.76 | -117.97 | 6576 |
C+--------------+-----------+-------+-------+-------+------+--------+-------+---------+------+
C  oeck0007100 68 37        6  3        69 44 63 333 0   0   2
C  oeck0007110 75 30        8  2        75 49 64 303 0   0   2
C  oeck0007128 80 34        7  2        80 57 49 303 0   0   2
C
C Then advance, if necessary, to January 1 to beging processing one
C year of data.
      
      READ (JOWE,10) CTMP
      READ (JOWE,10) CTMP
      READ (JOWE,10) CTMP
   10 FORMAT (A1)
      READ (JOWE,20,END=400) WSNAME,FPUID,WVER,WSTMM,WSTYY,WENDMM,
     &                       WENDYY,WRCCID,WSTAID,WLAT,WLONG,WELEV
   20 FORMAT(1X,A14,2X,A9,3X,A5,3X,I2,1X,I2,3X,I2,
     &       1X,I2,3X,A4,3X,A6,3X,F5.2,3X,F7.2,2X,I6)
      READ (JOWE,10) CTMP
   25 READ (JOWE,30,END=400) STANUM,WYR,WMON,WDAY,MAXTF,MINTF,PRECIP
   30 FORMAT (2X,A4,3(I2),26X,2(F3.0),9X,F4.0)

C     Translate station name to upper case   
      DO 40 I=1,4
        CALL UPCASE(STANUM(I:I))
   40 CONTINUE

      IF (STANUM .NE. WRCCID) THEN
C       Header record WRCC ID does not match station ID for daily data.
C       Write error message and exit
        WRITE (*,*)
     &  "Weather data header record WRCC ID does not match data."
C       Need to set error flag
        GOTO 450
      ENDIF

C     IF (WYR .EQ. OBYRC .AND. WMON .EQ. 1 .AND. WDAY .EQ. 1) THEN
C       at jan 1 of current outbreak year, start processing year
      IF (WMON .EQ. 1 .AND. WDAY .EQ. 1) THEN
C
C       At jan 1, start processing year
C       Set record counter (JDAY) to 1 and initialize variables used
C       in the annual weather processing.
C
        JDAY   = 1
        DAYF   = 0
        DDAYSF = 0.0
        DDAYSL = 0.0
        DAYL2  = 0
        DAYL4  = 0
        DAYL7  = 0
        DAYL8  = 0
        TREEDD = 0.0

        GOTO 50
      ELSE
C       read next record
        GOTO 25
      ENDIF

C     Loop to process 1 year of data.
C     Temperature data must be converted from Fahrenheit to Celsius.
C     **** (If a lapse rate is to be applied, it would be done ****
C     **** here standard lapse rate = 3.46 oF / 1000 ft)       ****
C     Save data in array for final processing based on the degree day
C     targets identified, then read next record.
C     Accumulate degree days for:
C        budworm life cycle phases based on 42 Deg F (5.5 Deg C)
C        trees up to budworm mid-L4 based on 42 Deg F (5.5 Deg C)
C        bud flush based on 40 Deg F (4.4 Deg C)
C        adult budworm after flight based on 75 Deg F (23.9 Deg C)
C     PRECIPitation is read in 100ths of inches and must be converted.
C
   50 CONTINUE
      MAXTC = (5.0 / 9.0) * (MAXTF - 32.0)
      MINTC = (5.0 / 9.0) * (MINTF - 32.0)
      WEATH(1,JDAY) = MAXTC
      WEATH(2,JDAY) = MINTC
      WEATH(3,JDAY) = PRECIP/100.0

      DDAYS =((MAXTC + MINTC)/2.0) - 5.5

      IF (DDAYS .GT. 0.0) THEN
        DDAYSL = DDAYSL + DDAYS
C       Accumulate tree degree days up to mid-L4
        IF (DAYL4 .EQ. 0.0) TREEDD = TREEDD + DDAYS
      ELSE
C       No degree days at 5.5 C skip to bud flush at 4.4 C
        GOTO 70
      ENDIF

      IF (DDAYSL .GE. 100.0 .AND. DAYL2 .EQ. 0) DAYL2 = JDAY
      IF (DDAYSL .GE. 305.0 .AND. DAYL4 .EQ. 0) DAYL4 = JDAY
      IF (DDAYSL .GE. 578.0 .AND. DAYL7 .EQ. 0) DAYL7 = JDAY
      IF (DDAYSL .GE. 764.0 .AND. DAYL8 .EQ. 0) DAYL8 = JDAY

   70 CONTINUE

      IF (JDAY .GE. 79 .AND. DAYF .EQ. 0) THEN
C       Accumulate DDays after March 20 for foliage (bud flush)
C       Target number of DDays is 220.
        DDAYS =((MAXTC + MINTC)/2.0) - 4.4
        IF (DDAYS .GT. 0.0) DDAYSF = DDAYSF + DDAYS
        IF (DDAYSF .GE. 220.0) DAYF = JDAY
      ENDIF

      IF (JDAY .EQ. 365) THEN
C       Have completed reading and processing 1 year of weather data.
C       Now calculate values that are based on the days identified above.
C
C       Accumulate precipitation around L2 emergence
C
        PPTL2E = 0.0
        I2 = DAYL2 - 5
        I3 = DAYL2 + 5
        DO I = I2,I3
          PPTL2E = PPTL2E + WEATH(3,I)
        END DO

C       Accumulate precipitation L2 to mid-L4 (small larvae)
C       Calc period length.
        PRDL24 = DAYL4 - DAYL2
        PPTL24 = 0.0
        DO I = DAYL2,DAYL4
          PPTL24 = PPTL24 + WEATH(3,I)
        END DO

C       Accumulate precipitation mid-L4 to L7 (large larvae)
C       Calc period length.
        PRDL46 = DAYL7 - DAYL4
        PPTL46 = 0.0
        DO I = DAYL4,DAYL7
          PPTL46 = PPTL46 + WEATH(3,I)
        END DO

C       Accumulate precipitation during L7 (pupal period)
C       Calc period length.
        PRDL7 = DAYL8 - DAYL7
        PPTL7 = 0.0
        DO I = DAYL7,DAYL8
          PPTL7 = PPTL7 + WEATH(3,I)
        END DO

C       Accummulate warm degree days 75 Deg F (23.9 Deg C) after flight.
        I2 = DAYL8 + 4
        DDFALL = 0.0
        DO I = I2, 365
          DDAYS=((WEATH(1,I) + WEATH(2,I))/2.0) - 23.9
          if (DDAYS .GT. 0.0) DDFALL = DDFALL + DDAYS
        END DO

C       Length of period from L2 emergence to bud flush.
        PRDE2B = DAYF - DAYL2
        GOTO 300
      ELSE
C       Read the next weather record. Should not hit the end of file,
C       if so, it is an incomplete year.
        READ (JOWE,30,END=400) STANUM,WYR,WMON,WDAY,MAXTF,MINTF,PRECIP
        JDAY = JDAY + 1
        GOTO 50
      ENDIF


C     Save budworm variables for year just processed.
C     Translate weather year from 2-digit to 4-digit.
C
  300 IYRCNT = IYRCNT + 1
  
      IF (WYR .LE. 29) THEN
        WYR = 2000 + WYR
      ELSE
        WYR = 1900 + WYR
      ENDIF

      WRITE (JOSTND, 310) WYR, DAYL2, DAYL4, DAYL7, DAYL8, 
     &       DAYF, DDAYSF, DDAYSL
  310 FORMAT (/,6X,"WYR=",I4," DAYL2=",I3," DAYL4=",I3, " DAYL7=",I3, 
     &     " DAYL8=",I3, " DAYF=",I3," DDAYSF=",F6.1," DDAYSL=",F6.1)

      BWPRMS(1,IYRCNT) = PRDL24
      BWPRMS(2,IYRCNT) = PRDL46
      BWPRMS(3,IYRCNT) = PRDL7
      BWPRMS(4,IYRCNT) = DDFALL
      BWPRMS(5,IYRCNT) = PRDE2B
      BWPRMS(6,IYRCNT) = TREEDD
      BWPRMS(7,IYRCNT) = PPTL24
      BWPRMS(8,IYRCNT) = PPTL46
      BWPRMS(9,IYRCNT) = PPTL7
      BWPRMS(10,IYRCNT) = PPTL2E
      BWPRMS(11,IYRCNT) = WYR

C     Process next year if less than 50.
      IF (IYRCNT .LT. 50) THEN
        GOTO 25
      ENDIF

C     End of file was reached, indicating an incomplete year.
C     Write error message if less than 1 complete year has been processed.
C     (Maybe add some corective measure, if possible, so that the
C     run does not terminate. To be determined with Kathy Sheehan)
C
  400 IF (IYRCNT .EQ. 0) THEN
        Write (*,*) "No complete years of weather data present."
      ELSE
        Write (*,410) IYRCNT, WFNAME
  410   FORMAT (/," ",I2," years of weather data processed from file: ",
     &          A)

        WRITE (JOSTND,*) "       PRDL24  PRDL46   PRDL7  DDFALL  ",
     &      "PRDE2B  TREEDD  PPTL24  PPTL46   PPTL7  PPTL2E     WYR"
        DO I2 = 1, IYRCNT
           WRITE (JOSTND,420) (BWPRMS(I,I2), I=1,11)
  420      FORMAT (/,6X,11(2X,F6.1))
        END DO
      ENDIF

C     Close weather data file and load final variables.
C
  450 CLOSE (JOWE)

      RETURN
      END
