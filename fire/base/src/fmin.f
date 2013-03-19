      SUBROUTINE FMIN (ICALL,NSP,LKECHO)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     FIRE - FIRE & SNAG MODEL
C
C     OPTION PROCESSOR FOR FIRE MODEL
C
C     CALLED FROM: INITRE [SINGLE-STAND VERSION & PPE]
C                  PPIN   [PPE]
C
C     PARAMETER DEFINITIONS
C     ICALL = 1 call is within a stand (called from INITRE)
C             2 call from outside a stand (called from PPIN)
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'METRIC.F77'
C
COMMONS
C
      INTEGER    KWCNT
      PARAMETER (KWCNT = 55)

      CHARACTER*4  NSP(MAXSP,3)
      CHARACTER*8  TABLE(KWCNT), KEYWRD, PASKEY
      CHARACTER*10 KARD(12)
      CHARACTER*10 APRMS(13)
      CHARACTER*40 PHOTOREF(32), REF
      CHARACTER*13 CHARCODE
      LOGICAL      LNOTBK(12), LOK, LKECHO
      INTEGER      NPARMS, IDT, ICALL, IFMD, NVALS, ILEN, ICANPR, J, K
      INTEGER      IRTNCD
      REAL         YRS50(2), YRS30(2)
      REAL         PRMS(13)
      REAL         ARRAY(12)

      DATA TABLE /
     >     'SALVSP  ','END     ','SVIMAGES','BURNREPT','MOISTURE',
     >     'SIMFIRE ','FLAMEADJ','POTFIRE ','SNAGFALL','SNAGBRK ',
     >     'SNAGDCAY','SNAGOUT ','SNAGCLAS','LANDOUT ','FUELOUT ',
     >     'FUELDCAY','DUFFPROD','MOREOUT ','FUELPOOL','SALVAGE ',
     >     'FUELINIT','SNAGINIT','PILEBURN','SNAGPBN ','FUELTRET',
     >     'STATFUEL','FUELREPT','MORTREPT','FUELMULT','POTFMOIS',
     >     'SNAGSUM', 'MORTCLAS','DROUGHT ','FUELMOVE','POTFWIND',
     >     'POTFTEMP','SNAGPSFT','FUELMODL','DEFULMOD','CANCALC ',
     >     'POTFSEAS','POTFPAB ','SOILHEAT','CARBREPT','CARBCUT ',
     >     'CARBCALC','CANFPROF','FUELFOTO','FIRECALC','FMODLIST',
     >     'DWDVLOUT','DWDCVOUT','FUELSOFT','USECFIM ','CFIMCALC'/    
      DATA PHOTOREF / 'Fischer INT-96                      ',
     >                'Fischer INT-97                      ',
     >                'Fischer INT-98                      ',
     >                '                                    ',
     >                'Koski and Fischer INT-46            ',
     >                'Maxwell and Ward PNW-52             ',
     >                'Blonski and Schramel PSW-56         ',
     >                'Maxwell and Ward PNW-105            ',
     >                'Ottmar and Hardy PNW-GTR-231        ',
     >                '                                    ',
     >                'Maxwell A-89-6-82                   ',
     >                'Southwestern region compilation     ',
     >                'Maxwell and Ward PNW-51             ',
     >                'Ottmar and others Volume I          ',
     >                'Ottmar and others Volume I          ',
     >                'Ottmar and Vihnanek Volume II / IIa ',
     >                'Ottmar and others Volume III        ',
     >                'Ottmar and others Volume V / Va     ',
     >                'Ottmar and others Volume VI / VIa   ',
     >                'Maxwell A-89-1-90                   ',
     >                'Ottmar and others Volume IV         ',
     >                'Wright and others PNW-GTR-545       ',
     >                'Ottmar and others PNW-GTR-258       ',
     >                'Lynch and Horton NA-FR-25           ',
     >                'Wilcox and others NA-FR-22          ',
     >                'Scholl and Waldrop GTR-SRS-26       ',
     >                'Ottmar and others Volume VII        ',
     >                'Maxwell and Ward PNW-95             ',
     >                'Sanders and Van Lear GTR-SE-49      ',
     >                'Wade and others GTR-SE-82           ',
     >                'Blank GTR-NC-77                     ',
     >                'Popp and Lundquist RMRS-GTR-172     ' /
      INTEGER KODE,IPRMPT,NUMBER,NPRMS,MYACT,
     &        II,JSP,IHEAD,ICHNG,ICLS,IDEC,
     &        ID,I,IFIRE,IARRY,KEY
      REAL    DKMULT,X,XSUM
C
C     **********          EXECUTION BEGINS          **********
C
   10 CONTINUE

C     THIS WAS INCREASED TO 9, AND THEN 12, BECAUSE WE NEEDED TO ADD FIELDS TO
C     THE POTFMOIS, MOISTURE, AND FUELINIT KEYWORDS 
C
      NVALS = 12
      CALL FMKEYRDR (IREAD,JOSTND,.FALSE.,KEYWRD,LNOTBK,
     >             ARRAY,IRECNT,KODE,KARD,LFLAG,NVALS)
      CALL fvsGetRtnCode(IRTNCD)
      IF (IRTNCD.NE.0) RETURN
C
C  RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK OR ANOTHER ERROR,2=EOF
C               LESS THAN ZERO...USE OF PARMS STATEMENT IS PRESENT.
C

      IF (KODE.LT.0) THEN
         IPRMPT=-KODE
      ELSE
         IPRMPT=0
      ENDIF
      IF (KODE .LE. 0) GO TO 30
      IF (KODE .EQ. 2) CALL ERRGRO(.FALSE.,2)
      CALL fvsGetRtnCode(IRTNCD)
      IF (IRTNCD.NE.0) RETURN
      CALL ERRGRO (.TRUE.,6)
      GOTO 10
   30 CONTINUE
      CALL FNDKEY (NUMBER,KEYWRD,TABLE,KWCNT,KODE,.FALSE.,JOSTND)
C
C     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.
C
      IF (KODE .EQ. 0) GOTO 90
      IF (KODE .EQ. 1) THEN
         CALL ERRGRO (.TRUE.,1)
         GOTO 10
      ENDIF
      GOTO 90
C
C     SPECIAL END-OF-FILE TARGET
C
   80 CONTINUE
      CALL ERRGRO (.FALSE.,2)
      CALL fvsGetRtnCode(IRTNCD)
      IF (IRTNCD.NE.0) RETURN

   90 CONTINUE
C
C     SIGNAL THAT THE FIRE MODEL IS NOW ACTIVE.
C
      LFMON = .TRUE.
C
C     PROCESS OPTIONS
C
      GO TO( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,
     &      1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,
     &      2100,2200,2300,2400,2500,2600,2700,2800,2900,3000,
     &      3100,3200,3300,3400,3500,3600,3700,3800,3900,4000,
     &      4100,4200,4300,4400,4500,4600,4700,4800,4900,5000,
     &      5100,5200,5300,5400,5500), NUMBER

  100 CONTINUE
C                        OPTION NUMBER 1 -- SALVSP
C
C     SET THE SPECIES TO BE CUT, OR LEFT, IN A SALVAGE OPERATION. ONCE IN
C     EFFECT IT STAYS IN EFFECT UNTIL RESET.
C

      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,2504) KEYWRD
  104    FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >      ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      MYACT = 2501
      IDT = 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IF (IPRMPT.GT.0) THEN
         IF (IPRMPT.NE.2) THEN
            CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
            CALL ERRGRO (.TRUE.,25)
         ELSE
            CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >                   IPRMPT,IRECNT,ICYC)
            CALL fvsGetRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         ENDIF
         GOTO 10
      ENDIF

      NPARMS= 2
      PRMS(1) = 0.0
      PRMS(2) = 0.0

      JSP = 0
      CALL SPDECD (2,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &             ARRAY,KARD)
      IF (JSP .EQ. -999) GOTO 10
      PRMS(1)=JSP

      IF (LNOTBK(3)) PRMS(2)= ARRAY(3)

      IF (PRMS(2) .LT. 1.0) PRMS(2) = 0.0
      IF (PRMS(2) .GE. 1.0) PRMS(2) = 1.0

      ILEN=3
      IF(JSP.LT.0)ILEN=ISPGRP(-JSP,52)
      IF(PRMS(2).LT. 1.0) THEN
        IF(LKECHO)WRITE(JOSTND,110) KEYWRD,IDT,KARD(2)(1:ILEN),JSP
  110   FORMAT(/1X,A8,'   DATE/CYCLE ',I4,' SPECIES=',A,' (CODE=',
     &   I3,') IS MARKED FOR CUTTING IN SUBSEQUENT SALVAGE',
     &       ' OPERATIONS. ')
      ELSE
        IF(LKECHO)WRITE(JOSTND,111)KEYWRD,IDT,KARD(2)(1:ILEN),JSP
  111   FORMAT(/1X,A8,'   DATE/CYCLE ',I4,' SPECIES=',A,' (CODE=',
     &   I3,') IS MARKED TO BE LEFT IN SUBSEQUENT SALVAGE',
     &       ' OPERATIONS. ')
      ENDIF

      MYACT = 2501
      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10

  200 CONTINUE
C                        OPTION NUMBER 2 -- END
      IF(LKECHO)WRITE(JOSTND,210) KEYWRD
  210 FORMAT (/1X,A8,'   END OF FIRE MODEL OPTIONS.')
      RETURN

  300 CONTINUE
C                        OPTION NUMBER 3 -- SVIMAGES
      IF (LNOTBK(1)) NFMSVPX = IFIX(ARRAY(1))
      IF(LKECHO)WRITE(JOSTND,310) KEYWRD, NFMSVPX
  310 FORMAT(/1X,A8,'   VISUALIZATION IMAGES PER FIRE= ',I4)
      GOTO 10
  400 CONTINUE
C                        OPTION NUMBER 4 -- BURNREPT
C
      IF (IDBRN .EQ. 0) CALL GETID (IDBRN)
C
      IDT = 1
      IF (LNOTBK(1)) IDT = IFIX(ARRAY(1))
      PRMS(1) = 200.0
      IF (LNOTBK(2) .AND. ARRAY(2) .GT. 0.0) PRMS(1) = ARRAY(2)
C
      IF(LKECHO)WRITE(JOSTND,415) KEYWRD,IDT,IFIX(PRMS(1))
  415 FORMAT(/1X,A8,'   THE BURN CONDITIONS REPORT WILL BE WRITTEN',
     &       ' WHEN A FIRE OCCURS.'
     &       /T13,'REPORTING WILL BEGIN IN DATE/CYCLE ',I4,
     &       ' AND WILL CONTINUE FOR',I4,' YEARS.')
C
      NPRMS = 1
      MYACT = 2504
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
      GOTO 10
  500 CONTINUE

C                        OPTION NUMBER 5 -- MOISTURE
C
C     SET THE FUEL MOISTURE CONDITIONS FOR THE DIFFERENT FUEL CATEGORIES
C     FOR VARIOUS STANDS IN A GIVEN YEAR.
C
      IF (ICALL .EQ. 2) THEN
        WRITE(JOSTND,504) KEYWRD
  504   FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >    ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
        GOTO 10
      ENDIF

      MYACT = 2505

      IDT= 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IF (IPRMPT .GT.0) THEN
         IF (IPRMPT.NE.2) THEN
            CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
            CALL ERRGRO (.TRUE.,25)
         ELSE
            CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >                   IPRMPT,IRECNT,ICYC)
            CALL fvsGetRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         ENDIF
         GOTO 10
      ENDIF

C     READ THE USER-DEFINED VALUES

      NPARMS= 7
      DO 505 II=1,NPARMS
         PRMS(II)= 0.0
         IF (LNOTBK(II+1)) PRMS(II)= ARRAY(II+1)
C
C     IF LIVE HERB MOISTURE NOT FILLED IN THEN ASSUME IT IS THE SAME AS
C     THE LIVE WOODY MOISTURE.
C
         IF ((II .EQ. 7) .AND. (.NOT. LNOTBK(II+1))) THEN
           PRMS(7) = PRMS(6)
         ENDIF
  505 CONTINUE

      IF(LKECHO)WRITE(JOSTND,510) KEYWRD, IDT, PRMS(1), PRMS(2),
     >                   PRMS(3), PRMS(4), PRMS(5), PRMS(6), PRMS(7)
  510 FORMAT(/1X,A8,'   IN DATE/CYCLE ', I4,' FUEL MOISTURE VALUES (%)',
     >                  ' WILL BE:',
     >      /T13,'1HR: ',F5.1,' 10HR: ',F5.1,' 100HR: ',F5.1,' 3+: ',
     >      F5.1,' DUFF: ',F5.1,' LIVE WOODY: ',F5.0,
     >      ' LIVE HERB: ',F5.0)
      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10
  600 CONTINUE
C                        OPTION NUMBER 6 -- SIMFIRE (formerly FIRECOND)
C
C     SET THE CONDITIONS UNDER WHICH THE FIRE WILL OCCUR: WIND, TEMPERATURE
C     AND FUEL MOISTURES.  SET THE MORTALITY CODE (0 = TURN OFF FFE MORTALITY,
C     1 = FFE ESTIMATES MORTALITY).  SET THE % OF THE STAND BURNED.
C     SET THE SEASON OF THE BURN.
C     1 = early spring (compact leaves), 2 = before greenup, 3 = after greenup, 4 = fall

      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,604) KEYWRD
  604    FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >     ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      MYACT = 2506

      IDT = 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IF (IPRMPT.GT.0) THEN
         IF (IPRMPT.NE.2) THEN
            CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
            CALL ERRGRO (.TRUE.,25)
         ELSE
            CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >                   IPRMPT,IRECNT,ICYC)
            CALL fvsGetRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         ENDIF
         GOTO 10
      ENDIF
C
      NPARMS= 6
      PRMS(1) = 20.0
      PRMS(2) = 1.0
      PRMS(3) = 70.0
      PRMS(4) = 1.0
      PRMS(5) = 100.0
      PRMS(6) = 1
C
      IF (LNOTBK(2)) PRMS(1) = ARRAY(2) * KMtoMI
      IF (LNOTBK(3)) PRMS(2) = ARRAY(3)
      IF (LNOTBK(4)) PRMS(3) = ARRAY(4) * CtoF1 + CtoF2
      IF (LNOTBK(5)) PRMS(4) = ARRAY(5)
      IF (LNOTBK(6)) PRMS(5) = ARRAY(6)
      IF (LNOTBK(7)) PRMS(6) = ARRAY(7)
      IF (PRMS(4) .LT. 0) PRMS(4) = 0
      IF (PRMS(4) .GT. 1) PRMS(4) = 1
      IF (PRMS(5) .LT. 0) PRMS(5) = 0
      IF (PRMS(5) .GT. 100) PRMS(5) = 100
      IF (PRMS(6) .LT. 1) PRMS(6) = 1
      IF (PRMS(6) .GT. 4) PRMS(6) = 4
C
      IF(LKECHO)WRITE(JOSTND,610) KEYWRD, IDT, PRMS(1) * MItoKM, 
     >  PRMS(2),PRMS(3)*FtoC1+FtoC2,PRMS(4),PRMS(5),PRMS(6)
  610 FORMAT(/1X,A8,T13,'FIRE CONDITIONS IN DATE/CYCLE ',
     >  I4,' WILL BE: WIND: ',F5.1,' KM/H.'
     >  /T13,'FUEL MOISTURE VALUES WILL USE THE ',
     >  'PRESET MOISTURE CONDITION ',F3.0
     >  /T13,'TEMPERATURE: ',F5.0,' DEGREES C.'
     >  /T13,'MORTALITY CODE: ',F2.0,' (0 = TURN OFF FFE MORTALITY',
     >  ', 1 = FFE ESTIMATES MORTALITY)'
     >  /T13,'PERCENTAGE OF THE STAND BURNED: ',F6.1
     >  /T13,'SEASON OF THE BURN: ',F2.0,' (1 = EARLY SPRING ',
     >  '(COMPACT LEAVES), 2 = BEFORE GREENUP, 3 = AFTER GREENUP,',
     >  ' 4 = FALL)')

      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10

  700 CONTINUE
C                        OPTION NUMBER 7 -- FLAMEADJ (formerly FIRETYPE)
C
C     SELECT THE TYPE OF FIRE WHICH WILL OCCUR (INCLUDING FLAME LENGTH OR FLAME
C     LENGTH MULTIPLIER, AND PERCENT OF TREES EXPERIENCING CROWNING).
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,704) KEYWRD
  704    FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >      ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      MYACT = 2507
      IDT = 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IF (IPRMPT.GT.0) THEN
         IF (IPRMPT.NE.2) THEN
            CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
            CALL ERRGRO (.TRUE.,25)
         ELSE
            CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >                   IPRMPT,IRECNT,ICYC)
            CALL fvsGetRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         ENDIF
         GOTO 10
      ENDIF

      NPARMS= 3
      PRMS(1) =  1.0
      PRMS(2) = -1.0
      PRMS(3) = -1.0
      PRMS(4) = -1.0

      IF (LNOTBK(2)) PRMS(1)= ARRAY(2)
      IF (LNOTBK(3)) PRMS(2)= ARRAY(3) * MtoFT
      IF (LNOTBK(4)) PRMS(3)= ARRAY(4)
      IF (LNOTBK(5)) THEN
         PRMS(4)= ARRAY(5) * MtoFT
         NPARMS= 4
      ENDIF

C
C     SET BOUNDS FOR SOME OF THE VALUES
C
      IF (PRMS(1) .LT. 0.0)   PRMS(1) = 1.0
      IF (PRMS(2) .LT. 0.0)   PRMS(2) = -1.0
      IF (PRMS(3) .LT. 0.0)   PRMS(3) = -1.0
      IF (PRMS(3) .GT. 100.0) PRMS(3) = 100.

      IF (PRMS(2) .GT. 0.0) THEN
         IF(LKECHO)WRITE(JOSTND,717) KEYWRD, IDT, PRMS(2) * FTtoM
  717    FORMAT (/1X,A8,T13,'IN DATE/CYCLE ', I4,
     >           ' FLAME LENGTH WILL BE: ',F6.1,' M.')
      ELSE
         IF(LKECHO)WRITE(JOSTND,719) KEYWRD, IDT, PRMS(1)
  719    FORMAT (/1X,A8,T13,'IN DATE/CYCLE ', I4,
     >        ' FLAME LENGTH WILL BE CALCULATED BASED ON APPLICABLE',
     >        ' CONDITIONS,',/T13,'AND THEN MULTIPLIED BY ',F7.3)
      ENDIF
      IF (PRMS(3).EQ.-1.) THEN
        IF(LKECHO)WRITE(JOSTND,721)
  721   FORMAT (T13,'THE MODEL PREDICTS THE % CROWNING.')
      ELSE
        IF(LKECHO)WRITE(JOSTND,722) PRMS(3)
  722   FORMAT (T13,F5.1,' % OF THE CROWN WILL UNDERGO CROWNING.')
      ENDIF
      IF (NPARMS.GE.4) THEN
        IF(LKECHO)WRITE(JOSTND,723) PRMS(4) * FTtoM
  723   FORMAT (T13,'SCORCH HEIGHT =',F10.2,' M.')
      ENDIF

      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10
  800 CONTINUE
C                        OPTION NUMBER 8 -- POTFIRE (formerly POTFLAME)
C
C     GENERATE THE POTENTIAL FIRE (FLAME LENGTH) REPORT.
C     RETREIVE AN ID NUMBER FOR THE REPORT.
C
      IF (IDPFLM .EQ. 0) CALL GETID (IDPFLM)
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,804) KEYWRD
  804    FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >     ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      IDT = 1
      PRMS(1) = 200
      PRMS(2) = 1.0

      IF (LNOTBK(1)) IDT = INT(ARRAY(1))
      IF (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))

      IF(LKECHO)WRITE(JOSTND,810) KEYWRD, IDT, INT(PRMS(1)),
     >                             INT(PRMS(2))
  810 FORMAT(/1X,A8,'   THE POTENTIAL FIRE REPORT WILL BE PRINTED ',
     &      'STARTING IN DATE/CYCLE ',I4,','/T13,'FOR ',I4,
     &      ' YEARS USING ',I2,' YEAR INCREMENTS.')

      MYACT = 2508
      NPARMS=2
      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10

  900 CONTINUE
C                        OPTION NUMBER 9 -- SNAGFALL
C
C     SET THE NEW FALL RATE PARAMETERS FOR THE SNAG MODEL.
C        NOTE THAT MORE THAN ONE SUPPLEMENTAL RECORD IS REQUIRED
C           IF ENTERING MULTIPLE SPECIES.
C
C
      JSP = 0
      CALL SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &             ARRAY,KARD)
      IF (JSP .EQ. -999) GOTO 10
      IF (JSP .NE. 0 ) THEN

         IF (LNOTBK(2)) FALLX(JSP) = ARRAY(2)
         IF (FALLX(JSP) .LT. 0.001) FALLX(JSP) = 0.001

         IF (LNOTBK(3)) ALLDWN(JSP) = ARRAY(3)
         IF (ALLDWN(JSP) .LT. 0.0) ALLDWN(JSP) = 0.0

      ELSE

         Do JSP=1,MAXSP
            IF (LNOTBK(2)) FALLX(JSP) = ARRAY(2)
            IF (FALLX(JSP) .LT. 0.001) FALLX(JSP) = 0.001
            IF (LNOTBK(3)) ALLDWN(JSP) = ARRAY(3)
            IF (ALLDWN(JSP) .LT. 0.0) ALLDWN(JSP) = 0.0
         ENDDO
         JSP=1

      ENDIF

      IF(LKECHO)WRITE(JOSTND,915) KEYWRD, KARD(1)(1:3), FALLX(JSP),
     >                            ALLDWN(JSP)
  915 FORMAT(/1X,A8,'   FOR SPECIES ',A,
     &     ', THE RATE-OF-FALL CORRECTION MULTIPLIER IS: ',F6.3,
     &     /T13,'THE SNAG AGE BY WHICH THE LAST 5% FALL: ',
     &     F6.1)

      GOTO 10
 1000 CONTINUE
C                        OPTION NUMBER 10 -- SNAGBRK
C
C     THIS KEYWORD USED TO BE USED TO SET THE "HEIGHT-LOSS CORRECTION FACTOR"
C     FOR EACH SPECIES, AND (FOR ALL SPECIES) THE PROPORTION OF SNAGS THAT ARE
C     INITIALLY SOFT, THE MULTIPLIER FOR ADJUSTING THE HEIGHT-LOSS RATE OF
C     INITIALLY SOFT SNAGS AND THE BASE RATE TO USE FOR ALL SPECIES FOR THE
C     FIRST AND LAST 50% OF HEIGHT LOSS.
C
C     THIS KEYWORD IS NOW CHANGED SO THAT USERS SPECIFY, FOR EVERY SPECIES AND
C     HARD/SOFT, THE YEARS FOR THE FIRST 50% AND THE NEXT 30% OF HEIGHT TO BE
C     LOST. THE PROGRAM THEN DETERMINES THE APPROPRIATE "HEIGHT-LOSS CORRECTION
C     FACTOR" THAT WILL BE USED IN THE EQUATIONS. THE PROPORTION OF SNAGS THAT
C     ARE INITIALLY SOFT HAS BEEN MOVED TO THE KEYWORD: SNAGPSFT. (FEB 2002)
C
C     NOTE: THESE LOG-BASED EQUATIONS WILL STILL HAVE A PROBLEM IF HTX*HTR > 1
C
      JSP = 0
      CALL SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &             ARRAY,KARD)
      IF (JSP .EQ. -999) GOTO 10

      YRS50(1) = INT(ARRAY(2))
      IF (YRS50(1) .LT. 1) YRS50(1) = 1
      YRS50(2) = INT(ARRAY(3))
      IF (YRS50(2) .LT. 1) YRS50(2) = 1

      YRS30(1) = INT(ARRAY(4))
      IF (YRS30(1) .LT. 1) YRS30(1) = 1
      YRS30(2) = INT(ARRAY(5))
      IF (YRS30(2) .LT. 1) YRS30(2) = 1

      IF (JSP .NE. 0 ) THEN
C        DO THE CALCULATIONS FOR INITIALLY HARD SNAGS
         IF (LNOTBK(2)) THEN
            HTX(JSP,1) = (1. - 0.5**(1./YRS50(1))) / HTR1
         ELSEIF (HTX(JSP,1) .NE. 0.0) THEN
            YRS50(1) = LOG(0.5) / LOG(1.-MIN(0.9,HTX(JSP,1)*HTR1))
         ELSE
            YRS50(1) = 999
         ENDIF
         IF (LNOTBK(4)) THEN
            IF (YRS30(1) .LE. YRS50(1)) YRS30(1) = YRS50(1) + 0.001
            HTX(JSP,2) = (1. - (0.3/0.5)**(1./(YRS30(1)-YRS50(1))))
     &                         / HTR2
         ELSEIF (HTX(JSP,2) .NE. 0.0) THEN
            YRS30(1) = YRS50(1) + LOG(0.3/0.5) /
     &                            LOG(1.-MIN(0.9,HTX(JSP,2)*HTR2))
         ELSE
            YRS30(1) = 999
         ENDIF

C        AND INITIALLY SOFT SNAGS
         IF (LNOTBK(3)) THEN
            HTX(JSP,3) = (1. - 0.5**(1./YRS50(2))) / (HTR1*HTXSFT)
         ELSEIF (HTX(JSP,3) .NE. 0.0 .AND. HTXSFT .NE. 0.0) THEN
            YRS50(2) = LOG(0.5) /
     &                 LOG(1.-MIN(0.9,HTX(JSP,3)*HTR1*HTXSFT))
         ELSE
            YRS50(2) = 999
         ENDIF
         IF (LNOTBK(5)) THEN
            IF (YRS30(2) .LE. YRS50(2)) YRS30(2) = YRS50(2) + 0.001
            HTX(JSP,4) = (1. - (0.3/0.5)**(1./(YRS30(2)-YRS50(2))))
     &                        / (HTR2*HTXSFT)
         ELSEIF (HTX(JSP,4) .NE. 0.0 .AND. HTXSFT .NE. 0.0) THEN
            YRS30(2) = YRS50(2) + LOG(0.3/0.5) /
     &                         LOG(1.-MIN(0.9,HTX(JSP,4)*HTR2*HTXSFT))
         ELSE
            YRS30(2) = 999
         ENDIF


      ELSE
         DO JSP=1,MAXSP
C           DO THE CALCULATIONS FOR INITIALLY HARD SNAGS
            IF (LNOTBK(2)) THEN
               HTX(JSP,1) = (1. - 0.5**(1./YRS50(1))) / HTR1
            ELSEIF (HTX(JSP,1) .NE. 0.0) THEN
               YRS50(1) = LOG(0.5) / LOG(1.-MIN(0.9,HTX(JSP,1)*HTR1))
            ELSE
               YRS50(1) = 999
            ENDIF
            IF (LNOTBK(4)) THEN
               IF (YRS30(1) .LE. YRS50(1)) YRS30(1) = YRS50(1) + 0.001
               HTX(JSP,2) = (1. - (0.3/0.5)**(1./(YRS30(1)-YRS50(1))))
     &                                    / HTR2
            ELSEIF (HTX(JSP,2) .NE. 0.0) THEN
               YRS30(1) = YRS50(1) + LOG(0.3/0.5) /
     &                               LOG(1.-MIN(0.9,HTX(JSP,2)*HTR2))
            ELSE
               YRS30(1) = 999
            ENDIF

C           AND INITIALLY SOFT SNAGS
            IF (LNOTBK(3)) THEN
               HTX(JSP,3) = (1. - 0.5**(1./YRS50(2))) / (HTR1*HTXSFT)
            ELSEIF (HTX(JSP,3) .NE. 0.0 .AND. HTXSFT .NE. 0.0) THEN
               YRS50(2) = LOG(0.5) /
     &                    LOG(1.-MIN(0.9,HTX(JSP,3)*HTR1*HTXSFT))
            ELSE
               YRS50(2) = 999
            ENDIF
            IF (LNOTBK(5)) THEN
               IF (YRS30(2) .LE. YRS50(2)) YRS30(2) = YRS50(2) + 0.001
               HTX(JSP,4) = (1. - (0.3/0.5)**(1./(YRS30(2)-YRS50(2))))
     &                         / (HTR2*HTXSFT)
            ELSEIF (HTX(JSP,4) .NE. 0.0 .AND. HTXSFT .NE. 0.0) THEN
               YRS30(2) = YRS50(2) + LOG(0.3/0.5) /
     &                    LOG(1.-MIN(.9,HTX(JSP,4)*HTR2*HTXSFT))
            ELSE
               YRS30(2) = 999
            ENDIF

         ENDDO
         JSP=1

      ENDIF

      IF(LKECHO)WRITE(JOSTND,1005) KEYWRD, KARD(1)(1:3), INT(YRS50(1)),
     >  INT(YRS50(2)),INT(YRS30(1)), INT(YRS30(2))
 1005 FORMAT(/1X,A8,'   FOR SPECIES ',A,
     >  ', THE YEARS TO 50% HT LOSS FOR INITIALLY HARD SNAGS IS: ',
     >  I4,/T13,' AND FOR INITIALLY SOFT SNAGS IS: ',I4,/T13,
     >  'YEARS TO THE NEXT 30% HT LOSS FOR INITIALLY HARD SNAGS: ',
     >  I4,/T13,' AND FOR INITIALLY SOFT SNAGS IS: ',I4)

      GOTO 10

 1100 CONTINUE
C                        OPTION NUMBER 11 -- SNAGDCAY
C
C     SET THE NEW DECAY RATE PARAMETERS FOR THE MODEL
C
      JSP = 0
      CALL SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &             ARRAY,KARD)
      IF (JSP .EQ. -999) GOTO 10
      IF (JSP .NE. 0 ) THEN

         IF (LNOTBK(2)) DECAYX(JSP) = ARRAY(2)
         IF (DECAYX(JSP) .LT. 0.0) DECAYX(JSP) = 0.0
      ELSE

         Do JSP=1,MAXSP
            IF (LNOTBK(2)) DECAYX(JSP) = ARRAY(2)
            IF (DECAYX(JSP) .LT. 0.0) DECAYX(JSP) = 0.0
         ENDDO
         JSP=1

      ENDIF

      IF(LKECHO)WRITE(JOSTND,1115) KEYWRD, KARD(1)(1:3), DECAYX(JSP)
 1115 FORMAT(/1X,A8,'   FOR SPECIES ',A,
     >     ', THE RATE-OF-DECAY CORRECTION MULTIPLIER IS:',F5.3)

      GOTO 10
 1200 CONTINUE
C                        OPTION NUMBER 12 --  SNAGOUT
C
C     PRINT THE SNAG OUTPUT TABLE. USERS MAY SELECT ONE/SEVERAL/ALL STANDS
C     IN ONE/PERIODIC/ALL YEARS
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,1204) KEYWRD
 1204    FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >      ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      IDT = 1
      PRMS(1)=200.
      PRMS(2)=5.
      PRMS(3)=13.
      PRMS(4)=0.

      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))
      IF (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))
      IF (LNOTBK(4)) PRMS(3) = INT(ARRAY(4))
      IF (LNOTBK(5)) PRMS(4) = INT(ARRAY(5))

      IF(LKECHO)WRITE(JOSTND,1215) KEYWRD, IDT, INT(PRMS(1)),
     >  INT(PRMS(2)), INT(PRMS(3))
 1215 FORMAT(/1X,A8,'   THE SNAG LIST WILL BE OUTPUT',
     &  ' STARTING IN DATE/CYCLE ',I4,','/T13,'FOR ',I4,
     &  ' YEARS USING ',I2,' YEAR INCREMENTS.'/
     &  T13,'OUTPUT WILL BE PRINTED TO UNIT ',I3)

      IF (PRMS(4).GT.0) THEN
        IF(LKECHO)WRITE(JOSTND,1216) ' NOT '
      ELSE
        IF(LKECHO)WRITE(JOSTND,1216) ' '
 1216   FORMAT(T13,'HEADINGS WILL',A,'BE PRINTED.')
      ENDIF

      MYACT = 2512
      NPARMS= 4
      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10
 1300 CONTINUE
C                        OPTION NUMBER 13 -- SNAGCLAS
C
C     ENTER THE LOWER BOUND OF THE CLASSES BY WHICH TO GROUP THE
C     SNAG LIST OUTPUT
C
      ICHNG = 0
      DO 1320 ICLS=1,6
         IF (LNOTBK(ICLS)) THEN
            SNPRCL(ICLS)= ARRAY(ICLS) * CMtoIN
            ICHNG = ICLS
            IF (SNPRCL(ICLS) .GT. 36.0 .AND. SNPRCL(ICLS) .NE. 999.0)
     &               SNPRCL(ICLS) = 36.0
         ENDIF
 1320 CONTINUE
      IF (ICHNG .GT. 0) THEN
         DO 1330 ICLS=ICHNG+1,6
            SNPRCL(ICLS) = 999.0
 1330    CONTINUE
      ENDIF

      IF(LKECHO)WRITE(JOSTND,1350) KEYWRD,
     >  (ICLS,SNPRCL(ICLS)*INtoCM,ICLS=1,6)
 1350 FORMAT (/1X,A8,'   THE LOWER BOUNDARY FOR EACH DBH CLASS FOR ',
     &  'PRINTING THE SNAG OUTPUT (IN CM): ',
     &  /T13,6('CLASS',I2,'=',F4.1:'; '))
      GOTO 10
 1400 CONTINUE
C                        OPTION NUMBER 14 -- LANDOUT
C
C     REQUEST THAT THE LANDSCAPE OUTPUT TABLES BE PRINTED. THERE ARE THREE:
C     1) FUEL LOADING LEVELS, 2) FUEL MODEL, 3) FIRE INFO
C     (EFFECTS/POTENTIAL FLAMES).
C     USING THE KEYWORD MEANS THAT THE OUTPUT FILES WILL BE PRINTED (EVEN IF
C     NONE OF THE FIELDS ARE ENTERED) UNLESS THE USER ENTERS A VALUE <= 0.
C
      IF (ICALL .EQ. 1) THEN
         IF(LKECHO)WRITE(JOSTND,1404) KEYWRD
 1404    FORMAT(/1X,A8,'   ***KEYWORD IS A LANDSCAPE-LEVEL KEYWORD',
     >         ' ONLY')
         GOTO 10
      ENDIF

      JLOUT(1) = 36
      JLOUT(2) = 37
      JLOUT(3) = 38
      PLSIZ(1) = 4
      PLSIZ(2) = 8
      IHEAD = 0
      IF (LNOTBK(1)) JLOUT(1)= IFIX(ARRAY(1))
      IF (LNOTBK(2)) JLOUT(2)= IFIX(ARRAY(2))
      IF (LNOTBK(3)) JLOUT(3)= IFIX(ARRAY(3))
      IF (LNOTBK(4)) PLSIZ(1)= IFIX(ARRAY(4) * MtoFT)
      IF (LNOTBK(5)) PLSIZ(2)= IFIX(ARRAY(5) * MtoFT)
      IF (LNOTBK(6)) IHEAD = IFIX(ARRAY(6))
      IF (IHEAD .NE. 0) LANHED = .FALSE.

      IF (JLOUT(1) .GT. 0 .OR. JLOUT(2) .GT. 0 .OR. JLOUT(3).GT. 0) THEN
         IF(LKECHO)WRITE(JOSTND, 1410) KEYWRD
 1410    FORMAT(/1X,A8,'   THE FOLLOWING LANDSCAPE-LEVEL REPORTS WILL',
     &          ' BE PRINTED EACH YEAR: ')

         IF((JLOUT(1) .GT. 0).AND.LKECHO)WRITE(JOSTND,1420) JLOUT(1)
         IF((JLOUT(2) .GT. 0).AND.LKECHO)WRITE(JOSTND,1430) JLOUT(2)
         IF((JLOUT(3) .GT. 0).AND.LKECHO)WRITE(JOSTND,1440) JLOUT(3),
     >    IFIX(PLSIZ(1)*MtoFT), IFIX(PLSIZ(2)*MtoFT)

 1420    FORMAT(T18,'LOADING CATEGORIES FOR FUEL AND SNAGS, UNIT: ',I3)
 1430    FORMAT(T18,'PERCENT OF LANDSCAPE USING EACH FUEL MODEL, ',
     &              'UNIT: ',I3)
 1440    FORMAT(T18,'FIRE EFFECTS & POTENTIAL FLAME LENGTH INFO, ',
     &              'UNIT: ',I3,/T18,'THE AREA WILL BE GROUPED INTO ',
     &              'FLAME LENGTHS THAT ARE ABOVE OR BELOW ',I3,' M',
     &              ' AND ABOVE ',I3,' M.')

         IF((.NOT. LANHED).AND.LKECHO)WRITE(JOSTND,1450)
 1450    FORMAT(T13,'TABLE HEADINGS WILL NOT BE PRINTED.')

      ELSE
        IF(LKECHO)WRITE(JOSTND,1490) KEYWRD
 1490   FORMAT(/1X,A8,'     NO LANDSCAPE-LEVEL REPORTS WILL BE PRINTED')
      ENDIF

      GOTO 10
 1500 CONTINUE
C                        OPTION NUMBER 15 -- FUELOUT
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,1504) KEYWRD
 1504    FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >     ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF
C
      IF (IDFLAL .EQ. 0) CALL GETID (IDFLAL)
C
      IDT = 1
      PRMS(1) = 200
      PRMS(2) = 1.0
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))
      IF (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))
C
      IF(LKECHO)WRITE(JOSTND,1515) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2))
 1515 FORMAT(/1X,A8,'   THE ALL FUELS REPORT WILL BEGIN PRINTING IN',
     &      ' DATE/CYCLE ',I4,','/T13,'FOR ',I4,
     &      ' YEARS USING ',I2,' YEAR INCREMENTS.')
C
      NPRMS = 2
      MYACT = 2515
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
      GOTO 10
 1600 CONTINUE
C                        OPTION NUMBER 16 -- FUELDCAY
C
C     CHANGE THE TOTAL DECAY RATE OF FUELS.  [LANDSCAPE-LEVEL]
C
      IDEC = 0
      IF (LNOTBK(1)) THEN
         ID = IFIX(ARRAY(1))
         IDEC = ID
         IF (ID .GT. 4) IDEC = 4
         IF (ID .LE. 0) IDEC = 1

         IF (LNOTBK(2)) DKR(10,IDEC) = ARRAY(2)
         IF (LNOTBK(3)) DKR(11,IDEC) = ARRAY(3)
         IF (LNOTBK(4)) DKR(1,IDEC) = ARRAY(4)
         IF (LNOTBK(5)) DKR(2,IDEC) = ARRAY(5)
         IF (LNOTBK(6)) DKR(3,IDEC) = ARRAY(6)
         IF (LNOTBK(7)) THEN
            DKR(4,IDEC) = ARRAY(7)
            DKR(5,IDEC) = ARRAY(7)
            DKR(6,IDEC) = ARRAY(7)
            DKR(7,IDEC) = ARRAY(7)
            DKR(8,IDEC) = ARRAY(7)
            DKR(9,IDEC) = ARRAY(7)
         ENDIF

C        NOW RE-DETERMINE THE DECAY RATE TO DUFF
         IF (ID .LT. 5) THEN
            DO 1620 I=1,10
               IF (DKR(I,IDEC) .GT. 1.0) DKR(I,IDEC) = 1.0
               TODUFF(I,IDEC) = DKR(I,IDEC) * PRDUFF(I)
 1620       CONTINUE
            IF(LKECHO)WRITE(JOSTND,1650) KEYWRD,IDEC,DKR(10,IDEC),
     &         DKR(11,IDEC),(DKR(I,IDEC),I=1,4)
         ELSE
            DO 1635 IDEC=1,4
              DO J=1,11
                DKR(J,IDEC) = DKR(J,4)
              ENDDO
              DO 1630 I=1,10
                IF (DKR(I,IDEC) .GT. 1.0) DKR(I,IDEC) = 1.0
                TODUFF(I,IDEC) = DKR(I,IDEC) * PRDUFF(I)
 1630         CONTINUE
 1635       CONTINUE

            IF(LKECHO)WRITE(JOSTND,1655) KEYWRD,DKR(10,4),DKR(11,4),
     &         (DKR(I,4),I=1,4)
         ENDIF

 1650    FORMAT (/1X,A8,'   THE TOTAL DECAY RATES FOR DECAY CLASS',I2,
     &       ' WILL BE',/T13,'LITTER: ',F5.3,' DUFF: ',F5.3,' 0-.25: ',
     &       F5.3,' .25-1: ',F5.3,' 1-3: ',F5.3,' >3: ',F5.3)
 1655    FORMAT (/1X,A8,'   THE TOTAL DECAY RATES FOR ALL DECAY CLAS',
     &    'SES WILL BE',/T13,'LITTER: ',F5.3,' DUFF: ',F5.3,' 0-.25: ',
     &       F5.3,' .25-1: ',F5.3,' 1-3: ',F5.3,' >3: ',F5.3)

      ELSE

         WRITE(JOSTND,1660) KEYWRD
 1660    FORMAT (/1X,A8,'    **** NO DECAY CLASS WAS SPECIFIED. ',
     &           'KEYWORD WILL BE IGNORED!')
         CALL ERRGRO (.TRUE.,1)
      ENDIF


      GOTO 10
 1700 CONTINUE
C                        OPTION NUMBER 17 -- DUFFPROD
C
C     CHANGE THE PROPORTION OF THE DECAY RATE THAT GOES TO DUFF (AS OPPOSED
C     TO GOING TO THE AIR).  THIS CAN BE DONE FOR THE 3 SMALLEST FUEL
C     CATEGORIES, EVERYTHING LARGER THAN 3" AND LITTER.
C
      IF (LNOTBK(1)) THEN
         IDEC = IFIX(ARRAY(1))
         IF (IDEC .LE. 0) IDEC = 1

         IF (LNOTBK(7)) THEN
            DO 1710 I=1,10
               PRDUFF(I) = ARRAY(7)
 1710       CONTINUE
         ENDIF
         IF (LNOTBK(2)) PRDUFF(10) = ARRAY(2)
         IF (LNOTBK(3)) PRDUFF(1) = ARRAY(3)
         IF (LNOTBK(4)) PRDUFF(2) = ARRAY(4)
         IF (LNOTBK(5)) PRDUFF(3) = ARRAY(5)
         IF (LNOTBK(6)) THEN
            PRDUFF(4) = ARRAY(6)
            PRDUFF(5) = ARRAY(6)
            PRDUFF(6) = ARRAY(6)
            PRDUFF(7) = ARRAY(6)
            PRDUFF(8) = ARRAY(6)
            PRDUFF(9) = ARRAY(6)
         ENDIF

C        NOW RE-DETERMINE THE DECAY RATES TO DUFF AND TO AIR
         IF (IDEC .LE. 4) THEN
            DO 1720 I=1,10
               IF (PRDUFF(I) .GT. 1.0) PRDUFF(I) = 1.0
               IF (PRDUFF(I) .LT. 0.0) PRDUFF(I) = 0.0
               TODUFF(I,IDEC) = PRDUFF(I) * DKR(I,IDEC)
 1720       CONTINUE

            IF(LKECHO)WRITE(JOSTND,1750) KEYWRD,IDEC,
     &         PRDUFF(10), (PRDUFF(I),I=1,4)
 1750       FORMAT (/1X,A8,'   THE PROPORTION OF THE DECOMPOSING ',
     &      ' MATERIAL WHICH GOES TO DUFF IN DECAY POOL ',I2,' IS:',
     &       /T13,'LITTER: ',F4.2,' 0-.64: ',F4.2,
     &      ' .64-2.5: ',F4.2,' 2.5-7.6: ',F4.2,' >7.6: ',F4.2)
         ELSE
            DO 1735 I=1,10
               IF (PRDUFF(I) .GT. 1.0) PRDUFF(I) = 1.0
               IF (PRDUFF(I) .LT. 0.0) PRDUFF(I) = 0.0
               DO 1730 IDEC=1,4
                  TODUFF(I,IDEC) = PRDUFF(I) * DKR(I,IDEC)
 1730          CONTINUE
 1735       CONTINUE

            IF(LKECHO)WRITE(JOSTND,1755) KEYWRD, PRDUFF(10),
     >                                   (PRDUFF(I),I=1,4)
 1755       FORMAT (/1X,A8,'   THE PROPORTION OF THE DECOMPOSING ',
     &      ' MATERIAL WHICH GOES TO DUFF IS',/T13,'LITTER: ',F4.2,
     &      ' 0-.64: ',F4.2,
     &      ' .64-2.5: ',F4.2,' 2.5-7.6: ',F4.2,' >7.6: ',F4.2)
         ENDIF
      ELSE
         WRITE(JOSTND,1760) KEYWRD
 1760    FORMAT (/1X,A8,'    ****NO DECAY POOL WAS SPECIFIED. ',
     &           'KEYWORD WILL BE IGNORED!')
      ENDIF

      GOTO 10
 1800 CONTINUE
C                        OPTION NUMBER 18 -- MOREOUT
C
C     A TOP-SECRET KEYWORD THAT ALLOWS US MODELLERS TO GET SOME
C     ADDITIONAL OUTPUT
C
      JCOUT = 30
      IF(LKECHO)WRITE(JOSTND,1860) KEYWRD
 1860 FORMAT (/1X,A8)

      GOTO 10
 1900 CONTINUE
C                        OPTION NUMBER 19 -- FUELPOOL
C
C     SET THE FUEL DECAY POOL THAT EACH SPECIES BELONGS TO.
C
      JSP = 0
      CALL SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &             ARRAY,KARD)
      IF (JSP .EQ. -999 .OR. .NOT.LNOTBK(2)) GOTO 10
      IDEC = INT(ARRAY(2))
      IF (IDEC .LT. 1 .OR. IDEC .GT. 4) GOTO 10

      IF (JSP .NE. 0) THEN
         DKRCLS(JSP) = IDEC
      ELSE
         Do JSP=1,MAXSP
            DKRCLS(JSP) = IDEC
         ENDDO
         JSP=1
      ENDIF

      IF(LKECHO)WRITE(JOSTND,1905) KEYWRD, KARD(1)(1:3), DKRCLS(JSP)
 1905 FORMAT(/1X,A8,'   FOR SPECIES ',A,', THE FUEL DECAY CLASS ',
     >     ' HAS BEEN CHANGED TO:',I1)

      GOTO 10
 2000 CONTINUE
C                        OPTION NUMBER 20 -- SALVAGE
C
C     THIS KEYWORD ALLOWS USERS TO REMOVE SNAGS FROM THE SNAG LIST.
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,2004) KEYWRD
 2004    FORMAT(/1X,A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >      ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      MYACT = 2520
      IDT = 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IF (IPRMPT.GT.0) THEN
         IF (IPRMPT.NE.2) THEN
            CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
            CALL ERRGRO (.TRUE.,25)
         ELSE
            CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >                   IPRMPT,IRECNT,ICYC)
            CALL fvsGetRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         ENDIF
         GOTO 10
      ENDIF

      NPARMS= 6
      PRMS(1) = 10.0
      PRMS(2) = 999.0
      PRMS(3) = 5.0
      PRMS(4) = 0.0
      PRMS(5) = 0.9
      PRMS(6) = 0.0

      IF (LNOTBK(2)) PRMS(1)= ARRAY(2) * CMtoIN
      IF (LNOTBK(3)) PRMS(2)= ARRAY(3) * CMtoIN
      IF (LNOTBK(4)) PRMS(3)= ARRAY(4)
      IF (LNOTBK(5)) PRMS(4)= ARRAY(5)
      IF (LNOTBK(6)) PRMS(5)= ARRAY(6)
      IF (LNOTBK(7)) PRMS(6)= ARRAY(7)
C
C     SET THE BOUNDS FOR VARIOUS VARIABLES
C
      PRMS(4) = INT(PRMS(4))
      IF (PRMS(4) .GT. 2.0 .OR. PRMS(4) .LT. 0.0) PRMS(4) = 0.0
      PRMS(5) = MIN(1.0,MAX(0.0,PRMS(5)))
      PRMS(6) = MIN(1.0,MAX(0.0,PRMS(6)))

      IF(LKECHO)WRITE(JOSTND,2010) KEYWRD, IDT,(PRMS(I),I=1,3)
 2010 FORMAT(/1X,A8,'   IN DATE/CYCLE ', I4,' THE PROPORTION OF SNAGS ',
     >      'THAT ARE BETWEEN ',F4.0,' AND ',F4.0,/T13,
     >      'CM DBH AND HAVE BEEN DEAD LESS THAN ',F4.0,' YEARS ',
     >      'AND ARE ')

      IF (PRMS(4) .EQ. 1.0) THEN
         IF(LKECHO)WRITE(JOSTND,2011) 'STILL HARD',PRMS(5)
      ELSE IF (PRMS(4) .EQ. 2.0) THEN
         IF(LKECHO)WRITE(JOSTND,2011) 'SOFT',PRMS(5)
      ELSE
         IF(LKECHO)WRITE(JOSTND,2011) 'EITHER HARD OR SOFT',PRMS(5)
      ENDIF
 2011 FORMAT(T13,A,' THAT WILL BE REMOVED AS A SALVAGE ',
     >    'CUT IS ',F5.3)
      IF(LKECHO)WRITE(JOSTND,2012) PRMS(6)
 2012 FORMAT(T13,'THE PROPORTION OF TREATED SNAGS THAT WILL REMAIN IN ',
     >    'THE STAND IS ',F5.3)

      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10

 2100 CONTINUE
C                        OPTION NUMBER 21 -- FUELINIT
C
C     THIS KEYWORD SETS THE INITIAL HARD/SOUND FUEL VALUES, PLUS LITTER AND DUFF
C     IF THEY ARE TO BE DIFFERENT THAN THE DEFAULT ONES.
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,2104) KEYWRD
 2104    FORMAT(/1X,A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >      ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      NPARMS = 12
      DO I = 1,NPARMS
        PRMS(I) = -1.0
        IF (LNOTBK(I)) PRMS(I)= ARRAY(I)
      ENDDO
      
      IDT = 1
      IF(LKECHO)WRITE(JOSTND,2110) KEYWRD,(PRMS(I),I=1,12)
 2110 FORMAT(/1X,A8,'   INITIAL HARD FUEL VALUES (TONNES/HA) ARE',
     >' (-1.0=NO VALUE SPECIFIED): FUELS <2.5cm=',F5.1,
     >'; FUELS 2.5-7.6cm=',F5.1/T13,'FUELS 7.6-15.2cm=',F5.1,
     >'; FUELS 15.2-30.5cm=',F5.1,'; FUELS 30.5-50.8cm=',F5.1,'; LITT=',
     >F5.1,'; DUFF=',F5.1/T13,'FUELS <.6cm=',F5.1,
     >'; FUELS .6-2.5cm=',F5.1/T13,'FUELS 50.8-89.0cm=',F5.1,
     >'; FUELS 89.0-127.1cm=',F5.1,'; FUELS >127.1cm=',F5.1)

      DO 2115 I= 1,NPARMS
	   IF (PRMS(I) .GT. 0.0) PRMS(I) = PRMS(I) * TMtoTI / HAtoACR
 2115 CONTINUE

      MYACT = 2521
      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10
 2200 CONTINUE
C                        OPTION NUMBER 22 -- SNAGINIT
C
C     THIS KEYWORD ALLOWS USERS TO ADD ADDITIONAL SNAGS TO ONE OR MORE STANDS
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,2204) KEYWRD
 2204    FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >      ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      IDT = 1
      NPARMS= 6
      PRMS(1) = -1.0
      PRMS(2) = -1.0
      PRMS(3) = -1.0
      PRMS(4) = -1.0
      PRMS(5) = -1.0
      PRMS(6) = -1.0

      JSP = 0
      CALL SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &             ARRAY,KARD)
      IF (JSP.EQ.0 .OR. JSP.EQ.-999) GOTO 10
      PRMS(1)= JSP
      IF (LNOTBK(2)) PRMS(2)= ARRAY(2)
      IF (LNOTBK(3)) PRMS(3)= ARRAY(3)
      IF (LNOTBK(4)) PRMS(4)= ARRAY(4)
      IF (LNOTBK(5)) PRMS(5)= ARRAY(5)
      IF (LNOTBK(6)) PRMS(6)= ARRAY(6)

      IF(LKECHO)WRITE(JOSTND,2210) KEYWRD,KARD(1)(1:3),(PRMS(I),I=2,6)
 2210 FORMAT(/1X,A8,'   INITIAL SNAG CHARACTERISTICS ',
     >      '(-1.0=NO VALUE SPECIFIED): SPECIES: ',A,
     &       '; DBH AT DEATH (CM): ',F5.1,
     &       /T13,'HEIGHT AT DEATH (M): ',F5.1,'; CURRENT HEIGHT: ',
     &       F5.1,'; AGE: ',F4.0,'; DENSITY (STEMS/HA): ',F6.1)

      IF (PRMS(2) .GT. 0.0) PRMS(2) = PRMS(2) * CMtoIN
      IF (PRMS(3) .GT. 0.0) PRMS(3) = PRMS(3) * MtoFT
      IF (PRMS(4) .GT. 0.0) PRMS(4) = PRMS(4) * MtoFT
      IF (PRMS(6) .GT. 0.0) PRMS(6) = PRMS(6) / HAtoACR

      MYACT = 2522
      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10
 2300 CONTINUE
C                        OPTION NUMBER 23 -- PILEBURN (formerly FUELBURN)
C
C     SET UP JACKPOT AND PILE BURNS.
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,2304) KEYWRD
 2304    FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >      ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      MYACT = 2523
      IDT = 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))
      IF (IPRMPT.GT.0) THEN
         IF (IPRMPT.NE.2) THEN
            CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
            CALL ERRGRO (.TRUE.,25)
         ELSE
            CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >                   IPRMPT,IRECNT,ICYC)
            CALL fvsGetRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         ENDIF
         GOTO 10
      ENDIF

      NPARMS= 5
      PRMS(1) = 1.0
      IF (LNOTBK(2)) PRMS(1)= ARRAY(2)

c     Now that we know what type of fuel burning it is, let's set the
c     defaults for that type.

      IF (PRMS(1) .EQ. 1.0) THEN
         PRMS(2) = 70.0
         PRMS(3) = 10.0
         PRMS(4) = 80.0
         PRMS(5) = 0.0
      ELSE
         PRMS(2) = 100.0
         PRMS(3) = 30.0
         PRMS(4) = 60.0
         PRMS(5) = 0.0
      ENDIF

      IF (LNOTBK(3)) PRMS(2)= ARRAY(3)
      IF (LNOTBK(4)) PRMS(3)= ARRAY(4)
      IF (LNOTBK(5)) PRMS(4)= ARRAY(5)
      IF (LNOTBK(6)) PRMS(5)= ARRAY(6)

      IF (PRMS(1) .EQ. 1.0) THEN
         IF(LKECHO)WRITE(JOSTND,2310) KEYWRD,IDT,'PILE'
 2310    FORMAT(/1X,A8,'   IN DATE/CYCLE ',I4,' A ',A,' BURN WILL',
     &          ' OCCUR.')
      ELSE
         IF(LKECHO)WRITE(JOSTND,2310) KEYWRD,IDT,'JACKPOT'
      ENDIF

      IF(LKECHO)WRITE(JOSTND,2312) (PRMS(I),I=2,5)
 2312 FORMAT(T13,
     &   'PERCENT OF THE STAND AREA FROM WHICH FUEL IS COLLECTED ',
     &   '(AFFECTED AREA):',T86,F4.0/T13,
     &   'PERCENT OF THE AFFECTED AREA WHERE THE FUEL IS ',
     &   'CONCENTRATED:',T86,F4.0/T13,
     &   'PERCENT OF THE FUEL FROM THE AFFECTED AREA THAT IS ',
     &   'COLLECTED:',T86,F4.0/T13,'PERCENT OF THE TREES IN THE ',
     &   'STAND WILL DIE AS A RESULT OF THE FIRE:',T86,F4.0)

      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10
 2400 CONTINUE
C                        OPTION NUMBER 24 -- SNAGPBN
C
C     CHANGE THE PARAMETERS WHICH SET THE RATE AT WHICH SNAGS FALL
C     AFTER A BURN.
C

      IF (LNOTBK(1)) PBSOFT = ARRAY(1)
      IF (LNOTBK(2)) PBSMAL = ARRAY(2)
      IF (LNOTBK(3)) PBTIME = ARRAY(3)
      IF (LNOTBK(4)) PBSIZE = ARRAY(4) * CMtoIN
      IF (LNOTBK(5)) PBSCOR = ARRAY(5) * MtoFT

      IF (PBSOFT .LT. 0.0) PBSOFT = 0.0
      IF (PBSOFT .GT. 1.0) PBSOFT = 1.0
      IF (PBSMAL .LT. 0.0) PBSMAL = 0.0
      IF (PBSMAL .GT. 1.0) PBSMAL = 1.0
      IF (PBTIME .LT. 1.0) PBTIME = 1.0
      IF (PBSIZE .LT. 0.0) PBSIZE = 0.0
      IF (PBSCOR .LT. 0.0) PBSCOR = 0.0

      IF(LKECHO)WRITE(JOSTND,2420) KEYWRD,PBSOFT,PBSMAL,PBTIME,
     >                    PBSIZE*INtoCM,PBSCOR*FTtoM
 2420 FORMAT(/1X,A8,'   THE PROPORTIONS OF SOFT OR SMALL SNAGS TO ',
     >       'FALL AFTER A BURN ARE (RESPECTIVELY): ',F4.2,F5.2,
     >       /T13,'THE NUMBER OF YEARS IN WHICH THEY FALL IS: ',F4.0,
     >       ' SMALL SNAGS ARE THOSE LESS THAN: ',F4.0,' CM DBH.',
     >       /T13,'THE THRESHOLD SCORCH HEIGHT FOR THE POST-BURN ',
     >       'FALLING IS: ',F5.1,' M.')

      GOTO 10
 2500 CONTINUE
C                        OPTION NUMBER 25 -- FUELTRET
C
C     SET THE TYPE OF FUEL TREATMENT AND HARVEST TYPE THAT WAS DONE.  NOTE
C     THAT THIS KEYWORD WILL ONLY AFFECT FIRE INTENSITY AND ONLY WITHIN 5
C     YEARS AFTER A STAND ENTRY.
C

      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,2504) KEYWRD
 2504    FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >      ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      MYACT = 2525
      IDT = 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IF (IPRMPT.GT.0) THEN
         IF (IPRMPT.NE.2) THEN
            CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
            CALL ERRGRO (.TRUE.,25)
         ELSE
            CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >                   IPRMPT,IRECNT,ICYC)
            CALL fvsGetRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         ENDIF
         GOTO 10
      ENDIF

      NPARMS= 3
      PRMS(1) = 0.0
      PRMS(2) = 1.0
      PRMS(3) = -1.0

      IF (LNOTBK(2)) PRMS(1)= ARRAY(2)
      IF (LNOTBK(3)) PRMS(2)= ARRAY(3)
      IF (LNOTBK(4)) PRMS(3)= ARRAY(4)

      IF (PRMS(1) .LT. 0.0) PRMS(1) = 0.0
      IF (PRMS(1) .GT. 2.0) PRMS(1) = 2.0
      IF (PRMS(2) .LT. 1.0) PRMS(2) = 1.0
      IF (PRMS(2) .GT. 3.0) PRMS(2) = 3.0

      IF(LKECHO)WRITE(JOSTND,2510) KEYWRD,IDT, INT(PRMS(1)),
     >   INT(PRMS(2))
 2510 FORMAT(/1X,A8,'   IN DATE/CYCLE ',I4,' FUEL TREATMENT TYPE IS ',
     &  I2,' AND HARVEST TYPE ',I2,' WAS USED FOR THE STAND',
     &  ' ENTRY.')

      IF((PRMS(3) .GE. 0.0).AND.LKECHO)WRITE(JOSTND,2511) PRMS(3)
 2511 FORMAT(T13,'MULTIPLIER FOR FUEL DEPTH WILL BE ',F5.1)

      MYACT = 2525
      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10

 2600 CONTINUE
C                        OPTION NUMBER 26 -- STATFUEL
C
C     DISABLE THE DYNAMIC FUEL MODEL FOR THE ENTIRE LANDSCAPE
C
      LDYNFM = .FALSE.
      IF(LKECHO)WRITE(JOSTND,2610) KEYWRD
 2610 FORMAT(/1X,A8,'   THE DYNAMIC FUEL MODEL IS DISABLED. ')

      GOTO 10
 2700 CONTINUE
C
C                        OPTION NUMBER 27 -- FUELREPT
C
C     PRODUCE THE FUEL CONSUMPTION AND PHYSICAL EFFECTS REPORT. RETREIVE
C     THE ID NUMBER FOR THE REPORT.
C
      IF (IDFUL .EQ. 0) CALL GETID (IDFUL)
C
      IDT = 1
      IF (LNOTBK(1)) IDT = IFIX(ARRAY(1))
      PRMS(1) = 200.0
      IF (LNOTBK(2) .AND. ARRAY(2) .GT. 0.0) PRMS(1) = ARRAY(2)
C
      IF(LKECHO)WRITE(JOSTND,2705) KEYWRD,IDT,IFIX(PRMS(1))
 2705 FORMAT(/1X,A8,'   THE FUEL CONSUMPTION AND PHYSICAL EFFECTS',
     &      ' REPORT WILL BE WRITTEN WHEN A FIRE OCCURS.'
     &       /T13,'REPORTING WILL BEGIN IN DATE/CYCLE ',I4,
     &      ' AND WILL CONTINUE FOR',I4,' YEARS.')
C
      NPRMS = 1
      MYACT = 2527
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
      GOTO 10
C
 2800 CONTINUE
C
C                        OPTION NUMBER 28 -- MORTREPT
C
C     PRODUCE THE TREE MORTALITY REPORT. RETREIVE THE ID NUMBER
C     FOR THE REPORT.
C
      IF (IDMRT .EQ. 0) CALL GETID (IDMRT)
C
      IDT = 1
      IF (LNOTBK(1)) IDT = IFIX(ARRAY(1))
      PRMS(1) = 200.0
      IF (LNOTBK(2) .AND. ARRAY(2) .GT. 0.0) PRMS(1) = ARRAY(2)
C
      IF(LKECHO)WRITE(JOSTND,2805) KEYWRD,IDT,IFIX(PRMS(1))
 2805 FORMAT(/1X,A8,'   THE TREE MORTALITY REPORT WILL BE WRITTEN',
     &      ' WHEN A FIRE OCCURS.'
     &       /T13,'REPORTING WILL BEGIN IN DATE/CYCLE ',I4,
     &       ' AND WILL CONTINUE FOR',I4,' YEARS.')
C
      NPRMS = 1
      MYACT = 2528
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
      GOTO 10
 2900 CONTINUE
C                        OPTION NUMBER 29 -- FUELMULT
C
C     CHANGE THE TOTAL DECAY RATE OF FUELS USING A MULTIPLIER
C
      DO 2910 IDEC = 1,4
         IF (LNOTBK(IDEC)) THEN
            DKMULT = ARRAY(IDEC)
            DO 2920 I=1,11
               DKR(I,IDEC) = DKR(I,IDEC) * DKMULT
               IF (DKR(I,IDEC) .GT. 1.0) DKR(I,IDEC) = 1.0
               IF (I .LE. 10) TODUFF(I,IDEC) = DKR(I,IDEC) * PRDUFF(I)
 2920       CONTINUE
         ELSE
            ARRAY(IDEC) = 1.0
         ENDIF
 2910 CONTINUE

      IF(LKECHO)WRITE(JOSTND,2950) KEYWRD, (ARRAY(IDEC),IDEC=1,4)
 2950 FORMAT (/1X,A8,'   THE MULTIPLIERS APPLIED TO THE TOTAL DECAY ',
     &     'RATES FOR DECAY RATE CLASS 1-4 ARE ',3(F5.3,",",2X),F5.3)

      GOTO 10
 3000 CONTINUE
C                        OPTION NUMBER 30 -- POTFMOIS
C
C     CHANGE THE WIND AND MOISTURE CONDITIONS FOR THE POTENTIAL FLAME REPORT
C
      IFIRE= 0
      IF (LNOTBK(1)) IFIRE = ARRAY(1)
      IF (IFIRE .GT. 2) IFIRE = 2
      IF (IFIRE .LT. 1) IFIRE = 1
      PRESVL(IFIRE,1) = 1

C     GET THE DEFAULTS.

      I=1
      IF (IFIRE.EQ.2) I=3
      CALL FMMOIS(I,MOIS)

C     LOAD THE VALUES CHANGED BY THE USER
C     IF THE LIVE HERB MOISTURE IS LEFT BLANK, USE THE LIVE WOODY 
C     MOISTURE.  THIS WAS ASSUMED SO THAT NO ONES RESULTS WOULD CHANGE
C     (THEY WOULDN'T HAVE TO REDO KEYWORD FILES) WHEN THE NEW MOISTURE
C     CATEGORY WAS ADDED. SAR

      DO IARRY=2,8
         IF (LNOTBK(IARRY)) THEN
            PRESVL(IFIRE,IARRY) = ARRAY(IARRY)*.01
         ELSE
            IF (IARRY.LT.7) THEN
               PRESVL(IFIRE,IARRY) = MOIS(1,IARRY-1)
            ELSEIF (IARRY .EQ. 7) THEN
               PRESVL(IFIRE,IARRY) = MOIS(2,1)
            ELSEIF (IARRY .EQ. 8) THEN
            	 PRESVL(IFIRE,IARRY) = PRESVL(IFIRE,7)
            ENDIF
         ENDIF
      ENDDO

      IF (IFIRE .EQ. 1) THEN
         IF(LKECHO)WRITE(JOSTND,3050) KEYWRD, 'SEVERE',
     &        (PRESVL(IFIRE,IARRY)*100.0,IARRY=2,8)
 3050    FORMAT (/1X,A8,'   FIRE MOISTURE CONDITIONS FOR CALCULATING ',
     &        A,' POTENTIAL FLAME LENGTHS ARE: '
     &       /T13,'% MOISTURE FOR 0-.64cm= ',F4.0,'; 0.64-2.5cm= ',F4.0,
     &        '; 2.5-7.6cm= ',F4.0,'; 7.6+cm= ',F4.0,'; DUFF=',F4.0,
     &        '; LIVE WOODY =',F4.0,'; LIVE HERB =',F4.0)
       ELSE
         IF(LKECHO)WRITE(JOSTND,3050) KEYWRD, 'MODERATE',
     &        (PRESVL(IFIRE,IARRY)*100.0,IARRY=2,8)
      ENDIF

      GOTO 10
 3100 CONTINUE
C
C                        OPTION NUMBER 31 -- SNAGSUM
C
C     REQUEST A SNAG REPORT SUMMARY
C
      ISNGSM = -1
      IF (ARRAY(1).GE. 0.) ISNGSM = 0
      IF (ISNGSM.GE.0) THEN
         IF(LKECHO)WRITE(JOSTND,3110) KEYWRD,'REQUESTED'
 3110    FORMAT (/1X,A8,'   SNAG SUMMARY REPORT ',A)
      ELSE
         IF(LKECHO)WRITE(JOSTND,3110) KEYWRD,'TURNED OFF'
      ENDIF
      GOTO 10
 3200 CONTINUE
C
C                        OPTION NUMBER 32 -- MORTCLAS
C
C     SET-UP THE SIZE CLASSES FOR REPORTING FIRE-BASED MORTALITY
C
      DO 3210 IARRY=1,7
         IF (LNOTBK(IARRY)) LOWDBH(IARRY)=ARRAY(IARRY) * CMtoIN
 3210 CONTINUE

      IF(LKECHO)WRITE(JOSTND,3250) KEYWRD,
     >  (IARRY, LOWDBH(IARRY)*INtoCM,IARRY=1,7)
 3250 FORMAT (/1X,A8,'   THE LOWER BOUND OF EACH SIZE CLASS USED ',
     &  'IN THE MORTALITY REPORT IS:',
     &  /T13,7('CLASS',I2,'=',F4.1:'; '))

      GOTO 10
 3300 CONTINUE
C
C                        OPTION NUMBER 33 -- DROUGHT
C
C     DEFINE THE YEARS IN WHICH A DROUGHT OR DORMANT YEAR OCCURS
C
      IF (ICALL .EQ. 2) THEN
        WRITE(JOSTND,3304) KEYWRD
 3304   FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     &     ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
        GOTO 10
      ENDIF

      MYACT = 2529
      IDT = 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IF (IPRMPT.GT.0) THEN
        IF (IPRMPT.NE.2) THEN
          CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
          CALL ERRGRO (.TRUE.,25)
        ELSE
          CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >                 IPRMPT,IRECNT,ICYC)
          CALL fvsGetRtnCode(IRTNCD)
          IF (IRTNCD.NE.0) RETURN
        ENDIF
        GOTO 10
      ENDIF

      NPARMS= 1
      PRMS(1) = 1.0
      IF (LNOTBK(2) .AND. ARRAY(2) .GT. 0.0) PRMS(1) = ARRAY(2)

      IF(LKECHO)WRITE(JOSTND,3305) KEYWRD,IDT,INT(PRMS(1))
 3305 FORMAT(/1X,A8,'   IN DATE/CYCLE ',I4,' DROUGHT/DORMANCY IS',
     &       ' SIMULATED FOR ', I3, ' YEARS.')

      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10
 3400 CONTINUE
C
C                        OPTION NUMBER 34 -- FUELMOVE
C
C     TRANSFER FUELS FROM ONE SIZE CATEGORY TO ANOTHER. TRANSFERS
C     MAY INCLUDE LARGER TO SMALLER, SMALLER TO LARGER. IMPORTING
C     AND EXPORTING MAY OCCUR. NOTE THAT THE SEQUENCE OF CATEGORIES
C     IS NOT ORDERED SMALLEST TO LARGEST: LITTER AND DUFF ARE IN THE
C     HIGHEST POSITIONS
C
      IF (ICALL .EQ. 2) THEN
        WRITE(JOSTND,3404) KEYWRD
 3404   FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     &     ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
        GOTO 10
      ENDIF

      MYACT = 2530
      IDT = 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IF (IPRMPT.GT.0) THEN
        IF (IPRMPT.NE.2) THEN
          CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
          CALL ERRGRO (.TRUE.,25)
        ELSE
          CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >                 IPRMPT,IRECNT,ICYC)
          CALL fvsGetRtnCode(IRTNCD)
          IF (IRTNCD.NE.0) RETURN
        ENDIF
        GOTO 10
      ENDIF

      NPARMS = 6
      II = 0
C
C     *SOURCE* CWD POOL - MUST HAVE A VALID ENTRY (0,1-11)
C     DEFAULT 6 (>12")
C       0 = IMPORTED (EXPORTED IN *TARGET* CONTEXT)
C       1 = <0.25" (<0.64 CM)
C       2 =  0.25 TO   1"  ( 0.64 TO  2.5 CM)
C       3 =  1    TO   3"  ( 2.5  TO  7.6 CM)
C       4 =  3    TO   6"  ( 7.6  TO 15.2 CM)
C       5 =  6    TO  12"  (15.2  TO 30.5 CM)
C       6 =  12   TO  20"
C       7 =  20   TO  35"
C       8 =  35   TO  50"
C       9 =  > 50"
C       10 = LITTER
C       11 = DUFF
C
      PRMS(1) = 6.0
      IF (LNOTBK(2)) THEN
        IF ((ARRAY(2) .LT. 0.0) .OR. (ARRAY(2) .GT. 11.0)) THEN
          II = II + 1
        ELSE
          PRMS(1) = ARRAY(2)
        ENDIF
      ENDIF
C
C     *TARGET* FUEL POOL (SAME CATEGORIES AS SOURCE); MUST HAVE A
C     VALID ENTRY (0,1-11), DEFAULT 11 (DUFF)
C
      PRMS(2) = 11.0
      IF (LNOTBK(3)) THEN
        IF ((ARRAY(2) .LT. 0.0) .OR. (ARRAY(2) .GT. 11.0)) THEN
           II = II + 1
        ELSE
          PRMS(2) = ARRAY(3)
        ENDIF
      ENDIF
C
C     INVALID TO HAVE SAME SOURCE AND TARGET POOL
C
      IF (INT(PRMS(1)) .EQ. INT(PRMS(2))) II = II + 1
C
C     AMOUNT TO MOVE (TONNES/HA) - MUST BE >=0
C
      PRMS(3) = 0.0
      IF (LNOTBK(4)) THEN
        IF(ARRAY(4) .GE. 0.0) THEN
          PRMS(3) = ARRAY(4) * TMtoTI / HAtoACR
        ELSE
          II = II + 1
        ENDIF
      ENDIF
C
C     PROPORTION TO MOVE  MUST BE 0-1
C
      PRMS(4) = 0.0
      IF (LNOTBK(5)) THEN
        IF (ARRAY(5) .GE. 0.0 .AND. ARRAY(5) .LE. 1.0) THEN
          PRMS(4) = ARRAY(5)
        ELSE
          II = II + 1
        ENDIF
      ENDIF
C
C     RESIDUAL AMOUNT (TONNES/HA) - MUST BE >=0
C
      PRMS(5) = 9999.0
      IF (LNOTBK(6)) THEN
        IF (ARRAY(6) .GE. 0.0) THEN
          PRMS(5) = ARRAY(6) * TMtoTI / HAtoACR
        ELSE
          II = II + 1
        ENDIF
      ENDIF
C
C     TARGET AMOUNT (TONNES/HA): UNSET= 0.0; ELSE >=0
C
      PRMS(6) = 0.0
      IF (LNOTBK(7)) THEN
        IF (ARRAY(7) .GE. 0.0) THEN
          PRMS(6) = ARRAY(7) * TMtoTI / HAtoACR
        ELSE
          II = II + 1
        ENDIF
      ENDIF

      IF (II .EQ. 0) THEN
        IF(LKECHO)WRITE(JOSTND,3405) KEYWRD,IDT,INT(PRMS(1)),
     >    INT(PRMS(2))
 3405   FORMAT(/1X,A8,'   IN DATE/CYCLE ',I4,' FUEL IN SIZE ',
     &    'CATEGORY',I3,' WILL BE MOVED TO',/1X,T13,'SIZE CATEGORY ',
     &    I2,'. WHICHEVER OF THE FOLLOWING 4 CRITERIA',/1X,T13,
     &    'MOVES THE MOST FUEL, WILL BE USED:')
        IF(LKECHO)WRITE(JOSTND, 3406) PRMS(3) * TItoTM / ACRtoHA
        IF(LKECHO)WRITE(JOSTND, 3407) PRMS(4)
        IF(LKECHO)WRITE(JOSTND, 3408) PRMS(5) * TItoTM / ACRtoHA
        IF(LKECHO)WRITE(JOSTND, 3409) PRMS(6) * TItoTM / ACRtoHA
 3406   FORMAT(T13,'TONNES/HA MOVED FROM SOURCE CATEGORY:    ',F12.3)
 3407   FORMAT(T13,'PROPORTION MOVED FROM SOURCE CATEGORY:   ',F12.3)
 3408   FORMAT(T13,'TONNES/HA REMAINING IN SOURCE CATEGORY:  ',F12.3)
 3409   FORMAT(T13,'FINAL TONNES/HA IN TARGET CATEGORY:      ',F12.3)
        CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      ELSE
        CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
        CALL ERRGRO (.TRUE.,4)
      ENDIF

      GOTO 10
 3500 CONTINUE
C
C                        OPTION NUMBER 35 -- POTFWIND
C
      IF (LNOTBK(1)) PREWND(1)=ARRAY(1) * KMtoMI
      IF (LNOTBK(2)) PREWND(2)=ARRAY(2) * KMtoMI

      IF(LKECHO)WRITE(JOSTND,3550) KEYWRD, PREWND * MItoKM
 3550 FORMAT (/1X,A8,'   FIRE WIND SPEEDS USED FOR CALCULATING ',
     &'POTENTIAL FLAME LENGTHS ARE',/T12,' FOR SEVERE FIRE: ',F5.0,
     &     ' AND FOR MODERATE FIRE: ',F5.0,' KM/H')

      GOTO 10
 3600 CONTINUE
C
C                        OPTION NUMBER 36 -- POTFTEMP
C
      IF (LNOTBK(1)) POTEMP(1)=ARRAY(1) * CtoF1 + CtoF2
      IF (LNOTBK(2)) POTEMP(2)=ARRAY(2) * CtoF1 + CtoF2

      IF(LKECHO)WRITE(JOSTND,3650) KEYWRD, POTEMP * FtoC1 + FtoC2
 3650 FORMAT (/1X,A8,'   FIRE TEMPERATURES USED FOR CALCULATING ',
     &'POTENTIAL FLAME LENGTHS ARE',/T12,' FOR SEVERE FIRE: ',F5.0,
     &     ' AND FOR MODERATE FIRE: ',F5.0,' DEGREES C')

      GOTO 10
 3700 CONTINUE
C                        OPTION NUMBER 37 -- SNAGPSFT
C
C     SET THE PROPORTION OF EACH SPECIES THAT IS INITIALLY SOFT
C        NOTE THE TWO CHANGES: THIS USED TO BE PART OF THE SNAGBRK KEYWORD
C        AND THIS DID NOT USE TO VARY BY SPECIES.
C
      JSP = 0
      CALL SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &             ARRAY,KARD)
      IF (JSP .EQ. -999) GOTO 10
      IF (JSP .NE. 0 ) THEN
        IF (LNOTBK(2)) THEN
          PSOFT(JSP) = ARRAY(2)
          IF (PSOFT(JSP) .LT. 0.0) PSOFT(JSP) = 0.0
          IF (PSOFT(JSP) .GT. 1.0) PSOFT(JSP) = 1.0
        ENDIF
      ELSE
        DO JSP = 1,MAXSP
          IF (LNOTBK(2)) PSOFT(JSP) = ARRAY(2)
          IF (PSOFT(JSP) .LT. 0.0) PSOFT(JSP) = 0.0
          IF (PSOFT(JSP) .GT. 1.0) PSOFT(JSP) = 1.0
        ENDDO
        JSP=1
      ENDIF

      IF(LKECHO)WRITE(JOSTND,3705) KEYWRD, KARD(1)(1:3), PSOFT(JSP)
 3705 FORMAT(/1X,A8,'   FOR SPECIES ',A,
     >  ', THE PROPORTION OF SNAGS THAT ARE SOFT AT ',
     >  'THE TIME THEY ARE CREATED:',F4.2)

      GOTO 10

 3800 CONTINUE
C
C                        OPTION NUMBER 38 -- FUELMODL
C
C     SET THE FUEL MODELS TO BE USED INSTEAD OF THE MODEL AUTOMATICALLY
C     DETERMINING THE APPROPRIATE ONES (ADDED FEB 2002)
C
      MYACT  = 2538
      NPARMS = 8
      IDT    = 1

      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IF (IPRMPT.GT.0) THEN
         IF (IPRMPT.NE.2) THEN
            CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
            CALL ERRGRO (.TRUE.,25)
         ELSE
            CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >           IPRMPT,IRECNT,ICYC)
            CALL fvsGetRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         ENDIF
         GOTO 10
      ENDIF

      NPARMS=0
      DO I=1,6
         PRMS(I)=ARRAY(I+1)
      ENDDO
      READ(IREAD,'(2F10.0)',ERR=3801) PRMS(7),PRMS(8)
      IRECNT = IRECNT + 1

C     FIND OUT HOW MANY VALID FUEL MODELS YOU HAVE.

      DO I=1,7,2
         IF (PRMS(I).GT.0. .AND. PRMS(I).LE.MXDFMD) THEN
            NPARMS=NPARMS+2
            IF (PRMS(I+1).LE.0.) PRMS(I+1)=1.
         ELSE
            EXIT
         ENDIF
      ENDDO

      IF (NPARMS.EQ.0) THEN
         IF(LKECHO)WRITE(JOSTND,3814) KEYWRD, IDT
 3814    FORMAT(/1X,A8,'   IN DATE/CYCLE ',I4,' AUTOMATIC FUEL ',
     &        'MODEL SELECTION WILL BE USED.',/1X)
      ELSE

C        SCALE THE WEIGHTS SO THAT THEY ADD UP TO 1

         X=0.
         DO I=2,NPARMS,2
            X=X+PRMS(I)
         ENDDO
         X=1./X
         DO I=2,NPARMS,2
            PRMS(I)=PRMS(I)*X
         ENDDO
         IF(LKECHO)WRITE(JOSTND,3815) KEYWRD, IDT
 3815    FORMAT(/1X,A8,'   IN DATE/CYCLE ',I4,' THE FUEL MODELS AND ',
     &        'WEIGHTS THAT WILL BE USED ARE:')
         DO I = 1,NPARMS,2
            IF(LKECHO)WRITE(JOSTND,3816) INT(PRMS(I)), 100.0*PRMS(I+1)
 3816       FORMAT(T13,'MODEL ',I3,': ',F6.1,'%')
         ENDDO

      ENDIF

      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)

      GOTO 10

 3801 CONTINUE
      CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
      CALL ERRGRO (.TRUE.,4)
      GOTO 10
 3900 CONTINUE
C
C                        OPTION NUMBER 39 -- DEFULMOD
C
C     DEFINE NEW OR ALTER EXISTING FUEL MODELS (ADDED MARCH 2002)
C
      MYACT = 2539
      IDT = 1
      IF (LNOTBK(1)) THEN
        I = IFIX(ARRAY(1))
        IF (I .GE. 0) IDT = I
      ENDIF

      IF (IPRMPT.GT.0) THEN
        IF (IPRMPT.NE.2) THEN
          CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
          CALL ERRGRO (.TRUE.,25)
        ELSE
          CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >                 IPRMPT,IRECNT,ICYC)
          CALL fvsGetRtnCode(IRTNCD)
          IF (IRTNCD.NE.0) RETURN
        ENDIF
        GOTO 10
      ENDIF

      NPARMS = 13
      DO I = 1,NPARMS
        PRMS(I) = -1.0
      ENDDO
C
C     READ VALUES FROM THE SUPPLEMENTAL RECORD
C     READ AS CHARACTER STRINGS SO THAT THE -1 CAN BE USED
C     TO SHOW THAT NO VALUE WAS ENTERED.
C
      IRECNT = IRECNT + 1
      READ(IREAD,'(7A10)',ERR=3901) (APRMS(I),I = 7,13)
      DO I = 7,13
        IF (APRMS(I) .NE. ' ')
     >    READ(APRMS(I),'(F10.0)',ERR=3901) PRMS(I)
      ENDDO
      GOTO 3903

 3901 CONTINUE
      CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
      CALL ERRGRO (.TRUE.,4)
      GOTO 10
C
C     CHECK FOR VALID FUEL MODEL NUMBER
C
 3903 LOK = .TRUE.
      IF (LNOTBK(2)) THEN
        IFMD = INT(ARRAY(2))
        IF (IFMD .GE. 1 .AND. IFMD .LE. MXDFMD) THEN
          PRMS(1) = FLOAT(IFMD)
        ELSE
          LOK = .FALSE.
        ENDIF
      ENDIF
C
C     IF THE MODEL NUMBER IS VALID, READ THE REMAINING 10 PARAMETERS
C
      IF (LOK) THEN

        DO I = 2,4
          IF (LNOTBK(I+1)) THEN
            PRMS(I) = ARRAY(I+1) / MtoFT
          ELSE
            PRMS(I) = SURFVL(IFMD,1,I-1)
          ENDIF
        ENDDO

        I = 5
        IF (LNOTBK(I+1)) THEN
          PRMS(I) = ARRAY(I+1) / MtoFT
        ELSE
          PRMS(I) = SURFVL(IFMD,2,1)
        ENDIF

        I = 6
        IF (LNOTBK(I+1)) THEN
          PRMS(I) = ARRAY(I+1) * KGtoLB/M2toFT2
        ELSE
          PRMS(I) = FMLOAD(IFMD,1,1)
        ENDIF

        DO I = 7,8
          IF (PRMS(I) .LT. 0.0) THEN
            PRMS(I) = FMLOAD(IFMD,1,I-5)
          ELSE
            PRMS(I) = PRMS(I) * KGtoLB/M2toFT2
          ENDIF
        ENDDO

        I = 9
        IF (PRMS(I) .LT. 0.0) THEN
          PRMS(I) = FMLOAD(IFMD,2,1)
        ELSE
          PRMS(I) = PRMS(I) * KGtoLB/M2toFT2
        ENDIF

        I = 10
        IF (PRMS(I) .LT. 0.0) THEN
          PRMS(I) = FMDEP(IFMD)
        ELSE
          PRMS(I) = PRMS(I) * MtoFT
        ENDIF

        I = 11
        IF (PRMS(I) .LT. 0.0) THEN
          PRMS(I) = MOISEX(IFMD)
        ENDIF

        I = 12
        IF (PRMS(I) .LT. 0.0) THEN
          PRMS(I) = SURFVL(IFMD,2,2)
        ELSE
          PRMS(I) = PRMS(I) / MtoFT
        ENDIF

        I = 13
        IF (PRMS(I) .LT. 0.0) THEN
        	PRMS(I) = FMLOAD(IFMD,2,2)
        ELSE
        	PRMS(I) = PRMS(I) * KGtoLB/M2toFT2
        ENDIF 

      ENDIF
C
C     CHECK FOR VALID (>=0) S/V RATIO
C
      DO I = 2,5
        IF (PRMS(I) .LT. 0.0) THEN
          LOK = .FALSE.
          EXIT
        ENDIF
      ENDDO
C
C     CHECK FOR NON-ZERO FUELS
C
      IF (LOK) THEN
        XSUM = 0.0
        DO I = 6,9
          XSUM = XSUM + PRMS(I)
        ENDDO
        XSUM = XSUM + PRMS(13)
        IF (XSUM .LT. 0.0001) LOK = .FALSE.
      ENDIF
C
C     CHECK FOR VALID (>=0) FUEL DEPTH
C
      IF (LOK .AND. PRMS(10) .LE. 0.0)  LOK = .FALSE.
C
C     CHECK FOR VALID (>=0) MOISTURE OF EXTINCTION
C
      IF (LOK .AND. PRMS(11) .LT. 0.0 . OR. PRMS(11) .GT. 1.0)
     >  LOK = .FALSE.

      IF (LOK) THEN
        IF(LKECHO)WRITE(JOSTND,3910) KEYWRD,IDT,INT(PRMS(1)),
     &    (PRMS(I) / FTtoM, I=2,5),PRMS(12)/FTtoM,
     &    (PRMS(I) * LBtoKG/FT2toM2, I=6,9),
     &    PRMS(13) * LBtoKG/FT2toM2,
     &    PRMS(10) * FTtoM, PRMS(11)  
 3910   FORMAT(/1X,A8,'   IN DATE/CYCLE ',I4,' THE VALUES FOR FUEL',
     &    ' MODEL', I3,' WILL BE: ',
     &    /T13, 'SURFACE TO VOL RATIO (/M):',
     &    /T15, '   (<0.64cm) = ', F6.0,
     &    /T15, '(0.64-2.5cm) = ', F6.0,
     &    /T15, ' (2.5-7.6cm) = ', F6.0,
     &    /T15, '(LIVE WOODY) = ', F6.0,
     &    /T15, ' (LIVE HERB) = ', F6.0, 
     &    /T13, 'FUEL LOADING (KG/SQ M):',
     &    /T15, '   (<0.64cm) = ', F6.0,
     &    /T15, '(0.64-2.5cm) = ', F6.0,
     &    /T15, ' (2.5-7.6cm) = ', F6.0,
     &    /T15, '(LIVE WOODY) = ', F6.0,
     &    /T15, ' (LIVE HERB) = ', F6.0, 
     &    /T15, '   DEPTH (M) = ', F6.3,
     &    /T13,'MOISTURE OF EXTINCTION =', F7.4)
        CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      ELSE
        WRITE(JOSTND,3911) KEYWRD,IRECNT-1,IRECNT
 3911   FORMAT(/1X,A8,'   ERROR AT CARD ', I4, ' OR CARD ', I4,
     &    ' MAKES IT INVALID. ')
        CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
        CALL ERRGRO (.TRUE.,4)
      ENDIF

      GOTO 10
 4000 CONTINUE
C                        OPTION NUMBER 40 -- CANCALC
C
C     SET OPTIONS FOR CALCULATION OF CANOPY BASE HEIGHT AND CANOPY BULK
C     DENSITY (note: already kg/m3).
C
      IF (ICALL .EQ. 2) THEN
         IF(LKECHO)WRITE(JOSTND,4004) KEYWRD
 4004    FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     &     ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF
C
      IF (LNOTBK(1)) ICBHMT = IFIX(ARRAY(1))
      IF (LNOTBK(2)) CANMHT = ARRAY(2) * MtoFT
      IF (LNOTBK(3)) ICANSP = IFIX(ARRAY(3))
      IF (LNOTBK(4)) CBHCUT = ARRAY(4)
      IF (LNOTBK(5)) FOLMC = ARRAY(5)
C
      IF(LKECHO)WRITE(JOSTND,4010) KEYWRD, ICBHMT, CANMHT * FTtoM,
     >                             ICANSP, CBHCUT, FOLMC
 4010 FORMAT(/1X,A8,T13,'CALCULATION OF CANOPY BASE HEIGHT AND CANOPY ',
     &  'BULK DENSITY WILL USE METHOD ',I1,',',/T13,'TREES ',
     &  'AT LEAST ',F5.1,' M TALL, SPECIES CATEGORY ',I1,
     &  ', A CUTOFF VALUE OF ',F5.1,',',/T13,'AND A FMC OF ',F5.1)
     
C
 4100 CONTINUE
C                        OPTION NUMBER 41 -- POTFSEAS 
      IF (LNOTBK(1)) POTSEAS(1)=ARRAY(1)
      IF (LNOTBK(2)) POTSEAS(2)=ARRAY(2)
      
      IF(LKECHO)WRITE(JOSTND,4150) KEYWRD, POTSEAS
 4150 FORMAT (/1X,A8,'   SEASONS USED FOR CALCULATING POTENTIAL FIRE ',
     &  'BEHAVIOR ARE ',/T12,' FOR SEVERE FIRE: ',I2,
     &  ' AND FOR MODERATE FIRE: ',I2)
      GOTO 10
 4200 CONTINUE
C                        OPTION NUMBER 42 -- POTFPAB
      IF (LNOTBK(1)) POTPAB(1)=ARRAY(1)
      IF (LNOTBK(2)) POTPAB(2)=ARRAY(2)
      
      IF(LKECHO)WRITE(JOSTND,4250) KEYWRD, POTPAB
 4250 FORMAT (/1X,A8,'   % AREA BURNED VALUES USED FOR CALCULATING ',
     &  'POTENTIAL FIRE EFFECTS ARE',/T12,' FOR SEVERE FIRE: ',F5.1,
     &  ' AND FOR MODERATE FIRE: ',F5.1) 
      GOTO 10
 4300 CONTINUE
C
C                        OPTION NUMBER 43 -- SOILHEAT
C
      IF (IDSHEAT .EQ. 0) CALL GETID (IDSHEAT)
C
      IDT = 1
      IF (LNOTBK(1)) IDT = IFIX(ARRAY(1))
      PRMS(1) = 200.0
      IF (LNOTBK(2) .AND. ARRAY(2) .GT. 0.0) PRMS(1) = ARRAY(2)
      PRMS(2) = 3
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))
      IF (PRMS(2) .LT. 1) PRMS(2) = 1
      IF (PRMS(2) .GT. 5) PRMS(2) = 5
C
      IF(LKECHO)WRITE(JOSTND,4315) KEYWRD,IDT,IFIX(PRMS(1)),
     &                             IFIX(PRMS(2))
 4315 FORMAT(/1X,A8,'   SOIL HEATING WILL BE ESTIMATED AND',
     &       ' REPORTED WHEN A FIRE OCCURS.'
     &       /T13,'REPORTING WILL BEGIN IN DATE/CYCLE ',I4,
     &       ' AND WILL CONTINUE FOR',I4,' YEARS.'
     &       /T13,'SOIL TYPE IS SET TO ',I4)
C
      NPRMS = 2
      MYACT = 2503
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
      GOTO 10
 4400 CONTINUE
C
C                        OPTION NUMBER 44 -- CARBREPT
C
      IF (ICALL .EQ. 2) THEN
        WRITE(JOSTND,4401) KEYWRD
 4401   FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >    ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
        GOTO 10
      ENDIF

      IF (IDCRPT .EQ. 0) CALL GETID (IDCRPT)

      IDT     =   1
      PRMS(1) = 200.0
      PRMS(2) =   1.0
      IF (LNOTBK(1)) IDT     = INT(ARRAY(1))
      IF (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))

      IF(LKECHO)WRITE(JOSTND,4410) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2))
 4410 FORMAT(/1X,A8,'   MAIN CARBON REPORT WILL BEGIN PRINTING IN',
     &      ' DATE/CYCLE ',I4,','/T13,'FOR ',I4,
     &      ' YEARS USING ',I2,' YEAR INCREMENTS.')

      NPRMS = 2
      MYACT = 2544
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)

      GOTO 10
 4500 CONTINUE
C
C                        OPTION NUMBER 45 -- CARBCUT
C
      IF (ICALL .EQ. 2) THEN
        WRITE(JOSTND,4501) KEYWRD
 4501   FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >    ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
        GOTO 10
      ENDIF

      IF (IDCHRV .EQ. 0) CALL GETID (IDCHRV)

      IDT     =   1
      PRMS(1) = 200.0
      PRMS(2) =   1.0
      IF (LNOTBK(1)) IDT     = INT(ARRAY(1))
      IF (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))
C
      IF(LKECHO)WRITE(JOSTND,4510) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2))
 4510 FORMAT(/1X,A8,T13,'HARVESTED PRODUCTS REPORT WILL BEGIN',
     &      ' PRINTING IN DATE/CYCLE ',I4,','/T13,'FOR ',I4,
     &      ' YEARS USING ',I2,' YEAR INCREMENTS.')

      NPRMS = 2
      MYACT = 2545
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)

      GOTO 10
 4600 CONTINUE
C
C                        OPTION NUMBER 46 -- CARBCALC
C
      IF (ICALL .EQ. 2) THEN
        WRITE(JOSTND,4601) KEYWRD
 4601   FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >    ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
        GOTO 10
      ENDIF

C     DEFAULTS (*) SET IN FMINIT:
C     FLD1: CARBON METHOD 0 = FFE (*), 1 = JENKINS
C     FLD2: UNITS TYPE 0 = IMPERIAL (*), 1 = METRIC, 2 = METRIC TONS/AC
C     FLD3: ROOT DECAY RATE = 0.0425 (*)
C     FLD4: SOFTWOOD DIAMETER BREAKPOINT =  9.0 IN (*)
C     FLD5: HARDWOOD DIAMETER BREAKPOINT = 11.0 IN (*)

      IF (LNOTBK(1)) ICMETH    = MAX(0,MIN(1,INT(ARRAY(1))))
      IF (LNOTBK(2)) ICMETRC   = MAX(0,MIN(2,INT(ARRAY(2))))
      IF (LNOTBK(3)) CRDCAY    = MAX(0.0,MIN(1.0,ARRAY(3)))
      IF (LNOTBK(4)) CDBRK(1)  = MAX(0.0,MIN(999.0,ARRAY(4)))*CMtoIN
      IF (LNOTBK(5)) CDBRK(2)  = MAX(0.0,MIN(999.0,ARRAY(5)))*CMtoIN
C
      IF(LKECHO)WRITE(JOSTND,4610) KEYWRD,ICMETH,ICMETRC,
     >  CRDCAY,CDBRK(1)*INtoCM,CDBRK(2)*INtoCM

 4610 FORMAT(/1X,A8,T13,'CARBON REPORTS WILL BE BASED ON METHOD',
     >      I2, ' (0=JENKINS, 1=FFE)',/T13, 'REPORT UNITS WILL BE',
     >      I2, ' (0=IMPERIAL, 1=METRIC, 2 = METRIC TONS/ACRE)',/T13,
     >      'PROPORTION OF DEAD ROOTS DECAYING ANNUALLY WILL BE: ',
     >      F7.4,' (<0 = NO DEAD ROOTS)',/T13,
     >      'SOFTWOOD DIAMETER BREAKPOINT: ', F5.1,/T13,
     >      'HARDWOOD DIAMETER BREAKPOINT: ', F5.1)

      GOTO 10
 4700 CONTINUE
C                        OPTION NUMBER 47 -- CANFPROF
      IF (ICALL .EQ. 2) THEN
        WRITE(JOSTND,4701) KEYWRD
 4701   FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >    ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
        GOTO 10
      ENDIF

      ICANPR = 1
      CALL DBSFMLINK(ICANPR)
      IDT = 1
      PRMS(1) = 200
      PRMS(2) = 1.0

      IF (LNOTBK(1)) IDT = INT(ARRAY(1))
      IF (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))

      IF(LKECHO)WRITE(JOSTND,4710) KEYWRD, IDT, INT(PRMS(1)),
     &                            INT(PRMS(2))
 4710 FORMAT(/1X,A8,'   THE CANOPY FUELS PROFILE TABLE WILL BE ',
     &      'OUTPUT STARTING IN DATE/CYCLE ',I4,','/T13,'FOR ',I4,
     &      ' YEARS USING ',I2,' YEAR INCREMENTS.')

      MYACT = 2547
      NPARMS=2
      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10
 4800 CONTINUE
C                        OPTION NUMBER 48 -- FUELFOTO
C
C     THIS KEYWORD SETS THE INITIAL FUEL VALUES USING FUELS PHOTO
C     SERIES INFORMATION.
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,4804) KEYWRD
 4804    FORMAT(/1X,A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >      ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      NPARMS= 2
      PRMS(1) = -1.0
      PRMS(2) = -1.0

      IDT = 1
      IF (LNOTBK(1)) PRMS(1)= NINT(ARRAY(1))
      IF (LNOTBK(2)) PRMS(2)= NINT(ARRAY(2))

C     THE PHOTO REFERENCE CODE MUST BE BETWEEN 1 AND 32.
C     4 AND 10 ARE NOT VALID REFERENCE CODES.

      IF ((NINT(PRMS(1)) .EQ. 4) .OR. (NINT(PRMS(1)) .EQ. 10) .OR.
     >    (NINT(PRMS(1)) .LT. 1) .OR. (NINT(PRMS(1)) .GT. 32)) THEN
        PRMS(1) = -1.0
      ENDIF

      IF (NINT(PRMS(2)) .LT. 1) PRMS(2) = -1.0

      SELECT CASE (NINT(PRMS(1)))
      CASE (1)
      IF (NINT(PRMS(2)) .GT. 22) PRMS(2) = -1.0

      CASE (2)
      IF (NINT(PRMS(2)) .GT. 59) PRMS(2) = -1.0

      CASE (3)
      IF (NINT(PRMS(2)) .GT. 66) PRMS(2) = -1.0

      CASE (5)
      IF (NINT(PRMS(2)) .GT. 17) PRMS(2) = -1.0

      CASE (6)
      IF (NINT(PRMS(2)) .GT. 27) PRMS(2) = -1.0

      CASE (7)
      IF (NINT(PRMS(2)) .GT. 56) PRMS(2) = -1.0

      CASE (8)
      IF (NINT(PRMS(2)) .GT. 86) PRMS(2) = -1.0

      CASE (9)
      IF (NINT(PRMS(2)) .GT. 26) PRMS(2) = -1.0

      CASE (11)
      IF (NINT(PRMS(2)) .GT. 26) PRMS(2) = -1.0

      CASE (12)
      IF (NINT(PRMS(2)) .GT. 90) PRMS(2) = -1.0

      CASE (13)
      IF (NINT(PRMS(2)) .GT. 42) PRMS(2) = -1.0

      CASE (14)
      IF (NINT(PRMS(2)) .GT. 29) PRMS(2) = -1.0

      CASE (15)
      IF (NINT(PRMS(2)) .GT. 29) PRMS(2) = -1.0

      CASE (16)
      IF (NINT(PRMS(2)) .GT. 41) PRMS(2) = -1.0

      CASE (17)
      IF (NINT(PRMS(2)) .GT. 35) PRMS(2) = -1.0

      CASE (18)
      IF (NINT(PRMS(2)) .GT. 43) PRMS(2) = -1.0

      CASE (19)
      IF (NINT(PRMS(2)) .GT. 34) PRMS(2) = -1.0

      CASE (20)
      IF (NINT(PRMS(2)) .GT. 26) PRMS(2) = -1.0

      CASE (21)
      IF (NINT(PRMS(2)) .GT. 25) PRMS(2) = -1.0

      CASE (22)
      IF (NINT(PRMS(2)) .GT. 36) PRMS(2) = -1.0

      CASE (23)
      IF (NINT(PRMS(2)) .GT. 26) PRMS(2) = -1.0

      CASE (24)
      IF (NINT(PRMS(2)) .GT. 27) PRMS(2) = -1.0

      CASE (25)
      IF (NINT(PRMS(2)) .GT. 14) PRMS(2) = -1.0

      CASE (26)
      IF (NINT(PRMS(2)) .GT. 16) PRMS(2) = -1.0

      CASE (27)
      IF (NINT(PRMS(2)) .GT. 30) PRMS(2) = -1.0

      CASE (28)
      IF (NINT(PRMS(2)) .GT. 30) PRMS(2) = -1.0

      CASE (29)
      IF (NINT(PRMS(2)) .GT. 16) PRMS(2) = -1.0

      CASE (30)
      IF (NINT(PRMS(2)) .GT. 16) PRMS(2) = -1.0

      CASE (31)
      IF (NINT(PRMS(2)) .GT. 10) PRMS(2) = -1.0

      CASE (32)
      IF (NINT(PRMS(2)) .GT. 39) PRMS(2) = -1.0

      END SELECT

      IF (NINT(PRMS(1)) .EQ. -1) THEN
        REF = 'UNKNOWN'
      ELSE
        REF = PHOTOREF(NINT(PRMS(1)))
      ENDIF

      J = NINT(PRMS(1))
      K = NINT(PRMS(2))

      IF ((J .NE. -1) .AND. (K .NE. -1)) THEN
        CALL FMPHOTOCODE(J, CHARCODE, K, 0)
      ELSE
        CHARCODE = 'UNKNOWN'
      ENDIF

      IF(LKECHO)WRITE(JOSTND,4810) KEYWRD, NINT(PRMS(1)),
     >    REF, NINT(PRMS(2)), CHARCODE
 4810 FORMAT(/1X,A8,'   PHOTO SERIES REFERENCE IS ',
     >    I4,' = ',A/T13,'PHOTO CODE IS ',I4,' = ',A)

      MYACT = 2548
      IF ((J .NE. -1) .AND. (K .NE. -1)) THEN
        CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      ELSE
            WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',
     &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ',
     &      'REQUIRED.  KEYWORD IGNORED.',/1X)")
            CALL RCDSET (2,.TRUE.)
      ENDIF
      GOTO 10
 4900 CONTINUE
C
C                        OPTION NUMBER 49 -- FIRECALC
C
      IF (ICALL .EQ. 2) THEN
        WRITE(JOSTND,4901) KEYWRD
 4901   FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >    ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
        GOTO 10
      ENDIF

      MYACT = 2549

      IDT = 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IF (IPRMPT.GT.0) THEN
         IF (IPRMPT.NE.2) THEN
            CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
            CALL ERRGRO (.TRUE.,25)
         ELSE
            CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >                   IPRMPT,IRECNT,ICYC)
            CALL fvsGetRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         ENDIF
         GOTO 10
      ENDIF

C     DEFAULTS (*) SET IN FMINIT:
C     PRM1: FIRE CALCULATION METHOD: 0 = OLD FM LOGIC (*), 1 = NEW FM LOGIC, 2 = USE MODELLED LOADS
C     PRM2: FUEL MODEL SET: 0 = 13, 1 = 40, 2 = 53 (*)
C     ===========================================================================
C           INTERNAL UNITS                                     METRIC INPUT UNITS
C     ===========================================================================
C     PRM3: ONE-HR SAV (1/FT) = 2000 (*)                   <- /m (m**2/m**3)
C     PRM4: HERB SAV (1/FT) = 1800 (*)                     <- /m
C     PRM5: LIVE WOODY SAV (1/FT) = 1500 (*)               <- /m
C     PRM6: BULK DENSITY - LIVE (LBS/FT3) = 0.10 (*)       <- kg/m**3
C     PRM7: BULK DENSITY - DEAD (LBS/FT3) = 0.75 (*)       <- kg/m**3
C     PRM8: DEAD AND LIVE HEAT CONTENT (BTU/LB) = 8000 (*) <- kilojoules/kilogram

C     PRM 2 IS USED WITH THE NEW FM LOGIC OPTION
C     PRMS 3 - 7 ARE USED WITH THE NEW FM LOGIC AND MODELLED LOADS OPTIONS
C     PRM 8 IS USED WITH THE MODELLED LOAD OPTION ONLY

      NPARMS= 8
      PRMS(1) = IFLOGIC
      PRMS(2) = IFMSET
      PRMS(3) = USAV(1)
      PRMS(4) = USAV(2)
      PRMS(5) = USAV(3)
      PRMS(6) = UBD(1)
      PRMS(7) = UBD(2)
      PRMS(8) = ULHV

      IF (LNOTBK(2)) PRMS(1) = MAX(0,MIN(2,INT(ARRAY(2))))
      IF (LNOTBK(3)) PRMS(2) = MAX(0,MIN(2,INT(ARRAY(3))))
      IF (LNOTBK(4)) PRMS(3) = MAX(0.0,ARRAY(4)) / FTtoM
      IF (LNOTBK(5)) PRMS(4) = MAX(0.0,ARRAY(5)) / FTtoM
      IF (LNOTBK(6)) PRMS(5) = MAX(0.0,ARRAY(6)) / FTtoM
      IF (LNOTBK(7)) PRMS(6) = MAX(0.0,ARRAY(7)) * FT3toM3/LBtoKG
      IF (LNOTBK(8)) PRMS(7) = MAX(0.0,ARRAY(8)) * FT3toM3/LBtoKG
      IF (LNOTBK(9)) PRMS(8) = MAX(0.0,ARRAY(9)) * LBtoKG/BTUtoKJ
C
      IF(LKECHO)WRITE(JOSTND,4910) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2)),
     >  PRMS(3)*FTtoM, PRMS(4)*FTtoM, PRMS(5)*FTtoM,
     >  PRMS(6)/(FT3toM3/LBtoKG), PRMS(7)/(FT3toM3/LBtoKG),
     >  PRMS(8)/(LBtoKG/BTUtoKJ)

 4910 FORMAT(/1X,A8,T13,'FIRE CALCULATIONS IN DATE/CYCLE ',
     >    I4,' WILL BE:'
     >    /T13, 'BASED ON METHOD',I2,' ', 
     >'(0=OLD FM LOGIC, 1=NEW FM LOGIC, 2=USE MODELLED LOADS DIRECTLY)',
     >    /T13, 'FUEL MODEL SET (IF USING NEW FM LOGIC) WILL BE',
     >    I2, ' (0=13, 1=40, 2=53)',/T13,
     >    'ONE-HOUR SAV (1/M) WILL BE: ',F6.0,/T13,
     >    'HERB SAV (1/M) WILL BE: ',F6.0,/T13,
     >    'LIVE WOODY SAV (1/M) WILL BE: ',F6.0,/T13,
     >    'LIVE FUEL BULK DENSITY (KG/M3) WILL BE: ',F6.3,/T13,
     >    'DEAD FUEL BULK DENSITY (KG/M3) WILL BE: ',F6.3,/T13,
     >    'HEAT CONTENT (KJ/KG) WILL BE: ',F6.0)

      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)

      GOTO 10
 5000 CONTINUE
C
C                        OPTION NUMBER 50 -- FMODLIST
C
      IF (ICALL .EQ. 2) THEN
        WRITE(JOSTND,5001) KEYWRD
 5001   FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >    ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
        GOTO 10
      ENDIF

      MYACT = 2550

      IDT = 1
      IF (LNOTBK(1)) IDT= IFIX(ARRAY(1))

      IF (IPRMPT.GT.0) THEN
         IF (IPRMPT.NE.2) THEN
            CALL FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
            CALL ERRGRO (.TRUE.,25)
         ELSE
            CALL OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,
     >                   IPRMPT,IRECNT,ICYC)
            CALL fvsGetRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         ENDIF
         GOTO 10
      ENDIF

C     PRM1: fuel model
C     PRM2: -1 = not set/default, 0 = on, 1 = off

      PRMS(1) = 1
      PRMS(2) = -1
      NPARMS= 2
      IF (LNOTBK(2)) PRMS(1) = MAX(0,INT(ARRAY(2)))
      IF (LNOTBK(3)) PRMS(2) = MAX(-1,MIN(1,INT(ARRAY(3))))
C
      IF(LKECHO)WRITE(JOSTND,5010) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2))

 5010 FORMAT(/1X,A8,T13,'IN DATE/CYCLE ',I4,' FUEL MODEL ',I3,
     >                  ' WILL BE:',I2,' (-1=DEFAULT, 0=ON, 1=OFF)')

      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)

      GOTO 10
 5100 CONTINUE
C                        OPTION NUMBER 51 -- DWDVLOUT
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,5104) KEYWRD
 5104    FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >     ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF
C
      IF (IDDWRP .EQ. 0) CALL GETID (IDDWRP)
C
      IDT = 1
      PRMS(1) = 200
      PRMS(2) = 1.0
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))
      IF (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))
C
      IF(LKECHO)WRITE(JOSTND,5115) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2))
 5115 FORMAT(/1X,A8,'   THE DOWN WOOD VOLUME REPORT WILL BEGIN',
     &      ' PRINTING IN DATE/CYCLE ',I4,','/T13,'FOR ',I4,
     &      ' YEARS USING ',I2,' YEAR INCREMENTS.')
C
      NPRMS = 2
      MYACT = 2551
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
      GOTO 10
 5200 CONTINUE
C                        OPTION NUMBER 52 -- DWDCVOUT
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,5204) KEYWRD
 5204    FORMAT(/1X,A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >     ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF
C
      IF (IDDWCV .EQ. 0) CALL GETID (IDDWCV)
C
      IDT = 1
      PRMS(1) = 200
      PRMS(2) = 1.0
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))
      IF (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))
C
      IF(LKECHO)WRITE(JOSTND,5215) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2))
 5215 FORMAT(/1X,A8,'   THE DOWN WOOD COVER REPORT WILL BEGIN',
     &      ' PRINTING IN DATE/CYCLE ',I4,','/T13,'FOR ',I4,
     &      ' YEARS USING ',I2,' YEAR INCREMENTS.')
C
      NPRMS = 2
      MYACT = 2552
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
      GOTO 10
 5300 CONTINUE
C                        OPTION NUMBER 53 -- FUELSOFT
C
C     THIS KEYWORD SETS THE INITIAL SOFT/ROTTEN FUEL VALUES, IF THEY ARE TO BE DIFFERENT
C     THAN THE DEFAULT ONES.
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,5304) KEYWRD
 5304    FORMAT(/1X,A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >      ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      NPARMS= 9
      PRMS(1) = -1.0
      PRMS(2) = -1.0
      PRMS(3) = -1.0
      PRMS(4) = -1.0
      PRMS(5) = -1.0
      PRMS(6) = -1.0
      PRMS(7) = -1.0
      PRMS(8) = -1.0
      PRMS(9) = -1.0

      IDT = 1
      IF (LNOTBK(1)) PRMS(1)= ARRAY(1)
      IF (LNOTBK(2)) PRMS(2)= ARRAY(2)
      IF (LNOTBK(3)) PRMS(3)= ARRAY(3)
      IF (LNOTBK(4)) PRMS(4)= ARRAY(4)
      IF (LNOTBK(5)) PRMS(5)= ARRAY(5)
      IF (LNOTBK(6)) PRMS(6)= ARRAY(6)
      IF (LNOTBK(7)) PRMS(7)= ARRAY(7)
      IF (LNOTBK(8)) PRMS(8)= ARRAY(8)
      IF (LNOTBK(9)) PRMS(9)= ARRAY(9)

      IF(LKECHO)WRITE(JOSTND,5310) KEYWRD,(PRMS(I),I=1,9)
 5310 FORMAT(/1X,A8,'   INITIAL SOFT FUEL VALUES (TONNES/HA) ARE',
     >     ' (-1.0=NO VALUE SPECIFIED): FUELS <.6cm=',F5.1/T13,
     >    'FUELS .6-2.5cm=',F5.1,'; FUELS 2.5-7.6cm=',F5.1,
     >    '; FUELS 7.6-15.2cm=',F5.1,'; FUELS 15.2-30.5cm=',F5.1,
     >    '; FUELS 30.5-50.8cm=',F5.1/T13,'FUELS 50.8-89.0cm=',F5.1,
     >    '; FUELS 89.0-127.1cm=',F5.1,'; FUELS >127.1cm=',F5.1)

      DO 5315 I= 1,NPARMS
        IF (PRMS(I) .GT. 0.0) PRMS(I) = PRMS(I) * TMtoTI / HAtoACR
 5315 CONTINUE

      MYACT = 2553
      CALL OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
      GOTO 10
 5400 CONTINUE
C                        OPTION NUMBER 54 -- USECFIM
C
C     THIS KEYWORD TELLS THE MODEL TO USE THE CFIM FIRE CALCULATIONS INSTEAD
C     OF THE DEFAULT FFE FIRE CALCULATIONS
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,5404) KEYWRD
 5404    FORMAT(/1X,A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >      ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      NPARMS= 7
C     BULK DENSITY
      PRMS(1) = 80.0
C     DROUGHT CODE
      PRMS(2) = 300
C     fuel_density(kg/m^3)---rho_surf   				398.0
      PRMS(3) = 398.0
C     surface_area_to_volume_ratio(1/m)---sigma_surf			3092.0
      PRMS(4) = 3092
C     surface_area_to_volume_ratio(1/m)---sigma_can				5401.0
      PRMS(5) = 5401.0
C     canopy_fuel_particle_diameter(m)---diameter				0.00074
      PRMS(6) = 0.00074
C     canopy_fuel_particle_length(m)---length				0.10
      PRMS(7) = 0.10

      IDT = 1
      IF (LNOTBK(1)) PRMS(1)= ARRAY(1)
      IF (LNOTBK(2)) PRMS(2)= ARRAY(2)
      IF (LNOTBK(3)) PRMS(3)= ARRAY(3)
      IF (LNOTBK(4)) PRMS(4)= ARRAY(4)
      IF (LNOTBK(5)) PRMS(5)= ARRAY(5)
      IF (LNOTBK(6)) PRMS(6)= ARRAY(6)
      IF (LNOTBK(7)) PRMS(7)= ARRAY(7)

      DO 5409 I=1,NPARMS
        IF (PRMS(I) .LT. 0) PRMS(I) = 0
5409  CONTINUE

      CFIM_ON = .TRUE.

C           BULK DENSITY
        CFIM_BD = PRMS(1)

C           DROUGHT CODE
        CFIM_DC = PRMS(2)

C           fuel_density(kg/m^3)---rho_surf   				398.0
        CFIM_INPUT(17) = PRMS(3)

C           surface_area_to_volume_ratio(1/m)---sigma_surf			3092.0
        CFIM_INPUT(16) = PRMS(4)

C           surface_area_to_volume_ratio(1/m)---sigma_can				5401.0
        CFIM_INPUT(19) = PRMS(5)

C           canopy_fuel_particle_diameter(m)---diameter				0.00074
        CFIM_INPUT(21) = PRMS(6)

C           canopy_fuel_particle_length(m)---length				0.10
        CFIM_INPUT(23) = PRMS(7)


      IF(LKECHO)WRITE(JOSTND,5410) KEYWRD,(PRMS(I),I=1,7)
 5410 FORMAT(/1X,A8,'   THE CFIM MODEL WILL BE USED FOR FIRE ',
     >    'CALCULATIONS.', /T13, 
     >    'BULK DENSITY=',F4.0, '; DROUGHT CODE=',I3/T13,
     >    'FUEL DENS(KG/M3)=',F5.1,
     >    '; SAV SURF=',F5.0,'; SAV CANOPY=',F5.0/T13,
     >    'CAN. PARTICLE DIAM (M)=',F7.5,
     >    '; CAN. PARTICLE LENGTH(M)=',F7.3)

      GOTO 10
 5500 CONTINUE
C                        OPTION NUMBER 55 -- CFIMCALC
C
C     THIS KEYWORD SETS SOME PARAMETERS THAT CONTROL THE MINI-SIMULATION
C     INSIDE THE CFIM MODEL. 
C
      IF (ICALL .EQ. 2) THEN
         WRITE(JOSTND,5505) KEYWRD
 5505    FORMAT(/1X,A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',
     >      ' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
         GOTO 10
      ENDIF

      NPARMS= 3
C     time_step(s)---tstep						1
      PRMS(1) = 1.0
C     iterations---iters						150
      PRMS(2) = 150.0
C     x_starting_position(m)---xstart					8.0
      PRMS(3) = 8.0

      IDT = 1
      IF (LNOTBK(1)) PRMS(1)= ARRAY(1)
      IF (LNOTBK(2)) PRMS(2)= ARRAY(2)
      IF (LNOTBK(3)) PRMS(3)= ARRAY(3)

C           time_step(s)---tstep						1
            IF (PRMS(1) .LT. 1) PRMS(1) = 1
            CFIM_INPUT(7) = PRMS(1)

C           iterations---iters						150
            IF (PRMS(2) .LT. 1) PRMS(2) = 1            
            CFIM_INPUT(8) = PRMS(2)

C           x_starting_position(m)---xstart					8.0
            IF (PRMS(3) .LT. 1) PRMS(3) = 1
            CFIM_INPUT(9) = PRMS(3)

      IF(LKECHO)WRITE(JOSTND,5510) KEYWRD,(PRMS(I),I=1,3)
 5510 FORMAT(/1X,A8,'   THE INTERNAL CFIM FIRE CALCULATION WILL USE THE'
     >    ' FOLLOWING PARAMETERS:', /T13,
     >    'TIME STEP=',F3.0,' ITERATIONS=',F5.0,' START POS=',F5.0)

      GOTO 10
C
C.... Special entry to retrieve keywords.

      ENTRY FMKEY (KEY,PASKEY)
      PASKEY= TABLE(KEY)
      RETURN
      END

      SUBROUTINE FMKEYDMP (IOUT,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
      IMPLICIT NONE
C----------
C  **FMKEYDMP DATE OF LAST REVISION:   08/03/05
C----------
      CHARACTER*10 KARD(*)
      CHARACTER*8 KEYWRD
      INTEGER IOUT, IRECNT, I, NVALS
      REAL ARRAY(NVALS)
C----------
C  THIS CODE ASSUMES NVALS IS LESS THAN OR EQUAL TO 12.
C----------
        WRITE (IOUT,70) IRECNT,KEYWRD,(ARRAY(I),I=1,NVALS)
        WRITE (IOUT,71) (KARD(I),I=1,NVALS)
   70   FORMAT (/' CARD NUM =',I8,'; KEYWORD FIELD = ''',A8,''''/
     >          '      NUMBERS=',12F13.7)
   71   FORMAT ('      CHARS  =',12('''',A10,'''':','))
C
      RETURN
      END

      SUBROUTINE FMKEYRDR (INUNIT,IOUT,LDEBUG,KEYWRD,LNOTBK,
     >                   ARRAY,IRECNT,KODE,KARD,LFLAG,NVALS)
      IMPLICIT NONE
C----------
C  **FMKEYRDR DATE OF LAST REVISION:  10/20/09
C----------
C
C     KEYWORD CARD READER FOR THE STAND PROGNOSIS SYSTEM
C
C     ARGUMENTS:
C     INUNIT= READER REFERENCE NUMBER
C     IOUT  = PRINTER REFERENCE NUMBER
C     LDEBUG= TRUE IF DEBUGGING.
C     KEYWRD= CHARACTER*8 KEYWORD.
C     LNOTBK= VALUES ARE TRUE IF THE CORRESPONDING
C             NUMERIC FIELD IS NOT BLANK, FALSE IF THEY ARE.
C     ARRAY = VALUES READ FROM THE NUMERIC FIELDS.
C     IRECNT= RECORD COUNTER.
C     KODE  = RETURN CODE:
C             0= NO ERRORS FOUND.
C             1= FIRST COLUMN OF CARD WAS BLANK OR INVALID CHAR DATA
C                WAS FOUND.
C             2= END-OF-FILE WAS FOUND.
C             LESS THAN ZERO:  PARMS STATEMENT IS ON THE RECORD START-
C             ING IN FIELD IABS(KODE).
C     KARD  = A CHARACTER IMAGE OF THE INPUT FIELDS
C     LFLAG = .TRUE. IF THE HEADING NEEDS TO BE CREATED, FALSE OTHERWISE
C     NVALS = THE NUMBER OF KEYWORD FIELDS, OFTEN 7, CAN BE DIFFERENT.
C----------
C  DECLARATIONS AND DATA STATEMENTS:
C----------
      INTEGER  NVALS
      CHARACTER*130 RECORD
      CHARACTER*10 KARD(NVALS)
      CHARACTER*8 KEYWRD,TMP
      LOGICAL LDEBUG,LNOTBK(NVALS),LCOM,LFLAG,L
      INTEGER IOUT,IRECNT,INUNIT,KODE,NF,IP,I,J,K,ISTLNB
      INTEGER IRTNCD
      REAL ARRAY(NVALS)
C----------
C  READ KEYWORD AND LOCATE BLANK PARAMETER FIELDS.
C----------
      LCOM=.FALSE.
    5 CONTINUE
      READ (INUNIT,'(A)',END=30) RECORD

      IRECNT = IRECNT + 1
      IF (RECORD(1:1).EQ.'!') GOTO 5
      IF (LFLAG .AND. RECORD.EQ.' ') GOTO 5
C
C     CHECK FOR "COMMENT" KEYWORD AND PROCESS.
C
      TMP=RECORD(1:8)
      DO I=1,8
         CALL UPCASE (TMP(I:I))
      ENDDO
      IF (TMP.EQ.'STOP') THEN
         IF (.NOT.LFLAG) WRITE (IOUT,'(/'' STOP'')')
         CALL GRSTOP
         CALL fvsGetRtnCode(IRTNCD)
         IF (IRTNCD.NE.0) RETURN
      ENDIF
      IF (LFLAG) THEN
         CALL GROHED (IOUT)
         CALL PPEATV (L)
         IF (L) WRITE (IOUT,11)
   11    FORMAT (/T40,'PARALLEL PROCESSING EXTENSION -- VERSION 1.0')
         WRITE (IOUT,12)
   12    FORMAT (/' ',130('-')//T50,'OPTIONS SELECTED BY INPUT'//
     >            ' ',130('-')/' KEYWORD    PARAMETERS:'/
     >                         ' --------   ',119('-'))
         LFLAG=.FALSE.
      ENDIF
      IF (RECORD(1:1).EQ.'*'.OR. RECORD.EQ.' ') THEN
         IF (.NOT.LCOM) THEN
            WRITE (IOUT,'(/)')
            LCOM=.TRUE.
         ENDIF
         WRITE (IOUT,'(T13,A)') RECORD(1:MAX(1,ISTLNB(RECORD)))
         GOTO 5
      ELSE
         LCOM=.FALSE.
      ENDIF
      IF (TMP.EQ.'COMMENT') THEN
         WRITE (IOUT,'(/1X,A)') TMP
   14    CONTINUE
         READ (INUNIT,'(A)',END=30) RECORD
         IRECNT=IRECNT+1
         TMP=RECORD (1:4)
         DO I=1,4
            CALL UPCASE (TMP(I:I))
         ENDDO
         IF (TMP(1:4).EQ.'END ') THEN
            WRITE (IOUT,'(/1X,A4)') TMP(1:4)
            GOTO 5
         ELSE
            WRITE (IOUT,'(T13,A)') RECORD(1:ISTLNB(RECORD))
            GOTO 14
         ENDIF
      ENDIF
C
C     CHECK FOR THE PRESENCE OF A 'P' SO THAT THE PARMS STATEMENT
C     MAY BE DETECTED.
C
      NF=NVALS
      IP=INDEX(RECORD(11:),'P')
      IF (IP.GT.0) THEN
C
C        BORROW THE USE OF KEYWRD TO HOLD "PARMS" IN MIXED, THEN UPPER
C        CASE...
C
         KEYWRD(1:5)=RECORD(IP+10:IP+14)
         DO I=1,5
            CALL UPCASE (KEYWRD(I:I))
         ENDDO
         IF (KEYWRD(1:5).EQ.'PARMS') THEN
            IF (MOD(IP,10).EQ.0) THEN
               NF=IP/10-1
            ELSE
               NF=IP/10
            ENDIF
         ENDIF
      ENDIF
C
C     LOAD THE KEYWORD INTO KEYWRD...DECODE THE FIELDS.
C
      KEYWRD=RECORD(1:8)
      J=1
      DO 25 I=1,NF
      J=J+10
      KARD(I)=RECORD(J:J+9)
      ARRAY(I)=0.0
C
C     MAKE SURE ALL OF THE CHARACTERS IN KARD ARE NUMBERS...
C
      DO 15 K=1,10
      IF (INDEX(' .+-eE0123456789',KARD(I)(K:K)).EQ.0) GOTO 25
   15 CONTINUE
      READ (KARD(I),'(G10.0)',ERR=25) ARRAY(I)
   25 CONTINUE
      GOTO 40
C
   30 CONTINUE
      KODE = 2
      RETURN
C
   40 CONTINUE
      CALL UPKEY(KEYWRD)
      KODE = 0
      DO 50 I=1,NF
      LNOTBK(I) = KARD(I).NE.' '
   50 CONTINUE
      IF (NF.LT.NVALS) THEN
         KODE=-(NF+1)
         DO 55 I=NF+1,NVALS
         LNOTBK(I)=.FALSE.
         ARRAY(I)=0.0
         J=I*10+1
         KARD(I)=RECORD(J:J+9)
   55    CONTINUE
      ENDIF
C
      IF ( LDEBUG ) CALL FMKEYDMP (IOUT,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
      RETURN
      END
