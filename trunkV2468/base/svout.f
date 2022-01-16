      SUBROUTINE SVOUT(IYEAR,IFMCLFG,AMSG)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C     D. ROBINSON   -- ESSA        -- MAY 2005
C     L.R. DAVID    -- FHTET FORT COLLINS -- 07/20/05
C     S.SCHAROSCH   -- ABACUS      -- MAR 2008
C     C. KEYSER     -- FS FMSC     -- AUG 2009
C
C     INPUT:
C     IYEAR = THE YEAR PART OF THE TITLE
C     IFMCLFG= THE IDENTIFIER OF THE CALLING CONTEXT
C               0=INVENTORY TIME
C               1=START OF CYCLE
C               2=AFTER CUTS
C               3=END OF PROJECTION (CALLED FROM MAIN)
C               4=CALLED FROM FIRE MODEL, INCLUDES FLAMES
C               5=CALLED FROM FIRE MODEL, NO FLAMES
C             CODES 0,1,2 HAVE EQ BEHAVIOR.
C     AMSG  = THE MESSAGE, USUALLY THE CALLING CONTEXT, OUTPUT ON
C             THE TITLE.
C
C     OUTPUT THE CURRENT VISUALIZATION DATA
C
C     Called from: GRINCR
C                  MAIN
C                  SVCUTS: to display post-thin SVS diagram.
C                  SVSTART: to display SVS diagram for initial inventory.
C                  FMSVOUT: to display fire burning through stand.
C
C
C     CURRENT TREE CLASSES:
C      0 - GREEN TREE (STANDING OR RECENTLY CUT TREE)
C     90 - NEW WESTWIDE PINE BEETLE KILL, OFF-GREEN
C     91 - 1 YEAR OLD WWPB KILL, RED TREE
C     92 - 2 YEAR OLD WWPB KILL, FADING TREE
C     94 - SNAG
C     95 - RED AND GREEN TREE (BURNT 99) NOT USED YET
C     96 - GREY SNAG (BURNT 94) - OLDER BURNED TREE
C     97 - GREY TREE (BURNT 98 OR 99) - RECENTLY BURNED TREE
C     98 - RED TREE (RECENTLY DEAD STANDING OR DOWN TREE)
C     99 - WILD CARD, WE DON'T USE THIS CODE...IT IS A GREEN
C          TREE (STANDING OR RECENTLY CUT TREE)
C----------
C
C
COMMONS

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

      INCLUDE 'ARRAYS.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'FMCOM.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'SVDATA.F77'

      INCLUDE 'SVDEAD.F77'

      INCLUDE 'METRIC.F77'

COMMONS

      CHARACTER(*), PARAMETER :: SUFFIX = '_index.svs'
      CHARACTER(*), PARAMETER :: STDTAG = '"Stand='
      INTEGER IFMCLFG,IYEAR,KYLAST,ISTLNB,I,J,K,KYFRST,
     >        NOUT,ISVOBJ,IPS,IDIR,ITC,IPUT,IX,ISNAG
      REAL    X,CW,CRAD,XICR,SNDI,SNHT,SNCRTO,SNCRDI,RAD,
     >        X1,Y1,X2,Y2,XM1,XM2,XM3
      REAL    EXPSNGS, NFFESNGS, NSTNDNG, XPROB
      CHARACTER*(*) AMSG
      CHARACTER*2 SPCD
      CHARACTER*4 SFILE
      CHARACTER*7 VVER
      CHARACTER*100 CBUFF
      CHARACTER*23 PLTGEO
      LOGICAL DEBUG,LOPEN
      CALL DBCHK (DEBUG,'SVOUT',5,ICYC)     
      IF (DEBUG) WRITE (JOSTND,5) IYEAR, AMSG, JSVOUT, NSVOBJ,
     >  JSVPIC, NIMAGE, IFMCLFG
    5 FORMAT (/' IN SVOUT: IYEAR=',I5,' AMSG=',A,' JSVOUT=',I3,
     >  ' NSVOBJ=',I6,' JSVPIC=',I4,' NIMAGE=',I5,' IFMCLFG=',I5)

      IF (DEBUG) THEN
        WRITE (JOSTND,1000) ICYC, IYEAR, ILYEAR,
     &                      NDEAD, NSVOBJ
 1000   FORMAT (' ','ENTERING SVOUT, ICYC=',I2,
     &              ', IYEAR=',I4,', ILYEAR=',I4,':', / ,
     &          ' ',T5,'NDEAD=',I4,', NSVOBJ=',I6,//,
     &          ' ',T5,'SNAG LIST:',//,
     &          ' ',T5,'  I   IDTREE  SPP ODIA OLEN IYRCOD STATUS '
     &                 'FALLDIR OPROB STDNG',/,
     &          ' ',T5,'---- -------- --- ---- ---- ------ ------ '
     &                 '------- ----- -----' )
C                       XXXX XXXXXXXX XXX XX.X XXX. XXXXXX XXXXXX
C                       XXXXXX. XXXX. XXXX.
        DO I=1,NDEAD
          WRITE(JOSTND,1010) I, OIDTRE(I), ISNSP(I), ODIA(I), OLEN(I),
     &                       IYRCOD(I), ISTATUS(I),
     &                       FALLDIR(I), SPROBS(I,1), SPROBS(I,2)
 1010     FORMAT(' ',T5,I4,1X,I8,1X,I3,1X,F4.1,1X,F4.0,1X,
     &               I6,1X,I6,1X,F7.0,1X,F5.0,1X,F5.0)
        ENDDO

        WRITE(JOSTND,1020)
 1020   FORMAT(' ','SNAG SVS OBJECTS CURRENTLY DEFINED:')
        DO ISVOBJ=1,NSVOBJ
          IF (IOBJTP(ISVOBJ) .EQ. 2) THEN
            WRITE(JOSTND,1030) ISVOBJ, IOBJTP(ISVOBJ), IS2F(ISVOBJ)
 1030       FORMAT(' ',T5,'ISVOBJ=',I6,', IOBJTP=',I1,', IS2F=',I6)
          ENDIF
        ENDDO
      ENDIF

      IF (JSVOUT.EQ.0) RETURN
      IF (JSVOUT.LT.0) GOTO 26 ! PROCESSING IMAGE, BUT NOT OUTPUTING
      
C     Make sure that the index file is opened (could be closed if a 
C     restart is being done.

      inquire(unit=JSVOUT,opened=LOPEN)

      if (.not.LOPEN) then
        open(unit=JSVOUT,file=trim(KWDFIL)//SUFFIX,
     >         status="old",err=7)

c       find out the last used value of NIMAGE. 
        
        do 
          read(jsvout,'(a)',end=2) CBUFF
          if (CBUFF(:7).eq.STDTAG) NIMAGE=NIMAGE+1
        enddo
    2   continue
        close(unit=JSVOUT)
        
        open(unit=JSVOUT,file=trim(KWDFIL)//SUFFIX,
     >         position="append",err=7)
        goto 9
    7   continue
        write (JOSTND,8) trim(KWDFIL)//SUFFIX
    8   format (/'**** FILE OPEN ERROR FOR FILE: ',A)
        CALL RCDSET (2,.TRUE.)
        JSVOUT=0
        RETURN
    9   continue
      endif
      
      IF (IMETRIC.EQ.0) THEN
        IF (IPLGEM.LT.2) THEN
           PLTGEO='#PLOTSIZE 208.71 208.71'
        ELSE
           PLTGEO='#ROUNDPLOT 235.5'
        ENDIF
      ELSE
        IF (IPLGEM.LT.2) THEN
           PLTGEO='#PLOTSIZE 100.00 100.00'
        ELSE
           PLTGEO='#ROUNDPLOT 112.84'
        ENDIF
      ENDIF

C     INCREMENT NIMAGE REGARDLESS OF THE FILE OUTPUT STRUCTURE...
C     THIS IS DONE TO INSURE THAT MULTIPLE RUNS ARE PROCESSED.

      NIMAGE = NIMAGE+1
      IF (JSVPIC.GT.0) THEN

C       FIND THE FIRST AND LAST CHAR OF THE KEYWORD NAME
C       WATCH FOR DIRECTORY LEVELS...WE DON'T WANT THEM.
 
        KYLAST=len_trim(KWDFIL)
        DO I=KYLAST,1,-1
          IF (KWDFIL(I:I).EQ.'/' .OR. KWDFIL(I:I).EQ.'\') EXIT
          KYFRST=I
        ENDDO
        IF (DEBUG) WRITE (JOSTND,*) 'KYFRST=',KYFRST,
     >    ' KYLAST=',KYLAST,' KWDFIL=',KWDFIL(KYFRST:KYLAST)
        IF (NIMAGE.LT.1000) THEN
          WRITE (CBUFF,'(A,''_'',I3.3,''.svs'')')
     >      KWDFIL(KYFRST:KYLAST),NIMAGE
        ELSE
          WRITE (CBUFF,'(A,''_'',I6.6,''.svs'')')
     >      KWDFIL(KYFRST:KYLAST),NIMAGE
        ENDIF
        IF (DEBUG) WRITE (JOSTND,*) 'FILE OPEN=',
     >    TRIM(KWDFIL(:KYLAST)//'/'//CBUFF)

C       TRY TO OPEN A FILE WITH THE DIRECTORY NAME INCLUDED.

        OPEN (UNIT=JSVPIC,FILE=TRIM(KWDFIL(:KYLAST)//'/'//CBUFF),
     >        STATUS="REPLACE",ERR=12)

        WRITE (JSVOUT,10) STDTAG,NPLT(1:MAX(1,ISTLNB(NPLT))),IYEAR,
     >      AMSG,KWDFIL(:KYLAST)//'/'//TRIM(CBUFF)
   10   FORMAT (A,A,' Year=',I4.4,' ',A,'" "',A,'"')
        GOTO 20 
  
C       IF THE OPEN FAILS, THEN OPEN ONE WITHOUT THE DIR NAME INCLUDED.

   12   CONTINUE

        OPEN (UNIT=JSVPIC,FILE=TRIM(CBUFF),STATUS="REPLACE",ERR=14)
        WRITE (JSVOUT,10) STDTAG,NPLT(1:MAX(1,ISTLNB(NPLT))),IYEAR,
     >        AMSG,TRIM(CBUFF)
        GOTO 20
   14   CONTINUE

C       IF THIS OPEN FAILS, THEN BAG SVS OUTPUT.

        WRITE (JOSTND,18) TRIM(CBUFF)
   18   FORMAT (/T13,'**** FILE OPEN ERROR FOR FILE: ',A)
        CALL RCDSET (2,.TRUE.)

C       SETTING JSVOUT TO ZERO TURNS OFF SVS...WE'RE DONE.

        JSVOUT=0
        RETURN
        
   20   CONTINUE
        NOUT=JSVPIC
      ELSE
        NOUT=JSVOUT
      ENDIF
      
  
      CALL VARVER (VVER)
      SELECT CASE (VVER(:2))
        CASE ('CS','LS','NE','SN')
          SFILE='EAST'
        CASE ('BC')
          SFILE='  BC'
        CASE ('ON')
          SFILE='  ON'
        CASE DEFAULT
          SFILE='WEST'
      END SELECT          
      WRITE (NOUT,21) NPLT(1:MAX(1,ISTLNB(NPLT))),IYEAR,AMSG,
     >  SFILE,PLTGEO(1:ISTLNB(PLTGEO))
   21 FORMAT ('#TITLE Stand=',A,' Year=',I4.4,' ',A/
     >  '#TREEFORM ',A,'.TRF'/
     >  '#FORMAT 2'/A)

      IF (IMETRIC.EQ.0) THEN
        WRITE(NOUT,"('#UNITS ENGLISH')")
      ELSE
        WRITE(NOUT,"('#UNITS METRIC')")
      ENDIF

      WRITE(NOUT,"(';                  trcl  stus             fang'/
     >  ';species        tr#  |crcl|   dbh   ht lang |edia crd',
     >  '  cr    crd  cr    crd  cr    crd  cr ex mk  xloc    yloc',
     >  '  z')")

C     OUTPUT SUBPLOT BOUNDARIES, IF DESIRED.

      IF (IDPLOTS.EQ.1) THEN

C       WRITE THE OUTSIDE BOUNDARY.

        IF (IMETRIC.EQ.0) THEN
          IF (IPLGEM.LT.2) THEN
            WRITE (NOUT,'(''#RECTANGLE 0 0 208.71 208.71'')')
          ELSE
            WRITE (NOUT,'(''#CIRCLE 117.75 117.75 117.75'')')
          ENDIF
        ELSE
          IF (IPLGEM.LT.2) THEN
            WRITE (NOUT,'(''#RECTANGLE 0 0 100.00 100.00'')')
          ELSE
            WRITE (NOUT,'(''#CIRCLE 56.42 56.42 56.42'')')
          ENDIF
        ENDIF
        IF (ISVINV.GT.1) THEN
          IF (IPLGEM.EQ.1) THEN
            DO I=1,ISVINV
              X1 = X1R1S(I)
              Y2 = Y2A2S(I)

              IF (IMETRIC.EQ.0) THEN
                XM1 = 208.7103
              ELSE
                XM1 = 100.0
              ENDIF

              IF (X1.GT. 0 .AND. X1.LT. XM1) THEN
                Y1=Y1A1S(I)
                WRITE (NOUT,23) X1,Y1,X1,Y2
   23           FORMAT ('#LINE ',4F8.2)
              ENDIF
              IF (Y2.GT. 0 .AND. Y2.LT. XM1) THEN
                X2 = X2R2S(I)
                WRITE (NOUT,23) X1,Y2,X2,Y2
              ENDIF
            ENDDO
          ELSEIF (IPLGEM.EQ.3) THEN
            RAD=0.
            IX =0
            DO I=1,ISVINV
              IF (X2R2S(I).GT.RAD) THEN
                IF (IX.GT.1) WRITE (NOUT,23) X1,Y1,X2,Y2
                IX = 1
                RAD=X2R2S(I)

                IF (IMETRIC.EQ.0) THEN
                  XM1 = 117.75
                  IF (RAD.LT.XM1) WRITE (NOUT,24) RAD
   24             FORMAT ('#CIRCLE 117.75 117.75',F8.2)
                ELSE
                  XM1 = 56.42
                  IF (RAD.LT.XM1) WRITE (NOUT,241) RAD
  241             FORMAT ('#CIRCLE 56.42 56.42',F8.2)
                ENDIF

              ELSE
                IX = IX+1
              ENDIF
              IF (IX.GT.1) WRITE (NOUT,23) X1,Y1,X2,Y2
              X1 = COS(Y1A1S(I))*X1R1S(I) + XM1
              Y1 = SIN(Y1A1S(I))*X1R1S(I) + XM1
              X2 = COS(Y1A1S(I))*X2R2S(I) + XM1
              Y2 = SIN(Y1A1S(I))*X2R2S(I) + XM1
            ENDDO
            IF (IX.GT.1) WRITE (NOUT,23) X1,Y1,X2,Y2
          ENDIF
        ENDIF
      ENDIF

C     OUTPUT THE GROUND FILE

      IF (IGRID.GT.0) CALL SVGRND(NOUT,KYLAST,KYFRST,IFMCLFG)

C     OUTPUT THE FIRE LINE DATA

      IF (IFMCLFG.EQ.4) CALL FMSVFL (NOUT)

C     OUTPUT RANGE POLES, IF DESIRED.

      IF (IRPOLES.EQ.1) THEN

        IF (IMETRIC.EQ.0) THEN
          I   = 100
          J   =  30  ! range pole 30 ft
          XM1 = 208.71
          XM2 = 117.75
          XM3 = 235.50
        ELSE
          I   =  50
          J   =   5  ! range pole 5 m
          XM1 = 100.0
          XM2 =  56.42
          XM3 = 112.84
        ENDIF

        IF (IPLGEM.LT.2) THEN

C         SQUARE PLOTS

          WRITE (NOUT,25) J,I,0.00,0.00
   25     FORMAT ('RANGEPOLE',T16,
     >      '0 0 0 1 ',I3,I4,' 0 0 0 0 0 0 0 0 0 0 0 1 0 ',
     >      2F8.2,' 0')
          WRITE (NOUT,25) J, I, 0.00,  XM1
          WRITE (NOUT,25) J, I,  XM1, 0.00
          WRITE (NOUT,25) J, I,  XM1,  XM1
        ELSE

C         ROUND PLOTS

          WRITE (NOUT,25) J, I,  XM2,  XM3
          WRITE (NOUT,25) J, I,  XM2, 0.00
          WRITE (NOUT,25) J, I,  XM3,  XM2
          WRITE (NOUT,25) J, I, 0.00,  XM2
        ENDIF
      ENDIF

   26 CONTINUE ! BRANCH HERE WHEN SKIPPING OUTPUT.
   
C     IF THE FFE IS ACTIVE, CALL SVCWD TO GENERATE CWD OBJECTS

      IF ( LFMON ) CALL SVCWD(IYEAR)
C
      IF (NSVOBJ.EQ.0) GOTO 50
C
      DO ISVOBJ=1,NSVOBJ
        I=IS2F(ISVOBJ)
        IF (I.GT.0) THEN
          IPS = 1
          IDIR= 0
          ITC = 0
          IF (IOBJTP(ISVOBJ).EQ.1 .OR. IOBJTP(ISVOBJ).EQ.3) THEN

C           TO HANDLE IOBJTP=3 OBJECTS, OR OBJECTS WHICH WILL BE
C           REMOVED AT THE END OF THIS CYCLE

            IF (IOBJTP(ISVOBJ) .EQ. 3) THEN
              IPS=0
              CALL SVRANN(X)
              IDIR = IFIX(360. * X +.5)
            ENDIF

C              GET CROWN WIDTH.

            CW=CRWDTH(I)
            CRAD=CW/2.

C           LOAD THE SPECIES CODE

            SPCD=' '
            IF (NSP(ISP(I),1)(1:1).EQ.' ') THEN
              SPCD(2:2)=' '
              SPCD(1:1)=NSP(ISP(I),1)(2:2)
            ELSE
              SPCD(1:2)=NSP(ISP(I),1)(1:2)
            ENDIF

C           ICR CAN BE NEGATIVE WHEN SVOUT IS CALLED DURING A
C           FIRE OR BUG MODEL CALL.

            XICR = ABS(ICR(I))*.01

            IF (JSVOUT.LT.0) GOTO 31 ! PROCESSING IMAGE, BUT NOT OUTPUTING
            IF (IMETRIC.EQ.0) THEN
              WRITE (NOUT,30) SPCD,I,ITC,0,IPS,DBH(I),HT(I),0,IDIR,
     >          0,CRAD,XICR,CRAD,XICR,CRAD,XICR,CRAD,XICR,
     >          1,0,XSLOC(ISVOBJ),YSLOC(ISVOBJ),0
            ELSE
              WRITE (NOUT,30) SPCD,I,ITC,0,IPS,
     >          DBH(I)*INtoCM,HT(I)*FTtoM,0,IDIR,
     >          0,CRAD*FTtoM,XICR,CRAD*FTtoM,XICR,CRAD*FTtoM,
     >          XICR,CRAD*FTtoM,XICR,
     >          1,0, XSLOC(ISVOBJ),YSLOC(ISVOBJ),0
            ENDIF
   30       FORMAT (A,T16,I5,I3,2I2,F6.1,F6.0,I2,I4,I2,
     >        4(F6.1,1X,F4.2),2I2,2F8.2,I2)
   31       CONTINUE
C
          ELSEIF (IOBJTP(ISVOBJ).EQ.2 .OR. IOBJTP(ISVOBJ).EQ.5) THEN

C           THE OBJECT IS A SNAG

            SPCD=' '
            IF (NSP(ISNSP(I),1)(1:1).EQ.' ') THEN
              SPCD(2:2)=' '
              SPCD(1:1)=NSP(ISNSP(I),1)(2:2)
            ELSE
              SPCD(1:2)=NSP(ISNSP(I),1)(1:2)
            ENDIF

C           If snag was flagged for salvage in SVSALV (IOBJTP=5):
C              1) Fell it now for display in the post thin/salvage display.
C              2) Update "remaining snag" count for cohort snags.
C              3) Snag will be removed at the bottom of SVOUT.

            IF (IOBJTP(ISVOBJ) .EQ. 5) THEN
              CALL SVRANN(X)
              FALLDIR(IS2F(ISVOBJ)) = IFIX(360. * X +.5)
              XPROB = 0.0
              DO K=1,MXDEAD
                IF ( ISTATUS(K) .NE. 0 ) THEN
                  IF ( OIDTRE(K)  .EQ. OIDTRE(IS2F(ISVOBJ)) .AND.
     &                 IYRCOD(K)  .EQ. IYRCOD(IS2F(ISVOBJ)) .AND.
     &                 FALLDIR(K) .EQ. -1 ) XPROB=XPROB+1
                ENDIF
              ENDDO
              DO K=1,MXDEAD
                IF ( ISTATUS(K) .NE. 0 ) THEN
                  IF ( OIDTRE(K) .EQ. OIDTRE(IS2F(ISVOBJ)) .AND.
     &                 IYRCOD(K) .EQ. IYRCOD(IS2F(ISVOBJ)) ) THEN
                    SPROBS(K,2) = XPROB
                  ENDIF
                ENDIF
              ENDDO
            ENDIF

            CALL SVSNAGE(IYEAR,IS2F(ISVOBJ),SNCRDI,SNCRTO,
     >        SNHT,SNDI)
              
C           Keep snags with diameter less than 1", for better
C           agreement with FFE logic.

            IF ( SNHT .LE. 0.) THEN
              IF (ISTATUS(IS2F(ISVOBJ)).GT.0) NDEAD=NDEAD-1
              IF (DEBUG) THEN
                WRITE (JOSTND,1050) IS2F(ISVOBJ), SNHT, NDEAD
 1050           FORMAT (' ',T5,'Dropping snag:',I5,' with SNHT=',F4.1,
     &                         '. NDEAD=',I5)
              ENDIF
              ISTATUS(IS2F(ISVOBJ))=0
              IOBJTP(ISVOBJ)=0
              GOTO 40
            ENDIF
            CRAD=SNCRDI/2
            XICR=SNCRTO*.01

            IF (FALLDIR(IS2F(ISVOBJ)).EQ.-1) THEN

C             STANDING DEAD TREES....

              IF (ISTATUS(IS2F(ISVOBJ)) .EQ. 1) THEN

C               THE PROGRAM SHOULD NEVER BE IN THIS CASE
C               IT IS USED AS A CHECK

                ITC = 0
                IDIR = 0
                IPS = 1
              ELSEIF (ISTATUS(IS2F(ISVOBJ)) .EQ. 2) THEN
                ITC = 98
                IDIR = 0
                IPS = 1
              ELSEIF (ISTATUS(IS2F(ISVOBJ)) .EQ. 3) THEN
                ITC = 94
                IDIR = 0
                IPS = 1
              ELSEIF (ISTATUS(IS2F(ISVOBJ)) .EQ. 4) THEN
                ITC = 94
                IDIR = 0
                IPS = 1
              ELSEIF (ISTATUS(IS2F(ISVOBJ)) .EQ. 5) THEN
                ITC = 97
                IDIR = 0
                IPS = 1
              ELSEIF (ISTATUS(IS2F(ISVOBJ)) .EQ. 6) THEN
                ITC = 96
                IDIR = 0
                IPS = 1
              ELSEIF (ISTATUS(IS2F(ISVOBJ)) .EQ. 90) THEN
                ITC = 90
                IDIR = 0
                IPS = 1
              ELSEIF (ISTATUS(IS2F(ISVOBJ)) .EQ. 91) THEN
                ITC = 91
                IDIR = 0
                IPS = 1
              ELSEIF (ISTATUS(IS2F(ISVOBJ)) .EQ. 92) THEN
                ITC = 92
                IDIR = 0
                IPS = 1
              ENDIF
            ELSE

C             Fallen/salvaged snags.
C             Test absolute value of status code, since salvaged snags
C             have had their status code multiplied by -1.

              IF (ABS(ISTATUS(IS2F(ISVOBJ))) .EQ. 1) THEN
                ITC = 0
                IPS = 0
                IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              ELSEIF (ABS(ISTATUS(IS2F(ISVOBJ))) .EQ. 2) THEN
                ITC = 98
                IPS = 0
                IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              ELSEIF (ABS(ISTATUS(IS2F(ISVOBJ))) .EQ. 3) THEN
                ITC = 94
                IPS = 0
                IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              ELSEIF (ABS(ISTATUS(IS2F(ISVOBJ))) .EQ. 4) THEN
                ITC = 94
                IPS = 3
                IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              ELSEIF (ABS(ISTATUS(IS2F(ISVOBJ))) .EQ. 5) THEN
                ITC = 97
                IPS = 0
                IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              ELSEIF (ABS(ISTATUS(IS2F(ISVOBJ))) .EQ. 6) THEN
                ITC = 96
                IPS = 0
                IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              ELSEIF (ABS(ISTATUS(IS2F(ISVOBJ))) .EQ. 90) THEN
                ITC = 90
                IPS = 0
                IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              ELSEIF (ABS(ISTATUS(IS2F(ISVOBJ))) .EQ. 91) THEN
                ITC = 91
                IPS = 0
                IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              ELSEIF (ABS(ISTATUS(IS2F(ISVOBJ))) .EQ. 92) THEN
                ITC = 92
                IPS = 0
                IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              ENDIF
            ENDIF
            
            IF( JSVOUT.LT.0) GOTO 32 ! PROCESSING IMAGE, BUT NOT OUTPUTING

            IF(IMETRIC.EQ.0) THEN
              WRITE (NOUT,30) SPCD,I,ITC,0,IPS,SNDI,SNHT,0,IDIR,
     >          0,CRAD,XICR,CRAD,XICR,CRAD,XICR,CRAD,XICR,
     >          1,0,XSLOC(ISVOBJ),YSLOC(ISVOBJ),0
            ELSE
              WRITE (NOUT,30) SPCD,I,ITC,0,IPS, SNDI*INtoCM,
     >          SNHT*FTtoM,0,IDIR,0,CRAD*FTtoM,XICR,CRAD*FTtoM,
     >          XICR,CRAD*FTtoM,XICR,CRAD*FTtoM,XICR,1,0,
     >          XSLOC(ISVOBJ),YSLOC(ISVOBJ),0
            ENDIF
            IF (ISTATUS(IS2F(ISVOBJ)) .EQ. 0) IOBJTP(ISVOBJ)=0

C              DRAW FLAME OBJECTS FOR THIS TREE

            IF (IFMCLFG.EQ.4 .AND. ITC.EQ.97 .AND. IDIR.EQ.0)
     >        CALL FMSVTREE (NOUT,ISVOBJ)

C              TO HANDLE IOBJTP=3 OBJECTS, OR OBJECTS WHICH WILL B
C              REMOVED AT THE END OF THIS CYCLE

  32        CONTINUE ! BRANCH HERE IF NOT OUTPUTTING

C
          ELSEIF (IOBJTP(ISVOBJ) .EQ. 4) THEN

C----------
C  The object is CWD:
C
C  For now, use the species code SNAG2. This will cause the CWD
C  to be tallied in the SVS tree/snag counts.
C  Special objects have Plant Type=15 in the SVS tree definition file,
C  so that they don't contribute to tree counts:
C     CAR, CRANEBOOM, CRANETOWER, CUBE, MARKER, RANGEPOLE, ROCK,
C     TETRAHEDRON, TRUCK
C  But special objects can't be depicted in a felled orientation.
C----------

            ITC = 0
            IPS = 3
            SNDI = CWDDIA(IS2F(ISVOBJ))
            SNHT = CWDLEN(IS2F(ISVOBJ))
            IDIR = IFIX(CWDDIR(IS2F(ISVOBJ)))
            CRAD = 0
            XICR = 0

C----------
C  Generate SVS records for the CWD objects.
C  SVS output vars are:
C     SPCD: species code
C     I:    plant ID
C     ITC:  tree class/growth form
C     0:    crown class
C     IPS:  plant status:
C       0 or 10: plant is cut, has branches, is lying on the ground
C       1 or 11: plant is standing and has branches
C       2 or 12: plant is no longer present and only a stump remains
C       3 or 13: plant is cut, has no branches, is lying on the ground
C       Plants with status codes of 0, 1, 2, or 3 can have their status
C       codes modified using the marking and treatment features of SVS.
C       Plants with status codes of 10, 11, 12, or 13 cannot have their
C       status codes modified within SVS.
C     SNDI: dbh
C     SNHT: ht
C     0:    lean angle
C     IDIR: felling angle (degrees)
C     0:    small end diam
C     CRAD: crown radius 1 (at 0 degrees)
C     XICR: crown ratio 1
C     CRAD: crown radius 2 (at 90 degrees)
C     XICR: crown ratio 2
C     CRAD: crown radius 3 (at 180 degrees)
C     XICR: crown ratio 3
C     CRAD: crown radius 4 (at 270 degrees)
C     XICR: crown ratio 4
C     1:    expansion factor
C     0:    marking status A
C     XSLOC(ISVOBJ): x location
C     YSLOC(ISVOBJ): y location
C     0: elevation
C----------
            IF( JSVOUT.LT.0) GOTO 33 ! PROCESSING IMAGE, BUT NOT OUTPUTING

            IF(IMETRIC.EQ.0) THEN
              WRITE (NOUT,1060) I, ITC, 0, IPS, SNDI, SNHT,
     &                          0, IDIR, 0,
     &                          CRAD, XICR, CRAD, XICR,
     &                          CRAD, XICR, CRAD, XICR,
     &                          1, 0, XSLOC(ISVOBJ), YSLOC(ISVOBJ), 0
            ELSE
              SNDI = SNDI * INtoCM
              SNHT = SNHT * FTtoM
              CRAD = CRAD * FTtoM
              WRITE (NOUT,1060) I, ITC, 0, IPS, SNDI, SNHT,
     &                          0, IDIR, 0,
     &                          CRAD, XICR, CRAD, XICR,
     &                          CRAD, XICR, CRAD, XICR,
     &                          1, 0, XSLOC(ISVOBJ), YSLOC(ISVOBJ), 0
 1060       FORMAT ('SNAG2',T16,I5,I3,2I2,F6.1,F6.0,
     &              I2,I4,I2,
     &              4(F6.1,1X,F4.2),
     &              2I2,2F8.2,I2)
            ENDIF
   33       CONTINUE
          ENDIF
        ENDIF
   40   CONTINUE
      ENDDO
      ILYEAR = IYEAR

C     CLEAN UP THE SVS DATA AFTER A DISPLAY.

      IPUT=0
      DO ISVOBJ=1,NSVOBJ
        IF (IOBJTP(ISVOBJ).LE.0 .OR. IS2F(ISVOBJ).EQ.0
     >    .OR. IOBJTP(ISVOBJ).EQ.3 .OR. IOBJTP(ISVOBJ).EQ.5) THEN
          IF (IPUT.EQ.0) IPUT=ISVOBJ
        ELSE
          IF (IPUT.GT.0 .AND. IPUT.LT.ISVOBJ) THEN
            IS2F(IPUT) = IS2F(ISVOBJ)
            XSLOC(IPUT) = XSLOC(ISVOBJ)
            YSLOC(IPUT) = YSLOC(ISVOBJ)
            IOBJTP(IPUT) = IOBJTP(ISVOBJ)
            IOBJTP(ISVOBJ) = 0
            IPUT=IPUT+1
          ENDIF
        ENDIF
      ENDDO
      IF (IPUT.GT.0) NSVOBJ=IPUT-1

C----------
C  Compress the base FVS snag arrays for:
C     1) Salvaged snags (ISTATUS is negative)
C     2) Snags whose dia or ht have dropped below the minimum (ISTATUS=0).
C  Need to update any associated SVS object pointers at the same time.
C----------

      IF (DEBUG) THEN
        WRITE (JOSTND,1070) NDEAD
 1070   FORMAT (' ',T5,'BEFORE COMPRESS, NDEAD=',I4)
      ENDIF

      IPUT = 0
      DO ISNAG=1,MXDEAD
        IF (ISTATUS(ISNAG).LE.0) THEN
          IF(IPUT.EQ.0) IPUT=ISNAG
        ELSE
          IF (IPUT.GT.0 .AND. IPUT.LT.ISNAG) THEN
            ISTATUS(IPUT) = ISTATUS(ISNAG)
            ISNSP(IPUT) = ISNSP(ISNAG)
            CRNDIA(IPUT) = CRNDIA(ISNAG)
            CRNRTO(IPUT) = CRNRTO(ISNAG)
            OLEN(IPUT) = OLEN(ISNAG)
            ODIA(IPUT) = ODIA(ISNAG)
            FALLDIR(IPUT) = FALLDIR(ISNAG)
            IYRCOD(IPUT) = IYRCOD(ISNAG)
            OIDTRE(IPUT) = OIDTRE(ISNAG)
            PBFALL(IPUT) = PBFALL(ISNAG)
            SPROBS(IPUT,1) = SPROBS(ISNAG,1)
            SPROBS(IPUT,2) = SPROBS(ISNAG,2)
            SPROBS(IPUT,3) = SPROBS(ISNAG,3)
            SNGDIA(IPUT) = SNGDIA(ISNAG)
            SNGLEN(IPUT) = SNGLEN(ISNAG)
            SNGCNWT(IPUT,0:3) = SNGCNWT(ISNAG,0:3)
            ISTATUS(ISNAG) = 0
            SPROBS(ISNAG,1) = 0
            SPROBS(ISNAG,2) = 0
            SPROBS(ISNAG,3) = 0
            DO J=1,NSVOBJ
              IF (IOBJTP(J).EQ.2 .AND. IS2F(J).EQ.ISNAG) THEN
                 IS2F(J) = IPUT
                 EXIT
              ENDIF
            ENDDO
            IPUT=IPUT+1
          ENDIF
        ENDIF
      ENDDO
      IF (IPUT.GT.0) NDEAD=IPUT-1

      IF (DEBUG) THEN
        WRITE (JOSTND,1075) NDEAD
 1075   FORMAT (' ',T5,'AFTER COMPRESS, NDEAD=',I4)
      ENDIF

      IF (DEBUG) THEN
        WRITE (JOSTND,1080) ICYC, IYEAR, ILYEAR,
     &                      NDEAD, NSVOBJ
 1080   FORMAT (' ','Leaving SVOUT, ICYC=',I2,
     &              ', IYEAR=',I4,', ILYEAR=',I4,':', / ,
     &          ' ',T5,'NDEAD=',I4,', NSVOBJ=',I6,//,
     &          ' ',T5,'SNAG LIST:',//,
     &          ' ',T5,'  I   IDTREE  SPP ODIA OLEN IYRCOD STATUS '
     &                 'FALLDIR OPROB STDNG',/,
     &          ' ',T5,'---- -------- --- ---- ---- ------ ------ '
     &                 '------- ----- -----' )
C                       XXXX XXXXXXXX XXX XX.X XXX. XXXXXX XXXXXX
C                       XXXXXX. XXXX. XXXX.
        DO I=1,NDEAD
          WRITE(JOSTND,1010) I, OIDTRE(I), ISNSP(I), ODIA(I), OLEN(I),
     &                       IYRCOD(I), ISTATUS(I),
     &                       FALLDIR(I), SPROBS(I,1), SPROBS(I,2)
        ENDDO
C
        NSTNDNG = 0.0
        EXPSNGS = 0.0
        DO I=1,NDEAD
          IF (ISTATUS(I).GT.0 .AND. IYRCOD(I).LE.IYEAR ) THEN
            IF (FALLDIR(I).EQ.-1) NSTNDNG=NSTNDNG+1
            EXPSNGS = EXPSNGS + SPROBS(I,3)
          ENDIF
        ENDDO

        NFFESNGS = 0.0
        DO I=1,NSNAG
          NFFESNGS = NFFESNGS + DENIH(I) + DENIS(I)
        ENDDO
        WRITE (JOSTND,1090) ICYC, IYEAR,
     &                      NDEAD, NSTNDNG, NFFESNGS, EXPSNGS
 1090   FORMAT (' ','IN SVOUT, ICYC=',I2,', IYEAR=',I4,':', / ,
     &          ' ',T5,'NDEAD=',I4,', NSTNDNG=',F5.0,', NFFESNGS=',F5.0,
     &              ', EXPSNGS=',F5.0)
      ENDIF

      IX=0
      DO ISVOBJ=1,NSVOBJ
        IF (IOBJTP(ISVOBJ).EQ.2) IX=IX+1
      ENDDO
      IF (DEBUG) WRITE (JOSTND,*) 'SVOUTNDEAD=',NDEAD,
     >  'DEADOBJECTS=',IX

   50 CONTINUE

C     CLOSE THE SVS TREE DATA FILE FOR THIS IMAGE (NO CLOSE
C     IS DONE IF THE FILE IS A SINGLE LONG FILE).

      IF (NOUT.EQ.JSVPIC) CLOSE (JSVPIC)

      IF (DEBUG) WRITE (JOSTND,60) NSVOBJ,JSVOUT,JSVPIC
   60 FORMAT (' IN SVOUT: END. NSVOBJ=',I6,' JSVOUT=',I4,
     >  ' JSVPIC=',I4)

C     QUICK CHECKS/DEBUG

      IF (DEBUG .AND. IFMCLFG.EQ.3) CALL SVCDBH(WK3,0)

C     END OF QUICK CHECK

      RETURN
      END
