!== last modified  09-14-2007
      SUBROUTINE FWINIT(VOLEQ,DBHOB,HTTOT,MHT,MTOPP,UPSHT1,UPSHT2,
     >     UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,JSP,RHFW,RFLW,
     >     TAPCOE,F,SETOPT,NEXTRA,HEX,DEX,ZEX,PINV_Z,FMOD,btr,FCLASS,
     >     ERRFLAG)
C**************************************************************
c        Initialiazes SF Subroutines using Flewelling profile models
c        Uses SF_TEST2 initializing code 
c                  J. Flewelling  July, 1996
c                  Modified by K. Cormier Sept, 1996

C     TREE VARIABLES
      REAL DBHOB,HTTOT,DBTBH,MHT,MTOPP,btr

C     3PT VARIABLES
      REAL UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
      REAL*4 HEX(2),DEX(2),ZEX(2),FMOD(3),PINV_Z(2)
      REAL*4 RHFW(4),RFLW(6),TAPCOE(12),F 

      INTEGER JSP, SETOPT(6),NEXTRA,htref,ERRFLAG,ICON,FCLASS,threeflag
C      LOGICAL METRIC
      Character*1 GEOCODE
c     character*1 unit
      Character*2 GEOSUB
      CHARACTER*3 SPEC
      CHARACTER*10 VOLEQ

c    Units in english or metric (E=english)       
c      UNIT='E'
C                              use default settings
c    Apply constraints to 3-pt fit (1=yes/0=no)
      ICON=1
c    Stem form: user regin when available (1=yes/0=no)
C         IREGIONU(1)=1
c    Bark thickness: user regin when available (1=yes/0=no)
C         IREGIONU(2)=1
C
c    Upper stem measurement (1=DOB/2=DIB)
C      IDTYPE=1 
c    Supplimental regression use intercept (1=yes/0=no)
C      IMOD_USE=1
C                Line from SF-SETERR
C      IER_UNIT=6
c                                                Other setup features
c                                                Note: Call changed from 1994.
C         FOLLOWING FROM SF_SET
C      if (unit.eq.'E' .or. unit.eq.'e') then
C          METRIC  = .false.
C          BH=4.5
C      else if  (unit.eq.'M' .or. unit.eq.'m') then
C          METRIC=.TRUE.
C          BH = 4.5 * 0.3048
C      else  
C          IER=1
C      endif

c           set an upper bound on fractional changes; 
c           predictions can exceed this only if measured data also exceeds.
      SETOPT(1)=ICON
c                              Region effects on bark predictions
      SETOPT(2)=1
      SETOPT(3)=1
       
c                          The following white noise option is ALWAYS set
c                            to zero (off). For future development.
      SETOPT(4)=0
c                           Numerical volume segments
c     SETOPT(5)= NVSEGU 
c     SETOPT(6)= IVOLINT
 
c                       The following is the maximum allowed 3-pt fractional
c                         change in DBHOB. Unless a greater change is observed,
c                         this will become FMODMAX  
C      SETC(1) = .15

c
c                               Prepare for modifier regressions
c      INT_USE = IMOD_USE
c     CALL SF_MODR0
C                                INGY
      ZEX(1) = 0
      ZEX(2) = 0
      HEX(1) = 0
      HEX(2) = 0
      DEX(1) = 0
      DEX(2) = 0
      NEXTRA = 0
      THREEFLAG = 0

      IF(BTR.GT.0.0 .AND. DBTBH.LE.0) DBTBH = DBHOB-(DBHOB*BTR/100.0)

      GEOCODE = VOLEQ(1:1)
      GEOSUB = VOLEQ(2:3)
      SPEC = VOLEQ(8:10)

C       ERROR CHECK THE GEOSUB CODE
      IF(GEOSUB.EQ.'OO' .OR. GEOSUB.EQ.'oo') GEOSUB = '00'
      
      IF(GEOCODE.EQ.'I' .OR. GEOCODE.EQ.'i' .OR. GEOCODE.EQ.'1') THEN
        IF (SPEC.EQ.'202'.or.spec.eq.'205'.or.spec.eq.'204') THEN
c     Douglas fir
          JSP = 11
        ELSEIF(SPEC.EQ.'073'.or.SPEC.EQ.'070') THEN
c     Western Larch
          JSP = 12
        ELSEIF(SPEC.EQ.'017') THEN
c     Grand fir
          JSP = 13
        ELSEIF(SPEC.EQ.'122') THEN
C     Ponderosa pine
          JSP = 14
        ELSEIF(SPEC.EQ.'108') THEN
c     Lodgepole pine        
          JSP = 15
        ELSEIF(SPEC.EQ.'242'.or.SPEC.EQ.'240') THEN
c     Western Red Cedar        
          JSP = 16
        ELSEIF(SPEC.EQ.'260'.or.SPEC.EQ.'263'.OR.SPEC.EQ.'264') THEN
c     Mountain Hemlock
          JSP = 17
        ELSEIF(SPEC.EQ.'119') THEN
c     White pine
          JSP = 18
        ELSEIF(SPEC.EQ.'093'.or.SPEC.EQ.'090') THEN
c     Engelmann Spruce
          JSP = 19
        ELSEIF(SPEC.EQ.'019') THEN
c     Subalpine fir
          JSP = 20
        ELSEIF(SPEC.EQ.'012') THEN
c     Balsam fir
          JSP = 21
        ELSE
          ERRFLAG = 1
          RETURN
        ENDIF

C                                WESTSIDE SPECIES
      ELSEIF(GEOCODE.EQ.'F' .OR. GEOCODE.EQ.'f') THEN
        IF (SPEC.EQ.'202'.or.spec.eq.'205'.or.spec.eq.'204') THEN
c     Douglas fir
          JSP = 3 
        ELSEIF(SPEC.EQ.'263') THEN
c     Western Hemlock
          JSP = 4
        ELSEIF(SPEC.EQ.'242') THEN
c     Western Red Cedar        
          JSP = 5
        ELSE
          ERRFLAG = 1
          RETURN
        ENDIF

C                                 REGION 2      
      ELSE IF(GEOCODE.EQ.'2') THEN
        IF(SPEC.EQ.'122') THEN
c         Black Hills model
          IF(GEOSUB.EQ.'03') THEN
            JSP = 22
          ELSE
c         region wide and san juan
              JSP = 23
          ENDIF
        ELSEIF(SPEC.EQ.'108') THEN
c         Lodgepole model
           JSP=25
        ELSEIF(SPEC.EQ.'202') THEN
c         Douglas Fir model
           JSP = 26
        ELSEIF(SPEC.EQ.'015') THEN
c         White fir model
           JSP = 27
        ELSE IF(SPEC.EQ.'746')THEN
c         Aspen model
	     JSP = 28
	  ELSE
          ERRFLAG = 1
          RETURN
        ENDIF
C                                 REGION 4
      ELSE IF(GEOCODE.EQ.'4') THEN
        IF(GEOSUB.EQ.'07')THEN
          IF(SPEC.EQ.'093') THEN
c         Engelmann spruce model
            JSP = 24
          ELSE IF(SPEC.EQ.'122') THEN
c         R2 Ponderosa Pine model with R4 bark
            JSP = 23
          ELSE
            ERRFLAG = 1
            RETURN
          ENDIF
        ELSE
          ERRFLAG = 1
          RETURN
        ENDIF
C                                 REGION 3
C     REGION 3
      ELSE IF (GEOCODE.EQ.'3') THEN
c         IF(GEOSUB.EQ.'01') THEN
            IF(SPEC.EQ.'122') JSP = 29
c         ENDIF
C                                 REGION 10
      ELSE IF(GEOCODE.EQ.'A') THEN
          IF(SPEC.EQ.'042') THEN
c         Alaska yellow cedar
            JSP = 31
          ELSE IF(SPEC.EQ.'242') THEN
c         Western Red Cedar
            JSP = 32
          ELSE IF(SPEC.EQ.'098') THEN
c         Spruce
            IF(GEOSUB.EQ.'02') THEN
	        JSP=35
            ELSE
              JSP = 33
	      ENDIF
          ELSE IF(SPEC.EQ.'263'.OR.SPEC.EQ.'260'.OR.SPEC.EQ.'264') THEN
c         Hemlock
            IF(GEOSUB.EQ.'02') THEN
	        JSP=36
            ELSE
              JSP = 34
    	      ENDIF
          ELSE
            ERRFLAG = 1
            RETURN
          ENDIF
      ELSE
        ERRFLAG = 1
        RETURN
      ENDIF

C-------------------------- end of set-up phase ------------------------------
c
c      WRITE(*,*)'FWINIT',JSP
C-----------------------------------------------------------------------------
C                 TWO POINT MODEL INITIALIZE,
C    check for total height
      IF(HTTOT.LE.0 .AND. MHT.GT.0) THEN
        CALL SF_2PTH(JSP,GEOSUB,SETOPT,DBHOB,MHT,MTOPP,DBTBH,
     >         HTTOT,HEX,ZEX,F,RHFW,RFLW,TAPCOE)
        if(httot .le. 0) then   !added  10/4/2001
           errflag = 11
           return
        endif
     	  if(upsht1 .gt. 0) threeflag = 1
      ELSE
         CALL sf_2pt(JSP,GEOSUB,SETOPT,DBHOB,HTTOT,DBTBH,F,RHFW,
     >                                              RFLW,TAPCOE)
      ENDIF
C          if (ierr.gt.0) then
C            write(6,106)  IERR
C106         format(' SF_2pt returned error code ', I6)
C            GOTO 1000
C          endif 

C-----------------------------------------------------------------------------
C                                                    THREE POINT MODEL INITIALIZE
c                                                       Specify upper dob(s)
      IF(VOLEQ(6:6).EQ.'3'.OR.THREEFLAG.EQ.1)THEN
        IF (htref.gt.0)then
            NEXTRA = 1
            upsht1 = float(htref)*HTTOT/100.0
        ELSE IF(UPSHT1.GT.0.0) THEN
           IF(UPSHT2 .GT. 0.0) THEN
               NEXTRA = 2
           ELSE
               NEXTRA = 1
           ENDIF
	  ELSE IF(FCLASS .GT. 0)THEN
	     NEXTRA = 1
           UPSHT1 = 17.5
           UPSD1 = ANINT(DBHOB*FCLASS) / 100.0
        ENDIF
        
        IF (NEXTRA .GT. 0) THEN
          HEX(1) = UPSHT1
          HEX(2) = UPSHT2
c                                                      Specify an upper dib
          IF (AVGZ1 .ne. 0.0) THEN
            ZEX(1) = AVGZ1
            ZEX(2) = AVGZ2

            CALL SF_3z(JSP,GEOSUB,SETOPT,NEXTRA,DBHOB,HTTOT,HEX,ZEX,
     >                       RFLW,RHFW,TAPCOE,F,PINV_Z,FMOD)

          ELSEIF(UPSD1 .GT. 0.0) THEN
            DEX(1) = UPSD1
            DEX(2) = UPSD2

            CALL sf_3pt(JSP,GEOSUB,setopt,DBHOB,HTTOT,DBTBH,NEXTRA,
     >                  HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,PINV_Z,FMOD) 
c                                                       Specify upper z(s)
          ENDIF
        ELSE
          ERRFLAG = 9
        ENDIF
      ENDIF
C                    END OF INTIALIZATION ROUTINE
      RETURN
      END  

C JSP VALUES
C
C    WESTSIDE:              3 - 5
C    INGY:                 11 - 21
c      Douglas fir            11
c      Western Larch          12
c      Grand fir              13
C      Ponderosa pine         14
c      Lodgepole pine         15
c      Western Red Cedar      16
c      Mountain Hemlock       17
c      White pine             18
c      Engelmann Spruce       19
c      Subalpine fir          20
c      Balsam fir             21
c
C    BLACK HILLS              22
C    SAN JUAN & R2 PP         23
C    DIXIE ES                 24
C    R2 LODGEPOLE             25
C    R2 DOUG FIR              26
C    R2 WHITE FIR             27 
C    R10 ALASKA CEDAR         31
C    R10 WESTERN RED CEDAR    32
C
C
C

