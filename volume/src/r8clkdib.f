C----------
C VOLUME $Id: r8clkdib.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C YW 2016/03/08 Modofied the DIB calculation for TOPHT less than 17.3 to avoid Nan vaue error.
      SUBROUTINE R8PREPCOEF(VOLEQ, COEFFS, ERRFLAG)
      USE CLKCOEF_MOD
      
      implicit none
      TYPE(CLKCOEF):: COEFFS
      CHARACTER*10 VOLEQ
      INTEGER ERRFLAG

C      *** LOCAL VARIABLES ***
      INTEGER SPEC,GEOA,EQN,SPECPR,GEOAPR,EQNPR,SPGRP
      INTEGER PTR,FIRST,LAST,HALF,GSPEC,CHECK,DONEFLAG,LASTFLAG
      INTEGER DIBCNT,TOTCNT,FOURCNT,NINECNT
      INCLUDE 'R8CLKCOEF.INC'
      INCLUDE 'R8DIB.INC'

      SPECPR = 0
      EQNPR = 0
      GEOAPR = 0

      READ(VOLEQ(8:10),'(I3)')SPEC
      READ(VOLEQ(2:2),'(I1)')GEOA

      IF (GEOA.LT.1 .OR. GEOA.GT.9 .OR. GEOA.EQ.8)THEN
         ERRFLAG = 1
         GO TO 999
      ENDIF

      READ(VOLEQ(3:3),'(I1)')EQN
      IF(EQN. NE. 0 .AND. EQN .NE. 4 .AND. EQN .NE. 7 .AND. EQN .NE. 9
     > .AND. EQN .NE. 8) THEN
        ERRFLAG = 1  
        GO TO 999
      ENDIF

      IF (SPEC.EQ.123 .OR. SPEC.EQ.197) THEN
         SPEC = 100
      ELSEIF  (SPEC.EQ.268) THEN       
         SPEC = 261                     
      ELSEIF  (SPEC.EQ.313 .OR. SPEC.EQ.314 .OR. SPEC.EQ.317 .OR.
     >          SPEC.EQ.650 .OR. SPEC.EQ.651. OR.
     >          SPEC.EQ.691 .OR. SPEC.EQ.711 .OR. SPEC.EQ.742 .OR.
     >          SPEC.EQ.762 .OR. SPEC.EQ.920 .OR. SPEC.EQ.930 .OR.
     >          SPEC.EQ.545 .OR. SPEC.EQ.546) THEN
         SPEC = 300
      ELSEIF  (SPEC.EQ.521 .OR. SPEC.EQ.550 .OR. SPEC.EQ.580 .OR.
     >          SPEC.EQ.601 .OR. SPEC.EQ.602 .OR. SPEC.EQ.318) THEN
         SPEC = 500
      ELSEIF  (SPEC.EQ.804 .OR. SPEC.EQ.817 .OR. SPEC.EQ.820 .OR.
     >          SPEC.EQ.823 .OR. SPEC.EQ.825. OR.
     >          SPEC.EQ.826 .OR. SPEC.EQ.830 .OR. SPEC.EQ.834) THEN
         SPEC = 800
      ENDIF

      IF (SPEC.NE.SPECPR .OR. EQN.NE.EQNPR .OR. GEOA.NE.GEOAPR) THEN
        SPECPR = SPEC
        EQNPR = EQN
        GEOAPR = GEOA
        GSPEC = GEOA*1000 + SPEC

C     BINARY SEARCH FOR CORRECT COEFFICIENTS
      DONEFLAG = 0
      LASTFLAG = 0
      FIRST = 1
      LAST = 182
      DO 5, WHILE (DONEFLAG.EQ.0)
         IF(FIRST.EQ.LAST) LASTFLAG = 1
C  DETERMINE WHERE TO CHECK
          HALF=((LAST-FIRST+1)/2) + FIRST   

          CHECK= INT(R8CF(HALF,1)*1000. + R8CF(HALF,2))
C  FOUND THE COEFFECIENTS
          IF(GSPEC.EQ.CHECK)THEN      
             PTR = HALF
             DONEFLAG=1
C  MOVE DOWN THE LIST
          ELSEIF(GSPEC.GT.CHECK)THEN  
             FIRST = HALF
C  MOVE UP THE LIST
          ELSEIF(GSPEC.LT.CHECK)THEN   
             LAST = HALF - 1
          ENDIF
C  DID NOT FIND A MATCH
          IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0)THEN   
C  END ROUTINE, NO MATCH
             IF(GEOA.EQ.9) THEN   
                SPECPR = 0
                EQNPR = 0
                GEOAPR = 0
                ERRFLAG = 6
                GO TO 999
C  SET GEOAPR TO 9 AND RETRY THE SEARCH
             ELSE                  
                GEOA = 9
                GSPEC = GEOA*1000 + SPEC
                FIRST = 1
                LAST = 182
                LASTFLAG = 0
             ENDIF
          ENDIF     
   5  CONTINUE

C     END BINARY SEARCH

      SPGRP = INT(R8CF(PTR,3) + .5)                
      IF (SPGRP.NE.100 .AND. SPGRP.NE.300 .AND. SPGRP.NE.500)THEN
        ERRFLAG = 6
        GO TO 999
      ENDIF

      COEFFS%A4 = R8CF(PTR,4)
      COEFFS%B4 = R8CF(PTR,5)

      DONEFLAG = 0
      LASTFLAG = 0
      FIRST = 1
      LAST = 49
      DO 10, WHILE (DONEFLAG.EQ.0)
         IF(FIRST.EQ.LAST) LASTFLAG = 1
C  DETERMINE WHERE TO CHECK
         HALF=((LAST-FIRST+1)/2) + FIRST   
          
C  FOUND THE COEFFECIENTS
         IF((INT(DIBMEN(HALF,1)+.5)).EQ.SPEC) THEN      
            DIBCNT = HALF
            DONEFLAG=1
C  MOVE DOWN THE LIST
         ELSEIF((INT(DIBMEN(HALF,1)+.5)).GT.SPEC)THEN  
            LAST = HALF -1
C  MOVE UP THE LIST
         ELSEIF((INT(DIBMEN(HALF,1)+.5)).LT.SPEC)THEN   
            FIRST = HALF
         ENDIF
C  DID NOT FIND A MATCH
            IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0) THEN
               ERRFLAG = 6
               GO TO 999
            ENDIF   
   10 CONTINUE
C     END BINARY SEARCH

      IF (EQN.EQ.0.OR.EQN.EQ.4 .OR. EQN.EQ.8) THEN
         COEFFS%FIXDI = DIBMEN(DIBCNT,2)
C         FIXDI = DIBMEN(DIBCNT,2)
      ELSE
         COEFFS%FIXDI = DIBMEN(DIBCNT,3)
C         FIXDI = DIBMEN(DIBCNT,3)
      ENDIF

C***********************************************************************
      IF (EQN.EQ.0 .OR. EQN.EQ.8) THEN
         DONEFLAG = 0
         LASTFLAG = 0
         FIRST = 1
         LAST = 49
         DO 20, WHILE (DONEFLAG.EQ.0)
            IF(FIRST.EQ.LAST) LASTFLAG = 1
C  DETERMINE WHERE TO CHECK
            HALF=((LAST-FIRST+1)/2) + FIRST   
          
C  FOUND THE COEFFECIENTS
            IF((INT(TOTAL(HALF,1)+.5)) .EQ. SPEC)THEN      
               TOTCNT = HALF
               DONEFLAG=1
C  MOVE DOWN THE LIST
            ELSEIF((INT(TOTAL(HALF,1)+.5)).GT.SPEC)THEN  
               LAST = HALF -1
C  MOVE UP THE LIST
            ELSEIF((INT(TOTAL(HALF,1)+.5)).LT.SPEC)THEN   
               FIRST = HALF 
            ENDIF
C  DID NOT FIND A MATCH
            IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0)THEN
               ERRFLAG = 6
               GO TO 999   
            ENDIF
   20    CONTINUE

         COEFFS%R = TOTAL (TOTCNT,2)
         COEFFS%C = TOTAL (TOTCNT,3)
         COEFFS%E = TOTAL (TOTCNT,4)
         COEFFS%P = TOTAL (TOTCNT,5)
         COEFFS%B = TOTAL (TOTCNT,6)
         COEFFS%A = TOTAL (TOTCNT,7)
         COEFFS%A17 = R8CF(PTR,14)               
         COEFFS%B17 = R8CF(PTR,15)               
         
      ELSEIF (EQN.EQ.4) THEN
         DONEFLAG = 0
         LASTFLAG = 0
         FIRST = 1
         LAST = 49
         DO 40, WHILE (DONEFLAG.EQ.0)
            IF(FIRST.EQ.LAST) LASTFLAG = 1
C  DETERMINE WHERE TO CHECK
            HALF=((LAST-FIRST+1)/2) + FIRST   
          
C  FOUND THE COEFFECIENTS
            IF((INT(FOUR(HALF,1)+.5)) .EQ. SPEC)THEN
               FOURCNT = HALF
               DONEFLAG=1
C  MOVE DOWN THE LIST
            ELSEIF((INT(FOUR(HALF,1)+.5)).GT.SPEC)THEN
               LAST = HALF -1
C  MOVE UP THE LIST
            ELSEIF((INT(FOUR(HALF,1)+.5)).LT.SPEC)THEN
               FIRST = HALF
            ENDIF
C  DID NOT FIND A MATCH
            IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0) THEN
               ERRFLAG = 6
               GO TO 999
            ENDIF
   40    CONTINUE

         COEFFS%R = FOUR (FOURCNT,2)
         COEFFS%C = FOUR (FOURCNT,3)
         COEFFS%E = FOUR (FOURCNT,4)
         COEFFS%P = FOUR (FOURCNT,5)
         COEFFS%Q = FOUR (FOURCNT,6)
         COEFFS%A17 = R8CF(PTR,8)
         COEFFS%B17 = R8CF(PTR,9)

      ELSEIF (EQN.EQ.7) THEN

         I = 1
 60      IF (INT(SEVEN(I,1)+.5).LT.SPEC .AND. I.LE.15) THEN
            I = I + 1
            GO TO 60
         ELSEIF (INT(SEVEN(I,1)+.5).GT.SPEC .OR. I.GT.15) THEN
            ERRFLAG = 6
            GO TO 999
         ENDIF

         COEFFS%R = SEVEN (I,2)
         COEFFS%C = SEVEN (I,3)
         COEFFS%E = SEVEN (I,4)
         COEFFS%P = SEVEN (I,5)
         COEFFS%Q = SEVEN (I,6)
         COEFFS%A17 = R8CF(PTR,10)
         COEFFS%B17 = R8CF(PTR,11)


      ELSEIF (EQN.EQ.9) THEN

         DONEFLAG = 0
         LASTFLAG = 0
         FIRST = 1
         LAST = 34
         DO 80, WHILE (DONEFLAG.EQ.0)
            IF(FIRST.EQ.LAST) LASTFLAG = 1
C  DETERMINE WHERE TO CHECK
            HALF=((LAST-FIRST+1)/2) + FIRST
          
C  FOUND THE COEFFECIENTS
            IF((INT(NINE(HALF,1)+.5)) .EQ. SPEC)THEN
               NINECNT = HALF
               DONEFLAG=1
C  MOVE DOWN THE LIST
            ELSEIF((INT(NINE(HALF,1)+.5)).GT.SPEC)THEN
               LAST = HALF -1
C  MOVE UP THE LIST
            ELSEIF((INT(NINE(HALF,1)+.5)).LT.SPEC)THEN
               FIRST = HALF
            ENDIF
C  DID NOT FIND A MATCH
            IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0) THEN
               ERRFLAG = 6
               GO TO 999
            ENDIF
   80    CONTINUE
         COEFFS%R = NINE(NINECNT,2)
         COEFFS%C = NINE(NINECNT,3)
         COEFFS%E = NINE(NINECNT,4)
         COEFFS%P = NINE(NINECNT,5)
         COEFFS%Q = NINE(NINECNT,6)
         COEFFS%A17 = R8CF(PTR,12)
         COEFFS%B17 = R8CF(PTR,13)

      ENDIF
      
      ENDIF
 999  CONTINUE
 
      RETURN
      END

C-------------------------------------------------------------------------------------
      SUBROUTINE R8CLKDIB(VOLEQ, FORST, DBHOB, HTTOT, UPSHT1,HTUP,DIB, 
     &                    ERRFLAG)
      USE CLKCOEF_MOD
      
      implicit none
      TYPE(CLKCOEF):: COEFFS
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST
      CHARACTER*1 CTYPE
      CHARACTER*2 CDANUW
      INTEGER ERRFLAG, EQN, IS, IB, IT, IM
      REAL DBHOB, HTTOT, HTUP, DIB, DIB17, DBHIB, TOPHT, HIGHHT,UPSHT1
      REAL R,C,E,P,B,A,Q, A17, B17, DX, AD, BD, VOLTMP(15)
      REAL HTONE, HTTWO, MTOPP

c      INCLUDE 'R8CLKCOEF.INC'
c      INCLUDE 'R8DIB.INC'
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      CDANUW(1:2) = FORST(1:2)
C

      READ(VOLEQ(3:3),'(I1)')EQN
C GET COEF FOR THE EQUATION
      CALL R8PREPCOEF(VOLEQ, COEFFS, ERRFLAG)

      R=COEFFS%R
      C=COEFFS%C
      E=COEFFS%E
      P=COEFFS%P
      B=COEFFS%B
      A=COEFFS%A
      Q=COEFFS%Q
      AD=COEFFS%A4
      BD=COEFFS%B4
      B17=COEFFS%B17
      A17=COEFFS%A17
      DX=COEFFS%FIXDI

      DBHIB=AD+BD*DBHOB

C--   Set height indicator variables
      IS=0
      IB=0
      IT=0
      IM=0

      IF(HTUP.LT.4.5) IS=1
      IF(HTUP.GE.4.5.AND.HTUP.LE.17.3) IB=1
      IF(HTUP.GT.17.3) IT=1
      IF(HTUP.LT.(17.3+A*(HTTOT-17.3))) IM=1

      HIGHHT = HTUP
C-----Get DIB at specified height---------------------------------------
C--     Total height provided (eqn 1)

      IF (EQN.EQ.0 .OR. EQN.EQ.8) THEN
        TOPHT = HTTOT
        DIB17=DBHOB*(A17+B17*(17.3/TOPHT)**2)

        dib=(is*(dbhib**2*(1+(c+e/dbhib**3)*((1-highht/topht)**r
     &       -(1-4.5/topht)**r)/(1-(1-4.5/topht)**r)))
     &     +ib*(dbhib**2-(dbhib**2-dib17**2)*((1-4.5/topht)**p
     &       -(1-highht/topht)**p)/((1-4.5/topht)**p-(1-17.3/topht)**p))
     &     +it*(dib17**2*(b*(((highht-17.3)/(topht-17.3))-1)**2
     &     +im*((1-b)/a**2)*(a-(highht-17.3)/(topht-17.3))**2)))**0.5

       ELSE
C--    HEIGHT TO 4, 7 OR 9 TOP DOB
         IF(UPSHT1.LT.0.1)THEN
c           SI = 0
c           BA = 0
c           CALL R8_MHTS(FORST,VOLEQ,DBHOB,HTTOT,SI,BA,UPSHT1,ERRFLAG)
C CALL R8VOL2 TO GET THE UPSHT1. THIS WILL BE SAME AS FVS
           HTONE = 0
           HTTWO = 0
           CTYPE = 'F'
           CALL R8VOL2(VOLEQ,VOLTMP,DBHOB,HTONE,HTTWO,MTOPP,HTTOT,CTYPE,
     >                  ERRFLAG)
           UPSHT1 = HTTWO
         ENDIF
         TOPHT = UPSHT1
         DIB17=DBHOB*(A17+B17*(17.3/TOPHT)**2)
c         dib=(is*(dbhib**2*(1+(c+e/dbhib**3)*((1-highht/topht)**r
c     &       -(1-4.5/topht)**r)/(1-(1-4.5/topht)**r)))
c     &     +ib*(dbhib**2-(dbhib**2-dib17**2)*((1-4.5/topht)**p
c     &       -(1-highht/topht)**p)/((1-4.5/topht)**p-(1-17.3/topht)**p))
c     &     +IT*(DIB17**2-(DIB17**2-DX**2)*(1-((TOPHT-HTUP)/
c     &      (TOPHT-17.3))**Q)))**0.5
C The codes below replaces the above comment out codes for NaN problem
C when TOPHT < 17.3 (YW 2016/03/08)
         IF(IS.GT.0)THEN
           dib=(is*(dbhib**2*(1+(c+e/dbhib**3)*((1-highht/topht)**r
     &       -(1-4.5/topht)**r)/(1-(1-4.5/topht)**r))))**0.5
         ELSEIF(IB.GT.0)THEN
           IF(TOPHT.GT.17.3)THEN
           dib = (ib*(dbhib**2-(dbhib**2-dib17**2)*((1-4.5/topht)**p
     &           -(1-highht/topht)**p)/((1-4.5/topht)**p
     &           -(1-17.3/topht)**p)))**0.5
           ELSE
             IF(highht.le.topht)THEN
               dib = (dbhib**2-(highht-4.5)/(topht-4.5)
     &               *(dbhib**2-DX**2))**0.5
             ELSEIF(HTTOT.GT.0)THEN
               dib = (DX**2-(highht-topht)/(httot-topht)*DX**2)**0.5
             ENDIF
           ENDIF
         ELSEIF(IT.GT.0)THEN
           IF(TOPHT.GT.17.3)THEN
           dib = (IT*(DIB17**2-(DIB17**2-DX**2)*(1-((TOPHT-HTUP)/
     &      (TOPHT-17.3))**Q)))**0.5
           ELSE
           dib = (DX**2-(highht-topht)/(httot-topht)*DX**2)**0.5
           ENDIF
         ENDIF
       ENDIF
       RETURN
       END
       
      