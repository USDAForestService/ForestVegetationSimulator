      SUBROUTINE BABSRAD
C------------------
C SUBROUTINE MODELS THE MOVEMENT AND ABSORPTION OF SOLAR RADIATION
C THROUGH THE CANOPY. TWO MEASURES OF RADIATIONS ARE ACOUNTED FOR: 
C (1) INSTANTANEOUS PAR---CALCULATED A LA HUNT, AS A MAXIMUM AT SOLAR NOON, 
C WHICH IS USED IN THE CALCULATION OF CONDUCTANCE AND PHOTOSYNTHESIS, AND
C (2) TOTAL DAILY SHORTWAVE RADIATION WHICH IS USED TO DRIVE SNOW MELT,
C EVAPORATION, AND TRANSPIRATION.
C
C SUBROUTINE IS CALLED DAILY
C
C NOTE: RADIATION(2)=QNOON in umol/m2/sec before expansion
C       RADIATION(1)=QDAY in kj/m2/day before expansion
C------------------

      INTEGER J, I1
      REAL QDAY, QNOON
      REAL Q1, Q2, Q1DIR, Q2DIR, MAXRAD
      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'

      Q1DIR=0.
      Q2DIR=0.
      SUMCOV=0.

C FIRST CALCULATE THE AMOUNT OF TOTAL DAILY RADIATION INCIDENT AT THE
C TOP OF EACH CANOPY ZONE.  (kJ/m2/day).

      QDAY=XRAD*(1.-S(6))
C NOW CALCULATE THE INSTANTANEOUS INCIDENT PAR AT SOLAR NOON (umol/m2/sec)
      CALL MAXPPFD(JD,TR,SLOPE,ASPECT,LAT,MAXRAD)
      QNOON=MAXRAD
C------------------
C ATTENUATE QDAY AND QNOON THROUGH THE CANOPY ZONES. CALCULATE THE AMOUNT 
C ABSORBED BY EACH ENTITY IN EACH ZONE, AND WHAT THE DIRECT AND DIFFUSE 
C RADIATION IS AT THE TOP OF THE NEXT ZONE.
      Q1=QDAY
      Q2=QNOON
      RADTOP=Q1
      DO 30 L=NUM,1,-1     !canopy layer loop
         I1=INDEX(L)
         I2=INDEX(L+1)
C reference zone according to lower boundary and define light incident         
C at the top of that zone (kj/m2/day and umol/m2/sec)
         J=I1               
         ZQ1(J)=Q1
         ZQ2(J)=Q2
         DO 20 I=1,NB         !entity loop
         IF(LAI(I,J).GT.0.0) THEN
C radiation absorbed by entity=i, zone=j. (kj/day and umol/sec)         
            ZQ1ABS(I,J)=ZQ1(J)*(1-EXP(B2(2)*LAI(I,J)/B2(15)))*BASE(I,J)
            ZQ2ABS(I,J)=ZQ2(J)*(1-EXP(B2(2)*LAI(I,J)/B2(15)))*BASE(I,J)
C accumulate rad across entities within zone and expand. (kj/day & umol/sec)
            TZQ1(J)=TZQ1(J)+ZQ1ABS(I,J)*EXPAND(I)
            TZQ2(J)=TZQ2(J)+ZQ2ABS(I,J)*EXPAND(I)
C accumulate rad across zones for each entity. (kj/day & umol/sec)
            ETZQ1(I)=ETZQ1(I)+ZQ1ABS(I,J)
            ETZQ2(I)=ETZQ2(I)+ZQ2ABS(I,J)
C use write statements to track variables
C           IF(JD.EQ.200) WRITE(*,*)  JD, I, J, QNOON, ZQ2(J), Q2
         ENDIF
   20    CONTINUE
C calculate total light passing to next zone (kj/m2/day & umol/m2/sec)
         Q1=(ZQ1(J)*AREA-TZQ1(J)) / AREA
         Q2=(ZQ2(J)*AREA-TZQ2(J)) / AREA
C set Q1 and Q2 to zero if less than .01
C        IF(Q1.LT.01) Q1=0.0
C        IF(Q2.LT.01) Q2=0.0
C sum cover and accumulate thru layer j.
C note: ZCOVER is defined in subroutine LAIJ. 
         SUMCOV=SUMCOV+ZCOVER(J)
C calculate potential direct beam radiation available to next layer
C note: this is the light in the gaps of the canopy.
         Q1DIR=QDAY*(AREA-SUMCOV)
         Q2DIR=QNOON*(AREA-SUMCOV)
C zero-out potential direct rad. if ZCOVER exceeds 10,000 m2
         IF(Q1DIR.LT.0.) THEN 
            Q1DIR=0.
         ENDIF
         IF(Q2DIR.LT.0.) THEN 
            Q2DIR=0.
         ENDIF
C!!! if the Q?DIR value is needed in a future subroutine,
C!!! do a new variable assignment here. ie, NEW_VAR=Q1DIR.
            
C zero-out dimensioned variables for next day
         TZQ1(J)=0.
         TZQ2(J)=0.
   30 CONTINUE
      RADBOT=Q1
      RETURN
      END


      SUBROUTINE MAXPPFD(JD,TR,SLOPE,ASPECT,LAT,MAXPFD)
C-------------------
C TAKEN DIRECTLY FROM BIOM-BGC. WRITTEN BY E.R. HUNT.
C SUBROUTINE RETURNS THE MIDDAY MAXIMUM PPFD FOR A GIVEN YEARDAY (JD) AND
C TRANSMISIVITY (TR). SLOPE=SLOPE IN PERCENT, ASPECT IN DEGREES,
C LATITUDE IN DEGREES.
C
C DIFFUSE RADIATION AFTER GATES 1980
C      
      INTEGER SOLARCON, AZIMUTH, XDAY
      REAL ASPECT, SLOPE
      REAL HOUR, EPHOTON, RAD2PAR, SLP, SSLP, DECLIN, ZENITH, MU, LAT
      REAL ALT, COSINCID, TRMOD, DIRPPFD, DIFPPFD, TRD, MAXPFD

C-------------------
C DEFINE FUNCTION FOR CONVERTING ANGLES TO RADIANS
C-------------------
      XRADS(X)=X*2.*3.1416/360.
C-------------------
C DEFINE CONSTANTS
C-------------------
C     Solar Noon
      HOUR=0.
C Photosynthetic Photon Energy (umol/J)
      EPHOTON=4.55
C Campbell's PAR is 50% of solar radiation
      RAD2PAR=0.5
C Solar Constant (W/m2)
      SOLARCON=1365
C Azimuth for Northern Hemisphere (degrees)
      AZIMUTH=180
C-------------------
C BEGIN CALCULATION
      SSLP=SLOPE/100.
      SLP=XRADS(90.)-ATAN(SSLP)
C-----get declination in degrees
      IF((JD+9).GT.364) THEN 
         XDAY=JD+9-364
      ELSE 
         XDAY=JD+9
      ENDIF
      DECLIN=23.47*(-COS(XDAY*2.*3.1416/365.))
C-----mu is the cos of the zenith angle
      MU=SIN(XRADS(LAT))*SIN(XRADS(DECLIN))+
     +   COS(XRADS(LAT))*COS(XRADS(DECLIN))*COS(XRADS(HOUR))
C-----get zenith in degrees
      ZENITH=ACOS(MU)*360./(2.*3.1416)
C-----get altitude in radians. Complement to zenith.
      ALT=XRADS(90.-ZENITH)
      COSINCID=COS(SLP)*COS(ALT)*COS(XRADS(AZIMUTH-ASPECT))
     &         +SIN(SLP)*SIN(ALT)
      IF(TR.LT.0.01) THEN TR=0.01
      TRMOD=TR**(1./MU)
      DIRPPFD=SOLARCON*RAD2PAR*EPHOTON*COSINCID*TRMOD
      TRD=0.271-0.294*TRMOD
      IF(TRD.LT.0.01) THEN TRD=0.01
      DIFPPFD=SOLARCON*RAD2PAR*EPHOTON*TRD
C-----calculate total
      MAXPFD=DIRPPFD+DIFPPFD
      RETURN
      END



