      SUBROUTINE BWATER
C------------------
C SUBROUTINE WATER CREATES THE PRECIPITATION AND SOIL WATER VARIABLES
C NEEDED IN SUBSEQUENT CALCULATIONS. CALL ON DAILY BASIS
C    AFTER HUNT, 1993.
C ADDING NEW VARS TO DAYWATER.OUT AJM 1/04
C------------------
C    Z1(15) is min. night temperature
C    Z1(3) is precipitation (m)
C    Z1(14) is daylight average air temperature (C)
C    Z1(8) is daily shortwave radiation (kj/day)
C    Z1(16) vapor pressure deficit (mbar)
C    Z1(18) daylength (seconds)
C    B2(1) is interception coefficient. Was B(4).      
C    S(5) is snowmelt coefficient
C----------------------------

      INTEGER YEAR, DSRAIN
      REAL TOTPPT(60), RADMELT, RADTRANS
      REAL EVAPPT, X, LATHEAT, EVAPRAD, EXCESS, GSOIL, GB
      REAL EVAP, DAYCONST, MAXOUT(10), PENMON, MAXH2O(10), VCONT(10)
      REAL VCONT_FC, SWC_FC(10), GRAVOUT(10) !New vars for saturation-to-FC adjustments ajm 5/01

      INCLUDE 'ENTITY.F77'    ! Structure, etc.
      INCLUDE 'SITE.F77'      ! S() variables

      NSL=2             !*!  (added 10/97 to implement soil layers -epv)

C---If leaves on then total LAI is sum of live and dead standing LAI
C   All LAI numbers are in subroutine LAIJ. 
C   SITELAI is sum LA(I,J)/AREA      
C      IF (LEAF_ON) THEN SITELAI=LIVELAI+DEADLAI
C      ELSE SITELAI=DEADLAI
C---Initialize SWC & DAYTRANS. SNOWPACK initialized in ZCOMP.
C   SWC & SNOWPACK are initially defined in SITE.DAT.
C   DAYTRANS is updated at the end of each day in GSV.FOR with
C   DAYTRANS=SUMTRANS right before SUMTRANS is zeroed for the next day.
      YEAR=Z1(1)
      IF(YEAR.EQ.1 .AND. JD.EQ.1) THEN 
        SWC=S(1)
        DAYTRANS=0.0
        DAYTRANS_GR=0.0         !*!
        DAYTRANS_SH=0.0           !added 10/00 ajm
        DAYTRANS_SM=0.0         !*!
        DAYTRANS_LG=0.0         !*!
        DAYTRANS_LG1=0.0        !*!
        DAYTRANS_LG2=0.0        !*!        

        DO 10  SL=1,NSL         !*!
          SWC(SL) = S(1)/NSL    !*!  divides total SWC equally between layers
          SD(SL)  = S(2)/NSL    !*!  divides total SD equally between layers
   10   CONTINUE                !*!

      ENDIF
C---Determine whether precip is rain or snow
      IF (Z1(15).GT.0.0) THEN     !RAIN gets interception removed
        RAIN=(Z1(3) - SITELAI * B2(1)) * AREA
        IF (RAIN.LT.0.0) RAIN=0.0
        SNOW=0.0
      ELSE
        SNOW=Z1(3)*AREA
        IF(SNOW.LT.0.0) SNOW=0.0
        RAIN=0.0
      ENDIF
C---Accumulate daily precipitation (YEAR=Z1(1) & is defined in ZCOMP.FOR)
      TOTPPT(YEAR)=TOTPPT(YEAR)+Z1(3)
C---Temperature based snowmelt
      SNWMELT=S(5) * Z1(14) * AREA
      SNWMELT=MAX(SNWMELT,0.0)
C---Radiation based snowmelt (Coughlan, Ph.D thesis, 1991)
C   latent heat of fusion in Kj/m m2
C   0.6=1-albedo of snow
C   RADTRANS=ZQ1(INDEX(1)) is radiation incident at top of bottom layer. 
C   calculated in subroutine ABSRAD.
      RADTRANS=ZQ1(INDEX(1))
      IF(Z1(14).GE.0.0) THEN
        RADMELT=0.6 * (RADTRANS/3.472E5) * AREA
        SNWMELT=MAX(RADMELT,0.0) + SNWMELT
      ELSE
        RADMELT=0.6 * (RADTRANS/2.84512E6) * AREA
        SNWMELT=MAX(RADMELT,0.0)
      ENDIF
C---Update SNOWPACK        
      SNOWPACK=SNOWPACK+SNOW
C---Make sure don't melt more snowpack than exists
      IF((SNOWPACK-SNWMELT).LT.0.0) SNWMELT=SNOWPACK
      SNOWPACK=SNOWPACK-SNWMELT
C---Calculate potential evaporation of intercepted PPT
      EVAPPT=(Z1(3)*AREA) - RAIN - SNOW
C---Calculate potential evaporation with radiation limit. 
C   First calculate latent heat of fusion, LATHEAT.
      X=2501.2 - 2.3787 * Z1(15)
      LATHEAT=X*1000.
      EVAPRAD=(Z1(8)/LATHEAT)*AREA
      IF(EVAPPT.LT.0.0) EVAPPT=0.0
      IF(EVAPPT.GT.EVAPRAD) THEN
        EXCESS=EVAPPT-EVAPRAD
        EVAPPT=EVAPRAD
      ELSE
        EXCESS=0.0
      ENDIF
C---Calculate bare soil evaporation using Penmon-Monteith equation. Soil
C   is wet when it has rained or when snowmelt
      IF(RAIN.GT.0.0 .OR. SNWMELT.GT.0.0) THEN
        DSRAIN=0
      ELSE 
        DSRAIN=DSRAIN+1
      ENDIF
C---Set gsoil constant according to days since last rain or snowmelt
      SELECT CASE(DSRAIN)
        CASE(0)
          GSOIL=0.002        ! 2mm/s data from Baldocchi and Meyers
        CASE(1)
          GSOIL=0.001
        CASE(2)
          GSOIL=0.0005
        CASE(3)
          GSOIL=0.00025
        CASE(4)
          GSOIL=0.000125
        CASE(5)
          GSOIL=0.000063
        CASE(6)
          GSOIL=0.00001
        CASE DEFAULT
          GSOIL=0.000001
      END SELECT
C---Call Penmon-Monteith function
      GB=0.001       ! boundary layer conductance for soil
      CALL BPENMAN(Z1(14),Z1(16),ZQ1(INDEX(1)),GB,GSOIL,
     +            Z1(18),PENMON)
      EVAP=PENMON
      DAYCONST=0.85
      EVAP=EVAP*AREA*Z1(18)*DAYCONST
C---Put ppt limit on evaporation      
      IF(RAIN.GT.0.0 .AND. EVAP.GT.(0.5*RAIN)) THEN
        EVAP=0.5*RAIN
        DSRAIN=4
      ENDIF
      IF(SNOW.GT.0.0) THEN                           
        EVAP=0.0
        DSRAIN=0
      ENDIF
C---Calculate ground water outflow. SWC is input at start
C   of run. SWC is updated daily.


c         WRITE(*,999) JD,RAIN,SNWMELT,EVAP,SWC(1),DAYTRANS
c 999     FORMAT(I4,5(1X,F8.2))
c------------------------------------------------------------------------------------

C--- VERS 6 -------------------------------------------------------------------------
C--- Partition daily precip into the different soil layers   !*!

              
      DO 20   SL=1,NSL
         MAXH2O(SL)=S(3) * SD(SL) * AREA

         IF (MAXSL.EQ.1) THEN
           TF1=1        ! TF1 = true factor 1
           TF2=0        ! TF2 = true factor 2
         ELSE
           TF1=0
           TF2=1
         ENDIF
C--- update soil water content, SWC.
C--- DAYTRANS is the transpiration of all plants the day before.  
C--- 1st layer = rain +1/2 snow +excess -evap -SM_trans -Lg_trans -out(1)
C--- 2nd layer = 1/2 snow +out(1) -LG_trans -out(2)  
C
C*****Major logic change being made 5/24/01 AJM.
C   First, we will remove yesterday's transpiration.
C   Then we will evaluate water content; if greater than FIELD CAPACITY (FC)
C   then SWC will be reduced by 1/2 the difference between FC and the SWC
C   That is, we're--in essence--draining off 1/2 the gravitational water present every 24 hours
C   {Prior to this change, I attempted to remove all gravitational water after 24 hours
C   (i.e. reduced SWC to FC after 1 day).  This was too harsh.
C   (heretofore, FC was never considered; soils got up
C   to saturation and SWC got reduced below sat ONLY via evap and transp.
C   Now, gravity will drain away gravitational water.  Equations will again be from
C   Saxton et al 1986.
C   After gravitational water moved to outflow, (bookkept separately than original OUTFLOW)
C   THEN rain and snow will be added, and evap and original outflow (outflow of water beyond
C   saturation) will be subtracted (as will the grav water outflow).
C   The timing of this calculation (at the beginning of the day) effectively drains away (at least some)
C   gravitational water AFTER allowing it to remain (i.e. hence available) for one day (the day before).

C   VCONT_FC IS VOLUMETRIC CONTENT AT FIELD CAPACITY.  AA & BB are soil-specific texture parameters
C   They are set in BINITIAL.  They and the eqns are from Saxton et al 1986.

         VCONT_FC=EXP((2.302 - LOG(AA)) / BB)
         IF (SL.EQ.1) THEN      ! top layer
                                ! First take out yesterday's transpiration
           SWC(SL)=SWC(SL)-DAYTRANS_GR -DAYTRANS_SM -DAYTRANS_LG1
     +      -DAYTRANS_SH
C   Next, evaluated status relative to FC.
            SWC_FC(SL) = VCONT_FC * SD(SL) * AREA
            GRAVOUT(SL) = 0.0
            IF (SWC(SL) .GT. SWC_FC(SL)) THEN
              GRAVOUT(SL) = (SWC(SL) - SWC_FC(SL)) / 2 !Keep track of gravitational water, add to next layer
              SWC(SL) = SWC(SL)- GRAVOUT(SL)           !Reduce SWC by amount of grav water drained
            ENDIF

           MAXOUT(SL) = SWC(SL)+RAIN +SNWMELT/2+EXCESS-MAXH2O(SL)
           OUTFLOW(SL) = MAX ( MAXOUT(SL), 0.0 )
           SWC(SL)=SWC(SL) +RAIN +SNWMELT/2 +EXCESS -EVAP - OUTFLOW(1)

         ELSE   ! 2nd layer
           SWC(SL)=SWC(SL)-DAYTRANS_LG2  ! Yesterday's transpiration
           SWC_FC(SL) = VCONT_FC * SD(SL) * AREA
           GRAVOUT(SL)=0.0
            IF (SWC(SL) .GT. SWC_FC(SL)) THEN
              GRAVOUT(SL) = (SWC(SL) - SWC_FC(SL)) / 2  !Keep track of gravitational water, to be drained away
              SWC(SL) = SWC(SL) - GRAVOUT(SL)           !Reduce SWC by amount of grav water drained
            ENDIF

C           MAXOUT(SL) = SWC(SL) +SNWMELT/2 -MAXH2O(SL)
C   Am going to modify above eqn.  Seems layer one's outflow should've been added to
C   determin MAXOUT.  I will add that, as well as the GRAVOUT.
           MAXOUT(SL) = SWC(SL) + SNWMELT/2 + OUTFLOW(1) + GRAVOUT(1)
     +       - MAXH2O(SL)
           OUTFLOW(SL) = MAX ( MAXOUT(SL), 0.0 )
           SWC(SL)=SWC(SL) + SNWMELT/2 + OUTFLOW(1) +GRAVOUT(1)
     +           -OUTFLOW(2) - GRAVOUT(2)
         ENDIF
C ***** END OF MODIFICATIONS (REDUCING SWC TO FC). *****
         
         SWC(SL) = MIN ( SWC(SL), MAXH2O(SL) )
C--- Calculate site soil water potential (-MPa)
         VCONT(SL) = SWC(SL) / (SD(SL) * AREA)
         SITEWP(SL) = 0.001 * AA * VCONT(SL) **BB

   20 CONTINUE      !*! end looping through soil layers

C--- sum water stress for use in calculating allocation fractions 
      IF (SITEWP(1).LT.SITEWP(2)) THEN                                  !*! check which layer
         BESTSL=1                                                       !*! has highest SWP
      ELSE                                                              !*!
         BESTSL=2                                                       !*! and set MAXSL
      ENDIF
                                     ! Variables below used in BALLOCA
      STRESS(1) = SITEWP(1)        !*! for grass and small trees [and now shrubs (10/00ajm)
      STRESS(2) = SITEWP(BESTSL)   !*! for large trees                  !*!
      IF(STRESS(1).GT.B2(3)) STRESS(1)=B2(3)                            !*!
      IF(STRESS(2).GT.B2(3)) STRESS(2)=B2(3)                            !*!
      SUMWP(1)=SUMWP(1)+STRESS(1)  !*! for grass and small trees [and now shrubs (10/00, ajm)
      SUMWP(2)=SUMWP(2)+STRESS(2)  !*! for large trees                  !*!

C--- write water variables out to daywatr.out.  ADDING 7 MORE 1/04 AJM
C   NOTE: "RAIN" IS THAT PORTION OF Z1(3) [ppt] MAKING IT TO SOIL (i.e. NOT 
C   INTERCEPTED BY LEAVES).
C   Z13*1000 IS PPT IN MM.  EVAPPT IS PORTION OF PPT INTERCEPTED AND 
C   EVAPORATED FROM VEG SURFACES
C   Z1(3) IS IN METERS.  REPORTED TO OUTPUT IS Z1(3)*1000 (i.e. mm)
C   EXCESS IS THAT MOISTURE ORIGINALLY INTERCEPTED BY VEG, BUT NOT SUBSEQUENTLY
C   EVAPORATED (IF ANY)--IT EVENTUALLY GOES TO SOIL.
C   TO BALANCE: OUTPUT_PPT * 10 = RAIN+SNOW+EVAPPT+EXCESS ( in CUBIC METERS/HA)
C   "*10" BECAUSE, PPT(mm) * 10,000 m2/ha / 1000 mm/m = CUBIC METERS PER HA
      IF(Z1(1).EQ.1 .AND. JD.EQ.1)  WRITE (71,890)
  890 FORMAT ('YEAR',T7,'JD',T14,'PPT',
     +   T21,'MAX1',T29,'MAX2',T37,'SWC1',T45,'SWC2',T51,'RATIO1',
     +   T59,'RATIO2',T70,'WP1',T78,'WP2',T85,'OUT1',
     +   T92,'TR_GR',T100,'TR_SM',T108,'TR_LG',T114,'SITELAI',
     +   T125,'RAIN',T133,'SNOW',T139,'EVAPPT',T147,'EXCESS'
     +   T157,'EVAP',T164,'SNWPK',T171,'SNWMLT')
      WRITE(71,892) Z1(1),JD,Z1(3)*1000.,
     +   MAXH2O(1),MAXH2O(2),SWC(1),SWC(2),SWC(1)/MAXH2O(1),
     +   SWC(2)/MAXH2O(2),SITEWP(1),SITEWP(2),OUTFLOW(1),
     +   DAYTRANS_GR,DAYTRANS_SM,DAYTRANS_LG,SITELAI,
     +   RAIN,SNOW,EVAPPT,EXCESS,EVAP,SNOWPACK,SNWMELT
  892 FORMAT (F4.0,I4,21(F8.2))
c      write(71,*) 'value of volmax used is= ', S(3)  !COMMENTED OUT 11/02 AJM
C------------------------------------------------------------------------------------

      RETURN
      END                                   
