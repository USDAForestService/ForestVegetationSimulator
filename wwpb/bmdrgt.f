      SUBROUTINE BMDRGT (IYR)
      
***********************************************************************
*  **BMDRGT  Date of last revision:  Sept 23, 1994
*----------------------------------------------------------------------
*  
*Purpose:  Calculates RVDSC, the rating value due to 'drought' (precipitation
*     effects) of each size class in each stand in the beetle model.  Also
*     calculates RVOD, the average rating value due to drought in the Outside,
*     from the average RVDST of all the stockable stands in the landscape.
*
*----------------------------------------------------------------------
*     
*  Notes:
*     It is possible that some of the needed HIGH and WET values will be 
*     missing in the input PPE data.  The missing values will have been 
*     read as zeros.  Since 0 is a legitimate value for both these variables, 
*     this routine cannot detect missing data and must assume that all zeros 
*     are intended.
*
*     When the coefficient of a variable which influences RVDL is negative,
*     the effect of higher-than-average values of the variable is to decrease 
*     RVDL while lower-than-average values will increase RVDL.  The opposite
*     occurs when the coefficient is positive.
*
*----------------------------------------------------------------------
*     
*  Local variable definitions:
*     KDBH:   coefficient which determines the influence of DBH on RVDSC
*     KHIGH:  coefficient which determines the influence of HIGH on RVDSC
*     KSDI:   coefficient which determines the influence of SDI on RVDSC
*     KWET:   coefficient which determines the influence of WET on RVDSC
*     PRESET: indicates whether the user has pre-set RVDL to some constant
*             value (PRESET = 1), or the subroutine should draw a random 
*             value for RVDL (PRESET = 0).
*     RND1, RND2: uniform random numbers between 0 and 1.
*     RVTEMP: Rating Value due to Drought effects in the current year alone,
*             for the current size class.
*     RVDL:   the average Rating Value due to Drought at the Landscape level
*     SDD:    standard deviation of RVDL (or the change in RVDL from 1 due to
*             drought effects, if PRESET = 1).
*     STDEFF: sum of the Stand Effects on RVDSC, for the current stand
*     TOTEFF: Total of the stand and size-class Effects on RVDSC, for the
*             current stand and size class          
*
*  Common block variables and parameters:
*     AVDBH:  basal-area-weighted average of SCDBH, for each stand 
*     AVHIGH: stand-area-weighted average of HIGH
*     AVSDI:  stand-area-weighted average of BMSDI
*     AVWET:  stand-area-weighted average of WET
*     BMSDI:  Beetle Model's Stand Density Index for each stand, 
*             calculated from TREE (host + nonhost), BAH and BANH so that it 
*             is updated for beetle activity during the growth cycle.
*     HIGH:   height (i.e. elevation) of each stand
*     RVOD:   host Rating Value Outside due to Drought effects
*     SCDBH:  DBH of a tree with the average basal area of the Size Class
*     SDDBH:  standard deviation of SCDBH (basal-area-weighted)
*     SDHIGH: standard deviation of HIGH (stand-area-weighted)
*     SDSDI:  standard deviation of BMSDI (stand-area-weighted)
*     SDWET:  standard deviation of WET (stand-area-weighted)
*     STOCK:  logical variable to indicate whether each stand is STOCKable   
*     TRURVD: the 'true' rating value due to drought of each size class
*             in each stand, as calculated by DMDRGT.  Unlike RVDSC, this
*             value can be > 1 and < 0.01.  It is this value that is used
*             to provide memory of drought effects between years (i.e. this
*             year's TRURVD is the average of the rating value due to 
*             drought effects in the current year and last year's TRURVD).
*     WASDRY: logical variable that indicates whether the drought model
*             was run last year (from 'WAS DRY') 
*     WET:    any measure of site moisture availability or drought resistance
*             that the user cares to enter
*
***********************************************************************

C.... Parameter include files.
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.
      INCLUDE 'PPCNTL.F77'
      INCLUDE 'BMCOM.F77'

C.... Variable declarations.
      REAL KDBH, KHIGH, KSDI, KWET, RVTEMP, RVDL
      REAL RND1, RND2, STDEFF, RVDST
      INTEGER ISTD, SINDX, ISIZ, PRESET, IRC
      INTEGER DUM(1)
      LOGICAL LOK 
      REAL    PRMS(7)

      SAVE

C.... Begin Routine

C First determine if the drought model is supposed to run this year.
      IYR1 = IYR
      NPRMS = 6
      CALL GPGET2 (309,IYR1,7,NPRMS,PRMS,1,I,DUM,LOK) 
        
      IF (LOK) THEN

        PRESET=  IFIX(PRMS(1))
        IF (PRMS(1) .NE. -1.0) THEN
           DRYON = .TRUE.
           SDD = PRMS(2)
           KDBH = PRMS(3)
           KHIGH = PRMS(4)
           KSDI = PRMS(5)
           KWET = PRMS(6)
        ELSE
           DRYON = .FALSE.
           SDD = 0.0
           KDBH = 0.0
           KHIGH = 0.0
           KSDI = 0.0
           KWET = 0.0  
        ENDIF   
      
        IF (LBMDEB) WRITE (JBMBPR,91) MICYC,IYR1,IFIX(PRMS(1)),PRMS(2)
   91   FORMAT (/' IN BMDRGT: MICYC=', I5, 'IYR1= ',I4,
     >         'Off/Fixed/Variable (-1/0/1) = ',I1,'SD= ', F7.4 )
      ENDIF

C If drought model not running this year then set WASDRY to FALSE to indicate 
c that the model was not run, set RVDSC and RVOD to 1.0, and return.  

      IF (.NOT. DRYON) THEN
        WASDRY = .FALSE. 
        RVOD = 1.0
        DO 10 I= 1,MXSTND
          DO 11 J= 1, NSCL
            RVDSC(I,J)= 1.0
   11     CONTINUE
   10   CONTINUE
        RETURN
      ENDIF
   
            
C Get BMAVG to calculate the necessary values, averages and sd's.  
c Initialize RVOD and TAREA.

      CALL BMAVG (KDBH, KHIGH, KSDI, KWET)
      RVOD = 0.0
      TAREA = 0.0
                                        
C Find RVDL.  If PRESET=0, this is a random number from a normal distribution
C with a mean of 1 and a sd of SDD.  Otherwise, RVDL is just 1 + SDD. 

      IF (PRESET .EQ. 0) THEN
        CALL BMRANN(RND1)
        CALL BMRANN(RND2)
        
C       this conversion of two uniform numbers to a random normal number is
C       from pg. 92 of JA Rice 1988 Mathematical Statistics and Data Analysis, 
C       published by Wadsworth and Brooks, Pacific Grove, California.

        RVDL = 1.0 
     &       + SDD * SQRT(-2.0*LOG(RND1)) * COS(2.0*3.141592654*RND2)
      ELSE
        RVDL = 1.0 + SDD
      END IF
      

C Loop through all the stands finding RVDSC for each size class in each stand.
C Skip the non-stockable stands.

      DO 900 ISTD = 1,BMSTND              
        IF (.NOT. STOCK(ISTD)) GOTO 900
        SINDX = BMSDIX(ISTD)
        RVDST = 0.0
        
C       find STDEFF

        STDEFF = 0.0
        IF ((KHIGH .NE. 0.0) .AND. (SDHIGH .NE. 0.0)) 
     &    STDEFF = STDEFF + KHIGH * (HIGH(SINDX)-AVHIGH) / SDHIGH
        IF ((KSDI .NE. 0.0) .AND. (SDSDI .NE. 0.0)) 
     &    STDEFF = STDEFF + KSDI * (BMSDI(SINDX)-AVSDI) / SDSDI
        IF ((KWET .NE. 0.0) .AND. (SDWET .NE. 0.0)) 
C     &    STDEFF = STDEFF + KWET * (WET(SINDX)-AVWET) / SDWET
C
C     Changed WET( to WTSC(  (RNH July98)
C
     &    STDEFF = STDEFF + KWET * (WTSC(SINDX)-AVWET) / SDWET
          
C       loop through all the size classes

        DO 100 ISIZ = 1,NSCL

C         find TOTEFF

          IF ((KDBH .NE. 0.0) .AND. (SDDBH(SINDX) .NE. 0.0)) THEN 
            TOTEFF = STDEFF + KDBH * 
     >        (SCDBH(SINDX,ISIZ)-AVDBH(SINDX)) / SDDBH(SINDX)
          ELSE
            TOTEFF = STDEFF
          END IF
          
C         find RVTEMP and average it to the existing value of TRURVD (i.e. the
C         value calculated last year) if the drought model ran last year.
C         otherwise, average it to 1.

          RVTEMP = RVDL + ABS(1-RVDL) * TOTEFF
          
          IF (WASDRY) THEN
            TRURVD(SINDX,ISIZ) = (TRURVD(SINDX,ISIZ) + RVTEMP) / 2.0
          ELSE          
            TRURVD(SINDX,ISIZ) = (1.0 + RVTEMP) / 2.0
          END IF          
C
C     Change the upper bound to ? instead of 1.  (RNH June98)
C          
C         find RVDSC making sure that it is between 0.01 and 1

          RVDSC(SINDX,ISIZ) = TRURVD(SINDX,ISIZ)
          IF (RVDSC(SINDX,ISIZ) .LT. 0.01) RVDSC(SINDX,ISIZ) = 0.01
          IF (RVDSC(SINDX,ISIZ) .GT. (1. + ABS(SDD)))
     1        RVDSC(SINDX,ISIZ) = (1. + ABS(SDD))
          IF (RVDSC(SINDX,ISIZ) .GT. 1.0) RVDSC(SINDX,ISIZ) = 1.0
C          
C         sum up RVDST, the BA-weighted stand average RVDSC
          RVDST = RVDST + RVDSC(SINDX,ISIZ) * BAH(SINDX,ISIZ)
  100   CONTINUE
  
C       sum up RVOD, the stand-area-weighted average of RVDST.  Only include
C       stands with host in them in the average.
        IF (BAH(SINDX,NSCL+1) .GT. 0.0) THEN
          RVDST = RVDST / BAH(SINDX,NSCL+1)
          CALL SPLAAR(SINDX, SAREA, IRC)
          TAREA = TAREA + SAREA
          RVOD = RVOD + RVDST * SAREA            
        END IF    
  900 CONTINUE
  
c     find RVOD
      IF (TAREA .GT. 0.0) THEN
        RVOD = RVOD / TAREA
      ELSE
        RVOD = 1.0
      END IF  
                   
C Set WASDRY to TRUE to indicate that the drought model has been run. 
          
      WASDRY = .TRUE.
      
      RETURN
      END
