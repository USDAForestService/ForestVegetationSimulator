      SUBROUTINE BMAVG (KDBH, KHIGH, KSDI, KWET)
      
***********************************************************************
*  **BMAVG  Date of last revision:  20 May 1994
*----------------------------------------------------------------------
*  
*Purpose: Calculates some stand values, stand averages and size-class 
*         averages (and their standard deviations) required by BMDRGT.
*
*----------------------------------------------------------------------
*
*  Local variable definitions:
*     SAREA:  area of the current stand
*     TAREA:  total area of all the stands used in the beetle model
*     BA:     total basal area in a size class or stand
*     TOTDEN: the total density of trees in a stand
*     ASD:    Average Stem Diameter, the diameter of a tree whose basal area 
*             equals the stand average
*
*
*  Common block variables and parameters:
*     
*     AVDBH:  basal-area-weighted average of SCDBH, for each stand 
*     AVHIGH: stand-area-weighted average of HIGH
*     AVSDI:  stand-area-weighted average value of BMSDI
*     AVWET:  stand-area-weighted average of WET
*     BAH:    total basal area per acre of host trees in each size class
*     BANH:   total basal area per acre of non-host trees in each size class
*     BMSDI:  Beetle Model's Stand Density Index for each stand, 
*             calculated from TREE (host + nonhost), BAH and BANH so that it 
*             is updated for beetle activity during the growth cycle.
*     HIGH:   height (i.e. elevation) of each stand
*     SCDBH:  DBH of a tree with the average basal area of the Size Class
*     SDDBH:  standard deviation of SCDBH (basal-area-weighted)
*     SDHIGH: standard deviation of HIGH (stand-area-weighted)
*     SDSDI:  standard deviation of BMSDI (stand-area-weighted)
*     SDWET:  standard deviation of WET (stand-area-weighted)          
*     STOCK:  logical variable to indicate whether each stand is STOCKable   
*     TREE:   number per acre of trees in each size class, dimensioned by
*             stand, size class and 1=host/2=non-host
*     WET:    any measure of site moisture availability or drought resistance
*             that the user cares to enter
*
***********************************************************************

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.

      INCLUDE 'BMCOM.F77'
                          
C.... Call Statement Variable Declarations
      REAL KDBH, KHIGH, KSDI, KWET
                                
C.... Variable declarations.
      INTEGER IRC, ISTD, SINDX, ISIZ
      REAL SAREA, TAREA, TBA(MXSTND), THIGH, TSDI, TWET
      REAL BA, TOTDEN, ASD, SLOPE

C.... Begin Routine

************************ASSUMING THAT HIGH, WET AND EVERYTHING ELSE ARE
************************INDEXED BY MXSTNDS, NOT BMSTNDS.
************************except STOCK, which is by BMSTNDS in BMATCT.FOR
                
C     Loop through all the stands in the beetle model, finding the
C     required weighted sums.  Sums are required for each variable which 
C     has a non-zero coefficient in the drought model (e.g. the weighted
C     sum of HIGH values across stands is needed if KHIGH <> 0).  Do not
C     include non-stockable stands in the averages.
                
      TAREA = 0.0
      THIGH = 0.0
      TSDI = 0.0
      TWET = 0.0
      
      DO 500 ISTD= 1,BMSTND
        IF (.NOT. STOCK(ISTD)) GOTO 500
        
        SINDX = BMSDIX(ISTD)
        CALL SPLAAR(SINDX,SAREA,IRC)
        IF (SAREA .EQ. 0.0) GOTO 500
        
        TAREA = TAREA + SAREA
        TBA(SINDX) = 0.0
                              
c       find the sum of HIGH                              
        IF (KHIGH .NE. 0.0) THIGH = THIGH + SAREA*HIGH(SINDX)
c       find the sum of WET                              
C
C     Renamed WET to WTSC to use data read in WETSCORE (RNH 1july98)
C
C        IF (KWET .NE. 0.0) TWET = TWET + SAREA*WET(SINDX) 
        IF (KWET .NE. 0.0) TWET = TWET + SAREA*WTSC(SINDX) 
C        
C        
c       calculate SCDBH and find AVDBH                              
        IF (KDBH .EQ. 0.0) GOTO 200
          AVDBH(SINDX) = 0.0
          DO 100 ISIZ = 1,NSCL
            BA = BAH(SINDX,ISIZ) + BANH(SINDX,ISIZ)
            
            IF (BA .LE. 0.0) THEN
              SCDBH(SINDX,ISIZ) = 0.0
              GOTO 100
            END IF                                 
            
            XD= TREE(SINDX,ISIZ,1)+TREE(SINDX,ISIZ,2)
            IF (XD .LE. 0.0) THEN
              SCDBH(SINDX,ISIZ) = 0.0
              GOTO 100
            END IF                                 
            
            
            TBA(SINDX) = TBA(SINDX) + BA
            
C           find the average DBH of trees in the size class from their
C           average basal area (basal area = pi * (dbh/2)^2), and convert
C           the result to inches.
            SCDBH(SINDX,ISIZ) = 12.0 * SQRT(1.273239545 * BA / XD)
            AVDBH(SINDX) = AVDBH(SINDX) + BA * SCDBH(SINDX,ISIZ)
            
  100     CONTINUE
  
          IF (TBA(SINDX) .GT. 0.0) AVDBH(SINDX) =AVDBH(SINDX)/TBA(SINDX)
  200   CONTINUE
  
  
C       calculate BMSDI and find the sum of SDI                              
        IF (KSDI .EQ. 0.0) GOTO 400
          
C         find the total density of trees in the stand, and total basal area
c         (which will already be available if AVDBH has been calculated)
          TOTDEN = 0.0
          BA = 0.0
          DO 300 ISIZ = 1,NSCL
            TOTDEN = TOTDEN + TREE(SINDX,ISIZ,1) + TREE(SINDX,ISIZ,2)
            IF (TBA(SINDX) .EQ. 0.0) 
     &          BA = BA + BAH(SINDX,ISIZ) + BANH(SINDX,ISIZ)
  300     CONTINUE
          IF (TBA(SINDX) .EQ. 0.0) TBA(SINDX) = BA
                         
          IF (TBA(SINDX) .LE. 0.0) THEN
            BMSDI(SINDX) = 0.0
            GOTO 400
          END IF
                         
C         find ASD from average basal area = pi * (ASD/2)^2.  This gives ASD
C         in square feet - convert it to inches.
          ASD = 12.0 * SQRT(1.273239545 * TBA(SINDX) / TOTDEN)
          
C         find SDI.  This equation is the one used by ANCSD.FOR in the
C         Annosus Bark Beetle model, and has been checked against the SDI
C         graph in Forest Mensuration (Husch et al.).  TOTDEN in this equation
C         is in trees/acre and ASD is in inches.
          SLOPE = -1.605
          BMSDI(SINDX) =10.0**(LOG10(TOTDEN) -(SLOPE*LOG10(ASD)) +SLOPE)
          
          TSDI = TSDI + SAREA * BMSDI(SINDX)
  400   CONTINUE
  
  500 CONTINUE
  
     
C     Find the required averages (AVDBH is already done).
      IF (TAREA .EQ. 0.0) GOTO 999          
      IF (KHIGH .NE. 0.0) AVHIGH = THIGH / TAREA
      IF (KWET .NE. 0.0) AVWET = TWET / TAREA
      IF (KSDI .NE. 0.0) AVSDI = TSDI / TAREA
      
      
C     Loop through all the stands in the beetle model, finding the
C     required weighted sums of squared deviations.  Again, skip any
C     non-stockable stands.
      THIGH = 0.0
      TSDI = 0.0
      TWET = 0.0
              
      DO 900 ISTD= 1,BMSTND
        IF (.NOT. STOCK(ISTD)) GOTO 900
        
        SINDX = BMSDIX(ISTD)
        CALL SPLAAR(SINDX,SAREA,IRC)
        IF (SAREA .EQ. 0.0) GOTO 900
                              
C       find the sum of the HIGH deviations
        IF (KHIGH .NE. 0.0) THIGH = THIGH +SAREA*(AVHIGH-HIGH(SINDX))**2
C       find the sum of the WET deviations
C
C     Renamed WET to WTSC to use data read in WETSCORE (RNH 1july98)
C
        IF (KWET .NE. 0.0) TWET = TWET + SAREA*(AVWET-WTSC(SINDX))**2 
C        IF (KWET .NE. 0.0) TWET = TWET + SAREA*(AVWET-WET(SINDX))**2 
C       find the sum of the SDI deviations                              
        IF (KSDI .NE. 0.0) TSDI = TSDI + SAREA*(AVSDI-BMSDI(SINDX))**2 
        
        
C       find SDDBH                              
        IF (KDBH .EQ. 0.0) GOTO 700
          SDDBH(SINDX) = 0.0
          
          DO 600 ISIZ = 1,NSCL
            BA = BAH(SINDX,ISIZ) + BANH(SINDX,ISIZ)
            SDDBH(SINDX) = SDDBH(SINDX) 
     &                    + BA*(AVDBH(SINDX)-SCDBH(SINDX,ISIZ))**2
  600     CONTINUE
  
          IF (TBA(SINDX) .GT. 0.0) 
     &      SDDBH(SINDX) = SQRT(SDDBH(SINDX) / TBA(SINDX))
  700   CONTINUE
  900 CONTINUE
  
     
C     Find the required standard deviations (SDDBH is already done).
      IF (KHIGH .NE. 0.0) SDHIGH = SQRT(THIGH / TAREA)
      IF (KWET .NE. 0.0) SDWET = SQRT(TWET / TAREA)
      IF (KSDI .NE. 0.0) SDSDI = SQRT(TSDI / TAREA)
      
  999 CONTINUE
      RETURN
      END
