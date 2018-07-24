      SUBROUTINE DMINIT
      IMPLICIT NONE
C----------
C CANADA-NEWMIST $Id$
C----------
C **DMINIT--NI  DATE OF LAST REVISION: 02/22/96
C----------
C Purpose:
C   This routine initializes all the common variables for the model.
C Explanations and definitions of the variables can be found in
C DMCOM.
C
C
C Called by:
C
C     MISIN0 
C
C Other routines called:
C
C     DMRNSD
C
C Argument list definitions:                        
C
C     [none]
C
C Local parameter and variable definitions:
C
C     INTEGER i          General loop counter
C     INTEGER j          General loop counter
C     INTEGER TPDMR      Dummy array used to load DMDMR
C     REAL    x          product of pi * sq-meter-to-acre conversion
C     REAL    y          radius of disc in circle area calculation
C     REAL    tmp        Temporary storage for calculation of ring
C                        scaling factor 'SF()' terms using DMALPH
C                        and DMBETA.
C     REAL    TPOPAQ     Dummy array used to load DMOPAQ
C     REAL    TSEED      Dummy variable used as a parameter to call DMRNSD.
C                        Never used. 
C
C
C Common block variables and parameters:
C
C     MXTHRX  DMCOM
C     MAXTRE  PRGPRM
C     DMRATE  DMCOM
C     NTDn    DMCOM
C     DCDn    DMCOM
C     ZeroPDn DMCOM
C     PIE     DMCOM
C     SQM2AC  DMCOM
C     MESH    DMCOM
C     CrArea  DMCOM          
C     Dstnce  DMCOM
C     CLUMP   DMCOM
C     DMALPH  DMCOM
C     DMBETA  DMCOM
C     SF      DMCOM
C     DMRDFF  DMCOM
C     DMDMR   DMCOM
C     DMOPQM  DMCOM
C     DMOPAQ  DMCOM [values are variant-specific]
C     DMLtnp  DMCOM
C     DMLtRx  DMCOM
C     DMKTUN  DMCOM
C     DMETUN  DMCOM
C     DMSTUN  DMCOM
C     DMITUN  DMCOM
C     DMFLWR  DMCOM
C     DMCAP   DMCOM
C     DMDETH  DMCOM
C     DMS0    DMCOM
C     DMSS    DMCOM
C
 
C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'DMCOM.F77'

C.... Local variable declarations.

      INTEGER i, j
      INTEGER TPDMR(0:6, CRTHRD)
      REAL    x, y, tmp(MXTHRX)
      REAL    TSEED 
      REAL    TPOPAQ(MAXSP)

C.... Data statements

C.... TPDMR is the dummy array used to load the array DMDMR.
C.... This is initialized this way so that if multiple stands are run through
C.... the model then the array DMDMR will get re-initialized properly for each
C.... stand.

      DATA TPDMR  / 0, 0, 0, 0, 1, 1, 2,
     &              0, 0, 1, 2, 2, 2, 2,
     &              0, 1, 1, 1, 1, 2, 2 /

C.... TPOPAQ is the dummy array used to load the array DMOPAQ.
C.... This is initialized this way so that if multiple stands are run through
C.... the model then the array DMOPAQ will get re-initialized properly for each
C.... stand.
C....
C.... The RELATIVE opacities of DMOPAQ() are taken from the April 1993
C.... Model Review Workshop Report, Table 4.1 (p.29). 
C.... These values can be changed by the keyword DMOPQ.
C.... The 15 (MAXSP) spcecies codes for the IB variant are:
C....
C.... 'PW','LW','FD','BG','HW','CW','PL','SE','BL','PY',
C.... 'EP','AT','AC','OC','OH'

                    
      DATA TPOPAQ /
     &  1.2, !PW
     &  0.9, !LW
     &  1.5, !FD
     &  1.8, !BG
     &  2.0, !HW
     &  1.8, !CW
     &  1.0, !PL
     &  1.7, !SE
     &  1.8, !BL
     &  1.0, !PY
     &  2.0, !EP
     &  2.0, !AT
     &  2.0, !AC
     &  1.5, !OC=FD
     &  2.0/ !OH=EP
                               
C.... Zero 0-6 DM rating. This probably redundant.

      DO 10 i = 1, MAXTRE
         DMRATE(i) = 0
   10 CONTINUE    
                                                                        
C.... Initialize logical triggers to .FALSE. These are responsible for  
C.... notifying whether the initial crown third assignments have been
C.... made; whether the damage codes have been assigned to the model's
C.... array of DM ratings; and whether the life history pools have been
C.... filled.

      NTDn = .FALSE.
      DCDn = .FALSE.
      ZPDn = .FALSE. 
      
C.... Compute areas in circles; a necessary step for subsequent sampling
C.... calculations. Then assign the distance to the midpoint of each
C.... sampling ring. 

      x = PIE * SQM2AC

      DO 70 i = 1, MXTHRX
         y = FLOAT(MESH * i) 
         CrArea(i) = x * y**2
   70 CONTINUE

      DO 80 i = 1, MXTHRX
         Dstnce(i) = FLOAT(MESH) * (FLOAT(i) - .5)
   80 CONTINUE

C.... Initialize the mean/variance ration to "random" (Poisson). Then
C.... initialize the autocorellation terms for the ring quadrats. These
C.... values are arbitrary, but could be measured in the field and made
C.... into sensible defaults at a later date.

      DMCLMP =  1.00
      DMALPH = -0.50
      DMBETA =  0.0

C.... Compute the default scaling weights for the autocorrelation
C.... function.

      DO 90 j = 1, MXTHRX
         tmp(j) = EXP(Dstnce(j) * DMBETA)
   90 CONTINUE
    
      DO 100 i = 0, 6
         x = FLOAT(i) * DMALPH

         DO 95 j = 1, MXTHRX
            SF(i, j) = EXP(x * tmp(j))
   95    CONTINUE
  100 CONTINUE

C.... Assign the matrix which defines the difference in DMR between any
C.... two DMR categories. Although the matrix is symmetric, 'i' indexes
C.... the *source* DMR and 'j' the *target* DMR when it is actually
C.... used.

      DO 210 i = 0, 6
         DO 200 j = 0, 6
            DMRDFF(i, j) = ABS(j-i)
  200    CONTINUE
  210 CONTINUE    

C.... Initialize default values for crown thirds DMR.

      DO 230 i = 0, 6 
         DO 220 j = 1, CRTHRD
            DMDMR(i,j) = TPDMR(i,j)
  220    CONTINUE
  230 CONTINUE

C.... Initialize default values for the relative opacities.

      DO 250 i = 1, MAXSP
         DMOPAQ(i) = TPOPAQ(i)
  250 CONTINUE

C.... The value of DMOPQM is arbitrarily chosen to be consistent with 
C.... the guess used for the Phase 1 model.  This value can be changed
C.... by the keyword DMOPQM.
                               
      DMOPQM = 0.20

      DO 300 i = 1, MAXSP

C....    Default coefficients for the light-driven forward and backward
C....    transitions.
C....    These correspond to 0% latent -> immature at 0% light
C....    and 100% latent -> immature at 100% light for DMLT().
C....    The reverse: (immature -> latent) transition coefficients are:
C....    100% immature -> C latent at 0% light and 0% immature -> latent
C....    at 100% light.
C....    Coefficients are the same for all species, and are modified
C....    by the DMLIGHT keyword.

         DMLtnp(i, 1) = 2
         DMLtRx(i, 1, 1, 1) = 0.
         DMLtRx(i, 1, 2, 1) = 0.
         DMLtRx(i, 1, 1, 2) = 1.
         DMLtRx(i, 1, 2, 2) = 1.

         DMLtnp(i, 2) = 2
         DMLtRx(i, 2, 1, 1) = 0.
         DMLtRx(i, 2, 2, 1) = 1.
         DMLtRx(i, 2, 1, 2) = 1.
         DMLtRx(i, 2, 2, 2) = 0.

C....    Spread and fecundity tuning parameters.
                                   
         DMKTUN(i) =  0
         DMETUN(i) =  1.00
         DMSTUN(i) =  1.00 
         DMITUN(i) =  1.00 
      
         DMFLWR(i) =  4 
         DMCAP(i)  =  3.00

         DMDETH(i) =  0.08

  300 CONTINUE
 
C.... Initial values for the random number generator.

      DMS0 = 55329.0D0
      DMSS = 55329.0

C.... Reset the random number generator. Value of 'i' is a dummy. 

      CALL DMRNSD (.FALSE., TSEED)

      RETURN
      END
