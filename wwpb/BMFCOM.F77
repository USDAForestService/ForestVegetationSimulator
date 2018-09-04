CODE SEGMENT BMFCOM
C----------
C WWPB $Id$
C----------
************************************************************************
*  **BMFCOM:  Date of last revision:  June 10, 1994
*-----------------------------------------------------------------------
*  Purpose:
*     Common block variables related to the fire model
*-----------------------------------------------------------------------
*
*  Common block variables:
*     ATEMP:  Air temperature
*     DEPTH:
*     FWG:    Surface loading, lb.sqft fuel type
*     FLAG:   Error flag: 1=dead fuel too moist, 2=wind speed exceeds reliable
*             extrapolation, 3=gross surf/vol too small.
*     FWIND:  Dominant wind speed 
*     HABTYP: Stand habitat type  
*     ISPH:   Main tree species in host and non-host types.
*     MEXT:   Moisture of extinction of dead fuel
*     MOIS:   Moisture content of fuel types
*     MPS:    Mean surf/vol per ft of fuel type
*     ND:     Number of dead fuel size classes to be condsidered
*     NL:     Same as ND but for live fuel    
*     SLP:    Slope of the stand (< 1.0)
***********************************************************************

      INTEGER ATEMP 
      REAL    DEPTH
      REAL    FWG(2,7)
      INTEGER FLAG(3)
      REAL    FWIND        
      INTEGER HABTYP(MXSTND)
      INTEGER ISPH(MXSTND,2)       
      REAL    MEXT(3)
      REAL    MOIS(2,3) 
      INTEGER MPS(2,3)
      INTEGER ND, NL 
      REAL    SLP(MXSTND)

      COMMON/BMFCOM/ ATEMP, DEPTH, FWG, FLAG, FWIND, ISPH,
     &               HABTYP, MEXT, MOIS, MPS,
     &               ND, NL, SLP
      
C.... End segment.
