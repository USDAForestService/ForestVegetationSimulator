      SUBROUTINE BCROWN(I,NEW_CR)
C------------------------------
C Subroutine to adjust crown ratios for entities. Called 
C from TRNOVR.FOR -- DWC,5/13/94.
C   Note: updates to comments made on 8/24/94,DWC.
C Re-written 7/13/95 -- KSM. 
C-------------------------------
      
      REAL HINC, CL, LOSS, NEW_CR, NEW_CL
      INCLUDE 'ENTITY.F77'

C calculate current crown length and height increment      
      CL=O_H(I) * CR(I)
      HINC=H(I) - O_H(I)
C check for undefined variables or conditions
      IF(HINC.LE.0.0) HINC=0.0
C if previous years' leaf carbon > current years' leaf carbon
C reduce CL by an amount such that the reduction in crown volume 
C just contains the leaf carbon loss. Carbon is removed from the
C base of the crown. If leaf carbon has increased, then
C crown length is increased by HINC.
      IF(O_LEAF(I).GT.LEAF(I)) THEN
        LOSS=O_LEAF(I) - LEAF(I)
      ELSE
        LOSS=0.0
      ENDIF
C calculate new CL based on start of period values for foliage density
C (FOLDENS (kg/m3) - in STRUCT), crown angle (BETA(I) in STRUCT), CL, and 
C equation for calculating volume of a cone frustrum (in STRUCT)
      FRUSTVOL=LOSS*1/FOLDENS(I)   !gives m3 of crown volume in leafcarbon
      PART=(3.145*BETA(I)**2.)/3.  ! coeff. on Beta(i) changed from 3 to 2
      C1=CL**3.                    ! 3/16/96. KSM.
      C2=FRUSTVOL/PART
      DIF=C1-C2
      IF(DIF.LE.0.0) THEN
         NEW_CL=0. ! can't have negative CL
      ELSE
         NEW_CL=(DIF**(1./3.))+HINC
      ENDIF
C control crown length by holding FOLDENS(I) constant      
C     CRWNVOL=LEAF(I)/FOLDENS(I)
C     NEW_CL=(CRWNVOL/PART)**(1./3.)+HINC
C     
      NEW_CR=NEW_CL/H(I)
C     WRITE(*,100) I, FOLDENS(I), CL, NEW_CL, CR(I), NEW_CR
C 100 FORMAT(I4,1X,F6.4,1X,2(F8.2,1X),2(F8.3,1X))   
      RETURN
      END
