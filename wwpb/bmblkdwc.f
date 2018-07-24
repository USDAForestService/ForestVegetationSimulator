      BLOCK DATA BMBLKD
C----------
C WWPB $Id$
C----------
***********************************************************************
* The following Westwide Pine Beetle Model variable initializations
* were originally within the subroutine BMINIT code.
* It was moved to this BLOCK DATA subprogram to satisfy the FORTRAN
* standards enforced by Lahey FORTRAN compiler.
* 08/16/94 Lance R. David
*
* Ammended 4/10/00.  Made host tree species and main pine beetle species
* undefined; that is, users must now explicitly define them.
*
* 5/31/05.  Snag falldown rates derived from information in the FFE addendum:
*(Reinhardt and Crookston 2005.  see note below AJM 
***********************************************************************

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.

      INCLUDE 'BMCOM.F77'
      INCLUDE 'BMRCOM.F77'

C.... Data statements

C     Logical variable to control initialization of beetle model. True
C     means that subroutine BMINIT needs to be called.
C
      DATA LBMINT/.TRUE./

c Defaults: PBSPEC= 1 : Mountain Pine Beetle is simulated
c           NBGEN=  1 : 1 generation of PBSPEC per year 
c           NIBGEN= 2 : 2 generation of Ips per year 
c           Keywords can modify

c      DATA PBSPEC/1/, NBGEN/1/, NIBGEN/2/
      DATA  NBGEN/1/, NIBGEN/2/
c Defaults: IPSON= false  :Ips is NOT a driving variable
c           IPSMIN= 2     :smallest size class Ips will attack
c           IPSMAX= 5     :biggest size class that Ips will kill tree in
c                          (but will attack larger classes)
C           PFSLSH=.9     :prop slash to fill before attacking trees
c           Keywords can modify

      DATA IPSON/.FALSE./, IPSMIN/2/, IPSMAX/5/
      DATA PFSLSH/0.9/

      DATA (HSPEC(1,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &                              0, 0, 0, 0, 0, 0/

      DATA (HSPEC(2,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &                              0, 0, 0, 0, 0, 0/

      DATA (HSPEC(3,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
     &                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &                              0, 0, 0, 0, 0, 0/

c      DATA (HSPEC(1,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0/
c      DATA (HSPEC(2,I), I=1,MAXSP) /0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0/
c      DATA (HSPEC(3,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0/

c Defaults: breakpoints for the DBH size classes. There can be no more than
c           NSCL of these. The computation which follows computes a rough
c           value for the basal area in each size class. "Rough" since it
c           has no treelist as a basis.

      DATA UPSIZ/3, 6, 9, 12, 15, 18, 21, 25, 30, 50/
  
C Defaults: DEAD WOODY POOL DBH SIZE CLASSES

      DATA WPSIZ/10, 20, 60/

C Defaults: Species falldown rates (1=fast,2=medium,3=slow) for standing dead
C
C Westside cascades tree species are:
C                 SF WF GF AF RF -- NF YC IC ES LP
C                 JP SP WP PP DF RW RC WH MH BM RA 
C                 WA PB GC AS CW WO  J LL WB KP PY 
C                 DG HT CH WI -- OT
      DATA ISPFLL /3, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2,
     &             3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2,
     &             2, 2, 2, 2, 2, 2, 3, 3, 2, 2, 3,
     &             3, 2, 2, 2, 2, 2/

C Note: above falldown rates arbitrarily assigned by Andrew McMahan 5/31/05 
C using data from table 4.17.3 FFE addendum (Reinhardt and Crookston, 2005)  
C Note: two sequence numbers are "blank", beware when comparing to table 4.17.3.
C If the falldown multipliers in FFE addendum were greater than 1, then herein
c the species is assigned a "medium" falldown rate.  Otherwise "slow".  
C The WWPB Model "fast" fall down rate is extremely fast (20% per year) relative
C to FFE's, so it wont be used.

C Defaults: Falldown rates (standing -> dead) for the different pool qualities
C           (fast, medium, slow)

      DATA FALLRT /0.2, 0.1, 0.05/

c Defaults: The smallest attrative size class varies with species.
c           MPB & WPB wont go into stands with only trees < 6 inches (sc<3), 
c           and Ips won't see stands with trees less than 3 inches only.
c           Keyword can modify

      DATA ISCMIN/3,3,2/

c Defaults: Seeds for the random number generator BMRANN.

      DATA BMS0/55329D0/, BMSS/55329./

      END
