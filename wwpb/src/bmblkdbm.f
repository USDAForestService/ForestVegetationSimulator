      BLOCK DATA BMBLKD
C----------
C  **BMBLKD BM--WESTWIDE PINE BEETLE MODEL  DATE OF LAST REVISION: 07/23/10
C            Blue Mountains FVS variant
C----------
***********************************************************************
* The following Westwide Pine Beetle Model variable initializations
* were originally within the subroutine BMINIT code.
* It was moved to this BLOCK DATA subprogram to satisfy the FORTRAN
* standards enforced by Lahey FORTRAN compiler.
* 08/16/94 Lance R. David
*
* Revision History:
* 04/27/2004 Lance David (FHTET)
*    Subroutine created from NI version.
* 07/23/2010 Lance David (FMSC)
*    Updated for the Blue Mountains 18 species expansion. Falldown 
*    rates for the species 1 to 10 came from NI and rates for species
*    11 to 18 came from existing assignments in variants SO and WC.
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

C Defaults: PBSPEC= 1 : Mountain Pine Beetle is simulated
C           NBGEN=  1 : 1 generation of PBSPEC per year 
C           NIBGEN= 2 : 2 generation of Ips per year 
C           Keywords can modify

      DATA  NBGEN/1/, NIBGEN/2/

C Defaults: IPSON= false  :Ips is NOT a driving variable
C           IPSMIN= 2     :smallest size class Ips will attack
C           IPSMAX= 5     :biggest size class that Ips will kill tree in
C                          (but will attack larger classes)
C           PFSLSH=.9     :prop slash to fill before attacking trees
C           Keywords can modify

      DATA IPSON/.FALSE./, IPSMIN/2/, IPSMAX/5/
      DATA PFSLSH/0.9/

C Defaults: Host species for MPB/WPB/I.
C           No defaults host assignments are made. Must eb specified
C           on bmparm keyword.
C  Blue Mountians species are:      WP WL DF GF MH WJ LP ES AF PP
C                                   WB LM PY YC AS CW OS OH
      DATA (HSPEC(1,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 0, 0,  0, 0,
     &                              0, 0, 0, 0, 0, 0, 0, 0 / 
      DATA (HSPEC(2,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &                              0, 0, 0, 0, 0, 0, 0, 0 / 
      DATA (HSPEC(3,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &                              0, 0, 0, 0, 0, 0, 0, 0 / 

C Defaults: breakpoints for the DBH size classes. There can be no more than
C           NSCL of these. The computation which follows computes a rough
C           value for the basal area in each size class. "Rough" since it
C           has no treelist as a basis.

      DATA UPSIZ/3, 6, 9, 12, 15, 18, 21, 25, 30, 50/
  
C Defaults: DEAD WOODY POOL DBH SIZE CLASSES

      DATA WPSIZ/10, 20, 60/

C Defaults: Species falldown rates (1=fast,2=medium,3=slow) for standing dead
C  Blue Mountians species are:
C                 WP WL DF GF MH WJ LP ES AF PP
C                 WB LM PY YC AS CW OS OH
      DATA ISPFLL /2, 3, 2, 1, 2, 3, 2, 1, 2, 1,
     &             2, 2, 3, 3, 2, 2, 3, 2 /

C Defaults: Falldown rates (standing -> dead) for the different pool qualities
C           (fast, medium, slow)

      DATA FALLRT /0.2, 0.1, 0.05/

C Defaults: The smallest attractive size class varies with species.
C           MPB & WPB wont go into stands with only trees < 6 inches (sc<3), 
C           and Ips won't see stands with trees less than 3 inches only.
C           Keyword can modify

      DATA ISCMIN/3,3,2/

C Defaults: Seeds for the random number generator BMRANN.

      DATA BMS0/55329D0/, BMSS/55329./

      END
