      BLOCK DATA MPBLKD
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     MOUNTAIN PINE BEETLE --
C     SEE MPBCUP OR MPBMOD FOR VARIABLE DISCRIPTIONS.
C
C REVISION HISTORY:
C     May-June, 2000 Glenn E. Brink
C       Added variables IDXWP,IDXWL,IDXDF,IDXLP and IDXPP, array indices of
C       White Pine, Western Larch, Douglas Fir, Lodgepole Pine and
C       Ponderosa Pine respectively.  Added to common block in file
C       MPBCOM.F77.
C       Added array MPBSPM to govern the computations in surfce.f by
C       species using an IF block as opposed to the old COMPUTED GO TO,
C       since the array allows the definition to be made in mpblkd.f,
C       instead of always having to change the COMPUTED GO TO.
C   	Added to common block in file MPBCOM.F77.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C   08/22/14 Lance R. David (FMSC)
C     Function name was used as variable name.
C     changed variable INT to INCRS
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'MPBCOM.F77'

COMMONS
C
      DATA  JOMPB  / 7 /

      DATA IPLTNO/ 1 /,IMPROB/ 1 /,NATR/ 2 /, KEYMPB/ 2,3,6*0,1 /,
     >     INCRS/ 10 /

C     SPECIES LIST FOR INLAND EMPIRE VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)        PINUS MONTICOLA
C     2 = WESTERN LARCH (L)              LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)               PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)                 ABIES GRANDIS
C     5 = WESTERN HEMLOCK (WH)           TSUGA HETEROPHYLLA
C     6 = WESTERN REDCEDAR (C)           THUJA PLICATA
C     7 = LODGEPOLE PINE (LP)            PINUS CONTORTA
C     8 = ENGLEMAN SPRUCE (S)            PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)             ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)            PINUS PONDEROSA
C    11 = OTHER (OT)  grown as MOUNTAIN HEMLOCK   TSUGA MERTENSIANA


      DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/1,2,3,7,10/

C     Use appropriate surrogate species for the calculations in surfce.f
C                   WP, L,DF,GF,WH, C,LP, S,AF,PP,OT
      DATA MPBSPM /  1, 2, 3, 3, 3, 3, 7, 1, 3,10, 3/

      END
