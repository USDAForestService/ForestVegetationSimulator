      BLOCK DATA MPBLKD
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     MOUNTAIN PINE BEETLE --
C     SEE MPBCUP OR MPBMOD FOR VARIABLE DISCRIPTIONS.
C
C Revision History
C   10/14/04 LANCE DAVID (FHTET)
C     CREATED THIS SORNEC-33 VARIANT VERSION TO ACCOMODATE 
C     THE 33 SPECIES REPRESENTED. SURROGATE SPECIES ASSIGNMENTS 
C     ARE BASED ON THOSE MADE IN THE NORTHERN IDAHO AND CENTRAL
C     ROCKIES VARIANTS.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C   08/22/14 Lance R. David (FMSC)
C     Function name was used as variable name.
C     changed variable INT to INCRS
C----------------------------------------------------------------------
C
COMMONS

      INCLUDE 'PRGPRM.F77'

      INCLUDE 'MPBCOM.F77'
C
COMMONS
C
      DATA  JOMPB  / 7 /

      DATA IPLTNO/ 1 /,IMPROB/ 1 /,NATR/ 2 /, KEYMPB/ 2,3,6*0,1 /,
     >     INCRS/ 10 /

C----------
C vv---- MPB surface area calculation surrogate specie (surfce.f)
C !!   SPECIES LIST FOR SORNEC-33 VARIANT.
C !!
C WP   1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
C WP   2 = SUGAR PINE (SP)                  PINUS LAMBERTIANA
C DF   3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
C DF   4 = WHITE FIR (WF)                   ABIES CONCOLOR (SO - WF/GF)
C WL   5 = MOUNTAIN HEMLOCK (MH)            TSUGA MERTENSIANA
C WL   6 = INCENSE CEDAR (IC)               LIBOCEDRUS DECURRENS
C LP   7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
C WP   8 = ENGLEMAN SPRUCE (ES)             PICEA ENGELMANNII
C DF   9 = SHASTA RED FIR (SH)              ABIES MAGNIFICA (SHASTENSIS)(FROM CA)
C PP  10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
C WL  11 = WESTERN JUNIPER (JU)             JUNIPERUS OCCIDENTALIS
C DF  12 = GRAND FIR (GF)                   ABIES GRANDIS (SO - WF/GF)
C DF  13 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
C DF  14 = PACIFIC SILVER FIR (SF)          ABIES AMABILIS (FROM EC)
C DF  15 = NOBLE FIR (NF)                   ABIES PROCERA (FROM WC)
C WL  16 = WHITEBARK PINE (WB)              PINUS ALBICAULIS (FROM TT)
C WL  17 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS (FROM EC)
C WL  18 = WESTERN REDCEDAR (RC)            THUJA PLICATA (FROM EC)
C DF  19 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA (FROM WC)
C WL  20 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA (FROM WC)
C WL  21 = WHITE ALDER (WA)                 ALNUS RHOMBIFOLIA (FROM WC)
C WL  22 = RED ALDER (RA)                   ALNUS RUBRA (FROM WC)
C DF  23 = BIGLEAF MAPLE (BM)               ACER MACROPHYLLUM (FROM WC)
C DF  24 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES (FROM UT)
C DF  25 = BLACK COTTONWOOD (CW)            POPULUS TRICHOCARPA (FROM WC)
C DF  26 = BITTER CHERRY (CH)               PRUNUS EMARGINATA (FROM WC)
C DF  27 = OREGON WHITE OAK (WO)            QUERCUS GARRYANA (FROM CA)
C DF  28 = WILLOW (WI)                      SALIX sp. (FROM WC)
C DF  29 = GIANT CHINKAPIN (GC) /           CASTANOPSIS CHRYSOPHYLLA (FROM WC)
C DF  30 = CURL-LEAF MOUNTAIN MAHOGANY (MC) CERCOCARPUS LEDIFOLIUS (FROM WC)
C DF  31 = BIRCHLEAF MOUNTAIN MAHOGANY (MB) CERCOCARPUS ALNIFOLIUS (FROM WC)
C WL  32 = OTHER SOFTWOODS (OS)             DOUGLAS-FIR (DF) (FROM SO)
C DF  33 = OTHER HARWOODS (OH)              MISCELLANEOUS HARDWOOD (FROM WC)
C----------

      DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/1,17,3,7,10/

C     Assign surrogate species for the calculations in surfce.f
      DATA MPBSPM/
C        1    2   3   4   5   6   7   8   9  10  11
C        WP, SP, DF, WF, MH, IC, LP, ES, SH, PP, JU,
     &    1,  1,  3,  3, 17, 17,  7,  1,  3, 10, 17,
C        12  13  14  15  16  17  18  19  20  21  22
C        GF, AF, SF, NF, WB, WL, RC, WH, PY, WA, RA,
     &    3,  3,  3,  3, 17, 17, 17,  3, 17, 17, 17,
C        23  24  25  26  27  28  29  30  31  32  33
C        BM, AS, CW, CH, WO, WI, GC, MC, MB, OS, OH
     &    3,  3,  3,  3,  3,  3,  3,  3,  3, 17,  3 /

      END
