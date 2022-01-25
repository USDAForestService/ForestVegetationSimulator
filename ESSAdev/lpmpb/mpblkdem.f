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
C   04/24/09 LANCE DAVID (FMSC)
C     CREATED THIS 19 SPECIES EASTERN MONTANA VARIANT VERSION TO 
C     ACCOMODATE THE 19 SPECIES REPRESENTED. SURROGATE SPECIES 
C     ASSIGNMENTS ARE BASED ON THOSE MADE IN THE NORTHERN IDAHO AND
C     CENTRAL ROCKIES VARIANTS.
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
C !!   
C !!   SPECIES LIST FOR EM VARIANT.
C !!   # CD NAME                  CD  NAME                               
C !!  -- -- --------------------- --- -----------------------------------
C WL   1 WB WHITEBARK PINE        101 PINUS ALBICALA                     
C WL   2 WL WESTERN LARCH         073 LARIX OCCIDENTALIS                 
C DF   3 DF DOUGLAS-FIR           202 PSEUDOTSUGA MENZIESII             
C WL   4 LM LIMBER PINE           113 PINUS FLEXILIS                      
C WL   5 LL SUBALPINE LARCH       072 LARIX LYALLII                      
C WL   6 RM ROCKY MTN JUNIPER     064 JUNIPERUS OCCIDENTALIS             
C LP   7 LP LODGEPOLE PINE        108 PINUS CONTORTA                     
C WP   8 ES ENGELMANN SPRUCE      093 PICEA ENGELMANNII                  
C DF   9 AF SUBALPINE FIR         019 ABIES LASIOCARPA                   
C PP  10 PP PONDEROSA PINE        122 PINUS PONDEROSA                    
C DF  11 GA GREEN ASH             544 FRAXINUS PENNSYLVANICA             
C DF  12 AS QUAKING ASPEN         746 POPULUS TREMULOIDES                
C DF  13 CW BLACK COTTONWOOD      747 POPULUS BALSAMIFERA VAR TRICHOCARPA
C DF  14 BA BALSAM POPLAR         741 POPULUS BALSAMIFERA                
C DF  15 PW PLAINS COTTONWOOD     745 POPULUS DELTOIDES VAR MONOLIFERA   
C DF  16 NC NARROWLEAF COTTONWOOD 749 POPULUS ANGUSTIFOLIA               
C DF  17 PB PAPER BIRCH           375 BETULA PAPYRIFERA                  
C WL  18 OS OTHER SOFTWOODS       298                                    
C DF  19 OH OTHER HARDWOODS       998                                    
C----------

C     Specie indexing. EM variant does not have White Pine (WP), so
C                      WP is indexed to Whitebark Pine (WB).
      DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/1,2,3,7,10/

C     Assign surrogate species for calculations in surfce.f
      DATA MPBSPM/
C        1    2   3   4   5   6   7   8   9  10  11 -- EM index
C        WB  WL  DF  LM  LL  RM  LP  ES  AF  PP  GA -- EM species
C        WL  WL  DF  WL  WL  WL  LP  WP  DF  PP  DF -- surfce surrogate
     &    2,  2,  3,  2,  2,  2,  7,  1,  3, 10,  3,
C        12  13  14  15  16  17  18  19             -- EM index
C        AS  CW  BA  PW  NC  PB  OS  OH             -- EM species
C        DF  DF  DF  DF  DF  DF  WL  DF             -- surfce surrogate
     &    3,  3,  3,  3,  3,  3,  2,  3 /

      END
