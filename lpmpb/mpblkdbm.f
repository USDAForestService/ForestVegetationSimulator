      BLOCK DATA MPBLKD
      IMPLICIT NONE
C----------
C  **MPBLKD--BM   DATE OF LAST REVISION:  08/22/14
C----------
C
C     MOUNTAIN PINE BEETLE --
C     SEE MPBCUP OR MPBMOD FOR VARIABLE DISCRIPTIONS.
C
C Revision History
C   06/08/09 LANCE DAVID (FMSC)
C     CREATED THIS 18 SPECIES BLUE MOUNTAINS VARIANT VERSION TO 
C     ACCOMODATE THE 18 SPECIES REPRESENTED. SURROGATE SPECIES 
C     ASSIGNMENTS ARE BASED ON THOSE MADE IN THE SORNEC AND
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
C !!   SPECIES LIST FOR BM VARIANT.
C !!   # CD NAME                  CD  NAME                               
C !!  -- -- --------------------- --- -----------------------------------
C WP   1 WP WESTERN WHITE PINE    119 PINUS MONTICOLA                   
C WL   2 WL WESTERN LARCH         073 LARIX OCCIDENTALIS                 
C DF   3 DF DOUGLAS-FIR           202 PSEUDOTSUGA MENZIESII             
C DF   4 GF GRAND FIR              17 ABIES GRANDIS                      
C WL   5 MH MOUNTAIN HEMLOCK      264 TSUGA MERTENSIANA                  
C WL   6 WJ WESTERN JUNIPER       064 JUNIPERUS OCCIDENTALIS             
C LP   7 LP LODGEPOLE PINE        108 PINUS CONTORTA                     
C WP   8 ES ENGELMANN SPRUCE      093 PICEA ENGELMANNII                  
C DF   9 AF SUBALPINE FIR         019 ABIES LASIOCARPA                   
C PP  10 PP PONDEROSA PINE        122 PINUS PONDEROSA                    
C WL  11 WB WHITEBARK PINE        101 PINUS ALBICAULIS             
C LP  12 LM LIMBER PINE           113 PINUS FLEXILIS                
C WL  13 PY PACIFIC YEW           231 TAXUS BREVIFOLIA
C WL  14 YC ALASKA CEDAR           42 CHAMAECYPARIS NOOTKATENSIS
C DF  15 AS QUAKING ASPEN         746 POPULUS TREMULOIDES
C DF  16 CW BLACK COTTONWOOD      747 POPULUS BALSAMIFERA           
C WL  17 PB OTHER SOFTWOODS       298                
C DF  18 OS OTHER HARDWOODS       998                                    
C
C----------

C     Specie indexing for Blue Mountains 18 species
C
      DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/1,2,3,7,10/

C     Assign surrogate species for calculations in surfce.f
      DATA MPBSPM/
C        1    2   3   4   5   6   7   8   9  10  11 -- EM index
C        WP  WL  DF  GF  MH  WJ  LP  ES  AF  PP  WB -- EM species
C        WP  WL  DF  DF  WL  WL  LP  WP  DF  PP  WL -- surfce surrogate
     &    1,  2,  3,  3,  2,  2,  7,  1,  3, 10,  2,
C        12  13  14  15  16  17  18                 -- EM index
C        LM  PY  YC  AS  CW  OS  OH                 -- EM species
C        WL  WL  WL  DF  DF  WL  DF                 -- surfce surrogate
     &    2,  2,  2,  3,  3,  2,  3/     

      END
