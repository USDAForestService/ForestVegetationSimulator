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
C   10/04/11 LANCE DAVID (FMSC)
C     CREATED THIS CENTRAL IDAHO VARIANT VERSION TO ACCOMODATE THE
C     19 SPECIES NOW REPRESENTED. SURROGATE SPECIES ASSIGNMENTS FOR
C     SURFACE AREA CALCULATIONS WERE MADE BASED ON PREVIOUS ASSIGNMENTS
C     IN NEARBY VARIANTS (ORIGINAL CI, TT, UT, SO, IE) VARIANTS.
C   08/22/14 Lance R. David (FMSC)
C     Function name was used as variable name.
C     changed variable INT to INCRS
C----------------------------------------------------------------------
C
COMMONS

      INCLUDE 'PRGPRM.F77'

      INCLUDE 'MPBCOM.F77'

      DATA  JOMPB  / 7 /

      DATA IPLTNO/ 1 /,IMPROB/ 1 /,NATR/ 2 /, KEYMPB/ 2,3,6*0,1 /,
     >     INCRS/ 10 /

C     SPECIES LIST FOR CENTRAL IDAHO VARIANT. ***** 19 species *****
C     
C  vv---- MPB surface area calculation surrogate specie (surfce.f)
C  !!   
C  !!  SPECIES LIST FOR UT VARIANT.
C  !!  ------FVS UT VARIANT-------  
C  !!   # CD COMMON NAME            SCIENTIFIC NAME          
C  !!  -- -- ---------------------  ---------------------    
C  WP   1 WP WESTERN WHITE PINE     PINUS MONTICOLA                      
C  WL   2 WL WESTERN LARCH          LARIX OCCIDENTALIS                   
C  DF   3 DF DOUGLAS-FIR            PSEUDOTSUGA MENZIESII                
C  DF   4 GF GRAND FIR              ABIES GRANDIS                       
C  DF   5 WH WESTERN HEMLOCK        TSUGA HETEROPHYLLA                   
C  WL   6 RC WESTERN REDCEDAR       THUJA PLICATA                        
C  LP   7 LP LODGEPOLE PINE         PINUS CONTORTA                       
C  WP   8 ES ENGLEMANN SPRUCE       PICEA ENGELMANNII                    
C  DF   9 AF SUBALPINE FIR          ABIES LASIOCARPA                     
C  PP  10 PP PONDEROSA PINE         PINUS PONDEROSA                      
C  WP  11 WB WHITEBARK PINE         PINUS ALBICAULIS                    
C  WL  12 PY PACIFIC YEW            TAXUS BREVIFOLIA                    
C  WL  13 AS QUAKING ASPEN          POPULUS TREMULOIDES                 
C  WL  14 WJ WESTERN JUNIPER        JUNIPERUS OCCIDENTALIS              
C  DF  15 MC CURLLEAF MOUNTAIN-     CERCOCARPUS LEDIFOLIUS             
C            MAHOGANY    
C  WL  16 LM LIMBER PINE            PINUS FLEXILIS                      
C  DF  17 CW BLACK COTTONWOOD       POPULUS BALSAMIFERA VAR. TRICHOCARPA
C  WL  18 OS OTHER SOFTWOODS                                            
C  DF  19 OH OTHER HARDWOODS                                            
C
C
C     LPMPB SPECIES INDICES FOR TT LIST ABOVE
 
      DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/1,2,3,7,10/
 
C     ASSIGN LPMPB SURROGATE SPECIES FOR CALCULATIONS IN SURFCE.F
C
C     CI 19 SPECIES LIST
C                  WP WL DF GF WH RC LP ES AF PP   -- CI 19 species
      DATA MPBSPM / 1, 2, 3, 3, 3, 3, 7, 1, 3,10,
C                  WB PY AS WJ MC LM CW OS OH      -- CI 19 species
     &              1, 2, 2, 2, 3, 2, 3, 2, 3/
      END
 