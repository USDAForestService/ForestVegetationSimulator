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
C   07/06/00 LANCE DAVID (FHTET)
C     CREATED THIS UTAH VARIANT VERSION TO ACCOMODATE THE 14 SPECIES
C     NOW REPRESENTED. SURROGATE SPECIES ASSIGNMENTS ARE BASED ON
C     THOSE MADE IN THE NORTHERN IDAHO AND CENTRAL ROCKIES VARIANTS.
C   08/21/09 LANCE DAVID (FMSC)
C     Utah variant expanded to 24 species.
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

      DATA  JOMPB  / 7 /

      DATA IPLTNO/ 1 /,IMPROB/ 1 /,NATR/ 2 /, KEYMPB/ 2,3,6*0,1 /,
     >     INCRS/ 10 /

C     SPECIES LIST FOR UTAH VARIANT. ***** 24 species *****
C     
C  vv---- MPB surface area calculation surrogate specie (surfce.f)
C  !!   
C  !!  SPECIES LIST FOR UT VARIANT.
C  !!  ------FVS UT VARIANT-------  
C  !!   # CD COMMON NAME            SCIENTIFIC NAME
C  !!  -- -- ---------------------  --------------------- 
C  WP   1 WB WHITEBARK PINE         PINUS ALBICAULIS          
C  WL   2 LM LIMBER PINE            PINUS FLEXILIS            
C  DF   3 DF DOUGLAS-FIR            PSEUDOTSUGA MENZIESII     
C  DF   4 WF WHITE FIR              ABIES CONCOLOR            
C  WP   5 BS BLUE SPRUCE            PICEA PUNGENS             
C  WL   6 AS QUAKING ASPEN          POPULUS TREMULOIDES       
C  LP   7 LP LODGEPOLE PINE         PINUS CONTORTA            
C  WP   8 ES ENGLEMANN SPRUCE       PICEA ENGELMANNII         
C  DF   9 AF SUBALPINE FIR          ABIES LASIOCARPA          
C  PP  10 PP PONDEROSA PINE         PINUS PONDEROSA           
C  WL  11 PI COMMON PINYON          PINUS EDULIS              
C  WL  12 WJ WESTERN JUNIPER        JUNIPERUS OCCIDENTALIS    
C  DF  13 GO GAMBEL OAK             QUERCUS sp.            
C  WL  14 PM SINLELEAF PINYON       PINUS MONOPHYLLA      
C  WL  15 RM ROCKY MOUNTAIN JUNIPER JUNIPERUS SCOPULORUM
C  WL  16 UJ UTAH JUNIPER           JUNIPERUS OSTEOSPERMA
C  WL  17 GB BRISTLECONE PINE       PINUS LOGAEVA
C            (Great Basin)       
C  DF  18 NC NARROWLEAF COTTONWOOD  POPULUS ANGUSTIFOLIA
C  DF  19 FC FREMONT COTTONWOOD     POPULUS FREMONTII
C  DF  20 MC CURLLEAF MOUNTAIN-     CERCOCARPUS LEDIFOLIUS
C            MAHOGANY
C  DF  21 BI BIGTOOTH MAPLE         ACER GRANDIDENTATUM
C  DF  22 BE BOX ELDER              ACER NEGUNDO
C  WL  23 OS OTHER SOFTWOODS        
C  DF  24 OH OTHER HARDWOODS        
C
C     LPMPB SPECIES INDICES FOR UT LIST ABOVE

      DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/1,2,3,7,10/

C     ASSIGN LPMPB SURROGATE SPECIES FOR CALCULATIONS IN SURFCE.F
C
C     UT 24 SPECIES LIST
C                  WB LM DF WF BS AS LP ES AF PP PI WJ 
      DATA MPBSPM / 1, 2, 3, 3, 1, 3, 7, 1, 3,10, 2, 2,
C                  GO PM RM UJ GB NC FC MC BI BE OS OH 
     &              3, 2, 2, 2, 2, 3, 3, 3, 3, 3, 2, 3/
      END
