      BLOCK DATA RDBLK1
      IMPLICIT NONE
C----------
C  **RDBLK1-UT     LAST REVISION:  08/27/14
C----------
C
C  Purpose :
C     This block data file initializes constants in the Root Disease
C     extension to FVS.
C
C  Revision History:
C  22-JUN-00 Lance David (FHTET)
C    .FVS UT variant expanded to 14 species. IRTSPC array set accordingly.
C  19-AUG-09 Lance David (FMSC)
C    .FVS UT variant expanded to 24 species. IRTSPC array set accordingly.
C
COMMONS
C

C.... PARAMETER INCLUDE FILES

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
      INCLUDE 'METRIC.F77'

C.... COMMON INCLUDE FILES

      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDCRY.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'RDADD.F77'


C.... The array IRTSPC is used to index the species dependent arrays
C.... HABFAC, PNINF, PKILLS, RRJSP, ISPS, DBIFAC, HTIFAC, PROOT,
C.... RSLOP, ROWDOM, ROWIBP, RRPSWT, SSSFAC, IDITYP, PCOLO.
C.... In the root disease model, the defaults for these variables 
C.... are indexed as follows :
C....
C.... RD Model species:
C....   #  Code  Name                   #  Code  Name
C....   1   WP   WHITE PINE            21   CB   CORKBARK FIR    
C....   2   WL   WESTERN LARCH         22   WB   WHITEBARK PINE  
C....   3   DF   DOUGLAS-FIR           23   LM   LIMBER PINE     
C....   4   GF   GRAND FIR             24   CO   COTTONWOOD      
C....   5   WH   WESTERN HEMLOCK       25   WS   WHITE SPRUCE    
C....   6   RC   W. REDCEDAR           26   JU   JUNIPER         
C....   7   LP   LODGEPOLE PINE        27   OC   OTHER CONIFERS  
C....   8   ES   ENGELMANN SPRUCE      28   GS   GIANT SEQUOIA   
C....   9   AF   SUBALPINE FIR         29   BO   BLACK OAK       
C....  10   PP   PONDEROSA PINE        30   OTH  OTHER           
C....  11   MH   MOUNTAIN HEMLOCK      31   JP   JEFFREY PINE    
C....  12   SP   SUGAR PINE            32   TO   TANOAK/CHINKAPIN
C....  13   WF   WHITE FIR             33   PI   PINYON PINE     
C....  14   IC   INCENSE CEDAR         34   YC   YELLOW CEDAR    
C....  15   RF   RED FIR               35   RW   REDWOOD         
C....  16   SF   P. SILVER FIR         36   LL   SUBALPINE LARCH 
C....  17   OS   OTHER SOFTWOOD        37   KP   KNOBCONE PINE   
C....  18   OH   OTHER HARDWOOD        38   PY   PACIFIC YEW     
C....  19   AS   ASPEN                 39   NF   NOBLE FIR       
C....  20   BS   BLUE SPRUCE           40   NH   NON-HOST        
C....
C.... SPECIES LIST FOR UTAH VARIANT. ***** 24 species *****
C.... 
C....  ------FVS UT VARIANT-------   WRD MODEL SPECIES    ANNOSUS     
C....   # CD COMMON NAME             OR SURROGATE SP.     TYPE        
C....  -- -- --------------------- ---------------------  --------
C....   1 WB WHITEBARK PINE          WHITEBARK PINE (22)  P-TYPE      
C....   2 LM LIMBER PINE                LIMBER PINE (23)  P-TYPE      
C....   3 DF DOUGLAS-FIR                DOUGLAS-FIR (3)   S-TYPE      
C....   4 WF WHITE FIR                    WHITE FIR (13)  S-TYPE      
C....   5 BS BLUE SPRUCE                BLUE SPRUCE (20)  S-TYPE      
C....   6 AS QUAKING ASPEN                    ASPEN (19)  NON-HOST    
C....   7 LP LODGEPOLE PINE          LODGEPOLE PINE (7)   P-TYPE      
C....   8 ES ENGLEMANN SPRUCE      ENGELMANN SPRUCE (8)   S-TYPE      
C....   9 AF SUBALPINE FIR            SUBALPINE FIR (9)   S-TYPE      
C....  10 PP PONDEROSA PINE          PONDEROSA PINE (10)  P-TYPE      
C....  11 PI COMMON PINYON              PINYON PINE (33)  P-TYPE      
C....  12 WJ WESTERN JUNIPER                JUNIPER (26)  P-TYPE      
C....  13 GO GAMBEL OAK              OTHER HARDWOOD (18)  NON-HOST 
C....  14 PM SINGLELEAF PINYON          PINYON PINE (33)  P-TYPE      
C....  15 RM ROCKY MOUNTAIN JUNIPER         JUNIPER (26)  P-TYPE
C....  16 UJ UTAH JUNIPER                   JUNIPER (26)  P-TYPE
C....  17 GB BRISTLECONE PINE           PINYON PINE (33)  P-TYPE      
C....        (Great Basin)       
C....  18 NC NARROWLEAF COTTONWOOD       COTTONWOOD (24)  NON-HOST
C....  19 FC FREMONT COTTONWOOD          COTTONWOOD (24)  NON-HOST
C....  20 MC CURLLEAF MOUNTAIN-            NON-HOST (40)  NON-HOST 
C....        MAHOGANY
C....  21 BI BIGTOOTH MAPLE                NON-HOST (40)  NON-HOST 
C....  22 BE BOX ELDER                     NON-HOST (40)  NON-HOST
C....  23 OS OTHER SOFTWOODS         OTHER SOFTWOOD (17)  P-TYPE
C....  24 OH OTHER HARDWOODS         OTHER HARDWOOD (18)  NON-HOST
C....
C.... IRTSPC can be modified for different variants of FVS so
C.... that species match between FVS and the root disease
C.... model.
C.... 
C.... The following IRTSPC is for the UT 24 species variant.
C....              1   2   3   4   5   6   7   8   9   10  11  12  -- FVS index
C....              WB  LM  DF  WF  BS  AS  LP  ES  AF  PP  PI  WJ  -- FVS species
      DATA IRTSPC /22, 23, 3,  13, 20, 19, 7,  8,  9,  10, 33, 26,
C....              13  14  15  16  17  18  19  20  21  22  23  24  -- FVS index
C....              GO  PM  RM  UJ  GB  NC  FC  MC  BI  BE  OS  OH  -- FVS species
     &             18, 33, 26, 26, 33, 24, 24, 40, 40, 40, 17, 18 /

      DATA DICLAS /0.0, 5.0, 12.0, 24.0/
      DATA DSFAC  /1.0, 0.75/

      DATA IOUNIT /22/
      DATA IRUNIT /18/
      
      END
