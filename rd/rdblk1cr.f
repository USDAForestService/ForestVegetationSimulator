      BLOCK DATA RDBLK1
      IMPLICIT NONE
C----------
C  **RDBLK1--CR     LAST REVISION:  08/27/14
C
C     The IRTSPC array is now initialized using a data statement in this
C     subroutine rather than in the RDINCR subroutine (RNH Dec98)
C     
C----------
C
C  Purpose :
C     This block data file initializes constants in the Root Disease
C     extension to FVS for the CR variant.
C
C  Revisions:
C     16-MAR-2000 Lance R. David
C       Changed mapping of Bristlecone Pine from Limber Pine to Pinyon.
C     07-JUL-2009 Lance R. David
C       Updated species mapping for Central Rockies expanded species 
C       list. Did not receive response from field on request to assist
C       with new species surrogate RD model species assignment so just
C       used my best judgement on suitable surrogates.
C----------
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
C.... IRTSPC can be modified for different variants of FVS so
C.... that species match between FVS and the root disease.
C....
C.... For this data block, CR variant:
C.... Bristlecone pine (BC) is mapped as Pinyon (PI 33);
C.... Utah Juniper (UJ), alligator juniper (AJ), Rocky Mountain juniper (RM),
C.... oneseed juniper (OJ) and Eastern redcedar (ER) is mapped as Juniper (JU 26);
C.... Gamble oak (GO) and Emory oak (EM) is mapped as other hardwood (OH 18);
C.... Arizona white oak (AW), Bur oak (BK) and silverleaf oak (SO) is mapped
C.... to black oak (BO 29)
C....
C.... The following IRTSPC is for the CR 38 species variant.
C.... Species codes from the FVS CR variant JSP array in blkdat.f
C....
C.... FVS Central Rockies Species list for all model types:
C....               1   2   3   4   5   6   7   8   9  10  11  12  -- FVS index
C....              AF  CB  DF  GF  WF  MH  RC  WL  BC  LM  LP  PI  -- FVS species
      DATA IRTSPC / 9, 21,  3,  4, 13, 11,  6,  2, 33, 23,  7, 33,
C....              13  14  15  16  17  18  19  20  21  22  23  24  -- FVS index
C....              PP  WB  SW  UJ  BS  ES  WS  AS  NC  PW  GO  AW  -- FVS species
     &             10, 22,  1, 26, 20,  8, 25, 19, 24, 24, 18, 29,
C....              25  26  27  28  29  30  31  32  33  34  35  36  -- FVS index
C....              EM  BK  SO  PB  AJ  RM  OJ  ER  PM  PD  AZ  CI  -- FVS species
     &             18, 29, 29, 40, 26, 26, 26, 26, 33, 33, 33, 10,
C....              37  38                                          -- FVS index
C....              OS  OH                                          -- FVS species
     &             17, 18/

      DATA DICLAS /0.0, 5.0, 12.0, 24.0/
      DATA DSFAC  /1.0, 0.75/

      DATA IOUNIT /22/
      DATA IRUNIT /18/
      
      END
