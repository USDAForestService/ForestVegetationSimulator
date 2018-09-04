      BLOCK DATA RDBLK1
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  Purpose :
C     This block data file initializes constants in the Root Disease
C     extension to FVS.
C
C  Previous revision date 04/25/11
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
C.... SPECIES LIST FOR WESTERN SIERRAS VARIANT. ***** 43 species *****
C.... 
C.... ----------FVS WS VARIANT-----------   WRD MODEL SPECIES    ANNOSUS     
C....  # CD COMMON NAME                     OR SURROGATE SP.     TYPE        
C.... -- -- ----------------------------- ---------------------  --------
C....  1 SP SUGAR PINE                          SUGAR PINE (12)  P-TYPE 
C....  2 DF DOUGLAS-FIR                        DOUGLAS-FIR (3)   S-TYPE 
C....  3 WF WHITE FIR                            WHITE FIR (13)  S-TYPE 
C....  4 GS GIANT SEQUOIA                    GIANT SEQUOIA (28)  S-TYPE 
C....  5 IC INCENSE CEDAR                    INCENSE CEDAR (14)  P-TYPE 
C....  6 JP JEFFREY PINE                      JEFFREY PINE (31)  P-TYPE 
C....  7 RF CALIFORNIA RED FIR                     RED FIR (15)  S-TYPE 
C....  8 PP PONDEROSA PINE                  PONDEROSA PINE (10)  P-TYPE 
C....  9 LP LODGEPOLE PINE                  LODGEPOLE PINE (7)   P-TYPE 
C.... 10 WB WHITEBARK PINE                  WHITEBARK PINE (22)  P-TYPE 
C.... 11 WP WESTERN WHITE PINE                  WHITE PINE (1)   P-TYPE 
C.... 12 PM SINGLELEAF PINYON                  PINYON PINE (33)  P-TYPE 
C.... 13 SF PACIFIC SILVER FIR               P. SILVER FIR (16)  S-TYPE 
C.... 14 KP KNOBCONE PINE                    KNOBCONE PINE (37)  P-TYPE 
C.... 15 FP FOXTAIL PINE                     KNOBCONE PINE (37)  P-TYPE 
C.... 16 CP COULTER PINE                     KNOBCONE PINE (37)  P-TYPE 
C.... 17 LM LIMBER PINE                        LIMBER PINE (23)  P-TYPE 
C.... 18 MP MONTEREY PINE                   PONDEROSA PINE (10)  P-TYPE 
C.... 19 GP GRAY PINE                        KNOBCONE PINE (37)  P-TYPE 
C....       (OR CALIFORNIA FOOTHILL PINE)
C.... 20 WE WASHOE PINE                      KNOBCONE PINE (37)  P-TYPE 
C.... 21 GB GREAT BASIN BRISTLECONE PINE       PINYON PINE (33)  P-TYPE 
C.... 22 BD BIGCONE DOUGLAS-FIR                DOUGLAS-FIR (3)   S-TYPE 
C.... 23 RW REDWOOD                                REDWOOD (35)  S-TYPE 
C.... 24 MH MOUNTAIN HEMLOCK              MOUNTAIN HEMLOCK (11)  S-TYPE 
C.... 25 WJ WESTERN JUNIPER                        JUNIPER (26)  P-TYPE 
C.... 26 UJ UTAH JUNIPER                           JUNIPER (26)  P-TYPE 
C.... 27 CJ CALIFORNIA JUNIPER                     JUNIPER (26)  P-TYPE 
C.... 28 LO CALIFORNIA LIVE OAK                  BLACK OAK (29)  NON-HOST
C.... 29 CY CANYON LIVE OAK                      BLACK OAK (29)  NON-HOST
C.... 30 BL BLUE OAK                             BLACK OAK (29)  NON-HOST
C.... 31 BO CALIFORNIA BLACK OAK                 BLACK OAK (29)  NON-HOST
C.... 32 VO VALLEY OAK                           BLACK OAK (29)  NON-HOST
C....       (OR CALIFORNIA WHITE OAK)
C.... 33 IO INTERIOR LIVE OAK                    BLACK OAK (29)  NON-HOST
C.... 34 TO TANOAK                        TANOAK/CHINKAPIN (32)  NON-HOST
C.... 35 GC GIANT CHINKAPIN               TANOAK/CHINKAPIN (32)  NON-HOST
C.... 36 AS QUAKING ASPEN                            ASPEN (19)  NON-HOST
C.... 37 CL CALIFORNIA-LAUREL                     NON-HOST (40)  NON-HOST
C.... 38 MA PACIFIC MADRONE                       NON-HOST (40)  NON-HOST
C.... 39 DG PACIFIC DOGWOOD                       NON-HOST (40)  NON-HOST
C.... 40 BM BIGLEAF MAPLE                         NON-HOST (40)  NON-HOST
C.... 41 MC CURLLEAF MOUNTAIN-MAHOGANY            NON-HOST (40)  NON-HOST
C.... 42 OS OTHER SOFTWOODS                 OTHER SOFTWOOD (17)  P-TYPE  
C.... 43 OH OTHER HARDWOODS                 OTHER HARDWOOD (18)  NON-HOST
C....
C....
C.... IRTSPC can be modified for different variants of FVS so
C.... that species match between FVS and the root disease
C.... model.

C.... The following IRTSPC is for the WS variant 43 species.

      DATA IRTSPC /12,  3, 13, 28, 14, 31, 15, 10,  7, 22,  1,
     &             33, 16, 37, 37, 37, 23, 10, 37, 37, 33,  3,
     &             35, 11, 26, 26, 26, 29, 29, 29, 29, 29, 29,
     &             32, 32, 19, 40, 40, 40, 40, 40, 17, 18 /

      DATA DICLAS /0.0, 5.0, 12.0, 24.0/
      DATA DSFAC  /1.0, 0.75/

      DATA IOUNIT /22/
      DATA IRUNIT /18/
      
      END
