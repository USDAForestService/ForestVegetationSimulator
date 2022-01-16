      BLOCK DATA RDBLK1
      IMPLICIT NONE
C----------
C  **RDBLK1-EM     LAST REVISION:  08/27/14
C----------
C
C  Purpose :
C     This block data file initializes constants in the Root Disease
C     extension to FVS.
C
C  Previous revision date 04/22/09
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
C.... IRTSPC can be modified for different variants of FVS so
C.... that species match between FVS and the root disease
C.... model.
C....
C.... The following IRTSPC is for the EM variant.
C.... Species codes from the FVS EM variant JSP array in blkdat.f
C....              WB, WL, DF, LM, LL, RM, LP, ES, AF, PP,
C....              GA, AS, CW, BA, PW, NC, PB, OS, OH

      DATA IRTSPC /22,  2,  3, 23, 36, 26,  7,  8,  9, 10,
     >             18, 19, 24, 24, 24, 24, 18, 17, 18/

      DATA DICLAS /0.0, 5.0, 12.0, 24.0/
      DATA DSFAC  /1.0, 0.75/

      DATA IOUNIT /22/
      DATA IRUNIT /18/
      
      END
