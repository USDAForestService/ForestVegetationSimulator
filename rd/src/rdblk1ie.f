      BLOCK DATA RDBLK1
      IMPLICIT NONE
C----------
C  **RDBLK1-IE     LAST REVISION:  08/26/14
C----------
C
C  Purpose :
C     This block data file initializes constants in the Root Disease
C     extension to FVS variant Inland Empire 23 species.
C
C  This file created by Lance David 08/07/2003
C
C  Previous revision date 04/30/09
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
C.... In the Root Disease model, the defaults for these variables 
C.... are indexed as follows :
C....
C.... Species #|  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 |
C.... Species  | WP | WL | DF | GF | WH | RC | LP | ES | AF | PP |
C....
C.... Species #| 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
C.... Species  | MH | SP | WF | IC | RF | SF | OS | OH | AS | BS |
C....
C.... Species #| 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 |
C.... Species  | CB | WB | LM | CO | WS | JU | OC | GS | BO | OTH|
C....
C.... Species #| 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 |
C.... Species  | JP | TO | PI | YC | RW | LL | KP | PY | NF | NH |
C....
C.... IRTSPC can be modified for different variants of FVS so
C.... that species match between FVS and the root disease
C.... model.
C....
C.... The following IRTSPC is used with variant IE 23 species.
C.... IE variant species Mountain maple (20) and Paper birch (21)
C.... do not have matching species in the RD models and are mapped
C.... to RD Other species (30).
C....
C....      1   2   3   4   5   6   7   8   9  10  11  12
C....     WP  WL  DF  GF  WH  RC  LP  ES  AF  PP  MH  WB
C....
C....     13  14  15  16  17  18  19  20  21  22  23
C....     LM  LL  PI  JU  PY  AS  CO  MM  PB  OH  OS
C....

      DATA IRTSPC /
     &     1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 22,
     &    23, 36, 33, 26, 38, 19, 24, 30, 30, 18, 17/


      DATA DICLAS /0.0, 5.0, 12.0, 24.0/
      DATA DSFAC  /1.0, 0.75/

      DATA IOUNIT /22/
      DATA IRUNIT /18/
      
      END
