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
C  Previous revision date 02/20/98
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
C.... Species #|  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 |
C.... Species  | WP | WL | DF | GF | WH |  C | LP |  S | AF | PP |
C....
C.... Species #| 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
C.... Species  | MH | SP | WF | IC | RF | SF | OS | OH | AS | BS |
C....
C.... Species #| 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 |
C.... Species  | CB | WB | LM | CW | WS | J  | OC | GS | BO | OTH|
C....
C.... Species #| 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 |
C.... Species  | JP | TO | P  | YC | RW | LL | KP | PY | NF | NH |
C....
C.... IRTSPC can be modified for different variants of FVS so
C.... that species match between FVS and the root disease
C.... model.

C.... The following IRTSPC is the base root disease model IRTSPC
C.... It is used for variants : NI, CI, and KT

      DATA IRTSPC /27, 12,  3, 13, 30, 14, 29, 32, 15, 10, 18, 35/

      DATA DICLAS /0.0, 5.0, 12.0, 24.0/
      DATA DSFAC  /1.0, 0.75/

      DATA IOUNIT /22/
      DATA IRUNIT /18/

      END
