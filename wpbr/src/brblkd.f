      BLOCK DATA BRBLKD
C**********************************************************************
C  **BRBLKD--NI   DATE OF LAST REVISION:  06/12/2006
C----------------------------------------------------------------------
C  Purpose:
C  Block data for the Blister Rust model.
C----------
C     SPECIES LIST FOR INLAND EMPIRE (NI) VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)        PINUS MONTICOLA
C     2 = WESTERN LARCH (L)              LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)               PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)                 ABIES GRANDIS
C     5 = WESTERN HEMLOCK (WH)           TSUGA HETEROPHYLLA
C     6 = WESTERN REDCEDAR (C)           THUJA PLICATA
C     7 = LODGEPOLE PINE (LP)            PINUS CONTORTA
C     8 = ENGLEMAN SPRUCE (S)            PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)             ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)            PINUS PONDEROSA
C    11 = OTHER (OT)  grown as MOUNTAIN HEMLOCK   TSUGA MERTENSIANA
C
C----------------------------------------------------------------------
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  13-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Initialize ISPBR array and IBRDAM variable.
C  30-MAR-2001 Lance R. David (FHTET)
C     Changed canker file default format and added stock type.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR array to BRSPM. Added BR alpha species code array,
C     BRSPC.
C  08-MAY-2006 Lance R. David (FHTET)
C     Changed random number seed variable names to unique variables
C     BRS0, BRSS.
C  02-JUN-2006 Lance R. David (FHTET)
C     Changed I/O units from 25, 26, 27 to units 55, 56, 57
C  12-JUN-2006 Lance R. David (FHTET)
C     Moved RIBUS initilization to brinit.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'

C.... Data statements.

      DATA RSF/2.3,1.0,0.64/
      DATA BRPI/3.14159/
      DATA BRS0/55329D0/, BRSS/55329./

C.... Input canker data file format. Order of variables is:
C.... TreeID, StockType, TreeAge, DistUp, DistOut, %Gird, TotalCount
C.... Col 1-7      9       11-13   15-19   21-25   27-30    32-35

      DATA ICFMT/'(I7,1X,I1,1X,F3.0,1X,F5.1,1X,F5.1,1X,F4.0,1X,F4.0)'/
      DATA ICIN/55/, IDTOUT/56/, IDCOUT/57/

C.... Blister Rust Species Map.
C.... WPBR Model species/indices are: WP/1, SP/2
C.... Western White Pine set as host to Blister Rust model species 1.

      DATA BRSPM/1,0,0,0,0,0,0,0,0,0,0/

C.... Blister Rust model alpha species codes.
      DATA BRSPC/'WP  ','SP  '/

C.... Blister Rust damage code
      DATA IBRDAM/36/

      END
