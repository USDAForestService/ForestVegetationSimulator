      SUBROUTINE SITERANGE(IWHO,IS,SILOW,SIHIGH)
      IMPLICIT NONE
C----------
C SO $Id: siterange.f 0000 2018-06-25 00:00:00Z gedixon $
C----------
C
C SUBROUTINE TO RETURN THE SITE INDEX RANGE FOR A SPECIES.
C
C THIS SUBROUTINE IS CALLED FROM **HTGF**,**REGENT**, AND **SITSET**.  
C----------
C  SPECIES ORDER:
C  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
C 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
C 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
C 31=MB, 32=OS, 33=OH
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
COMMONS
C----------
      REAL SITELO(MAXSP),SITEHI(MAXSP)
      REAL SILOW, SIHIGH
      INTEGER IS,IWHO
C----------
C  DEFINITION OF VARIABLES:
C----------
C      IS -- FVS SPECIES INDEX NUMBER
C    IWHO -- 1 IF CALLED FROM SUBROUTINE **SITSET**
C            2 IF CALLED FROM SUBROUTINE **REGENT**
C  SITEHI -- UPPER SITE INDEX VALUES 
C  SITELO -- LOWER SITE INDEX VALUES 
C----------
C  DATA STATEMENTS:
C----------
C  THESE VALUES SHOULD BE BASED ON THE BASE-AGE OF THE SITE CURVE
C  BEING USED FOR THAT SPECIES.
C----------
      DATA SITELO/
     &  13.,  27.,  21.,   5.,   5.,   5.,   5.,  12.,  10.,   7.,
     &   5.,   9.,   6.,   4.,   7.,  20.,  60.,  29.,   6.,   5.,
     &   5.,  56., 108.,  30.,  10.,  10.,  21.,  20.,   5.,   5.,
     &   5.,   5.,   5./
C
      DATA SITEHI/
     & 137., 178., 148., 195., 133., 169., 140., 227., 134., 176.,
     &  40., 173., 127., 221., 210.,  65., 147., 152., 203.,  75.,
     & 100., 192., 142.,  66., 191., 104.,  85.,  93., 100.,  75.,
     &  75., 175., 125./
C----------
      SELECT CASE (IWHO)
C
C  CALLED FROM SUBROUTINES **SITSET** AND **REGENT**
C
        CASE (1,2)
          SILOW = SITELO(IS)
          SIHIGH = SITEHI(IS)
C
C  SPACE FOR ADDITIONAL CALLS
C
        CASE DEFAULT
          SILOW = 0.
          SIHIGH = 999.
C
      END SELECT
C
      RETURN
      END
