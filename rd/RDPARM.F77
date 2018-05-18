CODE SEGMENT RDPARM
C----------
C  **RDPARM      LAST REVISION:  08/15/14
C----------
C
C  Parameters for the root disease model are :
C
      INTEGER DSO, DSII, DSIU, IRRTRE, IRRTP1, ITOTSP, ITOTRR

      PARAMETER (DSO=1)
      PARAMETER (DSII=2)
      PARAMETER (DSIU=3)

      PARAMETER (IRRTRE=1500)
      PARAMETER (IRRTP1=IRRTRE+1)
      PARAMETER (ITOTSP=40)
      PARAMETER (ITOTRR=4)
C
C     DSO    - Code for Dead Standing Outside (includes non-standing)
C     DSII   - Code for Dead Standing Inside Infected (includes
C              non-standing trees)
C     DSIU   - Code for Dead Standing Inside Uninfected (includes
C              non-standing)
C     IRRTRE - The max number of tree records that root disease can
C              process
C     IRRTP1 - The max number of tree records plus 1.
C     ITOTSP - Total number of species used by all variants.
C     ITOTRR - Total possible number of root diseases represented by
C              the model
C                 1 = P-TYPE ANNOSUS
C                 2 = S-TYPE ANNOSUS
C                 3 = ARMILLARIA
C                 4 = PHELLINUS
C
C            **If this is changed, the data statements in ANBLK1 must
C              also change.
C
C-----END SEGMENT

