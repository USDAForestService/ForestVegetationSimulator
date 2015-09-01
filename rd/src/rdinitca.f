      SUBROUTINE RDINIT
      IMPLICIT NONE
C----------
C  **RDINIT--CA      LAST REVISION:  03/24/15
C----------
C
C  Purpose :
C     This is the subroutine that initializes default values for
C     common block variables.
C
C  Called By :
C     INITRE  [FVS]
C
C  Calls :
C     none
C
C  Local Variables : 
C     I      - INT
C              counter
C     ICYLS  - INT
C              Counter to loop through cycles.
C     IDI    - INT
C              Counter to loop through disease species.
C     ISCL   - INT
C              Counter to loop through size classes.
C     ITEMP1 - INT
C              Temporary array used to initialize ISPS.
C     ITEMP2 - INT
C              Temporary array used to initialize IDITYP.
C     ITEMP3 - INT
C              Temporary array used to initialize IRGEN.
C     ITREC  - INT
C              Counter to loop through tree records.
C     IWOOD  - INT
C              Counter to loop through wood types.
C     J      - INT
C              Counter
C     KSP    - INT
C              Counter to loop through tree species.
C     NC     - INT 
C              Number of centers.
C     NCCS   - INT
C              Counter for number of centers in spread rate 
C              simulation.
C     TEMP1  - REAL
C              Temporary array used to initialize DECFN.
C     TEMP2  - REAL
C              Temporary array used to initialize YRSITF.
C     TEMP3  - REAL
C              Temporary array used to initialize RSITFN.
C     TEMP4  - REAL
C              Temporary array used to initialize HABFAC.
C     TEMP5  - REAL
C              Temporary array used to initialize PCOLO.
C     TEMP6  - REAL
C              Temporary array used to initialize PKILLS. 
C     TEMP7  - REAL
C              Temporary array used to initialize PNINF. 
C     TEMP8  - REAL 
C              Temporary array used to initialize RROBMR.
C     TEMP9  - REAL 
C              Temporary array used to initialize RROBOL.
C     TEMP10 - REAL 
C              Temporary array used to initialize RROBRD.
C     TEMP11 - REAL 
C              Temporary array used to initialize RROBSC.
C     TEMP12 - REAL 
C              Temporary array used to initialize RROBTS.
C     TEMP13 - REAL
C              Temporary array used to initialize RSLOP.
C     TEMP14 - REAL
C              Temporary array used to initialize STCUT.
C     TEMP15 - REAL
C              Temporary array used to initialize XMINLF.
C     TEMP16 - REAL
C              Temporary array used to initialize XXINF.
C     TEMP17 - REAL
C              Temporary array used to initialize YYINF.
C
C  Common Block Variables Used :
C
C  Revision History
C     05/22/97 - Matt Thompson (FHTET)
C                Added initialization for almost all of the variables
C                that were set in the block data file RDBLK1.  The RDBLK1
C                block data file only initializes constants.  This was 
C                done so that the Root Disease model could be run 
C                on multiple stands during one execution of the model.
C  26-MAR-2002 Lance R. David (FHTET)
C     Changed initial value of IRINIT from 13500 to 10*MAXTRE.
C     Added initialization of variables: IDOBB, LBBON and LRTYPE.
C  06-JUN-2002 Lance R. David (FHTET)
C     The index variable used on the TEMP15 variable to initialize
C     XMINLF was I instead if IDI so default values were not
C     properly loaded. Near label 1700.
C  22-JUL-02 Lance R. David (FHTET)
C     Removed unused array PROBO. It was also unused in the old
C     annosus model.
C  08/28/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C  03/24/15 Lance R. David (FMSC)
C     Added initialization of IDRDOUT array, report IDs for General
C     Report Writer facility.
C
C----------------------------------------------------------------------

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
      INCLUDE 'METRIC.F77'

C.... Common include files.

      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDCRY.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDADD.F77'

C.... Local variable declarations.

      INTEGER I, I1, I2, ICYLS, IDI, ISCL, ITREC, IWOOD, J,
     &        K, KSP, NC, NCCS,
     &        ITEMP1(ITOTSP), ITEMP2(ITOTSP), ITEMP3(10)
      REAL    TEMP1(ITOTRR, 2, 2)
      REAL    TEMP2(ITOTRR, 2, 2)
      REAL    TEMP3(ITOTRR, 2)
      REAL    TEMP4(ITOTSP, ITOTRR, 2)
      REAL    TEMP5(ITOTSP, ITOTRR)
      REAL    TEMP6(ITOTSP, ITOTRR)
      REAL    TEMP7(ITOTSP, ITOTRR)
      REAL    TEMP8(4)
      REAL    TEMP9(4)
      REAL    TEMP10(4)
      REAL    TEMP11(4)
      REAL    TEMP12(4)
      REAL    TEMP13(ITOTSP)
      REAL    TEMP14(5)
      REAL    TEMP15(4)
      REAL    TEMP16(5)
      REAL    TEMP17(5)

C.... Data statements.

C.... Data for array ISPS.
C.... ISPS is an array that holds the wood type for each tree species.
C.... 1 = resinous
C.... 2 = non-resinous

      DATA ITEMP1 / 1, 1, 1, 2, 2, 1, 1, 2, 2, 1,
     &              2, 1, 2, 2, 2, 2, 1, 2, 2, 2,
     &              2, 1, 1, 2, 2, 2, 2, 1, 1, 2,
     &              1, 1, 2, 1, 1, 2, 2, 2, 2, 2 /

C.... Data for array IDITYP.
C.... IDITYP is an array that holds the Annosus disease type for
C.... each tree species.       
C.... 0 = Non-host
C.... 1 = P-type Annosus.
C.... 2 = S-type Annosus.
C
C     The array ITEMP2 was modified to specify non-host for entries
C     18 (OH), 19 (OH) and 29 (BO), (RNH June98) 
C     
C....
C.... 3 = Armillaria, 4=Phellinus (both are posible for all species so
C....     not included here).

      DATA ITEMP2 / 1, 2, 2, 2, 2, 2, 1, 2, 2, 1,
     &              2, 1, 2, 1, 2, 2, 1, 0, 0, 2,
     &              1, 1, 1, 0, 2, 1, 1, 2, 0, 1,
     &              1, 0, 1, 2, 2, 2, 1, 1, 2, 0,
     &              0, 2, 1, 1, 1, 2 /

C.... Data for array IRGEN.
      DATA ITEMP3 / 4, 24, 0, 3000, 0, 0, 0, 3000, 3000, 0 /

C.... Data for variable DECFN.
C.... Line 1: slope     Line 2: intercept
C.... block 1: DBH <= 12   block 2: DBH > 12

      DATA TEMP1  /0.02212, 0.02212, 0.0,     0.0,
     &             1.2032,  1.2032,  1.6045,  1.2834,
     &             0.02212, 0.02212, 0.03214, 0.03214,
     &             1.2032,  1.2032,  1.1459,  0.9167/

C.... Data for variable YRSITF.
C.... Line 1: slope     Line 2:  intercept
C.... block 1: DBH <= 12   block 2: DBH > 12

      DATA TEMP2  /2.0, 2.0, 1.1111, 1.1111,
     &             0.0, 0.0, 0.0,    0.0,
     &             2.0, 2.0, 0.5556, 0.5556,
     &             0.0, 0.0, 6.6667, 6.6667/

C.... Data for variable RSITFN.
C.... Line 1: slope     Line 2:  intercept

      DATA TEMP3  /0.166667, 0.166667, 0.297, 0.2377,
     &                  0.0,      0.0,   0.0,    0.0/

C.... Data for array HABFAC.
C.... HABFAC: Relative time to death.
C.... 1st section: disease=1, P type annosus, habitat=1
C.... 2nd section: disease=2, S type annosus, habitat=1
C.... 3rd section: disease=3, Armillaria,     habitat=1
C.... 4th section: disease=4, Phellinus,      habitat=1
C.... Last half:   habitat=2

      DATA TEMP4  /
     &      1.0,  1.0,  1.0,  1.5, 1.75,  1.0,  1.5,  1.0,  1.5,  0.5,
     &     1.75,  1.0,  1.5, 1.75,  1.5,  1.5,  1.5,  1.0, 99.0,  1.0,
     &      1.0,  1.0,  1.0, 99.0,  1.0,  0.5,  1.0, 1.75, 99.0,  1.0,
     &      0.5, 99.0,  1.0,  1.5,  2.0,  1.5,  1.0,  1.5,  1.5, 99.0,
     &     99.0,  1.5,  1.5,  1.5,  1.5,  1.0, 
     &      1.0,  1.0,  1.0,  1.5, 1.75,  1.0,  1.5,  1.0,  1.5,  0.5,
     &     1.75,  1.0,  1.5, 1.75,  1.5,  1.5,  1.5,  1.0, 99.0,  1.0,
     &      1.0,  1.0,  1.0, 99.0,  1.0,  0.5,  1.0, 1.75, 99.0,  1.0,
     &      0.5, 99.0,  1.0,  1.5,  2.0,  1.5,  1.0,  1.5,  1.5, 99.0,
     &     99.0,  1.5,  1.5,  1.5,  1.5,  1.0, 
     &      1.8,  2.0,  1.0, 0.75,  0.9,  1.2,  1.8,  1.1, 0.75,  1.8,
     &      0.9,  1.8, 0.75,  0.9, 0.75, 0.75,  1.8,  0.9,  0.9,  1.1,
     &     0.75,  1.8,  1.8,  0.9,  1.1,  0.9, 0.75,  0.9,  0.9,  0.9,
     &      1.8,  0.9,  0.9,  0.2, 10.0,  1.1, 10.0,  0.2, 0.75, 99.0,
     &      1.2, 0.75,  1.8,  1.8,  1.8,  1.1,  
     &      3.0,  1.5,  1.0,  1.0,  1.5, 10.0,  3.0,  1.1,  1.0,  3.0,
     &      1.5,  3.0,  1.0,  1.5,  1.0,  1.0,  3.0,  1.5,  1.5,  1.1,
     &      1.0,  3.0,  3.0,  1.5,  1.1,  1.5,  1.0,  1.5,  1.5,  1.5,
     &      3.0,  1.5,  1.5, 10.0, 10.0,  1.5, 10.0, 10.0,  1.0, 99.0,
     &     10.0,  1.5,  3.0,  3.0,  3.0,  1.5,
     &      1.0,  1.0,  1.0,  1.5, 1.75,  1.0,  1.5,  1.0,  1.5,  0.5,
     &     1.75,  1.0,  1.5, 1.75,  1.5,  1.5,  1.5,  1.0, 99.0,  1.0,
     &      1.0,  1.0,  1.0, 99.0,  1.0,  0.5,  1.0, 1.75, 99.0,  1.0,
     &      0.5, 99.0,  1.0,  1.5,  2.0,  1.5,  1.0,  1.5,  1.5, 99.0,
     &     99.0,  1.5,  1.5,  1.5,  1.5,  1.0, 
     &      1.0,  1.0,  1.0,  1.5, 1.75,  1.0,  1.5,  1.0,  1.5,  0.5,
     &     1.75,  1.0,  1.5, 1.75,  1.5,  1.5,  1.5,  1.0, 99.0,  1.0,
     &      1.0,  1.0,  1.0, 99.0,  1.0,  0.5,  1.0, 1.75, 99.0,  1.0,
     &      0.5, 99.0,  1.0,  1.5,  2.0,  1.5,  1.0,  1.5,  1.5, 99.0,
     &     99.0,  1.5,  1.5,  1.5,  1.5,  1.0, 
     &      1.8,  2.0,  1.0, 0.75,  0.9,  1.2,  1.8,  1.1, 0.75,  1.8,
     &      0.9,  1.8, 0.75,  0.9, 0.75, 0.75,  1.8,  0.9,  0.9,  1.1,
     &     0.75,  1.8,  1.8,  0.9,  1.1,  0.9, 0.75,  0.9,  0.9,  0.9,
     &      1.8,  0.9,  0.9,  0.2, 10.0,  1.1, 10.0,  0.2, 0.75, 99.0,
     &      1.2, 0.75,  1.8,  1.8,  1.8,  1.1,  
     &      3.0,  1.5,  1.0,  1.0,  1.5, 10.0,  3.0,  1.1,  1.0,  3.0,
     &      1.5,  3.0,  1.0,  1.5,  1.0,  1.0,  3.0,  1.5,  1.5,  1.1,
     &      1.0,  3.0,  3.0,  1.5,  1.1,  1.5,  1.0,  1.5,  1.5,  1.5,
     &      3.0,  1.5,  1.5, 10.0, 10.0,  1.5, 10.0, 10.0,  1.0, 99.0,
     &     10.0,  1.5,  3.0,  3.0,  3.0,  1.5 /

C.... Data for array PCOLO.
C.... PCOLO is an array that holds the proportion roots colonized after
C.... death.
C.... blk1 : P-type Annosus.
C.... blk2 : S-type Annosus.
C.... blk3 : Armillaria - assume 100% after death.
C.... blk4 : Phellinus  - assume same after death as at death.

      DATA TEMP5 /
     &      0.8, 0.95, 0.95,  0.9,  0.9,  1.0, 0.75, 0.95,  0.9, 0.75,
     &      0.9,  0.8,  0.9, 0.95,  0.9,  0.9,  0.9,  1.0,  0.0,  1.0,
     &      1.0,  0.8,  1.0,  0.0, 0.95, 0.75,  0.9, 0.95,  0.0, 0.86,
     &     0.75,  0.0,  0.9, 0.95, 0.95, 0.95,  0.9, 0.95, 0.95,  0.0,
     &      0.0,  0.9, 0.75, 0.75, 0.75, 0.75,
     &      0.8, 0.95, 0.95,  0.9,  0.9,  1.0, 0.75, 0.95,  0.9, 0.75,
     &      0.9,  0.8,  0.9, 0.95,  0.9,  0.9,  0.9,  1.0,  0.0,  1.0,
     &      1.0,  0.8,  1.0,  0.0, 0.95, 0.75,  0.9, 0.95,  0.0, 0.86,
     &     0.75,  0.0,  0.9, 0.95, 0.95, 0.95,  0.9, 0.95, 0.95,  0.0,
     &      0.0,  0.9, 0.75, 0.75, 0.75, 0.75,
     &      1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,
     &      1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,
     &      1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,
     &      1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,
     &      1.0,  1.0,  1.0,  1.0,  1.0,  1.0,
     &      0.8,  0.6,  0.3, 0.85,  0.8,  0.8,  0.8,  0.8, 0.75, 0.65,
     &      0.8,  0.6,  0.3, 0.85,  0.3, 0.85,  0.8,  0.8, 0.75, 0.65,
     &      0.8,  0.8,  0.8,  0.8,  0.8,  0.8,  0.8,  0.8,  0.8,  0.8,
     &      0.8,  0.8,  0.8,  0.8,  0.8,  0.8,  0.8,  0.8,  0.8,  0.8,
     &      0.8,  0.6,  0.8,  0.8,  0.8,  0.6 /

C.... Data for array PKILLS.
C.... PKILLS is an array that holds the proportion of roots infected at
C.... death.
C.... blk1 : P-type Annosus.
C.... blk2 : S-type Annosus.
C.... blk3 : Armillaria.
C.... blk4 : Phellinus.

      DATA TEMP6 /
     &      0.6,  0.9,  0.9,  0.8,  0.8,  1.0,  0.5,  0.9,  0.8,  0.5,
     &      0.8,  0.6,  0.8,  0.9,  0.8,  0.8,  0.8,  1.0,  0.0,  1.0,
     &      1.0,  0.6,  1.0,  0.0,  0.9,  0.5,  0.75, 0.9,  0.0,  1.0,
     &      0.5,  0.0,  0.7,  0.8,  0.8,  0.8,  0.7,  0.8,  0.8,  1.0,
     &      0.0,  0.8,  0.5,  0.5,  0.5,  0.9,
     &      0.6,  0.9,  0.9,  0.8,  0.8,  1.0,  0.5,  0.9,  0.8,  0.5,
     &      0.8,  0.6,  0.8,  0.9,  0.8,  0.8,  0.8,  1.0,  0.0,  1.0,
     &      1.0,  0.6,  1.0,  0.0,  0.9,  0.5,  0.75, 0.9,  0.0,  1.0,
     &      0.5,  0.0,  0.7,  0.8,  0.8,  0.8,  0.7,  0.8,  0.8,  1.0,
     &      0.0,  0.8,  0.5,  0.5,  0.5,  0.9,
     &      0.3,  1.0,  0.8,  0.8,  0.8, 0.75,  0.3, 0.75,  0.8,  0.3,
     &      0.8,  0.3,  0.8,  0.8,  0.8,  0.8,  0.3,  0.8,  0.8, 0.75,
     &      0.8,  0.3,  0.3,  0.8, 0.75,  0.8,  0.8,  0.8,  0.8,  0.8,
     &      0.8,  0.8,  0.8,  0.8,  0.8,  0.8,  0.8,  0.8,  0.8,  1.0,
     &     0.75,  0.8,  0.3,  0.8,  0.8,  0.8,
     &     0.85, 0.75,  0.8,  0.6,  0.8, 0.85, 0.85, 0.65,  0.6, 0.85,
     &      0.8, 0.85,  0.6,  0.8,  0.6,  0.6, 0.85,  0.8,  0.8, 0.65,
     &      0.6, 0.85, 0.85,  0.8, 0.65,  0.8,  0.6,  0.8,  0.8,  0.8,
     &     0.85,  0.8,  0.8, 0.85, 0.85,  0.8, 0.85, 0.85,  0.6,  1.0 
     &      0.8,  0.6,  0.8,  0.8,  0.8,  0.6 /

C.... Data for array PNINF.
C.... PNINF is an array that holds the probability of infection.
C.... blk1 : P-type Annosus.
C.... blk2 : S-type Annosus.
C.... blk3 : Armillaria.
C.... blk4 : Phellinus.

      DATA TEMP7 /
     &      0.4,  0.4,  0.4,  0.5,  0.5,  0.0,  0.4,  0.4,  0.5,  0.5,
     &      0.5,  0.4,  0.5,  0.5,  0.5,  0.5,  0.5,  0.0,  0.0,  0.0,
     &      0.0,  0.4,  0.0,  0.0,  0.4,  0.4,  0.45, 0.5,  0.0,  0.0,
     &      0.5,  0.0,  0.4,  0.4,  0.4,  0.4,  0.4,  0.4,  0.4,  0.0,
     &      0.0,  0.5,  0.4,  0.4,  0.4,  0.4,
     &      0.4,  0.4,  0.4,  0.5,  0.5,  0.0,  0.4,  0.4,  0.5,  0.5,
     &      0.5,  0.4,  0.5,  0.5,  0.5,  0.5,  0.5,  0.0,  0.0,  0.0,
     &      0.0,  0.4,  0.0,  0.0,  0.0,  0.4,  0.45, 0.5,  0.0,  0.0,
     &      0.5,  0.0,  0.4,  0.4,  0.4,  0.4,  0.4,  0.4,  0.4,  0.0,
     &      0.0,  0.5,  0.4,  0.4,  0.4,  0.4,
     &      0.1,  0.05, 0.5,  0.6,  0.1,  0.1,  0.2,  0.5,  0.5,  0.2,
     &      0.1,  0.1,  0.6,  0.1,  0.5,  0.6,  0.1,  0.1,  0.1,  0.5,
     &      0.6,  0.1,  0.1,  0.1,  0.5,  0.1,  0.6,  0.1,  0.1,  0.1,
     &      0.2,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.4,  0.0,
     &      0.1,  0.6,  0.1,  0.1,  0.1,  0.5,
     &      0.1,  0.2,  0.4,  0.4,  0.2,  0.02, 0.1,  0.2,  0.4,  0.1,
     &      0.4,  0.1,  0.4,  0.4,  0.4,  0.4,  0.1,  0.4,  0.4,  0.2,
     &      0.4,  0.1,  0.1,  0.4,  0.2,  0.4,  0.4,  0.4,  0.4,  0.4,
     &      0.1,  0.4,  0.4, 0.02, 0.02,  0.4,  0.1, 0.02,  0.4,  0.0
     &     0.02,  0.2,  0.1,  0.1,  0.1,  0.2 /

C.... Data for array RROBMR.
      DATA TEMP8  / 0.1, 0.88, 0.88, 0.75 /

C.... Data for array RROBOL.
      DATA TEMP9  / 150.0, 10.0, 10.0, 1.0 /

C.... Data for array RROBRD.
      DATA TEMP10 / 0.0, 0.0, 0.3, 0.4 /

C.... Data for array RROBSC.
      DATA TEMP11 / 8.0, 0.0, 10.0, 4.0 /

C.... Data for array RROBTS.
      DATA TEMP12 / 10.0, 3.0, 4.0, 10.0 /

C.... Data for array RSLOP.
      DATA TEMP13 / 4*14.26, 14.5, 9.35, 4*14.26,
     &              10*14.26, 10*14.26, 10*14.26 /

C.... Data for array STCUT.
      DATA TEMP14 / 0.0, 12.0, 24.0, 48.0, 100.0 /

C.... Data for array XMINLF.
      DATA TEMP15 / 0.0, 0.0, 0.0, 20.0 /

C.... Data for array XXINF.
      DATA TEMP16 / 0.0, 3.9, 35.4, 0.0, 0.0 /

C.... Data for array YYINF.
      DATA TEMP17 / 0.0, 5.0, 40.0, 0.0, 0.0 /

C.... Initialize common block variables.

      DO 400 ICYLS = 1, 3 
         DO 300 ISCL = 1, 5 
            DO 200 IWOOD = 1, 2
               DO 100 IDI = 1, ITOTRR
                  STOUT(IDI, IWOOD, ISCL, ICYLS)  = 0.0
                  STUIN(IDI, IWOOD, ISCL, ICYLS)  = 0.0
                  DBHOUT(IDI, IWOOD, ISCL, ICYLS) = 0.0
                  DBHUIN(IDI, IWOOD, ISCL, ICYLS) = 0.0
                  RTOUT(IDI, IWOOD, ISCL, ICYLS)  = 0.0
                  RTUIN(IDI, IWOOD, ISCL, ICYLS)  = 0.0
  100          CONTINUE
  200       CONTINUE
  300    CONTINUE
  400 CONTINUE

      DO 600 NC = 1, 100
         DO 500 IDI = 1, ITOTRR
            ICENSP(IDI, NC) = 0
            JCENSP(IDI, NC) = 0
            RRATES(IDI, NC) = 0.0
  500    CONTINUE
  600 CONTINUE

      DO 650 I = 1, 3
         LSPFLG(I) = .FALSE.
         ISDATE(I) = 0
  650 CONTINUE 

      DO 700 IDI = 1, ITOTRR
         AREANU(IDI) = 0.0

         NCENTS(IDI) = 20

         RISTU(IDI)  = 0.0

         RRGEN(IDI,1)  = 0.0
         RRGEN(IDI,2)  = 20.0
         RRGEN(IDI,3)  = 0.0
         RRGEN(IDI,4)  = 0.0
         RRGEN(IDI,5)  = 1.0
         RRGEN(IDI,6)  = 1.0
         RRGEN(IDI,7)  = 0.0
         RRGEN(IDI,8)  = 0.0
         RRGEN(IDI,9)  = 100.0
         RRGEN(IDI,10) = 0.0

         RRINCS(IDI) = 0.1

         SPDBH(IDI)  = 14.0
         SPPROP(IDI) = 0.0
         SPYTK(IDI)  = 3.0
         SPTRAN(IDI) = 0.5
  700 CONTINUE
 
C.... The arrays ANUINF, IRDPLT, and MCRATE are not related (The
C.... dimension of 50 is not related between the 3 arrays.)  

      DO 900 I = 1, 50
         DO 800 IDI = 1, ITOTRR
            ANUINF(IDI, I) = 0.0
            IANPLT(IDI, I) = 0
            INFISD(IDI, I) = 0
            IRDPLT(IDI, I) = 0
            MCRATE(IDI, I) = 0.0
            PLPROP(IDI, I) = 0.0
  800    CONTINUE
  900 CONTINUE

      DO 1000 ITREC = 1, MAXTRE
         IDPLOT(ITREC) = 0
         IPRFL(ITREC)  = 0
 1000 CONTINUE

      DO 1400 J = 1, 2
         DO 1300 I = 1, 2
            DO 1200 IDI = 1, ITOTRR 
               DECFN(IDI, I, J) = TEMP1(IDI, I, J)
               YRSITF(IDI, I, J) = TEMP2(IDI, I, J)
 1200       CONTINUE
 1300    CONTINUE
 1400 CONTINUE

C.... The arrays RSITFN, CORINF, and EXPINF are not related (The
C.... dimension of 2 is not related between the 3 arrays.)  

      DO 1600 I = 1, 2
         DO 1500 IDI = 1, ITOTRR 
            CORINF(IDI, I) = 0.0
            EXPINF(IDI, I) = 0.0
            RSITFN(IDI, I) = TEMP3(IDI, I)
 1500    CONTINUE
 1600 CONTINUE

      DO 1700 IDI = 1, ITOTRR
         FRINGE(IDI) = 0.0
         IPCFLG(IDI) = 0
         PAREA(IDI)  = 25.0
         PISIZE(IDI) = 4.0
         PRKILL(IDI) = 0.5
         PRUN(IDI)   = 0.5
         RRJINC(IDI) = 0.0
         RRRSET(IDI) = 1.0
         LONECT(IDI) = 0
         LPAREA(IDI) = .FALSE.
         NSCEN(IDI)  = 0
         OOAREA(IDI) = 0.0
         RINUF(IDI)  = 0.0
         RINNF(IDI)  = 0.0
         RRRATE(IDI) = 0.0
         XMINKL(IDI) = 0.0
         XMINLF(IDI) = TEMP15(IDI)
 1700 CONTINUE

      DO 2000 ISCL = 1, 5
         DO 1900 IWOOD = 1, 2
            DO 1800 IDI = 1, ITOTRR
               CRNSTO(IDI, IWOOD, ISCL) = 0.0
               DBHD(IDI, IWOOD, ISCL)   = 0.0
               PROBD(IDI, IWOOD, ISCL)  = 0.0
               PROBIN(IDI, IWOOD, ISCL) = 0.0
               ROOTD(IDI, IWOOD, ISCL)  = 0.0
 1800       CONTINUE
 1900    CONTINUE
 2000 CONTINUE

      DO 2400 ICYLS = 1, 41 
         DO 2300 ISCL = 1, 5
            DO 2200 IWOOD = 1, 2
               DO 2100 IDI = 1, ITOTRR
                  DBHDA(IDI, IWOOD, ISCL, ICYLS)  = 0.0
                  DECRAT(IDI, IWOOD, ISCL, ICYLS) = 0.0
                  JRAGED(IDI, IWOOD, ISCL, ICYLS) = 0
                  PROBDA(IDI, IWOOD, ISCL, ICYLS) = 0.0
                  ROOTDA(IDI, IWOOD, ISCL, ICYLS) = 0.0
 2100          CONTINUE
 2200       CONTINUE
 2300    CONTINUE
 2400 CONTINUE

      DO 2500 I = 1, ITOTSP
         DBIFAC(I) = 1.0
         HTIFAC(I) = 1.0
         IDITYP(I) = ITEMP2(I)
         ISPS(I)   = ITEMP1(I)
         PROOT(I)  = 1.0
         ROWDOM(I) = 80.0

         ROWIBP(I,1) = 1.0
         ROWIBP(I,2) = 1.0

         RRPSWT(I) = 1.0
         RSLOP(I)  = TEMP13(I)
 2500 CONTINUE

      DO 2800 I = 1, 2
         DO 2700 IDI = 1, ITOTRR
            DO 2600 KSP = 1, ITOTSP
               HABFAC(KSP, IDI, I) = TEMP4(KSP, IDI, I)
 2600       CONTINUE
 2700    CONTINUE
 2800 CONTINUE

      DO 3000 IDI = 1, ITOTRR
         DO 2900 KSP = 1, ITOTSP
            PCOLO(KSP, IDI)  = TEMP5(KSP, IDI)
            PKILLS(KSP, IDI) = TEMP6(KSP, IDI)
            PNINF(KSP, IDI)  = TEMP7(KSP, IDI)
            SSSFAC(KSP, IDI) = 1.0
 2900    CONTINUE
 3000 CONTINUE

      DO 3100 I = 1, 4
         RROBMR(I) = TEMP8(I)
         RROBOL(I) = TEMP9(I)
         RROBRD(I) = TEMP10(I)
         RROBSC(I) = TEMP11(I)
         RROBTS(I) = TEMP12(I)
 3100 CONTINUE

      DO 3200 I = 1, 5
         STCUT(I) = TEMP14(I)
         XXINF(I) = TEMP16(I)
         YYINF(I) = TEMP17(I)
 3200 CONTINUE

      DO 3500 I=1,IRRTRE

         DO 3250 J = 1, 3
            OAKL(J,I) = 0.0
            PROAKL(J,I) = 0.0
 3250    CONTINUE

         DO 3300 J=1,4
            ROOTH(J,I) = -1.0
            XMTH(J,I)  = -1.0
 3300    CONTINUE

         DO 3400 J=1,40
            PROBI(I,J,1) = 0.0
            PROPI(I,J,1) = 0.0
            PROBI(I,J,2) = 0.0
            PROPI(I,J,2) = 0.0
 3400    CONTINUE

         FPROB(I)  = 0.0

         ISTEMI(I) = 0
         ISTEML(I) = 0

         PROBIT(I) = 0.0
         PROBIU(I) = 0.0
         PROBL(I)  = 0.0

         RDKILL(I) = 0.0
         ROOTL(I)  = 0.0
         RRIRAD(I) = 0.0
         RRKILL(I) = 0.0
         RROOTT(I) = 0.0

         XRRI(I)   = 0.0
         YRRI(I)   = 0.0

         WINDWI(I) = 0.0
         WINDWL(I) = 0.0
         WK22(I)   = 0.0
 3500 CONTINUE

      DO 3600 I = 1, 10
         IRGEN(I) = ITEMP3(I)
 3600 CONTINUE

      DO 3900 I = 1, 3
         DO 3800 NC = 1, 100
            DO 3700 IDI = 1, ITOTRR
               PCENTS(IDI, NC, I) = 0.0
               SHCENT(IDI, NC, I) = 0.0
 3700       CONTINUE
 3800    CONTINUE
 3900 CONTINUE

      DO 4000 I = 1, (ITOTRR + MAXSP)
         PRINF(I)  = 0.0
         PRPTOT(I) = 0.0
 4000 CONTINUE

      DO 4200 I = 1, 2
         DO 4100 NCCS = 1, 50
            RADINF(NCCS,I) = 0.0
 4100    CONTINUE
 4200 CONTINUE

      DO 4300 I = 1, MAXSP
         ROCRI(I)  = 0.0
         ROCRL(I)  = 0.0
         RROBNK(I) = 0.0
         STEMSI(I) = 0.0
         STEMSL(I) = 0.0
 4300 CONTINUE

      DO 4400 I = 1, 4
         ROOT4(I) = 0.0
         TXP12(I) = 0.0
 4400 CONTINUE 

      DO 4500 NCCS = 1, 50
         RRSDBH(NCCS) = 0.0
         RRSRAD(NCCS) = 0.0
         XRRS(NCCS)   = 0.0
         YRRS(NCCS)   = 0.0
 4500 CONTINUE

      DO 4700 J = 1, 4
         DO 4600 I = 1, MAXSP
            WINDSP(I,J) = 0.0
 4600    CONTINUE
 4700 CONTINUE 

      DO 4800 I = 1, 82
         PROP1(I) = 0.0
         TXP8(I)  = 0.0
 4800 CONTINUE

      DO 4900 I = 1, (3 * MAXSP)
         HOST(I)   = 0
         IIRATE(I) = 0
         IURATE(I) = 0
 4900 CONTINUE

      DO 5200 K = 1, 2
         DO 5100 J = 1, 4
            DO 5000 I = 1, IRRTRE
               DPROB(I,J,K) = 0.0
 5000       CONTINUE
 5100    CONTINUE
 5200 CONTINUE

      DO 5400 J = 1, 2
         DO 5300 I = 1, IRRTRE
            FFPROB(I,J) = 0.0
 5300    CONTINUE
 5400 CONTINUE
 

      AGECUR  = 0.0

      BB1GO   = .FALSE.
      BB2GO   = .FALSE.

      BOTRT   = 0.95
      BODBH   = 12.0

      CURAGE  = 0.0

      DEDAGE(1) = 5 
      DEDAGE(2) = 10
      DEDAGE(3) = 15 
      DEDAGE(4) = 15 
      DEDAGE(5) = 15 

      DGTOT  = 0.0

      DIMEN  = 2087.0

      DSEED  = 889347.0

      HTIMP  = 0.0
      HTTOT  = 0.0

      I1     = 0
      I2     = 0
      IDOBB  = 0
      IDRDOUT(1) = 0
      IDRDOUT(2) = 0
      IDRDOUT(3) = 0
      IFRRC  = 0
      IIEND  = 0
      ILEND  = 0
      INFLAG = 0
      IPUSH  = 0
      IRCOMP = 400
      IRDOUT = 0
      IRFLAG = 0
      IRHAB  = 1
      IRINIT = 10*MAXTRE
      IRIPTY = 1
      IROOT  = 0
      IRRSP  = 1
      IRSNYR = 20
      IRSPTY = 1
      IRSTYP = 0

C     Initialize ISTEP to 1 so that initial stumps (from tree list or
C     keyword) are kept separately from stumps created in the first
C     time step.

      ISTEP  = 1

      ISTFLG = 0
      IYEAR  = 0

      JRSIT  = 0

      LBBON     = .TRUE.
      LBORAX(1) = .FALSE. 
      LBORAX(2) = .FALSE. 
      LMTRIC    = .FALSE.
      LPLINF    = .FALSE.
      LRTYPE    = .FALSE.

      MAXRR  = 2
      MINRR  = 1

      NINSIM = 1
      NMONT  = 10
      NNCENT = 3
      NNDEC  = 5
      NNDMX  = 4
      NNINF  = 3
      NRSTEP = 5
      NSTUMP = 0
      NTREES = 0
      NUMBB  = 0
      NUMTRE = 0

      PINSET = 1.0
      PINT   = 10
      PRREM  = 0.0
      PTRE   = 0.0

      REINGO = .FALSE.
      RRTINV = .FALSE.
      RRMAN  = .FALSE.

      RRIDIM = 0.0
      RRIMEN = 0.0

      RRNEW(1) = 0.001
      RRNEW(2) = 0.001
      RRNEW(3) = 0.05
      RRNEW(4) = 0.05

      RRSARE = 0.0
      RRSARE = 0.0

      SDISLP = -0.0033
      SDNORM = 369.0

      SPINF(1) = 0.1 
      SPINF(2) = 0.1 
      SPINF(3) = 0.0 
      SPINF(4) = 0.0 

      TNJUMP(1) = 0.0
      TNJUMP(2) = 0.0
      TNJUMP(3) = 1.0
      TNJUMP(4) = 0.0

      TSTEMS = 0.0

      WINGO  = .FALSE.

      YINCPT = 1.0

      PPUSH  = 1.0

      REINEK(1) = 0.0
      REINEK(2) = 0.0
      REINEK(3) = 0.0
      REINEK(4) = -1.605

      ROWIND = 0.0
      ROWMIN = 0.0
      RRIARE = 0.1
      RRSFRN = 1.0

      SAREA  = 100.0
      S0     = 55329D0
      SS     = 55329.0

      WINDN  = 0.0

      XDBH(1) = 0.0
      XDBH(2) = 0.5
      XHT(1)  = 0.0
      XHT(2)  = 0.5

      YDBH(1) = 1.0
      YDBH(2) = 0.0
      YHT(1)  = 1.0
      YHT(2)  = 0.0

      YTKILL = 0.0

      RETURN
      END
