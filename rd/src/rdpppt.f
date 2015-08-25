      SUBROUTINE RDPPPT (WK3, IPNT, ILIMIT)
      IMPLICIT NONE
***********************************************************************
*  **RDPPPT      DATE OF LAST REVISION:  09/10/14
*----------------------------------------------------------------------
*  Purpose:
*     Put (store) the Root Disease model data for a given stand into
*     buffer.
*     This is part of the Parallel Processing Extension.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     ILIMIT: (I)  Size of buffer WK3.
*     IPNT:   (IO) Pointer to curent element in print buffer WK3.
*     WK3:    (IO) Work array used as a buffer.
*
*  Local variable definitions:
*     I:      Loop counter.
*     J:      Loop counter.
*     K:      Loop counter.
*     INTS:   Array of length MXI to hold integer values.
*     LOGICS: Array of length MXL to hold logical values.
*     REALS:  Array of length XMR to hold real values.
*
*  Common block variables and parameters:
*     See comments in common block files.
*
*  Revision History :
*     04-FEB-2000 - Lance R. David (FHTET)
*       This subroutine was written.
*     28-AUG-2001 - Lance R. David (FHTET)
*       Added new RDCOM variables LBBON, LRTYPE, IDOBB, IROOT
*     22-JUL-02 Lance R. David (FHTET)
*       Removed unused array PROBO from RDARRY common area processing.
*   09/10/14 Lance R. David (FMSC)
*     Updated, added implicit none and declared variables.
*
***********************************************************************

C.... Parameter statements.
      INTEGER  MXL, MXR, MXI
      PARAMETER (MXL=11,MXR=30,MXI=43)

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
      INCLUDE 'METRIC.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDADD.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDCRY.F77'

C
C     WRITE ALL INTEGER VARIABLES WITH IFWRIT, LOGICAL VARIABLES
C     WITH LFWRIT, AND REAL VARIABLES WITH BFWRIT.
C     LONG INTEGERS ARE EQIVALENCED TO REAL VARIABLES AND WRITTEN
C     WITH BFWRIT.
C     DOUBLE PRECISION VARIABLES (RANDOM NUMBER SEEDS) ARE EQUIVALENCED
C     TO REAL ARRAYS OF LENGTH 2 AND WRITTEN WITH BFWRIT.
C
C.... Variable declarations.

      LOGICAL LOGICS(MXL)
      INTEGER INTS(MXI), I, J, K, ILIMIT, IPNT
      REAL    REALS(MXR), WK3(*)
      REAL    SEED0(2), SEED1(2)
      EQUIVALENCE (SEED0,S0), (SEED1,S1)

C.... Store logical scalars in LOGICS array.
C
C---- from common METRIC ------------
      LOGICS ( 1) = LMTRIC
C---- from common RDADD ------------
      LOGICS ( 2) = BB1GO
      LOGICS ( 3) = BB2GO
      LOGICS ( 4) = LPLINF
      LOGICS ( 5) = REINGO
      LOGICS ( 6) = RRMAN
      LOGICS ( 7) = RRTINV
      LOGICS ( 8) = WINGO
C---- from common RDCOM ------------
      LOGICS ( 9) = LBBON
      LOGICS (10) = LRTYPE
      LOGICS (11) = LXNOTE

C     Write logical scalars to buffer.
C
      CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

C     Write logical arrays to buffer.
C
      CALL LFWRIT (WK3, IPNT, ILIMIT, LBORAX,      2, 2)
      CALL LFWRIT (WK3, IPNT, ILIMIT, LPAREA, ITOTRR, 2)
      CALL LFWRIT (WK3, IPNT, ILIMIT, LSPFLG,      3, 2)

C.... Load integer scalars into the INTS array.
C
C---- from common RDADD ------------
      INTS ( 1) = ISDOUT
      INTS ( 2) = MINRR
      INTS ( 3) = MAXRR
      INTS ( 4) = PINT
C---- from common RDCOM ------------
      INTS ( 5) = IBBOUT
      INTS ( 6) = IDOBB
      INTS ( 7) = IDRDOUT(1)
      INTS ( 8) = IDRDOUT(2)
      INTS ( 9) = IDRDOUT(3)
      INTS (10) = IFRRC
      INTS (11) = IIEND
      INTS (12) = ILEND
      INTS (13) = IMCOUT
      INTS (14) = INFLAG
      INTS (15) = IOUNIT
      INTS (16) = IPUSH
      INTS (17) = IRCOMP
      INTS (18) = IRDOUT
      INTS (19) = IRFLAG
      INTS (20) = IRHAB
      INTS (21) = IRINIT
      INTS (22) = IRIPTY
      INTS (23) = IROOT
      INTS (24) = IRRSP
      INTS (25) = IRSNYR
      INTS (26) = IRSPTY
      INTS (27) = IRSTYP
      INTS (28) = IRUNIT
      INTS (29) = ISTEP
      INTS (30) = ISTFLG
      INTS (31) = IYEAR
      INTS (32) = JRSIT
      INTS (33) = NINSIM
      INTS (34) = NMONT
      INTS (35) = NNCENT
      INTS (36) = NNDEC
      INTS (37) = NNDMX
      INTS (38) = NNINF
      INTS (39) = NRSTEP
      INTS (40) = NSTUMP
      INTS (41) = NTREES
      INTS (42) = NUMBB
      INTS (43) = NUMTRE

C.... Write integer scalars to buffer.
C
      CALL IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)

C.... Load real scalars into the REALS array.
C
C---- from common RDADD ------------
      REALS ( 1) = BODBH
      REALS ( 2) = BOTRT
      REALS ( 3) = SDISLP
      REALS ( 4) = SDNORM
      REALS ( 5) = SS
      REALS ( 6) = YINCPT
C---- from common RDCOM ------------
      REALS ( 7) = AGECUR
      REALS ( 8) = CURAGE
      REALS ( 9) = DGTOT
      REALS (10) = DIMEN
      REALS (11) = DSEED
      REALS (12) = HTIMP
      REALS (13) = HTTOT
      REALS (14) = PINSET
      REALS (15) = PPUSH
      REALS (16) = PRREM
      REALS (17) = PTRE
      REALS (18) = ROTSIT
      REALS (19) = ROWIND
      REALS (20) = ROWMIN
      REALS (21) = RRIARE
      REALS (22) = RRIDIM
      REALS (23) = RRIMEN
      REALS (24) = RRSARE
      REALS (25) = RRSFRN
      REALS (26) = SAREA
      REALS (27) = TSTEMS
      REALS (28) = WINDN
      REALS (29) = YTKILL
      REALS (30) = OLDPRP

C.... Write real scalars to buffer.
C
      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)

C.... Write double precision scalars (random number seeds) to buffer.
C
      CALL BFWRIT (WK3, IPNT, ILIMIT, SEED0, 2, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SEED1, 2, 2)


C.... Write integer arrays to buffer.
C
C     Note on processing arrays:
C     When handling a 2-dimensional array, consider a string of values a
C     complete column of the array and the length of the array as the 
C     number of rows. So, a call to IFWRIT or BFWRIT will process 1 column
C     of the specified number of rows (length) in a 2-dimensional array.
C
C     CALL IFWRIT (WK3,IPNT,ILIMIT, array, length, 2)
C---- from common RDADD ------------
      CALL IFWRIT (WK3, IPNT, ILIMIT, DEDAGE,             5, 2)
      DO 20 I = 1, 50
      CALL IFWRIT (WK3, IPNT, ILIMIT, IANPLT(1,I),   ITOTRR, 2) !(ITOTRR,50)
   20 CONTINUE
      CALL IFWRIT (WK3, IPNT, ILIMIT, ISDATE,             3, 2)
      DO 22 I = 1, 100
      CALL IFWRIT (WK3, IPNT, ILIMIT, ICENSP(1,I),   ITOTRR, 2) !(ITOTRR,100)
   22 CONTINUE
      CALL IFWRIT (WK3, IPNT, ILIMIT, IDPLOT,        MAXTRE, 2)
      DO 24 I = 1, 50
      CALL IFWRIT (WK3, IPNT, ILIMIT, INFISD(1,I),   ITOTRR, 2) !(ITOTRR,50)
   24 CONTINUE
      CALL IFWRIT (WK3, IPNT, ILIMIT, IPRFL,         MAXTRE, 2)
      DO 26 I = 1, 50
      CALL IFWRIT (WK3, IPNT, ILIMIT, IRDPLT(1,I),   ITOTRR, 2) !(ITOTRR,50)
   26 CONTINUE
      DO 28 I = 1, 100
      CALL IFWRIT (WK3, IPNT, ILIMIT, JCENSP(1,I),   ITOTRR, 2) !(ITOTRR,100)
   28 CONTINUE
      CALL IFWRIT (WK3, IPNT, ILIMIT, LONECT,        ITOTRR, 2)
C---- from common RDARRY ------------
      CALL IFWRIT (WK3, IPNT, ILIMIT, ISTEML,        IRRTRE, 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, ISTEMI,        IRRTRE, 2)
C---- from common RDCOM ------------
      CALL IFWRIT (WK3, IPNT, ILIMIT, HOST,         3*MAXSP, 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, IDITYP,        ITOTSP, 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, IPCFLG,        ITOTRR, 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, IRGEN,             10, 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, IRTSPC,         MAXSP, 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, ISPS,          ITOTSP, 2)
      DO 32 I = 1, 2
      DO 31 J = 1, 5
      DO 30 K = 1, 41
      CALL IFWRIT (WK3, IPNT, ILIMIT, JRAGED(1,I,J,K),ITOTRR,2) !(ITOTRR,2,5,41)
   30 CONTINUE
   31 CONTINUE
   32 CONTINUE
      DO 33 I = 1, 50
      CALL IFWRIT (WK3, IPNT, ILIMIT, MCTREE(1,I),   ITOTRR, 2) !(ITOTRR,50)
   33 CONTINUE

      CALL IFWRIT (WK3, IPNT, ILIMIT, NCENTS,        ITOTRR, 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, NSCEN,         ITOTRR, 2)

C---- from common RDCRY ------------
C     no integer arrays


C.... Write real arrays to buffer.
C
C---- from common RDADD ------------
      DO 34 I = 1, 50
      CALL BFWRIT (WK3, IPNT, ILIMIT, ANUINF(1,I),    ITOTRR, 2) !(ITOTRR,50)
   34 CONTINUE
      DO 35 I = 1, IRRTRE
      CALL BFWRIT (WK3, IPNT, ILIMIT, BBKILL(1,I),         4, 2) !(4,IRRTRE)
   35 CONTINUE
      DO 38 I = 1, 2
      DO 37 J = 1, 5
      DO 36 K = 1, 3
      CALL BFWRIT (WK3, IPNT, ILIMIT, DBHOUT(1,I,J,K), ITOTRR,2) !(ITOTRR,2,5,3)
   36 CONTINUE
   37 CONTINUE
   38 CONTINUE
      DO 41 I = 1, 2
      DO 40 J = 1, 5
      DO 39 K = 1, 3
      CALL BFWRIT (WK3, IPNT, ILIMIT, DBHUIN(1,I,J,K), ITOTRR,2) !(ITOTRR,2,5,3)
   39 CONTINUE
   40 CONTINUE
   41 CONTINUE
      DO 43 I = 1, 2
      DO 42 J = 1, 2
      CALL BFWRIT (WK3, IPNT, ILIMIT, DECFN(1,I,J),   ITOTRR, 2) !(ITOTRR,2,2)
   42 CONTINUE
   43 CONTINUE
      DO 45 I = 1, 4
      DO 44 J = 1, 2
      CALL BFWRIT (WK3, IPNT, ILIMIT, DPROB(1,I,J),  IRRTRE, 2) !(IRRTRE,4,2)
   44 CONTINUE
   45 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, FFPROB(1,1),   IRRTRE, 2) !(IRRTRE,2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FFPROB(1,2),   IRRTRE, 2) !----------
      DO 46 I = 1, IRRTRE
      CALL BFWRIT (WK3, IPNT, ILIMIT, OAKL(1,I),          3, 2) !(3,IRRTRE)
   46 CONTINUE
      DO 47 I = 1, 50
      CALL BFWRIT (WK3, IPNT, ILIMIT, PLPROP(1,I),   ITOTRR, 2) !(ITOTRR,50)
   47 CONTINUE
      DO 48 I = 1, IRRTRE
      CALL BFWRIT (WK3, IPNT, ILIMIT, PROAKL(1,I),        3, 2) !(3,IRRTRE)
   48 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, RINNF,         ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RINUF,         ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RISTU,         ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RRNEW,         ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RSITFN(1,1),   ITOTRR, 2) !(ITOTRR,2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RSITFN(1,2),   ITOTRR, 2) !----------
      DO 51 I = 1, 2
      DO 50 J = 1, 5
      DO 49 K = 1, 3
      CALL BFWRIT (WK3, IPNT, ILIMIT, RTOUT(1,I,J,K), ITOTRR,2) !(ITOTRR,2,5,3)
   49 CONTINUE
   50 CONTINUE
   51 CONTINUE
      DO 54 I = 1, 2
      DO 53 J = 1, 5
      DO 52 K = 1, 3
      CALL BFWRIT (WK3, IPNT, ILIMIT, RTUIN(1,I,J,K), ITOTRR,2) !(ITITRR,2,5,3)
   52 CONTINUE
   53 CONTINUE
   54 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, SPDBH,         ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SPINF,         ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SPPROP,        ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SPTRAN,        ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SPYTK,         ITOTRR, 2)
      DO 57 I = 1, 2
      DO 56 J = 1, 5
      DO 55 K = 1, 3
      CALL BFWRIT (WK3, IPNT, ILIMIT, STOUT(1,I,J,K),ITOTRR, 2) !(ITOTRR,2,5,3)
   55 CONTINUE
   56 CONTINUE
   57 CONTINUE
      DO 60 I = 1, 2
      DO 59 J = 1, 5
      DO 58 K = 1, 3
      CALL BFWRIT (WK3, IPNT, ILIMIT, STUIN(1,I,J,K),ITOTRR, 2) !(ITOTRR,2,5,3)
   58 CONTINUE
   59 CONTINUE
   60 CONTINUE
      DO 62 I = 1, 2
      DO 61 J = 1, 2
      CALL BFWRIT (WK3, IPNT, ILIMIT, YRSITF(1,I,J), ITOTRR, 2) !(ITOTRR,2,2)
   61 CONTINUE
   62 CONTINUE

C---- from common RDARRY ------------
      CALL BFWRIT (WK3, IPNT, ILIMIT, FPROB,         IRRTRE, 2)
      DO 64 I = 1, 41
      DO 63 J = 1, 2
      CALL BFWRIT (WK3, IPNT, ILIMIT, PROBI(1,I,J),  IRRTRE, 2) !(IRRTRE,41,2)
   63 CONTINUE
   64 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, PROBIT,        IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PROBIU,        IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PROBL,         IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PROP1,             82, 2)
      DO 66 I = 1, 41
      DO 65 J = 1, 2
      CALL BFWRIT (WK3, IPNT, ILIMIT, PROPI(1,I,J),  IRRTRE, 2) !(IRRTRE,41,2)
   65 CONTINUE
   66 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, ROOTL,         IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RRKILL,        IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, TXP8,              82, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, WINDWL,        IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, WINDWI,        IRRTRE, 2)
C---- from common RDCOM ------------
      CALL BFWRIT (WK3, IPNT, ILIMIT, AREANU,        ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CORINF(1,1),   ITOTRR, 2) !(ITOTRR,2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CORINF(1,2),   ITOTRR, 2) !----------
      DO 68 I = 1, 2
      DO 67 J = 1, 5
      CALL BFWRIT (WK3, IPNT, ILIMIT, DBHD(1,I,J),   ITOTRR, 2) !(ITOTRR,2,5)
   67 CONTINUE
   68 CONTINUE
      DO 71 I = 1, 2
      DO 70 J = 1, 5
      DO 69 K = 1, 41
      CALL BFWRIT (WK3, IPNT, ILIMIT, DBHDA(1,I,J,K),ITOTRR, 2) !(ITOTRR,2,5,41)
   69 CONTINUE
   70 CONTINUE
   71 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, DBIFAC,        ITOTSP, 2)
      DO 74 I = 1, 2
      DO 73 J = 1, 5
      DO 72 K = 1, 41
      CALL BFWRIT (WK3, IPNT, ILIMIT, DECRAT(1,I,J,K),ITOTRR,2) !(ITOTRR,2,5,41)
   72 CONTINUE
   73 CONTINUE
   74 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, DICLAS,           4, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, DSFAC,            2, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, EXPINF(1,1), ITOTRR, 2) !(ITOTRR,2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, EXPINF(1,2), ITOTRR, 2) !----------
      CALL BFWRIT (WK3, IPNT, ILIMIT, FRINGE,      ITOTRR, 2)

      DO 76 I = 1, ITOTRR
      DO 75 J = 1, 2
      CALL BFWRIT (WK3, IPNT, ILIMIT, HABFAC(1,I,J),ITOTSP,2) !(ITOTSP,ITOTRR,2)
   75 CONTINUE
   76 CONTINUE

      CALL BFWRIT (WK3, IPNT, ILIMIT, HTIFAC,      ITOTSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, IURATE,     3*MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, IIRATE,     3*MAXSP, 2)
      DO 77 I = 1, 50
      CALL BFWRIT (WK3, IPNT, ILIMIT, MCRATE(1,I), ITOTRR, 2) !(ITOTRR,50)
   77 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, MINDBH,     3*MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, MININF,     3*MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, ORATE,      3*MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, OFRATE,     3*MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, OOAREA,      ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PAREA,       ITOTRR, 2)
      DO 79 I = 1, 100
      DO 78 J = 1, 3
      CALL BFWRIT (WK3, IPNT, ILIMIT, PCENTS(1,I,J),ITOTRR,2) !(ITOTRR,100,3)
   78 CONTINUE
   79 CONTINUE
      DO 80 I = 1, ITOTRR
      CALL BFWRIT (WK3, IPNT, ILIMIT, PCOLO(1,I),  ITOTSP, 2) !(ITOTSP,ITOTRR)
   80 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, PISIZE,      ITOTRR, 2)
      DO 81 I = 1, ITOTRR
      CALL BFWRIT (WK3, IPNT, ILIMIT, PKILLS(1,I), ITOTSP, 2) !(ITOTSP,ITOTRR)
   81 CONTINUE
      DO 82 I = 1, ITOTRR
      CALL BFWRIT (WK3, IPNT, ILIMIT, PNINF(1,I),  ITOTSP, 2) !(ITOTSP,ITOTRR)
   82 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, PRANKL,      IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PRINF, ITOTRR+MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PRKILL,      ITOTRR, 2)
      DO 84 I = 1, 2
      DO 83 J = 1, 5
      CALL BFWRIT (WK3, IPNT, ILIMIT, PROBD(1,I,J), ITOTRR,2) !(ITOTRR,2,5)
   83 CONTINUE
   84 CONTINUE
      DO 87 I = 1, 2
      DO 86 J = 1, 5
      DO 85 K = 1, 41
      CALL BFWRIT (WK3, IPNT, ILIMIT, PROBDA(1,I,J,K),ITOTRR,2) !(ITOTRR,2,5,41)
   85 CONTINUE
   86 CONTINUE
   87 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, PROOT,        ITOTSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PRPTOT, ITOTRR+MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PRUN,         ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RADINF(1,1),      50, 2) !(50,2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RADINF(1,2),      50, 2) !------
      CALL BFWRIT (WK3, IPNT, ILIMIT, RDKILL,       IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, REINEK,            4, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, ROCRI,         MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, ROCRL,         MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, ROOT4,             4, 2)
      DO 89 I = 1, 2
      DO 88 J = 1, 5
      CALL BFWRIT (WK3, IPNT, ILIMIT, ROOTD(1,I,J), ITOTRR, 2) !(ITOTRR,2,5)
   88 CONTINUE
   89 CONTINUE
      DO 92 I = 1, 2
      DO 91 J = 1, 5
      DO 90 K = 1, 41
      CALL BFWRIT (WK3, IPNT, ILIMIT, ROOTDA(1,I,J,K),ITOTRR,2) !(ITOTRR,2,5,41)
   90 CONTINUE
   91 CONTINUE
   92 CONTINUE
      DO 93 I = 1, IRRTRE
      CALL BFWRIT (WK3, IPNT, ILIMIT, ROOTH(1,I),       4, 2) !(4,IRRTRE)
   93 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, ROWDOM,      ITOTSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, ROWIBP(1,1), ITOTSP, 2) !(ITOTSP,2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, ROWIBP(1,2), ITOTSP, 2) !----------
      DO 94 I = 1, 100
      CALL BFWRIT (WK3, IPNT, ILIMIT, RRATES(1,I), ITOTRR, 2) !(ITOTRR,100)
   94 CONTINUE
      DO 95 I = 1, 10
      CALL BFWRIT (WK3, IPNT, ILIMIT, RRGEN(1,I),  ITOTRR, 2) !(ITOTRR,10)
   95 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, RRINCS,      ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RRIRAD,      IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RRJINC,      ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RROBMR,      ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RROBNK,       MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RROBOL,           4, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RROBRD,           4, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RROBSC,           4, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RROBTS,           4, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RROOTT,      IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RRPSWT,      ITOTSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RRRATE,      ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RRRSET,      ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RRSDBH,          50, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RRSRAD,          50, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, RSLOP,       ITOTSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SDRATE,      IRRTRE, 2)
      DO 97 I = 1, 100
      DO 96 J = 1, 3
      CALL BFWRIT (WK3, IPNT, ILIMIT, SHCENT(1,I,J),ITOTRR,2) !(ITOTRR,100,3)
   96 CONTINUE
   97 CONTINUE
      DO 98 I = 1, 50
      CALL BFWRIT (WK3, IPNT, ILIMIT, SPRQMD(1,I), ITOTRR, 2) !(ITOTRR,50)
   98 CONTINUE
      DO 99 I = 1, ITOTRR
      CALL BFWRIT (WK3, IPNT, ILIMIT, SSSFAC(1,I), ITOTSP, 2) !(ITOTSP,ITOTRR)
   99 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, STCUT,            5, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, STEMSI,       MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, STEMSL,       MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, TNJUMP,      ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, TXP12,            4, 2)
      DO 100 I = 1, 4
      CALL BFWRIT (WK3, IPNT, ILIMIT, WINDSP(1,I),  MAXSP, 2) !(MAXSP,4)
  100 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, WK22,        IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, XDBH,             2, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, XHT,              2, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, XMINKL,      ITOTRR, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, XMINLF,      ITOTRR, 2)
      DO 101 I = 1, IRRTRE
      CALL BFWRIT (WK3, IPNT, ILIMIT, XMTH(1,I),        4, 2) !(4,IRRTRE)
  101 CONTINUE
      CALL BFWRIT (WK3, IPNT, ILIMIT, XRRI,        IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, XRRS,            50, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, XXINF,            5, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, YDBH,             2, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, YHT,              2, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, YRRI,        IRRTRE, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, YRRS,            50, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, YYINF,            5, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CDF,           1001, 2)
C---- from common RDCRY ------------
      DO 103 I = 1, 2
      DO 102 J = 1, 5
      CALL BFWRIT (WK3, IPNT, ILIMIT, PROBIN(1,I,J), ITOTRR, 2)!(ITOTRR,2,5)
  102 CONTINUE
  103 CONTINUE

      DO 105 I = 1, 2
      DO 104 J = 1, 5
      CALL BFWRIT (WK3, IPNT, ILIMIT, CRNSTO(1,I,J), ITOTRR, 2)!(ITOTRR,2,5)
  104 CONTINUE
  105 CONTINUE

      RETURN
      END
