      SUBROUTINE RDPPGT (WK3, IPNT, ILIMIT)
***********************************************************************
*  **RDPPGT      DATE OF LAST REVISION:  07/22/02
*----------------------------------------------------------------------
*  Purpose:
*     Get (read) the Root Disease model data for a given stand from
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
*
***********************************************************************

C.... Parameter statements.

      PARAMETER (MXL=11,MXR=29,MXI=41)

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
      INCLUDE 'METRIC.F77'
      INCLUDE 'PPEPRM.F77'

C.... Common include files.

      INCLUDE 'PPCNTL.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDADD.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDCRY.F77'

C
C     READ ALL INTEGER VARIABLES WITH IFREAD, LOGICAL VARIABLES
C     WITH LFREAD, AND REAL VARIABLES WITH BFREAD.
C     LONG INTEGERS ARE EQIVALENCED TO REAL VARIABLES AND READ
C     WITH BFREAD.
C     DOUBLE PRECISION VARIABLES (RANDOM NUMBER SEEDS) ARE EQUIVALENCED
C     TO REAL ARRAYS OF LENGTH 2 AND WRITTEN WITH BFREAD.
C
C.... Variable declarations.

      LOGICAL LOGICS(MXL)
      INTEGER INTS(MXI), I, J, K, ILIMIT, IPNT
      REAL    REALS(MXR), WK3(*)
      REAL    SEED0(2), SEED1(2)
      EQUIVALENCE (SEED0,S0), (SEED1,S1)

      IF (PDEBUG)
     >   WRITE (JOPPRT,'(/'' IN RDPPGT: ISTND,ICYC='',I7,I4)')
     >         ISTND,ICYC

C     Read logical scalars from buffer.
C
      CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN RDPPGT: LOGICS=',
     >            LOGICS

C.... Load logical scalars from LOGICS array.
C
C---- from common METRIC ------------
      LMTRIC = LOGICS ( 1)
C---- from common RDADD ------------
      RRTINV = LOGICS ( 2)
      RRMAN  = LOGICS ( 3)
      BB2GO  = LOGICS ( 4)
      WINGO  = LOGICS ( 5)
      BB1GO  = LOGICS ( 6)
      REINGO = LOGICS ( 7)
      LPLINF = LOGICS ( 8)
C---- from common RDCOM ------------
      LXNOTE = LOGICS ( 9)
      LRTYPE = LOGICS (10)
      LBBON  = LOGICS (11)

C     Read logical arrays from buffer.
C
      CALL LFREAD (WK3, IPNT, ILIMIT, LBORAX,      2, 2)
      CALL LFREAD (WK3, IPNT, ILIMIT, LPAREA, ITOTRR, 2)
      CALL LFREAD (WK3, IPNT, ILIMIT, LSPFLG,      3, 2)

C.... Read integer scalars from buffer.
C
      CALL IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN RDPPGT: INTS=',
     >            INTS

C.... Load integer scalars from the INTS array.
C
C---- from common RDADD ------------
      ISDOUT = INTS ( 1)
      MINRR  = INTS ( 2)
      MAXRR  = INTS ( 3)
      PINT   = INTS ( 4)
C---- from common RDCOM ------------
      IBBOUT = INTS ( 5)
      IFRRC  = INTS ( 6)
      IIEND  = INTS ( 7)
      ILEND  = INTS ( 8)
      IMCOUT = INTS ( 9)
      INFLAG = INTS (10)
      IOUNIT = INTS (11)
      IPUSH  = INTS (12)
      IRCOMP = INTS (13)
      IRDOUT = INTS (14)
      IRFLAG = INTS (15)
      IRHAB  = INTS (16)
      IRINIT = INTS (17)
      IRIPTY = INTS (18)
      IROOT  = INTS (19)
      IRRSP  = INTS (20)
      IRSNYR = INTS (21)
      IRSPTY = INTS (22)
      IRSTYP = INTS (23)
      IRUNIT = INTS (24)
      ISTEP  = INTS (25)
      ISTFLG = INTS (26)
      IYEAR  = INTS (27)
      JRSIT  = INTS (28)
      NINSIM = INTS (29)
      NMONT  = INTS (30)
      NNCENT = INTS (31)
      NNDEC  = INTS (32)
      NNDMX  = INTS (33)
      NNINF  = INTS (34)
      NRSTEP = INTS (35)
      NSTUMP = INTS (36)
      NTREES = INTS (37)
      NUMBB  = INTS (38)
      NUMTRE = INTS (39)
      IDOBB  = INTS (40)
      IROOT  = INTS (41)

C.... Read real scalars from buffer.
C
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN RDPPGT: REALS=',
     >            REALS

C.... Load real scalars from the REALS array.
C
C---- from common RDADD ------------
      BODBH  = REALS ( 1)
      BOTRT  = REALS ( 2)
      SDISLP = REALS ( 3)
      SDNORM = REALS ( 4)
      YINCPT = REALS ( 5)
C---- from common RDANN ------------
      SS     = REALS ( 6)
C---- from common RDCOM ------------
      AGECUR = REALS ( 7)
      CURAGE = REALS ( 8)
      DGTOT  = REALS ( 9)
      DIMEN  = REALS (10)
      DSEED  = REALS (11)
      HTIMP  = REALS (12)
      HTTOT  = REALS (13)
      PINSET = REALS (14)
      PPUSH  = REALS (15)
      PRREM  = REALS (16)
      PTRE   = REALS (17)
      ROTSIT = REALS (18)
      ROWIND = REALS (19)
      ROWMIN = REALS (20)
      RRIARE = REALS (21)
      RRIDIM = REALS (22)
      RRIMEN = REALS (23)
      RRSARE = REALS (24)
      RRSFRN = REALS (25)
      SAREA  = REALS (26)
      TSTEMS = REALS (27)
      WINDN  = REALS (28)
      YTKILL = REALS (29)

C.... Read double precision scalars (random number seeds) from buffer.
C
      CALL BFREAD (WK3, IPNT, ILIMIT, SEED0, 2, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SEED1, 2, 2)


C.... Read integer arrays from buffer.
C
C     Note on processing arrays:
C     When handling a 2-dimensional array, consider a string of values a
C     complete column of the array and the length of the array as the 
C     number of rows. So, a call to IFREAD or BFREAD will process 1 column
C     of the specified number of rows (length) in a 2-dimensional array.
C
C     CALL IFREAD (WK3,IPNT,ILIMIT, array, length, 2)
C---- from common RDADD ------------
      CALL IFREAD (WK3, IPNT, ILIMIT, DEDAGE,             5, 2)
      DO 20 I = 1, 50
      CALL IFREAD (WK3, IPNT, ILIMIT, IANPLT(1,I),   ITOTRR, 2) !(ITOTRR,50)
   20 CONTINUE
      CALL IFREAD (WK3, IPNT, ILIMIT, ISDATE,             3, 2)
      DO 22 I = 1, 100
      CALL IFREAD (WK3, IPNT, ILIMIT, ICENSP(1,I),   ITOTRR, 2) !(ITOTRR,100)
   22 CONTINUE
      CALL IFREAD (WK3, IPNT, ILIMIT, IDPLOT,        MAXTRE, 2)
      DO 24 I = 1, 50
      CALL IFREAD (WK3, IPNT, ILIMIT, INFISD(1,I),   ITOTRR, 2) !(ITOTRR,50)
   24 CONTINUE
      CALL IFREAD (WK3, IPNT, ILIMIT, IPRFL,         MAXTRE, 2)
      DO 26 I = 1, 50
      CALL IFREAD (WK3, IPNT, ILIMIT, IRDPLT(1,I),   ITOTRR, 2) !(ITOTRR,50)
   26 CONTINUE
      DO 28 I = 1, 100
      CALL IFREAD (WK3, IPNT, ILIMIT, JCENSP(1,I),   ITOTRR, 2) !(ITOTRR,100)
   28 CONTINUE
      CALL IFREAD (WK3, IPNT, ILIMIT, LONECT,        ITOTRR, 2)
C---- from common RDARRY ------------
      CALL IFREAD (WK3, IPNT, ILIMIT, ISTEML,        IRRTRE, 2)
      CALL IFREAD (WK3, IPNT, ILIMIT, ISTEMI,        IRRTRE, 2)
C---- from common RDCOM ------------
      CALL IFREAD (WK3, IPNT, ILIMIT, HOST,         3*MAXSP, 2)
      CALL IFREAD (WK3, IPNT, ILIMIT, IDITYP,        ITOTSP, 2)
      CALL IFREAD (WK3, IPNT, ILIMIT, IPCFLG,        ITOTRR, 2)
      CALL IFREAD (WK3, IPNT, ILIMIT, IRGEN,             10, 2)
      CALL IFREAD (WK3, IPNT, ILIMIT, IRTSPC,         MAXSP, 2)
      CALL IFREAD (WK3, IPNT, ILIMIT, ISPS,          ITOTSP, 2)
      DO 32 I = 1, 2
      DO 31 J = 1, 5
      DO 30 K = 1, 41
      CALL IFREAD (WK3, IPNT, ILIMIT, JRAGED(1,I,J,K),ITOTRR,2) !(ITOTRR,2,5,41)
   30 CONTINUE
   31 CONTINUE
   32 CONTINUE
      DO 33 I = 1, 50
      CALL IFREAD (WK3, IPNT, ILIMIT, MCTREE(1,I),   ITOTRR, 2) !(ITOTRR,50)
   33 CONTINUE
      CALL IFREAD (WK3, IPNT, ILIMIT, NCENTS,        ITOTRR, 2)
      IF (PDEBUG) WRITE (JOPPRT,*) 'IN RDPPGT: NCENTS=',
     >            NCENTS
      CALL IFREAD (WK3, IPNT, ILIMIT, NSCEN,         ITOTRR, 2)
      IF (PDEBUG) WRITE (JOPPRT,*) 'IN RDPPGT: NSCEN=',
     >            NSCEN
C---- from common RDCRY ------------
C     no integer arrays


C.... Read real arrays from buffer.
C
C---- from common RDADD ------------
      DO 34 I = 1, 50
      CALL BFREAD (WK3, IPNT, ILIMIT, ANUINF(1,I),    ITOTRR, 2) !(ITOTRR,50)
   34 CONTINUE
      DO 35 I = 1, IRRTRE
      CALL BFREAD (WK3, IPNT, ILIMIT, BBKILL(1,I),         4, 2) !(4,IRRTRE)
   35 CONTINUE
      DO 38 I = 1, 2
      DO 37 J = 1, 5
      DO 36 K = 1, 3
      CALL BFREAD (WK3, IPNT, ILIMIT, DBHOUT(1,I,J,K), ITOTRR,2) !(ITOTRR,2,5,3)
   36 CONTINUE
   37 CONTINUE
   38 CONTINUE
      DO 41 I = 1, 2
      DO 40 J = 1, 5
      DO 39 K = 1, 3
      CALL BFREAD (WK3, IPNT, ILIMIT, DBHUIN(1,I,J,K), ITOTRR,2) !(ITOTRR,2,5,3)
   39 CONTINUE
   40 CONTINUE
   41 CONTINUE
      DO 43 I = 1, 2
      DO 42 J = 1, 2
      CALL BFREAD (WK3, IPNT, ILIMIT, DECFN(1,I,J),   ITOTRR, 2) !(ITOTRR,2,2)
   42 CONTINUE
   43 CONTINUE
      DO 45 I = 1, 4
      DO 44 J = 1, 2
      CALL BFREAD (WK3, IPNT, ILIMIT, DPROB(1,I,J),  IRRTRE, 2) !(IRRTRE,4,2)
   44 CONTINUE
   45 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, FFPROB(1,1),   IRRTRE, 2) !(IRRTRE,2)
      CALL BFREAD (WK3, IPNT, ILIMIT, FFPROB(1,2),   IRRTRE, 2) !----------
      DO 46 I = 1, IRRTRE
      CALL BFREAD (WK3, IPNT, ILIMIT, OAKL(1,I),          3, 2) !(3,IRRTRE)
   46 CONTINUE
      DO 47 I = 1, 50
      CALL BFREAD (WK3, IPNT, ILIMIT, PLPROP(1,I),   ITOTRR, 2) !(ITOTRR,50)
   47 CONTINUE
      DO 48 I = 1, IRRTRE
      CALL BFREAD (WK3, IPNT, ILIMIT, PROAKL(1,I),        3, 2) !(3,IRRTRE)
   48 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, RINNF,         ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RINUF,         ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RISTU,         ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RRNEW,         ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RSITFN(1,1),   ITOTRR, 2) !(ITOTRR,2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RSITFN(1,2),   ITOTRR, 2) !----------
      DO 51 I = 1, 2
      DO 50 J = 1, 5
      DO 49 K = 1, 3
      CALL BFREAD (WK3, IPNT, ILIMIT, RTOUT(1,I,J,K), ITOTRR,2) !(ITOTRR,2,5,3)
   49 CONTINUE
   50 CONTINUE
   51 CONTINUE
      DO 54 I = 1, 2
      DO 53 J = 1, 5
      DO 52 K = 1, 3
      CALL BFREAD (WK3, IPNT, ILIMIT, RTUIN(1,I,J,K), ITOTRR,2) !(ITITRR,2,5,3)
   52 CONTINUE
   53 CONTINUE
   54 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, SPDBH,         ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SPINF,         ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SPPROP,        ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SPTRAN,        ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SPYTK,         ITOTRR, 2)
      DO 57 I = 1, 2
      DO 56 J = 1, 5
      DO 55 K = 1, 3
      CALL BFREAD (WK3, IPNT, ILIMIT, STOUT(1,I,J,K),ITOTRR, 2) !(ITOTRR,2,5,3)
   55 CONTINUE
   56 CONTINUE
   57 CONTINUE
      DO 60 I = 1, 2
      DO 59 J = 1, 5
      DO 58 K = 1, 3
      CALL BFREAD (WK3, IPNT, ILIMIT, STUIN(1,I,J,K),ITOTRR, 2) !(ITOTRR,2,5,3)
   58 CONTINUE
   59 CONTINUE
   60 CONTINUE
      DO 62 I = 1, 2
      DO 61 J = 1, 2
      CALL BFREAD (WK3, IPNT, ILIMIT, YRSITF(1,I,J), ITOTRR, 2) !(ITOTRR,2,2)
   61 CONTINUE
   62 CONTINUE
C---- from common RDARRY ------------
      CALL BFREAD (WK3, IPNT, ILIMIT, FPROB,         IRRTRE, 2)
      DO 64 I = 1, 41
      DO 63 J = 1, 2
      CALL BFREAD (WK3, IPNT, ILIMIT, PROBI(1,I,J),  IRRTRE, 2) !(IRRTRE,41,2)
   63 CONTINUE
   64 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, PROBIT,        IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PROBIU,        IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PROBL,         IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PROP1,             82, 2)
      DO 66 I = 1, 41
      DO 65 J = 1, 2
      CALL BFREAD (WK3, IPNT, ILIMIT, PROPI(1,I,J),  IRRTRE, 2) !(IRRTRE,41,2)
   65 CONTINUE
   66 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, ROOTL,         IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RRKILL,        IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TXP8,              82, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, WINDWL,        IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, WINDWI,        IRRTRE, 2)
C---- from common RDCOM ------------
      CALL BFREAD (WK3, IPNT, ILIMIT, AREANU,        ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, CORINF(1,1),   ITOTRR, 2) !(ITOTRR,2)
      CALL BFREAD (WK3, IPNT, ILIMIT, CORINF(1,2),   ITOTRR, 2) !----------
      DO 68 I = 1, 2
      DO 67 J = 1, 5
      CALL BFREAD (WK3, IPNT, ILIMIT, DBHD(1,I,J),   ITOTRR, 2) !(ITOTRR,2,5)
   67 CONTINUE
   68 CONTINUE
      DO 71 I = 1, 2
      DO 70 J = 1, 5
      DO 69 K = 1, 41
      CALL BFREAD (WK3, IPNT, ILIMIT, DBHDA(1,I,J,K),ITOTRR, 2) !(ITOTRR,2,5,41)
   69 CONTINUE
   70 CONTINUE
   71 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, DBIFAC,        ITOTSP, 2)
      DO 74 I = 1, 2
      DO 73 J = 1, 5
      DO 72 K = 1, 41
      CALL BFREAD (WK3, IPNT, ILIMIT, DECRAT(1,I,J,K),ITOTRR,2) !(ITOTRR,2,5,41)
   72 CONTINUE
   73 CONTINUE
   74 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, DICLAS,           4, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, DSFAC,            2, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, EXPINF(1,1), ITOTRR, 2) !(ITOTRR,2)
      CALL BFREAD (WK3, IPNT, ILIMIT, EXPINF(1,2), ITOTRR, 2) !----------
      CALL BFREAD (WK3, IPNT, ILIMIT, FRINGE,      ITOTRR, 2)
      DO 76 I = 1, 2
      DO 75 J = 1, ITOTRR
      CALL BFREAD (WK3, IPNT, ILIMIT, HABFAC(1,I,J),ITOTSP,2) !(ITOTSP,ITOTRR,2)
   75 CONTINUE
   76 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, HTIFAC,      ITOTSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, IURATE,     3*MAXSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, IIRATE,     3*MAXSP, 2)
      DO 77 I = 1, 50
      CALL BFREAD (WK3, IPNT, ILIMIT, MCRATE(1,I), ITOTRR, 2) !(ITOTRR,50)
   77 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, MINDBH,     3*MAXSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, MININF,     3*MAXSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, ORATE,      3*MAXSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, OFRATE,     3*MAXSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, OOAREA,      ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PAREA,       ITOTRR, 2)
      DO 79 I = 1, 100
      DO 78 J = 1, 3
      CALL BFREAD (WK3, IPNT, ILIMIT, PCENTS(1,I,J),ITOTRR,2) !(ITOTRR,100,3)
   78 CONTINUE
   79 CONTINUE
      DO 80 I = 1, ITOTRR
      CALL BFREAD (WK3, IPNT, ILIMIT, PCOLO(1,I),  ITOTSP, 2) !(ITOTSP,ITOTRR)
   80 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, PISIZE,      ITOTRR, 2)
      DO 81 I = 1, ITOTRR
      CALL BFREAD (WK3, IPNT, ILIMIT, PKILLS(1,I), ITOTSP, 2) !(ITOTSP,ITOTRR)
   81 CONTINUE
      DO 82 I = 1, ITOTRR
      CALL BFREAD (WK3, IPNT, ILIMIT, PNINF(1,I),  ITOTSP, 2) !(ITOTSP,ITOTRR)
   82 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, PRANKL,      IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PRINF, ITOTRR+MAXSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PRKILL,      ITOTRR, 2)
      DO 84 I = 1, 2
      DO 83 J = 1, 5
      CALL BFREAD (WK3, IPNT, ILIMIT, PROBD(1,I,J), ITOTRR,2) !(ITOTRR,2,5)
   83 CONTINUE
   84 CONTINUE
      DO 87 I = 1, 2
      DO 86 J = 1, 5
      DO 85 K = 1, 41
      CALL BFREAD (WK3, IPNT, ILIMIT, PROBDA(1,I,J,K),ITOTRR,2) !(ITOTRR,2,5,41)
   85 CONTINUE
   86 CONTINUE
   87 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, PROOT,        ITOTSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PRPTOT, ITOTRR+MAXSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PRUN,         ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RADINF(1,1),      50, 2) !(50,2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RADINF(1,2),      50, 2) !------
      CALL BFREAD (WK3, IPNT, ILIMIT, RDKILL,       IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, REINEK,            4, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, ROCRI,         MAXSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, ROCRL,         MAXSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, ROOT4,             4, 2)
      DO 89 I = 1, 2
      DO 88 J = 1, 5
      CALL BFREAD (WK3, IPNT, ILIMIT, ROOTD(1,I,J), ITOTRR, 2) !(ITOTRR,2,5)
   88 CONTINUE
   89 CONTINUE
      DO 92 I = 1, 2
      DO 91 J = 1, 5
      DO 90 K = 1, 41
      CALL BFREAD (WK3, IPNT, ILIMIT, ROOTDA(1,I,J,K),ITOTRR,2) !(ITOTRR,2,5,41)
   90 CONTINUE
   91 CONTINUE
   92 CONTINUE
      DO 93 I = 1, IRRTRE
      CALL BFREAD (WK3, IPNT, ILIMIT, ROOTH(1,I),       4, 2) !(4,IRRTRE)
   93 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, ROWDOM,      ITOTSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, ROWIBP(1,1), ITOTSP, 2) !(ITOTSP,2)
      CALL BFREAD (WK3, IPNT, ILIMIT, ROWIBP(1,2), ITOTSP, 2) !----------
      DO 94 I = 1, 100
      CALL BFREAD (WK3, IPNT, ILIMIT, RRATES(1,I), ITOTRR, 2) !(ITOTRR,100)
   94 CONTINUE
      DO 95 I = 1, 10
      CALL BFREAD (WK3, IPNT, ILIMIT, RRGEN(1,I),  ITOTRR, 2) !(ITOTRR,10)
   95 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, RRINCS,      ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RRIRAD,      IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RRJINC,      ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RROBMR,      ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RROBNK,       MAXSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RROBOL,           4, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RROBRD,           4, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RROBSC,           4, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RROBTS,           4, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RROOTT,      IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RRPSWT,      ITOTSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RRRATE,      ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RRRSET,      ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RRSDBH,          50, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RRSRAD,          50, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, RSLOP,       ITOTSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SDRATE,      IRRTRE, 2)
      DO 97 I = 1, 100
      DO 96 J = 1, 3
      CALL BFREAD (WK3, IPNT, ILIMIT, SHCENT(1,I,J),ITOTRR,2) !(ITOTRR,100,3)
   96 CONTINUE
   97 CONTINUE
      DO 98 I = 1, 50
      CALL BFREAD (WK3, IPNT, ILIMIT, SPRQMD(1,I), ITOTRR, 2) !(ITOTRR,50)
   98 CONTINUE
      DO 99 I = 1, ITOTRR
      CALL BFREAD (WK3, IPNT, ILIMIT, SSSFAC(1,I), ITOTSP, 2) !(ITOTSP,ITOTRR)
   99 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, STCUT,            5, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, STEMSI,       MAXSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, STEMSL,       MAXSP, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TNJUMP,      ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TXP12,            4, 2)
      DO 100 I = 1, 4
      CALL BFREAD (WK3, IPNT, ILIMIT, WINDSP(1,I),  MAXSP, 2) !(MAXSP,4)
  100 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, WK22,        IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, XDBH,             2, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, XHT,              2, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, XMINKL,      ITOTRR, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, XMINLF,      ITOTRR, 2)
      DO 101 I = 1, IRRTRE
      CALL BFREAD (WK3, IPNT, ILIMIT, XMTH(1,I),        4, 2) !(4,IRRTRE)
  101 CONTINUE
      CALL BFREAD (WK3, IPNT, ILIMIT, XRRI,        IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, XRRS,            50, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, XXINF,            5, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, YDBH,             2, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, YHT,              2, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, YRRI,        IRRTRE, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, YRRS,            50, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, YYINF,            5, 2)
C---- from common RDCRY ------------
      DO 103 I = 1, 2
      DO 102 J = 1, 5
      CALL BFREAD (WK3, IPNT, ILIMIT, PROBIN(1,I,J), ITOTRR, 2)!(ITOTRR,2,5)
  102 CONTINUE
  103 CONTINUE
      DO 105 I = 1, 2
      DO 104 J = 1, 5
      CALL BFREAD (WK3, IPNT, ILIMIT, CRNSTO(1,I,J), ITOTRR, 2)!(ITOTRR,2,5)
  104 CONTINUE
  105 CONTINUE

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN RDPPGT: CRNSTO=',
     >            CRNSTO

      IF (PDEBUG)
     >    WRITE (JOPPRT,'(/'' IN RDPPGT: END OF ISTND,ICYC='',I7,I4)')
     >          ISTND,ICYC

      RETURN
      END
