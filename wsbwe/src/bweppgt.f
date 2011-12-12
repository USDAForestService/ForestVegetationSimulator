      SUBROUTINE BWEPPGT (WK3, IPNT, ILIMIT, IB)
      IMPLICIT NONE
C----------
C  **BWEPPGT                DATE OF LAST REVISION:  07/14/10
C----------
C  Purpose:
C     Get (read) the GenDefol/Budworm model data for a given stand
C     from buffer.
C     This is part of the Parallel Processing Extension.
C----------
C
C  Call list definitions:
C     ILIMIT: (I)  Size of buffer WK3.
C     IPNT:   (IO) Pointer to curent element in print buffer WK3.
C     WK3:    (IO) Work array used as a buffer.
C     IB:     (I)  Identifies variable set: 1=numeric, 2=character
C
C  Local variable definitions:
C     IIA:    Loop counter.
C     IIB:    Loop counter.
C     IIC:    Loop counter.
C     IID:    Loop counter.
C     IIX:    Array index.
C     J:      Array index.
C     K:      Series indicator for reading of DA file.
C     INTS:   Array of length MXI to hold integer values.
C     LOGICS: Array of length MXL to hold logical values.
C     REALS:  Array of length XMR to hold real values.
C
C  Common block variables and parameters:
C     See comments in common block files.
C
C  Revision History :
C    08-SEP-2006 - Lance R. David (FHTET)
C       This subroutine was written.
C    14-JUL-2010 Lance R. David (FMSC)
C       Added IMPLICIT NONE and declared variables as needed.
C
C----------


C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'METRIC.F77'
      INCLUDE 'PPEPRM.F77'

C.... Common include files.

      INCLUDE 'PPCNTL.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'BWEBOX.F77'
      INCLUDE 'BWECM2.F77'
      INCLUDE 'BWECOM.F77'
      INCLUDE 'BWESTD.F77'

C
C     READ ALL INTEGER VARIABLES WITH IFREAD, LOGICAL VARIABLES
C     WITH LFREAD, AND REAL VARIABLES WITH BFREAD.
C     LONG INTEGERS ARE EQIVALENCED TO REAL VARIABLES AND READ
C     WITH BFREAD.
C     DOUBLE PRECISION VARIABLES (RANDOM NUMBER SEEDS) ARE EQUIVALENCED
C     TO REAL ARRAYS OF LENGTH 2 AND READ WITH BFREAD.
C
C.... Variable declarations.
C.... Parameter statements.
      INTEGER LNCBUF, MXI, MXL, MXR

      PARAMETER (LNCBUF=IRECLN*4)
      PARAMETER (MXL=21,MXI=74,MXR=918)
 
      CHARACTER CBUFF(LNCBUF)

      LOGICAL LOGICS(MXL)
      INTEGER I, IID, INTS(MXI), IB, IIA, IIB, IIC, IIX, J, K,
     &        ILIMIT, IPNT
      REAL    REALS(MXR), WK3(*)
      REAL    SEEDA(2), SEEDB(2), SEEDC(2)
      EQUIVALENCE (SEEDA,DSEEDD), (SEEDB,OBSEED), (SEEDC,WSEED)

      IF(IB .EQ. 2) GOTO 150

      K = 2
      DO J=1,MXL
         LOGICS(J) = .FALSE.
      END DO
      DO J=1,MXI
         INTS(J) = 0
      END DO
      DO J=1,MXR
         REALS(J) = 0.0
      END DO


      IF (PDEBUG)
     >   WRITE (JOPPRT,'(/'' IN BWEPPGT: ISTND,ICYC='',I7,I4)')
     >         ISTND,ICYC

C     Read logical scalars from buffer.
C
      CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: LOGICS=',
     >            LOGICS

C.... Load logical scalars from LOGICS array.
C
C---- from common BWEBOX ------------
      LTEMP1(1) = LOGICS(1)
      LTEMP1(2) = LOGICS(2)
      LTEMP1(3) = LOGICS(3)
      LTEMP1(4) = LOGICS(4)
      LP1       = LOGICS(5)
      LP2       = LOGICS(6)
      LP3       = LOGICS(7)
      LP4       = LOGICS(8)
      LP5       = LOGICS(9)
      LP6       = LOGICS(10)
      LP7       = LOGICS(11)
      LP8       = LOGICS(12)
C---- from common BWECM2 ------------
      LBUDL     = LOGICS(13)
      LFIRST    = LOGICS(14)
      LREGO     = LOGICS(15)
      LSPRAY    = LOGICS(16)
C---- from common RDCOM ------------
      LBWDAM    = LOGICS(17)
      LBWPDM    = LOGICS(18)
      LCALBW    = LOGICS(19)
      LDEFOL    = LOGICS(20)
      LTOPK     = LOGICS(21)

C.... Read integer scalars from buffer.
C
      CALL IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: INTS=',
     >            INTS

C.... Load integer scalars from the INTS array.
C
C---- from common BWEBOX ------------
      IBUDYR      = INTS(1)
      IBWOPT      = INTS(2)
      IDEFPR      = INTS(3)
      IDEFSP(1)   = INTS(4)
      IDEFSP(2)   = INTS(5)
      IDEFSP(3)   = INTS(6)
      IDEFSP(4)   = INTS(7)
      IDEFSP(5)   = INTS(8)
      IFLAG       = INTS(9)
      IOBACT      = INTS(10)
      IOBSCH(1,1) = INTS(11)
      IOBSCH(1,2) = INTS(12)
      IOBSCH(2,1) = INTS(13)
      IOBSCH(2,2) = INTS(14)
      IOBSCH(3,1) = INTS(15)
      IOBSCH(3,2) = INTS(16)
      IPARA       = INTS(17)
      IQUALD      = INTS(18)
      IQUALW      = INTS(19)
      ISTN        = INTS(20)
      ISTNUM      = INTS(21)
      ITEMP(1)    = INTS(22)
      ITEMP(2)    = INTS(23)
      ITEMP(3)    = INTS(24)
      ITEMP(4)    = INTS(25)
      ITEMP(5)    = INTS(26)
      IWSRC       = INTS(27)
      JOWE        = INTS(28)
      NEVENT      = INTS(29)
      NOBDON      = INTS(30)
      NOBSCH      = INTS(31)
      NUMCOL      = INTS(32)
C---- from common BWECM2 ------------
      ILOBYR      = INTS(33)
      IOBDUR      = INTS(34)
      IOBLOC      = INTS(35)
      IOBOPT      = INTS(36)
      ISPRAY      = INTS(37)
      ISPVAR      = INTS(38)
      IWOPT       = INTS(39)
      IWYR        = INTS(40)
      IYRECV      = INTS(41)
      IYREND      = INTS(42)
      IYROBL      = INTS(43)
      IYRSRC      = INTS(44)
      IYRST       = INTS(45)
      JOBWP1      = INTS(46)
      JOBWP2      = INTS(47)
      JOBWP3      = INTS(48)
      JOBWP4      = INTS(49)
      JOBWP5      = INTS(50)
      JOBWP6      = INTS(51)
      JOBWP7      = INTS(52)
      JOBWP8      = INTS(53)
      KRECVR      = INTS(54)
      LIMITS      = INTS(55)
      LOWYRS      = INTS(56)
      NSPRAY      = INTS(57)
      NUMAPP      = INTS(58)
C---- from common BWECOM ------------
      IBWYR1      = INTS(59)
      IBWYR2      = INTS(60)
      ICUMYR      = INTS(61)
      IPRBYR      = INTS(62)
      ITODO       = INTS(63)
      IYRCUR      = INTS(64)
      JOWSBW      = INTS(65)
      NCUMYR      = INTS(66)
      NTODO       = INTS(67)
C---- from common BWESTD ------------
      IFHOST(1)   = INTS(68)
      IFHOST(2)   = INTS(69)
      IFHOST(3)   = INTS(70)
      IFHOST(4)   = INTS(71)
      IFHOST(5)   = INTS(72)
      IFHOST(6)   = INTS(73)
      IFHOST(7)   = INTS(74)

C.... Read larger integer arrays from buffer
C
C     Note on processing arrays:
C     When handling a 2-dimensional array, consider a string of values a
C     complete column of the array and the length of the array as the 
C     number of rows. So, a call to IFREAD or BFREAD will process 1 column
C     of the specified number of rows (length) in a 2-dimensional array.
C
C     CALL IFREAD (WK3,IPNT,ILIMIT, array, length, K)
C     .....................................................................

      DO 20 I = 1, 5
      CALL IFREAD (WK3, IPNT, ILIMIT, IDEF(1,I),        100, K) !(100,5)
   20 CONTINUE

      DO 22 I = 1, 4
      CALL IFREAD (WK3, IPNT, ILIMIT, IEVENT(1,I),      250, K) !(4,250)
   22 CONTINUE

C.... Read real varaibles (scalars and arrays) from buffer.

      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, 111, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 1 - REALS=',
     >            REALS

C.... Load real scalars then arrays from the REALS array.
C     Scalars and small one-dimensional arrays are handled first
C     for ease of indexing. 
C
      RAINDM     = REALS(1)
      RAINDS     = REALS(2)
      RAINM(1)   = REALS(3)
      RAINM(2)   = REALS(4)
      RAINM(3)   = REALS(5)
      RAINS(1)   = REALS(6)
      RAINS(2)   = REALS(7)
      RAINS(3)   = REALS(8)
      WHOTM      = REALS(9)
      WHOTSD     = REALS(10)
      DISPMR     = REALS(11)
      EGGS       = REALS(12)
      OLDMAX     = REALS(13)
      TREEDD     = REALS(14)
      WCOLDW     = REALS(15)
      WHOTF      = REALS(16)
      WRAIND     = REALS(17)
      DSEEDR     = REALS(18)
      OBSEER     = REALS(19)
      WSEEDR     = REALS(20)
      DEFLYR     = REALS(21)
      HOSTST     = REALS(22)
      SPEFF      = REALS(23)
      TRIGGR     = REALS(24)
      WRAIN1(1)  = REALS(25)
      WRAIN1(2)  = REALS(26)
      WRAIN1(3)  = REALS(27)
      WRAIN2(1)  = REALS(28)
      WRAIN2(2)  = REALS(29)
      WRAIN2(3)  = REALS(30)
      WRAIN3(1)  = REALS(31)
      WRAIN3(2)  = REALS(32)
      WRAIN3(3)  = REALS(33)
      WRAINA(1)  = REALS(34)
      WRAINA(1)  = REALS(35)
      WRAINA(3)  = REALS(36)
      WRAINB(1)  = REALS(37)
      WRAINB(2)  = REALS(38)
      WRAINB(3)  = REALS(39)

      IIX = 39
      DO 30 IIA = 1,4
        DO 30 IIB = 1,3
          IIX = IIX + 1
          NEMULT(IIA,IIB) = REALS(IIX)
   30 CONTINUE
C.... IIX at 51
      DO 32 IIA = 1,9
        DO 32 IIB = 1,6
          IIX = IIX + 1
          BW(IIA,IIB) = REALS(IIX)
   32 CONTINUE
C.... IIX at 105
      DO 34 IIA = 1,6
        IIX = IIX + 1
        DEFYRS(IIA) = REALS(IIX)
   34 CONTINUE
C.... IIX ends at 111

C.... Read next real array(s) from buffer.

      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, 918, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 2 - REALS=',
     >            REALS

      IIX = 0
      DO 36 IIA = 1,9
        DO IIB = 1,6
          DO IIC = 1,17
            IIX = IIX + 1
            OUT1(IIA,IIB,IIC) = REALS(IIX)
          END DO
        END DO
   36 CONTINUE

C.... Read next real array(s) from buffer.

      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, 144, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 3 - REALS=',
     >            REALS

      IIX = 0
      DO 38 IIA = 1,6
        DO 38 IIB = 1,3
          DO 38 IIC = 1,8
          IIX = IIX + 1
          OUT2(IIA,IIB,IIC) = REALS(IIX)
   38 CONTINUE

C.... Read next real array(s) from buffer.
C.... Due to the size of this array, it is done in 2 parts,
C.... Third dimension (IIC) 1-10, part 1.

      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, 540, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 4A - REALS=',
     >            REALS

      IIX = 0
      DO 40 IIA = 1,9
        DO 40 IIB = 1,6
          DO 40 IIC = 1,10
            IIX = IIX + 1
            OUT3(IIA,IIB,IIC) = REALS(IIX)
   40 CONTINUE

C.... Read next real array(s) from buffer.
C.... Due to the size of this array, it is done in 2 parts,
C.... Third dimension (IIC) 11-20, part 2.

      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, 540, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 4B - REALS=',
     >            REALS

      IIX = 0
      DO 41 IIA = 1,9
        DO 41 IIB = 1,6
          DO 41 IIC = 11,20
            IIX = IIX + 1
            OUT3(IIA,IIB,IIC) = REALS(IIX)
   41 CONTINUE

C.... Read next real array(s) from buffer.

      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, 399, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 5 - REALS=',
     >            REALS

      IIX = 0
      DO 42 IIA = 1,6
        DO 42 IIB = 1,3
          DO 42 IIC = 1,2
            DO 42 IID = 1,5
              IIX = IIX + 1
              APRBYR(IIA,IIB,IIC,IID) = REALS(IIX)
   42 CONTINUE
C.... IIX at 180
      DO 44 IIA = 1,6
        DO 44 IIB = 1,3
          IIX = IIX + 1
          AVYRMX(IIA,IIB) = REALS(IIX)
   44 CONTINUE
C.... IIX at 198
      DO 46 IIA = 1,6
        DO 46 IIB = 1,3
          IIX = IIX + 1
          BWMXCD(IIA,IIB) = REALS(IIX)
   46 CONTINUE
C.... IIX at 216
      DO 48 IIA = 1,7
        DO 48 IIB = 1,3
          IIX = IIX + 1
          BWTPHA(IIA,IIB) = REALS(IIX)
   48 CONTINUE
C.... IIX at 237
      DO 50 IIA = 1,6
        DO 50 IIB = 1,3
          IIX = IIX + 1
          CDEF(IIA,IIB) = REALS(IIX)
   50 CONTINUE
C.... IIX at 255
      DO 52 IIA = 1,6
        DO 52 IIB = 1,3
          DO 52 IIC = 1,5
            IIX = IIX + 1
            CUMDEF(IIA,IIB,IIC) = REALS(IIX)
   52 CONTINUE
C.... IIX at 345
      DO 54 IIA = 1,9
        DO 54 IIB = 1,6
          IIX = IIX + 1
          FNEW(IIA,IIB) = REALS(IIX)
   54 CONTINUE
C.... IIX ends at 399

C.... Read next real array(s) from buffer.

      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, 333, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 6 - REALS=',
     >            REALS

      IIX = 0
      DO 56 IIA = 1,6
        DO 56 IIB = 1,9
          DO 56 IIC = 1,4
            IIX = IIX + 1
            FOLADJ(IIA,IIB,IIC) = REALS(IIX)
   56 CONTINUE
C.... IIX at 216
      DO 58 IIA = 1,9
        DO 58 IIB = 1,6
          IIX = IIX + 1
          FOLD1(IIA,IIB) = REALS(IIX)
   58 CONTINUE
C.... IIX at 270
      DO 60 IIA = 1,9
        DO 60 IIB = 1,6
          IIX = IIX + 1
          FOLD2(IIA,IIB) = REALS(IIX)
   60 CONTINUE
C.... IIX at 324
      DO 62 IIA = 1,9
        IIX = IIX + 1
        FOLNH(IIA) = REALS(IIX)
   62 CONTINUE
C.... IIX ends at 333

C.... Read next real array(s) from buffer.

      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, 522, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 7 - REALS=',
     >            REALS

      IIX = 0
      DO 64 IIA = 1,6
        DO 64 IIB = 1,9
          DO 64 IIC = 1,4
          IIX = IIX + 1
          FOLPOT(IIA,IIB,IIC) = REALS(IIX)
   64 CONTINUE
C.... IIX at 216
      DO 66 IIA = 1,9
        DO 66 IIB = 1,6
          IIX = IIX + 1
          FREM(IIA,IIB) = REALS(IIX)
   66 CONTINUE
C.... IIX at 270
      DO 68 IIA = 1,6
        DO 68 IIB = 1,3
          IIX = IIX + 1
          PEDDS(IIA,IIB) = REALS(IIX)
   68 CONTINUE
C.... IIX at 288
      DO 70 IIA = 1,6
        DO 70 IIB = 1,3
          IIX = IIX + 1
          PEHTG(IIA,IIB) = REALS(IIX)
   70 CONTINUE
C.... IIX at 306
      DO 72 IIA = 1,6
        DO 72 IIB = 1,9
          DO 72 IIC = 1,4
            IIX = IIX + 1
            POFPOT(IIA,IIB,IIC) = REALS(IIX)
   72 CONTINUE
C.... IIX ends at 522

C.... Read next real array(s) from buffer.

      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, 306, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 8 - REALS=',
     >            REALS

      IIX = 0
      DO 74 IIA = 1,6
        DO 74 IIB = 1,9
          DO 74 IIC = 1,4
            IIX = IIX + 1
            PRBIO(IIA,IIB,IIC) = REALS(IIX)
   74 CONTINUE
C.... IIX at 216
      DO 76 IIA = 1,6
        DO 76 IIB = 1,3
          IIX = IIX + 1
          RDDSM1(IIA,IIB) = REALS(IIX)
   76 CONTINUE
C.... IIX at 234
      DO 78 IIA = 1,6
        DO 78 IIB = 1,3
          IIX = IIX + 1
          RHTGM1(IIA,IIB) = REALS(IIX)
   78 CONTINUE
C.... IIX at 252
      DO 80 IIA = 1,9
        DO 80 IIB = 1,6
          IIX = IIX + 1
            ACTNEW(IIA,IIB) = REALS(IIX)
   80 CONTINUE
C.... IIX ends at 306

C.... Read next real array(s) from buffer.

      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, 298, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 9 - REALS=',
     >            REALS

      IIX = 0

      DO 82 IIA = 1,9
        DO 82 IIB = 1,6
          IIX = IIX + 1
          ANTDEN(IIA,IIB) = REALS(IIX)
   82 CONTINUE
C.... IIX at 54
      DO 84 IIA = 1,9
        DO 84 IIB = 1,6
          IIX = IIX + 1
          BIRDEN(IIA,IIB) = REALS(IIX)
   84 CONTINUE
C.... IIX at 108
      DO 86 IIA = 1,9
        DO 86 IIB = 1,6
          IIX = IIX + 1
          MYS1DN(IIA,IIB) = REALS(IIX)
   86 CONTINUE
C.... IIX at 162
      DO 88 IIA = 1,9
        DO 88 IIB = 1,6
          IIX = IIX + 1
          MYS2DN(IIA,IIB) = REALS(IIX)
   88 CONTINUE
C.... IIX at 216
      DO 90 IIA = 1,9
        DO 90 IIB = 1,6
          IIX = IIX + 1
          MYS3DN(IIA,IIB) = REALS(IIX)
   90 CONTINUE
C.... IIX at 270
      DO 92 IIA = 1,4
        IIX = IIX + 1
        FOLDVY(IIA) = REALS(IIX)
   92 CONTINUE
C.... IIX at 274
      DO 94 IIA = 1,4
        IIX = IIX + 1
        FOLWTY(IIA) = REALS(IIX)
   94 CONTINUE
C.... IIX at 278
      DO 96 IIA = 1,10
        IIX = IIX + 1
        SPEFFS(IIA) = REALS(IIX)
   96 CONTINUE
C.... IIX at 288
      DO 98 IIA = 1,10
        IIX = IIX + 1
        SPINST(IIA) = REALS(IIX)
   98 CONTINUE
C.... IIX ends at 298

C
C.... Read double precision scalars (random number seeds) from buffer.
C     SEEDA is DSEEDD, SEEDB is OBSEED, SEEDC is WSEED

      CALL BFREAD (WK3, IPNT, ILIMIT, SEEDA, 2, K)
      CALL BFREAD (WK3, IPNT, ILIMIT, SEEDB, 2, K)
      CALL BFREAD (WK3, IPNT, ILIMIT, SEEDC, 2, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 10 - SEEDA=',SEEDA,
     >            ' SEEDB=',SEEDB,' SEEDC=',SEEDC

      GOTO 210
  150 CONTINUE

C
C.... Read character variables from buffer.
C
C     Static data items are not stored and retrieved between stands.
C
C     --- from BWEBOX ---
C     CHARACTER*4 MGMIDB
C     CHARACTER*8 DEFLAB,DLABS(5)
C     CHARACTER*12 STATES(10,2)  -- Static 
C     CHARACTER*16 WSLOOK(100,10)  -- Static
C     CHARACTER*20 TEMPS2(3)  -- Static
C     CHARACTER*40 OUTNAM(8),  -- Static
C                  STNAME,   -- Static
C                  WFNAME    -- Static
C     CHARACTER*50 EVENT(20) -- Static
C     CHARACTER*72 ITITLB
C     --- from BWECM2 ---
C     CHARACTER*3 IOUT6A(3)
C     --- from BWECOM ---
C     CHARACTER*8 TABLE(25)  -- Static

      K=1
      DO 200 I=1,4
        CALL CHREAD(CBUFF,IPNT,LNCBUF,MGMIDB(I:I), K)
        K=2
  200 CONTINUE

      DO 202 I=1,8
        CALL CHREAD(CBUFF,IPNT,LNCBUF,DEFLAB(I:I), K)
  202 CONTINUE

      DO 204 J=1,5
        DO 204 I=1,8
          CALL CHREAD(CBUFF,IPNT,LNCBUF,DLABS(J)(I:I), K)
  204 CONTINUE

      DO 206 I=1,72
        CALL CHREAD(CBUFF,IPNT,LNCBUF,ITITLB(I:I), K)
  206 CONTINUE

      DO 208 J=1,3
        DO 208 I=1,3
          IF (I .EQ. 3) K=3
          CALL CHREAD(CBUFF,IPNT,LNCBUF,IOUT6A(J)(I:I), K)
  208 CONTINUE

  210 CONTINUE
      IF (PDEBUG)
     >    WRITE (JOPPRT,'(/'' IN BWEPPGT: END OF ISTND,ICYC='',I7,I4)')
     >          ISTND,ICYC

      RETURN
      END
