      SUBROUTINE BWEPPGT (WK3, IPNT, ILIMIT, IB)
      IMPLICIT NONE
C----------
C WSBWE $Id$
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
C   08-SEP-2006 - Lance R. David (FHTET)
C     This subroutine was written.
C   14-JUL-2010 Lance R. David (FMSC)
C     Added IMPLICIT NONE and declared variables as needed.
C   23-JUN-2011 Lance R. David (FMSC)
C     Added BWPRMS array for RAWS daily weather processing to BLCOM3.
C   20-SEP-2013 Lance R. David (FMSC)
C     Added RAWS weather year range (IYRNG array), expanded IEVENT array
C     and rectified with bewpppt.f.
C   15-OCT-2014 Lance R. David (FMSC)
C     Complete rebuild of routine to include all integer and real variables
C     and arrays in common blocks. There are a few character array noted at
C     the end of this routine that are static and not stored or retrieved.
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

      PARAMETER (MXL=21,MXI=86,MXR=918)
      PARAMETER (LNCBUF=IRECLN*4)
 
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
C---- from common BWECOM ------------
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
      INSTSP      = INTS(34)
      IOBDUR      = INTS(35)
      IOBLOC      = INTS(36)
      IOBOPT      = INTS(37)
      ISPRAY      = INTS(38)
      ISPVAR      = INTS(39)
      ISPYR(1)    = INTS(40)
      ISPYR(2)    = INTS(41)
      ISPYR(3)    = INTS(42)
      ISPYR(4)    = INTS(43)
      ISPYR(5)    = INTS(44)
      IWOPT       = INTS(45)
      IWYR        = INTS(46)
      IYRCNT      = INTS(47)
      IYRECV      = INTS(48)
      IYREND      = INTS(49)
      IYRNG(1)    = INTS(50)
      IYRNG(2)    = INTS(51)
      IYROBL      = INTS(52)
      IYRSRC      = INTS(53)
      IYRST       = INTS(54)
      JOBWP1      = INTS(55)
      JOBWP2      = INTS(56)
      JOBWP3      = INTS(57)
      JOBWP4      = INTS(58)
      JOBWP5      = INTS(59)
      JOBWP6      = INTS(60)
      JOBWP7      = INTS(61)
      JOBWP8      = INTS(62)
      KRECVR      = INTS(63)
      LIMITS      = INTS(64)
      LOWYRS      = INTS(65)
      NSPRAY      = INTS(66)
      NUMAPP      = INTS(67)
C---- from common BWECOM ------------
      IBWYR1      = INTS(68)
      IBWYR2      = INTS(69)
      ICUMYR      = INTS(70)
      IPRBYR      = INTS(71)
      ITODO       = INTS(72)
      IYRCUR      = INTS(73)
      JOWSBW      = INTS(74)
      NCROWN      = INTS(75)
      NCUMYR      = INTS(76)
      NHOSTS      = INTS(77)
      NTODO       = INTS(78)
      TABSZ       = INTS(79)
C---- from common BWESTD ------------
      IFHOST(1)   = INTS(80)
      IFHOST(2)   = INTS(81)
      IFHOST(3)   = INTS(82)
      IFHOST(4)   = INTS(83)
      IFHOST(5)   = INTS(84)
      IFHOST(6)   = INTS(85)
      IFHOST(7)   = INTS(86)

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

      DO 22 I = 1, 5
      CALL IFREAD (WK3, IPNT, ILIMIT, IEVENT(1,I),      250, K) !(250,5)
   22 CONTINUE

C.... Read real varaibles (scalars and arrays) from buffer.
      IIX = 301
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, IIX, K)

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
C BWECM2.F77 - BLCOMN
      DEFYRS(1)  = REALS(11)
      DEFYRS(2)  = REALS(12)
      DEFYRS(3)  = REALS(13)
      DEFYRS(4)  = REALS(14)
      DEFYRS(5)  = REALS(15)
      DEFYRS(6)  = REALS(16)
      DISPMR     = REALS(17)
      EGG1(1)    = REALS(18)
      EGG1(2)    = REALS(19)
      EGG1(3)    = REALS(20)
      EGG1(4)    = REALS(21)
      EGG1(5)    = REALS(22)
      EGG1(6)    = REALS(23)
      EGG2(1)    = REALS(24)
      EGG2(2)    = REALS(25)
      EGG2(3)    = REALS(26)
      EGG2(4)    = REALS(27)
      EGG2(5)    = REALS(28)
      EGG2(6)    = REALS(29)
      EGGDEN     = REALS(30)
      EGGS       = REALS(31)
      FRESHC(1)  = REALS(32)
      FRESHC(2)  = REALS(33)
      FRESHC(3)  = REALS(34)
      FRESHC(4)  = REALS(35)
      FRESHC(5)  = REALS(36)
      FRESHC(6)  = REALS(37)
      FWSURV     = REALS(38)
      GPERM2     = REALS(39)
      OLDMAX     = REALS(40)
      TREEDD     = REALS(41)
      WASTED(1)  = REALS(42)
      WASTED(2)  = REALS(43)
      WCOLDW     = REALS(44)
      WHOTF      = REALS(45)
      WRAIND     = REALS(46)
C BWECM2.F77 - BLCOM2
      AVEAMT(1)  = REALS(47)
      AVEAMT(2)  = REALS(48)
      AVEAMT(3)  = REALS(49)
      AVEAMT(4)  = REALS(50)
      AVEAMT(5)  = REALS(51)
      AVEAMT(6)  = REALS(52)
      DAYS(1)    = REALS(53)
      DAYS(2)    = REALS(54)
      DAYS(3)    = REALS(55)
      DISPX(1)   = REALS(56)
      DISPX(2)   = REALS(57)
      DISPX(3)   = REALS(58)
      DISPX(4)   = REALS(59)
      DISPY(1)   = REALS(60)
      DISPY(2)   = REALS(61)
      DISPY(3)   = REALS(62)
      DISPY(4)   = REALS(63)
      EWTX(1)    = REALS(64)
      EWTX(2)    = REALS(65)
      EWTX(3)    = REALS(66)
      EWTX(4)    = REALS(67)
      EWTY(1)    = REALS(68)
      EWTY(2)    = REALS(69)
      EWTY(3)    = REALS(70)
      EWTY(4)    = REALS(71)
      OLDX(1)    = REALS(72)
      OLDX(2)    = REALS(73)
      OLDX(3)    = REALS(74)
      OLDX(4)    = REALS(75)
      OLDY(1)    = REALS(76)
      OLDY(2)    = REALS(77)
      OLDY(3)    = REALS(78)
      OLDY(4)    = REALS(79)
      PMATED     = REALS(80)
      SRATIO     = REALS(81)
      WASTO      = REALS(82)
C BWECM2.F77 - BLCOM3
      ANT_BD(1)  = REALS(83)
      ANT_BD(2)  = REALS(84)
      ANT_BD(3)  = REALS(85)
      ANT_BD(4)  = REALS(86)
      ANT_BD(5)  = REALS(87)
      ANT_BD(6)  = REALS(88)
      ANT_BD(7)  = REALS(89)
      ANT_BD(8)  = REALS(90)
      ANT_BD(9)  = REALS(91)
      APRED(1)   = REALS(92)
      APRED(2)   = REALS(93)
      APRED(3)   = REALS(94)
      BPRED(1)   = REALS(95)
      BPRED(2)   = REALS(96)
      BPRED(3)   = REALS(97)
      DEFLYR     = REALS(98)
      DEVEL      = REALS(99)
      DEVELS(1)  = REALS(100)
      DEVELS(2)  = REALS(101)
      DEVELS(3)  = REALS(102)
      DFLUSH     = REALS(103)
      FOLDVX(1)  = REALS(104)
      FOLDVX(2)  = REALS(105)
      FOLDVX(3)  = REALS(106)
      FOLDVX(4)  = REALS(107)
      FOLDVY(1)  = REALS(108)
      FOLDVY(2)  = REALS(109)
      FOLDVY(3)  = REALS(110)
      FOLDVY(4)  = REALS(111)
      FOLWTX(1)  = REALS(112)
      FOLWTX(2)  = REALS(113)
      FOLWTX(3)  = REALS(114)
      FOLWTX(4)  = REALS(115)
      FOLWTY(1)  = REALS(116)
      FOLWTY(2)  = REALS(117)
      FOLWTY(3)  = REALS(118)
      FOLWTY(4)  = REALS(119)
      M1PRED(1)  = REALS(120)
      M1PRED(2)  = REALS(121)
      M1PRED(3)  = REALS(122)
      M2PRED(1)  = REALS(123)
      M2PRED(2)  = REALS(124)
      M2PRED(3)  = REALS(125)
      M3PRED(1)  = REALS(126)
      M3PRED(2)  = REALS(127)
      M3PRED(3)  = REALS(128)
      OBPHX(1)   = REALS(129)
      OBPHX(2)   = REALS(130)
      OBPHX(3)   = REALS(131)
      OBPHX(4)   = REALS(132)
      OBPHY(1)   = REALS(133)
      OBPHY(2)   = REALS(134)
      OBPHY(3)   = REALS(135)
      OBPHY(4)   = REALS(136)
      PRATE(1)   = REALS(137)
      PRATE(2)   = REALS(138)
      PRATE(3)   = REALS(139)
      PRATE(4)   = REALS(140)
      PRATE(5)   = REALS(141)
      PRATE(6)   = REALS(142)
      PRATE(7)   = REALS(143)
      PRATE(8)   = REALS(144)
      PRATE(9)   = REALS(145)
      RPHEN(1)   = REALS(146)
      RPHEN(2)   = REALS(147)
      RPHEN(3)   = REALS(148)
      RPHEN(4)   = REALS(149)
      RPHEN(5)   = REALS(150)
      RPHEN(6)   = REALS(151)
      SPEFF      = REALS(152) 
      SYNCHX(1)  = REALS(153)
      SYNCHX(2)  = REALS(154)
      SYNCHX(3)  = REALS(155)
      SYNCHX(4)  = REALS(156)
      SYNCHX(5)  = REALS(157)
      SYNCHX(6)  = REALS(158)
      SYNCHY(1)  = REALS(159)
      SYNCHY(2)  = REALS(160)
      SYNCHY(3)  = REALS(161)
      SYNCHY(4)  = REALS(162)
      SYNCHY(5)  = REALS(163)
      SYNCHY(6)  = REALS(164)
      TRIGGR     = REALS(165) 
      WRAIN1(1)  = REALS(166) 
      WRAIN1(2)  = REALS(167) 
      WRAIN1(3)  = REALS(168) 
      WRAIN2(1)  = REALS(169) 
      WRAIN2(2)  = REALS(170) 
      WRAIN2(3)  = REALS(171) 
      WRAIN3(1)  = REALS(172) 
      WRAIN3(2)  = REALS(173) 
      WRAIN3(3)  = REALS(174) 
      WRAINA(1)  = REALS(175)
      WRAINA(2)  = REALS(176)
      WRAINA(3)  = REALS(177)
      WRAINB(1)  = REALS(178)
      WRAINB(2)  = REALS(179)
      WRAINB(3)  = REALS(180)
C BWECM2.F77 - BLCOM4
      ADMORT     = REALS(181)
      DISPDR(1)  = REALS(182)
      DISPDR(2)  = REALS(183)
      DISPDR(3)  = REALS(184)
      DISPDR(4)  = REALS(185)
      DISPDR(5)  = REALS(186)
      DISPDR(6)  = REALS(187)
      DISPDR(7)  = REALS(188)
      DISPDR(8)  = REALS(189)
      DISPDR(9)  = REALS(190)
      DISPMX(1)  = REALS(191)
      DISPMX(2)  = REALS(192)
      DISPMX(3)  = REALS(193)
      DISPMX(4)  = REALS(194)
      DISPMY(1)  = REALS(195)
      DISPMY(2)  = REALS(196)
      DISPMY(3)  = REALS(197)
      DISPMY(4)  = REALS(198)
      EGDISP     = REALS(199)
      EPMASS     = REALS(200)
      HOSTST     = REALS(201)
      OBSEER     = REALS(202)
      WSEEDR     = REALS(203)
C BWECOM.F77
      BWFINT     = REALS(204)
      DSEEDR     = REALS(205)   
C BWESTD.F77
      FOLNH(1)   = REALS(206)
      FOLNH(2)   = REALS(207)
      FOLNH(3)   = REALS(208)
      FOLNH(4)   = REALS(209)
      FOLNH(5)   = REALS(210)
      FOLNH(6)   = REALS(211)
      FOLNH(7)   = REALS(212)
      FOLNH(8)   = REALS(213)
      FOLNH(9)   = REALS(214)
      PRCRN3(1)  = REALS(215)
      PRCRN3(2)  = REALS(216)
      PRCRN3(3)  = REALS(217)
      PRCRN3(4)  = REALS(218)
      PRCRN3(5)  = REALS(219)
      PRCRN3(6)  = REALS(220)
      PRCRN3(7)  = REALS(221)
      PRCRN3(8)  = REALS(222)
      PRCRN3(9)  = REALS(223)
C
C     Load REAL arrays larger than single dimension of 9
C     set index based on preceding assignment in scalar section.
C
      IIX = 223
      DO IIA = 1,4
        DO IIB = 1,3
          IIX = IIX + 1
          NEMULT(IIA,IIB) = REALS(IIX)   ! NEMULT(4,3) BWEBOX.F77
        END DO
      END DO
C.... IIX at 235
      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          BW(IIA,IIB) = REALS(IIX)       ! BW(9,6) BWECM2.FYY
        END DO
      END DO
C.... IIX at 289
      DO IIA = 1,2
        DO IIB = 1,6
          IIX = IIX + 1
          EATEN(IIA,IIB) = REALS(IIX)    ! EATEN(2,6) BWECM2.F77
        END DO
      END DO
C.... IIX ends at 301

C.... Read next real array(s) from buffer.
      IIX = 192
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, IIX, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 2 - REALS=',
     >            REALS
C.... Load next real array(s) from REALS.

      IIX = 0

      DO IIA = 1,3
        DO IIB = 1,2
          IIX = IIX + 1
          OBPHAS(IIA,IIB) = REALS(IIX)    ! OBPHAS(3,2) BWECM2.F77
        END DO
      END DO
C.... IIX at 6

      DO IIA = 1,8
        DO IIB = 1,3
          IIX = IIX + 1
          OBTABL(IIA,IIB) = REALS(IIX)    ! OBTABL(8,3) BWECM2.F77
        END DO
      END DO
C.... IIX at 30

      DO IIA = 1,6
        DO IIB = 1,3
          DO IIC = 1,8
            IIX = IIX + 1
             OUT2(IIA,IIB,IIC) = REALS(IIX) ! OUT2(6,3,8) BWECM2.F77
          END DO
        END DO
      END DO
C.... IIX at 174

      DO IIA = 1,3
        DO IIB = 1,6
          IIX = IIX + 1
          OBTABL(IIA,IIB) = REALS(IIX)    ! PHENOL(3,6) BWECM2.F77
        END DO
      END DO
C.... IIX ends at 192

C.... Read next real array(s) from buffer.
      IIX = 918
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, IIX, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 3 - REALS=',
     >            REALS

C.... Load next real array(s) from REALS.

      IIX = 0
      DO IIA = 1,9
        DO IIB = 1,6
          DO IIC = 1,17
            IIX = IIX + 1
            OUT1(IIA,IIB,IIC) = REALS(IIX)  ! OUT1(9,6,17) BWECM2.F77
          END DO
        END DO
      END DO
C.... IIX ends at 918

C.... Read next real array(s) from buffer.
      IIX = 540
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, IIX, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 4A - REALS=',
     >            REALS
C.... Load next real array(s) from REALS.
C.... Due to the size of this array, it is done in 2 parts,
C.... Third dimension (IIC) 1-10, part 1.
      IIX = 0
      DO IIA = 1,9
        DO IIB = 1,6
          DO IIC = 1,10
            IIX = IIX + 1
            OUT3(IIA,IIB,IIC) = REALS(IIX)  ! OUT3(9,6,20) BWECM2.F77
          END DO
        END DO
      END DO
C.... IIX ends at 540

C.... Read next real array(s) from buffer.
      IIX = 540
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, IIX, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 4B - REALS=',
     >            REALS

C.... Load next real array(s) from REALS.
C.... Due to the size of this array, it is done in 2 parts,
C.... Third dimension (IIC) 11-20, part 2.
      IIX = 0
      DO IIA = 1,9
        DO IIB = 1,6
          DO IIC = 11,20
            IIX = IIX + 1
            OUT3(IIA,IIB,IIC) = REALS(IIX) ! OUT3(9,6,20) BWECM2.F77
          END DO
        END DO
      END DO
C.... IIX ends at 540

C.... Read next real array(s) from buffer.
      IIX = 768
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, IIX, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 5 - REALS=',
     >            REALS

      IIX = 0
      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          A1(IIA,IIB) = REALS(IIX)    ! A1(9,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 54

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          A2(IIA,IIB) = REALS(IIX)    ! A2(9,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 108

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          A3(IIA,IIB) = REALS(IIX)    ! A3(9,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 162

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          A4(IIA,IIB) = REALS(IIX)    ! A4(9,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 216

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          ACTNEW(IIA,IIB) = REALS(IIX) ! ACTNEW(9,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 270

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          ANTDEN(IIA,IIB) = REALS(IIX)    ! ANTDEN(9,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 324

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          BIRDEN(IIA,IIB) = REALS(IIX)    ! BIRDEN(9,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 378

      DO IIA = 1,2
        DO IIB = 1,6
          IIX = IIX + 1
          ECI(IIA,IIB) = REALS(IIX)    ! ECI(2,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 390

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          GMAX(IIA,IIB) = REALS(IIX)    ! GMAX(9,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 444

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          GMIN(IIA,IIB) = REALS(IIX)    ! GMIN(9,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 498

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          MYS1DN(IIA,IIB) = REALS(IIX)    ! MYS1DN(9,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 552

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          MYS2DN(IIA,IIB) = REALS(IIX)    ! MYS2DN(9,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 606

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          MYS3DN(IIA,IIB) = REALS(IIX)    ! MYS3DN(9,6) BWECM2.F77
        END DO
      END DO
C.... IIX at 660

      DO IIA = 1,2
        DO IIB = 1,4
          IIX = IIX + 1
          STARVX(IIA,IIB) = REALS(IIX)    ! STARVX(2,4) BWECM2.F77
        END DO
      END DO
C.... IIX at 714

      DO IIA = 1,2
        DO IIB = 1,4
          IIX = IIX + 1
          STARVY(IIA,IIB) = REALS(IIX)    ! STARVY(2,4) BWECM2.F77
        END DO
      END DO
C.... IIX ends at 768

C.... Read next real array(s) from buffer.
      IIX = 610
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, IIX, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 6 - REALS=',
     >            REALS

C.... Load next real array(s) from REALS.
      IIX = 0
      DO IIA = 1,10
        DO IIB = 1,4
          IIX = IIX + 1
          BWEATH(IIA,IIB) = REALS(IIX)    ! BWEATH(10,4) BWECM2.F77
        END DO
      END DO
C.... IIX at 40

      DO IIA = 1,10
        IIX = IIX + 1
        SPEFFS(IIA) = REALS(IIX)         ! SPEFFS(10) BWECM2.F77
      END DO
C.... IIX at 50

      DO IIA = 1,10
        IIX = IIX + 1
        SPINST(IIA) = REALS(IIX)         ! SPINST(10) BWECM2.F77
      END DO
C.... IIX at 60

      DO IIA = 1,11
        DO IIB = 1,50
          IIX = IIX + 1
          BWPRMS(IIA,IIB) = REALS(IIX)   ! BWPRMS(11,50) BWECM2.F77
        END DO
      END DO
C.... IIX ends at 610

C.... Read next real array(s) from buffer.
      IIX = 399
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, IIX, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 7 - REALS=',
     >            REALS

C.... Load next real array(s) from REALS.
      IIX = 0
      DO IIA = 1,6
        DO IIB = 1,3
          DO IIC = 1,2
            DO IID = 1,5
              IIX = IIX + 1
              APRBYR(IIA,IIB,IIC,IID) = REALS(IIX)   ! APRBYR(6,3,2,5) BWESTD.F77
            END DO
          END DO
        END DO
      END DO
C.... IIX at 180

      DO IIA = 1,6
        DO IIB = 1,3
          IIX = IIX + 1
          AVYRMX(IIA,IIB) = REALS(IIX)   ! AVYRMX(6,3) BWESTD.F77
        END DO
      END DO
C.... IIX at 198

      DO IIA = 1,6
        DO IIB = 1,3
          IIX = IIX + 1
          BWMXCD(IIA,IIB) = REALS(IIX)   ! BWMXCD(6,3) BWESTD.F77
        END DO
      END DO
C.... IIX at 216

      DO IIA = 1,7
        DO IIB = 1,3
          IIX = IIX + 1
          BWTPHA(IIA,IIB) = REALS(IIX)   ! BWTPHA(7,3) BWESTD.F77
        END DO
      END DO
C.... IIX at 237

      DO IIA = 1,6
        DO IIB = 1,3
          IIX = IIX + 1
          CDEF(IIA,IIB) = REALS(IIX)     ! CDEF(6,3) BWESTD.F77
        END DO
      END DO
C.... IIX at 255

      DO IIA = 1,6
        DO IIB = 1,3
          DO IIC = 1,5
            IIX = IIX + 1
            CUMDEF(IIA,IIB,IIC) = REALS(IIX)   ! CUMDEF(6,3,5) BWESTD.F77
          END DO
        END DO
      END DO
C.... IIX at 345

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          FNEW(IIA,IIB) = REALS(IIX)   ! FNEW(9,6) BWESTD.F77
        END DO
      END DO
C.... IIX ends at 399

C.... Read next real array(s) from buffer.
      IIX = 324
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, IIX, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 8 - REALS=',
     >            REALS

C.... Load next real array(s) from REALS.

      IIX = 0
      DO IIA = 1,6
        DO IIB = 1,9
          DO IIC = 1,4
            IIX = IIX + 1
            FOLADJ(IIA,IIB,IIC) = REALS(IIX)   ! FOLADJ(6,9,4) BWESTD.F77
          END DO
        END DO
      END DO
C.... IIX at 216

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          FOLD1(IIA,IIB) = REALS(IIX)   ! FOLD1(9,6) BWESTD.F77
        END DO
      END DO
C.... IIX at 270

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          FOLD2(IIA,IIB) = REALS(IIX)   ! FOLD2(9,6) BWESTD.F77
        END DO
      END DO
C.... IIX ends at 324

C.... Read next real array(s) from buffer.
      IIX = 522
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, IIX, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 9 - REALS=',
     >            REALS

C.... Load next real array(s) from REALS.

      IIX = 0
      DO IIA = 1,6
        DO IIB = 1,9
          DO IIC = 1,4
            IIX = IIX + 1
            FOLPOT(IIA,IIB,IIC) = REALS(IIX)   ! FOLPOT(6,9,4) BWESTD.F77
          END DO
        END DO
      END DO
C.... IIX at 216

      DO IIA = 1,9
        DO IIB = 1,6
          IIX = IIX + 1
          FREM(IIA,IIB) = REALS(IIX)   ! FREM(9,6) BWESTD.F77
        END DO
      END DO
C.... IIX at 270

      DO IIA = 1,6
        DO IIB = 1,3
          IIX = IIX + 1
          PEDDS(IIA,IIB) = REALS(IIX)   ! PEDDS(6,3) BWESTD.F77
        END DO
      END DO
C.... IIX at 288

      DO IIA = 1,6
        DO IIB = 1,3
          IIX = IIX + 1
          PEHTG(IIA,IIB) = REALS(IIX)   ! PEHTG(6,3) BWESTD.F77
        END DO
      END DO
C.... IIX at 306

      DO IIA = 1,6
        DO IIB = 1,9
          DO IIC = 1,4
            IIX = IIX + 1
            POFPOT(IIA,IIB,IIC) = REALS(IIX)   ! POFPOT(6,9,4) BWESTD.F77
          END DO
        END DO
      END DO
C.... IIX ends at 522

C.... Read next real array(s) from buffer.
      IIX = 360
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, IIX, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 10 - REALS=',
     >            REALS

C.... Load next real array(s) into REALS.

      IIX = 0
      DO IIA = 1,6
        DO IIB = 1,9
          DO IIC = 1,4
            IIX = IIX + 1
            PRBIO(IIA,IIB,IIC) = REALS(IIX)   ! PRBIO(6,9,4) BWESTD.F77
          END DO
        END DO
      END DO
C.... IIX at 216

      DO IIA = 1,6
        DO IIB = 1,3
          IIX = IIX + 1
          RDDSM1(IIA,IIB) = REALS(IIX)   ! RDDSM1(6,3) BWESTD.F77
        END DO
      END DO
C.... IIX at 234

      DO IIA = 1,4
        DO IIB = 1,9
          IIX = IIX + 1
          RELFX(IIA,IIB) = REALS(IIX)   ! RELFX(4,9) BWESTD.F77
        END DO
      END DO
C.... IIX at 270

      DO IIA = 1,4
        DO IIB = 1,9
          IIX = IIX + 1
          RELFY(IIA,IIB) = REALS(IIX)    ! RELFY(4,9) BWESTD.F77
        END DO
      END DO
C.... IIX at 306

      DO IIA = 1,6
        DO IIB = 1,3
          IIX = IIX + 1
          RHTGM1(IIA,IIB) = REALS(IIX)   ! RHTGM1(6,3) BWESTD.F77
        END DO
      END DO
C.... IIX at 324

      DO IIA = 1,4
        DO IIB = 1,9
          IIX = IIX + 1
          THEOFL(IIA,IIB) = REALS(IIX)   ! THEOFL(4,9) BWESTD.F77
        END DO
      END DO
C.... IIX ends at 360

C
C.... Read double precision scalars (random number seeds) from buffer.
C     SEEDA is DSEEDD, SEEDB is OBSEED, SEEDC is WSEED

      CALL BFREAD (WK3, IPNT, ILIMIT, SEEDA, 2, K)
      CALL BFREAD (WK3, IPNT, ILIMIT, SEEDB, 2, K)
      CALL BFREAD (WK3, IPNT, ILIMIT, SEEDC, 2, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPGT: 11 - SEEDA=',SEEDA,
     >            ' SEEDB=',SEEDB,' SEEDC=',SEEDC

      GOTO 210
C     These exist for control of numeric or character processing
C     specified by subroutine parameter IB.
  150 CONTINUE

C
C.... Read character variables from buffer.
C
C     Static data items are not stored and retrieved between stands.
C
C     --- from BWEBOX ---
C     CHARACTER*4 MGMIDB
C     CHARACTER*8 DEFLAB,DLABS(5)
C     CHARACTER*12 STATES(10,2)    -- Static 
C     CHARACTER*16 WSLOOK(100,10)  -- Static
C     CHARACTER*20 TEMPS2(3)       -- Static
C     CHARACTER*40 OUTNAM(8)       -- Static
C                  STNAME          -- Static
C                  WFNAME          -- Static
C     CHARACTER*50 EVENT(20)       -- Static
C     CHARACTER*72 ITITLB
C     --- from BWECM2 ---
C     CHARACTER*3 IOUT6A(3)
C     --- from BWECOM ---
C     CHARACTER*8 TABLE(25)        -- Static

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
