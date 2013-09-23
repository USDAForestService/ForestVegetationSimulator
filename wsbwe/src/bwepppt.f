      SUBROUTINE BWEPPPT (WK3, IPNT, ILIMIT, IB)
      IMPLICIT NONE
C----------
C  **BWEPPPT                DATE OF LAST REVISION:  09/20/13
C----------
C  Purpose:
C     Put (store) the GenDefol/Budworm model data for a given stand
C     into buffer.
C     This is part of the Parallel Processing Extension.
C----------------------------------------------------------------------
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
C   11-SEP-2006 - Lance R. David (FHTET)
C     This subroutine was written.
C   14-JUL-2010 Lance R. David (FMSC)
C     Added IMPLICIT NONE and declared variables as needed.
C   23-JUN-2011 Lance R. David (FMSC)
C     Added BWPRMS array for RAWS daily weather processing to BLCOM3.
C   20-SEP-2013 Lance R. David (FMSC)
C     Added RAWS weather year range (IYRNG array), expanded IEVENT array.
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
C     WRITE ALL INTEGER VARIABLES WITH IFWRIT, LOGICAL VARIABLES
C     WITH LFWRIT, AND REAL VARIABLES WITH BFWRIT.
C     LONG INTEGERS ARE EQIVALENCED TO REAL VARIABLES AND WRITTEN
C     WITH BFWRIT.
C     DOUBLE PRECISION VARIABLES (RANDOM NUMBER SEEDS) ARE EQUIVALENCED
C     TO REAL ARRAYS OF LENGTH 2 AND WRITTEN WITH BFWRIT.
C
C.... Variable declarations.
C.... Parameter statements.
      INTEGER LNCBUF, MXI, MXL, MXR
      
      PARAMETER (MXL=21,MXI=83,MXR=918)
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
     >   WRITE (JOPPRT,'(/'' IN BWEPPPT: ISTND,ICYC='',I7,I4)')
     >         ISTND,ICYC

C.... Load logical scalars into LOGICS array.
C
C---- from common BWEBOX ------------
      LOGICS(1)  = LTEMP1(1)
      LOGICS(2)  = LTEMP1(2)
      LOGICS(3)  = LTEMP1(3)
      LOGICS(4)  = LTEMP1(4)
      LOGICS(5)  = LP1      
      LOGICS(6)  = LP2      
      LOGICS(7)  = LP3      
      LOGICS(8)  = LP4      
      LOGICS(9)  = LP5      
      LOGICS(10) = LP6      
      LOGICS(11) = LP7      
      LOGICS(12) = LP8      
C---- from common BWECM2 ------------
      LOGICS(13) = LBUDL    
      LOGICS(14) = LFIRST   
      LOGICS(15) = LREGO    
      LOGICS(16) = LSPRAY   
C---- from common RDCOM ------------
      LOGICS(17) = LBWDAM   
      LOGICS(18) = LBWPDM   
      LOGICS(19) = LCALBW   
      LOGICS(20) = LDEFOL   
      LOGICS(21) = LTOPK    

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: LOGICS=',
     >            LOGICS

C     Write logical scalars to buffer.
C
      CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, K)

C.... Load integer scalars into INTS array.
C
C---- from common BWEBOX ------------
      INTS(1)  = IBUDYR     
      INTS(2)  = IBWOPT     
      INTS(3)  = IDEFPR     
      INTS(4)  = IDEFSP(1)  
      INTS(5)  = IDEFSP(2)  
      INTS(6)  = IDEFSP(3)  
      INTS(7)  = IDEFSP(4)  
      INTS(8)  = IDEFSP(5)  
      INTS(9)  = IFLAG      
      INTS(10) = IOBACT     
      INTS(11) = IOBSCH(1,1)
      INTS(12) = IOBSCH(1,2)
      INTS(13) = IOBSCH(2,1)
      INTS(14) = IOBSCH(2,2)
      INTS(15) = IOBSCH(3,1)
      INTS(16) = IOBSCH(3,2)
      INTS(17) = IPARA      
      INTS(18) = IQUALD     
      INTS(19) = IQUALW     
      INTS(20) = ISTN       
      INTS(21) = ISTNUM     
      INTS(22) = ITEMP(1)   
      INTS(23) = ITEMP(2)   
      INTS(24) = ITEMP(3)   
      INTS(25) = ITEMP(4)   
      INTS(26) = ITEMP(5)   
      INTS(27) = IWSRC      
      INTS(28) = JOWE       
      INTS(29) = NEVENT     
      INTS(30) = NOBDON     
      INTS(31) = NOBSCH     
      INTS(32) = NUMCOL     
C---- from common BWECM2 ------------
      INTS(33) = ILOBYR     
      INTS(34) = INSTSP
      INTS(35) = IOBDUR     
      INTS(36) = IOBLOC     
      INTS(37) = IOBOPT     
      INTS(38) = ISPRAY     
      INTS(39) = ISPVAR     
      INTS(40) = ISPYR(1)
      INTS(41) = ISPYR(2)
      INTS(42) = ISPYR(3)
      INTS(43) = ISPYR(4)
      INTS(44) = ISPYR(5)
      INTS(45) = IWOPT      
      INTS(46) = IWYR       
      INTS(47) = IYRCNT
      INTS(48) = IYRECV     
      INTS(49) = IYREND     
      INTS(50) = IYRNG(1)
      INTS(51) = IYRNG(2)
      INTS(52) = IYROBL     
      INTS(53) = IYRSRC     
      INTS(54) = IYRST      
      INTS(55) = JOBWP1     
      INTS(56) = JOBWP2     
      INTS(57) = JOBWP3     
      INTS(58) = JOBWP4     
      INTS(59) = JOBWP5     
      INTS(60) = JOBWP6     
      INTS(61) = JOBWP7     
      INTS(62) = JOBWP8     
      INTS(63) = KRECVR     
      INTS(64) = LIMITS     
      INTS(65) = LOWYRS     
      INTS(66) = NSPRAY     
      INTS(67) = NUMAPP     
C---- from common BWECOM ------------
      INTS(68) = IBWYR1     
      INTS(69) = IBWYR2     
      INTS(70) = ICUMYR     
      INTS(71) = IPRBYR     
      INTS(72) = ITODO      
      INTS(73) = IYRCUR     
      INTS(74) = JOWSBW     
      INTS(75) = NCUMYR     
      INTS(76) = NTODO      
C---- from common BWESTD ------------
      INTS(77) = IFHOST(1)  
      INTS(78) = IFHOST(2)  
      INTS(79) = IFHOST(3)  
      INTS(80) = IFHOST(4)  
      INTS(81) = IFHOST(5)  
      INTS(82) = IFHOST(6)  
      INTS(83) = IFHOST(7)  

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: INTS=',
     >            INTS

C.... Write integer scalars to buffer.
C
      CALL IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, K)

C.... Write larger integer arrays from buffer
C
C     Note on processing arrays:
C     When handling a 2-dimensional array, consider a string of values a
C     complete column of the array and the length of the array as the 
C     number of rows. So, a call to IFREAD or BFREAD will process 1 column
C     of the specified number of rows (length) in a 2-dimensional array.
C
C     CALL IFWRIT (WK3,IPNT,ILIMIT, array, length, K)
C     .....................................................................

      DO 20 I = 1, 5
      CALL IFWRIT (WK3, IPNT, ILIMIT, IDEF(1,I),        100, K) !(100,5)
   20 CONTINUE

      DO 22 I = 1, 5
      CALL IFWRIT (WK3, IPNT, ILIMIT, IEVENT(1,I),      250, K) !(250,5)
   22 CONTINUE

C.... Load real scalars then arrays into the REALS array.
C     Scalars and small one-dimensional arrays are handled first
C     for ease of indexing. 
C
      REALS(1)  = RAINDM    
      REALS(2)  = RAINDS    
      REALS(3)  = RAINM(1)  
      REALS(4)  = RAINM(2)  
      REALS(5)  = RAINM(3)  
      REALS(6)  = RAINS(1)  
      REALS(7)  = RAINS(2)  
      REALS(8)  = RAINS(3)  
      REALS(9)  = WHOTM     
      REALS(10) = WHOTSD    
      REALS(11) = DISPMR    
      REALS(12) = EGGS      
      REALS(13) = OLDMAX    
      REALS(14) = TREEDD    
      REALS(15) = WCOLDW    
      REALS(16) = WHOTF     
      REALS(17) = WRAIND    
      REALS(18) = DSEEDR    
      REALS(19) = OBSEER    
      REALS(20) = WSEEDR    
      REALS(21) = DEFLYR    
      REALS(22) = HOSTST    
      REALS(23) = SPEFF     
      REALS(24) = TRIGGR    
      REALS(25) = WRAIN1(1) 
      REALS(26) = WRAIN1(2) 
      REALS(27) = WRAIN1(3) 
      REALS(28) = WRAIN2(1) 
      REALS(29) = WRAIN2(2) 
      REALS(30) = WRAIN2(3) 
      REALS(31) = WRAIN3(1) 
      REALS(32) = WRAIN3(2) 
      REALS(33) = WRAIN3(3) 
      REALS(34) = WRAINA(1) 
      REALS(35) = WRAINA(1) 
      REALS(36) = WRAINA(3) 
      REALS(37) = WRAINB(1) 
      REALS(38) = WRAINB(2) 
      REALS(39) = WRAINB(3) 

      IIX = 39
      DO 30 IIA = 1,4
        DO 30 IIB = 1,3
          IIX = IIX + 1
          REALS(IIX) = NEMULT(IIA,IIB)
   30 CONTINUE
C.... IIX at 51
      DO 32 IIA = 1,9
        DO 32 IIB = 1,6
          IIX = IIX + 1
          REALS(IIX) = BW(IIA,IIB)
   32 CONTINUE
C.... IIX at 105
      DO 34 IIA = 1,6
        IIX = IIX + 1
        REALS(IIX) = DEFYRS(IIA)
   34 CONTINUE
C.... IIX ends at 111


      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 1 - REALS=',
     >            REALS

C.... Write real varaibles (scalars and arrays) to buffer.

      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, 111, K)

C.... Load next real array(s) into REALS.

      IIX = 0
      DO 36 IIA = 1,9
        DO IIB = 1,6
          DO IIC = 1,17
            IIX = IIX + 1
            REALS(IIX) = OUT1(IIA,IIB,IIC)
          END DO
        END DO
   36 CONTINUE

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 2 - REALS=',
     >            REALS

C.... Write next real array(s) to buffer.

      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, 918, K)

C.... Load next real array(s) into REALS.

      IIX = 0
      DO 38 IIA = 1,6
        DO IIB = 1,3
          DO IIC = 1,8
            IIX = IIX + 1
            REALS(IIX) = OUT2(IIA,IIB,IIC)
          END DO
        END DO
   38 CONTINUE

C.... Write next real array(s) to buffer.

      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, 144, K)

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 3 - REALS=',
     >            REALS

C.... Load next real array(s) to REALS.
C.... Due to the size of this array, it is done in 2 parts,
C.... Third dimension (IIC) 1-10, part 1.
      IIX = 0
      DO 40 IIA = 1,9
        DO IIB = 1,6
          DO IIC = 1,10
            IIX = IIX + 1
            REALS(IIX) = OUT3(IIA,IIB,IIC)
          END DO
        END DO
   40 CONTINUE

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 4A - REALS=',
     >            REALS

C.... Write next real array(s) to buffer.

      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)


C.... Load next real array(s) to REALS.
C.... Due to the size of this array, it is done in 2 parts,
C.... Third dimension (IIC) 11-20, part 2.
      IIX = 0
      DO 41 IIA = 1,9
        DO IIB = 1,6
          DO IIC = 11,20
            IIX = IIX + 1
            REALS(IIX) = OUT3(IIA,IIB,IIC)
          END DO
        END DO
   41 CONTINUE

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 4B - REALS=',
     >            REALS

C.... Write next real array(s) to buffer.

      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, IIX, K)

C.... Load next real array(s) into REALS.

      IIX = 0
      DO 42 IIA = 1,6
        DO 42 IIB = 1,3
          DO 42 IIC = 1,2
            DO 42 IID = 1,5
              IIX = IIX + 1
              REALS(IIX) = APRBYR(IIA,IIB,IIC,IID)
   42 CONTINUE
C.... IIX at 180
      DO 44 IIA = 1,6
        DO 44 IIB = 1,3
          IIX = IIX + 1
          REALS(IIX) = AVYRMX(IIA,IIB)
   44 CONTINUE
C.... IIX at 198
      DO 46 IIA = 1,6
        DO 46 IIB = 1,3
          IIX = IIX + 1
          REALS(IIX) = BWMXCD(IIA,IIB)
   46 CONTINUE
C.... IIX at 216
      DO 48 IIA = 1,7
        DO 48 IIB = 1,3
          IIX = IIX + 1
          REALS(IIX) = BWTPHA(IIA,IIB)
   48 CONTINUE
C.... IIX at 237
      DO 50 IIA = 1,6
        DO 50 IIB = 1,3
          IIX = IIX + 1
          REALS(IIX) = CDEF(IIA,IIB)
   50 CONTINUE
C.... IIX at 255
      DO 52 IIA = 1,6
        DO 52 IIB = 1,3
          DO 52 IIC = 1,5
            IIX = IIX + 1
            REALS(IIX) = CUMDEF(IIA,IIB,IIC)
   52 CONTINUE
C.... IIX at 345
      DO 54 IIA = 1,9
        DO 54 IIB = 1,6
          IIX = IIX + 1
          REALS(IIX) = FNEW(IIA,IIB)
   54 CONTINUE
C.... IIX ends at 399
C.... Write next real array(s) to buffer.

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 5 - REALS=',
     >            REALS

      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, 399, K)

C.... Load next real array(s) to REALS.

      IIX = 0
      DO 56 IIA = 1,6
        DO 56 IIB = 1,9
          DO 56 IIC = 1,4
            IIX = IIX + 1
            REALS(IIX) = FOLADJ(IIA,IIB,IIC)
   56 CONTINUE
C.... IIX at 216
      DO 58 IIA = 1,9
        DO 58 IIB = 1,6
          IIX = IIX + 1
          REALS(IIX) = FOLD1(IIA,IIB)
   58 CONTINUE
C.... IIX at 270
      DO 60 IIA = 1,9
        DO 60 IIB = 1,6
          IIX = IIX + 1
          REALS(IIX) = FOLD2(IIA,IIB)
   60 CONTINUE
C.... IIX at 324
      DO 62 IIA = 1,9
        IIX = IIX + 1
        REALS(IIX) = FOLNH(IIA)
   62 CONTINUE
C.... IIX ends at 333

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 6 - REALS=',
     >            REALS

C.... Write next real array(s) from buffer.

      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, 333, K)

C.... Load next real array(s) into REALS.

      IIX = 0
      DO 64 IIA = 1,6
        DO 64 IIB = 1,9
          DO 64 IIC = 1,4
          IIX = IIX + 1
          REALS(IIX) = FOLPOT(IIA,IIB,IIC)
   64 CONTINUE
C.... IIX at 216
      DO 66 IIA = 1,9
        DO 66 IIB = 1,6
          IIX = IIX + 1
          REALS(IIX) = FREM(IIA,IIB)
   66 CONTINUE
C.... IIX at 270
      DO 68 IIA = 1,6
        DO 68 IIB = 1,3
          IIX = IIX + 1
          REALS(IIX) = PEDDS(IIA,IIB)
   68 CONTINUE
C.... IIX at 288
      DO 70 IIA = 1,6
        DO 70 IIB = 1,3
          IIX = IIX + 1
          REALS(IIX) = PEHTG(IIA,IIB)
   70 CONTINUE
C.... IIX at 306
      DO 72 IIA = 1,6
        DO 72 IIB = 1,9
          DO 72 IIC = 1,4
            IIX = IIX + 1
            REALS(IIX) = POFPOT(IIA,IIB,IIC)
   72 CONTINUE
C.... IIX ends at 522

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 7 - REALS=',
     >            REALS

C.... Write next real array(s) to buffer.

      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, 522, K)


C.... Load next real array(s) into REALS.

      IIX = 0
      DO 74 IIA = 1,6
        DO 74 IIB = 1,9
          DO 74 IIC = 1,4
            IIX = IIX + 1
            REALS(IIX) = PRBIO(IIA,IIB,IIC)
   74 CONTINUE
C.... IIX at 216
      DO 76 IIA = 1,6
        DO 76 IIB = 1,3
          IIX = IIX + 1
          REALS(IIX) = RDDSM1(IIA,IIB)
   76 CONTINUE
C.... IIX at 234
      DO 78 IIA = 1,6
        DO 78 IIB = 1,3
          IIX = IIX + 1
          REALS(IIX) = RHTGM1(IIA,IIB)
   78 CONTINUE
C.... IIX at 252
      DO 80 IIA = 1,9
        DO 80 IIB = 1,6
          IIX = IIX + 1
            REALS(IIX) = ACTNEW(IIA,IIB)
   80 CONTINUE
C.... IIX ends at 306

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 8 - REALS=',
     >            REALS

C.... Write next real array(s) to buffer.

      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, 306, K)


C.... Load next real array(s) into REALS.

      IIX = 0

      DO 82 IIA = 1,9
        DO 82 IIB = 1,6
          IIX = IIX + 1
          REALS(IIX) = ANTDEN(IIA,IIB)
   82 CONTINUE
C.... IIX at 54
      DO 84 IIA = 1,9
        DO 84 IIB = 1,6
          IIX = IIX + 1
          REALS(IIX) = BIRDEN(IIA,IIB)
   84 CONTINUE
C.... IIX at 108
      DO 86 IIA = 1,9
        DO 86 IIB = 1,6
          IIX = IIX + 1
          REALS(IIX) = MYS1DN(IIA,IIB)
   86 CONTINUE
C.... IIX at 162
      DO 88 IIA = 1,9
        DO 88 IIB = 1,6
          IIX = IIX + 1
          REALS(IIX) = MYS2DN(IIA,IIB)
   88 CONTINUE
C.... IIX at 216
      DO 90 IIA = 1,9
        DO 90 IIB = 1,6
          IIX = IIX + 1
          REALS(IIX) = MYS3DN(IIA,IIB)
   90 CONTINUE
C.... IIX at 270
      DO 92 IIA = 1,4
        IIX = IIX + 1
        REALS(IIX) = FOLDVY(IIA)
   92 CONTINUE
C.... IIX at 274
      DO 94 IIA = 1,4
        IIX = IIX + 1
        REALS(IIX) = FOLWTY(IIA)
   94 CONTINUE
C.... IIX at 278
      DO 96 IIA = 1,10
        IIX = IIX + 1
        REALS(IIX) = SPEFFS(IIA)
   96 CONTINUE
C.... IIX at 288
      DO 98 IIA = 1,10
        IIX = IIX + 1
        REALS(IIX) = SPINST(IIA)
   98 CONTINUE
C.... IIX ends at 298

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 9 - REALS=',
     >            REALS

C.... Write next real array(s) to buffer.

      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, 298, K)

C.... Load next real array(s) into REALS.

      IIX = 0

      DO 100 IIA = 1,11
        DO 100 IIB = 1,50
          IIX = IIX + 1
          REALS(IIX) = BWPRMS(IIA,IIB)
  100 CONTINUE
C.... IIX ends at 550

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 10 - REALS=',
     >            REALS

C.... Write next real array(s) to buffer.

      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, 550, K)

C
C.... Write double precision scalars (random number seeds) to buffer.
C     SEEDA is DSEEDD, SEEDB is OBSEED, SEEDC is WSEED

      IF (PDEBUG) WRITE (JOPPRT,*) 'IN BWEPPPT: 10 - SEEDA=',SEEDA,
     >            ' SEEDB=',SEEDB,' SEEDC=',SEEDC

      CALL BFWRIT (WK3, IPNT, ILIMIT, SEEDA, 2, K)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SEEDB, 2, K)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SEEDC, 2, K)

      GOTO 210
C     These exist for control of numeric or character processing
C     specified by subroutine parameter IB.
  150 CONTINUE

C
C.... Write character variables to buffer.
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
C     CHARACTER*8 TABLE(25)  -- Static

      K=1
      DO 200 I=1,4
        CALL CHWRIT(CBUFF,IPNT,LNCBUF,MGMIDB(I:I), K)
        K=2
  200 CONTINUE

      DO 202 I=1,8
        CALL CHWRIT(CBUFF,IPNT,LNCBUF,DEFLAB(I:I), K)
  202 CONTINUE

      DO 204 J=1,5
        DO 204 I=1,8
          CALL CHWRIT(CBUFF,IPNT,LNCBUF,DLABS(J)(I:I), K)
  204 CONTINUE

      DO 206 I=1,72
        CALL CHWRIT(CBUFF,IPNT,LNCBUF,ITITLB(I:I), K)
  206 CONTINUE

      DO 208 J=1,3
        DO 208 I=1,3
          IF (I .EQ. 3) K=3
          CALL CHWRIT(CBUFF,IPNT,LNCBUF,IOUT6A(J)(I:I), K)
  208 CONTINUE

  210 CONTINUE
      IF (PDEBUG)
     >    WRITE (JOPPRT,'(/'' IN BWEPPPT: END OF ISTND,ICYC='',I7,I4)')
     >          ISTND,ICYC

      RETURN
      END



