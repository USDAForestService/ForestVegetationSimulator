      SUBROUTINE DBSPPGET (WK3, IPNT, ILIMIT)
      IMPLICIT NONE
C
C $Id$
C
COMMONS
C
C
      INCLUDE 'DBSCOM.F77'
C
C
COMMONS
C
C
      INTEGER MXI
      PARAMETER (MXI=69)

      INTEGER INTS(MXI), ILIMIT, IPNT
      REAL    WK3(*)

      CALL IFREAD(WK3, IPNT, ILIMIT, INTS, MXI, 2)
      ICASE        = INTS(  1)
      ISUMARY      = INTS(  2)
      ICOMPUTE     = INTS(  3)
      ITREELIST    = INTS(  4)
      IPOTFIRE     = INTS(  5)
      IFUELS       = INTS(  6)
      ITREEIN      = INTS(  7)
      ICUTLIST     = INTS(  8)
      IDM1         = INTS(  9)
      IDM2         = INTS( 10)
      IDM3         = INTS( 11)
      IDM5         = INTS( 12)
      IDM6         = INTS( 13)
      IFUELC       = INTS( 14)
      IBURN        = INTS( 15)
      IMORTF       = INTS( 16)
      ISSUM        = INTS( 17)
      ISDET        = INTS( 18)
      ISTRCLAS     = INTS( 19)
      IBMMAIN      = INTS( 20)
      IBMBKP       = INTS( 21)
      IBMTREE      = INTS( 22)
      IBMVOL       = INTS( 23)
      IDBSECON     = INTS( 24)
      CMPUID       = INTS( 25)
      SUMRYID      = INTS( 26)
      TREEOUTID    = INTS( 27)
      FMPFID       = INTS( 28)
      FUELID       = INTS( 29)
      CUTSID       = INTS( 30)
      DM1ID        = INTS( 31)
      DM2ID        = INTS( 32)
      DM3ID        = INTS( 33)
      DM5ID        = INTS( 34)
      DM6ID        = INTS( 35)
      CONID        = INTS( 36)
      BURNID       = INTS( 37)
      MORTID       = INTS( 38)
      SSUMID       = INTS( 39)
      SDETID       = INTS( 40)
      STRCLID      = INTS( 41)
      CMRPTID      = INTS( 42)
      CHRPTID      = INTS( 43)
      ISPOUT6      = INTS( 44)
      ISPOUT17     = INTS( 45)
      ISPOUT21     = INTS( 46)
      ISPOUT23     = INTS( 47)
      ISPOUT30     = INTS( 48)
      ISPOUT31     = INTS( 49)
      BM_MNID      = INTS( 50)
      BMTREID      = INTS( 51)
      BMVOLID      = INTS( 52)
      BMBKPID      = INTS( 53)
      IATRTLIST    = INTS( 54)
      IATRTLID     = INTS( 55)
      I_CMPU       = INTS( 56)
      IADDCMPU     = INTS( 57)
      ICMRPT       = INTS( 58)
      ICHRPT       = INTS( 59)
      ICANPR       = INTS( 60)
      CANPRID      = INTS( 61)
      IRGIN        = INTS( 62)
      IDWDVOL      = INTS( 63)
      IDWDCOV      = INTS( 64)
      DWDVID       = INTS( 65)
      DWDCID       = INTS( 66)
      ! note that ConnHndl[In,Out] could be long ints! If
      ! the ConnHndlIn values are positive, then the data
      ! bases are already opened. Make sure they are closed
      CALL DBSCLOSE(.TRUE.,.TRUE.)
      IF (INTS(67).EQ.1) ConnHndlIn = 0 ! signal to reopen
      IF (INTS(68).EQ.1) ConnHndlOut= 0
      ICLIM        = INTS( 69)

      CALL IFREAD(WK3, IPNT, ILIMIT, LENSTRINGS, 3, 2)
C
      RETURN
      END

      SUBROUTINE DBSCHGET (CBUFF, IPNT, LNCBUF)
      IMPLICIT NONE
C----------
C  **DBSPPPUT--DBS DATE OF LAST REVISION: 10/31/2011
C----------
C
C
COMMONS
C
C
      INCLUDE 'DBSCOM.F77'
C
C
COMMONS
C
C
      INTEGER LNCBUF
      CHARACTER CBUFF(LNCBUF)
      INTEGER K,J,I,IPNT,KODE
      IF (LENSTRINGS(1).GT.0) THEN
        DO J=1,LENSTRINGS(1)
          CALL CHREAD(CBUFF,IPNT,LNCBUF,DSNIN(J:J),2)
        ENDDO
      ENDIF

      IF (LENSTRINGS(1).GT.0) THEN
        DO J=1,LENSTRINGS(2)
          CALL CHREAD(CBUFF,IPNT,LNCBUF,DSNOUT(J:J),2)
        ENDDO
      ENDIF

      IF (LENSTRINGS(1).GT.0) THEN
        DO J=1,LENSTRINGS(3)
          CALL CHREAD(CBUFF,IPNT,LNCBUF,KEYFNAME(J:J),2)
        ENDDO
      ENDIF

      ! reopen connections that were in use.

      IF (ConnHndlIn.EQ.0) THEN
        ConnHndlIn = -1
        CALL DBSOPEN(DSNIN,EnvHndlIn,ConnHndlIn,DBMSIN,0,
     -     .FALSE.,KODE)
        IF (KODE.EQ.0) PRINT *,"Reopen DBSIN failed. DSNIN=",
     -     DBMSIN(:LEN_TRIM(DBMSIN))
      ENDIF
      IF (ConnHndlOut.EQ.0) THEN
        ConnHndlOut = -1
        CALL DBSOPEN(DSNOUT,EnvHndlOut,ConnHndlOut,DBMSOUT,0,
     -     .FALSE.,KODE)
        IF (KODE.EQ.0) PRINT *,"Reopen DBMSOUT failed. DSNOUT=",
     -     DBMSIN(:LEN_TRIM(DBMSIN))
      ENDIF

      RETURN
      END

