      SUBROUTINE DBSPPPUT (WK3, IPNT, ILIMIT)
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
      INTEGER MXI,I
      PARAMETER (MXI=69)

      INTEGER INTS(MXI), ILIMIT, IPNT
      REAL    WK3(*)

      INTS(  1) = ICASE
      INTS(  2) = ISUMARY
      INTS(  3) = ICOMPUTE
      INTS(  4) = ITREELIST
      INTS(  5) = IPOTFIRE
      INTS(  6) = IFUELS
      INTS(  7) = ITREEIN
      INTS(  8) = ICUTLIST
      INTS(  9) = IDM1
      INTS( 10) = IDM2
      INTS( 11) = IDM3
      INTS( 12) = IDM5
      INTS( 13) = IDM6
      INTS( 14) = IFUELC
      INTS( 15) = IBURN
      INTS( 16) = IMORTF
      INTS( 17) = ISSUM
      INTS( 18) = ISDET
      INTS( 19) = ISTRCLAS
      INTS( 20) = IBMMAIN
      INTS( 21) = IBMBKP
      INTS( 22) = IBMTREE
      INTS( 23) = IBMVOL
      INTS( 24) = IDBSECON
      INTS( 25) = CMPUID
      INTS( 26) = SUMRYID
      INTS( 27) = TREEOUTID
      INTS( 28) = FMPFID
      INTS( 29) = FUELID
      INTS( 30) = CUTSID
      INTS( 31) = DM1ID
      INTS( 32) = DM2ID
      INTS( 33) = DM3ID
      INTS( 34) = DM5ID
      INTS( 35) = DM6ID
      INTS( 36) = CONID
      INTS( 37) = BURNID
      INTS( 38) = MORTID
      INTS( 39) = SSUMID
      INTS( 40) = SDETID
      INTS( 41) = STRCLID
      INTS( 42) = CMRPTID
      INTS( 43) = CHRPTID
      INTS( 44) = ISPOUT6
      INTS( 45) = ISPOUT17
      INTS( 46) = ISPOUT21
      INTS( 47) = ISPOUT23
      INTS( 48) = ISPOUT30
      INTS( 49) = ISPOUT31
      INTS( 50) = BM_MNID
      INTS( 51) = BMTREID
      INTS( 52) = BMVOLID
      INTS( 53) = BMBKPID
      INTS( 54) = IATRTLIST
      INTS( 55) = IATRTLID
      INTS( 56) = I_CMPU
      INTS( 57) = IADDCMPU
      INTS( 58) = ICMRPT
      INTS( 59) = ICHRPT
      INTS( 60) = ICANPR
      INTS( 61) = CANPRID
      INTS( 62) = IRGIN
      INTS( 63) = IDWDVOL
      INTS( 64) = IDWDCOV
      INTS( 65) = DWDVID
      INTS( 66) = DWDCID
      INTS( 67) = 0
      INTS( 68) = 0
      IF (ConnHndlIn .NE.-1) INTS( 67)=1 ! The connection was openned
      IF (ConnHndlOut.NE.-1) INTS( 68)=1
      INTS( 69) = ICLIM
C
      CALL IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)
      LENSTRINGS(1) = LEN_TRIM(DSNIN)
      LENSTRINGS(2) = LEN_TRIM(DSNOUT)
      LENSTRINGS(3) = LEN_TRIM(KEYFNAME)
      CALL IFWRIT (WK3, IPNT, ILIMIT, LENSTRINGS, 3, 2)
      RETURN
      END

      SUBROUTINE DBSCHPUT (CBUFF, IPNT, LNCBUF)
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
      INTEGER K,J,I,IPNT

      IF (LENSTRINGS(1).GT.0) THEN
        DO J=1,LENSTRINGS(1)
          CALL CHWRIT(CBUFF,IPNT,LNCBUF,DSNIN(J:J),2)
        ENDDO
      ENDIF

      IF (LENSTRINGS(2).GT.0) THEN
        DO J=1,LENSTRINGS(2)
          CALL CHWRIT(CBUFF,IPNT,LNCBUF,DSNOUT(J:J),2)
        ENDDO
      ENDIF

      IF (LENSTRINGS(3).GT.0) THEN
        DO J=1,LENSTRINGS(3)
          CALL CHWRIT(CBUFF,IPNT,LNCBUF,KEYFNAME(J:J),2)
        ENDDO
      ENDIF

      CALL DBSCLOSE(.TRUE.,.TRUE.)

      RETURN
      END


