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
      PARAMETER (MXI=41)

      INTEGER INTS(MXI), ILIMIT, IPNT
      REAL    WK3(*)

      INTS(  1) = ISUMARY
      INTS(  2) = ICOMPUTE
      INTS(  3) = ITREELIST
      INTS(  4) = IPOTFIRE
      INTS(  5) = IFUELS
      INTS(  6) = ITREEIN
      INTS(  7) = ICUTLIST
      INTS(  8) = IDM1
      INTS(  9) = IDM2
      INTS( 10) = IDM3
      INTS( 11) = IDM5
      INTS( 12) = IDM6
      INTS( 13) = IFUELC
      INTS( 14) = IBURN
      INTS( 15) = IMORTF
      INTS( 16) = ISSUM
      INTS( 17) = ISDET
      INTS( 18) = ISTRCLAS
      INTS( 19) = IBMMAIN
      INTS( 20) = IBMBKP
      INTS( 21) = IBMTREE
      INTS( 22) = IBMVOL
      INTS( 23) = IDBSECON
      INTS( 24) = ISPOUT6
      INTS( 25) = ISPOUT17
      INTS( 26) = ISPOUT21
      INTS( 27) = ISPOUT23
      INTS( 28) = ISPOUT30
      INTS( 29) = ISPOUT31
      INTS( 30) = IATRTLIST
      INTS( 31) = I_CMPU
      INTS( 32) = IADDCMPU
      INTS( 33) = ICMRPT
      INTS( 34) = ICHRPT
      INTS( 35) = ICANPR
      INTS( 36) = IRGIN
      INTS( 37) = IDWDVOL
      INTS( 38) = IDWDCOV
      INTS( 39) = 0
      INTS( 40) = 0
      IF (ConnHndlIn .NE.-1) INTS( 39)=1 ! The connection was openned
      IF (ConnHndlOut.NE.-1) INTS( 40)=1
      INTS( 41) = ICLIM
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


