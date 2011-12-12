      SUBROUTINE EXBM
      IMPLICIT NONE
C----------
C  **EXBM--BASE   DATE OF LAST REVISION:  07/23/08
C----------
C
C     EXTERNAL REFERENCES FOR THE Westwide Pine Beetle model
C
C----------
C
      INTEGER I3(6),JOPPRT,IRECNT,IREAD,I2,I1,KEY,IOUT,I
C
C
      CHARACTER*8 NOBM,KEYWRD
      LOGICAL     LACTV,LKECHO,LTEE
      REAL R1(*),XBM1,XBM2
C
      DATA NOBM/'**NO BM '/
C----------
C BMSDIT CALLED FROM PPMAIN.
C----------
      ENTRY BMSDIT
      RETURN
C----------
C BMKILL CALLED FROM PPMAIN
C----------
      ENTRY BMKILL
      RETURN
C----------
C BMSETP CALLED FROM ALSTD1
C----------
      ENTRY BMSETP
      RETURN
C----------
C BMDRV CALLED FROM ALSTD2
C----------
      ENTRY BMDRV
      RETURN
C----------
C BMPPIN CALLED FROM PPIN
C----------
      ENTRY BMPPIN(IREAD,IRECNT,JOPPRT)
      WRITE(JOPPRT,10) IRECNT,NOBM
      CALL ERRGRO (.TRUE.,11)
   10 FORMAT (/1X,A8,'RECORD: ', I6, A8)
      RETURN
C----------
C BMSLSH CALLED FROM CUTS
C----------
      ENTRY BMSLSH (I1,XBM1,XBM2,I2)
      RETURN
C----------
C BMTRIP CALLED FROM TRIPLE
C----------
      ENTRY BMTRIP (I1,I2,XBM1)
      RETURN
C----------
C BMKEY CALLED FROM OPLIST
C----------
      ENTRY BMKEY (KEY,KEYWRD)
      KEYWRD=NOBM
      RETURN
C----------
C BMPPPT CALLED FROM PUTSTD
C----------
      ENTRY BMPPPT (I1,I2)
      RETURN
C----------
C BMPPGT CALLED FROM GETSTD
C----------
      ENTRY BMPPGT (I1,I2)
      RETURN
C----------
C BMDAM  CALLED FROM DAMCDS
C----------
      ENTRY BMDAM (I1,I3)
      RETURN
C
C----------
C BMIN  CALLED FROM INITRE
C----------
      ENTRY BMIN(LKECHO)
      WRITE(JOPPRT,20) IRECNT,NOBM
      CALL ERRGRO (.TRUE.,11)
   20 FORMAT (/1X,A8,'RECORD: ', I6, A8)
      RETURN
C----------
C BMLNKD  CALLED FROM DBSIN
C----------
      ENTRY BMLNKD(LACTV)
      LACTV = .FALSE.
      RETURN
C-----------
C BMDBS--CALLED FROM DBSIN
C
      ENTRY BMDBS(I,IOUT)
      RETURN
C-----------
C RRATV CALLED FROM GETSTD AND PUTSTD
C-----------
      ENTRY RRATV(LACTV,LTEE)
      LACTV=.FALSE.
      LTEE=.FALSE.
      RETURN
C-----------
C RRPPPT CALLED FROM PUTSTD
C-----------
      ENTRY RRPPPT(R1,I1,I2)
      RETURN
C-----------
C RRPPGT CALLED FROM GETSTD
C-----------
      ENTRY RRPPGT(R1,I1,I2)
      RETURN
C-----------
      END
