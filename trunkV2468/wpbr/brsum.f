      SUBROUTINE BRSUM
      IMPLICIT NONE
C**********************************************************************
C  **BRSUM        DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C  Loads summary data into the summary array for both the
C  Blister Rust Stand Summary Table and the Blister Rust 2" DBH Class
C  Summary Table.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  20-APR-1999 Lance David
C     A upper limit of 0.99 is imposed on variable PITCA in subroutine
C     BRSTAT and then 0.99 changed to 1.0 for reporting. There was no
C     explanation why, so I removed this value change.
C     0.99 is the value used and reported.
C  15-MAR-2001 Lance R. David (FHTET)
C     Stand Deviation Factor (DFACT) is now species and stock type
C     specific.
C     * * * * The reporting of the values has not yet been handled.* * * *
C  01-MAY-2001 Lance R. David (FHTET)
C     Expanded BROUT array by species dimension.
C     * * * * 2 inch diameter class not yet addressed. * * * *
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM. Instead of just being and indicator of a
C     species being a host, BRSPM holds the array index value for that
C     species and is used to access all species-specific BR arrays.
C  11-MAY-2001 Lance R. David (FHTET)
C     Changed summary table total to reflect current Live + historical
C     mortality.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'

      INTEGER I3, I4, IBYR, IBBYR, II

C.... Get current year.

      IBYR=ICYC+1
      IBBYR=IY(IBYR)

C.... BROUT holds the information for the stand summary table.
      DO 20 I3 = 1, MAXSP

         IF(BRSPM(I3) .EQ. 0) GO TO 20

C....    Set blister ruswt species index
         I4=BRSPM(I3)

         BROUT(I4,01,IBYR)=IBBYR
         BROUT(I4,02,IBYR)=AVGGI(I4)/.3048
         BROUT(I4,03,IBYR)=AVGRI(I4)
         BROUT(I4,04,IBYR)=AVGSTS(I4)
         BROUT(I4,05,IBYR)=DFACT(I4,1)
         BROUT(I4,06,IBYR)=AVTCPT(I4)
         BROUT(I4,07,IBYR)=AVLCPT(I4)
         BROUT(I4,08,IBYR)=AVECPT(I4)
C        PPITCA=PITCA
C        IF(PPITCA.EQ.0.99) PPITCA=1.0
         BROUT(I4,09,IBYR)=PITCA(I4)
         BROUT(I4,10,IBYR)=PILCA(I4)
         BROUT(I4,11,IBYR)=TBRCLN(I4)
         BROUT(I4,12,IBYR)=TBRNOL(I4)
         BROUT(I4,13,IBYR)=TBRPRN(I4)
         BROUT(I4,14,IBYR)=TBREXC(I4)
         BROUT(I4,15,IBYR)=TBRNOS(I4)
         BROUT(I4,16,IBYR)=TBRGIR(I4)
         BROUT(I4,17,IBYR)=TBRMRT(I4)
         BROUT(I4,18,IBYR)=THPROB(I4)+TBRHMR(I4)

C....    Set logical variable so that tables full of zeros are not written.
         IF(THPROB(I4) .NE. 0.0) LBROUT(I4)=.TRUE.

   20 CONTINUE

C.... BRDBHO holds the information for the 2" DBH Class table.
C.... In this table, for YEAR, use BROUT(sp,01,IBYR).

      DO 30 II=1,10
         BRDBHO(II,01,IBYR)=D2AVT(II)
         BRDBHO(II,02,IBYR)=D2AVL(II)
         BRDBHO(II,03,IBYR)=D2AVE(II)
         BRDBHO(II,04,IBYR)=D2PIT(II)
         BRDBHO(II,05,IBYR)=D2PIL(II)
         BRDBHO(II,06,IBYR)=D2CLN(II)
         BRDBHO(II,07,IBYR)=D2NOL(II)
         BRDBHO(II,08,IBYR)=D2PRN(II)
         BRDBHO(II,09,IBYR)=D2EXC(II)
         BRDBHO(II,10,IBYR)=D2NOS(II)
         BRDBHO(II,11,IBYR)=D2GIR(II)
         BRDBHO(II,12,IBYR)=D2DED(II)
         BRDBHO(II,13,IBYR)=D2WP(II)
   30 CONTINUE

C.... Common return.

      RETURN
      END
