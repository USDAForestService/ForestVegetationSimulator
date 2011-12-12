      SUBROUTINE GRDTIM (DAT,TIM)
      IMPLICIT NONE
C----------
C  **GRDTIM--BASE  DATE OF LAST REVISION: 07/23/08
C----------
C
C     THE TIME AND DATE STAMPS FOR THE PROGNOSIS HEADING.
C----------
      CHARACTER DAT*10,TIM*8
C***  CHARACTER TIME1*10
C**   CHARACTER UDAT*11,UTIM*9
C**   CHARACTER CFCDAT*8,CFCTIM*8
C**   CHARACTER LAHDAT*8
      CHARACTER IBMDAT*8,IBMTIM*10,IBMZ*5
      INTEGER IBMDT(8)
      INTEGER IDATIM(8),IDAT(3),ITIM(3)
      EQUIVALENCE (IDAT,IDATIM),(ITIM,IDATIM(4))

C----------
C     DUMMY DATE AND TIME RETURNS BLANK FIELDS.
C----------
      DAT=' '
      TIM=' '
C----------
C     CALL THE DATE AND TIME ROUTINE.  IBM FORTRAN VERSION 2.
C----------
C*    CALL DATIM (IDATIM)
C*    WRITE (DAT,'(I2.2,''-'',I2.2,''-'',I4.4)')
C*   >             IDATIM(7),IDATIM(6),IDATIM(8)
C*    WRITE (TIM,'(I2.2,'':'',I2.2,'':'',I2.2)')
C*   >             IDATIM(5),IDATIM(4),IDATIM(3)
C----------
C     CALL THE DATE AND TIME ROUTINES.  DG VERSION.
C----------
C*    CALL DATE (IDAT)
C*    CALL TIME (ITIM)
C*    WRITE (DAT,'(I2.2,''-'',I2.2,''-'',I4.4)')
C*   >             IDAT(2),  IDAT(3),   IDAT(1)
C*    WRITE (TIM,'(I2.2,'':'',I2.2,'':'',I2.2)') ITIM
C----------
C  CALL DATE AND TIME ROUTINES--RYAN MCFARLAND PC VERSION.
C----------
C*    CALL GETDAT (IYEAR,IMONTH,IDAY)
C*    CALL GETTIM (IHOUR,IMIN,ISEC,IHUND)
C*    WRITE (DAT,'(I2.2,''-'',I2.2,''-'',I4.4)')
C*   >             IMONTH,    IDAY,      IYEAR
C*    WRITE (TIM,'(I2.2,'':'',I2.2,'':'',I2.2)')
C*   >             IHOUR,     IMIN,      ISEC
C----------
C     CALL THE DATE AND TIME ROUTINES.  LAHEY F77L VERSION.
C----------
C**   CALL DATE (LAHDAT)
C**   IF (LAHDAT(7:8).GE.'98') THEN
C**      DAT = LAHDAT(1:2)//'-'//LAHDAT(4:5)//'-19'//LAHDAT(7:8)
C**   ELSE
C**      DAT = LAHDAT(1:2)//'-'//LAHDAT(4:5)//'-20'//LAHDAT(7:8)
C**   ENDIF
C**   CALL TIME (TIM)
C
C----------
C     CALL THE DATE AND TIME ROUTINE.  LAHEY LF90 AND LF95 VERSIONS.
C----------
C***  CALL DATE_AND_TIME (LAHDAT,TIME1)
C***  IF (LAHDAT(3:4).GE.'98') THEN
C***  DAT = LAHDAT(5:6)//'-'//LAHDAT(7:8)//'-19'//LAHDAT(3:4)
C***  ELSE
C***  DAT = LAHDAT(5:6)//'-'//LAHDAT(7:8)//'-20'//LAHDAT(3:4)
C***  ENDIF
C***  TIM= TIME1(1:2)//':'//TIME1(3:4)//':'//TIME1(5:6)
C
C----------
C  CALL DATE AND TIME ROUTINES--MICROSOFT PC VERSION.
C----------
C     CALL GETDAT (IYEAR,IMONTH,IDAY)
C     CALL GETTIM (IHOUR,IMIN,ISEC,IHUND)
C     WRITE (DAT, '(I2,''-'',I2,''-'',I4)')
C    >              IMONTH, IDAY, IYEAR
C     WRITE (TIM, '(I2,'':'',I2,'':'',I2)')
C    >               IHOUR, IMIN,  ISEC
C----------
C  CALL DATE AND TIME ROUTINES -- xlf (IBM rs6000) VERSION
C----------
      CALL DATE_AND_TIME (IBMDAT,IBMTIM,IBMZ,IBMDT)
      WRITE (DAT,'(I2.2,''-'',I2.2,''-'',I4.4)')
     >             IBMDT(2),IBMDT(3),IBMDT(1) 
      WRITE (TIM, '(I2.2,'':'',I2.2,'':'',I2.2)')
     >               IBMDT(5),IBMDT(6),IBMDT(7)
      RETURN
      END
