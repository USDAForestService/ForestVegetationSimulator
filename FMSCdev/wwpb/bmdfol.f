      SUBROUTINE BMDFOL (ISTD,IYR)
C----------
C WWPB $Id$
C----------
c     CALLED FROM BMDRV
***********************************************************************
* **BMDFOL    Date of last revision:  June 14, 1994
*
*  Definitions:  
*     ITYP:   Type of tree: 1=host (PB host), 2=non-host
*
*  Common block variables and parameters:
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.
      INCLUDE 'PPCNTL.F77'
      INCLUDE 'BMCOM.F77'

C.... Variable declarations.
                         
      INTEGER ISIZ, MINSIZ, MAXSIZ
      INTEGER DUM(1)
      LOGICAL LOK
      REAL    PATTCK
      REAL    PRMS(4)

      SAVE
      
      IF(LBMDEB) WRITE(JBMBPR,10) IYR, ISTD
   10 FORMAT(' Begin BMDFOL: Year= ',I5, 'Stand= ', I6)

C     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.   
   
      IF (ICNT .GE. BMEND) ICNT = 0      
      ICNT = ICNT + 1

C     FETCH THE MOST RECENTLY SCEHEDULED UNTRIGGERED ACTIVITY. IF THERE
C     ARE ANY SET FOR THIS YEAR (IYR), THEN THEY TAKE OVER ANY CURRENT
C     SET OF PARAMETERS.

      IF (ICNT .EQ. 1) THEN

        IYR1= IYR
        NPRMS= 4

        CALL GPGET2 (314, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

        IF (LOK) THEN
  
          IYR2= IYR1 + IFIX(PRMS(1)) - 1

          PATTCK = PRMS(2)
          MINSIZ = INT(PRMS(3))
          MAXSIZ = INT(PRMS(4))

          IF (LBMDEB) WRITE (JBMBPR,101) IYR1,PATTCK,MINSIZ,MAXSIZ
  101     FORMAT (/' IN BMDFOL: IYR1=',I5,' PATTCK=',F5.3,
     &       ' MINSIZE=',I4,' MAXSIZE=',I4)
  
          IF (IYR2.GE.MIY(MICYC)) THEN
            PRMS(1) = IYR2 - MIY(MICYC) + 1
            CALL GPADD (KODE, MIY(MICYC), 314, NPRMS, PRMS, 1, DUM) 
            IYR2 = MIY(MICYC) - 1
            IF (LBMDEB) WRITE (JBMBPR,103) PRMS(1),IYR2
  103       FORMAT (/' IN BMDFOL: DEFOLIATORS ARE SCHEDULED FOR ',
     >        'THE NEXT MASTER CYCLE. DURATION WILL BE =',F5.0,
     >        '  NEW IYR2=',I5)     
          ENDIF
        ELSE  
C         Zero out the attact rate to signify that defoliators are not active this year.
          IF (IYR .GT. IYR2) PATTCK= 0.0
        ENDIF 
      ENDIF

      DO 30 ISIZ= 1,NSCL
        RVDFOL(ISTD,ISIZ) = 1.0
   30 CONTINUE        

C     LEAVE ROUTINE IF THERE ARE NO TREES ATTACKED.
      
      IF (PATTCK .LE. 0.0) RETURN


C     RV FROM DEFOLIATORS IS SIMPLY 1 - PROP. TREES > 70% ATTACKED (user defined)

      DO 50 ISIZ= MINSIZ,MAXSIZ
        RVDFOL(ISTD,ISIZ) = 1.0 - PATTCK
   50 CONTINUE        
 

      IF(LBMDEB) WRITE(JBMBPR,99) IYR, ISTD
   99 FORMAT(' End BMDFOL: Year= ',I5, 'Stand= ', I6)
      
      RETURN
      END
