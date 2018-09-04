      SUBROUTINE BMQMRT (ISTD,IYR)
C----------
C WWPB $Id$
C----------
***********************************************************************
*
*     This routine simly calculates extra mortality on host or non-host 
*     trees that is increasing each year of the period.  Note that this
*     is condsidered a "slow" mortality source and will never be more
*     than is predicted by FVS
*
*  CALLED FROM BMDRV
*
*  Definitions:  
*     ITYP:   Type of tree: 1=host (PB host), 2=non-host
*     MINSIZ: Minimum size class for mortality
*     MAXSIZ: Maximum size class for mortality
*     NYEAR:  Total number of years of extra mortality
*     R:      Temporary variable used in calcs of yearly mortality
*     SUM:    Variable needed in calcs of yearly mortality
*     TSURV:  Total mortality over the course of the period
*     YMORT:  Mortality in current year (proportion)
*
*  Common block variables and parameters:
*     NYR:    Number of years into the current extra mortality period
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

      LOGICAL LOK
      INTEGER ISIZ
      INTEGER ITYP
      INTEGER MINSIZ, MAXSIZ
      INTEGER NYEAR, NPRMS 
      INTEGER DUM(1)
      REAL    R
      REAL    SUM
      REAL    TSURV
      REAL    YMORT

      DIMENSION PRMS(6)

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
        NPRMS= 6 
        TSURV= 1.0 

        CALL GPGET2 (307, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

        IF (LOK) THEN
  
          IYR2= IYR1 + IFIX(PRMS(1)) - 1

          ITYP = INT(PRMS(2))
          MINSIZ = INT(PRMS(3))
          MAXSIZ = INT(PRMS(4))
          TSURV = 1 - PRMS(5)
          NYEAR = INT(PRMS(6))

          IF (LBMDEB) WRITE (JBMBPR,101) IYR1,NYEAR,MINSIZ,MAXSIZ,
     &                                   ITYP, TSURV          
  101     FORMAT (/' IN BMQMRT: IYR1=',I5,' NYEAR=',I4,' MINSIZE=',
     &       I4,' MAXSIZE=',I4,' SPECIES=',I3,' SURVIVAL=',F5.3)
  
          IF (IYR2.GE.MIY(MICYC)) THEN
            PRMS(1) = IYR2 - MIY(MICYC) + 1
            CALL GPADD (KODE, MIY(MICYC), 307, NPRMS, PRMS, 1, DUM) 
            IYR2 = MIY(MICYC) - 1
            IF (LBMDEB) WRITE (JBMBPR,103) PRMS(1),IYR2
  103       FORMAT (/' IN BMQMRT: QUICK MORTALITY SCHEDULED FOR ',
     >        'THE NEXT MASTER CYCLE. DURATION WILL BE =',F5.0,
     >        '  NEW IYR2=',I5)     
          ENDIF
        ELSE  
C         Set survival to 100% to signify that no extra mortalty occurs this year.
          IF (IYR .GT. IYR2) TSURV= 1.0
        ENDIF 
      ENDIF
      
      IF (TSURV .GE. 1.0) RETURN
      
c     Check to make sure some trees exist in those size classes
      DO 15 ISIZ= MINSIZ, MAXSIZ
          IF (TREE(ISTD,ISIZ,ITYP) .GT. 0.0) GOTO 17
   15 CONTINUE
          RETURN
   17 CONTINUE
          
c     Mortality is determined through an equation relating the total mortality 
C     (or survival) over the period to decreasing survival each year of the 
c     outbreak to simulate the cummulative effects of defoliators.
c     The basic equation is: TSURV=MULT(r^i) where MULT goes from
c     1 to NYEAR.

                                              
      NYR(ISTD) = NYR(ISTD) + 1

      SUM = 0.0
      DO 20 I= 1,NYEAR
         SUM= SUM + I
   20 CONTINUE
   
      R = TSURV**(1/SUM)
      YMORT = 1 - (R**NYR(ISTD))

      YMORT = AMIN1(YMORT,1.0)
      YMORT = AMAX1(YMORT,0.0)
                                                                            
      DO 30 ISIZ= MINSIZ,MAXSIZ
          IF (TREE(ISTD,ISIZ,ITYP) .LE. 0.0) GOTO 30

             TKILL = TREE(ISTD,ISIZ,ITYP) * YMORT
                         
C            Accumulate OAKILL and dead woody pools
      
             OAKILL(ISTD,ISIZ,ITYP) = OAKILL(ISTD,ISIZ,ITYP) + YMORT
             J = L2D(ISIZ)
             IF (ITYP .EQ. 1 .AND. J .GT. 0) 
     &           DWPHOS(ISTD,1,J) = DWPHOS(ISTD,1,J) + TKILL
             J = J + 1
             K = IQPTYP(ISTD,ITYP)
             SDWP(ISTD,K,J,1) = SDWP(ISTD,K,J,1) + 
     &                              TKILL * TVOL(ISTD,ISIZ,1)

   30 CONTINUE   


C     Reset the number of years if this is last year of outbreak

      IF (NYR(ISTD) .EQ. NYEAR) NYR(ISTD)= 0 
      
      IF(LBMDEB) WRITE(JBMBPR,99) IYR, ISTD
   99 FORMAT(' End BMQMRT: Year= ',I5, 'Stand= ', I6)
      

      RETURN
      END
