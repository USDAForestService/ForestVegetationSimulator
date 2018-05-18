      SUBROUTINE BMPSTC (IYR) 
      
*     CALLED FROM: BMDRV
***********************************************************************      
* **BMPSTC:   Date of last revision: June 22, 1994
*
* Pesticide spraying is done only in high quality stand in which there
* are no beetles already. It is 100% effective at killing beetles that 
* try to land on the trees, and lasts for 2 years on the trees.
* Keyword: SPRAY yrstart, prop. stand sprayed
*
* Definitions
*     PSPRAY:     prop sprayed host trees in stand (100% effective)
*     INFEST:     0,1 switch for if beetles are present
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

      INTEGER INFEST
      INTEGER ISIZ
      INTEGER SCNT, MYLST(MXSTND)
      LOGICAL LOK
      REAL    PSPRAY

      SAVE

      DIMENSION PRMS(1)
      
c     Move spray value for second year into this year's slot in case
c     there is no pesticide applied this year

      DO 3 ISTD= 1,BMSTND
        IF(STOCK(ISTD)) THEN
          SPRAY(ISTD,1) = SPRAY(ISTD,2)
          SPRAY(ISTD,2) = 0.0
        ENDIF
    3 CONTINUE

C.... Check for debug.

      IF(LBMDEB) WRITE(JBMBPR,10) IYR
   10 FORMAT(' Begin BMPSTC: Year = ',I5)

C     Check if this is year for pesticide

      IYR1 = 0
      NPRMS = 1
      IYR1 = IYR
      CALL GPGET2 (304,IYR1,7,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)      
      IF (LOK) THEN 
        PSPRAY = PRMS(1)        
        DO 200 I = 1, SCNT
        
          ISTD = MYLST(I)
          IF (.NOT.STOCK(ISTD) .OR. ISTD .LE. 0) GOTO 200
          
          INFEST = 0
          DO 20 ISIZ = 1,NSCL               

C           if there are any pests in the stand then no spraying occurs
            IF (PBKILL(ISTD,ISIZ) .GT. 0.0) THEN   
              INFEST = 1

              IF(LBMDEB) WRITE(JBMBPR,15) ISTD
   15         FORMAT(' Pests present in stand ',I5,
     >               ' so no pesticide was applied')
              
              GOTO 200
                           
            END IF

   20     CONTINUE   
      
          IF (INFEST .EQ. 0) THEN 

C           Pesticide is effective for two years so put in both slots of array
            SPRAY(ISTD,1) = PSPRAY      
            SPRAY(ISTD,2) = PSPRAY
          ENDIF
      
  200   CONTINUE
      ENDIF

      IF(LBMDEB) WRITE(JBMBPR,90) IYR
   90 FORMAT(' End BMPSTC: Year = ',I5)

      RETURN
      END
