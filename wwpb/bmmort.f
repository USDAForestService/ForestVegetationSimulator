      SUBROUTINE BMMORT (ISTD,IYR,SLOW)
      
c     CALLED FROM BMDRV
***********************************************************************
* **BMMORT    Date of last revision:  09/28/05
*
*  Local variables:
*     PRDEAD: proportion of the number of trees that were killed
*
*  Call list definitions:
*     CAREA:  current stand area
*
*  Common block variables and parameters:
*     ALLKLL From BMCOM; Number of Ips-killed trees in each size class
*     BAH:    From BMCOM; Array containing total host BA by size class
*     BANH:   From BMCOM; Array containing total nonhost BA by size class
*     ICYC:   From CONTRL; cycle index number.
*     JOSTND: From CONTRL; logical unit number for stand output.
*     MSBA:   FROM BMCOM; Array containing the BA in each size class
*     NSCL:   From BMPRM; Number of dbh size classes
*     OAKILL: From BMCOM, Array with proportion of other agent killed trees
*     PBKILL: From BMCOM; Array containing number of Beetle-killed trees
*             in each size class
*     TPBK:   Predicted mortality for FAST(1), SLOW(2) and BEETLE(3)
*             processes, where the number is the third index of the var, accumulated
*             annually over the master cycle.
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'BMCOM.F77'
      INCLUDE 'BMPCOM.F77'

C.... Variable declarations.
                         
      INTEGER ISIZ                    
      LOGICAL SLOW     
      REAL    PRDEAD

      IF(LBMDEB) WRITE(JBMBPR,10) IYR, ISTD
   10 FORMAT(' Begin BMMORT: Year= ',I5, 'Stand= ', I6)


C     IT IS NECESSARY TO CALL THIS ROUTINE
C     TWICE. THE FIRST CALL WILL REMOVE FAST OAKILL; THE SECOND CALL WILL REMOVE SLOW
C     OAKILL AND PBKILL. USE EITHER A TRUE/FALSE ARGUMENT


c     Convert OAKILL() from a proportion to TPA, decrement host basal
c     area and tree/acre,add year mortality to the accumulating pool of mortality

      
      BAH(ISTD,NSCL+1)= 0.0
      BANH(ISTD,NSCL+1)= 0.0
      TREE(ISTD,NSCL+1,1)=0.0   !AJM 9/05
      TREE(ISTD,NSCL+1,2)=0.0   !AJM 9/05
                            
      DO 800 ISIZ= 1,NSCL
      
c       PBKILL(ISTD,ISIZ)= PBKILL(ISTD,ISIZ) * TREE(ISTD,ISIZ,1) << done in BMISTD >>
        OAKILL(ISTD,ISIZ,1)= OAKILL(ISTD,ISIZ,1) * TREE(ISTD,ISIZ,1)
        OAKILL(ISTD,ISIZ,2)= OAKILL(ISTD,ISIZ,2) * TREE(ISTD,ISIZ,2)
        
c        IF ((OAKILL(ISTD,ISIZ,1) + OAKILL(ISTD,ISIZ,2)) .LE. 0.0) 
c     &         GO TO 800
        
        PRDEAD = 0.0
        IF (SLOW) THEN 
          IF (TREE(ISTD,ISIZ,1) .GT. 0.0) THEN
             PRDEAD = (PBKILL(ISTD,ISIZ) + OAKILL(ISTD,ISIZ,1) + 
     &               ALLKLL(ISTD,ISIZ)) / TREE(ISTD,ISIZ,1)
          ELSE
             PRDEAD = 0.0
          ENDIF
          BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) * (1 - PRDEAD)
          TREE(ISTD,ISIZ,1)= TREE(ISTD,ISIZ,1) * (1 - PRDEAD)

        ELSE
          FASTK(ISTD,1) = FASTK(ISTD,1) + OAKILL(ISTD,ISIZ,1) 
     &                                  + OAKILL(ISTD,ISIZ,2)
          FASTK(ISTD,2) = FASTK(ISTD,2) + OAKILL(ISTD,ISIZ,1) * 
     &                    TVOL(ISTD,ISIZ,1) + OAKILL(ISTD,ISIZ,2) *
     &                    TVOL(ISTD,ISIZ,2)
     
          IF (TREE(ISTD,ISIZ,1) .GT. 0.0) THEN
             PRDEAD = OAKILL(ISTD,ISIZ,1) / TREE(ISTD,ISIZ,1)
          ELSE
             PRDEAD = 0.0
          ENDIF
          FASTK(ISTD,2) = FASTK(ISTD,2) + BAH(ISTD,ISIZ) * PRDEAD
          BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) * (1 - PRDEAD)
          TREE(ISTD,ISIZ,1)= TREE(ISTD,ISIZ,1) - OAKILL(ISTD,ISIZ,1)

          IF (TREE(ISTD,ISIZ,2) .GT. 0.0) THEN 
            PRDEAD = OAKILL(ISTD,ISIZ,2) / TREE(ISTD,ISIZ,2)
          ELSE
            PRDEAD = 0.0
          ENDIF

          FASTK(ISTD,3) = FASTK(ISTD,3) + BANH(ISTD,ISIZ) * PRDEAD
          BANH(ISTD,ISIZ)= BANH(ISTD,ISIZ) * (1 - PRDEAD)
          TREE(ISTD,ISIZ,2)= TREE(ISTD,ISIZ,2) - OAKILL(ISTD,ISIZ,2)  
        
        ENDIF


c       Constrain to positive values.
         
        BAH(ISTD,ISIZ)= AMAX1(BAH(ISTD,ISIZ), 0.0)
        BANH(ISTD,ISIZ)= AMAX1(BANH(ISTD,ISIZ), 0.0)
        TREE(ISTD,ISIZ,1)= AMAX1(TREE(ISTD,ISIZ,1), 0.0)
        TREE(ISTD,ISIZ,2)= AMAX1(TREE(ISTD,ISIZ,2), 0.0)

C       CHECK TO MAKE SURE THAT BA IS GONE IF TREES ARE GONE

        IF (TREE(ISTD,ISIZ,1) .LE. 0.0) BAH(ISTD,ISIZ) = 0.0
        IF (TREE(ISTD,ISIZ,2) .LE. 0.0) BANH(ISTD,ISIZ) = 0.0
        
C       RECALCULATE TOTAL BA IN STAND BY TYPE (NSCL+1) CLASS

        BAH(ISTD,NSCL+1)= BAH(ISTD,NSCL+1) + BAH(ISTD,ISIZ)
        BANH(ISTD,NSCL+1)= BANH(ISTD,NSCL+1) + BANH(ISTD,ISIZ)
        TREE(ISTD,NSCL+1,1)=TREE(ISTD,NSCL+1,1)+TREE(ISTD,ISIZ,1)   !AJM 9/05
        TREE(ISTD,NSCL+1,2)=TREE(ISTD,NSCL+1,2)+TREE(ISTD,ISIZ,2)   !AJM 9/05
        

        IF (SLOW) THEN
          TPBK(ISTD,ISIZ,1,3)= TPBK(ISTD,ISIZ,1,3) + PBKILL(ISTD,ISIZ) 
     &                          + ALLKLL(ISTD,ISIZ)
          TPBK(ISTD,ISIZ,1,2)= TPBK(ISTD,ISIZ,1,2) + OAKILL(ISTD,ISIZ,1)
          TPBK(ISTD,ISIZ,2,2)= TPBK(ISTD,ISIZ,2,2) + OAKILL(ISTD,ISIZ,2)
        ELSE
          TPBK(ISTD,ISIZ,1,1)= TPBK(ISTD,ISIZ,1,1) + OAKILL(ISTD,ISIZ,1)
          TPBK(ISTD,ISIZ,2,1)= TPBK(ISTD,ISIZ,2,1) + OAKILL(ISTD,ISIZ,2)
        ENDIF
        
C       Zero out the other agent arrays, ready for use next timestep
C       PBKILL and ALLKLL will get zeroed after BKP emerges

        OAKILL(ISTD,ISIZ,1)= 0.0
        OAKILL(ISTD,ISIZ,2)= 0.0
  800 CONTINUE 
       
      IF(LBMDEB) WRITE(JBMBPR,99) IYR, ISTD
   99 FORMAT(' End BMMORT: Year= ',I5, 'Stand= ', I6)
      
      RETURN
      END
