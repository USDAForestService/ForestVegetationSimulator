      SUBROUTINE GROHED (IUNIT)
      IMPLICIT NONE
C----------
C  **GROHED-CR   DATE OF LAST REVISION:  10/13/11
C----------
C     WRITES HEADER FOR BASE MODEL PORTION OF PROGNOSIS SYSTEM
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
      INCLUDE 'PLOT.F77'
C
C
C COMMONS
C
C----------
      INTEGER IUNIT
      CHARACTER DAT*10,TIM*8,VVER*7,DVVER*7,REV*10,SVN*8
      DATA DVVER/'SM     '/
      DATA SVN/"OFVS-002"/
C----------
C     CALL REVISE TO GET THE LATEST REVISION DATE FOR THIS VARIANT.
C----------
      CALL REVISE (DVVER,REV)
C----------
C     CALL THE DATE AND TIME ROUTINE FOR THE HEADING.
C----------
      CALL GRDTIM (DAT,TIM)
C----------
C     CALL PPE TO CLOSE OPTION TABLE IF IT IS OPEN.
C----------
      CALL PPCLOP (IUNIT)
C----------
C FOR INITIAL ENTRY TO GROHED (BEGINNING OF KEYWORD LISTING) PRINT
C GENERIC TITLE SINCE IMODTY HASN'T BEEN SET YET.
C----------
      IF(IMODTY .EQ. 0) THEN
        WRITE (IUNIT,50) SVN,REV,DAT,TIM
   50   FORMAT ('1',T10,'FOREST VEGETATION SIMULATOR',
     >  5X,'VERSION ',A,'                                            ',
     >  T97,'RV:',A,T112,A,2X,A)
        GO TO 1000
      ENDIF
C----------
C BRANCH TO APPROPRIATE MODEL TYPE
C----------
      GO TO (100,200,300,400,500,600,600,600),IMODTY
C
C********************************************
C SOUTHWEST MIXED CONIFER TYPE             **
C********************************************
C
  100 CONTINUE
      WRITE (IUNIT,140) SVN,REV,DAT,TIM
  140 FORMAT ('1',T10,'FOREST VEGETATION SIMULATOR',
     > 5X,'VERSION ',A,' -- CENTRAL ROCKIES SW MIXED CONIFERS GENGYM',
     > T97,'RV:',A,T112,A,2X,A)
      GO TO 1000
C
C**********************************************
C SOUTHWEST PONDEROSA PINE TYPE              **
C**********************************************
C
  200 CONTINUE
      DVVER = 'SP 6.31'
      WRITE (IUNIT,240) SVN,REV,DAT,TIM
  240 FORMAT ('1',T10,'FOREST VEGETATION SIMULATOR',
     > 5X,'VERSION ',A,' -- CENTRAL ROCKIES SW PONDEROSA PINE GENGYM',
     > T97,'RV:',A,T112,A,2X,A)
      GO TO 1000
C
C*********************************************
C BLACK HILLS PONDEROSA PINE TYPE           **
C*********************************************
C
  300 CONTINUE
      DVVER = 'BP 6.31'
      WRITE (IUNIT,340) SVN,REV,DAT,TIM
  340 FORMAT ('1',T10,'FOREST VEGETATION SIMULATOR',
     > 5X,'VERSION ',A,' -- CENTRAL ROCKIES BLACK HILLS/NEBR GENGYM',
     > T97,'RV:',A,T112,A,2X,A)
      GO TO 1000
C
C*******************************************
C SPRUCE-FIR TYPE                         **
C*******************************************
C
  400 CONTINUE
      DVVER = 'SF 6.31'
      WRITE (IUNIT,440) SVN,REV,DAT,TIM
  440 FORMAT ('1',T10,'FOREST VEGETATION SIMULATOR',
     > 5X,'VERSION ',A,' -- CENTRAL ROCKIES SPRUCE-FIR GENGYM        ',
     > T97,'RV:',A,T112,A,2X,A)
      GO TO 1000
C
C*******************************************
C LODGEPOLE PINE TYPE                     **
C*******************************************
C
  500 CONTINUE
      DVVER = 'LP 6.31'
      WRITE (IUNIT,540) SVN,REV,DAT,TIM
  540 FORMAT ('1',T10,'FOREST VEGETATION SIMULATOR',
     > 5X,'VERSION ',A,' -- CENTRAL ROCKIES LODGEPOLE PINE GENGYM    ',
     > T97,'RV:',A,T112,A,2X,A)
      GO TO 1000
C
C****************************************************
C SPACE FOR FUTURE MODEL TYPES INCLUDING ASPEN     **
C****************************************************
C
  600 CONTINUE
      GO TO 100
C
 1000 CONTINUE
      RETURN
C
C
      ENTRY VARVER (VVER)
C----------
C     SUPPLY THE VARIANT AND VERSION NUMBER.
C----------
      IF(IMODTY .LE. 0) GO TO 1
      GO TO (1,2,3,4,5), IMODTY
    1 VVER = 'SM 6.31'
      GO TO 10
    2 VVER = 'SP 6.31'
      GO TO 10
    3 VVER = 'BP 6.31'
      GO TO 10
    4 VVER = 'SF 6.31'
      GO TO 10
    5 VVER = 'LP 6.31'
      GO TO 10
C     VVER=DVVER
   10 CONTINUE
      RETURN
      END
