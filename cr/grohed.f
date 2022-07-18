      SUBROUTINE GROHED (IUNIT)
      IMPLICIT NONE
C----------
C CR $Id$
C----------
C     WRITES HEADER FOR BASE MODEL PORTION OF PROGNOSIS SYSTEM
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'INCLUDESVN.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C COMMONS
C----------
      INTEGER IUNIT
      CHARACTER DAT*10,TIM*8,REV*10,SVN*4
C----------
C     CALL REVISE TO GET THE LATEST REVISION DATE FOR THIS VARIANT.
C----------
      CALL REVISE (VARACD,REV)
C----------
C     CALL THE DATE AND TIME ROUTINE FOR THE HEADING.
C----------
      CALL GRDTIM (DAT,TIM)
C----------
C BRANCH TO APPROPRIATE MODEL TYPE
C----------
      SELECT CASE (IMODTY)
C
C********************************************
C SOUTHWEST MIXED CONIFER TYPE             **
C********************************************
C
        CASE (1)
        WRITE (IUNIT,140) SVN,REV,DAT,TIM
  140   FORMAT (//T10,'FOREST VEGETATION SIMULATOR',
     >  5X,'VERSION ',A,' -- CEN. ROCKIES SW MIXED CONIFERS GENGYM',
     >  T97,'RV:',A,T112,A,2X,A)
C
C**********************************************
C SOUTHWEST PONDEROSA PINE TYPE              **
C**********************************************
C
        CASE (2)
        WRITE (IUNIT,240) SVN,REV,DAT,TIM
  240   FORMAT (//T10,'FOREST VEGETATION SIMULATOR',
     >  5X,'VERSION ',A,' -- CEN. ROCKIES SW PONDEROSA PINE GENGYM',
     >  T97,'RV:',A,T112,A,2X,A)
C
C*********************************************
C BLACK HILLS PONDEROSA PINE TYPE           **
C*********************************************
C
        CASE (3)
        WRITE (IUNIT,340) SVN,REV,DAT,TIM
  340   FORMAT (//T10,'FOREST VEGETATION SIMULATOR',
     >  5X,'VERSION ',A,' -- CEN. ROCKIES BLACK HILLS/NEBR GENGYM',
     >  T97,'RV:',A,T112,A,2X,A)
C
C*******************************************
C SPRUCE-FIR TYPE                         **
C*******************************************
C
        CASE (4)
        WRITE (IUNIT,440) SVN,REV,DAT,TIM
  440   FORMAT (//T10,'FOREST VEGETATION SIMULATOR',
     >  5X,'VERSION ',A,' -- CEN. ROCKIES SPRUCE-FIR GENGYM        ',
     >  T97,'RV:',A,T112,A,2X,A)
C
C*******************************************
C LODGEPOLE PINE TYPE                     **
C*******************************************
C
        CASE (5)
        WRITE (IUNIT,540) SVN,REV,DAT,TIM
  540   FORMAT (//T10,'FOREST VEGETATION SIMULATOR',
     >  5X,'VERSION ',A,' -- CEN. ROCKIES LODGEPOLE PINE GENGYM    ',
     >  T97,'RV:',A,T112,A,2X,A)
C
C****************************************************
C MODEL TYPE 0 -- HASN'T BEEN SET YET (AT BEGINNING OF KEYWORD LISTING)
C ALSO FOR FUTURE MODEL TYPES INCLUDING ASPEN     **
C****************************************************
C
        CASE DEFAULT
        WRITE (IUNIT,50) SVN,REV,DAT,TIM
   50   FORMAT (//T10,'FOREST VEGETATION SIMULATOR',
     >  5X,'VERSION ',A,' -- CENTRAL ROCKIES                         ',
     >  T97,'RV:',A,T112,A,2X,A)
C
      END SELECT
C
      RETURN
      END
