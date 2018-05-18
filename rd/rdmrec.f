      SUBROUTINE RDMREC (ITYP,I,KSP,OAMOVE)
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  Purpose :
C     This subroutine re-arranges other agent mortality so that when
C     uninfected trees become infected or just inside a center, the
C     appropriate other agent mortality will still be applied.
C
C  Called By :
C     RDINF   [ROOT DISEASE]
C     RDINSD  [ROOT DISEASE]
C
C  Calls :
C     RDSSIZ   (SUBROUTINE)   [ROOT DISEASE]
C     RDSTP    (SUBROUTINE)   [ROOT DISEASE]
C
C  Revision History :
C     06/12/96 - Matthew K. Thompson
C                Moved the declaration of DSO, DSII, and DSIU to the
C                parameter include file RDPARM.
C     10/07/97 - Matthew K. Thompson
C                Commented out the line of code that sets 
C                PROBI = PROBI - OAMOVE(DSII)
C                The other agent mortality has already been deleted
C                from PROBI.
C   08/29/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C


C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Coomon include fules.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'

C.... Local variable declaraions.

      INTEGER  I, ISL, ITYP, KSP
      REAL     OAMOVE(3)
      INTEGER IDANUW

C.... Make sure that OAKL continues to contain the right number of
C.... killed trees. Assume that some proportion of those trees that
C.... were outside killed are now inside killed (either infected or
C.... uninfected) (eg. If 10% of the outside trees were killed from
C.... bark beetles or windthrow then 10% of those trees just becoming
C.... inside trees were killed from bark beetles or windthrow).
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = ITYP
C     
      OAKL(DSO,I) = OAKL(DSO,I) - OAMOVE(DSO)
      OAKL(DSII,I) = OAKL(DSII,I) + OAMOVE(DSII)
      OAKL(DSIU,I) = OAKL(DSIU,I) + OAMOVE(DSIU)

      IF (OAMOVE(DSIU) .LT. 0.0 .AND. OAKL(DSIU,I) .GT. 0.0) THEN

C....    New OAKL goes from DSIU to DSII.

         BBKILL(DSII,I) = BBKILL(DSII,I) + OAMOVE(DSII) *
     &                                 (BBKILL(DSIU,I) / OAKL(DSIU,I))
         BBKILL(DSIU,I) = BBKILL(DSIU,I) + OAMOVE(DSIU) *
     &                                 (BBKILL(DSIU,I) / OAKL(DSIU,I))
     
      ELSEIF (OAMOVE(DSIU) .GE. 0.0 .AND. OAKL(DSO,I) .GT. 0.0) THEN

C....    New OAKL goes from DSO to DSII and DSIU.

         BBKILL(DSII,I) = BBKILL(DSII,I) + OAMOVE(DSII) *
     &                                   (BBKILL(DSO,I) / OAKL(DSO,I))
         BBKILL(DSIU,I) = BBKILL(DSIU,I) + OAMOVE(DSIU) *
     &                                   (BBKILL(DSO,I) / OAKL(DSO,I))
         BBKILL(DSO,I) = BBKILL(DSO,I) - (BBKILL(DSO,I) / OAKL(DSO,I)) *
     &                                   OAMOVE(DSO)
      ENDIF

C.... Bark beetle and windthrown trees are already removed from PROBI.
C.... This line of code is double counting the removal.
C     PROBI(I,ISTEP,ITYP) = PROBI(I,ISTEP,ITYP) - OAMOVE(DSII)

      RRKILL(I) = RRKILL(I) + OAMOVE(DSII)

      CALL RDSSIZ(KSP,DBH(I),STCUT,ISL,ISPS,IRTSPC)
      CALL RDSTP (ISL,KSP,OAMOVE(DSII),DBH(I),ROOTL(I))


      RETURN
      END
