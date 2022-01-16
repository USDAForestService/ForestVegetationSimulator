      SUBROUTINE RDINF
      IMPLICIT NONE
C----------
C  **RDINF       LAST REVISION:  08/28/14
C----------
C
C  Purpose :
C     This subroutine takes a newly infected area and converts it
C     into the equivalent number of infected trees of each tree
C     class in the tree list.
C
C  Called By :
C     RDCNTL  [ROOT DISEASE]
C     RDJUMP  [ROOT DISEASE]
C
C  Calls :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
C
C  Revision History :
C     06/12/96 - Matthew K. Thompson
C                Moved the declaration of DSO, DSII, and DSIU to the
C                parameter include file RDPARM.
C   08/28/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'

C.... Local variable declarations.

      LOGICAL DEBUG

      INTEGER  I, I1, I2, IDI, J, KSP
      REAL     ADDINF, NUINSD, OAMOVE(3), PNSP, PROPN, RDRANP

C.... See if we need to do some debug.

      CALL DBCHK (DEBUG,'RDINF',5,ICYC) 

C.... Don't bother doing anything if there is no new area.
      
      IF (AREANU(IRRSP) .LE. 0.0) RETURN

      IDI = MAXRR
      DO 500 KSP=1,MAXSP
         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))
C
C     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
C     If non-host species then IDI = 0, and loop should be skipped to
C     prevent array out of bounds error
C
      IF (IDI .LE. 0) GO TO 500
C
         PNSP = PNINF(IRTSPC(KSP),IDI)

C....    First, modify probability of infection to account for the
C....    number of years in a cycle.         

         PNSP = 1 - ((1 - PNSP) ** (FINT/PINT))
 
C....    Modify probability of infection given root to root contact
C....    based on the proportion of centers that are spore initiated
C....    (SPPROP).
 
         PNSP = PNSP * ((SPPROP(IDI) * SPTRAN(IDI)) + (1 - SPPROP(IDI)))

         IF ((ISCT(KSP,1) .EQ. 0) .OR. (IDI .NE. IRRSP)) GOTO 500

         I1 = ISCT(KSP,1)
         I2 = ISCT(KSP,2)

         DO 400 J=I1, I2
            I = IND1(J)
            
            IF (FPROB(I) .LE. 0.0) GOTO 400
         
            NUINSD = AREANU(IRRSP) * FPROB(I)
            ADDINF = NUINSD * PNSP

C           IF (ICYC .GT. 1) THEN          
C              PROPI(I,ISTEP) = .001
C           ENDIF   
 
C....       Get a random proportion of roots infected based around a 
C....       mean proportion of roots infected for a new infection
C....       mean = 0.001 (Annosus), 0.05 (Armillaria, Phellinus)
               
            PROPI(I,ISTEP,2) = RDRANP(RRNEW(IDI))
            PROBI(I,ISTEP,2) = PROBI(I,ISTEP,2) + ADDINF
            PROBIU(I) = PROBIU(I) + NUINSD * (1 - PNSP)
                                                   
C....       Add the new infection resulting from the center area
C....       expanding.  
C....       (,1)=# trees infected, (,2)=total # new trees in area.

            EXPINF(IDI,1) = EXPINF(IDI,1) + ADDINF
            EXPINF(IDI,2) = EXPINF(IDI,2) + NUINSD
                                                   
C....       Make sure that OAKL continues to contain the right number
C....       of killed trees. Assume that some proportion of those trees
C....       that were outside killed are now inside killed (either
C....       infected or uninfected) (eg. If 10% of the outside trees
C....       were killed from BB or windthrow then 10% of those trees
C....       just becoming inside trees were killed from BB or windthrow)
C....
C....       NUINSD -- Number of new trees inside the center.
C....       OAMOVE -- Amount to move from one OAKL slot to another.
 
            IF (OAKL(DSO,I) .GT. 0.0) THEN
               PROPN = OAKL(DSO,I) / (FPROB(I) * (SAREA - PAREA(IRRSP) + 
     &                 AREANU(IRRSP)))             
     
               OAMOVE(DSO) = PROPN * NUINSD
               OAMOVE(DSII) = PROPN * ADDINF
               OAMOVE(DSIU) = PROPN * (NUINSD - ADDINF)
              
               CALL RDMREC (2,I,KSP,OAMOVE)
            
            ENDIF
            
            IF (DEBUG) WRITE(JOSTND,401) PROBI(I,ISTEP,2), PROBIU(I)
  401       FORMAT(' IN RDINF PROBI PROBIU',2F10.2)
  400    CONTINUE
  500 CONTINUE

C.... Call RDSUM to calculate new value of PROBIT.
 
      CALL RDSUM(ITRN,PROBIT,PROBI,ISTEP)

      RETURN
      END
