      SUBROUTINE MISINT
***********************************************************************
C MISTOE $Id: misint.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
*----------------------------------------------------------------------
*  Purpose:
*     Mistletoe parameter initialization routine. This routine is
*  variant dependent and sets the variant dependent variables for other
*  mistletoe routines. This is the Inland Empire (N. Idaho) version.
*----------------------------------------------------------------------
*
*  Call list definitions:
*
*  Local variable definitions:
*     DEBUG:  Logical flag to turn debug on and off.
*     AFIT:   Array of MISFIT data.
*     ACSP:   Array of CSPARR data.
*     ADGP:   Array of DGPMDR data.
*     APMC:   Array of PMCSP data.
*
*  Common block variables and parameters:
*     CSPARR: From MISCOM; 2-char. representations of all species.
*     DGPDMR: From MISCOM; diameter growth potentials based on species
*                and DMR (0-6).
*     ICYC:   From CONTRL; cycle index number.
*     JOSTND: From CONTRL; logical unit number for stand output.
*     MISFIT: From MISCOM; tells which species are affected by DM.
*     PMCSP:  From MISCOM; percent mortality coefficients by species.
*
*  Revision History :
*     07-OCT-2005 Lance R. David (FHTET)
*       Corrected specie codes to match specie codes in FVS.
*
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'MISCOM.F77'

C.... Variable declarations.

      LOGICAL DEBUG
      REAL ADGP(MAXSP,7),APMC(MAXSP,3)
      CHARACTER*2 ACSP(MAXSP)
      INTEGER AFIT(MAXSP)

C.... Data statements.

C.... Species character representations

      DATA (ACSP(I),I=1,15)
     &   /'PW','LW','FD','BG','HW','CW','PL','SE','BL','PY',
     &    'EP','AT','AC','OC','OH'/

C.... Species affected by mistletoe

      DATA (AFIT(I),I=1,15)
     &   /  0,   1,   1,   0,   1,   0,   1,   0,   0,   1,
     &      0,   0,   0,   1,   0/

C.... Diameter growth rates

      DATA ((ADGP(I,J),J=1,7),I=1,15)
     &   /1.0,1.0,1.0,1.0,1.0,1.0,1.0, !PW
     &    1.0,.94,.92,.88,.84,.58,.54, !LW
     &    1.0,.98,.97,.85,.80,.52,.44, !FD
     &    1.0,1.0,1.0,1.0,1.0,1.0,1.0, !BG
     &    1.0,1.0,1.0,1.0,.82,.82,.82, !HW
     &    1.0,1.0,1.0,1.0,1.0,1.0,1.0, !CW
     &    1.0,1.0,1.0,1.0,.94,.80,.59, !PL
     &    1.0,1.0,1.0,1.0,1.0,1.0,1.0, !SE
     &    1.0,1.0,1.0,1.0,1.0,1.0,1.0, !BL
     &    1.0,1.0,1.0,.98,.86,.73,.50, !PY
     &    1.0,1.0,1.0,1.0,1.0,1.0,1.0, !EP
     &    1.0,1.0,1.0,1.0,1.0,1.0,1.0, !AT
     &    1.0,1.0,1.0,1.0,1.0,1.0,1.0, !AC
     &    1.0,.98,.97,.85,.80,.52,.44, !OC=FD
     &    1.0,1.0,1.0,1.0,1.0,1.0,1.0/ !OH

C.... Mortality coefficients

      DATA ((APMC(I,J),J=1,3),I=1,15)
     &   /0.0,0.0,0.0,                 !PW
     &   0.01319,-0.01627,0.00822,     !LW
     &   0.01319,-0.01627,0.00822,     !FD
     &   0.0,0.0,0.0,                  !BG
     &   0.00681,-0.00580,0.00935,     !HW
     &   0.0,0.0,0.0,                  !CW
     &   0.00112,0.02170,-0.00171,     !PL
     &   0.0,0.0,0.0,                  !SE
     &   0.0,0.0,0.0,                  !BL
     &   0.00681,-0.00580,0.00935,     !PY
     &   0.0,0.0,0.0,                  !EP
     &   0.0,0.0,0.0,                  !AT
     &   0.0,0.0,0.0,                  !AC
     &   0.01319,-0.01627,0.00822,     !OC=FD                    
     &   0.0,0.0,0.0/                  !OH  

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISINT',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,10)ICYC
   10 FORMAT(' Begin/end MISINTIB: Cycle = ',I5)

C.... Mistletoe model initializations.

      DO 200 I=1,MAXSP
         MISFIT(I)=AFIT(I)
         CSPARR(I)=ACSP(I)
         DO 100 J=1,7
            DGPDMR(I,J)=ADGP(I,J)
  100    CONTINUE
         DO 150 J=1,3
            PMCSP(I,J)=APMC(I,J)
  150    CONTINUE
  200 CONTINUE

      RETURN
      END
