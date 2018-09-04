      SUBROUTINE MISINT
***********************************************************************
C METRIC-MISTOE $Id$
*----------------------------------------------------------------------
*  Purpose:
*     Mistletoe parameter initialization routine. This routine is
*  variant dependent and sets the variant dependent variables for other
*  mistletoe routines. This is the Pacific Northwest Coast version.
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
*     19-OCT-2005 Lance R. David (FHTET)
*       Corrected specie codes to match specie codes in FVS.
*     21-APR-2009 Lance R David (FMSC)
*       Changed species codes CO to CW (Black Cottonwood),
*       J to WJ (Western Juniper), BC to CH (Bitter Cherry).
***********************************************************************
      IMPLICIT NONE
      
C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'MISCOM.F77'

C.... Variable declarations.

      LOGICAL DEBUG
      REAL AFIT(MAXSP),ADGP(MAXSP,7),APMC(MAXSP,3)
      CHARACTER*2 ACSP(MAXSP)
      INTEGER I,J

C.... Data statements.

C.... Species character representations

      DATA (ACSP(I),I=1,39)
     & /'BA','BC','BG','BL','BM','SS','BP','YC',
     &'OA','SE','PL','JP','PS','PW','PP','FD','OC','CW',
     &'HW','HM','MB','DR','RA','EP','GC','AT','AC','QG','WJ',
     &'LA','PA','KP','TW','GP','HT','VB','W ','**','OT'/
C

C.... Species affected by mistletoe

      DATA (AFIT(I),I=1,39)
     &   /  0,   0,   0,   1,   1,   0,   0,   0,   0,   0,
     &      1,   0,   0,   0,   0,   1,   0,   0,   1,   1,
     &      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &      0,   0,   0,   0,   0,   0,   0,   0,   0/

C.... Diameter growth rates

      DATA ((ADGP(I,J),J=1,7),I=1,39)
     &   /1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, .98, .95, .70, .50,
     &   1.0, 1.0, 1.0, .98, .95, .70, .50,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, .98, .97, .85, .80, .52, .44,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, .82, .82, .82,
     &   1.0, 1.0, 1.0, .98, .86, .73, .50,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/

C.... Mortality coefficients

      DATA ((APMC(I,J),J=1,3),I=1,39)
     &   /0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.00159,  0.00508,  
     &   0.0,  0.00159,  0.00508,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.00112,  0.02170,  -0.00171,
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.01319,  -0.01627,  0.00822,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.00681,  -0.00580,  0.00935,  
     &   0.00681,  -0.00580,  0.00935,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0,  
     &   0.0,  0.0,  0.0/

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISINT',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,10)ICYC
   10 FORMAT(' Begin/end MISINTWC: Cycle = ',I5)

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

      DMRMIN = 1.0

      RETURN
      END
