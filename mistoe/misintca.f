      SUBROUTINE MISINT
***********************************************************************
C MISTOE $Id: misint.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
*----------------------------------------------------------------------
*  Purpose:
*     Mistletoe parameter initialization routine. This routine is
*  variant dependent and sets the variant dependent variables for other
*  mistletoe routines. This is the Inland California / Southern
*  Cascades version.
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
*     03/21/97 Julie C. Williams-Cipriani (FHTET-FC)
*       Initial version.
*     18-OCT-2005 Lance R. David (FHTET)
*       Corrected specie codes to match specie codes in FVS.
*     21-APR-2009 Lance R. David (FMSC)
*       Specie code JU change to WJ.
*  12-JUL-2011 Lance R. David (FMSC)
*    Added arrays for height growth impacts.
*    Impact values must be supplied by MistHMod keyword.
*
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
      REAL ADGP(MAXSP,7),AHGP(MAXSP,7),APMC(MAXSP,3)
      CHARACTER*2 ACSP(MAXSP)
      INTEGER I,J,AFIT(MAXSP)

C.... Data statements.

C.... Species character representations

      DATA (ACSP(I),I=1,MAXSP)
     &   /'PC','IC','RC','WF','RF','SH','DF','WH','MH','WB',
     &    'KP','LP','CP','LM','JP','SP','WP','PP','MP','GP',
     &    'WJ','BR','GS','PY','OS','LO','CY','BL','EO','WO',
     &    'BO','VO','IO','BM','BU','RA','MA','GC','DG','FL',
     &    'WN','TO','SY','AS','CW','WI','CN','CL','OH'/

C.... Species affected by mistletoe

      DATA (AFIT(I),I=1,MAXSP)
     &   /0,   0,   0,   1,   1,   1,   1,   1,   1,   1,
     &    1,   1,   0,   1,   1,   1,   1,   1,   0,   0,
     &    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &    0,   0,   0,   0,   0,   0,   0,   0,   0/

C.... Diameter growth rates

      DATA ((ADGP(I,J),J=1,7),I=1,10)
     &  /1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, .93, .93, .93, .93,
     &   1.0, 1.0, 1.0, .93, .93, .93, .93,
     &   1.0, 1.0, 1.0, .93, .93, .93, .93,
     &   1.0, .98, .97, .85, .80, .52, .44,
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59,
     &   1.0, 1.0, 1.0, .98, .86, .73, .50,
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59/

      DATA ((ADGP(I,J),J=1,7),I=11,20)
     &  /1.0, 1.0, 1.0, 1.0, .94, .80, .59,
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59,
     &   1.0, 1.0, 1.0, .98, .86, .73, .50,
     &   1.0, 1.0, 1.0, .72, .72, .72, .72,
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59,
     &   1.0, .96, .96, .96, .96, .96, .96,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/

      DATA ((ADGP(I,J),J=1,7),I=21,30)
     &  /1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/

      DATA ((ADGP(I,J),J=1,7),I=31,40)
     &  /1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/

      DATA ((ADGP(I,J),J=1,7),I=41,MAXSP)
     &  /1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/

C.... Height growth potential rates
C....
C.... Using Douglas-fir height growth impact values described in:
C....
C.... Marshall, Katy 2007. Permanent plots for measuring spread and
C.... impact of Douglas-fir dwarf mistletoe in the Southern Oregon
C.... Cascades, Pacific Northwest Region: Results of the ten year
C.... remeasurement. USDA Forest Service, Pacific Northwest Region,
C.... Southwest Oregon Forest Insect and Disease Service Center, 
C.... Central Point, Oregon. SWOFIDSC-07-04. 34 pp.
C....
C.... Default values for DF in this table would be:
C.... &   1.0,1.0,1.0,.95,.65,.50,.10,
C.... So that impacts are not unknowingly applied to projections,
C.... the values must be supplied with the MistHMod keyword.
C.... when appropriat default values are developed, they will be
C.... set here.

      DATA ((AHGP(I,J),J=1,7),I=1,10)
     &  /1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/

      DATA ((ADGP(I,J),J=1,7),I=11,20)
     &  /1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/

      DATA ((ADGP(I,J),J=1,7),I=21,30)
     &  /1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/

      DATA ((ADGP(I,J),J=1,7),I=31,40)
     &  /1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/

      DATA ((ADGP(I,J),J=1,7),I=41,MAXSP)
     &  /1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/

C.... Mortality coefficients

      DATA ((APMC(I,J),J=1,3),I=1,10)
     &  /0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.00159,  0.00508,
     &   0.0,      0.00159,  0.00508,
     &   0.0,      0.00159,  0.00508,
     &   0.01319, -0.01627,  0.00822,
     &   0.00681, -0.00580,  0.00935,
     &   0.00681, -0.00580,  0.00935,
     &   0.00112,  0.02170, -0.00171/

      DATA ((APMC(I,J),J=1,3),I=11,20)
     &  /0.00112,  0.02170, -0.00171,
     &   0.00112,  0.02170, -0.00171,
     &   0.0,      0.0,      0.0,
     &   0.00112,  0.02170, -0.00171,
     &   0.00681, -0.00580,  0.00935,
     &   0.00112,  0.02170, -0.00171,
     &   0.00112,  0.02170, -0.00171,
     &   0.00681, -0.00580,  0.00935,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0/

      DATA ((APMC(I,J),J=1,3),I=21,30)
     &  /0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0/

      DATA ((APMC(I,J),J=1,3),I=31,40)
     &  /0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0/

      DATA ((APMC(I,J),J=1,3),I=41,49)
     &  /0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0,
     &   0.0,      0.0,      0.0/

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
            HGPDMR(I,J)=AHGP(I,J)
  100    CONTINUE
         DO 150 J=1,3
            PMCSP(I,J)=APMC(I,J)
  150    CONTINUE
  200 CONTINUE

      RETURN
      END
