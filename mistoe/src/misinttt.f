      SUBROUTINE MISINT
***********************************************************************
*  **MISINT--TT  Date of last revision:  07/12/11
*----------------------------------------------------------------------
*  Purpose:
*     Mistletoe parameter initialization routine. This routine is
*  variant dependent and sets the variant dependent variables for other
*  mistletoe routines. This is the Teton version.
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
      REAL AFIT(MAXSP),ADGP(MAXSP,7),AHGP(MAXSP,7),APMC(MAXSP,3)
      CHARACTER*2 ACSP(MAXSP)
      INTEGER I,J

C.... Data statements.
C
C SPECIES ORDER FOR TETONS VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH
C
C VARIANT EXPANSION:
C BS USES ES EQUATIONS FROM TT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C PP USES PP EQUATIONS FROM CI
C UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C MM USES MM EQUATIONS FROM IE
C NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM TT
C
C.... Species character representations

      DATA (ACSP(I),I=1,MAXSP)
     &   /'WB','LM','DF','PM','BS','AS','LP','ES','AF','PP',
     &    'UJ','RM','BI','MM','NC','MC','OS','OH'/

C.... Species affected by mistletoe

      DATA (AFIT(I),I=1,MAXSP)
     &   /  1,   1,   1,   1,   1,   0,   1,   0,   1,   1,
     &      0,   0,   0,   0,   0,   0,   0,   0/

C.... Diameter growth rates

      DATA ((ADGP(I,J),J=1,7),I=1,MAXSP)/
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59,   !  1 WB
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59,   !  2 LM
     &   1.0, .98, .97, .85, .80, .52, .44,   !  3 DF
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59,   !  4 PM (    LP  )
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,   !  5 BS
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,   !  6 AS
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59,   !  7 LP
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,   !  8 ES
     &   1.0, 1.0, 1.0, .98, .95, .70, .50,   !  9 AF
     &   1.0, 1.0, 1.0, .98, .86, .73, .50,   ! 10 PP (CI  PP  )
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,   ! 11 UJ
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,   ! 12 RM
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,   ! 13 BI
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,   ! 14 MM
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,   ! 15 NC
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,   ! 16 MC
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,   ! 17 OS
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/   ! 18 OH

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

      DATA ((AHGP(I,J),J=1,7),I=1,MAXSP)
     &  /1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/

C.... Mortality coefficients

      DATA ((APMC(I,J),J=1,3),I=1,MAXSP)/
     &   0.00112,  0.02170, -0.00171,         !  1 WB             
     &   0.00112,  0.02170, -0.00171,         !  2 LM             
     &   0.01319, -0.01627,  0.00822,         !  3 DF             
     &   0.00112,  0.02170, -0.00171,         !  4 PM (    LP  )  
     &       0.0,      0.0,      0.0,         !  5 BS             
     &       0.0,      0.0,      0.0,         !  6 AS             
     &   0.00112,  0.02170, -0.00171,         !  7 LP             
     &       0.0,      0.0,      0.0,         !  8 ES             
     &       0.0,  0.00159,  0.00508,         !  9 AF             
     &   0.00681, -0.00580,  0.00935,         ! 10 PP (CI  PP  )            
     &       0.0,      0.0,      0.0,         ! 11 UJ             
     &       0.0,      0.0,      0.0,         ! 12 RM             
     &       0.0,      0.0,      0.0,         ! 13 BI             
     &       0.0,      0.0,      0.0,         ! 14 MM             
     &       0.0,      0.0,      0.0,         ! 15 NC             
     &       0.0,      0.0,      0.0,         ! 16 MC             
     &       0.0,      0.0,      0.0,         ! 17 OS             
     &       0.0,      0.0,      0.0/         ! 18 OH             

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISINT',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,10)ICYC
   10 FORMAT(' Begin/end MISINTTT: Cycle = ',I5)

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

