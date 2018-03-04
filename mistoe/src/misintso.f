      SUBROUTINE MISINT
***********************************************************************
C MISTOE $Id: misint.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
*----------------------------------------------------------------------
*  Purpose:
*     Mistletoe parameter initialization routine. This routine is
*  variant dependent and sets the variant dependent variables for other
*  mistletoe routines. This is the SORNEC version.
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
*  09/24/04 - Lance R. David (FHTET)
*    This routine was created from existing diameter growth rates
*    and mortality coefficients from the original SO variant and
*    from the CA, EC, WC, TT and UT variants for new SO species.
*  04-APR-2009 Lance R. David (FMSC)
*    Changed species code JU to WJ (Western Juniper)
*  08-JUL-2011 Lance R. David (FMSC)
*    Added arrays for height growth impacts.
*    Impact values must be supplied by MistHMod keyword.
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
C....
      DATA (ACSP(I),I=1,MAXSP)
C....    1    2    3    4    5    6    7    8    9    10   11
     & /'WP','SP','DF','WF','MH','IC','LP','ES','SH','PP','WJ',
C....    12   13   14   15   16   17   18   19   20   21   22
     &  'GF','AF','SF','NF','WB','WL','RC','WH','PY','WA','RA',
C....    23   24   25   26   27   28   29   30   31   32   33
     &  'BM','AS','CW','CH','WO','WI','GC','MC','MB','OS','OH'/

C.... Species affected by mistletoe

      DATA (AFIT(I),I=1,MAXSP)
     & /  1,   1,   1,   1,   1,   0,   1,   0,   1,   1,   0,
     &    1,   1,   1,   1,   1,   1,   0,   1,   0,   0,   0,
     &    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0/

C.... Diameter growth rates
C....    values described in:
C....    Interim Dwarf Mistletoe Impact Modeling System
C....    Users Guide and Reference Manual, February, 1993

      DATA ((ADGP(I,J),J=1,7),I=1,MAXSP)
     &  /1.0,1.0,1.0,1.0,.94,.80,.59,   ! WP 1
     &   1.0,1.0,1.0,1.0,.94,.80,.59,   ! SP 2
     &   1.0,.98,.97,.85,.80,.52,.44,   ! DF 3
     &   1.0,1.0,1.0,.98,.95,.70,.50,   ! WF 4
     &   1.0,1.0,1.0,.98,.86,.73,.50,   ! MH 5
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! IC 6
     &   1.0,1.0,1.0,1.0,.94,.80,.59,   ! LP 7
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! ES 8
     &   1.0,1.0,1.0,.98,.95,.70,.50,   ! SH 9
     &   1.0,1.0,1.0,.98,.86,.73,.50,   ! PP 10
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WJ 11
     &   1.0,1.0,1.0,.98,.95,.70,.50,   ! GF 12
     &   1.0,1.0,1.0,.98,.95,.70,.50,   ! AF 13
     &   1.0,1.0,1.0,.98,.95,.70,.50,   ! SF 14
     &   1.0,1.0,1.0,.98,.95,.70,.50,   ! NF 15
     &   1.0,1.0,1.0,1.0,.94,.80,.59,   ! WB 16
     &   1.0,.94,.92,.88,.84,.58,.54,   ! WL 17
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! RC 18
     &   1.0,1.0,1.0,1.0,.94,.80,.59,   ! WH 19
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! PY 20
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WA 21
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! RA 22
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! BM 23
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! AS 24
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! CW 25
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! CH 26
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WO 27
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WI 28
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! GC 29
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! MC 30
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! MB 31
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! OS 32
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/   ! OH 33

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
     &  /1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WP 1
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! SP 2
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! DF 3
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WF 4
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! MH 5
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! IC 6
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! LP 7
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! ES 8
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! SH 9
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! PP 10
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WJ 11
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! GF 12
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! AF 13
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! SF 14
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! NF 15
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WB 16
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WL 17
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! RC 18
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WH 19
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! PY 20
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WA 21
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! RA 22
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! BM 23
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! AS 24
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! CW 25
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! CH 26
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WO 27
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! WI 28
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! GC 29
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! MC 30
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! MB 31
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! OS 32
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/   ! OH 33

C.... Mortality coefficients

      DATA ((APMC(I,J),J=1,3),I=1,MAXSP)
     &  /0.00112,  0.02170, -0.00171,  ! WP 1 
     &   0.00112,  0.02170, -0.00171,  ! SP 2 
     &   0.01319, -0.01627,  0.00822,  ! DF 3 
     &   0.0,      0.00159,  0.00508,  ! WF 4 
     &   0.00681, -0.00580,  0.00935,  ! MH 5 
     &   0.0,0.0,0.0,                  ! IC 6 
     &   0.00112,  0.02170, -0.00171,  ! LP 7 
     &   0.0,0.0,0.0,                  ! ES 8 
     &   0.0,      0.00159,  0.00508,  ! SH 9 
     &   0.00681, -0.00580,  0.00935,  ! PP 10
     &   0.0,0.0,0.0,                  ! JU 11
     &   0.0,      0.00159,  0.00508,  ! GF 12
     &   0.0,      0.00159,  0.00508,  ! AF 13
     &   0.0,      0.00159,  0.00508,  ! SF 14
     &   0.0,      0.00159,  0.00508,  ! NF 15
     &   0.00112,  0.02170, -0.00171,  ! WB 16
     &   0.01319, -0.01627,  0.00822,  ! WL 17
     &   0.0,0.0,0.0,                  ! RC 18
     &   0.00681, -0.00580,  0.00935,  ! WH 19
     &   0.0,0.0,0.0,                  ! PY 20
     &   0.0,0.0,0.0,                  ! WA 21
     &   0.0,0.0,0.0,                  ! RA 22
     &   0.0,0.0,0.0,                  ! BM 23
     &   0.0,0.0,0.0,                  ! AS 24
     &   0.0,0.0,0.0,                  ! CW 25
     &   0.0,0.0,0.0,                  ! CH 26
     &   0.0,0.0,0.0,                  ! WO 27
     &   0.0,0.0,0.0,                  ! WI 28
     &   0.0,0.0,0.0,                  ! GC 29
     &   0.0,0.0,0.0,                  ! MC 30
     &   0.0,0.0,0.0,                  ! MB 31
     &   0.0,0.0,0.0,                  ! OS 32
     &   0.0,0.0,0.0/                  ! OH 33

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISINT',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,10)ICYC
   10 FORMAT(' Begin/end MISINTSO: Cycle = ',I5)

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
