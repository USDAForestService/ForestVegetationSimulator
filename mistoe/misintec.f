      SUBROUTINE MISINT
      IMPLICIT NONE
***********************************************************************
C MISTOE $Id$
*----------------------------------------------------------------------
*  Purpose:
*     Mistletoe parameter initialization routine. This routine is
*  variant dependent and sets the variant dependent variables for other
*  mistletoe routines. This is the East Cascades version.
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
*     03/01/95 - Lance R. David (MAG)
*                Exception values for growth modification replaced by
*                values described in the Interim Dwarf Mistletoe Impact
*                Modeling System Users Guide and Reference Manual.
*  12-JUL-2011 Lance R. David (FMSC)
*    Added arrays for height growth impacts.
*    Impact values must be supplied by MistHMod keyword.
*  01-JAN-2012 Gary Dixon (FMSC)
*    Expanded arrays to accomodate 32 species
*    Formatted routine to comply with standard FVS coding practices
*
***********************************************************************
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'MISCOM.F77'
C
COMMONS
C----------
C  Variable declarations.
C----------
      LOGICAL DEBUG
      CHARACTER*2 ACSP(MAXSP)
      INTEGER I,J,AFIT(MAXSP)
      REAL ADGP(MAXSP,7),AHGP(MAXSP,7),APMC(MAXSP,3)
C----------
C  Data statements.
C
C  Species character representations
C----------
      DATA (ACSP(I),I=1,MAXSP)/
     & 'WP', 'WL', 'DF', 'SF', 'RC', 'GF', 'LP', 'ES', 'AF', 'PP',
     & 'WH', 'MH', 'PY', 'WB', 'NF', 'WF', 'LL', 'YC', 'WJ', 'BM',
     & 'VN', 'RA', 'PB', 'GC', 'DG', 'AS', 'CW', 'WO', 'PL', 'WI',
     & 'OS', 'OH'/
C----------
C  Species affected by mistletoe
C----------
      DATA (AFIT(I),I=1,MAXSP)/
     &    1,    1,    1,    1,    0,    1,    1,    0,    1,    1,
     &    1,    1,    0,    0,    1,    1,    0,    0,    0,    0,
     &    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     &    0,    0/
C----------
C  Diameter growth rates
C  03/01/95 - Lance R. David (MAG)
C  Exception values replaced by values described in 
C    Interim Dwarf Mistletoe Impact Modeling System
C    Users Guide and Reference Manual, February, 1993
C    Pages 20-24 for species DF, SF, GF and PP.
C    The original values found on page 31 were:
C      &   1.0,1.0,1.0,1.0,.82,.73,.63,
C      &   1.0,.93,.93,.93,.93,.93,.93,
C      &   1.0,1.0,1.0,.84,.84,.84,.84,
C      &   1.0,1.0,1.0,.78,.78,.78,.78,
C----------
      DATA ((ADGP(I,J),J=1,7),I=1,MAXSP)/
     &  1.0,  1.0,  1.0,  1.0,  .94,  .80,  .59,    ! 1=WP
     &  1.0,  .94,  .92,  .88,  .84,  .58,  .54,    ! 2=WL
     &  1.0,  .98,  .97,  .85,  .80,  .52,  .44,    ! 3=DF
     &  1.0,  1.0,  1.0,  .98,  .95,  .70,  .50,    ! 4=SF
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    ! 5=RC
     &  1.0,  1.0,  1.0,  .98,  .95,  .70,  .50,    ! 6=GF
     &  1.0,  1.0,  1.0,  1.0,  .94,  .80,  .59,    ! 7=LP
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    ! 8=ES
     &  1.0,  1.0,  1.0,  .98,  .95,  .70,  .50,    ! 9=AF
     &  1.0,  1.0,  1.0,  .98,  .86,  .73,  .50,    !10=PP
     &  1.0,  1.0,  1.0,  1.0,  .94,  .80,  .59,    !11=WH
     &  1.0,  1.0,  1.0,  .98,  .86,  .73,  .50,    !12=MH
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !13=PY
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !14=WB
     &  1.0,  1.0,  1.0,  .98,  .95,  .70,  .50,    !15=NF
     &  1.0,  1.0,  1.0,  .98,  .95,  .70,  .50,    !16=WF
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !17=LL
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !18=YC
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !19=WJ
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !20=BM
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !21=VN
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !22=RA
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !23=PB
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !24=GC
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !25=DG
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !26=AS
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !27=CW
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !28=WO
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !29=PL
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !30=WI
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !31=OS
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0/    !32=OH
C----------
C  Height growth potential rates
C
C  Using Douglas-fir height growth impact values described in:
C
C  Marshall, Katy 2007. Permanent plots for measuring spread and
C  impact of Douglas-fir dwarf mistletoe in the Southern Oregon
C  Cascades, Pacific Northwest Region: Results of the ten year
C  remeasurement. USDA Forest Service, Pacific Northwest Region,
C  Southwest Oregon Forest Insect and Disease Service Center, 
C  Central Point, Oregon. SWOFIDSC-07-04. 34 pp.
C
C  Default values for DF in this table would be:
C  &   1.0,1.0,1.0,.95,.65,.50,.10,
C  So that impacts are not unknowingly applied to projections,
C  the values must be supplied with the MistHMod keyword.
C  when appropriat default values are developed, they will be
C  set here.
C----------
      DATA ((AHGP(I,J),J=1,7),I=1,MAXSP)/
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    ! 1=WP
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    ! 2=WL
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    ! 3=DF
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    ! 4=SF
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    ! 5=RC
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    ! 6=GF
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    ! 7=LP
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    ! 8=ES
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    ! 9=AF
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !10=PP
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !11=WH
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !12=MH
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !13=PY
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !14=WB
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !15=NF
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !16=WF
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !17=LL
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !18=YC
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !19=WJ
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !20=BM
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !21=VN
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !22=RA
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !23=PB
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !24=GC
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !25=DG
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !26=AS
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !27=CW
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !28=WO
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !29=PL
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !30=WI
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,    !31=OS
     &  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0/    !32=OH
C----------
C  Mortality coefficients
C----------
      DATA ((APMC(I,J),J=1,3),I=1,MAXSP)/
     &  0.00112,  0.02170, -0.00171,    ! 1=WP
     &  0.01319, -0.01627,  0.00822,    ! 2=WL
     &  0.01319, -0.01627,  0.00822,    ! 3=DF
     &      0.0,  0.00159,  0.00508,    ! 4=SF
     &      0.0,      0.0,      0.0,    ! 5=RC
     &      0.0,  0.00159,  0.00508,    ! 6=GF
     &  0.00112,  0.02170, -0.00171,    ! 7=LP
     &      0.0,      0.0,      0.0,    ! 8=ES
     &      0.0,  0.00159,  0.00508,    ! 9=AF
     &  0.00681, -0.00580,  0.00935,    !10=PP
     &  0.00681, -0.00580,  0.00935,    !11=WH
     &  0.00681, -0.00580,  0.00935,    !12=MH
     &      0.0,      0.0,      0.0,    !13=PY
     &      0.0,      0.0,      0.0,    !14=WB
     &      0.0,  0.00159,  0.00508,    !15=NF
     &      0.0,  0.00159,  0.00508,    !16=WF
     &      0.0,      0.0,      0.0,    !17=LL
     &      0.0,      0.0,      0.0,    !18=YC
     &      0.0,      0.0,      0.0,    !19=WJ
     &      0.0,      0.0,      0.0,    !20=BM
     &      0.0,      0.0,      0.0,    !21=VN
     &      0.0,      0.0,      0.0,    !22=RA
     &      0.0,      0.0,      0.0,    !23=PB
     &      0.0,      0.0,      0.0,    !24=GC
     &      0.0,      0.0,      0.0,    !25=DG
     &      0.0,      0.0,      0.0,    !26=AS
     &      0.0,      0.0,      0.0,    !27=CW
     &      0.0,      0.0,      0.0,    !28=WO
     &      0.0,      0.0,      0.0,    !29=PL
     &      0.0,      0.0,      0.0,    !30=WI
     &      0.0,      0.0,      0.0,    !31=OS
     &      0.0,      0.0,      0.0/    !32=OH
C----------
C  Check for debug.
C----------
      CALL DBCHK(DEBUG,'MISINT',6,ICYC)
C
      IF(DEBUG) WRITE(JOSTND,10)ICYC
   10 FORMAT(' Begin/end MISINTEC: Cycle = ',I5)
C----------
C  Mistletoe model initializations.
C----------
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
C
      RETURN
      END
