      SUBROUTINE BRIBA
C**********************************************************************
C  **BRIBA        DATE OF LAST REVISION:  05/01/1994
C----------------------------------------------------------------------
C  Purpose:
C  BRIBA calculates the Rust Index based on Basal Area.
C
C  The functions used to determine the rust index from basal area were
C  provided by Geral McDonald, Intermountain Research Station, Moscow,
C  ID. Additional documentation can be found in Research Paper INT-258,
C  Computer Simulation of White Pine Blister Rust Epidemics, pp. 81-83.
C  Julie Williams-Cipriani provided assistance with the implementation
C  of these functions in the model code.
C
C  Rust Index is calculated once per cycle. The calculation is a three
C  step process.  Proportion Full Sunlight (PFS) is calculated first,
C  then Ribes Denity (RD) is calculated from PFS, and finally the
C  Rust Index (RI) is calculated.
C
C  BRIBA is called from BRSETP at initialization (cycle 0) and from
C  BRTREG each cycle depending on RI assignment method.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'CONTRL.F77'

C.... Local variable declarations.
C....    BRI    - Blister Rust Index.
C....    RD     - Array of 3. Ribes Density (Hud, Lac, Vis)
C....    RDP    - Ribes density multiplied by proportion of total pop.
C....    PFS    - Proportion Full sunlight
C....    I      - Counter for ribes species index.

      REAL PFS,RD(3),BRI,RDP
      INTEGER I
      LOGICAL DEBUG

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRIBA',5,ICYC)
      IF(DEBUG) WRITE(JOSTND,10) ICYC
   10 FORMAT(' Entering subroutine BRIBA: cycle = ',I2)

C.... Calculate Proportion Full Sunlight using the FVS current
C.... basal area (BA from common block PLOT);
C.... BA is stored in FVS as feet**2/acre --  the original calculation
C.... was based on meters**2/hectare.  The .00459 constant comes from
C.... dividing the original .02 by 4.36 which is the conversion factor
C.... from m2/ha to f2/ac.

      PFS=EXP(-(0.00459*BA))

C.... Calculate Ribes Density for all three Ribes species.
C.... The original functions are hectares; therefore, they are
C.... divided by 2.47 for conversion to acres.
C.... (bushes/hectare divided by acres/hectare = bushes/acre)

      RD(1)=(0.05+(2.15*(PFS**16.38)))/2.47
      RD(2)=(40.0+(190.0*(PFS**10.96)))/2.47
      RD(3)=(40.0+(660.0*(PFS**27.03)))/2.47

C.... Calculate Rust Index value. Weighted average is applied to
C.... ribes density before calculation of rust index.

      RIDEF=0.0
      DO 50 I=1,3

C....    The ribes population values (RIBPRP) are the proportions listed
C....    in fields 4, 5, and 6 of the RUSTINDX keyword.
C....    If RIBPRP is 0 for any of the ribes species, don't do the
C....    calculation for that species (even when a 0 is entered for RDP
C....    you get a value anyway for BRI and it skews the rust index RI).

         IF(RIBPRP(I).GT.0) THEN
            RDP=RD(I)*RIBPRP(I)
            BRI=(0.499675+0.4*ATAN((RDP/150.0)-3))*RSF(I)
            RIDEF=RIDEF+BRI
         ENDIF
   50 CONTINUE

C.... Common return.

      IF(DEBUG) WRITE(JOSTND,60) RIDEF,ICYC
   60 FORMAT(27X,'RIDEF = ',F10.8,/,
     &       ' Leaving subroutine BRIBA: cycle = ',I2)
      RETURN
      END
