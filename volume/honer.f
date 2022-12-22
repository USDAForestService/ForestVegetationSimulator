      SUBROUTINE Voleq_Honer(VOLEQ,DBH,HT,MTOPP,MTOPS,
     & VOL,ERRFLG)
C      SUBROUTINE Voleq_Honer(ispc, dbh, ht, MinCUFTDBH,
C     & MinCUFTTop, MinBDFTDBH, MinBDFTTop, strVolEq,
C     & CalcCUFTVolume, CalcBDFTVolume, Calcerror)
C     YW 2019/04/03 This equation is added from BIA equations
C     It is using the NVEL equation number C00DVEE***
      INTEGER ispc, ERRFLG
      REAL MTOPP,MTOPS
      REAL dbh, ht, MinCUFTDBH, MinCUFTTop, MinBDFTDBH,
     & MinBDFTTop, CalcCUFTVolume, CalcBDFTVolume, VOL(15)
C      CHARACTER strVolEq
      CHARACTER(10) VOLEQ
      LOGICAL Calcerror

C      '------------------'
C      '                    '
C      '                      '
C      '                        '
C      '--------------------------'
C      '          Honer            '
C      '--------------------------'
C      '                        '
C      '                      '
C      '                    '
C      '------------------'
C     Honer, T.G. 1967. Standard volume tables and merchantable conversion factors 
C     for the commercial tree species of central and eastern Canada. 
C     Can. Dept. Forestry Rural Devel., For. Mgmt. Res. and Serv. Inst. Info. Rep. FMR-X-5.

      INTEGER SpeciesIndex
C     DBH
      REAL D
C     Top Diameter cubic foot
      REAL Tcu
C     Top Diameter Board foot
      REAL Tbd
C     Total Tree Height
      REAL Tht
C     holds species specific parameters
      REAL B(5)
C     Interum calculation for Honer
      REAL X1
C     Interum calculation for Honer
      REAL X2
      REAL X3, Vtops
C     Volume total in cubic feet for Honer
      REAL Vtcf
C     Volume in merchantable board feet for Honer
      REAL Vmbf
C     Volume in merchantable cubic feet for Honer
      REAL Vmcf





      REAL Scribner_Vol
      REAL CubicFoot_Vol


C     '---------------------------------------------------------'
C     ' Definitions used by: Honer                              '
C     '---------------------------------------------------------'
C     Dim HonerTotalCUFT1(29)
C     Dim HonerTotalCUFT2(29)
C     Dim HonerMerchCUFT1(29)
C     Dim HonerMerchCUFT2(29)
C     Dim HonerBDFT(2)
C     Dim HonerAll(4)


C     '-----------------------------'
C     ' A list of all species names '
C     '-----------------------------'
      CHARACTER*14, HonerASpecies(29)

C     '-----------------------------'
C     ' A list of all species codes '
C     '-----------------------------'
      INTEGER HonerIspecies(29)

      REAL HonerTotalCUFT1(29)

      REAL HonerTotalCUFT2(29)
      REAL HonerMerchCUFT1(29)
      REAL HonerMerchCUFT2(29)
      REAL HonerMerchCUFT3(29)
      REAL HonerBDFT(3)
      real HonerAll(5)
      REAL Stump
      INTEGER intConifer
      CHARACTER txtset(4), txtset2(4), txtEqn(4)
      DOUBLE PRECISION aCoeff
      DOUBLE PRECISION bCoeff

      INTEGER RNDINT

      HonerASpecies = [('BALSAM FIR    '), ('TAMARACK      '),
     & ('WHITE SPRUCE  '), ('BLACK SPRUCE  '), ('RED SPRUCE    '),
     & ('JACK PINE     '), ('RED PINE      '), ('WHITE PINE    '),
     & ('N WHITE-CEDAR '), ('HEMLOCK       '), ('STRIPED MAPLE '),
     & ('SOFT MAPLE    '), ('SILVER MAPLE  '), ('HARD MAPLE    '),
     & ('YELLOW BUCKEYE'), ('YELLOW BIRCH  '), ('PAPER BIRCH   '),
     & ('GRAY BIRCH    '), ('BEECH         '), ('WHITE ASH     '),
     & ('BLACK ASH     '), ('COTTONWOOD    '), ('BIGTOOTH ASPEN'),
     & ('QUAKING ASPEN '), ('BLACK CHERRY  '), ('N. RED OAK    '),
     & ('BASSWOOD      '), ('MISC COM      '), ('MISC HARDWOOD ')]

      DATA HonerIspecies /12,71,94,95,97,105,125,129,
     &                   241,261,315,316,317,318,330,
     &                   371,375,379,531,541,543,742,
     &                   743,746,762,833,951,990,999/



      DATA HonerTotalCUFT1 /2.139, 2.139, 1.44, 1.588, 1.226, 0.897,
     &                     0.71, 0.691, 4.167, 1.112, 1.046, 1.046,
     &                     1.046, 1.046, 1.046, 1.449, 2.222, 2.222,
     &                     0.959, 0.033, 0.033, -0.312, -0.312,
     &                   - 0.312, 0.033, 1.512, 0.948, 1.046, 1.046/



      DATA HonerTotalCUFT2 /301.634, 301.634, 342.175, 333.364,
     &                      315.832, 348.53, 355.623, 363.676,
     &                      244.906, 350.092, 383.972, 383.972,
     &                      383.972, 383.972, 383.972, 344.754,
     &                      300.373, 300.373, 334.829, 393.336,
     &                      393.336, 436.683, 436.683, 436.683,
     &                      393.336, 336.972, 401.456, 383.972,
     &                      383.972/



      DATA HonerMerchCUFT1 /0.9352, 0.9352, 0.9611, 0.9644, 0.9644,
     &                      0.9635, 0.9672, 0.9735, 0.9645, 0.9645,
     &                      0.9057, 0.9057, 0.9057, 0.9057, 0.9057,
     &                      0.8778, 0.9087, 0.9087, 0.9057, 0.9057,
     &                      0.9057, 0.9354, 0.9354, 0.9354, 0.9057,
     &                      0.9057, 0.9057, 0.9057, 0.9057/


      DATA HonerMerchCUFT2 /-0.0395, -0.0395, -0.2456, -0.0995,
     &                      -0.0995, -0.15, -0.0393, -0.2346,
     &                      -0.1616, -0.1616, -0.0708, -0.0708,
     &                      -0.0708, -0.0708, -0.0708, -0.2417,
     &                      -0.3049, -0.3049, -0.0708, -0.0708,
     &                      -0.0708, 0.0957, 0.0957, 0.0957,
     &                      -0.0708, -0.0708, -0.0708, -0.0708,
     &                      -0.0708/


      DATA HonerMerchCUFT3 /-0.8147, -0.8147, -0.6801, -0.7658,
     &                      -0.7658, -0.8081, -1.0523, -0.7378,
     &                      -0.7945, -0.7945, -0.8375, -0.8375,
     &                      -0.8375, -0.8375, -0.8375, -0.5247,
     &                      -0.5107, -0.5107, -0.8375, -0.8375,
     &                      -0.8375, -1.1613, -1.1613, -1.1613,
     &                      -0.8375, -0.8375, -0.8375, -0.8375,
     &                      -0.8375/


      DATA HonerBDFT /5.4332, -1.6281, -4.471/

C     'Parameter set to use for situation of unrecognized species
      DATA HonerAll /1.046, 383.972, 0.9604, -0.166, -0.7868/

C     'WARNING: HARDCODED ASSUMPTION OF HALF FOOT STUMP HEIGHT!!!
      Stump = 0.5


C     Initiate variables (YW 2019/04/03)
      READ(VOLEQ(8:10),'(i3)') ispc
      IF(MTOPP.LE.0.0)THEN
        IF(ispc.LT.300)THEN
          MTOPP = 7.0
        ELSE
          MTOPP = 9.0
        ENDIF
      ENDIF
      IF(MTOPS.LE.0.0) MTOPS = 4.0
      MinCUFTDBH = 0.0
      MinCUFTTop = MTOPP
      MinBDFTDBH = 0.0
      MinBDFTTop = MTOPP
      
C     'Set softwood species

      intConifer = 0

C     'Conifer
      IF (ispc.LT.300) THEN
          intConifer = 1
C     'Hardwood
      ELSE
          intConifer = 0
      ENDIF


      txtset = "xxxx"
      txtset2 = "xxxx"
      txtEqn = "xxxx"

C      ''*************************************************
C      ''  Declaring coefficients and calling the
C      ''  NatLog subroutine
C      ''*************************************************


      Scribner_Vol = 0
      CubicFoot_Vol = 0

C     'Use variables in original program for now
      D = dbh
      Tht = ht

      aCoeff = 0
      bCoeff = 0


      SELECT CASE (ispc)
C     'balsam fir
          CASE (12)
              aCoeff = 1.4280441
              bCoeff = 23.90997
              SpeciesIndex = 1
              txtEqn = "BFir"
C     'Spruce spp and Jack Pine
          CASE (89:100)
              aCoeff = -10.447476
              bCoeff = 28.757579
              SpeciesIndex = 2
              txtEqn = "Spr"
C     'Jack Pine
          CASE (105)
              aCoeff = -10.447476
              bCoeff = 28.757579
              SpeciesIndex = 3
              txtEqn = "Spr"
C     'Eastern White Pine using info from Eastern Units
C     M1 and M3 info
          CASE (129)
              aCoeff = -56.60902
              bCoeff = 46.367614
              SpeciesIndex = 4
              txtEqn = "WP"
C     'Cedar
          CASE (241)
              aCoeff = -7.8889417
              bCoeff = 23.178239
              SpeciesIndex = 5
              txtEqn = "Cedr"
C     'Hemlock
          CASE (261)
              aCoeff = -6.9185956
              bCoeff = 25.324268
              SpeciesIndex = 6
              txtEqn = "HEM"
C     'Red Maple
          CASE (316)
              aCoeff = 1.5686147
              bCoeff = 24.858361
              SpeciesIndex = 7
              txtEqn = "RED"
C     'Maple Spp
          CASE (315, 318)
              aCoeff = 0.68346361
              bCoeff = 25.53011
              txtEqn = "MAP"
C     'Yellow Birch
          CASE (371)
              aCoeff = 1.3424364
              bCoeff = 23.922973
              SpeciesIndex = 8
              txtEqn = "YB"
C     'White Birch, Gray Birch
          CASE (375)
              aCoeff = 1.6167322
              bCoeff = 25.132103
              SpeciesIndex = 9
              txtEqn = "WB"
C     'Beech
          CASE (531)
              aCoeff = -0.71135914
              bCoeff = 24.085024
              SpeciesIndex = 10
              txtEqn = "BEE"
C     'EASTERN ASH
          CASE (540:545)
              aCoeff = -11.523509
              bCoeff = 31.470543
              SpeciesIndex = 12
              txtEqn = "ASH"
C     'Aspen/Cottonwood
          CASE (740:747)
              aCoeff = 8.4054744
              bCoeff = 24.099637
              SpeciesIndex = 13
              txtEqn = "POP"
C     'B. CHERRY
          CASE (762)
              aCoeff = -11.523509
              bCoeff = 31.470543
              SpeciesIndex = 14
              txtEqn = "ASH"
C     'USING ALL HARDWOODS
          CASE (751, 990, 999)
              aCoeff = 1.6167322
              bCoeff = 25.132103
              SpeciesIndex = 16
              txtEqn = "MISC"
C     'USING ALL HARDWOODS
          CASE DEFAULT
              aCoeff = 1.6167322
              bCoeff = 25.132103
              SpeciesIndex = 15
              txtEqn = "MISC"

C              'Print #6, "Equation n/a"; iSpc
      END SELECT
C     'If ispc = 315 Or ispc = 318 Then j = 7.5 'Maple Spp

      B(1) = HonerTotalCUFT1(SpeciesIndex)
      B(2) = HonerTotalCUFT2(SpeciesIndex)
      B(3) = HonerMerchCUFT1(SpeciesIndex)
      B(4) = HonerMerchCUFT2(SpeciesIndex)
      B(5) = HonerMerchCUFT3(SpeciesIndex)

C      '   Print #6, "Honer#1"; ispc, SpeciesIndex, B(0), B(1)
C      '--------------------------------------------------------'
C      ' Calculate Cubic Foot Volume IF AND ONLY IF adequate
C      ' data is available '
C      '--------------------------------------------------------'

C     RNDINT integer used as a workaound to round result to
C     two decimal places



      Tcu = MinCUFTTop
      IF (Tht.GT.0.0) THEN
C     volume in total cuft
          Vtcf = REAL((D**2) / (B(1) + (B(2) / Tht)))
          VOL(1) = Vtcf
      ELSE
        RETURN
      ENDIF
      
      IF (D.GT.Tcu) THEN
C     a value that uses top diameter
          X2 = REAL(((Tcu / D)**2) * (1.0 + (Stump / Tht)))
C     volume in merch. cubic foot
          Vmcf = REAL(Vtcf * (B(3) + B(4) * X2 + B(5) * (X2**2)))
C          VOL(15) = Vtcf - vmcf
C     topwood calculation
         IF(MTOPS.GT.0.0.AND.MTOPP.GT.MTOPS)THEN
           X3 = REAL(((MTOPS / D)**2) * (1.0 + (Stump / Tht)))  
           Vtops = REAL(Vtcf * (B(3) + B(4) * X3 + B(5) * (X3**2)))    
           VOL(7) = Vtops - Vmcf
C           VOL(15) = Vtcf - Vtops
         ENDIF
      ELSE
          Vmcf = 0
      ENDIF

C      CalcCUFTVolume = REAL(Math.Round(Vmcf, 2))
      RNDINT = ANINT(Vmcf*100.0)
      CalcCUFTVolume = RNDINT/100.0
      VOL(4) = CalcCUFTVolume


      Tbd = MinBDFTTop
      IF(Tbd.LE.0.0) Tbd = 7.0

C    '----------------------------------------------------------'
C    ' Calculate Board Foot Volume IF AND ONLY IF adequate data
C    ' is available '
C    '----------------------------------------------------------'
      IF (D.GT.MinBDFTDBH) THEN
          IF (Tht.GT.0.0 .AND. Tbd.GT.0.0.AND.D.GT.Tbd) THEN
C          	  volume in total cuft
              Vtcf = REAL((D**2) / (B(1) + (B(2) / Tht)))
C             a value that uses top diameter
              X1 = REAL(((Tbd / D)**2) * (1.0 + (Stump / Tht)))
C             volume in merch board foot
              Vmbf = REAL(Vtcf * (HonerBDFT(1) + HonerBDFT(2) *
     &                    X1 + HonerBDFT(3) * (X1**2)))
          ELSE
              Vmbf = 0
          ENDIF
      ELSE
          Vmbf = 0
      ENDIF

C     CalcBDFTVolume = REAL(Math.Round(Vmbf, 2))
      RNDINT = ANINT(Vmbf*100.0)
      CalcBDFTVolume = RNDINT/100.0
      VOL(2) = CalcBDFTVolume

      END