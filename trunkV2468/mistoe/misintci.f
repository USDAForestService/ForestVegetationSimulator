      SUBROUTINE MISINT
      IMPLICIT NONE
C----------
C MISTOE $Id: misint.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
C----------
C  Purpose:
C  Mistletoe parameter initialization routine. This routine is
C  variant dependent and sets the variant dependent variables for other
C  mistletoe routines. This is the Central Idaho version.
C----------
C
C  Call list definitions:
C
C  Local variable definitions:
C     DEBUG:  Logical flag to turn debug on and off.
C     AFIT:   Array of MISFIT data.
C     ACSP:   Array of CSPARR data.
C     ADGP:   Array of DGPMDR data.
C     APMC:   Array of PMCSP data.
C
C  Common block variables and parameters:
C     CSPARR: From MISCOM; 2-char. representations of all species.
C     DGPDMR: From MISCOM; diameter growth potentials based on species
C                and DMR (0-6).
C     ICYC:   From CONTRL; cycle index number.
C     JOSTND: From CONTRL; logical unit number for stand output.
C     MISFIT: From MISCOM; tells which species are affected by DM.
C     PMCSP:  From MISCOM; percent mortality coefficients by species.
C
C  12-JUL-2011 Lance R. David (FMSC)
C    Added arrays for height growth impacts.
C    Impact values must be supplied by MistHMod keyword.
C
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
C
      LOGICAL DEBUG
      CHARACTER*2 ACSP(MAXSP)
      INTEGER I,J,AFIT(MAXSP)
      REAL ADGP(MAXSP,7),AHGP(MAXSP,7),APMC(MAXSP,3)
C----------
C     SPECIES LIST FOR CENTRAL IDAHO VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
C     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)                   ABIES GRANDIS
C     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
C     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
C     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
C     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
C    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
C    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
C    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
C    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
C    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
C    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
C    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
C    18 = OTHER SOFTWOODS (OS)
C    19 = OTHER HARDWOODS (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C  FROM THE IE VARIANT:
C      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
C      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
C      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
C      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)
C
C  FROM THE UT VARIANT:
C      USE 12(WJ) FOR 14(WJ)
C      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
C                                                  REALLY WC39=OT)
C----------
C  Data statements.
C
C  Species character representations
C----------
      DATA (ACSP(I),I=1,MAXSP)/
     & 'WP','WL','DF','GF','WH','RC','LP','ES','AF','PP',
     & 'WB','PY','AS','WJ','MC','LM','CW','OS','OH'/
C----------
C  Species affected by mistletoe
C----------
      DATA (AFIT(I),I=1,MAXSP)/
     &    1,   1,   1,   1,   0,   0,   1,   0,   1,   1,
     &    1,   0,   0,   0,   0,   1,   0,   0,   0/
C----------
C  Diameter growth rates
C----------
      DATA ((ADGP(I,J),J=1,7),I=1,MAXSP)/
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59,
     &   1.0, .94, .92, .88, .84, .58, .54,
     &   1.0, .98, .97, .85, .80, .52, .44,
     &   1.0, 1.0, 1.0, .98, .95, .70, .50,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, .98, .95, .70, .50,
     &   1.0, 1.0, 1.0, .98, .86, .73, .50,
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59, !LP (for WB)
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, .94, .80, .59, !LP (for LM)
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &   1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/
C----------
C  Mortality coefficients
C----------
      DATA ((APMC(I,J),J=1,3),I=1,MAXSP)/
     &   0.00112,  0.02170, -0.00171,
     &   0.01319, -0.01627,  0.00822,
     &   0.01319, -0.01627,  0.00822,
     &       0.0,  0.00159,  0.00508,
     &       0.0,      0.0,      0.0,
     &       0.0,      0.0,      0.0,
     &   0.00112,  0.02170, -0.00171,
     &       0.0,      0.0,      0.0,
     &       0.0,  0.00159,  0.00508,
     &   0.00681, -0.00580,  0.00935,
     &   0.00112,  0.02170,  -0.00171, ! LP (for WB)
     &       0.0,      0.0,      0.0,
     &       0.0,      0.0,      0.0,
     &       0.0,      0.0,      0.0,
     &       0.0,      0.0,      0.0,
     &   0.00112,  0.02170,  -0.00171, ! LP (for LM)
     &       0.0,      0.0,      0.0,
     &       0.0,      0.0,      0.0,
     &       0.0,      0.0,      0.0/
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
      DATA ((AHGP(I,J),J=1,7),I=1,MAXSP)
     &  /1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
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
C----------
C  Check for debug.
C----------
      CALL DBCHK(DEBUG,'MISINT',6,ICYC)
C
      IF(DEBUG) WRITE(JOSTND,10)ICYC
   10 FORMAT(' Begin/end MISINTCI: Cycle = ',I5)
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
