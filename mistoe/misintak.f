      SUBROUTINE MISINT
***********************************************************************
C MISTOE $Id$
*----------------------------------------------------------------------
*  Purpose:
*     Mistletoe parameter initialization routine. This routine is
*  variant dependent and sets the variant dependent variables for other
*  mistletoe routines. This is the (Alaska) version.
*
*  Host species are: SF, AF, LP, WH, MH and set based on:
*
*  Forest Health Protection, Alaska Region, USDA Forest Service.
*     April 2019. Pocket Guide for the Identification of Common Forest
*     Diseases and Insects in Alaska Forest Service Alaska Region
*       
*  Hennon, Paul E.; Beatty, Jerome S.; Hildebrand, Diane. January 2001.
*      Forest Insect and Disease Leaflet. Hemlock Dwarf Mistletoe
*
* Prepared for 23 species AK variant by Lance David 06/03/2019
*----------
* SPECIES LIST FOR ALASKA VARIANT.
*
*     Mistletoe
*       Host
* Number --  Code  Common Name         FIA  PLANTS Scientific Name
*   1    dm  SF   Pacific silver fir  011  ABAM   Abies amabilis
*   2    dm  AF   subalpine fir       019  ABLA   Abies lasiocarpa
*   3        YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
*   4        TA   tamarack            071  LALA   Larix laricina
*   5        WS   white spruce        094  PIGL   Picea glauca
*   6        LS   Lutz’s spruce            PILU   Picea lutzii
*   7        BE   black spruce        095  PIMA   Picea mariana
*   8        SS   Sitka spruce        098  PISI   Picea sitchensis
*   9    dm  LP   lodgepole pine      108  PICO   Pinus contorta
*  10        RC   western redcedar    242  THPL   Thuja plicata
*  11    dm  WH   western hemlock     263  TSHE   Tsuga heterophylla
*  12    dm  MH   mountain hemlock    264  TSME   Tsuga mertensiana
*  13        OS   other softwoods     298  2TE
*  14        AD   alder species       350  ALNUS  Alnus species
*  15        RA   red alder           351  ALRU2  Alnus rubra
*  16        PB   paper birch         375  BEPA   Betula papyrifera
*  17        AB   Alaska birch        376  BENE4  Betula neoalaskana
*  18        BA   balsam poplar       741  POBA2  Populus balsamifera
*  19        AS   quaking aspen       746  POTR5  Populus tremuloides
*  20        CW   black cottonwood    747  POBAT  Populus trichocarpa
*  21        WI   willow species      920  SALIX  Salix species
*  22        SU   Scouler’s willow    928  SASC   Salix scouleriana
*  23        OH   other hardwoods     998  2TD
*----------------------------------------------------------------------
*
*  Call list definitions:
*
*  Local variable definitions:
*     DEBUG:  Logical flag to turn debug on and off.
*     AFIT:   Array of MISFIT data.
*     ACSP:   Array of CSPARR data.
*     ADGP:   Array of DGPMDR data.
*     AHGP:   Array of HGPMDR data.
*     APMC:   Array of PMCSP data.
*
*  Common block variables and parameters:
*     CSPARR: From MISCOM; 2-char. representations of all species.
*     DGPDMR: From MISCOM; diameter growth potentials based on species
*                and DMR (0-6).
*     HGPDMR: From MISCOM; height growth potentials based on species
*                and DMR (0-6).
*     ICYC:   From CONTRL; cycle index number.
*     JOSTND: From CONTRL; logical unit number for stand output.
*     MISFIT: From MISCOM; tells which species are affected by DM.
*     PMCSP:  From MISCOM; percent mortality coefficients by species.
*
*  12-JUL-2011 Lance R. David (FMSC)
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

      DATA (ACSP(I),I=1,13)
     &  /'SF','AF','YC','TA','WS','LS','BE','SS','LP','RC',
     &   'WH','MH','OS','AD','RA','PB','AB','BA','AS','CW',
     &   'WI','SU','OH'/


C.... Species affected by mistletoe

      DATA (AFIT(I),I=1,13)
     &   / 1,   1,   0,   0,   0,   0,   0,   0,   1,   0,
     &     1,   1,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0 /

C.... Diameter growth rates

      DATA ((ADGP(I,J),J=1,7),I=1,MAXSP)
     &  /1.0,1.0,1.0,.98,.95,.70,.50,
     &   1.0,1.0,1.0,.98,.95,.70,.50,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,.98,.95,.70,.50,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,.94,.80,.59,
     &   1.0,1.0,1.0,.98,.86,.73,.50,
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
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/

C.... Mortality coefficients

      DATA ((APMC(I,J),J=1,3),I=1,MAXSP)
     &  /0.0,0.00159,0.00508,
     &   0.0,0.00159,0.00508,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.00112,0.02170,-0.00171,
     &   0.0,0.0,0.0,
     &   0.00112,0.02170,-0.00171,
     &   0.00681,-0.00580,0.00935,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0,
     &   0.0,0.0,0.0/

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISINT',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,10)ICYC
   10 FORMAT(' Begin/end MISINTAK: Cycle = ',I5)

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
