      BLOCK DATA MPBLKD
      IMPLICIT NONE
C----------
C  **MPBLKD--CR   DATE OF LAST REVISION:  08/22/14
C----------
C
C     MOUNTAIN PINE BEETLE --
C     SEE MPBCUP OR MPBMOD FOR VARIABLE DISCRIPTIONS.
C
C Revision History
C     May-June, 2000 Glenn E. Brink
C       Added variables IDXWP,IDXWL,IDXDF,IDXLP and IDXPP, array indices of
C       White Pine, Western Larch, Douglas Fir, Lodgepole Pine and
C       Ponderosa Pine respectively.  Added to common block in file
C       MPBCOM.F77.
C       Added array MPBSPM to govern the computations in surfce.f by
C       species using an IF block as opposed to the old COMPUTED GO TO,
C       since the array allows the definition to be made in mpblkd.f,
C       instead of always having to change the COMPUTED GO TO.
C   	Added to common block in file MPBCOM.F77.
C   07/09/09 - Lance R. David (FMSC)
C     Update to Central Rockies expansion to 38 species.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C   08/22/14 Lance R. David (FMSC)
C     Function name was used as variable name.
C     changed variable INT to INCRS
C----------------------------------------------------------------------
C
COMMONS

      INCLUDE 'PRGPRM.F77'

      INCLUDE 'MPBCOM.F77'

      DATA  JOMPB  / 7 /

      DATA IPLTNO/ 1 /,IMPROB/ 1 /,NATR/ 2 /, KEYMPB/ 2,3,6*0,1 /,
     >     INCRS/ 10 /

C----------
C   CENTRAL ROCKIES VARIANT 38 SPECIES LIST.
C
C   #  Code  Common Name             Scientific Name
C   1  AF    subalpine fir           Abies lasiocarpa var. lasiocarpa
C   2  CB    corkbark fir            Abies lasiocarpa var. arizonica
C   3  DF    Douglas-fir             Pseudotsuga menziesii
C   4  GF    grand fir               Abies grandis
C   5  WF    white fir               Abies concolor
C   6  MH    mountain hemlock        Tsuga mertensiana
C   7  RC    western redcedar        Thuja plicata
C   8  WL    western larch           Larix occidentalis
C   9  BC    bristlecone pine        Pinus aristata
C   10 LM    limber pine             Pinus flexilis var. flexilis
C   11 LP    lodgepole pine          Pinus contorta
C   12 PI    common pinyon pine      Pinus edulis
C   13 PP    ponderosa pine          Pinus ponderosa
C   14 WB    whitebark pine          Pinus albicaulis
C   15 SW    Southwestern white pine Pinus strobiformis
C   16 UJ    Utah juniper            Juniperus osteosperma  
C   17 BS    blue spruce             Picea pungens
C   18 ES    Engelmann spruce        Picea engelmannii
C   19 WS    white spruce            Picea glauca
C   20 AS    quaking aspen           Populus tremuloides
C   21 NC    narrowleaf cottonwood   Populus angustifolia
C   22 PW    plains cottonwood       Populus delltoides var. monolifera
C   23 GO    Gambel oak              Quercus gambelii  
C   24 AW    Arizona white oak       Quercus arizonica  
C   25 EM    Emory oak               Quercus emoryi  
C   26 BK    Bur Oak                 Quercus macrocarpa
C   27 SO    silverleaf oak          Quercus hypoleucoides  
C   28 PB    paper birch             Betula papyrifera
C   29 AJ    Alligator juniper       Juniperus deppeana  
C   30 RM    Rocky Mountain juniper  Juniperus scopulorum  
C   31 OJ    Oneseed juniper         Juniperus monosperma  
C   32 ER    Eastern Redcedar        Juniperus virginiana
C   33 PM    Singleleaf pinyon       Pinus monophylla  
C   34 PD    Border pinyon           Pinus discolor  
C   35 AZ    Arizona pinyon pine     Pinus monophylla var. fallax
C   36 CI    Chihuahua pine          Pinus leiophylla var. chihuahuana
C   37 OS    other softwoods         
C   38 OH    other hardwoods         

      DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/15,8,3,11,13/

C     Use appropriate surrogate species for the calculations in surfce.f
C                 AF CB DF GF WF MH RC WL BC LM LP PI PP WB SW  -- CR species
      DATA MPBSPM/ 3, 3, 3, 3, 3, 8, 8, 8, 8, 8,11, 8,13, 8,15,
C                 UJ BS ES WS AS NC PW GO AW EM BK SO PB AJ RM  -- CR species
     &             8,15,15,15, 3, 3, 3, 3, 3, 3, 3, 3, 3, 8, 8, 
C                 OJ ER PM PD AZ CI OS OH  -- CR species
     &             8, 8, 8, 8, 8,13, 8, 3/ 

      END
