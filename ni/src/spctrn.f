      SUBROUTINE SPCTRN (SPCIN, ISPC1)
      IMPLICIT NONE
C----------
C  **SPCTRN--NI  DATE OF LAST REVISION: 01/12/12
C----------
C  CALLED FROM INTREE, WHEN THE INPUT SPECIES CODE IS NOT RECOGNIZED.
C  THIS ROUTINE ASSIGNS THE MOST SIMILAR SPECIES SEQUENCE NUMBER TO THE
C  SPECIES NOT RECOGNIZED IN THE VARIANT.  TRANSLATES FVS ALPHA CODES,
C  FIA CODES, AND USDA PLANTS SYMBOLS.  THE ASPT ARRAY CONTAINS THE 
C  SPECIES MAPPING.  ALL SPECIES MAPPING WAS APPROVED BY THE FVS 
C  REGIONAL CONTACTS IN APRIL 2007. TO PROVIDE ADEQUATE CODE 
C  DOCUMENTATION, STANDARD FVS CODING FORMAT HAS BEEN RELAXED TO ALLOW
C  THE USE OF EXCLAMATION MARKS (!) TO PLACE COMMENTS AT THE
C  END OF VALID FORTRAN STATEMENTS.
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CONTRL.F77'      
C
COMMONS
C----------
      CHARACTER VVER*7
      CHARACTER*(*)SPCIN
      INTEGER MAXASPT, ISPC1, I, J, J2, IJSPIN
      PARAMETER (MAXASPT=516)
      CHARACTER*4 SPCOUT
      CHARACTER*8 ASPT(MAXASPT,20)
      CHARACTER VAR*2
C----------
C  DATA STATEMENT
C----------
C
      DATA ((ASPT(I,J),J=1,10),I=1,10) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'OF ','010','ABIES   ','SF ','GF ','WF ','AF ','AF ','GF ','AF ',    !Abies
     & 'SH ','021','ABSH    ','OS ','OS ','SH ','OS ','OS ','OT ','OS ',    !Abies ×shastensis
     & 'SF ','011','ABAM    ','SF ','OS ','SH ','OS ','OS ','SF ','OS ',    !Abies amabilis
     & 'SL ','014','ABBR    ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Abies bracteata
     & 'WF ','015','ABCO    ','OS ','GF ','WF ','GF ','WF ','GF ','AF ',    !Abies concolor
     & '   ','   ','ABCOC   ','OS ','GF ','WF ','GF ','WF ','GF ','AF ',    !Abies concolor var. concolor
     & '   ','   ','ABCOL   ','OS ','GF ','WF ','GF ','WF ','GF ','AF ',    !Abies concolor var. lowiana
     & 'GF ','017','ABGR    ','OS ','GF ','WF ','GF ','GF ','GF ','AF ',    !Abies grandis
     & 'AF ','019','ABLA    ','AF ','AF ','OS ','AF ','AF ','AF ','AF ',    !Abies lasiocarpa
     & 'CB ','018','ABLAA   ','OS ','AF ','OS ','AF ','CB ','AF ','OS '/    !Abies lasiocarpa var. arizonica
C
      DATA ((ASPT(I,J),J=11,20),I=1,10) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'GF ','GF ','WF ','GF ','SF ','WF ','AF ','AF ','SF ','WF ',         !Abies
     & 'OS ','OT ','RF ','OT ','RF ','SH ','OS ','WF ','RF ','RF ',         !Abies ×shastensis
     & 'OS ','OT ','RF ','OT ','SF ','SF ','OS ','OS ','SF ','SF ',         !Abies amabilis
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Abies bracteata
     & 'GF ','GF ','WF ','GF ','WF ','WF ','AF ','WF ','WF ','WF ',         !Abies concolor
     & 'GF ','GF ','WF ','GF ','WF ','WF ','AF ','WF ','WF ','WF ',         !Abies concolor var. concolor
     & 'GF ','GF ','WF ','GF ','WF ','WF ','AF ','WF ','WF ','WF ',         !Abies concolor var. lowiana
     & 'GF ','GF ','WF ','GF ','GF ','GF ','AF ','OS ','GF ','WF ',         !Abies grandis
     & 'AF ','AF ','OS ','AF ','AF ','AF ','AF ','AF ','AF ','OS ',         !Abies lasiocarpa
     & 'OS ','AF ','OS ','AF ','AF ','OS ','AF ','AF ','AF ','OS '/         !Abies lasiocarpa var. arizonica
C
      DATA ((ASPT(I,J),J=1,10),I=11,20) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','ABLAL   ','AF ','AF ','OS ','AF ','AF ','AF ','AF ',    !Abies lasiocarpa var. lasiocarpa
     & 'RF ','020','ABMA    ','OS ','OS ','RF ','OS ','OS ','OT ','OS ',    !Abies magnifica
     & 'NF ','022','ABPR    ','OS ','OS ','RF ','OS ','OS ','SF ','OS ',    !Abies procera
     & '   ','   ','ABMAS   ','OS ','OS ','SH ','OS ','OS ','OT ','OS ',    !Abies x shastensis
     & 'AA ','300','ACACI   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia
     & '   ','   ','ACAN    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia angustissima
     & '   ','   ','ACANS   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia angustissima var. shrevei
     & '   ','   ','ACANS2  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia angustissima var. suffrutescens
     & '   ','   ','ACANT4  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia angustissima var. texensis
     & '   ','   ','ACBA    ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Acacia baileyana
C
      DATA ((ASPT(I,J),J=11,20),I=11,20) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'AF ','AF ','OS ','AF ','AF ','AF ','AF ','AF ','AF ','OS ',         !Abies lasiocarpa var. lasiocarpa
     & 'OS ','OT ','RF ','OT ','RF ','SH ','OS ','WF ','RF ','RF ',         !Abies magnifica
     & 'OS ','OT ','RF ','OT ','NF ','NF ','OS ','OS ','NF ','RF ',         !Abies procera
     & 'OS ','OT ','RF ','OT ','RF ','SH ','OS ','WF ','RF ','RF ',         !Abies x shastensis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia angustissima
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia angustissima var. shrevei
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia angustissima var. suffrutescens
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia angustissima var. texensis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Acacia baileyana
C
      DATA ((ASPT(I,J),J=1,10),I=21,30) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','ACCO2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia constricta
     & '   ','   ','ACCOC   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia constricta var. constricta
     & '   ','   ','ACCOP9  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia constricta var. paucispina
     & '   ','   ','ACCY2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia cyclops
     & '   ','   ','ACDE3   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia dealbata
     & '   ','   ','ACDE    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia decurrens
     & '   ','   ','ACEL    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia elata
     & '   ','   ','ACFA    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia farnesiana
     & '   ','   ','ACGR    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia greggii
     & '   ','   ','ACGRG3  ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Acacia greggii var. greggii
C
      DATA ((ASPT(I,J),J=11,20),I=21,30) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia constricta
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia constricta var. constricta
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia constricta var. paucispina
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia cyclops
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia dealbata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia decurrens
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia elata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia farnesiana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia greggii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Acacia greggii var. greggii
C
      DATA ((ASPT(I,J),J=1,10),I=31,40) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','ACLO    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia longifolia
     & '   ','   ','ACME80  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia mearnsii
     & '   ','   ','ACME    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia melanoxylon
     & '   ','   ','ACMI    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia millefolia
     & '   ','   ','ACNE4   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia neovernicosa
     & '   ','   ','ACPA8   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia paradoxa
     & '   ','   ','ACPO2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia podalyriifolia
     & '   ','   ','ACPY3   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia pycnantha
     & '   ','   ','ACRE9   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia redolens
     & '   ','   ','ACRE2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Acacia retinodes
C
      DATA ((ASPT(I,J),J=11,20),I=31,40) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia longifolia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia mearnsii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia melanoxylon
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia millefolia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia neovernicosa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia paradoxa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia podalyriifolia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia pycnantha
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia redolens
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Acacia retinodes
C
      DATA ((ASPT(I,J),J=1,10),I=41,50) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','ACSA    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia saligna
     & '   ','   ','ACVE2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acacia verticillata
     & 'OM ','310','ACER    ','OH ','OH ','BM ','OH ','OH ','OT ','OH ',    !Acer
     & 'VN ','   ','ACCI    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer circinatum
     & 'MM ','321','ACGL    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer glabrum
     & '   ','   ','ACGLD3  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer glabrum var. diffusum
     & '   ','   ','ACGLD4  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer glabrum var. douglasii
     & '   ','   ','ACGLG2  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer glabrum var. glabrum
     & '   ','   ','ACGLG   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer glabrum var. greenei
     & '   ','   ','ACGLN2  ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Acer glabrum var. neomexicanum
C
      DATA ((ASPT(I,J),J=11,20),I=41,50) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia saligna
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acacia verticillata
     & 'MM ','OT ','MA ','OT ','BM ','BM ','BI ','BI ','BM ','BM ',         !Acer
     & 'OH ','OT ','OH ','OT ','CH ','CH ','OH ','OH ','CH ','OH ',         !Acer circinatum
     & 'MM ','OT ','OH ','OT ','OT ','OH ','MM ','BI ','OT ','OH ',         !Acer glabrum
     & 'MM ','OT ','OH ','OT ','OT ','OH ','MM ','BI ','OT ','OH ',         !Acer glabrum var. diffusum
     & 'MM ','OT ','OH ','OT ','OT ','OH ','MM ','BI ','OT ','OH ',         !Acer glabrum var. douglasii
     & 'MM ','OT ','OH ','OT ','OT ','OH ','MM ','BI ','OT ','OH ',         !Acer glabrum var. glabrum
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acer glabrum var. greenei
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','BI ','OT ','OH '/         !Acer glabrum var. neomexicanum
C
      DATA ((ASPT(I,J),J=1,10),I=51,60) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','ACGLT2  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer glabrum var. torreyi
     & 'BI ','322','ACGR3   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer grandidentatum
     & '   ','   ','ACGRG   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer grandidentatum var. grandidentatum
     & '   ','   ','ACGRS   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer grandidentatum var. sinuosum
     & 'BM ','312','ACMA3   ','OH ','OH ','BM ','OH ','OH ','OT ','OH ',    !Acer macrophyllum
     & '   ','   ','ACNE2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer negundo
     & '   ','   ','ACNEA   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer negundo var. arizonicum
     & '   ','   ','ACNEC2  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer negundo var. californicum
     & '   ','   ','ACNEI2  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer negundo var. interius
     & '   ','   ','ACNEN   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Acer negundo var. negundo
C
      DATA ((ASPT(I,J),J=11,20),I=51,60) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','BI ','OT ','OH ',         !Acer glabrum var. torreyi
     & 'MM ','OT ','OH ','OT ','OT ','OH ','MM ','BI ','OT ','OH ',         !Acer grandidentatum
     & 'MM ','OT ','OH ','OT ','OT ','OH ','MM ','BI ','OT ','OH ',         !Acer grandidentatum var. grandidentatum
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','BI ','OT ','OH ',         !Acer grandidentatum var. sinuosum
     & 'OH ','OT ','MA ','OT ','BM ','BM ','OH ','OH ','BM ','BM ',         !Acer macrophyllum
     & 'OH ','OT ','OH ','OT ','OT ','OH ','BI ','BE ','OT ','OH ',         !Acer negundo
     & 'OH ','OT ','OH ','OT ','OT ','OH ','BI ','BE ','OT ','OH ',         !Acer negundo var. arizonicum
     & 'OH ','OT ','OH ','OT ','OT ','OH ','BI ','BE ','OT ','OH ',         !Acer negundo var. californicum
     & 'OH ','OT ','OH ','OT ','OT ','OH ','BI ','BE ','OT ','OH ',         !Acer negundo var. interius
     & 'OH ','OT ','OH ','OT ','OT ','OH ','BI ','BE ','OT ','OH '/         !Acer negundo var. negundo
C
      DATA ((ASPT(I,J),J=1,10),I=61,70) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','ACNET   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer negundo var. texanum
     & '   ','   ','ACPL    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer platanoides
     & '   ','   ','ACSA2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Acer saccharinum
     & 'BH ','330','AESCU   ','OH ','OH ','BU ','OH ','OH ','OT ','OH ',    !Aesculus
     & 'BU ','333','AECA    ','OH ','OH ','BU ','OH ','OH ','OT ','OH ',    !Aesculus californica
     & 'OI ','331','AEGL    ','OH ','OH ','BU ','OH ','OH ','OT ','OH ',    !Aesculus glabra
     & '   ','   ','AEHI    ','OH ','OH ','BU ','OH ','OH ','OT ','OH ',    !Aesculus hippocastanum
     & 'TH ','341','AIAL    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ailanthus altissima
     & '   ','   ','ALJU    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Albizia julibrissin
     & '   ','   ','ALLE    ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Albizia lebbeck
C
      DATA ((ASPT(I,J),J=11,20),I=61,70) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','BI ','BE ','OT ','OH ',         !Acer negundo var. texanum
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acer platanoides
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Acer saccharinum
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Aesculus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Aesculus californica
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Aesculus glabra
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Aesculus hippocastanum
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ailanthus altissima
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Albizia julibrissin
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Albizia lebbeck
C
      DATA ((ASPT(I,J),J=1,10),I=71,80) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'AD ','350','ALNUS   ','RA ','OH ','RA ','OH ','OH ','OT ','OH ',    !Alnus
     & '   ','   ','ALCO13  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Alnus cordata
     & 'EA ','355','ALGL2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Alnus glutinosa
     & '   ','   ','ALIN2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Alnus incana
     & '   ','   ','ALINT   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Alnus incana ssp. tenuifolia
     & '   ','   ','ALOB2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Alnus oblongifolia
     & 'WA ','352','ALRH2   ','OH ','OH ','RA ','OH ','OH ','OT ','OH ',    !Alnus rhombifolia
     & 'RA ','351','ALRU2   ','RA ','OH ','RA ','OH ','OH ','OT ','OH ',    !Alnus rubra
     & '   ','   ','ALVI5   ','RA ','OH ','OH ','OH ','OH ','OT ','OH ',    !Alnus viridis
     & '   ','   ','ALVIC   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Alnus viridis ssp. crispa
C
      DATA ((ASPT(I,J),J=11,20),I=71,80) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','RA ','RA ','OH ','OH ','RA ','OH ',         !Alnus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Alnus cordata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Alnus glutinosa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Alnus incana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Alnus incana ssp. tenuifolia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Alnus oblongifolia
     & 'OH ','OT ','OH ','OT ','WA ','WA ','OH ','OH ','WA ','OH ',         !Alnus rhombifolia
     & 'OH ','OT ','OH ','OT ','RA ','RA ','OH ','OH ','RA ','OH ',         !Alnus rubra
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Alnus viridis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Alnus viridis ssp. crispa
C
      DATA ((ASPT(I,J),J=1,10),I=81,90) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','ALVIF   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Alnus viridis ssp. fruticosa
     & '   ','   ','ALVIS   ','RA ','OH ','OH ','OH ','OH ','OT ','OH ',    !Alnus viridis ssp. sinuata
     & '   ','   ','ARALI   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Aralia
     & '   ','   ','AREL8   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Aralia elata
     & 'MS ','360','ARBUT   ','OH ','OH ','MA ','OH ','OH ','OT ','OH ',    !Arbutus
     & 'AM ','362','ARAR2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Arbutus arizonica
     & 'MA ','361','ARME    ','OH ','OH ','MA ','OH ','OH ','OT ','OH ',    !Arbutus menziesii
     & 'M  ','   ','        ','OH ','OH ','MA ','OH ','OH ','OT ','OH ',    !Arbutus menziesii (Old Code)
     & '   ','   ','ARXA80  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Arbutus xalapensis
     & 'OB ','370','BETUL   ','OH ','OH ','OH ','OH ','OH ','OT ','PB '/    !Betula
C
      DATA ((ASPT(I,J),J=11,20),I=81,90) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Alnus viridis ssp. fruticosa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Alnus viridis ssp. sinuata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Aralia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Aralia elata
     & 'OH ','OT ','MA ','OT ','WA ','OH ','OH ','OH ','WA ','MA ',         !Arbutus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Arbutus arizonica
     & 'OH ','OT ','MA ','OT ','WA ','OH ','OH ','OH ','WA ','MA ',         !Arbutus menziesii
     & 'OH ','OT ','MA ','OT ','WA ','OH ','OH ','OH ','WA ','MA ',         !Arbutus menziesii (Old Code)
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Arbutus xalapensis
     & 'PB ','OT ','OH ','OT ','PB ','OH ','OH ','OH ','PB ','OH '/         !Betula
C
      DATA ((ASPT(I,J),J=1,10),I=91,100) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','BEEA    ','OH ','OH ','OH ','OH ','OH ','OT ','PB ',    !Betula ×eastwoodiae
     & 'NW ','378','BEUT    ','OH ','OH ','OH ','OH ','OH ','OT ','PB ',    !Betula ×utahensis
     & '   ','   ','BEGL    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Betula glandulosa
     & '   ','   ','BENA    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Betula nana
     & '   ','   ','BENE4   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Betula neoalaskana
     & 'WT ','374','BEOC2   ','OH ','OH ','OH ','OH ','OH ','OT ','PB ',    !Betula occidentalis
     & 'PB ','375','BEPA    ','OH ','OH ','OH ','OH ','PB ','OT ','PB ',    !Betula papyrifera
     & '   ','   ','BEPAC   ','OH ','OH ','OH ','OH ','PB ','OT ','PB ',    !Betula papyrifera var. commutata
     & '   ','   ','BEPAK   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Betula papyrifera var. kenaica
     & '   ','   ','BEPAP   ','OH ','OH ','OH ','OH ','PB ','OT ','PB '/    !Betula papyrifera var. papyrifera
C
      DATA ((ASPT(I,J),J=11,20),I=91,100) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'PB ','OT ','OH ','OT ','PB ','OH ','OH ','OH ','PB ','OH ',         !Betula ×eastwoodiae
     & 'PB ','OT ','OH ','OT ','PB ','OH ','OH ','OH ','PB ','OH ',         !Betula ×utahensis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Betula glandulosa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Betula nana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Betula neoalaskana
     & 'PB ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Betula occidentalis
     & 'PB ','OT ','OH ','OT ','PB ','OH ','OH ','OH ','PB ','OH ',         !Betula papyrifera
     & 'PB ','OT ','OH ','OT ','PB ','OH ','OH ','OH ','PB ','OH ',         !Betula papyrifera var. commutata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Betula papyrifera var. kenaica
     & 'PB ','OT ','OH ','OT ','PB ','OH ','OH ','OH ','PB ','OH '/         !Betula papyrifera var. papyrifera
C
      DATA ((ASPT(I,J),J=1,10),I=101,110) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','BEPE3   ','OH ','OH ','OH ','OH ','OH ','OT ','PB ',    !Betula pendula
     & '   ','   ','BEPU4   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Betula pumila
     & '   ','   ','BEPUG   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Betula pumila var. glandulifera
     & '   ','   ','BRPO6   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Brachychiton populneum
     & 'IC ','081','CADE27  ','OS ','OS ','IC ','OS ','OS ','OT ','OS ',    !Calocedrus decurrens
     & '   ','   ','CAGI10  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Carnegiea gigantea
     & '   ','   ','CATAL   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Catalpa
     & '   ','   ','CABI8   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Catalpa bignonioides
     & '   ','   ','CASP8   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Catalpa speciosa
     & '   ','   ','CELTI   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Celtis
C
      DATA ((ASPT(I,J),J=11,20),I=101,110) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'PB ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Betula pendula
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Betula pumila
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Betula pumila var. glandulifera
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Brachychiton populneum
     & 'OS ','OT ','IC ','OT ','IC ','IC ','OH ','OH ','IC ','IC ',         !Calocedrus decurrens
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Carnegiea gigantea
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Catalpa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Catalpa bignonioides
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Catalpa speciosa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Celtis
C
      DATA ((ASPT(I,J),J=1,10),I=111,120) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','CEEH    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Celtis ehrenbergiana
     & '   ','   ','CELA    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Celtis laevigata
     & '   ','   ','CELAB   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Celtis laevigata var. brevipes
     & '   ','   ','CELAR   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Celtis laevigata var. reticulata
     & '   ','   ','CELAT8  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Celtis laevigata var. texana
     & '   ','   ','CEOC    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Celtis occidentalis
     & '   ','   ','CESI3   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ceratonia siliqua
     & '   ','   ','CERCO   ','OH ','OH ','OH ','MC ','OH ','OT ','OH ',    !Cercocarpus
     & 'MB ','478','CEAL8   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Cercocarpus alnifolius
     & '   ','   ','CEIN7   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Cercocarpus intricatus
C
      DATA ((ASPT(I,J),J=11,20),I=111,120) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Celtis ehrenbergiana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Celtis laevigata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Celtis laevigata var. brevipes
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Celtis laevigata var. reticulata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Celtis laevigata var. texana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Celtis occidentalis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ceratonia siliqua
     & 'OH ','OT ','OH ','OT ','OT ','MB ','MC ','MC ','OT ','MC ',         !Cercocarpus
     & 'OH ','OT ','OH ','OT ','OT ','MB ','MC ','MC ','OT ','MC ',         !Cercocarpus alnifolius
     & 'OH ','OT ','OH ','OT ','OT ','MB ','MC ','MC ','OT ','MC '/         !Cercocarpus intricatus
C
      DATA ((ASPT(I,J),J=1,10),I=121,130) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'MC ','475','CELE3   ','OH ','OH ','OH ','MC ','OH ','OT ','OH ',    !Cercocarpus ledifolius
     & '   ','   ','CELEI   ','OH ','OH ','OH ','MC ','OH ','OT ','OH ',    !Cercocarpus ledifolius var. intercedens
     & '   ','   ','CELEL   ','OH ','OH ','OH ','MC ','OH ','OT ','OH ',    !Cercocarpus ledifolius var. ledifolius
     & '   ','   ','CEMO2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Cercocarpus montanus
     & 'BF ','   ','CEMOG   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Cercocarpus montanus var. glaber
     & '   ','   ','CEMOM4  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Cercocarpus montanus var. montanus
     & '   ','   ','CEMOP   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Cercocarpus montanus var. paucidentatus
     & 'PC ','041','CHLA    ','OS ','OS ','PC ','OS ','OS ','OT ','OS ',    !Chamaecyparis lawsoniana
     & 'YC ','042','CHNO    ','YC ','YC ','PC ','OS ','OS ','OT ','OS ',    !Chamaecyparis nootkatensis
     & 'CS ','040','CHAMA4  ','YC ','YC ','PC ','OS ','OS ','OT ','OS '/    !Chamaecyparis spp.
C
      DATA ((ASPT(I,J),J=11,20),I=121,130) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','MC ','MC ','MC ','OT ','MC ',         !Cercocarpus ledifolius
     & 'OH ','OT ','OH ','OT ','OT ','MC ','MC ','MC ','OT ','MC ',         !Cercocarpus ledifolius var. intercedens
     & 'OH ','OT ','OH ','OT ','OT ','MC ','MC ','MC ','OT ','MC ',         !Cercocarpus ledifolius var. ledifolius
     & 'OH ','OT ','OH ','OT ','OT ','MB ','MC ','MC ','OT ','MC ',         !Cercocarpus montanus
     & 'OH ','OT ','OH ','OT ','OT ','MB ','MC ','MC ','OT ','MC ',         !Cercocarpus montanus var. glaber
     & 'OH ','OT ','OH ','OT ','OT ','MB ','MC ','MC ','OT ','MC ',         !Cercocarpus montanus var. montanus
     & 'OH ','OT ','OH ','OT ','OT ','MB ','MC ','MC ','OT ','MC ',         !Cercocarpus montanus var. paucidentatus
     & 'OS ','OT ','DF ','OT ','DF ','OS ','OS ','OS ','DF ','OS ',         !Chamaecyparis lawsoniana
     & 'OS ','OT ','DF ','OT ','YC ','OS ','OS ','OS ','YC ','OS ',         !Chamaecyparis nootkatensis
     & 'OS ','OT ','DF ','OT ','YC ','OS ','OS ','OS ','YC ','OS '/         !Chamaecyparis spp.
C
      DATA ((ASPT(I,J),J=1,10),I=131,140) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','CHRYS15 ','OH ','OH ','GC ','OH ','OH ','OT ','OH ',    !Chrysolepis
     & '   ','   ','CHCH7   ','OH ','OH ','GC ','OH ','OH ','OT ','OH ',    !Chrysolepis chrysophylla
     & '   ','   ','CACH6   ','OH ','OH ','GC ','OH ','OH ','OT ','OH ',    !Chrysolepis chrysophylla var. chrysophylla
     & 'GC ','431','CHCHC4  ','OH ','OH ','GC ','OH ','OH ','OT ','OH ',    !Chrysolepis chrysophylla var. chrysophylla
     & '   ','   ','CHCHM   ','OH ','OH ','GC ','OH ','OH ','OT ','OH ',    !Chrysolepis chrysophylla var. minor
     & '   ','   ','CICA    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Cinnamomum camphora
     & '   ','   ','COAU12  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Cordyline australis
     & 'DS ','490','CORNU   ','OH ','OH ','DG ','OH ','OH ','OT ','OH ',    !Cornus
     & '   ','   ','COGL3   ','OH ','OH ','DG ','OH ','OH ','OT ','OH ',    !Cornus glabrata
     & 'DG ','492','CONU4   ','OH ','OH ','DG ','OH ','OH ','OT ','OH '/    !Cornus nuttallii
C
      DATA ((ASPT(I,J),J=11,20),I=131,140) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','GC ','GC ','OH ','OH ','GC ','GC ',         !Chrysolepis
     & 'OH ','OT ','OH ','OT ','GC ','GC ','OH ','OH ','GC ','GC ',         !Chrysolepis chrysophylla
     & 'OH ','OT ','OH ','OT ','GC ','GC ','OH ','OH ','GC ','GC ',         !Chrysolepis chrysophylla var. chrysophylla
     & 'OH ','OT ','OH ','OT ','GC ','GC ','OH ','OH ','GC ','GC ',         !Chrysolepis chrysophylla var. chrysophylla
     & 'OH ','OT ','OH ','OT ','GC ','GC ','OH ','OH ','GC ','GC ',         !Chrysolepis chrysophylla var. minor
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Cinnamomum camphora
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Cordyline australis
     & 'OH ','OT ','OH ','OT ','DG ','OH ','OH ','OH ','DG ','DG ',         !Cornus
     & 'OH ','OT ','OH ','OT ','DG ','OH ','OH ','OH ','DG ','DG ',         !Cornus glabrata
     & 'OH ','OT ','OH ','OT ','DG ','OH ','OH ','OH ','DG ','DG '/         !Cornus nuttallii
C
      DATA ((ASPT(I,J),J=1,10),I=141,150) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','COSE16  ','OH ','OH ','DG ','OH ','OH ','OT ','OH ',    !Cornus sericea
     & '   ','   ','COSEO   ','OH ','OH ','DG ','OH ','OH ','OT ','OH ',    !Cornus sericea ssp. occidentalis
     & '   ','   ','COSES   ','OH ','OH ','DG ','OH ','OH ','OT ','OH ',    !Cornus sericea ssp. sericea
     & '   ','   ','COSE3   ','OH ','OH ','DG ','OH ','OH ','OT ','OH ',    !Cornus sessilis
     & 'HZ ','   ','COCO6   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Corylus cornuta
     & '   ','   ','COCI4   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Corymbia citriodora
     & 'HT ','500','CRATA   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Crataegus
     & '   ','   ','CRDO2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Crataegus douglasii
     & '   ','   ','CRER    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Crataegus erythropoda
     & '   ','   ','CRRI    ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Crataegus rivularis
C
      DATA ((ASPT(I,J),J=11,20),I=141,150) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','DG ','OH ','OH ','OH ','DG ','DG ',         !Cornus sericea
     & 'OH ','OT ','OH ','OT ','DG ','OH ','OH ','OH ','DG ','DG ',         !Cornus sericea ssp. occidentalis
     & 'OH ','OT ','OH ','OT ','DG ','OH ','OH ','OH ','DG ','DG ',         !Cornus sericea ssp. sericea
     & 'OH ','OT ','OH ','OT ','DG ','OH ','OH ','OH ','DG ','DG ',         !Cornus sessilis
     & 'OH ','OT ','OH ','OT ','CH ','OH ','OH ','OH ','CH ','OH ',         !Corylus cornuta
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Corymbia citriodora
     & 'OH ','OT ','OH ','OT ','HT ','OH ','OH ','OH ','HT ','OH ',         !Crataegus
     & 'OH ','OT ','OH ','OT ','HT ','OH ','OH ','OH ','HT ','OH ',         !Crataegus douglasii
     & 'OH ','OT ','OH ','OT ','HT ','OH ','OH ','OH ','HT ','OH ',         !Crataegus erythropoda
     & 'OH ','OT ','OH ','OT ','HT ','OH ','OH ','OH ','HT ','OH '/         !Crataegus rivularis
C
      DATA ((ASPT(I,J),J=1,10),I=151,160) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','CRSA2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Crataegus saligna
     & '   ','   ','CRSU5   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Crataegus succulenta
     & 'CU ','050','CUPRE   ','OS ','WJ ','WJ ','OS ','RM ','OT ','OS ',    !Cupressus
     & '   ','   ','CUAB    ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Cupressus abramsiana
     & 'AC ','051','CUAR    ','OS ','OS ','OS ','OS ','RM ','OT ','OS ',    !Cupressus arizonica
     & '   ','   ','CUARA   ','OS ','OS ','OS ','OS ','RM ','OT ','OS ',    !Cupressus arizonica ssp. arizonica
     & '   ','   ','CUARN2  ','OS ','OS ','OS ','OS ','RM ','OT ','OS ',    !Cupressus arizonica ssp. nevadensis
     & '   ','   ','CUARS2  ','OS ','OS ','OS ','OS ','RM ','OT ','OS ',    !Cupressus arizonica ssp. stephensonii
     & 'CC ','052','CUBA    ','OS ','WJ ','WJ ','OS ','OS ','OT ','OS ',    !Cupressus bakeri
     & 'TC ','053','CUFO2   ','OS ','OS ','OS ','OS ','OS ','OT ','OS '/    !Cupressus forbesii
C
      DATA ((ASPT(I,J),J=11,20),I=151,160) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','HT ','OH ','OH ','OH ','HT ','OH ',         !Crataegus saligna
     & 'OH ','OT ','OH ','OT ','HT ','OH ','OH ','OH ','HT ','OH ',         !Crataegus succulenta
     & 'OS ','OT ','OS ','OT ','OT ','WJ ','OS ','WJ ','OT ','WJ ',         !Cupressus
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','WJ ','OT ','OS ',         !Cupressus abramsiana
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','WJ ','OT ','OS ',         !Cupressus arizonica
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','WJ ','OT ','OS ',         !Cupressus arizonica ssp. arizonica
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','WJ ','OT ','OS ',         !Cupressus arizonica ssp. nevadensis
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','WJ ','OT ','OS ',         !Cupressus arizonica ssp. stephensonii
     & 'OS ','OT ','OS ','OT ','OT ','WJ ','OS ','OS ','OT ','WJ ',         !Cupressus bakeri
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS '/         !Cupressus forbesii
C
      DATA ((ASPT(I,J),J=1,10),I=161,170) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','CUGO    ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Cupressus goveniana
     & '   ','   ','CUGOG   ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Cupressus goveniana ssp. goveniana
     & '   ','   ','CUGOP2  ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Cupressus goveniana ssp. pygmaea
     & '   ','   ','CUMA    ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Cupressus macnabiana
     & 'MO ','054','CUMA2   ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Cupressus macrocarpa
     & '   ','   ','CUNO    ','YC ','YC ','PC ','OS ','OS ','OT ','OS ',    !Cupressus nootkatensis
     & 'SA ','055','CUSA3   ','OS ','OS ','WJ ','OS ','OS ','OT ','OS ',    !Cupressus sargentii
     & '   ','   ','DIVI5   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Diospyros virginiana
     & '   ','   ','ELAEA   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Elaeagnus
     & '   ','   ','ELAN    ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Elaeagnus angustifolia
C
      DATA ((ASPT(I,J),J=11,20),I=161,170) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Cupressus goveniana
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Cupressus goveniana ssp. goveniana
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Cupressus goveniana ssp. pygmaea
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Cupressus macnabiana
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Cupressus macrocarpa
     & 'OS ','OT ','DF ','OT ','YC ','OS ','OS ','OS ','YC ','OS ',         !Cupressus nootkatensis
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','WJ ',         !Cupressus sargentii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Diospyros virginiana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Elaeagnus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Elaeagnus angustifolia
C
      DATA ((ASPT(I,J),J=1,10),I=171,180) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','ELCO    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Elaeagnus commutata
     & '   ','   ','ERJA3   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Eriobotrya japonica
     & '   ','   ','EUCAL   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Eucalyptus
     & '   ','   ','EUMO5   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Eucalyptus ×mortoniana
     & '   ','   ','EUCA2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Eucalyptus camaldulensis
     & '   ','   ','EUCL    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Eucalyptus cladocalyx
     & '   ','   ','EUGL    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Eucalyptus globulus
     & '   ','   ','EUGLG   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Eucalyptus globulus ssp. globulus
     & '   ','   ','EUPO    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Eucalyptus polyanthemos
     & '   ','   ','EUPU    ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Eucalyptus pulverulenta
C
      DATA ((ASPT(I,J),J=11,20),I=171,180) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Elaeagnus commutata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Eriobotrya japonica
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Eucalyptus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Eucalyptus ×mortoniana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Eucalyptus camaldulensis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Eucalyptus cladocalyx
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Eucalyptus globulus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Eucalyptus globulus ssp. globulus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Eucalyptus polyanthemos
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Eucalyptus pulverulenta
C
      DATA ((ASPT(I,J),J=1,10),I=181,190) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','EUSI2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Eucalyptus sideroxylon
     & '   ','   ','EUTE    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Eucalyptus tereticornis
     & '   ','   ','EUTO11  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Eucalyptus torquata
     & '   ','   ','EUVI    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Eucalyptus viminalis
     & '   ','   ','FAGUS   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Fagus
     & '   ','   ','FAGR    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Fagus grandifolia
     & '   ','   ','FASY    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Fagus sylvatica
     & '   ','   ','FICUS   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ficus
     & '   ','   ','FICA    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ficus carica
     & '   ','   ','FIPA2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Ficus palmata
C
      DATA ((ASPT(I,J),J=11,20),I=181,190) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Eucalyptus sideroxylon
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Eucalyptus tereticornis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Eucalyptus torquata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Eucalyptus viminalis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fagus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fagus grandifolia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fagus sylvatica
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ficus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ficus carica
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Ficus palmata
C
      DATA ((ASPT(I,J),J=1,10),I=191,200) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','FIRU4   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ficus rubiginosa
     & 'BT ','   ','FRPU7   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Frangula purshiana
     & 'OX ','540','FRAXI   ','OH ','OH ','FL ','OH ','OH ','OT ','GA ',    !Fraxinus
     & '   ','   ','FRAM2   ','OH ','OH ','OH ','OH ','OH ','OT ','GA ',    !Fraxinus americana
     & '   ','   ','FRAN2   ','OH ','OH ','OH ','OH ','OH ','OT ','GA ',    !Fraxinus anomala
     & '   ','   ','FRANA   ','OH ','OH ','OH ','OH ','OH ','OT ','GA ',    !Fraxinus anomala var. anomala
     & '   ','   ','FRANL   ','OH ','OH ','OH ','OH ','OH ','OT ','GA ',    !Fraxinus anomala var. lowellii
     & '   ','   ','FRCU    ','OH ','OH ','OH ','OH ','OH ','OT ','GA ',    !Fraxinus cuspidata
     & '   ','   ','FRDI2   ','OH ','OH ','OH ','OH ','OH ','OT ','GA ',    !Fraxinus dipetala
     & '   ','   ','FRGO    ','OH ','OH ','OH ','OH ','OH ','OT ','GA '/    !Fraxinus gooddingii
C
      DATA ((ASPT(I,J),J=11,20),I=191,200) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ficus rubiginosa
     & 'OH ','OT ','OH ','OT ','CH ','OH ','OH ','OH ','CH ','OH ',         !Frangula purshiana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus americana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus anomala
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus anomala var. anomala
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus anomala var. lowellii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus cuspidata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus dipetala
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Fraxinus gooddingii
C
      DATA ((ASPT(I,J),J=1,10),I=201,210) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','FRGR2   ','OH ','OH ','OH ','OH ','OH ','OT ','GA ',    !Fraxinus greggii
     & 'FL ','542','FRLA    ','OH ','OH ','FL ','OH ','OH ','OT ','GA ',    !Fraxinus latifolia
     & '   ','   ','FRPA4   ','OH ','OH ','OH ','OH ','OH ','OT ','GA ',    !Fraxinus papillosa
     & 'GA ','544','FRPE    ','OH ','OH ','OH ','OH ','OH ','OT ','GA ',    !Fraxinus pennsylvanica
     & '   ','   ','FRUH    ','OH ','OH ','OH ','OH ','OH ','OT ','GA ',    !Fraxinus uhdei
     & 'VA ','547','FRVE2   ','OH ','OH ','OH ','OH ','OH ','OT ','GA ',    !Fraxinus velutina
     & '   ','   ','FRCA6   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Fremontodendron californicum
     & '   ','   ','FRME2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Fremontodendron mexicanum
     & '   ','   ','GLTR    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Gleditsia triacanthos
     & '   ','   ','HOPO5   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Hoheria populnea
C
      DATA ((ASPT(I,J),J=11,20),I=201,210) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus greggii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus latifolia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus papillosa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus pennsylvanica
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus uhdei
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fraxinus velutina
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fremontodendron californicum
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Fremontodendron mexicanum
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Gleditsia triacanthos
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Hoheria populnea
C
      DATA ((ASPT(I,J),J=1,10),I=211,220) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','ILAQ80  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ilex aquifolium
     & 'WN ','600','JUGLA   ','OH ','OH ','WN ','OH ','OH ','OT ','OH ',    !Juglans
     & 'SB ','604','JUCA    ','OH ','OH ','WN ','OH ','OH ','OT ','OH ',    !Juglans californica
     & 'CA ','603','JUHI    ','OH ','OH ','WN ','OH ','OH ','OT ','OH ',    !Juglans hindsii
     & '   ','   ','JUMA    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Juglans major
     & 'TW ','605','JUMI    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Juglans microcarpa
     & '   ','   ','JUNI    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Juglans nigra
     & '   ','   ','JURE80  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Juglans regia
     & 'JU ','057','JUNIP   ','OS ','WJ ','WJ ','WJ ','RM ','OT ','RM ',    !Juniperus
     & 'J  ','   ','        ','OS ','WJ ','WJ ','WJ ','RM ','OT ','RM '/    !Juniperus (Old Code)
C
      DATA ((ASPT(I,J),J=11,20),I=211,220) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ilex aquifolium
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Juglans
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Juglans californica
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Juglans hindsii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Juglans major
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Juglans microcarpa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Juglans nigra
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Juglans regia
     & 'RM ','OT ','OS ','OT ','WJ ','WJ ','RM ','WJ ','WJ ','WJ ',         !Juniperus
     & 'RM ','OT ','OS ','OT ','WJ ','WJ ','RM ','WJ ','WJ ','WJ '/         !Juniperus (Old Code)
C
      DATA ((ASPT(I,J),J=1,10),I=221,230) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'CJ ','062','JUCA7   ','OS ','WJ ','WJ ','WJ ','RM ','OT ','OS ',    !Juniperus californica
     & 'RJ ','059','JUCO11  ','OS ','OS ','OS ','OS ','RM ','OT ','OS ',    !Juniperus coahuilensis
     & '   ','   ','JUCOA2  ','OS ','OS ','OS ','OS ','RM ','OT ','OS ',    !Juniperus coahuilensis var. arizonica
     & '   ','   ','JUCOC2  ','OS ','OS ','OS ','OS ','RM ','OT ','OS ',    !Juniperus coahuilensis var. coahuilensis
     & '   ','   ','JUCO6   ','OS ','WJ ','WJ ','OS ','OS ','OT ','RM ',    !Juniperus communis
     & '   ','   ','JUCOD   ','OS ','WJ ','WJ ','OS ','OS ','OT ','RM ',    !Juniperus communis var. depressa
     & '   ','   ','JUCOS2  ','OS ','WJ ','WJ ','OS ','OS ','OT ','RM ',    !Juniperus communis var. saxatilis
     & 'AJ ','063','JUDE2   ','OS ','OS ','OS ','WJ ','AJ ','OT ','OS ',    !Juniperus deppeana
     & '   ','   ','JUER    ','OS ','OS ','OS ','OS ','RM ','OT ','OS ',    !Juniperus erythrocarpa
     & 'OJ ','069','JUMO    ','OS ','OS ','OS ','WJ ','OJ ','OT ','OS '/    !Juniperus monosperma
C
      DATA ((ASPT(I,J),J=11,20),I=221,230) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OS ','OT ','OS ','OT ','WJ ','WJ ','OS ','WJ ','WJ ','CJ ',         !Juniperus californica
     & 'OS ','OT ','OS ','OT ','WJ ','OS ','OS ','OS ','WJ ','OS ',         !Juniperus coahuilensis
     & 'OS ','OT ','OS ','OT ','WJ ','OS ','OS ','OS ','WJ ','OS ',         !Juniperus coahuilensis var. arizonica
     & 'OS ','OT ','OS ','OT ','WJ ','OS ','OS ','OS ','WJ ','OS ',         !Juniperus coahuilensis var. coahuilensis
     & 'RM ','OT ','OS ','OT ','WJ ','WJ ','OS ','OS ','WJ ','WJ ',         !Juniperus communis
     & 'RM ','OT ','OS ','OT ','WJ ','WJ ','OS ','OS ','WJ ','WJ ',         !Juniperus communis var. depressa
     & 'RM ','OT ','OS ','OT ','WJ ','WJ ','OS ','OS ','WJ ','WJ ',         !Juniperus communis var. saxatilis
     & 'OS ','OT ','OS ','OT ','WJ ','OS ','OS ','WJ ','WJ ','OS ',         !Juniperus deppeana
     & 'OS ','OT ','OS ','OT ','WJ ','OS ','OS ','OS ','WJ ','OS ',         !Juniperus erythrocarpa
     & 'OS ','OT ','OS ','OT ','WJ ','OS ','OS ','WJ ','WJ ','OS '/         !Juniperus monosperma
C
      DATA ((ASPT(I,J),J=1,10),I=231,240) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'WJ ','064','JUOC    ','OS ','WJ ','WJ ','WJ ','RM ','OT ','RM ',    !Juniperus occidentalis
     & '   ','   ','JUOCA   ','OS ','WJ ','WJ ','WJ ','RM ','OT ','RM ',    !Juniperus occidentalis var. australis
     & '   ','   ','JUOCO   ','OS ','WJ ','WJ ','WJ ','RM ','OT ','RM ',    !Juniperus occidentalis var. occidentalis
     & 'UJ ','065','JUOS    ','OS ','WJ ','OS ','WJ ','UJ ','OT ','RM ',    !Juniperus osteosperma
     & 'PJ ','058','JUPI    ','OS ','OS ','OS ','OS ','RM ','OT ','OS ',    !Juniperus pinchotii
     & 'RM ','066','JUSC2   ','OS ','WJ ','OS ','WJ ','RM ','OT ','RM ',    !Juniperus scopulorum
     & 'ER ','068','JUVI    ','OS ','OS ','OS ','OS ','ER ','OT ','OS ',    !Juniperus virginiana
     & '   ','   ','JUVIV   ','OS ','OS ','OS ','OS ','ER ','OT ','OS ',    !Juniperus virginiana var. virginiana
     & '   ','   ','KOPA    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Koelreuteria paniculata
     & '   ','   ','LAAN2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Laburnum anagyroides
C
      DATA ((ASPT(I,J),J=11,20),I=231,240) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'RM ','OT ','OS ','OT ','WJ ','WJ ','RM ','WJ ','WJ ','WJ ',         !Juniperus occidentalis
     & 'RM ','OT ','OS ','OT ','WJ ','WJ ','RM ','WJ ','WJ ','WJ ',         !Juniperus occidentalis var. australis
     & 'RM ','OT ','OS ','OT ','WJ ','WJ ','RM ','WJ ','WJ ','WJ ',         !Juniperus occidentalis var. occidentalis
     & 'RM ','OT ','OS ','OT ','WJ ','WJ ','UJ ','UJ ','WJ ','UJ ',         !Juniperus osteosperma
     & 'OS ','OT ','OS ','OT ','WJ ','OS ','OS ','OS ','WJ ','OS ',         !Juniperus pinchotii
     & 'RM ','OT ','OS ','OT ','WJ ','WJ ','RM ','RM ','WJ ','OS ',         !Juniperus scopulorum
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Juniperus virginiana
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Juniperus virginiana var. virginiana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Koelreuteria paniculata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Laburnum anagyroides
C
      DATA ((ASPT(I,J),J=1,10),I=241,250) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'OL ','070','LARIX   ','OS ','WL ','OS ','WL ','WL ','WL ','WL ',    !Larix
     & 'TA ','071','LALA    ','OS ','WL ','OS ','WL ','WL ','WL ','WL ',    !Larix laricina
     & 'LL ','072','LALY    ','OS ','AF ','OS ','WL ','WL ','AF ','LL ',    !Larix lyallii
     & 'WL ','073','LAOC    ','OS ','WL ','OS ','WL ','WL ','WL ','WL ',    !Larix occidentalis
     & 'L  ','   ','        ','OS ','WL ','OS ','WL ','WL ','WL ','WL ',    !Larix occidentalis (Old Code)
     & '   ','   ','LANO80  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Laurus nobilis
     & '   ','   ','LEPTO   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Leptochloa
     & '   ','   ','LEUCA   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Leucaena
     & '   ','   ','LEES2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Leucaena esculenta
     & '   ','   ','LELE10  ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Leucaena leucocephala
C
      DATA ((ASPT(I,J),J=11,20),I=241,250) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'WL ','WL ','OS ','WL ','YC ','WL ','OS ','OS ','YC ','OS ',         !Larix
     & 'WL ','WL ','OS ','WL ','YC ','WL ','OS ','OS ','YC ','OS ',         !Larix laricina
     & 'LL ','WL ','OS ','WL ','LL ','OS ','OS ','OS ','LL ','OS ',         !Larix lyallii
     & 'WL ','WL ','OS ','WL ','YC ','WL ','OS ','OS ','YC ','OS ',         !Larix occidentalis
     & 'WL ','WL ','OS ','WL ','YC ','WL ','OS ','OS ','YC ','OS ',         !Larix occidentalis (Old Code)
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Laurus nobilis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Leptochloa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Leucaena
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Leucaena esculenta
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Leucaena leucocephala
C
      DATA ((ASPT(I,J),J=1,10),I=251,260) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','LIDE    ','OS ','OS ','IC ','OS ','OS ','OT ','OS ',    !Libocedrus decurrens
     & '   ','   ','LIST2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Liquidambar styraciflua
     & '   ','   ','LITHO   ','OH ','OH ','TO ','OH ','OH ','OT ','OH ',    !Lithocarpus
     & 'TO ','631','LIDE3   ','OH ','OH ','TO ','OH ','OH ','OT ','OH ',    !Lithocarpus densiflorus
     & '   ','   ','LIDED2  ','OH ','OH ','TO ','OH ','OH ','OT ','OH ',    !Lithocarpus densiflorus var. densiflorus
     & '   ','   ','LIDEE   ','OH ','OH ','TO ','OH ','OH ','OT ','OH ',    !Lithocarpus densiflorus var. echinoides
     & '   ','   ','LIMO4   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Lithrea molleoides
     & '   ','   ','LYONO   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Lyonothamnus
     & '   ','   ','LYFL2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Lyonothamnus floribundus
     & '   ','   ','LYFLA   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Lyonothamnus floribundus ssp. aspleniifolius
C
      DATA ((ASPT(I,J),J=11,20),I=251,260) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OS ','OT ','IC ','OT ','IC ','IC ','OS ','OS ','IC ','IC ',         !Libocedrus decurrens
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Liquidambar styraciflua
     & 'OH ','OT ','TO ','OT ','GC ','OH ','OH ','OH ','GC ','TO ',         !Lithocarpus
     & 'OH ','OT ','TO ','OT ','GC ','OH ','OH ','OH ','GC ','TO ',         !Lithocarpus densiflorus
     & 'OH ','OT ','TO ','OT ','GC ','OH ','OH ','OH ','GC ','TO ',         !Lithocarpus densiflorus var. densiflorus
     & 'OH ','OT ','TO ','OT ','GC ','OH ','OH ','OH ','GC ','TO ',         !Lithocarpus densiflorus var. echinoides
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Lithrea molleoides
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Lyonothamnus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Lyonothamnus floribundus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Lyonothamnus floribundus ssp. aspleniifolius
C
      DATA ((ASPT(I,J),J=1,10),I=261,270) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','LYFLF   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Lyonothamnus floribundus ssp. floribundus
     & 'AL ','660','MALUS   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Malus
     & '   ','   ','MACO5   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Malus coronaria
     & 'OR ','661','MAFU    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Malus fusca
     & '   ','   ','MAPU    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Malus pumila
     & 'MU ','992','MEQU    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Melaleuca quinquenervia
     & '   ','   ','MEAZ    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Melia azedarach
     & 'WM ','   ','MOCA6   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Morella californica
     & '   ','   ','MORUS   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Morus
     & '   ','   ','MOAL    ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Morus alba
C
      DATA ((ASPT(I,J),J=11,20),I=261,270) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Lyonothamnus floribundus ssp. floribundus
     & 'OH ','OT ','OH ','OT ','CH ','OH ','OH ','OH ','CH ','OH ',         !Malus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Malus coronaria
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','CH ','OH ',         !Malus fusca
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Malus pumila
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Melaleuca quinquenervia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Melia azedarach
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Morella californica
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Morus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Morus alba
C
      DATA ((ASPT(I,J),J=1,10),I=271,280) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','MOMI    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Morus microphylla
     & 'DI ','990','OLTE    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Olneya tesota
     & '   ','   ','OSTRY   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ostrya
     & '   ','   ','OSKN    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ostrya knowltonii
     & '   ','   ','OSVI    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ostrya virginiana
     & '   ','   ','OSVIV   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ostrya virginiana  var. virginiana
     & 'OT ','999','2TREE   ','OH ','OS ','OH ','OS ','OS ','OT ','OS ',    !Other
     & 'OC ','   ','        ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Other Conifer
     & 'OH ','998','2TD     ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Other Hardwood
     & '   ','290','        ','OS ','OS ','OS ','OS ','OS ','OT ','OS '/    !Other Softwood
C
      DATA ((ASPT(I,J),J=11,20),I=271,280) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Morus microphylla
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Olneya tesota
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ostrya
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ostrya knowltonii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ostrya virginiana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ostrya virginiana  var. virginiana
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Other
     & 'OS ','OT ','OS ','OT ','DF ','OS ','OS ','OS ','DF ','OS ',         !Other Conifer
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Other Hardwood
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS '/         !Other Softwood
C
      DATA ((ASPT(I,J),J=1,10),I=281,290) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'OS ','298','2TE     ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Other Softwood
     & '   ','   ','PHOEN2  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Phoenix
     & '   ','   ','PHCA13  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Phoenix canariensis
     & '   ','   ','PHDA4   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Phoenix dactylifera
     & 'SR ','090','PICEA   ','SS ','ES ','BR ','ES ','ES ','ES ','ES ',    !Picea
     & 'BR ','092','PIBR    ','OS ','OS ','BR ','OS ','OS ','ES ','OS ',    !Picea breweriana
     & 'ES ','093','PIEN    ','OS ','ES ','BR ','ES ','ES ','ES ','ES ',    !Picea engelmannii
     & 'S  ','   ','        ','OS ','ES ','BR ','ES ','ES ','ES ','ES ',    !Picea engelmannii (Old Code)
     & '   ','   ','PIENE   ','OS ','ES ','BR ','ES ','ES ','ES ','ES ',    !Picea engelmannii var. engelmannii
     & '   ','   ','PIENM2  ','OS ','ES ','BR ','ES ','ES ','ES ','ES '/    !Picea engelmannii var. mexicana
C
      DATA ((ASPT(I,J),J=11,20),I=281,290) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Other Softwood
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Phoenix
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Phoenix canariensis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Phoenix dactylifera
     & 'ES ','ES ','OS ','ES ','SS ','ES ','ES ','ES ','ES ','OS ',         !Picea
     & 'OS ','OT ','OS ','OT ','OT ','ES ','OS ','OS ','OT ','OS ',         !Picea breweriana
     & 'ES ','ES ','OS ','ES ','ES ','ES ','ES ','ES ','ES ','OS ',         !Picea engelmannii
     & 'ES ','ES ','OS ','ES ','ES ','ES ','ES ','ES ','ES ','OS ',         !Picea engelmannii (Old Code)
     & 'ES ','ES ','OS ','ES ','ES ','ES ','ES ','ES ','ES ','OS ',         !Picea engelmannii var. engelmannii
     & 'ES ','ES ','OS ','ES ','ES ','ES ','ES ','ES ','ES ','OS '/         !Picea engelmannii var. mexicana
C
      DATA ((ASPT(I,J),J=1,10),I=291,300) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'WS ','094','PIGL    ','WS ','OS ','OS ','OS ','WS ','OT ','ES ',    !Picea glauca
     & '   ','   ','PILU    ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Picea lutzii
     & 'BE ','095','PIMA    ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Picea mariana
     & 'BS ','096','PIPU    ','OS ','OS ','OS ','OS ','BS ','OT ','ES ',    !Picea pungens
     & 'SS ','098','PISI    ','SS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Picea sitchensis
     & 'OP ','100','PINUS   ','LP ','PP ','SP ','PP ','PP ','LP ','LP ',    !Pinus
     & '   ','   ','PIAT2   ','OS ','OS ','MP ','OS ','OS ','OT ','OS ',    !Pinus ×attenuradiata
     & 'WB ','101','PIAL    ','OS ','WB ','WB ','WB ','WB ','AF ','WB ',    !Pinus albicaulis
     & 'BC ','102','PIAR    ','OS ','OS ','OS ','OS ','BC ','OT ','OS ',    !Pinus aristata
     & 'AR ','135','PIAR5   ','OS ','OS ','OS ','OS ','PP ','OT ','OS '/    !Pinus arizonica
C
      DATA ((ASPT(I,J),J=11,20),I=291,300) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'ES ','ES ','OS ','ES ','OT ','OS ','OS ','OS ','OT ','OS ',         !Picea glauca
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Picea lutzii
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Picea mariana
     & 'ES ','ES ','OS ','ES ','OT ','OS ','BS ','BS ','OT ','OS ',         !Picea pungens
     & 'OS ','OT ','OS ','OT ','SS ','OS ','OS ','OS ','ES ','OS ',         !Picea sitchensis
     & 'LP ','LP ','SP ','LP ','LP ','PP ','LP ','LP ','LP ','PP ',         !Pinus
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','MP ',         !Pinus ×attenuradiata
     & 'WB ','LP ','OS ','LP ','WB ','WB ','WB ','WB ','WB ','WB ',         !Pinus albicaulis
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Pinus aristata
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS '/         !Pinus arizonica
C
      DATA ((ASPT(I,J),J=1,10),I=301,310) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','PIARA   ','OS ','OS ','OS ','OS ','PP ','OT ','OS ',    !Pinus arizonica var. arizonica
     & '   ','   ','PIARS2  ','OS ','OS ','OS ','OS ','PP ','OT ','OS ',    !Pinus arizonica var. stormiae
     & 'KP ','103','PIAT    ','OS ','LP ','KP ','OS ','OS ','OT ','OS ',    !Pinus attenuata
     & 'FP ','104','PIBA    ','OS ','WB ','WB ','OS ','OS ','OT ','OS ',    !Pinus balfouriana
     & '   ','   ','PIBAA   ','OS ','WB ','WB ','OS ','OS ','OT ','OS ',    !Pinus balfouriana ssp. austrina
     & '   ','   ','PIBAB   ','OS ','WB ','WB ','OS ','OS ','OT ','OS ',    !Pinus balfouriana ssp. balfouriana
     & 'ME ','140','PICE    ','OS ','OS ','OS ','OS ','PI ','OT ','OS ',    !Pinus cembroides
     & 'LP ','108','PICO    ','LP ','LP ','LP ','LP ','LP ','LP ','LP ',    !Pinus contorta
     & '   ','   ','PICOB   ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Pinus contorta var. bolanderi
     & '   ','   ','PICOC   ','LP ','OS ','OS ','OS ','OS ','OT ','OS '/    !Pinus contorta var. contorta
C
      DATA ((ASPT(I,J),J=11,20),I=301,310) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Pinus arizonica var. arizonica
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Pinus arizonica var. stormiae
     & 'OS ','OT ','OS ','OT ','KP ','LP ','OS ','OS ','KP ','GB ',         !Pinus attenuata
     & 'OS ','OT ','OS ','OT ','WB ','WB ','OS ','OS ','WB ','FP ',         !Pinus balfouriana
     & 'OS ','OT ','OS ','OT ','WB ','WB ','OS ','OS ','WB ','FP ',         !Pinus balfouriana ssp. austrina
     & 'OS ','OT ','OS ','OT ','WB ','WB ','OS ','OS ','WB ','FP ',         !Pinus balfouriana ssp. balfouriana
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','PI ','OT ','OS ',         !Pinus cembroides
     & 'LP ','LP ','PP ','LP ','LP ','LP ','LP ','LP ','LP ','LP ',         !Pinus contorta
     & 'OS ','OT ','OS ','OT ','LP ','OS ','OS ','OS ','OT ','OS ',         !Pinus contorta var. bolanderi
     & 'OS ','OT ','OS ','OT ','LP ','OS ','OS ','OS ','OT ','OS '/         !Pinus contorta var. contorta
C
      DATA ((ASPT(I,J),J=1,10),I=311,320) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','PICOL   ','LP ','LP ','LP ','LP ','LP ','LP ','LP ',    !Pinus contorta var. latifolia
     & '   ','   ','PICOM   ','OS ','LP ','LP ','LP ','LP ','LP ','LP ',    !Pinus contorta var. murrayana
     & 'CP ','109','PICO3   ','OS ','OS ','CP ','OS ','OS ','OT ','OS ',    !Pinus coulteri
     & 'PD ','134','PIDI3   ','OS ','OS ','OS ','OS ','PD ','OT ','OS ',    !Pinus discolor
     & 'PI ','106','PIED    ','OS ','OS ','OS ','OS ','PI ','OT ','OS ',    !Pinus edulis
     & 'AP ','112','PIEN2   ','OS ','OS ','OS ','OS ','PP ','OT ','OS ',    !Pinus engelmannii
     & 'LM ','113','PIFL2   ','OS ','LM ','LM ','LM ','LM ','OT ','LM ',    !Pinus flexilis
     & '   ','   ','PIHA7   ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Pinus halepensis
     & 'JP ','116','PIJE    ','OS ','OS ','JP ','OS ','OS ','OT ','OS ',    !Pinus jeffreyi
     & 'SP ','117','PILA    ','OS ','OS ','SP ','OS ','OS ','WP ','OS '/    !Pinus lambertiana
C
      DATA ((ASPT(I,J),J=11,20),I=311,320) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'LP ','LP ','PP ','LP ','LP ','LP ','LP ','LP ','LP ','LP ',         !Pinus contorta var. latifolia
     & 'LP ','LP ','PP ','LP ','LP ','LP ','LP ','LP ','LP ','LP ',         !Pinus contorta var. murrayana
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','CP ',         !Pinus coulteri
     & 'OS ','OT ','OS ','OT ','OT ','OS ','PM ','PI ','OT ','OS ',         !Pinus discolor
     & 'PI ','OT ','OS ','OT ','OT ','OS ','PM ','PI ','OT ','OS ',         !Pinus edulis
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','PP ','OT ','OS ',         !Pinus engelmannii
     & 'LM ','OT ','OS ','OT ','OT ','WB ','LM ','LM ','OT ','LM ',         !Pinus flexilis
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Pinus halepensis
     & 'OS ','OT ','PP ','OT ','JP ','PP ','OS ','OS ','JP ','JP ',         !Pinus jeffreyi
     & 'OS ','OT ','SP ','OT ','SP ','SP ','OS ','OS ','SP ','SP '/         !Pinus lambertiana
C
      DATA ((ASPT(I,J),J=1,10),I=321,330) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','PILE    ','OS ','OS ','OS ','OS ','CI ','OT ','OS ',    !Pinus leiophylla
     & 'CI ','118','PILEC   ','OS ','OS ','OS ','OS ','CI ','OT ','OS ',    !Pinus leiophylla var. chihuahuana
     & 'GB ','142','PILO    ','OS ','OS ','OS ','OS ','BC ','OT ','OS ',    !Pinus longaeva
     & 'PM ','133','PIMO    ','OS ','OS ','OS ','OS ','PM ','OT ','OS ',    !Pinus monophylla
     & '   ','   ','PIMOC   ','OS ','OS ','OS ','OS ','PI ','OT ','OS ',    !Pinus monophylla var. californiarum
     & 'AZ ','143','PIMOF   ','OS ','OS ','OS ','OS ','AZ ','OT ','OS ',    !Pinus monophylla var. fallax
     & '   ','   ','PIMOM2  ','OS ','OS ','OS ','OS ','PI ','OT ','OS ',    !Pinus monophylla var. monophylla
     & 'WP ','119','PIMO3   ','OS ','WP ','WP ','WP ','SW ','WP ','PP ',    !Pinus monticola
     & 'BP ','120','PIMU    ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Pinus muricata
     & 'PN ','136','PINI    ','OS ','OS ','OS ','PP ','PP ','OT ','OS '/    !Pinus nigra
C
      DATA ((ASPT(I,J),J=11,20),I=321,330) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Pinus leiophylla
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Pinus leiophylla var. chihuahuana
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','GB ','OT ','GB ',         !Pinus longaeva
     & 'PI ','OT ','OS ','OT ','OT ','OS ','PM ','PM ','OT ','PM ',         !Pinus monophylla
     & 'OS ','OT ','OS ','OT ','OT ','OS ','PM ','PM ','OT ','PM ',         !Pinus monophylla var. californiarum
     & 'PI ','OT ','OS ','OT ','OT ','OS ','PM ','PM ','OT ','PM ',         !Pinus monophylla var. fallax
     & 'PI ','OT ','OS ','OT ','OT ','OS ','PM ','PM ','OT ','PM ',         !Pinus monophylla var. monophylla
     & 'WP ','WP ','SP ','WP ','WP ','WP ','OS ','OS ','WP ','WP ',         !Pinus monticola
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Pinus muricata
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','PP ','OT ','OS '/         !Pinus nigra
C
      DATA ((ASPT(I,J),J=1,10),I=331,340) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','PIPI7   ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Pinus pinea
     & 'PP ','122','PIPO    ','OS ','PP ','PP ','PP ','PP ','PP ','PP ',    !Pinus ponderosa
     & '   ','   ','PIPOA2  ','OS ','OS ','OS ','OS ','PP ','OT ','OS ',    !Pinus ponderosa ssp. arizonica
     & '   ','   ','PIPOA   ','OS ','OS ','OS ','OS ','PP ','OT ','OS ',    !Pinus ponderosa var. arizonica
     & '   ','   ','PIPOP   ','OS ','PP ','PP ','PP ','PP ','PP ','PP ',    !Pinus ponderosa var. ponderosa
     & '   ','   ','PIPOS   ','OS ','PP ','PP ','PP ','PP ','PP ','PP ',    !Pinus ponderosa var. scopulorum
     & 'FO ','138','PIQU    ','OS ','OS ','OS ','OS ','OS ','OT ','OS ',    !Pinus quadrifolia
     & 'MP ','124','PIRA2   ','OS ','OS ','MP ','OS ','OS ','OT ','OS ',    !Pinus radiata
     & 'GP ','127','PISA2   ','OS ','OS ','GP ','OS ','OS ','OT ','OS ',    !Pinus sabiniana
     & 'SW ','114','PIST3   ','OS ','OS ','OS ','OS ','SW ','OT ','OS '/    !Pinus strobiformis
C
      DATA ((ASPT(I,J),J=11,20),I=331,340) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Pinus pinea
     & 'PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ',         !Pinus ponderosa
     & 'OS ','OT ','OS ','OT ','OT ','OS ','PP ','PP ','OT ','OS ',         !Pinus ponderosa ssp. arizonica
     & 'OS ','OT ','OS ','OT ','OT ','OS ','PP ','PP ','OT ','OS ',         !Pinus ponderosa var. arizonica
     & 'PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ',         !Pinus ponderosa var. ponderosa
     & 'PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ',         !Pinus ponderosa var. scopulorum
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Pinus quadrifolia
     & 'OS ','OT ','PP ','OT ','LP ','OS ','OS ','OS ','OT ','MP ',         !Pinus radiata
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','GP ',         !Pinus sabiniana
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS '/         !Pinus strobiformis
C
      DATA ((ASPT(I,J),J=1,10),I=341,350) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'ST ','130','PISY    ','OS ','OS ','OS ','OS ','LP ','OT ','OS ',    !Pinus sylvestris
     & 'TP ','139','PITO    ','OS ','OS ','MP ','OS ','OS ','OT ','OS ',    !Pinus torreyana
     & '   ','   ','PITOI2  ','OS ','OS ','MP ','OS ','OS ','OT ','OS ',    !Pinus torreyana var. insularis
     & '   ','   ','PITOT2  ','OS ','OS ','MP ','OS ','OS ','OT ','OS ',    !Pinus torreyana var. torreyana
     & 'WE ','137','PIWA    ','OS ','OS ','PP ','OS ','OS ','OT ','OS ',    !Pinus washoensis
     & '   ','   ','PISTA   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Pistacia
     & '   ','   ','PIAT4   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Pistacia atlantica
     & '   ','   ','PICH4   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Pistacia chinensis
     & '   ','   ','PLOC    ','OH ','OH ','SY ','OH ','NC ','OT ','OH ',    !Platanus occidentalis
     & 'SY ','730','PLRA    ','OH ','OH ','SY ','OH ','NC ','OT ','OH '/    !Platanus racemosa
C
      DATA ((ASPT(I,J),J=11,20),I=341,350) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OS ','OT ','OS ','OT ','OT ','OS ','LP ','LP ','OT ','OS ',         !Pinus sylvestris
     & 'OS ','OT ','PP ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Pinus torreyana
     & 'OS ','OT ','PP ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Pinus torreyana var. insularis
     & 'OS ','OT ','PP ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Pinus torreyana var. torreyana
     & 'OS ','OT ','PP ','OT ','OT ','OS ','OS ','OS ','OT ','WE ',         !Pinus washoensis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Pistacia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Pistacia atlantica
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Pistacia chinensis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Platanus occidentalis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Platanus racemosa
C
      DATA ((ASPT(I,J),J=1,10),I=351,360) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'PS ','729','PLATA   ','OH ','OH ','SY ','OH ','NC ','OT ','OH ',    !Platanus spp.
     & '   ','   ','PLWR2   ','OH ','OH ','SY ','OH ','NC ','OT ','OH ',    !Platanus wrightii
     & 'CO ','740','POPUL   ','CW ','CW ','CW ','CW ','NC ','OT ','CW ',    !Populus
     & '   ','   ','POAC5   ','OH ','CW ','CW ','CW ','NC ','OT ','CW ',    !Populus ×acuminata
     & '   ','   ','POBR7   ','OH ','CW ','CW ','CW ','NC ','OT ','CW ',    !Populus ×brayshawii
     & '   ','   ','POCA19  ','OH ','CW ','CW ','CW ','OH ','OT ','OH ',    !Populus ×canadensis
     & '   ','   ','POHI8   ','OH ','CW ','CW ','CW ','NC ','OT ','CW ',    !Populus ×hinckleyana
     & '   ','   ','POIN23  ','OH ','CW ','CW ','CW ','NC ','OT ','CW ',    !Populus ×inopina
     & '   ','   ','POJA2   ','OH ','CW ','CW ','CW ','OH ','OT ','OH ',    !Populus ×jackii
     & '   ','   ','POPA11  ','OH ','CW ','CW ','CW ','NC ','OT ','CW '/    !Populus ×parryi
C
      DATA ((ASPT(I,J),J=11,20),I=351,360) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Platanus spp.
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Platanus wrightii
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus ×acuminata
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus ×brayshawii
     & 'OH ','OT ','OH ','OT ','CW ','CW ','OH ','NC ','CW ','OH ',         !Populus ×canadensis
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus ×hinckleyana
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus ×inopina
     & 'OH ','OT ','OH ','OT ','CW ','CW ','OH ','NC ','CW ','OH ',         !Populus ×jackii
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH '/         !Populus ×parryi
C
      DATA ((ASPT(I,J),J=1,10),I=361,370) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','POAL7   ','OH ','CW ','CW ','CW ','OH ','OT ','CW ',    !Populus alba
     & 'NC ','749','POAN3   ','OH ','CW ','CW ','CW ','NC ','OT ','NC ',    !Populus angustifolia
     & 'BA ','741','POBA2   ','CW ','CW ','CW ','CW ','NC ','OT ','BA ',    !Populus balsamifera
     & '   ','   ','POBAB2  ','OH ','CW ','CW ','CW ','NC ','OT ','CW ',    !Populus balsamifera ssp. balsamifera
     & 'CW ','747','POBAT   ','CW ','CW ','CW ','CW ','NC ','OT ','CW ',    !Populus balsamifera ssp. trichocarpa
     & '   ','   ','PODE3   ','OH ','CW ','CW ','CW ','PW ','OT ','CW ',    !Populus deltoides
     & 'PW ','745','PODEM   ','OH ','CW ','CW ','CW ','PW ','OT ','PW ',    !Populus deltoides ssp. monilifera
     & '   ','   ','PODEW   ','OH ','CW ','CW ','CW ','PW ','OT ','OH ',    !Populus deltoides ssp. wislizeni
     & 'FC ','748','POFR2   ','OH ','CW ','CW ','CW ','NC ','OT ','OH ',    !Populus fremontii
     & '   ','   ','POFRF3  ','OH ','CW ','CW ','CW ','NC ','OT ','OH '/    !Populus fremontii ssp. fremontii
C
      DATA ((ASPT(I,J),J=11,20),I=361,370) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'CO ','OT ','OH ','OT ','CW ','CW ','OH ','NC ','CW ','OH ',         !Populus alba
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus angustifolia
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus balsamifera
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus balsamifera ssp. balsamifera
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus balsamifera ssp. trichocarpa
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus deltoides
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus deltoides ssp. monilifera
     & 'OH ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus deltoides ssp. wislizeni
     & 'OH ','OT ','OH ','OT ','CW ','CW ','NC ','FC ','CW ','OH ',         !Populus fremontii
     & 'OH ','OT ','OH ','OT ','CW ','CW ','NC ','FC ','CW ','OH '/         !Populus fremontii ssp. fremontii
C
      DATA ((ASPT(I,J),J=1,10),I=371,380) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','POFRM   ','OH ','CW ','CW ','CW ','NC ','OT ','OH ',    !Populus fremontii ssp. mesetae
     & '   ','   ','PONI    ','OH ','CW ','CW ','CW ','NC ','OT ','CW ',    !Populus nigra
     & 'AS ','746','POTR5   ','OH ','AS ','AS ','AS ','AS ','OT ','AS ',    !Populus tremuloides
     & '   ','   ','POTR15  ','CW ','CW ','CW ','CW ','NC ','OT ','CW ',    !Populus trichocarpa
     & 'MQ ','755','PROSO   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Prosopis
     & '   ','   ','PRGL2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Prosopis glandulosa
     & '   ','   ','PRGLG   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Prosopis glandulosa var. glandulosa
     & 'HM ','756','PRGLT   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Prosopis glandulosa var. torreyana
     & 'SM ','758','PRPU    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Prosopis pubescens
     & 'VM ','757','PRVE    ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Prosopis velutina
C
      DATA ((ASPT(I,J),J=11,20),I=371,380) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','CW ','CW ','NC ','FC ','CW ','OH ',         !Populus fremontii ssp. mesetae
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus nigra
     & 'AS ','OT ','OH ','OT ','AS ','AS ','AS ','AS ','AS ','AS ',         !Populus tremuloides
     & 'CO ','OT ','OH ','OT ','CW ','CW ','NC ','NC ','CW ','OH ',         !Populus trichocarpa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Prosopis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Prosopis glandulosa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Prosopis glandulosa var. glandulosa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Prosopis glandulosa var. torreyana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Prosopis pubescens
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Prosopis velutina
C
      DATA ((ASPT(I,J),J=1,10),I=381,390) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'PL ','760','PRUNU   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Prunus
     & '   ','   ','PRAM    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Prunus americana
     & 'CH ','768','PREM    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Prunus emarginata
     & '   ','   ','PRVID   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Prunus virginiana var. demissa
     & 'OD ','200','PSEUD7  ','OS ','DF ','DF ','DF ','DF ','DF ','DF ',    !Pseudotsuga
     & 'BD ','201','PSMA    ','OS ','DF ','OS ','OS ','OS ','DF ','OS ',    !Pseudotsuga macrocarpa
     & 'DF ','202','PSME    ','OS ','DF ','DF ','DF ','DF ','DF ','DF ',    !Pseudotsuga menziesii
     & '   ','   ','PSMEG   ','OS ','DF ','DF ','DF ','DF ','DF ','DF ',    !Pseudotsuga menziesii var. glauca
     & '   ','   ','PSMEM   ','OS ','DF ','DF ','DF ','DF ','DF ','DF ',    !Pseudotsuga menziesii var. menziesii
     & '   ','   ','PTELE   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Ptelea
C
      DATA ((ASPT(I,J),J=11,20),I=381,390) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','CH ','CH ','OH ','OH ','CH ','OH ',         !Prunus
     & 'OH ','OT ','OH ','OT ','CH ','CH ','OH ','OH ','CH ','OH ',         !Prunus americana
     & 'OH ','OT ','OH ','OT ','CH ','CH ','OH ','OH ','CH ','OH ',         !Prunus emarginata
     & 'OH ','OT ','OH ','OT ','CH ','CH ','OH ','OH ','CH ','OH ',         !Prunus virginiana var. demissa
     & 'DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ',         !Pseudotsuga
     & 'OS ','OT ','OS ','OT ','DF ','OS ','OS ','OS ','DF ','BD ',         !Pseudotsuga macrocarpa
     & 'DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ',         !Pseudotsuga menziesii
     & 'DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ',         !Pseudotsuga menziesii var. glauca
     & 'DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ',         !Pseudotsuga menziesii var. menziesii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Ptelea
C
      DATA ((ASPT(I,J),J=1,10),I=391,400) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','PTTR    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ptelea trifoliata
     & '   ','   ','PTTRP   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ptelea trifoliata ssp. pallida
     & '   ','   ','PTTRC2  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ptelea trifoliata ssp. pallida  var. confinis
     & '   ','   ','PTTRP2  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ptelea trifoliata ssp. polyadenia
     & '   ','   ','PYCO    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Pyrus communis
     & 'OA ','800','QUERC   ','OH ','OH ','LO ','OH ','GO ','OT ','OH ',    !Quercus
     & 'OE ','850','        ','OH ','OH ','LO ','OH ','EM ','OT ','OH ',    !Quercus
     & '   ','   ','QUMO2   ','OH ','OH ','OH ','OH ','GO ','OT ','OH ',    !Quercus ×moreha
     & '   ','   ','QUPA4   ','OH ','OH ','LO ','OH ','GO ','OT ','OH ',    !Quercus ×pauciloba
     & 'LO ','801','QUAG    ','OH ','OH ','LO ','OH ','OH ','OT ','OH '/    !Quercus agrifolia
C
      DATA ((ASPT(I,J),J=11,20),I=391,400) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ptelea trifoliata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ptelea trifoliata ssp. pallida
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ptelea trifoliata ssp. pallida  var. confinis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ptelea trifoliata ssp. polyadenia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Pyrus communis
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','GO ','WO ','BO ',         !Quercus
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','GO ','WO ','BO ',         !Quercus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','BO ',         !Quercus ×moreha
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','GO ','WO ','LO ',         !Quercus ×pauciloba
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','OH ','WO ','LO '/         !Quercus agrifolia
C
      DATA ((ASPT(I,J),J=1,10),I=401,410) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'AW ','803','QUAR    ','OH ','OH ','OH ','OH ','AW ','OT ','OH ',    !Quercus arizonica
     & 'CY ','805','QUCH2   ','OH ','OH ','CY ','OH ','EM ','OT ','OH ',    !Quercus chrysolepis
     & '   ','   ','QUCHC   ','OH ','OH ','CY ','OH ','EM ','OT ','OH ',    !Quercus chrysolepis var. chrysolepis
     & 'BL ','807','QUDO    ','OH ','OH ','BL ','OH ','EM ','OT ','OH ',    !Quercus douglasii
     & '   ','   ','QUDU    ','OH ','OH ','IO ','OH ','OH ','OT ','OH ',    !Quercus dumosa
     & 'EM ','810','QUEM    ','OH ','OH ','OH ','OH ','EM ','OT ','OH ',    !Quercus emoryi
     & 'EO ','811','QUEN    ','OH ','OH ','EO ','OH ','EM ','OT ','OH ',    !Quercus engelmannii
     & 'GO ','814','QUGA    ','OH ','OH ','OH ','OH ','GO ','OT ','OH ',    !Quercus gambelii
     & '   ','   ','QUGAG   ','OH ','OH ','OH ','OH ','GO ','OT ','OH ',    !Quercus gambelii var. gambelii
     & 'WO ','815','QUGA4   ','OH ','OH ','WO ','OH ','OH ','OT ','OH '/    !Quercus garryana
C
      DATA ((ASPT(I,J),J=11,20),I=401,410) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Quercus arizonica
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','GO ','WO ','CY ',         !Quercus chrysolepis
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','GO ','WO ','CY ',         !Quercus chrysolepis var. chrysolepis
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','OH ','WO ','BL ',         !Quercus douglasii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Quercus dumosa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Quercus emoryi
     & 'OH ','OT ','BO ','OT ','OT ','OH ','OH ','OH ','OT ','BO ',         !Quercus engelmannii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','GO ','OT ','BO ',         !Quercus gambelii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','GO ','OT ','BO ',         !Quercus gambelii var. gambelii
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','OH ','WO ','BO '/         !Quercus garryana
C
      DATA ((ASPT(I,J),J=1,10),I=411,420) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','QUGAF   ','OH ','OH ','WO ','OH ','OH ','OT ','OH ',    !Quercus garryana var. fruticosa
     & '   ','   ','QUGAG2  ','OH ','OH ','WO ','OH ','OH ','OT ','OH ',    !Quercus garryana var. garryana
     & '   ','   ','QUGAS   ','OH ','OH ','WO ','OH ','OH ','OT ','OH ',    !Quercus garryana var. semota
     & 'GR ','846','QUGR3   ','OH ','OH ','OH ','OH ','EM ','OT ','OH ',    !Quercus grisea
     & 'SO ','843','QUHY    ','OH ','OH ','OH ','OH ','SO ','OT ','OH ',    !Quercus hypoleucoides
     & 'BO ','818','QUKE    ','OH ','OH ','BO ','OH ','OH ','OT ','OH ',    !Quercus kelloggii
     & 'VO ','821','QULO    ','OH ','OH ','VO ','OH ','OH ','OT ','OH ',    !Quercus lobata
     & 'BK ','823','QUMA2   ','OH ','OH ','OH ','OH ','BK ','OT ','OH ',    !Quercus macrocarpa
     & 'CK ','826','QUMU    ','OH ','OH ','OH ','OH ','BK ','OT ','OH ',    !Quercus muehlenbergii
     & 'MK ','829','QUOB    ','OH ','OH ','OH ','OH ','EM ','OT ','OH '/    !Quercus oblongifolia
C
      DATA ((ASPT(I,J),J=11,20),I=411,420) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','OH ','WO ','BO ',         !Quercus garryana var. fruticosa
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','OH ','WO ','BO ',         !Quercus garryana var. garryana
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','OH ','WO ','BO ',         !Quercus garryana var. semota
     & 'OH ','OT ','BO ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Quercus grisea
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Quercus hypoleucoides
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','OH ','WO ','BO ',         !Quercus kelloggii
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','OH ','WO ','VO ',         !Quercus lobata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Quercus macrocarpa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Quercus muehlenbergii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Quercus oblongifolia
C
      DATA ((ASPT(I,J),J=1,10),I=421,430) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','QURU4   ','OH ','OH ','OH ','OH ','EM ','OT ','OH ',    !Quercus rugosa
     & '   ','   ','QUSA2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Quercus sadleriana
     & '   ','   ','QUTO2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Quercus toumeyi
     & '   ','   ','QUTU2   ','OH ','OH ','OH ','OH ','EM ','OT ','OH ',    !Quercus turbinella
     & '   ','   ','QUUN    ','OH ','OH ','OH ','OH ','EM ','OT ','OH ',    !Quercus undulata
     & '   ','   ','QUVA    ','OH ','OH ','OH ','OH ','EM ','OT ','OH ',    !Quercus vacciniifolia
     & 'IO ','839','QUWI2   ','OH ','OH ','IO ','OH ','OH ','OT ','OH ',    !Quercus wislizeni
     & '   ','   ','QUWIF   ','OH ','OH ','IO ','OH ','OH ','OT ','OH ',    !Quercus wislizeni var. frutescens
     & '   ','   ','QUWIW   ','OH ','OH ','IO ','OH ','OH ','OT ','OH ',    !Quercus wislizeni var. wislizeni
     & '   ','   ','ROBIN   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Robinia
C
      DATA ((ASPT(I,J),J=11,20),I=421,430) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Quercus rugosa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Quercus sadleriana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Quercus toumeyi
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','GO ','OT ','OH ',         !Quercus turbinella
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','GO ','OT ','OH ',         !Quercus undulata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','GO ','OT ','OH ',         !Quercus vacciniifolia
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','OH ','WO ','IO ',         !Quercus wislizeni
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','OH ','WO ','IO ',         !Quercus wislizeni var. frutescens
     & 'OH ','OT ','BO ','OT ','WO ','WO ','OH ','OH ','WO ','IO ',         !Quercus wislizeni var. wislizeni
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Robinia
C
      DATA ((ASPT(I,J),J=1,10),I=431,440) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','ROHO    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Robinia ×holdtii
     & '   ','   ','ROHI    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Robinia hispida
     & '   ','   ','ROHIF8  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Robinia hispida var. fertilis
     & '   ','   ','ROHIH   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Robinia hispida var. hispida
     & 'NM ','902','RONE    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Robinia neomexicana
     & '   ','   ','RONEN   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Robinia neomexicana var. neomexicana
     & '   ','   ','RONER   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Robinia neomexicana var. rusbyi
     & 'RP ','901','ROPS    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Robinia pseudoacacia
     & 'WI ','920','SALIX   ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix
     & '   ','   ','SAAL    ','OH ','OH ','WI ','OH ','OH ','OT ','OH '/    !Salix alaxensis
C
      DATA ((ASPT(I,J),J=11,20),I=431,440) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Robinia ×holdtii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Robinia hispida
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Robinia hispida var. fertilis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Robinia hispida var. hispida
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Robinia neomexicana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Robinia neomexicana var. neomexicana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Robinia neomexicana var. rusbyi
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Robinia pseudoacacia
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH '/         !Salix alaxensis
C
      DATA ((ASPT(I,J),J=1,10),I=441,450) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','SAALA   ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix alaxensis var. alaxensis
     & '   ','   ','SAALL   ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix alaxensis var. longistylis
     & '   ','   ','SAAL2   ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix alba
     & '   ','   ','SAAM2   ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix amygdaloides
     & '   ','   ','SABE2   ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix bebbiana
     & '   ','   ','SABO    ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix bonplandiana
     & '   ','   ','SAEX    ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix exigua
     & '   ','   ','SAFR    ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix fragilis
     & '   ','   ','SAGL    ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix glauca
     & '   ','   ','SAGO    ','OH ','OH ','WI ','OH ','OH ','OT ','OH '/    !Salix gooddingii
C
      DATA ((ASPT(I,J),J=11,20),I=441,450) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix alaxensis var. alaxensis
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix alaxensis var. longistylis
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix alba
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix amygdaloides
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix bebbiana
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix bonplandiana
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix exigua
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix fragilis
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix glauca
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH '/         !Salix gooddingii
C
      DATA ((ASPT(I,J),J=1,10),I=451,460) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','SALA5   ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix lasiandra
     & '   ','   ','SALA6   ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix lasiolepis
     & '   ','   ','SALAB   ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix lasiolepis var. bigelovii
     & '   ','   ','SALAL2  ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix lasiolepis var. lasiolepis
     & '   ','   ','SALI    ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix ligulifolia
     & '   ','   ','SALU    ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix lucida
     & '   ','   ','SALUL   ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix lucida ssp. lasiandra
     & '   ','   ','SAMA12  ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix maccalliana
     & '   ','   ','SAMO2   ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix monticola
     & '   ','   ','SAPL2   ','OH ','OH ','WI ','OH ','OH ','OT ','OH '/    !Salix planifolia
C
      DATA ((ASPT(I,J),J=11,20),I=451,460) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix lasiandra
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix lasiolepis
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix lasiolepis var. bigelovii
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix lasiolepis var. lasiolepis
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix ligulifolia
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix lucida
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix lucida ssp. lasiandra
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix maccalliana
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix monticola
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH '/         !Salix planifolia
C
      DATA ((ASPT(I,J),J=1,10),I=461,470) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','SAPLP4  ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix planifolia sp. planifolia
     & 'SU ','928','SASC    ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix scouleriana
     & '   ','   ','SATA    ','OH ','OH ','WI ','OH ','OH ','OT ','OH ',    !Salix taxifolia
     & '   ','   ','SAMBU   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Sambucus
     & '   ','   ','SANIC4  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Sambucus nigra ssp. canadensis
     & '   ','   ','SAPIN   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Sapindus
     & '   ','   ','SASA4   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Sapindus saponaria
     & '   ','   ','SASAD   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Sapindus saponaria var. drummondii
     & '   ','   ','SCHIN   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Schinus
     & '   ','   ','SCMO    ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Schinus molle
C
      DATA ((ASPT(I,J),J=11,20),I=461,470) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix planifolia sp. planifolia
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix scouleriana
     & 'OH ','OT ','OH ','OT ','WI ','WI ','OH ','OH ','WI ','OH ',         !Salix taxifolia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Sambucus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Sambucus nigra ssp. canadensis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Sapindus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Sapindus saponaria
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Sapindus saponaria var. drummondii
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Schinus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Schinus molle
C
      DATA ((ASPT(I,J),J=1,10),I=471,480) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','SCPO7   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Schinus polygamus
     & '   ','   ','SCTE    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Schinus terebinthifolius
     & '   ','   ','SCTER2  ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Schinus terebinthifolius var. raddianus
     & 'RW ','211','SESE3   ','OS ','OS ','GS ','OS ','OS ','OT ','OS ',    !Sequoia sempervirens
     & 'GS ','212','SEGI2   ','OS ','OS ','GS ','OS ','OS ','OT ','OS ',    !Sequoiadendron giganteum
     & '   ','   ','SYRIN   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Syringa
     & '   ','   ','SYRE2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Syringa reticulata
     & '   ','   ','SYREA   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Syringa reticulata ssp. amurensis
     & '   ','   ','SYVU    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Syringa vulgaris
     & 'SC ','991','TAMAR2  ','OH ','OH ','OH ','OH ','OS ','OT ','OH '/    !Tamarix
C
      DATA ((ASPT(I,J),J=11,20),I=471,480) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Schinus polygamus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Schinus terebinthifolius
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Schinus terebinthifolius var. raddianus
     & 'OS ','OT ','OS ','OT ','RW ','OS ','OH ','OS ','RW ','RW ',         !Sequoia sempervirens
     & 'OS ','OT ','OS ','OT ','RW ','OS ','OS ','OS ','RW ','GS ',         !Sequoiadendron giganteum
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Syringa
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Syringa reticulata
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Syringa reticulata ssp. amurensis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Syringa vulgaris
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Tamarix
C
      DATA ((ASPT(I,J),J=1,10),I=481,490) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','TACH2   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Tamarix chinensis
     & '   ','   ','TAPA4   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Tamarix parviflora
     & '   ','   ','TARA    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Tamarix ramosissima
     & 'OY ','230','TAXUS   ','OS ','PY ','PY ','PY ','OS ','OT ','OS ',    !Taxus
     & '   ','   ','TABA80  ','OS ','PY ','PY ','PY ','OS ','OT ','OS ',    !Taxus baccata
     & 'PY ','231','TABR2   ','OS ','PY ','PY ','PY ','OS ','OT ','OS ',    !Taxus brevifolia
     & 'RC ','242','THPL    ','RC ','GF ','RC ','RC ','RC ','RC ','OS ',    !Thuja plicata
     & 'C  ','   ','        ','RC ','GF ','RC ','RC ','RC ','RC ','OS ',    !Thuja plicata (Old Code)
     & 'TS ','240','THUJA   ','RC ','GF ','RC ','RC ','RC ','RC ','OS ',    !Thuja spp.
     & '   ','   ','TIEU4   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Tilia ×europaea
C
      DATA ((ASPT(I,J),J=11,20),I=481,490) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Tamarix chinensis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Tamarix parviflora
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Tamarix ramosissima
     & 'PY ','OT ','OS ','OT ','PY ','PY ','OS ','OS ','PY ','OS ',         !Taxus
     & 'PY ','OT ','OS ','OT ','PY ','PY ','OS ','OS ','PY ','OS ',         !Taxus baccata
     & 'PY ','OT ','OS ','OT ','PY ','PY ','OS ','OS ','PY ','OS ',         !Taxus brevifolia
     & 'RC ','RC ','OS ','RC ','RC ','RC ','OS ','OS ','RC ','OS ',         !Thuja plicata
     & 'RC ','RC ','OS ','RC ','RC ','RC ','OS ','OS ','RC ','OS ',         !Thuja plicata (Old Code)
     & 'RC ','RC ','OS ','RC ','RC ','RC ','OS ','OS ','RC ','OS ',         !Thuja spp.
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Tilia ×europaea
C
      DATA ((ASPT(I,J),J=1,10),I=491,500) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & 'CN ','251','TOCA    ','OS ','OS ','CN ','OS ','OS ','OT ','OS ',    !Torreya californica
     & '   ','   ','TRSE6   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Triadica sebifera
     & 'HS ','260','TSUGA   ','WH ','MH ','WH ','WH ','MH ','GF ','OS ',    !Tsuga
     & '   ','   ','TSJE    ','WH ','MH ','WH ','OS ','OS ','GF ','OS ',    !Tsuga ×jeffreyi
     & 'WH ','263','TSHE    ','WH ','GF ','WH ','WH ','OS ','GF ','OS ',    !Tsuga heterophylla
     & 'MH ','264','TSME    ','MH ','MH ','MH ','OS ','MH ','GF ','OS ',    !Tsuga mertensiana
     & '   ','   ','ULMUS   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ulmus
     & '   ','   ','ULAM    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ulmus americana
     & '   ','   ','ULGL    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ulmus glabra
     & '   ','   ','ULPA    ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Ulmus parvifolia
C
      DATA ((ASPT(I,J),J=11,20),I=491,500) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OS ','OT ','OS ','OT ','OT ','OS ','OS ','OS ','OT ','OS ',         !Torreya californica
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Triadica sebifera
     & 'WH ','WH ','DF ','WH ','WH ','WH ','OS ','OS ','WH ','MH ',         !Tsuga
     & 'WH ','WH ','DF ','WH ','WH ','WH ','OS ','OS ','WH ','OS ',         !Tsuga ×jeffreyi
     & 'WH ','WH ','DF ','WH ','WH ','WH ','OS ','OS ','WH ','OS ',         !Tsuga heterophylla
     & 'MH ','WH ','OS ','WH ','MH ','MH ','OS ','AF ','MH ','MH ',         !Tsuga mertensiana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ulmus
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ulmus americana
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ulmus glabra
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Ulmus parvifolia
C
      DATA ((ASPT(I,J),J=1,10),I=501,510) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','ULPR    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ulmus procera
     & '   ','   ','ULPU    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Ulmus pumila
     & '   ','   ','UMBEL   ','OH ','OH ','CL ','OH ','OH ','OT ','OH ',    !Umbellularia
     & 'CL ','981','UMCA    ','OH ','OH ','CL ','OH ','OH ','OT ','OH ',    !Umbellularia californica
     & '   ','   ','UMCAC   ','OH ','OH ','CL ','OH ','OH ','OT ','OH ',    !Umbellularia californica var. californica
     & '   ','   ','UMCAF   ','OH ','OH ','CL ','OH ','OH ','OT ','OH ',    !Umbellularia californica var. fresnensis
     & '   ','   ','VAUQU   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Vauquelinia
     & '   ','   ','VACA5   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Vauquelinia californica
     & '   ','   ','VACAP   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Vauquelinia californica  ssp. pauciflora
     & '   ','   ','VEFO    ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Vernicia fordii
C
      DATA ((ASPT(I,J),J=11,20),I=501,510) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ulmus procera
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Ulmus pumila
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','CL ',         !Umbellularia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','CL ',         !Umbellularia californica
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','CL ',         !Umbellularia californica var. californica
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','CL ',         !Umbellularia californica var. fresnensis
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Vauquelinia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Vauquelinia californica
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Vauquelinia californica  ssp. pauciflora
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Vernicia fordii
C
      DATA ((ASPT(I,J),J=1,10),I=511,516) /
C      ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     & '   ','   ','WASHI   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Washingtonia
     & '   ','   ','WAFI    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Washingtonia filifera
     & '   ','   ','WARO    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Washingtonia robusta
     & '   ','   ','YUBR    ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Yucca brevifolia
     & '   ','   ','YUBRB   ','OH ','OH ','OH ','OH ','OH ','OT ','OH ',    !Yucca brevifolia var. brevifolia
     & '   ','   ','YUBRJ   ','OH ','OH ','OH ','OH ','OH ','OT ','OH '/    !Yucca brevifolia var. jaegeriana
C
      DATA ((ASPT(I,J),J=11,20),I=511,516) /
C       IE    KT    NC    NI    PN    SO    TT    UT    WC    WS             SPECIES
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Washingtonia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Washingtonia filifera
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Washingtonia robusta
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Yucca brevifolia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH ',         !Yucca brevifolia var. brevifolia
     & 'OH ','OT ','OH ','OT ','OT ','OH ','OH ','OH ','OT ','OH '/         !Yucca brevifolia var. jaegeriana
C
C----------
C  INITIALIZATIONS
C----------
      CALL VARVER(VVER)
      VAR=VVER(:2)
      IF(VAR.EQ.'BP' .OR. VAR.EQ.'LP' .OR. VAR.EQ.'SF' .OR. 
     & VAR.EQ.'SM' .OR. VAR.EQ.'SP')VAR='CR'
      IJSPIN=3
      ISPC1= 0
      SPCOUT= 'XX  '
C----------
C  TRANSLATE INCOMING SPECIES, SPCIN, TO VARIANT SPECIFIC RECOGNIZED
C  SPECIES, SPCOUT.
C----------
      DO 100 I= 1,MAXASPT
      IF (SPCIN .EQ. ASPT(I,1)) THEN
        IJSPIN=1
        SELECT CASE (VAR)
          CASE('AK')
            SPCOUT= ASPT(I,4)
          CASE('BM')
            SPCOUT= ASPT(I,5)
          CASE('CA')
            SPCOUT= ASPT(I,6)
          CASE('CI')
            SPCOUT= ASPT(I,7)
          CASE('CR')
            SPCOUT= ASPT(I,8)
          CASE('EC')
            SPCOUT= ASPT(I,9)
          CASE('EM')
            SPCOUT= ASPT(I,10)
          CASE('IE')
            SPCOUT= ASPT(I,11)
          CASE('KT')
            SPCOUT= ASPT(I,12)
          CASE('NC')
            SPCOUT= ASPT(I,13)
          CASE('NI')
            SPCOUT= ASPT(I,14)
          CASE('PN')
            SPCOUT= ASPT(I,15)
          CASE('SO')
            SPCOUT= ASPT(I,16)
          CASE('TT')
            SPCOUT= ASPT(I,17)
          CASE('UT')
            SPCOUT= ASPT(I,18)
          CASE('WC')
            SPCOUT= ASPT(I,19)
          CASE('WS')
            SPCOUT= ASPT(I,20)
        END SELECT 
        GO TO 150        
      ELSEIF (SPCIN .EQ. ASPT(I,2)) THEN
        IJSPIN=2
        SELECT CASE (VAR)
          CASE('AK') 
            SPCOUT= ASPT(I,4)
          CASE('BM') 
            SPCOUT= ASPT(I,5)
          CASE('CA') 
            SPCOUT= ASPT(I,6)
          CASE('CI') 
            SPCOUT= ASPT(I,7)
          CASE('CR') 
            SPCOUT= ASPT(I,8)
          CASE('EC') 
            SPCOUT= ASPT(I,9)
          CASE('EM') 
            SPCOUT= ASPT(I,10)
          CASE('IE') 
            SPCOUT= ASPT(I,11)
          CASE('KT') 
            SPCOUT= ASPT(I,12)
          CASE('NC') 
            SPCOUT= ASPT(I,13)
          CASE('NI') 
            SPCOUT= ASPT(I,14)
          CASE('PN') 
            SPCOUT= ASPT(I,15)
          CASE('SO') 
            SPCOUT= ASPT(I,16)
          CASE('TT') 
            SPCOUT= ASPT(I,17)
          CASE('UT') 
            SPCOUT= ASPT(I,18)
          CASE('WC') 
            SPCOUT= ASPT(I,19)
          CASE('WS') 
            SPCOUT= ASPT(I,20)
        END SELECT
        GO TO 150   
      ELSEIF (SPCIN .EQ. ASPT(I,3)) THEN
        IJSPIN=3
        SELECT CASE (VAR)
          CASE('AK') 
            SPCOUT= ASPT(I,4)
          CASE('BM') 
            SPCOUT= ASPT(I,5)
          CASE('CA') 
            SPCOUT= ASPT(I,6)
          CASE('CI') 
            SPCOUT= ASPT(I,7)
          CASE('CR') 
            SPCOUT= ASPT(I,8)
          CASE('EC') 
            SPCOUT= ASPT(I,9)
          CASE('EM') 
            SPCOUT= ASPT(I,10)
          CASE('IE') 
            SPCOUT= ASPT(I,11)
          CASE('KT') 
            SPCOUT= ASPT(I,12)
          CASE('NC') 
            SPCOUT= ASPT(I,13)
          CASE('NI') 
            SPCOUT= ASPT(I,14)
          CASE('PN') 
            SPCOUT= ASPT(I,15)
          CASE('SO') 
            SPCOUT= ASPT(I,16)
          CASE('TT') 
            SPCOUT= ASPT(I,17)
          CASE('UT') 
            SPCOUT= ASPT(I,18)
          CASE('WC') 
            SPCOUT= ASPT(I,19)
          CASE('WS') 
            SPCOUT= ASPT(I,20)
        END SELECT
        GO TO 150   
      ENDIF      
  100 CONTINUE
  150 CONTINUE  
C----------
C  FIND STANDARD SPECIES NUMBER FOR VARIANT TO RETURN TO INTREE.
C  IF SPECIES CODE WAS NOT SET, RETURN MAXIMUM SPECIES NUMBER.
C----------
      ISPC1= 0
      IF(SPCOUT.EQ.'XX  ')THEN
        ISPC1=MAXSP
        GO TO 300
      ENDIF
      DO 200 J2= 1,MAXSP
      IF(SPCOUT.EQ.NSP(J2,1)(1:2)) THEN
        ISPC1= J2
        GO TO 300
      ENDIF
  200 CONTINUE
  300 CONTINUE
      JSPIN(ISPC1)=IJSPIN
C----------
C  IN THE EVENT THAT EXECUTABLE IS THE OLD 11 SPECIES SORNEC VARIANT
C  SET ISPC1 EQUAL TO MAXSP
C----------
      IF(ISPC1.EQ.0)ISPC1=MAXSP
      RETURN
      END
