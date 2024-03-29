      SUBROUTINE HTCALC (N,IVAR,ISPC,SI,YRS,H,AGET,HTMAX,HTG1,JOSTND,
     &                   DEBUG)
      IMPLICIT NONE
C----------
C LS $Id$
C----------
C  THIS SUBROUTINE COMPUTES THE PREDICTED HEIGHT USING SITE INDEX
C  CURVES SHOWN IN:
C  WILLARD H. CARMEAN, JEROLD T. HAHN, & RODNEY D. JACOBS,
C  SITE INDEX CURVES FOR FOREST TREE SPECIES IN THE EASTERN UNITED 
C  STATES. USDA FOREST SERVICE, NORTH CENTRAL FOREST EXPERIMENT STATION,
C  GTR: NC-128. 142P.
C
C  IF N .LE. 0 THEN CALCULATE AGE USING HEIGHT = H
C  IF N .GT. 0 AND .LT. 2. THEN CALCULATE HEIGHT H USING AGE AGET
C  IF N .GT. 2 THEN CALCULATE HEIGHT INCREMENT
C
C  CALLED FROM: ESSUBH, FINDAG, HTGF, REGENT
C
C  LOCAL VARIABLES:
C  IVAR = VARIANT INDICATOR
C         1 = LS
C         2 = CS
C         3 = NE
C  ISPC = SPECIES
C    SI = SITE INDEX FOR THE SPECIES
C   YRS = PERIOD LENGTH FOR HT GROWTH (E.G. 5 YRS OR 10 YRS)
C  AGET = TREE AGE
C     H = CURRENT TREE HEIGHT
C     N = CALCULATION MODE (DESCRIBED ABOVE)
C
C  RETURNED VARIABLES
C   HTG1 = HEIGHT INCREMENT FOR CYCLE
C  HTMAX = MAXIMUM HEIGHT FOR SPECIES AT SITE SI
C   AGET = TREE AGE RETURNED IF N <= 0
C----------
      LOGICAL DEBUG
      REAL LTBHEC(6,127)
      INTEGER MAPCS(96),MAPLS(68),MAPNE(108)
      INTEGER JOSTND,ISPC,IVAR,N,J,I,INDX
      REAL HTG1,HTMAX,AGET,H,YRS,SI,B1,B2,B3,B4,B5,BH,HTG0,HTGP5
C
      DATA MAPCS/
     &  58,   58,   78,  125,  110,   58,  104,   16,   16,   27,
     &  27,   26,   27,   10,   10,   10,   10,   10,   10,   10,
     &  10,   19,   10,   11,   14,   15,   12,   28,    1,   58,
     &   4,   35,   53,   53,   19,   53,   53,   53,   53,   53,
     &  25,   51,    2,   12,   12,   15,   41,   38,   37,   49,
     &  42,   58,   36,   44,   36,   44,   36,   36,   46,   36,
     &  43,   36,   45,   44,   36,   36,   43,   36,   50,    1,
     &  14,   58,   50,   25,   32,   32,   50,   53,   25,   21,
     &   5,   19,   50,   50,   58,   58,   58,   58,   58,   53,
     &  50,   25,   27,   16,   58,   58/
C
      DATA MAPLS/
     &  74,  108,   95,   95,  104,   68,   63,   55,   70,   59,
     & 126,  127,   58,   58,   14,   15,   28,    4,    1,   35,
     &  53,   53,   53,    5,   51,    2,    2,   11,   12,   41,
     &  44,   36,   36,   38,   49,   36,   10,   10,   10,   32,
     &  32,   25,    8,   16,   16,   16,   58,   50,   58,   58,
     &  58,   58,   58,   53,   19,   58,   58,   58,   27,   25,
     &  58,   58,   58,   50,   50,   58,   50,   58/
C
      DATA MAPNE/
     &  55,   59,   68,   73,   63,   70,   73,   95,  104,  110,
     & 125,  126,   57,   58,  126,  127,  127,   95,   74,   78,
     &  58,  125,  102,  108,   58,    1,    2,    2,    4,    5,
     &   5,    5,    8,    5,   10,   10,   10,   10,   10,   11,
     &  12,   12,   14,   15,   15,   25,   19,   25,   32,   25,
     &  28,   32,   30,   35,   41,   36,   36,   36,   36,   42,
     &  36,   44,   36,   46,   44,   44,   38,   37,   49,   43,
     &  58,   58,   58,    5,   19,   58,   58,   16,   16,   50,
     &  27,   27,   58,   26,   27,   58,   58,   25,   36,   50,
     &  50,   50,   51,   51,   53,   53,   53,   58,   58,   58,
     &  58,   58,   58,   58,   58,   58,   58,   58/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 1,10)/
C 1:RED MAPLE, N.WISCONSIN AND UPPER MICHIGAN
     & 2.9435,     0.9132,     -0.0141,     1.6580,     -0.1095,  0.0,
C 2:SUGAR MAPLE, VERMONT GREEN MTNS
     & 3.3721,     0.8407,     -0.0150,     2.6208,     -0.2661,  0.0,
C 3:SUGAR MAPLE, N.WISCONSIN AND UPPER MICHIGAN
     & 6.1308,     0.6904,     -0.0195,    10.1563,     -0.5330,  0.0,
C 4:SILVER MAPLE, SOUTHEASTERN IOWA
     & 1.0645,     0.9918,     -0.0812,     1.5754,     -0.0272,  0.0,
C 5:YELLOW BIRCH, VERMONT GREEN MTNS
     & 2.2835,     0.9794,     -0.0054,     0.5819,     -0.0281,  0.0,
C 6:YELLOW BIRCH, N.WISCONSIN AND UPPER MICHIGAN
     & 6.0522,     0.6768,     -0.0217,    15.4232,     -0.6354,  0.0,
C 7:PAPER BIRCH, N.WISCONSIN AND UPPER MICHIGAN
     & 1.5980,     1.0000,     -0.0198,     0.9824,      0.0000,  0.0,
C 8:PAPER BIRCH, VERMONT GREEN MTNS
     & 1.7902,     0.9522,     -0.0173,     1.1668,     -0.1206,  0.0,
C 9:PAPER BIRCH, N.WISCONSIN AND UPPER MICHIGAN
     & 2.4321,     0.9207,     -0.0168,     1.5247,     -0.1042,  0.0,
C 10:HICKORIES, CENTRAL STATES, CUMBERLAND MTNS KY
     & 1.8326,     1.0015,     -0.0207,     1.4080,     -0.0005,  0.0/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 11,20)/
C 11:AMERICAN BEECH, N.WISCONSIN AND UPPER MICHIGAN
     &29.7300,     0.3631,     -0.0127,    16.7616,     -0.6804,  0.0,
C 12:WHITE ASH, VERMONT GREEN MTNS
     & 1.5768,     0.9978,     -0.0156,     0.6705,      0.0182,  0.0,
C 13:WHITE ASH, N.WISCONSIN AND UPPER MICHIGAN
     & 4.1492,     0.7531,     -0.0269,    14.5384,     -0.5811,  0.0,
C 14:BLACK ASH, N.WISCONSIN AND UPPER MICHIGAN
     & 4.2286,     0.7857,     -0.0178,     4.6219,     -0.3591,  0.0,
C 15:GREEN ASH, MISSISSIPPI VALLEY ALLUVIUM
     & 1.6505,     0.9096,     -0.0644,   125.7045,     -0.8908,  0.0,
C 16:BLACK WALNUT PLANTATIONS, CENTRAL STATES
     & 1.2898,     0.9982,     -0.0289,     0.8546,      0.0171,  0.0,
C 17:BLACK WALNUT PLANTATIONS, SHALLOW FLOOD PLAINS
     & 1.6622,     0.8860,     -0.1113,    66.8363,     -1.0584,  0.0,
C 18:BLACK WALNUT PLANTATIONS, DEEP FLOOD PLAINS
     & 2.2349,     0.8420,     -0.0808,    15.0884,     -0.6292,  0.0,
C 19:SWEETGUM, MARYLAND
     & 1.5932,     1.0124,     -0.0122,     0.6245,      0.0130,  0.0,
C 20:SWEETGUM, MISSISSIPPI VALLEY ALLUVIUM
     & 3.5384,     0.7932,     -0.0244,    29.2355,     -0.7291,  4.5/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 21,30)/
C 21:SWEETGUM, MISSISSIPPI VALLEY ALLUVIUM
     & 1.0902,     1.0298,     -0.0354,     0.7011,      0.1178,  0.0,
C 22:SWEETGUM, ALABAMA
     & 6.0673,     0.6774,     -0.0391,     9.6135,     -0.5504,  0.0,
C 23:YELLOW-POPLAR, PIEDMONT
     & 1.1798,     1.0000,     -0.0339,     0.8117,     -0.0001,  0.0,
C 24:YELLOW-POPLAR, MOUNTAINS
     & 1.2673,     1.0000,     -0.0331,     1.1149,      0.0001,  0.0,
C 25:YELLOW-POPLAR, WEST VIRGINIA APPALACHIANS
     & 1.2941,     0.9892,     -0.0315,     1.0481,     -0.0368,  0.0,
C 26:WATER TUPELO, LOWER COASTAL PLAIN GEORGIA
     & 1.2721,     0.9995,     -0.0256,     0.7447,     -0.0019,  0.0,
C 27:SWAMP TUPELO, LOWER COASTAL PLAIN GEORGIA
     & 1.3213,     0.9995,     -0.0254,     0.8549,     -0.0016,  0.0,
C 28:COTTONWOOD, S.ILLINOIS BOTTOMLANDS
     & 1.3615,     0.9813,     -0.0675,     1.5494,     -0.0767,  0.0,
C 29:COTTONWOOD, MISSISSIPPI VALLEY ALLUVIUM
     & 1.9426,     0.8886,     -0.0640,    21.0753,     -0.6786,  0.0,
C 30:COTTONWOOD, IOWA BOTTOMLANDS
     & 1.2834,     0.9571,     -0.0680,   100.0000,     -0.9223,  0.0/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 31,40)/
C 31:QUAKING ASPEN
     & 1.4475,     1.0098,     -0.0200,     0.8489,      0.0146,  0.0,
C 32:BIGTOOTH AND QUAKING ASPENS, N.WISCONSIN AND UPPER MICHIGAN
     & 5.2188,     0.6855,     -0.0301,    50.0071,     -0.8695,  0.0,
C 33:QUAKING ASPEN, NORTH CENTRAL ONTARIO
     & 2.1567,     0.9374,     -0.0155,     2.4997,     -0.2338,  4.5,
C 34:BLACK CHERRY, N.WISCONSIN AND UPPER MICHIGAN
     & 5.1493,     0.6948,     -0.0248,    20.9210,     -0.7143,  0.0,
C 35:BLACK CHERRY, NORTHWESTERN PENNSYLVANIA ALLEGHENY PLATEAU
     & 7.1846,     0.6781,     -0.0222,    13.9186,     -0.5268,  0.0,
C 36:UPLAND OAKS, MICHIGAN-GEORGIA, MISSOURI-MARYLAND
     & 2.1037,     0.9140,     -0.0275,     3.7962,     -0.2530,  0.0,
C 37:UPLAND OAKS, PIEDMONT AND N.APPALACHIAN MTNS
     & 1.2866,     0.9962,     -0.0355,     1.4485,     -0.0316,  0.0,
C 38:N.RED OAK AND BLACK OAK, BOSTON MTNS ARKANSAS
     & 0.4737,     1.2905,     -0.0236,     0.0979,      0.6121,  0.0,
C 39:BLACK, SCARLET, AND WHITE OAKS, MISSOURI OZARKS
     & 4.9676,     0.7459,     -0.0154,     7.3640,     -0.5144,  0.0,
C 40:WHITE OAK, BOSTON MTNS ARKANSAS
     & 0.5605,     1.3105,     -0.0145,     0.1779,      0.4323,  0.0/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 41,50)/
C 41:WHITE OAK, UNGLACIATED UPLANDS OH,KY,IN,IL,& MO
     & 4.5598,     0.8136,     -0.0132,     2.2410,    -0.1880,    0.0,
C 42:SCARLET OAK, UNGLACIATED UPLANDS OH,KY,IL, & MO
     & 1.6763,     0.9837,     -0.0220,     0.9949,     0.0240,   0.0,
C 43:CHERRYBARK OAK, MISSISSIPPI VALLEY ALLUVIUM
     & 1.0945,     0.9938,     -0.0755,     2.5601,     0.0114,   0.0,
C 44:WATER OAK, MISSISSIPPI VALLEY ALLUVIUM
     & 1.3466,     0.9590,     -0.0574,     8.9538,    -0.3454,   0.0,
C 45:NUTTALL OAK, MISSISSIPPI VALLEY ALLUVIUM
     & 1.3295,     0.9565,     -0.0668,    16.0085,    -0.4157,   0.0,
C 46:CHESTNUT OAK, UNGLACIATED UPLANDS OH,KY,& IN
     & 1.9044,     0.9752,     -0.0162,     0.9262,     0.0000,   0.0,
C 47:NORTHERN RED OAK, SOUTHWEST WISCONSIN
     & 1.5403,     1.0006,     -0.0216,     1.0616,    -0.0044,   0.0,
C 48:NORTHERN RED OAK, N.WISCONSIN AND UPPER MICHIGAN
     & 6.1785,     0.6619,     -0.0241,    25.0185,    -0.7400,   0.0,
C 49:BLACK OAK, UNGLACIATED UPLANDS OH,KY,IN, & MO
     & 2.9989,     0.8435,     -0.0200,     3.4635,    -0.3020,   0.0,
C 50:BLACK LOCUST PLANTATIONS, CENTRAL STATES
     & 0.9680,     1.0301,     -0.0468,     0.1639,     0.4127,   0.0/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 51,60)/
C 51:AMERICAN BASSWOOD, N.WISCONSIN AND UPPER MICHIGAN
     & 4.7633,     0.7576,     -0.0194,     6.5110,    -0.4156,   0.0,
C 52:AMERICAN ELM, SOUTHERN IOWA
     & 1.0370,     1.1906,     -0.0030,     0.1391,     0.2655,   0.0,
C 53:AMERICAN ELM, N.WISCONSIN AND UPPER MICHIGAN
     & 6.4362,     0.6827,     -0.0194,    10.9767,    -0.5477,   0.0,
C 54:BALSAM FIR, LAKE STATES
     & 1.0438,     1.0708,     -0.0222,     1.5915,    -0.1068,   4.5,
C 55:BALSAM FIR, LAKE STATES
     & 2.0770,     0.9303,     -0.0285,     2.8937,    -0.1414,   0.0,
C 56:BALSAM FIR, N.MAINE
     & 8.1200,     0.6748,     -0.0111,     6.4229,    -0.4586,   4.5,
C 57:ATLANTIC WHITE-CEDAR, FLORIDA-MASSACHUSETTS
     & 1.5341,     1.0013,     -0.0208,     0.9986,    -0.0012,   0.0,
C 58:EASTERN REDCEDAR, TENNESSEE VALLEY
     & 0.9276,     1.0591,     -0.0424,     0.3529,     0.3114,   0.0,
C 59:EUROPEAN LARCH PLANTATIONS, NEW YORK & S. NEW ENGLAND
     & 1.1151,     1.0000,     -0.0504,     1.3076,     0.0009,   0.0,
C 60:TAMARACK, MINNESOTA
     & 1.5470,     1.0000,     -0.0225,     1.1129,     0.0000,   0.0/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 61,70)/
C 61:JAPANESE LARCH PLANTATIONS, NEW YORK
     & 2.5878,     0.8755,     -0.0220,     3.5505,    -0.2882,   0.0,
C 62:SPRUCES, N.MAINE
     & 1.0716,     1.1276,      0.0179,     1.2495,    -0.0029,   4.5,
C 63:NORWAY SPRUCE PLANTATIONS, WISCONSIN
     & 6.7791,     0.6876,     -0.0280,    12.1447,    -0.4142,   0.0,
C 64:NORWAY SPRUCE PLANTATIONS, VERMONT
     & 8.6744,     0.9986,     -0.0031,     0.8904,    -0.0006,   0.0,
C 65:NORWAY SPRUCE PLANTATIONS, SOUTHERN ONTARIO
     & 2.0854,     0.9547,     -0.0427,     3.8109,    -0.2288,   4.5,
C 66:WHITE SPRUCE, MINNESOTA
     & 4.4803,     0.7382,     -0.0289,    26.6132,    -0.6461,   4.5,
C 67:WHITE SPRUCE, MINNESOTA
     &11.3079,     0.5419,     -0.0345,    34.1568,    -0.6078,   0.0,
C 68:WHITE SPRUCE PLANTATIONS, ONTARIO - PETAWAWA
     & 1.3342,     1.0008,     -0.0401,     1.8068,     0.0248,   0.0,
C 69:WHITE SPRUCE PLANTATIONS, N. CENTRAL ONTARIO
     &20.3317,     0.4649,     -0.0275,     7.2043,    -0.5166,   4.5,
C 70:BLACK SPRUCE, NORTHEASTERN MINNESOTA SUPERIOR NF
     & 1.7620,     1.0000,     -0.0201,     1.2307,     0.0000,   0.0/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 71,80)/
C 71:BLACK SPRUCE, PEATLANDS ONTARIO CLAY BELT
     &16.2120,     0.4580,     -0.0135,     8.0105,    -0.5321,   0.0,
C 72:BLACK SPRUCE, NORTH CENTRAL ONTARIO
     & 2.9232,     0.8737,     -0.0178,     4.6529,    -0.3319,   4.5,
C 73:RED SPRUCE, MAINE, NEW HAMPSHIRE, & VERMONT
     & 1.3307,     1.0442,     -0.0496,     3.5829,     0.0945,   0.0,
C 74:JACK PINE, LAKE STATES
     & 1.6330,     1.0000,     -0.0223,     1.2419,     0.0000,   0.0,
C 75:JACK PINE, NORTH CENTRAL ONTARIO
     & 2.0141,     0.8989,     -0.0236,     7.9469,    -0.5084,   4.5,
C 76:JACK PINE PLANTATIONS, WISCONSIN
     & 2.3102,     0.8350,     -0.0512,   152.9926,    -1.0625,   0.0,
C 77:SAND PINE, FLORIDA OCALA NF
     & 1.2660,     1.0034,     -0.0365,     1.5515,    -0.0221,   0.0,
C 78:SHORTLEAF PINE, SOUTHERN STATES
     & 1.4232,     0.9989,     -0.0285,     1.2156,     0.0088,   0.0,
C 79:SHORTLEAF PINE, NORTH CAROLINA PIEDMONT
     & 1.4809,     1.0145,     -0.0142,     0.6040,     0.0242,   0.0,
C 80:SHORTLEAF PINE, SOUTHEASTERN MISSOURI
     & 0.8977,     1.0624,     -0.0398,     0.1419,     0.4709,   0.0/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 81,90)/
C 81:SHORTLEAF PINE, QUACHITA MTNS ARKANSAS
     & 2.2746,     0.9019,     -0.0220,     2.5540,    -0.2208,   0.0,
C 82:SHORTLEAF PINE PLANTATIONS, TN,AL,& GA
     & 1.7327,     0.9998,     -0.0384,     1.1430,    -0.0004,   0.0,
C 83:SHORTLEAF PINE PLANTATIONS, SOUTHERN ILLINOIS
     & 1.4105,     0.9977,     -0.0747,     2.0668,    -0.0104,   0.0,
C 84:SLASH PINE, SOUTHERN STATES
     & 1.1557,     1.0031,     -0.0408,     0.9807,     0.0314,   0.0,
C 85:SLASH PINE, SOUTHEASTERN AND SOUTHERN STATES
     & 1.6072,     0.9192,     -0.0390,    12.9271,    -0.6427,   0.0,
C 86:SLASH PINE, FLORIDA
     & 1.3379,     0.9999,     -0.0534,     0.9608,    -0.0001,   0.0,
C 87:SLASH PINE, FLORIDA AND SOUTHERN GEORGIA
     & 1.1510,     1.0000,     -0.0375,     0.8394,     0.0000,   0.0,
C 88:SLASH PINE PLANTATIONS, FLORIDA
     & 2.0185,     0.8815,     -0.0902,    18.8088,    -0.5520,   0.0,
C 89:SLASH PINE PLANTATIONS, GA MID COAST PLAIN & CAROLINA SANDHILLS
     & 1.2144,     0.9999,     -0.0916,     1.7968,    -0.0002,   0.0,
C 90:SLASH PINE PLANTATIONS, WEST GULF
     & 1.8395,     1.0001,     -0.0335,     1.0774,     0.0001,   0.0/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 91,100)/
C 91:SLASH PINE DIRECT SEEDED, LOUISIANA
     & 1.4698,     0.9998,     -0.0645,     1.7575,     0.0009,   0.0,
C 92:LONGLEAF PINE, SOUTHERN STATES
     & 1.4210,     0.9947,     -0.0269,     1.1344,    -0.0109,   0.0,
C 93:LONGLEAF PINE, ATLANTIC COASTAL PLAIN
     & 1.1672,     1.0010,     -0.0459,     1.3460,     0.0204,   0.0,
C 94:LONGLEAF PINE, COASTAL PLAIN FL,GA,AL, & MS
     & 1.3196,     1.0000,     -0.0356,     1.4271,   0.000017,   0.0,
C 95:RED PINE, MINNESOTA
     & 1.8900,     1.0000,     -0.0198,     1.3892,     0.0000,   0.0,
C 96:RED PINE PLANTATIONS, NEW YORK
     &19.0635,     0.5885,    -0.00081,     3.3922,    -0.3418,   4.5,
C 97:RED PINE PLANTATIONS, VERMONT
     & 2.0401,     1.0003,     -0.0361,     1.7914,    -0.0090,   0.0,
C 98:RED PINE PLANTATIONS, WISCONSIN
     & 2.6359,     0.8259,     -0.0389,    21.5578,    -0.6271,   0.0,
C 99:RED PINE PLANTATIONS, ILLINOIS
     & 0.7666,     1.0909,     -0.0733,     3.2335,    -0.2947,   4.5,
C 100:RED PINE PLANTATIONS, EASTERN ONTARIO - PETAWAWA
     & 2.0434,     0.9978,     -0.0147,     1.0937,    -0.0035,   0.0/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 101,110)/
C 101:RED PINE PLANTATIONS, NORTH CENTRAL ONTARIO
     &13.6713,     0.5404,     -0.0283,     8.7720,    -0.5308,   4.5,
C 102:POND PINE, COASTAL PLAIN NC,SC, & GA
     & 1.1266,     1.0051,     -0.0367,     0.6780,     0.0404,   0.0,
C 103:EASTERN WHITE PINE, N.WISCONSIN
     & 1.9660,     1.0000,     -0.0240,     1.8942,     0.0000,   0.0,
C 104:EASTERN WHITE PINE, APPALACHIAN MTNS VA,TN,NC & GA
     & 3.2425,     0.7980,     -0.0435,    52.0549,    -0.7064,   0.0,
C 105:EASTERN WHITE PINE PLANTATIONS, ILLINOIS
     & 0.6689,     1.1249,     -0.0785,     1.9792,    -0.1365,   4.5,
C 106:EASTERN WHITE PINE PLANTATIONS, VERMONT
     & 1.5269,     0.9955,     -0.0550,     1.9118,    0.00048,   0.0,
C 107:EASTERN WHITE PINE PLANTATIONS, S.APPALACHIAN MTNS NC,TN, & GA
     & 1.4638,     0.9979,     -0.0743,     2.2460,    -0.0040,   0.0,
C 108:SCOTCH PINE PLANTATIONS, VERMONT
     & 1.2096,     1.0027,     -0.0671,     1.2282,     0.0335,   0.0,
C 109:LOBLOLLY PINE, SOUTHERN STATES
     & 1.1727,     1.0042,     -0.0439,     1.3558,     0.0240,   0.0,
C 110:LOBLOLLY PINE, COASTAL PLAIN MARYLAND-ALABAMA
     & 1.1421,     1.0042,     -0.0374,     0.7632,     0.0358,   0.0/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 111,120)/
C 111:LOBLOLLY PINE, COASTAL PLAIN VA,NC, & SC
     & 3.0849,     0.8076,     -0.0341,    26.2342,    -0.6702,   0.0,
C 112:LOBLOLLY PINE, N.LOUISIANA & S.ARKANSAS
     & 1.1643,     0.9999,     -0.0413,     1.1057,    -0.0002,   0.0,
C 113:LOBLOLLY PINE PLANTATIONS, SC PIEDMONT
     & 1.1579,     1.0000,     -0.0930,     1.4274,     0.0001,   0.0,
C 114:LOBLOLLY PINE PLANTATIONS, GA PIEDMONT
     & 2.9579,     0.8274,     -0.0581,     9.3221,    -0.4616,   0.0,
C 115:LOBLOLLY PINE PLANTATIONS, PIEDMONT
     & 1.0060,     1.1098,     -0.0535,     0.5548,     0.2433,   0.0,
C 116:LOBLOLLY PINE PLANTATIONS, COASTAL PLAIN
     & 1.0107,     1.1384,     -0.0393,     0.4584,     0.2413,   0.0,
C 117:LOBLOLLY PINE PLANTATIONS, COASTAL PLAIN NC & SC
     & 1.1519,     1.0000,     -0.1003,     1.6640,     0.0001,   0.0,
C 118:LOBLOLLY PINE PLANTATIONS, COASTAL PLAIN NC & SC
     & 1.5177,     1.0000,     -0.0551,     1.4360,    -0.0001,   0.0,
C 119:LOBLOLLY PINE PLANTATIONS, TN,AL, & GA
     & 1.5861,     0.9999,     -0.0390,     0.9753,    -0.0017,   0.0,
C 120:LOBLOLLY PINE PLANTATIONS, INTERIOR W.GULF TX,LA, & AR
     & 1.1547,     0.9973,     -0.0915,     1.2294,     0.0029,   0.0/
C
      DATA ((LTBHEC(I,J),I=1,6), J= 121,127)/
C 121:LOBLOLLY PINE PLANTATIONS, LOWER W.GULF TX,LA,AR, & AL
     & 2.7644,     0.9991,     -0.0090,     0.6293,     0.0003,   0.0,
C 122:LOBLOLLY PINE PLANTATIONS, ILLINOIS
     & 1.2697,     1.0002,     -0.0885,     2.0238,     0.0006,   0.0,
C 123:VIRGINIA PINE, PIEDMONT NC
     & 1.2096,     1.0140,     -0.0380,     1.3247,     0.0374,   0.0,
C 124:VIRGINIA PINE, PIEDMONT MY,VA,NC & SC
     & 1.1204,     0.9984,     -0.0597,     2.4448,    -0.0284,   0.0,
C 125:VIRGINIA PINE, WV,MY, & PN
     & 0.7716,     1.1087,     -0.0348,     0.1099,     0.5274,   0.0,
C 126:NORTHERN WHITE-CEDAR, LAKE STATES
     & 1.9730,     1.0000,     -0.0154,     1.0895,     0.0000,   0.0,
C 127:EASTERN HEMLOCK, NY, MI, & S.APPALACHIAN
     & 2.1493,     0.9979,     -0.0175,     1.4086,    -0.0008,   0.0/
C
C----------
C   SET COEFFICIENTS
C----------
      IF(IVAR .EQ. 1)THEN
        INDX = MAPLS(ISPC)
      ELSEIF(IVAR .EQ. 2)THEN
        INDX = MAPCS(ISPC)
      ELSEIF(IVAR .EQ. 3)THEN
        INDX = MAPNE(ISPC)
      ELSE
        INDX = 1
      ENDIF

      IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC INDX=',INDX

      B1= LTBHEC(1,INDX)
      B2= LTBHEC(2,INDX)
      B3= LTBHEC(3,INDX)
      B4= LTBHEC(4,INDX)
      B5= LTBHEC(5,INDX)
      BH= LTBHEC(6,INDX)

      IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC B1,B2,B3,B4,B5,BH=',
     &  B1,B2,B3,B4,B5,BH

C----------
C  CALCULATE MAXIMUM HEIGHT FOR SPECIES, IF CURRENT TREE HEIGHT
C  H >= HTMAX THEN RETURN
C----------
      HTMAX=(B1*SI**B2)
      IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC HTMAX=',HTMAX,' H=',H

      IF(HTMAX-H.LE.1.) GO TO 900
C----------
C  CALCUALTE TREE AGE IF N<=0
C----------
      IF (N .LE. 0) THEN
        AGET= 1./B3*(ALOG(1-((H-BH)/B1/SI**B2)**(1./B4/SI**B5)))
        HTG1= 0.0
        IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC AGET=',AGET
        GO TO 900
      ENDIF
C----------
C  IF 0 < N < 2 THEN CALCULATE TREE HEIGHT BASED ON AGE
C----------
      IF ((N .GT. 0) .AND. (N .LT. 2)) THEN
C
        H= BH + B1*SI**B2*(1.-EXP(B3*AGET))**(B4*SI**B5)
        IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC HEIGHT ON AGE H=',H
        GOTO 900
      ENDIF
C-----------
C  HEIGHT GROWTH EQUATION, USING NC128 TREE HEIGHT COEFFS.
C  EVALUATED FOR EACH TREE EACH CYCLE (YRS YEARS STANDARD CYCLE LENGTH)
C-----------
      HTG0=  BH + B1*SI**B2*(1.-EXP(B3*AGET))**(B4*SI**B5)
      HTGP5= BH + B1*SI**B2*(1.-EXP(B3*(AGET+YRS)))**(B4*SI**B5)
C
      HTG1= HTGP5 - HTG0
      IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC HTG0,HTGP5,HTG1=',
     & HTG0,HTGP5,HTG1
C
  900 CONTINUE
C
      IF(DEBUG)WRITE(JOSTND,*)' HTCALC B1,B2,B3,B4,B5,SI,H,HTG1,AGET=',
     &B1,B2,B3,B4,B5,SI,H,HTG1,AGET
C
      RETURN
      END
