!== last modified  01-23-2013
!== tdh added httwo = httot before prod 08
C YW added to also check httot > 20 for prod 08 and changed subroutine PROD8 to return zero volume for height < 17.3
C 01/18/2013 Added stump vol calculation (VOL(14))
      SUBROUTINE R8VOL2(VOLEQ,VOL,DBHOB,HTONE,HTTWO,MTOPP,HTTOT,CTYPE,
     >                  ERRFLAG)
C      *** SUBROUTINE TO CALCULATE NEW VOLUMES ***
C      ***          FOR REGION 8               ***
C      This routine is used for calculating volumes using
C      Clarks profile models.

C      FVS is allowed to call the 4,7,9 models with total height
C      only.  The model calculates merch height and returns
C      appropriate volumes.  FVS calls the pulpwood model to get
C      ht to the 4" and merch cubic vol.  If it is a saw tree
C      it calls the 7/9" model and reports total merch as saw
C      volume plus topwood.

C      *** WRITTEN BY KEN CORMIER AND BRAD JONES ***

C      *** CREATED:  06-18-91 ***
CREV   Revised TDH 06/01/11 Changed error handling/goto 998.
    
      IMPLICIT NONE

C      ***  PASSED VARIABLES ***
      INTEGER ERRFLAG!,I,J
      character*10 VOLEQ
	    CHARACTER*1 CTYPE
      REAL VOL(15),DBHOB,HTONE,HTTWO,DF1,MTOPP,HTTOT,fclss

C      *** LOCAL VARIABLES ***
      INTEGER SPEC,GEOA,EQN,SPECPR,GEOAPR,EQNPR,SPGRP
      INTEGER PTR,FIRST,LAST,HALF,GSPEC,CHECK,DONEFLAG,LASTFLAG
      INTEGER DIBCNT,TOTCNT,FOURCNT,NINECNT,THTFLAG
      REAL FIXDI,AD,BD,AF,BF  
      REAL R,C,E,P,B,A,Q
      REAL RO,CO,EO,PO,BO,AO
      REAL TR,TC,TE,TP,TB,TA,TAF,TBF
      REAL TRO,TCO,TEO,TPO,TBO,TAO,TAFI,TBFI
      REAL DIBMEN(49,3), TOTAL(49,7), NINE(34,6)
      REAL SEVEN(15,6), FOUR(49,6), TOPRAT(3)
      REAL OTOTAL(49,7)
      
C      *** INCLUDE COMMON BLOCKS ***

c       COMMON /AREA3A/ IPOINT,LSTREC,SRTIN,SRTOUT,ERRFLAG,DAX,TMP1,TMP2,
c     >                 UNTF,LOG,RAW,VAR,PUVOL
c       INTEGER IPOINT,LSTREC
c       INTEGER SRTIN,SRTOUT,ERRFLAG,DAX,TMP1,TMP2,UNTF,LOG,RAW,VAR,PUVOL
c       INCLUDE 'COMM2'
c       INCLUDE 'COMM3'
c       INCLUDE 'COMM8'

      INCLUDE 'R8DIB.INC'

C      *** SET ALL VARIABLES USED TO SKIP COEF. LOOKUPS TO ZERO ***
      DATA SPECPR,GEOAPR,EQNPR,SPGRP / 0,0,0,0 /       
      DATA R,C,E,P,B,A,AD,BD,AF,BF / 0,0,0,0,0,0,0,0,0,0 /
      DATA RO,CO,EO,PO,BO,AO / 0,0,0,0,0,0/   

      DATA TR,TC,TE,TP,TB,TA,TAF,TBF / 0,0,0,0,0,0,0,0 /
      DATA TRO,TCO,TEO,TPO,TBO,TAO,TAFI,TBFI / 0,0,0,0,0,0,0,0 /   

      DATA Q,FIXDI / 0,0 /
      DATA TOPRAT / 0,0,0 /

C      *** ARRAYS FOR HOLDING COEFFICIENTS ***
      DATA ((DIBMEN(J,I), I=1,3), J=1,49) /
     *    100, 3.5, 6.3,   107, 3.5, 6.5,   110, 3.5, 6.3,
     *    111, 3.4, 6.1,   115, 3.6, 6.4,   121, 3.5, 6.2,
     *    123, 3.5, 6.3,   126, 3.5, 6.1,   128, 3.4, 6.0,
     *    129, 3.6, 6.4,   131, 3.6, 6.4,   132, 3.8, 6.7,
     *    221, 3.6, 6.5,   222, 3.4, 6.2,   261, 3.6, 6.3,
     *    300, 3.5, 8.0,   316, 3.6, 8.2,   330, 3.5, 8.1,
     *    370, 3.6, 8.3,   400, 3.4, 7.9,   404, 3.2, 7.8,
     *    460, 3.6, 8.2,   500, 3.5, 8.0,   531, 3.7, 8.5,
     *    541, 3.3, 7.8,   544, 3.4, 7.9,   611, 3.5, 8.0,
     *    621, 3.5, 7.9,   652, 3.6, 8.2,   653, 3.4, 7.9,
     *    693, 3.3, 7.7,   694, 3.3, 7.9,   731, 3.7, 8.6,
     *    800, 3.5, 8.0,   802, 3.4, 8.0,   806, 3.5, 8.1,
     *    812, 3.5, 7.9,   813, 3.5, 8.1,   822, 3.3, 8.0,
     *    827, 3.6, 8.3,   828, 3.5, 8.1,   831, 3.5, 8.1,
     *    832, 3.3, 7.7,   833, 3.5, 8.1,   835, 3.4, 7.9,
     *    837, 3.4, 7.8,   901, 3.4, 7.7,   950, 3.4, 8.1,
     *    970, 3.4, 8.1 /

      DATA ((TOTAL(J,I), I=1,7), J=1,15) /
     *    100,24.20656,0.46472,  72.22792, 7.15667,2.27267,0.66773,
     *    107,22.11866,0.24225,  30.35804, 2.39719,2.20596,0.67682,
     *    110,25.43531,0.45525,  28.38927, 8.21438,2.86552,0.72623,
     *    111,32.39761,0.77487,  -2.25836, 4.80100,2.52226,0.73935,
     *    115,30.60204,0.53021, -41.90943, 4.62512,1.89883,0.62823,
     *    121,24.40837,0.46799,  10.67266, 3.59700,2.03709,0.65814,
     *    123,15.05927,0.26895,  -4.14299, 1.85472,1.57211,0.55552,
     *    126, 2.38073,0.24517,  56.21296, 5.08318,2.41963,0.67900,
     *    128,14.31325,0.43675,  89.93895, 3.24869,2.06410,0.65079,
     *    129,12.19768,0.35840,  19.63087,10.31373,1.74982,0.60458,
     *    131,31.66250,0.57402, 110.96000, 8.57300,2.36238,0.68464,
     *    132, 8.77959,0.30226,  27.58681, 7.18305,2.07630,0.61061,
     *    221,54.20738,2.93064,1173.88000,14.72859,2.87922,0.77001,
     *    222,27.91571,2.66539,2208.19000,10.58239,2.63530,0.76388,
     *    261,22.76763,0.58987,   9.28454, 8.48372,1.55322,0.59329/

      DATA ((TOTAL(J,I), I=1,7), J=16,33) /
     *    300,0.76387, 0.66804,  62.00767,14.04531,1.38226,0.43214,
     *    316,20.22964,0.48000, 117.15000, 7.63003,1.12677,0.21122,
     *    330,-0.31952,0.56300, -86.84458, 5.74912,1.36316,0.45678,
     *    370,51.28318,1.07553,-109.52000, 9.97617,1.17446,0.22942,
     *    400,29.12050,0.74397,  25.20574,16.91338,1.30183,0.31675,
     *    404,15.54633,0.84611,  75.40930,18.87904,1.37417,0.38353,
     *    460,9.85600, 0.83775,  39.78865,15.24815,1.10274,0.18436,
     *    500,16.59025,0.82445, -50.66507,12.94614,1.25121,0.30693,
     *    531,44.69823,1.23348,  53.34998, 6.76763,1.10386,0.13563,
     *    541,1.21583, 0.52363,  85.64375, 9.06721,1.38969,0.42542,
     *    544,6.55702, 0.68587, 190.64000,12.89430,1.30346,0.39763,
     *    611,39.27999,0.99797, -12.28068,18.01587,1.50811,0.48173,
     *    621,27.29531,0.52621,  80.63279,12.58635,1.37761,0.43356,
     *    652,32.65160,0.87127,-117.56000, 4.14724,1.42352,0.40489,
     *    653,26.40590,1.43248,  -3.16336,11.54432,1.56727,0.54963,
     *    693,12.27907,0.47988, 234.31000,14.15661,1.58441,0.47041,
     *    694,3.85938, 1.51531, 168.05000,16.30110,2.12815,0.68482,
     *    731,28.54231,0.73200,  63.49480,13.80676,1.59877,0.58084/

      DATA ((TOTAL(J,I), I=1,7), J=34,49) /
     *    800,18.46026,0.85487, -31.64460,13.79400,1.24408,0.29817,
     *    802,27.18247,0.88257, 174.73000,12.99263,1.28811,0.29565,
     *    806,47.31765,0.89697, 108.22000,14.17655,1.25713,0.30215,
     *    812,35.42420,0.97958,  60.98724,12.87467,1.33146,0.34634,
     *    813,51.63038,1.44613,-221.76000, 9.84136,1.26945,0.29916,
     *    822,13.23593,0.72601, 436.83000,16.35515,1.31793,0.35603,
     *    827,34.89075,1.05711,  19.83006,11.33225,1.32858,0.36417,
     *    828,-7.08703,0.91570,-441.90000,20.83134,1.23544,0.27706,
     *    831,36.75301,1.28866,  -6.10171,11.56397,1.20946,0.26427,
     *    832,25.38194,0.52813, 126.23000,10.28050,1.31163,0.35547,
     *    833,32.44798,0.84396,  12.01150,11.66991,1.39963,0.38637,
     *    835,34.64245,0.92900, 158.65000, 9.53959,1.24843,0.27255,
     *    837,33.04640,0.86211,  90.25670,12.69298,1.34191,0.33751,
     *    901,34.01475,0.47442, -19.13306, 7.50541,1.19776,0.32282,
     *    950,34.92280,0.63078,-112.60000, 7.65548,1.54223,0.51305,
     *    970,0.32727,0.83154,  -72.51669,19.16713,1.20639,0.26369/

      DATA ((OTOTAL(J,I), I=1,7), J=1,15) /
     *   100,21.01306,0.50856, 157.11000, 7.78474,2.36095,0.68914,
     *   107, 9.02872,0.25504,  60.41609, 4.16861,2.38632,0.70896,
     *   110,24.26750,0.53260, 107.65000, 9.22332,3.04710,0.74591,
     *   111,27.16454,0.79755,  65.52791, 4.88907,2.76770,0.76513,
     *   115,31.09115,0.51977, -43.19648, 3.70448,1.96014,0.64781,
     *   121,19.57711,0.54088,  22.15904, 4.34898,2.17272,0.68448,
     *   123,16.73758,0.23255, 257.35000, 2.51066,1.68324,0.61762,
     *   126, 6.93687,0.26874,  96.50804, 6.15648,2.52679,0.69903,
     *   128, 9.98989,0.40750, 126.78000, 3.90993,2.19115,0.67531,
     *   129,10.41255,0.36290,  59.64184,11.42481,1.78758,0.63240,
     *   131,25.29597,0.58370, 221.45000, 8.88273,2.39522,0.70372,
     *   132, 9.17462,0.38821,  48.07668, 9.24015,2.11000,0.62645,
     *   221,53.02593,2.80399,1183.93000,15.06926,3.20594,0.79079,
     *   222,25.10773,2.23655,2782.32000,11.60876,2.88567,0.78623,
     *   261,21.36317,0.54174,  47.97097, 9.54910,1.65131,0.60760/

      DATA ((OTOTAL(J,I), I=1,7), J=16,33) /
     *   300, 1.48199,0.63392,  97.98536,13.13006,1.46888,0.46150,
     *   316,22.00135,0.45472, 166.10000, 7.31546,1.17064,0.27213,
     *   330, 3.02215,0.58715,-154.72000, 5.04720,1.45465,0.49221,
     *   370,49.41385,1.01241, -91.82769,11.23179,1.19704,0.23928,
     *   400,26.74384,0.71015,  44.42209,15.94480,1.34232,0.33340,
     *   404,19.65583,0.76605, 156.29000,18.71336,1.49941,0.44660,
     *   460, 7.58001,0.77904,  98.86053,14.72719,1.13162,0.21363,
     *   500,17.37375,0.76817,  19.73232,12.25625,1.29942,0.34212,
     *   531,44.36826,1.22158,  79.44636, 6.36236,1.11382,0.14312,
     *   541, 0.64600,0.49680, 127.87000, 7.52915,1.49287,0.47222,
     *   544, 7.32061,0.61584, 288.36000,11.17396,1.36940,0.43492,
     *   611,39.02874,0.92761,   8.38221,17.80982,1.60164,0.49790,
     *   621,26.40588,0.51359, 118.64000,12.35256,1.45155,0.45728,
     *   652,29.53040,0.84545,-177.27000, 4.83280,1.45934,0.41750,
     *   653,23.01219,1.31139,  47.93370,11.28550,1.70245,0.58013,
     *   693,13.27517,0.43775, 357.62000,13.62877,1.71575,0.51314,
     *   694, 3.85564,1.45568, 230.65000,15.92478,2.37582,0.69671,
     *   731,25.41167,0.72445,  58.38711,13.54775,1.63890,0.59893/

      DATA ((OTOTAL(J,I), I=1,7), J=34,49) /
     *   800,19.20350,0.80440,  55.79724,13.13272,1.28810,0.32973,
     *   802,26.85889,0.84829, 232.96000,12.28295,1.32630,0.31541,
     *   806,42.73753,0.86145, 214.31000,13.53863,1.29882,0.33330,
     *   812,31.98221,0.92734, 119.69000,12.75583,1.37269,0.36674,
     *   813,50.51179,1.36917,-215.92000, 9.30537,1.28815,0.31282,
     *   822,15.45114,0.62728, 618.81000,14.60416,1.35596,0.38248,
     *   827,34.76470,1.08768,  -6.46961,10.78112,1.36987,0.38666,
     *   828,-5.45119,0.88020,-457.82000,21.01569,1.28504,0.31629,
     *   831,35.40807,1.26850,  38.61354,11.59408,1.23843,0.28533,
     *   832,22.30226,0.49235, 206.69000, 9.71683,1.38908,0.39346,
     *   833,31.32975,0.81328,  78.94096,11.45671,1.43496,0.39984,
     *   835,30.82091,0.83441, 335.71000,10.33370,1.29478,0.30931,
     *   837,31.60343,0.81484, 124.19000,12.19245,1.39091,0.35814,
     *   901,25.57939,0.35922,  -8.66136, 7.02926,1.24010,0.35218,
     *   950,35.87830,0.61921,-129.42000, 8.73588,1.65110,0.54681,
     *   970, 0.81866,0.77141,-51.36724,18.74117,1.25822,0.29448/

      DATA ((NINE(J,I), I=1,6), J=1,18) /
     *    300, -3.38842, 0.63527,   35.64541,   8.64166,  0.84805,
     *    316,  5.08903, 0.35892,  397.93000,   3.36726,  0.81660,
     *    330,  8.11618, 0.65081, -186.60000,   2.42080,  0.85800,
     *    370, 21.35601, 1.00420, -171.83000,   4.30630,  0.82470,
     *    400, 15.08240, 0.74606,  -10.47815,   9.35044,  0.71776,
     *    404, 10.43971, 0.81311,  364.38000,  13.40870,  0.82007,
     *    460,  5.18978, 0.81944,  291.32000,   8.92998,  0.86631,
     *    500,  7.60744, 0.80668, -135.64000,   8.13717,  0.82778,
     *    531, 21.59996, 1.03168,  698.22000,   2.93435,  0.76075,
     *    541,  6.47354, 0.38099,  563.99000,   2.59500,  0.76787,
     *    544,  4.99216, 0.66430,  414.55000,   7.22585,  0.83080,
     *    611, 19.20174, 0.88488,  227.60000,  10.86478,  0.77359,
     *    621, 11.43042, 0.49799,  107.56000,   7.11898,  0.89475,
     *    652, 13.14198, 0.79169, -119.21000,   1.87158,  0.64756,
     *    653,  2.59433, 1.18333, -306.80000,   5.97795,  0.85325,
     *    693,  8.25373, 0.52928,  270.28000,   6.43568,  0.77604,
     *    694, -2.71851, 1.38885,   97.84611,   8.39752,  0.84134,
     *    731, 12.96010, 0.66258,  191.64000,   7.11830,  0.94439/

      DATA ((NINE(J,I), I=1,6), J=19,34) /
     *    800,  6.82692, 0.81097, -138.20000,   8.05425,  0.83987,
     *    802, 14.81018, 0.75907,  561.93000,   6.95857,  0.67254,
     *    806, 18.30355, 0.60872,  689.33000,   6.33216,  0.71691,
     *    812, 15.60123, 0.85057,  330.49000,   6.44760,  0.74774,
     *    813, 25.85612, 1.30682,   18.28815,   5.44066,  0.68075,
     *    822, 10.15596, 0.69455,  950.57000,  10.74129,  0.81700,
     *    827, 19.34362, 0.97238,  312.28000,   6.17519,  0.70453,
     *    828, -6.10108, 0.91746, -715.31000,  13.63445,  0.86288,
     *    831, 12.19866, 1.06860,   82.03535,   5.51792,  0.72478,
     *    832, 12.42912, 0.46087,  329.68000,   4.41306,  0.81073,
     *    833, 19.27614, 0.71461,  395.04000,   6.19723,  0.76292,
     *    835, 12.86349, 0.94766,  -35.35108,   4.51201,  0.57498,
     *    837, 16.94705, 0.76609,  154.47000,   7.35601,  0.73888,
     *    901, 21.74255, 0.43665,  191.42000,   3.85237,  0.90554,
     *    950, 21.68899, 0.63425, -132.52000,   4.60915,  0.78797,
     *    970, -0.44140, 0.94606, -513.62000,  11.10968,  0.80767/

      DATA ((SEVEN(J,I), I=1,6), J=1,15) /
     *    100, 17.52141,  0.42085,  212.30000,   4.62902,  0.80018,
     *    107, 10.68723,  0.17796,   93.82565,   1.70062,  0.85324,
     *    110, 17.05141,  0.43650,   51.21248,   5.75237,  0.71945,
     *    111, 19.65766,  0.80263,  -56.75203,   3.19922,  0.84606,
     *    115, 19.30980,  0.50996,  -43.34937,   2.89792,  0.85346,
     *    121, 16.90410,  0.43537,   98.61610,   2.69963,  0.84133,
     *    123, 10.42064,  0.26006,   23.97904,   0.85155,  0.80446,
     *    126,  2.23631,  0.22531,  125.40000,   3.29779,  0.73752,
     *    128,  9.60731,  0.35444,  226.99000,   1.87862,  0.81246,
     *    129,  7.38775,  0.34603,   51.29501,   7.24845,  0.96549,
     *    131, 15.88358,  0.51074,  222.76000,   5.56549,  0.78687,
     *    132,  6.45340,  0.33177,   26.01610,   4.41421,  0.74619,
     *    221, 39.05969,  2.41213, 2893.88000,  10.43738,  0.85367,
     *    222, 21.21374,  1.19920, 6242.22000,   5.91880,  0.86106,
     *    261, 16.76497,  0.57427,  124.66000,   5.68176,  1.01169/

      DATA ((FOUR(J,I), I=1,6), J=1,15) /
     *    100, 19.57453,  0.45136,   94.08746,  5.75423,  0.87838,
     *    107, 14.01730,  0.22702,   32.18823,  1.90452,  0.79851,
     *    110, 20.16797,  0.45149,   19.96681,  6.77594,  0.78347,
     *    111, 25.11416,  0.79979,  -22.04987,  4.14493,  0.89896,
     *    115, 23.42265,  0.51276,  -33.06464,  3.48541,  0.95851,
     *    121, 19.89731,  0.46021,   34.37219,  3.23574,  0.91527,
     *    123, 13.63629,  0.27304,    4.50100,  1.27639,  0.93012,
     *    126,  1.82766,  0.24206,   62.44004,  4.17798,  0.82003,
     *    128, 12.63044,  0.42976,  108.72000,  2.50664,  0.90102,
     *    129,  8.84930,  0.35232,   22.75990,  8.58707,  1.05487,
     *    131, 19.16609,  0.54668,  110.14000,  6.68944,  0.85323,
     *    132,  8.46601,  0.34417,    8.79772,  5.45777,  0.80151,
     *    221, 44.97219,  2.80066, 1400.23000, 12.37375,  0.85144,
     *    222, 22.62266,  2.50092, 2541.35000,  8.08325,  0.86240,
     *    261, 18.47210,  0.58220,   33.90974,  6.69498,  1.11415/

      DATA ((FOUR(J,I), I=1,6), J=16,33) /
     *    300,  2.07623,  0.67626,   63.16373, 10.83048,  1.06932,
     *    316, 18.50884,  0.43793,  194.17000,  8.59395,  1.25124,
     *    330,  6.91120,  0.60709,  -81.35488,  4.12073,  1.01379,
     *    370, 31.20891,  0.94760,  -49.21044,  7.27450,  1.00884,
     *    400, 25.05401,  0.77669,   70.92241, 12.72475,  0.98817,
     *    404, 14.09389,  0.85160,   87.48764, 15.90593,  1.09058,
     *    460,  9.36889,  0.86292,  176.99000, 12.35128,  1.30637,
     *    500, 15.32087,  0.84179,  -24.78451, 10.85296,  1.11952,
     *    531, 28.35035,  1.11249,  193.25000,  4.45848,  1.11262,
     *    541,  3.50540,  0.53051,  103.36000,  6.57758,  0.99593,
     *    544,  5.16768,  0.68273,  199.85000, 10.39890,  1.12269,
     *    611, 30.26910,  0.97162,   27.97553, 14.25132,  1.00000,
     *    621, 14.39865,  0.51127,   24.47831,  9.96576,  1.10228,
     *    652, 23.16078,  0.82787,  -87.83604,  3.11163,  0.90353,
     *    653, 15.34203,  1.28997,   24.48406,  8.72476,  0.98665,
     *    693, 16.69555,  0.59628,  211.22000,  8.80290,  0.86816,
     *    694,  1.65312,  1.48029,  174.46000, 13.20124,  0.91073,
     *    731, 19.16546,  0.69450,   88.01033, 10.40165,  0.98498/

      DATA ((FOUR(J,I), I=1,6), J=34,49) /
     *    800, 14.94166,  0.85216,  -14.39119, 10.75215,  1.13698,
     *    802, 19.96023,  0.84871,  199.76000,  9.78031,  1.01687,
     *    806, 27.54356,  0.74944,  263.92000,  9.04254,  1.02048,
     *    812, 22.97463,  0.93866,  131.18000,  9.35568,  0.97967,
     *    813, 39.22492,  1.42308, -165.61000,  7.12822,  1.02097,
     *    822, 11.98363,  0.72520,  519.63000, 13.59938,  1.13856,
     *    827, 24.68063,  1.00719,   38.43151,  8.86923,  0.99800,
     *    828, -5.68827,  0.91707, -443.84000, 17.23428,  1.23161,
     *    831, 24.43783,  1.20379,   47.53710,  8.45645,  1.07274,
     *    832, 12.36718,  0.49047,   76.86862,  6.13653,  1.10297,
     *    833, 22.87602,  0.74119,  114.37000,  8.61102,  1.03013,
     *    835, 19.27158,  0.95880,   31.31768,  5.93130,  0.99587,
     *    837, 26.20262,  0.82750,   76.51710,  9.77858,  1.02112,
     *    901, 27.28041,  0.47147,   -3.85360,  5.94068,  1.20251,
     *    950, 31.06399,  0.64665, -110.91000,  6.20686,  1.01855,
     *    970,  7.62297,  0.91368, -240.91000, 14.84517,  1.14963/

C     END  ! BLOCK DATA !

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      READ(VOLEQ(8:10),'(I3)')SPEC
C       READ (SP,'(I3)') SPEC
      READ(VOLEQ(2:2),'(I1)')GEOA
	
C       GEOA = INT ((VOLEQ(2) - 800) / 10)
      IF (GEOA.LT.1 .OR. GEOA.GT.9 .OR. GEOA.EQ.8)THEN
         ERRFLAG = 1
         GO TO 999
      ENDIF

      READ(VOLEQ(3:3),'(I1)')EQN
C       EQN  = VOLEQ(2) - (800 + GEOA * 10)

      
      IF(EQN. NE. 0 .AND. EQN .NE. 4 .AND. EQN .NE. 7 .AND. EQN .NE. 9
     > .AND. EQN .NE. 8) THEN
        ERRFLAG = 1  
        GO TO 999
      ENDIF
     
      IF (EQN.EQ.0 .AND. HTTWO.LT.20 .AND. HTTOT.LT.20) THEN
        ERRFLAG = 10
        GO TO 999
      ENDIF
      
      IF (SPEC.EQ.123 .OR. SPEC.EQ.197) THEN
         SPEC = 100
      ELSEIF  (SPEC.EQ.268) THEN       
         SPEC = 261                     
      ELSEIF  (SPEC.EQ.313 .OR. SPEC.EQ.314 .OR. SPEC.EQ.317 .OR.
     >          SPEC.EQ.650 .OR. SPEC.EQ.651. OR.
     >          SPEC.EQ.691 .OR. SPEC.EQ.711 .OR. SPEC.EQ.742 .OR.
     >          SPEC.EQ.762 .OR. SPEC.EQ.920 .OR. SPEC.EQ.930 .OR.
     >          SPEC.EQ.545 .OR. SPEC.EQ.546) THEN
         SPEC = 300
      ELSEIF  (SPEC.EQ.521 .OR. SPEC.EQ.550 .OR. SPEC.EQ.580 .OR.
     >          SPEC.EQ.601 .OR. SPEC.EQ.602 .OR. SPEC.EQ.318) THEN
         SPEC = 500
      ELSEIF  (SPEC.EQ.804 .OR. SPEC.EQ.817 .OR. SPEC.EQ.820 .OR.
     >          SPEC.EQ.823 .OR. SPEC.EQ.825. OR.
     >          SPEC.EQ.826 .OR. SPEC.EQ.830 .OR. SPEC.EQ.834) THEN
         SPEC = 800
      ENDIF
c     *********************************************
c        check for httwo equal zero and totht > 0
      IF((EQN.EQ.4.OR.EQN.EQ.7.OR.EQN.EQ.9).AND.
     >             (HTTOT.GT.0.AND.HTTWO.LE.0.AND.CTYPE.EQ.'F'))THEN
	  IF(HTTOT .LT. 20) THEN
	     ERRFLAG = 4
	     GOTO 999
        ENDIF
	  THTFLAG = 1
	ELSE
  	  THTFLAG = 0
      ENDIF

      IF (SPEC.EQ.SPECPR .AND. EQN.EQ.EQNPR .AND. GEOA.EQ.GEOAPR) THEN
         
         IF(EQN.EQ.0) THEN
             CALL TOTHT(VOL,DBHOB,HTTOT,HTTWO,FIXDI,SPEC,SPGRP,
     >              TR,TC,TE,TP,TB,TA,AD,BD,TAF,TBF,TRO,TCO,TEO,
     >              TPO,TBO,TAO,TAFI,TBFI,THTFLAG) 
           
	   ELSEIF(EQN.EQ.8) THEN
	      
	      IF(HTTWO.LT.20.AND.HTTOT.GT.20) HTTWO = HTTOT
	      CALL PROD8(VOL,DBHOB,HTTWO,FIXDI,SPEC,SPGRP,TR,TC,TE,TP,TB,
     >       TA,AD,BD,TAF,TBF,TRO,TCO,TEO,TPO,TBO,TAO,TAFI,TBFI,MTOPP)
     
         ELSE
        
            IF(THTFLAG.EQ.1) THEN
               CALL TOTHT(VOL,DBHOB,HTTOT,HTTWO,FIXDI,SPEC,SPGRP,
     >          TR,TC,TE,TP,TB,TA,AD,BD,TAF,TBF,TRO,TCO,TEO,
     >          TPO,TBO,TAO,TAFI,TBFI,THTFLAG)       
	       
	          HTONE = HTTWO
	       
            ENDIF

            CALL HT479(EQN,VOL,DBHOB,HTONE,HTTWO,FIXDI,FCLSS,SPEC,SPGRP,
     >           R,C,E,P,Q,AD,BD,AF,BF,TOPRAT)

C        IF FVS, USE FCLASS TO FIND VOLUME TO 4 INCH TOP
         ENDIF
        
        GO TO 999
      
      ENDIF
      SPECPR = SPEC
      EQNPR = EQN
      GEOAPR = GEOA
 
      GSPEC = GEOA*1000 + SPEC

C     BINARY SEARCH FOR CORRECT COEFFICIENTS
      DONEFLAG = 0
      LASTFLAG = 0
      FIRST = 1
      LAST = 182
      DO 5, WHILE (DONEFLAG.EQ.0)
         IF(FIRST.EQ.LAST) LASTFLAG = 1
	!DETERMINE WHERE TO CHECK
          HALF=((LAST-FIRST+1)/2) + FIRST   

          CHECK=R8CF(HALF,1)*1000+R8CF(HALF,2)
	!FOUND THE COEFFECIENTS
          IF(GSPEC.EQ.CHECK)THEN      
             PTR = HALF
             DONEFLAG=1
	!MOVE DOWN THE LIST
          ELSEIF(GSPEC.GT.CHECK)THEN  
             FIRST = HALF
	!MOVE UP THE LIST
          ELSEIF(GSPEC.LT.CHECK)THEN   
             LAST = HALF - 1
          ENDIF
	!DID NOT FIND A MATCH
          IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0)THEN   
	!END ROUTINE, NO MATCH
	       IF(GEOA.EQ.9) THEN   
                SPECPR = 0
                EQNPR = 0
                GEOAPR = 0
                ERRFLAG = 6
                GO TO 999
	!SET GEOAPR TO 9 AND RETRY THE SEARCH
	       ELSE                  
                GEOA = 9
                GSPEC = GEOA*1000 + SPEC
                FIRST = 1
                LAST = 182
                LASTFLAG = 0
             ENDIF
          ENDIF     
   5  CONTINUE

C     END BINARY SEARCH

      SPGRP = R8CF(PTR,3) + .5                 
      IF (SPGRP.NE.100 .AND. SPGRP.NE.300 .AND. SPGRP.NE.500)THEN
        ERRFLAG = 6
        GO TO 999
      ENDIF

      AD = R8CF(PTR,4)
      BD = R8CF(PTR,5)
      TOPRAT(1) = R8CF(PTR,16)
      TOPRAT(2) = R8CF(PTR,17)
      TOPRAT(3) = R8CF(PTR,18)

      DONEFLAG = 0
      LASTFLAG = 0
      FIRST = 1
      LAST = 49
      DO 10, WHILE (DONEFLAG.EQ.0)
         IF(FIRST.EQ.LAST) LASTFLAG = 1
	!DETERMINE WHERE TO CHECK
         HALF=((LAST-FIRST+1)/2) + FIRST   
          
	!FOUND THE COEFFECIENTS
         IF((INT(DIBMEN(HALF,1)+.5)).EQ.SPEC) THEN      
            DIBCNT = HALF
            DONEFLAG=1
	!MOVE DOWN THE LIST
         ELSEIF((INT(DIBMEN(HALF,1)+.5)).GT.SPEC)THEN  
            LAST = HALF -1
	!MOVE UP THE LIST
         ELSEIF((INT(DIBMEN(HALF,1)+.5)).LT.SPEC)THEN   
            FIRST = HALF
         ENDIF
	!DID NOT FIND A MATCH
            IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0) THEN
               ERRFLAG = 6
               GO TO 999
            ENDIF   
   10 CONTINUE
C     END BINARY SEARCH

      IF (EQN.EQ.0.OR.EQN.EQ.4 .OR. EQN.EQ.8) THEN
         FIXDI = DIBMEN(DIBCNT,2)
      ELSE
         FIXDI = DIBMEN(DIBCNT,3)
      ENDIF
C***********************************************************************
      IF ((EQN.EQ.0 .OR. EQN.EQ.8).OR.THTFLAG.EQ.1) THEN
         DONEFLAG = 0
         LASTFLAG = 0
         FIRST = 1
         LAST = 49
         DO 20, WHILE (DONEFLAG.EQ.0)
            IF(FIRST.EQ.LAST) LASTFLAG = 1
	!DETERMINE WHERE TO CHECK
            HALF=((LAST-FIRST+1)/2) + FIRST   
          
	!FOUND THE COEFFECIENTS
            IF((INT(TOTAL(HALF,1)+.5)) .EQ. SPEC)THEN      
               TOTCNT = HALF
               DONEFLAG=1
	!MOVE DOWN THE LIST
            ELSEIF((INT(TOTAL(HALF,1)+.5)).GT.SPEC)THEN  
               LAST = HALF -1
	!MOVE UP THE LIST
            ELSEIF((INT(TOTAL(HALF,1)+.5)).LT.SPEC)THEN   
               FIRST = HALF 
            ENDIF
	!DID NOT FIND A MATCH
            IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0)THEN
               ERRFLAG = 6
               GO TO 999   
            ENDIF
   20    CONTINUE

         TR = TOTAL (TOTCNT,2)
         TC = TOTAL (TOTCNT,3)
         TE = TOTAL (TOTCNT,4)
         TP = TOTAL (TOTCNT,5)
         TB = TOTAL (TOTCNT,6)
         TA = TOTAL (TOTCNT,7)
         TRO = OTOTAL (TOTCNT,2)
         TCO = OTOTAL (TOTCNT,3)
         TEO = OTOTAL (TOTCNT,4)
         TPO = OTOTAL (TOTCNT,5)
         TBO = OTOTAL (TOTCNT,6)
         TAO = OTOTAL (TOTCNT,7)
         TAFI = R8CF(PTR,6)               
         TBFI = R8CF(PTR,7)               
         TAF = R8CF(PTR,14)               
         TBF = R8CF(PTR,15)               
      ENDIF

      IF(THTFLAG.EQ.1) THEN
           CALL TOTHT(VOL,DBHOB,HTTOT,HTTWO,FIXDI,SPEC,SPGRP,
     >                TR,TC,TE,TP,TB,TA,AD,BD,TAF,TBF,TRO,TCO,TEO,
     >                TPO,TBO,TAO,TAFI,TBFI,THTFLAG)
	   HTONE = HTTWO
      ENDIF

      IF ((EQN.EQ.0 .OR. EQN.EQ.8)) THEN
	   IF(HTTWO.LT.20.AND.HTTOT.GT.20) HTTWO = HTTOT

         IF(EQN.EQ.8) THEN
	      CALL PROD8(VOL,DBHOB,HTTWO,FIXDI,SPEC,SPGRP,TR,TC,TE,TP,TB,
     >       TA,AD,BD,TAF,TBF,TRO,TCO,TEO,TPO,TBO,TAO,TAFI,TBFI,MTOPP)
         ELSE
           CALL TOTHT(VOL,DBHOB,HTTOT,HTTWO,FIXDI,SPEC,SPGRP,
     >                TR,TC,TE,TP,TB,TA,AD,BD,TAF,TBF,TRO,TCO,TEO,
     >                TPO,TBO,TAO,TAFI,TBFI,THTFLAG)

	   ENDIF

      ELSEIF (EQN.EQ.4) THEN
         DONEFLAG = 0
         LASTFLAG = 0
         FIRST = 1
         LAST = 49
         DO 40, WHILE (DONEFLAG.EQ.0)
            IF(FIRST.EQ.LAST) LASTFLAG = 1
	!DETERMINE WHERE TO CHECK
            HALF=((LAST-FIRST+1)/2) + FIRST   
          
	!FOUND THE COEFFECIENTS
            IF((INT(FOUR(HALF,1)+.5)) .EQ. SPEC)THEN
               FOURCNT = HALF
               DONEFLAG=1
	!MOVE DOWN THE LIST
            ELSEIF((INT(FOUR(HALF,1)+.5)).GT.SPEC)THEN
               LAST = HALF -1
	!MOVE UP THE LIST
            ELSEIF((INT(FOUR(HALF,1)+.5)).LT.SPEC)THEN
               FIRST = HALF
            ENDIF
	!DID NOT FIND A MATCH
            IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0) THEN
               ERRFLAG = 6
               GO TO 999
            ENDIF
   40    CONTINUE

         R = FOUR (FOURCNT,2)
         C = FOUR (FOURCNT,3)
         E = FOUR (FOURCNT,4)
         P = FOUR (FOURCNT,5)
         Q = FOUR (FOURCNT,6)
         AF = R8CF(PTR,8)
         BF = R8CF(PTR,9)

         CALL HT479(EQN,VOL,DBHOB,HTONE,HTTWO,FIXDI,FCLSS,SPEC,SPGRP,  
     >                R,C,E,P,Q,AD,BD,AF,BF,TOPRAT)
C        IF FVS, USE FCLASS TO FIND VOLUME TO 4 INCH TOP


      ELSEIF (EQN.EQ.7) THEN

         I = 1
 60      IF (INT(SEVEN(I,1)+.5).LT.SPEC .AND. I.LE.15) THEN
            I = I + 1
            GO TO 60
         ELSEIF (INT(SEVEN(I,1)+.5).GT.SPEC .OR. I.GT.15) THEN
            ERRFLAG = 6
            GO TO 999
         ENDIF

         R = SEVEN (I,2)
         C = SEVEN (I,3)
         E = SEVEN (I,4)
         P = SEVEN (I,5)
         Q = SEVEN (I,6)
         AF = R8CF(PTR,10)
         BF = R8CF(PTR,11)

         CALL HT479(EQN,VOL,DBHOB,HTONE,HTTWO,FIXDI,FCLSS,SPEC,SPGRP, 
     >                R,C,E,P,Q,AD,BD,AF,BF,TOPRAT)
C        IF FVS, USE FCLSS TO FIND VOLUME TO 4 INCH TOP

      ELSEIF (EQN.EQ.9) THEN

         DONEFLAG = 0
         LASTFLAG = 0
         FIRST = 1
         LAST = 34
         DO 80, WHILE (DONEFLAG.EQ.0)
            IF(FIRST.EQ.LAST) LASTFLAG = 1
	!DETERMINE WHERE TO CHECK
            HALF=((LAST-FIRST+1)/2) + FIRST
          
	!FOUND THE COEFFECIENTS
            IF((INT(NINE(HALF,1)+.5)) .EQ. SPEC)THEN
               NINECNT = HALF
               DONEFLAG=1
	!MOVE DOWN THE LIST
            ELSEIF((INT(NINE(HALF,1)+.5)).GT.SPEC)THEN
               LAST = HALF -1
	!MOVE UP THE LIST
            ELSEIF((INT(NINE(HALF,1)+.5)).LT.SPEC)THEN
               FIRST = HALF
            ENDIF
	!DID NOT FIND A MATCH
            IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0) THEN
               ERRFLAG = 6
               GO TO 999
            ENDIF
   80    CONTINUE
         R = NINE(NINECNT,2)
         C = NINE(NINECNT,3)
         E = NINE(NINECNT,4)
         P = NINE(NINECNT,5)
         Q = NINE(NINECNT,6)
         AF = R8CF(PTR,12)
         BF = R8CF(PTR,13)

         CALL HT479(EQN,VOL,DBHOB,HTONE,HTTWO,FIXDI,FCLSS,SPEC,SPGRP,  
     >                R,C,E,P,Q,AD,BD,AF,BF,TOPRAT)
C        IF FVS, USE FCLASS TO FIND VOLUME TO 4 INCH TOP

      ELSE
      !   GO TO 998

      ENDIF
      
 999  CONTINUE
 
      RETURN
      END


c**********************************************************************
       SUBROUTINE TOTHT(VOL,DBH,THT,HTTWO,UPPERD,SPEC,SPGRP,      
     >                   R,C,E,P,B,A,AD,BD,AF,BF,
     >                   RO,CO,EO,PO,BO,AO,AFI,BFI,THTFLAG)       

C      PURPOSE: CALCULATES PULPWOOD VOLUME TO A 4" TOP USING
C               FORMULAS BASED ON TOTAL HEIGHT

C      *** DECLARE PASSED VARIABLES ***
      REAL VOL(15), DBH, HTTWO,THT                     
      REAL R, C, E, P, B, A, AD, BD, AF, BF
      REAL RO,CO,EO,PO,BO,AO,AFI,BFI                     
      INTEGER SPEC, SPGRP,THTFLAG

C      *** DECLARE LOCAL VARIABLES ***
      REAL DIB2, DIB3, FCLSS,FCLSS2, V,W,X,Y,Z,T,V1,V2,QA,QB,QC,
     >      HT,SEG1,SEG2,SEG3,LOWER,UPPER,L1,L2,L3,U1,U2,U3,S1,S2,S3,
     >      VO,WO,XO,YO,ZO,DBH2,DBH3,FCDOB,FCDOB2,UPPERD,UPPERD2
      INTEGER IS,IB,IT,IM,I1,I2,I3,I4,I5,I6
      REAL FCMIN, FCDIB, VOLINI,DIB
      
C     ***Testing variables
      REAL FCLSCLC
      FCLSCLC = 0.0

      VOLINI = 0.0
      FCDIB = 0.0

C      CALCULATE DBH INSIDE BARK

      DIB =  AD + BD*DBH
      IF (DIB.LT.UPPERD) DIB = UPPERD

C      CALCULATE DIAMETER AT 17.3 FEET (FORMCLASS)
      IF(THT .LE. 0) THT = HTTWO
C      THT = TREEHT

  2   FCLSS = DBH * (AF + BF * (17.3/THT)**2)
      FCLSCLC = FCLSS
      IF (FCLSS.LT.0.0) FCLSS = 0.0

      IF (SPEC.EQ.221 .OR. SPEC.EQ.222 .OR. SPEC.EQ.544) GO TO 8

      IF (SPGRP.EQ.100) THEN
             IF (THT.LT.32.5)                   FCMIN = 56
             IF (THT.GE.32.5 .AND. THT.LT.37.5) FCMIN = 64
             IF (THT.GE.37.5 .AND. THT.LT.42.5) FCMIN = 66
             IF (THT.GE.42.5)                   FCMIN = 67
      ELSEIF (SPGRP.EQ.300) THEN
             IF (THT.LT.32.5)                   FCMIN = 57
             IF (THT.GE.32.5 .AND. THT.LT.37.5) FCMIN = 60
             IF (THT.GE.37.5 .AND. THT.LT.42.5) FCMIN = 64
             IF (THT.GE.42.5)                   FCMIN = 67
      ELSE
             IF (THT.LT.32.5)                   FCMIN = 58
             IF (THT.GE.32.5 .AND. THT.LT.37.5) FCMIN = 65
             IF (THT.GE.37.5 .AND. THT.LT.42.5) FCMIN = 67
             IF (THT.GE.42.5)                   FCMIN = 69
      ENDIF

      FCDIB = DBH * FCMIN * .01

      IF (THT.LT.47.5 .AND. FCLSS.LT.FCDIB) FCLSS = FCDIB

   8  FCDOB = (FCLSS - AFI)/BFI
      If (FCDOB.LT.FCLSS) FCDOB = FCLSS

C      CALCULATE VARIABLES FOR HEIGHT
      IF (UPPERD .LE. 4) THEN
            UPPERD2 = 16
	ELSEIF (UPPERD .LE. 7) THEN
	   UPPERD2 = 49
	ELSEIF (UPPERD.LE. 9) THEN
	   UPPERD2 = 81
      ENDIF

      DBH2 = DBH**2
      DBH3 = DBH**3
      FCDOB2 = FCDOB**2

      VO = (1-4.5/THT)**RO
      WO = (CO + EO/(DBH3))/(1-VO)
      XO = (1-4.5/THT)**PO
      YO = (1-17.3/THT)**PO
      ZO = (DBH2 - FCDOB2)/(XO-YO)

C  *** CALCULATE HEIGHT TO 4, 7, OR 9" TOP ***

      IF (UPPERD2.GT.DBH2) THEN
           HT = 4.5
           GO TO 10
      ENDIF

C      SET INDICATOR VARIABLES

      IS = 0
      IB = 0
      IT = 0
      IM = 0

      IF (UPPERD2.GE.DBH2) IS = 1
      IF (DBH2.GT.UPPERD2.AND.UPPERD2.GE.FCDOB2)  IB = 1
      IF (FCDOB2.GT.UPPERD2)   IT = 1
      IF (UPPERD2.GT.((BO*(AO-1)**2)*FCDOB2))  IM = 1

C      CALCULATE HEIGHT  *******************************************


C -- INITIALIZE SEG* VARIABLES
      IF(IS.EQ.1) THEN
         HTTWO = THT*(1-((UPPERD2/DBH2-1)/WO+VO)**(1/RO))
      ELSEIF(IB.EQ.1) THEN
         HTTWO = THT*(1-(XO-(DBH2-UPPERD2)/ZO)**(1/PO))
      ELSE
          V1 = THT - 17.3
          QA = BO + IM*((1-BO)/AO**2)
          QB = -2 * BO - IM * 2*((1-BO)/AO)
          QC = BO + (1-BO)*IM - UPPERD2/FCDOB2
          V2 = (-QB - (SQRT(QB**2 - 4*QA*QC)))/(2*QA)
          HTTWO = (17.3 + V1*V2)
      ENDIF

c     height to the upper top
      IF (HTTWO.LT.4.5) HTTWO = 4.5
       
      IF(THTFLAG.EQ.1) GO TO 998

C  *** CALCULATE CUBIC VOLUME ***

  10  DIB2 = DIB**2
      DIB3 = DIB**3
      FCLSS2 = FCLSS**2

      V = (1-4.5/THT)**R
      W = (C + E/(DIB3))/(1-V)
      X = (1-4.5/THT)**P
      Y = (1-17.3/THT)**P
      Z = (DIB2 - FCLSS2)/(X-Y)
      T = (DIB2 - Z * X)

C      FIND UPPER AND LOWER HEIGHTS OF EACH SEGMENT

      LOWER = 0.5
      UPPER = HTTWO
      L1 = 0.5
      U1 = 4.5
      L2 = 4.5
      U2 = 17.3
      L3 = 17.3
      U3 = THT
      IF (UPPER.LT.4.5) U1 = UPPER
      IF (LOWER.GT.4.5) L2 = LOWER
      IF (UPPER.LT.17.3) U2 = UPPER
      IF (LOWER.GT.17.3) L3 = LOWER
      IF (UPPER.LT.THT) U3 = UPPER

C      SET INDICATOR VARIABLES

      I1 = 1
      I2 = 1
      I3 = 0
      I4 = 0
      I5 = 0
      I6 = 0
      IF (UPPER.GT.4.5)  I3 = 1
      IF (UPPER.GT.17.3)  I4 = 1
      IF ((L3-17.3).LT.A*(THT-17.3))   I5 = 1
      IF ((U3-17.3).LT.A*(THT-17.3))  I6 = 1

C      CALCULATE CUBIC VOLUME

       S1 = I1*DIB2*((1-V*W)*(U1-L1)+W*((1-
     >    L1/THT)**R*(THT-L1)-(1-U1/THT)**R*(THT-U1))/(R+1))
       S2 = I2*I3*(T*(U2-L2)+Z*((1-L2/THT)**P*(THT-L2)-
     >    (1-U2/THT)**P*(THT-U2))/(P+1))
       S3 = I4*FCLSS2*(B*(U3-L3)-B*((U3-17.3)**2-(L3-17.3)**2)
     >    /(THT-17.3)+(B/3)*((U3-17.3)**3-(L3-17.3)**3)/(THT-
     >    17.3)**2 + I5*(1.0/3.0)*((1-B)/(A)**2)*(A*(THT-17.3)
     >    -(L3-17.3))**3/(THT-17.3)**2 - I6*(1.0/3.0)*
     >    ((1-B)/(A)**2)*(A*(THT-17.3)-(U3-17.3))**3/(THT-17.3)**2)

      VOL(4) = 0.005454*(S1 + S2 + S3)

      IF (THT.GE.47.5 .AND. FCLSS.LT.FCDIB) THEN
         VOLINI = VOL(4)
         THT = 47.49
         GO TO 2
      ENDIF

      IF (VOLINI.GT.VOL(4)) VOL(4) = VOLINI
      IF (VOL(4).LT.0.3) VOL(4) = 0.3
c      VOL(5) = VOL(4)

c      OPEN (UNIT=FCLSOUT, FILE='fclass.txt', ACCESS = 'APPEND',
c     &       STATUS='UNKNOWN')
c	       WRITE (FCLSOUT,100)DBH,',',FCLSS,',',FCLSCLC
c 100         FORMAT(F4.1,A,F4.1,A,F4.1)
c      CLOSE(FCLSOUT)
C      Calculate stump vol
       VOL(14)=0.005454154*DIB**2*LOWER       

  998 RETURN
      END


c**********************************************************************
       SUBROUTINE HT479(EQN,VOL,DBH,THT1,THT2,FIXDI,FCLSS,SPEC,
     >                  SPGRP,R,C,E,P,Q,AD,BD,AF,BF,TOPRAT)

C      PURPOSE:  CALCULATES CUBIC FOOT VOLUMES FOR PULPWOOD,
C                SAWTIMBER, AND TOPWOOD USING EQUATIONS BASED ON
C                HEIGHT TO A 4 INCH, 7 INCH, OR 9 INCH TOP

C      *** DECLARE PASSED VARIABLES ***
       INTEGER EQN, SPEC, SPGRP                               
       REAL VOL(15), DBH, THT1, THT2, FIXDI
       REAL R, C, E, P, Q, AD, BD, AF, BF
       REAL TOPRAT(3)

C      *** DECLARE LOCAL VARIABLES ***
       REAL VOL4,VOLM,VOL79,NLE,YR,AT,BT,CT,STFAC,STUMP
       REAL DIB,FCLSS,HT, HT1, HT2, FCMIN, FCDIB, VOLINI     
       
C     ***Testing variables
      REAL FCLSCLC
      FCLSCLC = 0.0 
      IF(EQN.EQ.4)THEN
        STUMP=0.5
      ELSE
        STUMP=1.0
      ENDIF
       VOLINI = 0.0                                           
       FCDIB = 0.0                                            
       HT1 = THT1
       HT2 = THT2

       STFAC = 1.0
       IF (EQN.EQ.4) THEN
         IF (HT2.LE.17.3) THEN
           STFAC = (THT2 - 0.5) / (17.31 - 0.5)
           HT2 = 17.31
         ENDIF
         HT = HT2
       ELSE IF (EQN.EQ.7.OR.EQN.EQ.9) THEN
         IF (HT2.LE.17.3) THEN
           STFAC = (HT1 - 1.0) / (17.3 - 1.0)
           HT2 = 17.31
           HT1 = 17.31
         ENDIF
         HT = HT1
       ENDIF

C      CALCULATE DBH INSIDE BARK

       DIB =  AD + BD*DBH
       IF (DIB.LT.FIXDI) DIB = FIXDI

C      CALCULATE DIAMETER AT 17.3 FEET (FORMCLASS)

   2   FCLSS = DBH * (AF + BF * (17.3/HT2)**2)
       FCLSCLC = FCLSS
       IF (FCLSS.LT.FIXDI) FCLSS = FIXDI

       IF (SPEC.EQ.221 .OR. SPEC.EQ.222 .OR. SPEC.EQ.544) GO TO 8
       IF (EQN.EQ.4) THEN
          IF (SPGRP.EQ.100) THEN
             IF (DBH.LT.5.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 70
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 75
                IF (HT2.GE.27.5)                   FCMIN = 80
             ELSEIF (DBH.LT.6.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 66
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 70
                IF (HT2.GE.27.5)                   FCMIN = 74
             ELSEIF (DBH.LT.7.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 64
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 67
                IF (HT2.GE.27.5)                   FCMIN = 72
             ELSEIF (DBH.LT.8.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 60
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 67
                IF (HT2.GE.27.5)                   FCMIN = 69
             ELSE
                IF (HT2.LT.22.5)                   FCMIN = 59
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 67
                IF (HT2.GE.27.5)                   FCMIN = 69
             ENDIF
          ELSEIF (SPGRP.EQ.300) THEN
             IF (DBH.LT.5.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 74
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 76
                IF (HT2.GE.27.5)                   FCMIN = 76
             ELSEIF (DBH.LT.6.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 65
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 69
                IF (HT2.GE.27.5)                   FCMIN = 74
             ELSEIF (DBH.LT.7.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 61
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 65
                IF (HT2.GE.27.5)                   FCMIN = 68
             ELSE
                IF (HT2.LT.22.5)                   FCMIN = 60
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 62
                IF (HT2.GE.27.5)                   FCMIN = 68
             ENDIF
          ELSE
             IF (DBH.LT.5.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 71
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 72
                IF (HT2.GE.27.5)                   FCMIN = 76
             ELSEIF (DBH.LT.6.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 68
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 71
                IF (HT2.GE.27.5)                   FCMIN = 74
             ELSE
                IF (HT2.LT.22.5)                   FCMIN = 63
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 67
                IF (HT2.GE.27.5)                   FCMIN = 70
             ENDIF
          ENDIF
          FCDIB = DBH * FCMIN * .01
          IF (HT2.LT.32.5 .AND. FCLSS.LT.FCDIB) FCLSS = FCDIB
       ENDIF

C      CALCULATE CUBIC FOOT VOLUMES

   8   CALL TAPVOL (VOLM,DIB,FCLSS,FIXDI,R,C,E,P,Q,HT,HT2,EQN)

       IF (EQN.EQ.4 .AND. HT2.GE.32.5 .AND. FCLSS.LT.FCDIB) THEN
           VOLINI = VOLM
           HT2 = 32.49
           HT = HT2
           GO TO 2
       ENDIF

       IF (VOLINI.GT.VOLM) VOLM = VOLINI

       VOL(4) = VOLM * STFAC
       IF (VOL(4).LT.0.5) VOL(4) = 0.5

       IF (EQN.EQ.7.OR.EQN.EQ.9) THEN

c         VOL(5) = VOL(4) - (VOL(4)*DEF1)/100

         AT = TOPRAT(1)
         BT = TOPRAT(2)
         CT = TOPRAT(3)

         NLE = 2.71828
         HT = HT2

         YR = (NLE**(AT*((FCLSS**BT)*(HT2**CT))))

         CALL TAPVOL (VOL79,DIB,FCLSS,FIXDI,R,C,E,P,Q,HT,HT2,EQN)
         VOL4 = YR * VOL79 * STFAC
         VOL(7) = VOL4 - vol(4)
         VOL(8) = VOL(7)
 
       ELSE
         VOL(5) = VOL(4)
C--  USE VOL(4) INSTEAD OF VOLM BECAUSE IT HAS BEEN ADJUSTED
C--  FOR SHORT PULPWOOD TREES
C--      VOL(5) = VOLM

       ENDIF
C      Calculate stump vol
       VOL(14)=0.005454154*DIB**2*STUMP       
!       OPEN (UNIT=FCLSOUT, FILE='fclass.txt', ACCESS = 'APPEND',
!     &       STATUS='UNKNOWN')
!	       WRITE (FCLSOUT,100)DBH,',',FCLSS,',',FCLSCLC
! 100         FORMAT(F4.1,A,F4.1,A,F4.1)
!      CLOSE(FCLSOUT)

 999   RETURN
       END

c**********************************************************************
       SUBROUTINE TAPVOL (VOLU,DIB,FCLSS,FIXDI,
     >                       R,C,E,P,Q,HT,HT2,EQN)

C      PURPOSE:  CALCULATE THE CUBIC FOOT VOLUME FOR A GIVEN
C                HEIGHT (HT), DIB, AND FORM CLASS USING EQUATIONS
C                FOR A 4, 7, OR 9 INCH DIAMETER TOP

       REAL VOLU, DIB,FCLSS,FIXDI,R,C,E,P,Q,HT,HT2
	 REAL TERML1,TERML2,TERML3,TERMU1,TERMU2,TERMU3
	REAL TL1,TL2,TL3,TU1,TU2,TU3
       REAL DIB2, FCLSS2, DIB3, LOWER, UPPER, L1,
     >      L2, L3, U1, U2, U3, V, W, X, Y, Z, T, G
       REAL V1,X1,Y1,G1
       real CAPR, N
       INTEGER I1, I2, I3, I4, EQN
C      real term1, term2, term3

C      INITIALIZE VARIABLES FOR THE VOLUME EQUATION


       DIB2 = DIB**2
       FCLSS2 = FCLSS**2
       DIB3 = DIB**3
       V1 = (1.-4.5/HT2)
       IF(V1.LT.0.01)THEN
         V = 0
       ELSE
         V = (V1)**R
       ENDIF
       
       W = (C + E/(DIB3))/(1-V)
       X1 = (1-4.5/HT2)
       IF(X1.LT.0.01)THEN
         X = 0
       ELSE
         X = (X1)**P
       ENDIF

       Y1 = (1-17.3/HT2)
       IF(Y1.LT.0.01)THEN
         Y = 0
       ELSE
         Y = (Y1)**P
       ENDIF
       Z = (DIB2-FCLSS2)/(X-Y)
       T = DIB2-Z*X

       G1 = (1.-17.3/HT2)
       IF(G1.LT.0.01)THEN
         G = 0
       ELSE
         G = (G1)**Q
       ENDIF
       IF (G.EQ.0) THEN
         CAPR = 0
       ELSE
         CAPR = (FCLSS2-FIXDI**2)/G
       ENDIF
       N = FCLSS2 - CAPR*G

C      SET UPPER AND LOWER HEIGHTS FOR EACH SEGMENT

       IF (EQN.EQ.4) THEN
          LOWER = 0.5
       ELSE
          LOWER = 1.0
       ENDIF

       UPPER = HT
       L1 = LOWER
       U1 = 4.5
       L2 = 4.5
       U2 = 17.3
       L3 = 17.3
       U3 = HT2
       IF (UPPER.LT.4.5) U1 = UPPER
       IF (LOWER.GT.4.5) L2 = LOWER
       IF (UPPER.LT.17.3) U2 = UPPER
       IF (LOWER.GT.17.3) L3 = LOWER
       IF (UPPER.LT.HT2) U3 = UPPER

C      SETS INDICATOR VARIABLES

       I1 = 1
       I2 = 1
       I3 = 0
       I4 = 0
       IF (UPPER.GT.4.5)   I3 = 1
       IF (UPPER.GT.17.3)  I4 = 1

      TL1 = 1.0 - L1/HT2
      IF(TL1 .LT. 0.01) THEN
         TERML1 = 0.0
	ELSE
	   TERML1 = TL1**R*(HT2-L1)
	ENDIF

      TU1 = 1.0 - U1/HT2
      IF(TU1 .LT. 0.01)THEN
         TERMU1 = 0.0
	ELSE
	   TERMU1 = TU1**R*(HT2-U1)
	ENDIF

      TL2 = 1.0 - L2/HT2
      IF(TL2.LT. 0.01)THEN
         TERML2 = 0.0
	ELSE
	   TERML2 = TL2**P*(HT2-L2)
	ENDIF

      TU2 = 1.0 - U2/HT2
      IF(TU2 .LT. 0.01)THEN
         TERMU2 = 0.0
	ELSE
	   TERMU2 = TU2**P*(HT2-U2)
	ENDIF

	TL3 = 1.0 - L3/HT2
      IF(TL3 .LT. 0.01)THEN
         TERML3 = 0.0
	ELSE
	   TERML3 = TL3**Q *(HT2-L3)
	ENDIF

      TU3 = 1.0 - U3/HT2
      IF(TU3 .LT. 0.01)THEN
         TERMU3 = 0.0
	ELSE
	   TERMU3 = TU3**Q *(HT2-U3)
	ENDIF

	VOLU = 0.005454 * (I1*DIB2*((1.-V*W)*(U1-L1)+
     >   W*((TERML1)-(TERMU1))/(R+1))+
     >	I2*I3*(T*(U2-L2)+Z*((TERML2)-
     >   (TERMU2))/(P+1.))+ I4*(N*(U3-L3)+ 
     >   CAPR*((TERML3)-
     >   (TERMU3))/(Q+1.)))

c      VOLU = 0.005454 * (I1*DIB2*((1.-V*W)*(U1-L1)+W*((1.-
c     >    L1/HT2)**R*(HT2-L1)-(1.-U1/HT2)**R*(HT2-U1))
c     >    /(R+1))+I2*I3*(T*(U2-L2)+Z*((1-L2/HT2)**P
c     >    *(HT2-L2)-(1-U2/HT2)**P*(HT2-U2))/(P+1.))+
c     >    I4*(N*(U3-L3)+ CAPR*((1-L3/HT2)**Q *(HT2-L3)-(1.-
c     >    U3/HT2)**Q *(HT2-U3))/(Q+1.)))
      
      
       RETURN
       END


************************************************************************
	SUBROUTINE PROD8(VOL,DBH,TREEHT,D,SPEC,SPGRP,      
     >                   R,C,E,P,B,A,AD,BD,AF,BF,
     >                   RO,CO,EO,PO,BO,AO,AFI,BFI,MTOPP)
************************************************************************
************************************************************************

C      PURPOSE: CALCULATES PULPWOOD VOLUME TO A 4" TOP USING
C               FORMULAS BASED ON TOTAL HEIGHT
C               CALCULATES HEIGHT TO MTOPP
C               RETURNS VOLUME TO MTOPP AND TOPWOOD TO 4 INCH TOP

C      *** DECLARE PASSED VARIABLES ***
       REAL VOL(15), DBH, TREEHT, D                       
       REAL R, C, E, P, B, A, AD, BD, AF, BF
       REAL RO,CO,EO,PO,BO,AO,AFI,BFI,MTOPP
       INTEGER SPEC, SPGRP                                

C      *** DECLARE LOCAL VARIABLES ***
      CHARACTER*1 EQN
       REAL FCLSS,V1,V2,QA,QB,QC,HT,SEG1,SEG2,SEG3,
     >      VO,WO,XO,YO,ZO,DBH2,DBH3,D42,FCDOB,FCDOB2
       INTEGER IS,IB,IT,IM
       REAL THT, FCMIN, FCDIB, VOLINI,DIB,DBHIB
      INTEGER LAST,TOPLOP,I,FIRST,HALF,ERRFLG
      REAL MHT,TOP1,MINLEN,STUMP,HT2,VOLM,VOL4,Q
	
C     return Zero volume for tree height less than 17.3 (YW 10/11/2012)
	IF (TREEHT.LT.17.3) THEN
	  VOL(4) = 0.0
	  VOL(7) = 0.0
	  RETURN
	ENDIF
	
      MINLEN = 12.0
	STUMP = 0.5
      Q = 0
      EQN = '0'

       VOLINI = 0.0
       FCDIB = 0.0

C      CALCULATE DBH INSIDE BARK

       DBHIB =  AD + BD*DBH
       IF (DBHIB.LT.D) DBHIB = D

C      CALCULATE DIAMETER AT 17.3 FEET (FORMCLASS)

       THT = TREEHT

  2    FCLSS = DBH * (AF + BF * (17.3/THT)**2)
       If (FCLSS.LT.0) FCLSS = 0.0

       IF (SPEC.EQ.221 .OR. SPEC.EQ.222 .OR. SPEC.EQ.544) GO TO 8

       IF (SPGRP.EQ.100) THEN
             IF (THT.LT.32.5)                   FCMIN = 56
             IF (THT.GE.32.5 .AND. THT.LT.37.5) FCMIN = 64
             IF (THT.GE.37.5 .AND. THT.LT.42.5) FCMIN = 66
             IF (THT.GE.42.5)                   FCMIN = 67
       ELSEIF (SPGRP.EQ.300) THEN
             IF (THT.LT.32.5)                   FCMIN = 57
             IF (THT.GE.32.5 .AND. THT.LT.37.5) FCMIN = 60
             IF (THT.GE.37.5 .AND. THT.LT.42.5) FCMIN = 64
             IF (THT.GE.42.5)                   FCMIN = 67
       ELSE
             IF (THT.LT.32.5)                   FCMIN = 58
             IF (THT.GE.32.5 .AND. THT.LT.37.5) FCMIN = 65
             IF (THT.GE.37.5 .AND. THT.LT.42.5) FCMIN = 67
             IF (THT.GE.42.5)                   FCMIN = 69
       ENDIF

       FCDIB = DBH * FCMIN * .01

       IF (THT.LT.47.5 .AND. FCLSS.LT.FCDIB) FCLSS = FCDIB

   8   FCDOB = (FCLSS - AFI)/BFI
       If (FCDOB.LT.FCLSS) FCDOB = FCLSS

C      CALCULATE VARIABLES FOR HEIGHT

       D42 = 16.0
       DBH2 = DBH**2
       DBH3 = DBH**3
       FCDOB2 = FCDOB**2

       VO = (1-4.5/THT)**RO
       WO = (CO + EO/(DBH3))/(1-VO)
       XO = (1-4.5/THT)**PO
       YO = (1-17.3/THT)**PO
       ZO = (DBH2 - FCDOB2)/(XO-YO)

C  *** CALCULATE HEIGHT TO 4" TOP ***

       IF (D42.GT.DBH2) THEN
           HT = 4.5
           GO TO 10
       ENDIF

C      SET INDICATOR VARIABLES

       IS = 0
       IB = 0
       IT = 0
       IM = 0

       IF (D42.GE.DBH2) IS = 1
       IF (DBH2.GT.D42.AND.D42.GE.FCDOB2)  IB = 1
       IF (FCDOB2.GT.D42)   IT = 1
       IF (D42.GT.((BO*(AO-1)**2)*FCDOB2))  IM = 1

C      CALCULATE HEIGHT

       IF (IT.EQ.1) THEN
          V1 = THT - 17.3
             QA = BO + IM*((1-BO)/AO**2)
             QB = -2 * BO - IM * 2*((1-BO)/AO)
             QC = BO + (1-BO)*IM - D42/FCDOB2
          V2 = (-QB - (SQRT(QB**2 - 4*QA*QC)))/(2*QA)
       ENDIF

C -- INITIALIZE SEG* VARIABLES
       SEG1 = 0.0
       SEG2 = 0.0
       SEG3 = 0.0

       IF (IS.EQ.1) THEN
         SEG1 = IS*THT*(1-((D42/DBH2-1)/WO+VO)**(1/RO))
       ENDIF

       IF (IB.EQ.1) THEN
         SEG2 = IB*THT*(1-(XO-(DBH2-D42)/ZO)**(1/PO))
       ENDIF

       SEG3 = IT*(17.3 + V1*V2)

c     height to the four inch top
       HT = SEG1 + SEG2 + SEG3
       IF (HT.LT.4.5) HT = 4.5

C  *** CALCULATE CUBIC VOLUME ***

 10   CALL R8CUBIC(VOL4,EQN,DBHIB,FCLSS,THT,STUMP,
     &              HT,4.0,R,C,E,P,B,A,Q,ERRFLG)

       IF (VOL4.LT.0.3) VOL4 = 0.3

C     FIND HEIGHT TO MTOPP USING ITERITIVE APPROACH
C--   SET TOP1 EQUAL TO DESIRED TOP TIMES 10 TRUNCATED
       TOP1=ANINT((MTOPP +.005)*10.00)

       FIRST = 1
C--   FIND DESIRED TOP TO THE NEAREST TENTH OF A FOOT
       LAST=INT(THT+.5)*10
       TOPLOP = LAST

       DO 99 I = 1,TOPLOP
          IF (FIRST .EQ. LAST) THEN
             GO TO 100
          ENDIF
          HALF = (FIRST + LAST + 1 ) / 2
          HT2=FLOAT(HALF)/10.0

C##########
          CALL R8DIB(DIB,DBHIB,FCLSS,THT,HT2,
     &                 R,C,E,P,B,A,Q,ERRFLG)

C--     CONVERT TOP DIAMETER TO TENTH INCH TRUNCATED.
C--     BEFORE TRUNCATION ADD .005 TO ROUND UP THE DIAMETERS
C--     THAT ARE CLOSE - 5.995 AND ABOVE WILL BE CONVERTED TO 6

          DIB=INT((DIB+.005)*10.0)
          IF(TOP1 .EQ. DIB) THEN
	      GO TO 100
          ELSE IF (TOP1 .LT. DIB) THEN
C--          MOVE UP STEM
             FIRST = HALF
          ELSE
C--        MOVE DOWN THE STEM
             LAST = HALF - 1
          ENDIF
   99  CONTINUE

  100  CONTINUE

       MHT = FLOAT(FIRST)/10.0 - STUMP
      
C     IF HEIGHT MINUS STUMP IS GE 12.0, FIND VOLUME
       IF(MHT .GE. MINLEN) THEN
          CALL R8CUBIC(VOLM,EQN,DBHIB,FCLSS,THT,STUMP,
     &              MHT,MTOPP,R,C,E,P,B,A,Q,ERRFLG)

C     FIND TOPWOOD VOLUME
         VOL(4) = VOLM
	   VOL(7) = VOL4 - VOLM
	   IF(VOL(7).LT.0.0001) VOL(7) = 0.0
	 ELSE
	   VOL(4) = 0
         VOL(7) = VOL4
       ENDIF
C     calculate stump vol
      VOL(14)=0.005454154*DBHIB**2*STUMP       
C     RETURN VOLUMES



       RETURN
       END
************************************************************************
      subroutine R8CUBIC(volume,topdob,dbhib,dib17,topht,lowht,
     &                   highht,topdib,r,c,e,p,b,a,q,errflg)
************************************************************************
C  Calculates cubic foot volume (volume) from the stump height (lowht) 
C  to the specified height (highht), if 
C  that is lower), given inside-bark dbh (dbhib), inside-bark diameter 
C  at 17.3' (dib17) and height (topht) to the top diameter (topdob), 
C  for the specified species (spp) and  
C  r, c, e, p, b, a and q are the coefficients for inside-bark 
C  volume calculation.

      integer   errflg,i1,i2,i3,i4,i5,i6
      real      topht,lowht,volume,dbhib,dib17,topdib
      real      highht
      real      r,c,e,p,b,a,q,g,w,x,y,z,t,jj,rr,n
      real      l1,l2,l3,u1,u2,u3
      character*1 topdob

      volume=0.0

C--   Set combined variables
      g=(1-4.5/topht)**r
      w=(c+e/dbhib**3)/(1-g)
      x=(1-4.5/topht)**p
      y=(1-17.3/topht)**p
      z=(dbhib**2-dib17**2)/(x-y)
      t=dbhib**2-z*x
      jj=(1-17.3/topht)**q
      rr=(dib17**2-topdib**2)/jj
      n=dib17**2-rr*jj
      l1=max(lowht,0.0)
      u1=min(highht,4.5)
      l2=max(lowht,4.5)
      u2=min(highht,17.3)
      l3=max(lowht,17.3)
      u3=min(topht,highht)

C--   Set indicator variables
      if(lowht.lt.4.5) then
        i1=1
      else
        i1=0
      endif
      if(lowht.lt.17.3) then
        i2=1
      else
        i2=0
      endif
      if(highht.gt.4.5) then
        i3=1
      else
        i3=0
      endif
      if(highht.gt.17.3) then
        i4=1
      else
        i4=0
      endif
      if((l3-17.3).lt.a*(topht-17.3)) then
        i5=1
      else
        i5=0
      endif
      if((u3-17.3).lt.a*(topht-17.3)) then
        i6=1
      else
        i6=0
      endif

C-----Calculate cubic volume stump to specified height------------------
      if(topdob.eq.'0') then
C--     Total height provided - volume is to 4" DOB.
        volume=0.005454154*(i1*dbhib**2*((1-g*w)*(u1-l1)
     &          +w*((1-l1/topht)**r*(topht-l1)-(1-u1/topht)**r
     &          *(topht-u1))/(r+1))
     &        +i2*i3*(t*(u2-l2)+z*((1-l2/topht)**p*(topht-l2)
     &          -(1-u2/topht)**p*(topht-u2))/(p+1))
     &        +i4*dib17**2*(b*(u3-l3)-b*((u3-17.3)**2-(l3-17.3)**2)
     &          /(topht-17.3)+(b/3)*((u3-17.3)**3-(l3-17.3)**3)
     &          /(topht-17.3)**2
     &        +i5*(1.0/3.0)*((1-b)/a**2)*(a*(topht-17.3)-(l3-17.3))**3
     &          /(topht-17.3)**2
     &        -i6*(1.0/3.0)*((1-b)/a**2)*(a*(topht-17.3)-(u3-17.3))**3
     &          /(topht-17.3)**2))
      else
C--     Height to a 4", 7", or 9" DOB provided
        volume=0.005454154*(i1*dbhib**2*((1-g*w)*(u1-l1)
     &          +w*((1-l1/topht)**r*(topht-l1)-(1-u1/topht)**r
     &          *(topht-u1))/(r+1))
     &        +i2*i3*(t*(u2-l2)+z*((1-l2/topht)**p*(topht-l2)
     &          -(1-u2/topht)**p*(topht-u2))/(p+1))
     &        +i4*(n*(u3-l3)+rr*((1-l3/topht)**q*(topht-l3)
     &          -(1-u3/topht)**q*(topht-u3))/(q+1)))
      endif        
      if(volume.lt.0.0) volume=0.0
      return
      end

************************************************************************
      subroutine R8DIB(dib,dbhib,dib17,topht,highht,
     &                 r,c,e,p,b,a,q,errflg)
************************************************************************
C  Calculates inside-bark diameter (dib) at any specified height (highht)
C  given inside-bark dbh (dbhib), inside-bark diameter at 17.3' (dib17) 
C  and total height (topht) for the specified 
C  species (spp).   r, c, e, p, b, a, and q 
C  are the coefficients for inside-bark diameter calculation.

      integer   errflg,is,ib,it,im
      real      dib,topht,dbhib,dib17
      real      highht
      real      r,c,e,p,b,a,q

      dib=0.0

C--   Set height indicator variables
      if(highht.lt.4.5) then
        is=1
      else
        is=0
      endif
      if(highht.ge.4.5 .and. highht.le.17.3) then
        ib=1
      else
        ib=0
      endif
      if(highht.gt.17.3) then
        it=1
      else
        it=0
      endif
      if(highht.lt.(17.3+a*(topht-17.3))) then
        im=1
      else
        im=0
      endif

C-----Get DIB at specified height---------------------------------------
C--     Total height provided (eqn 1)
        dib=(is*(dbhib**2*(1+(c+e/dbhib**3)*((1-highht/topht)**r
     &       -(1-4.5/topht)**r)/(1-(1-4.5/topht)**r)))
     &     +ib*(dbhib**2-(dbhib**2-dib17**2)*((1-4.5/topht)**p
     &       -(1-highht/topht)**p)/((1-4.5/topht)**p-(1-17.3/topht)**p))
     &     +it*(dib17**2*(b*(((highht-17.3)/(topht-17.3))-1)**2
     &     +im*((1-b)/a**2)*(a-(highht-17.3)/(topht-17.3))**2)))**0.5
      return
      end

