       SUBROUTINE MIN(ISC,AGE,FIZ,DBH,HT,SH,TD,HTRUNC,GROSIMP,TVOLIMP)
C----------
C CANADA-BC $Id$
C----------
C
CC  -by Tim Hammond.
CC
CC    This routine and its subroutine LOG were substituted for the 
CC  standard volume calculations in FVS in JUNE of 1990.  This
CC  routine converts species codes and inventory zone codes from
CC  FVS format to Kozak's format.  It determines the maturity
CC  of a tree based on its height, DBH and species, and finally it  
CC  converts the output of Kozak's volume function (LOG) to imperial 
CC  units.
CC   
CC  SPECIES CODES 
CC  Prog-code    FVS       Name             Kozak   Kozak-code
CC      1        WP        White Pine       Pw      7
CC      2        WL        Western Larch    L       10
CC      3        DF        Douglas Fir      F       1
CC      4        GF        Grand Fir        B       4
CC      5        WH        Western Hemlock  H       3
CC      6        C         Cedar            C       2
CC      7        LP        Lodgepole Pine   Pl      8
CC      8        S         Spruce           S       5
CC      9        AF        Subalpine Fir    B       4
CC     10        PP        Ponderosa Pine   Py      9
CC     11        EP        paper birch      Bi     14
CC     12        AT        trembling aspen  A      15
CC     13        AC        cottonwood       CT     11
CC     14        OC        other conifer    F       1
CC     15        OH        other hardwood   Bi     14
C
CC    *** IM represents the maturity of the tree  
CC    *** it is equal to 1 if the tree is immature and 2 otherwise
CC    *** AGE is the age of the tree passed as an argument
C
C
      include 'PRGPRM.F77'
c
c
      include 'CONTRL.F77'
c
c
      include 'METRIC.F77'
c
c
      INTEGER*2 IS, IM, IFZ
      INTEGER   ISC, AGE, FIZ
C 
CC    *** IS represents the Kozak species code
CC    *** ISC is another argument from FVS representing the FVS
CC    *** species code
C
C     ***  DBHMET = diameter outside bark in cm at breast height. 
CC    ***  DBH    = diameter outside bark in inches at breast height.
CC    ***  HTMET  = total height in m.
CC    ***  HT     = total height in feet.
CC    ***  SHMET  = stump height in m.
CC    ***  SH     = stump height in feet.
CC    ***  TD     = top diameter for use in inches.
CC    ***  TDMET  = top diameter for utilization in cm. 
CC    ***  GOL    = log length in m.
CC    ***  HTRUNC = Height to point of top kill in feet
CC    ***  HTM    = Height to point of top kill in metres
CC    ***  HMERC  = Merchantable height in metres    ! not used
CC    ***  HMRIMP = Merchantable height in feet      ! not used
C
      REAL DBH, DBHMET, HTMET, HT, SHMET, SH, TDMET, TD, GOL
      REAL HTRUNC, HTM, HMERC
C
CC    *** volumes and top diameters are in metric
CC    *** VLOG     = volumes for each log (40) 
CC    *** TDL      = top diameters for each log (40)
CC    *** HLL      = length of top log
CC    *** DBT      = butt diameter of first log
CC    *** TVOL     = volume between stump ht and top diameter for use
CC    *** NL       = Number of logs.
CC    *** GROS     = total volume from ground to top
CC    *** GROSIMP  = total volume from ground to top in imperial
CC    *** BAR      = bark thickness (not available in this program )
C
      REAL VLOG(50),TDL(50)
      REAL GROS, GROSIMP, TVOL, TVOLIMP, HLL, DBT, BAR
      INTEGER*2 NL, SPTR(MAXSP)
      LOGICAL DEBUG
      COMMON /VOLMET/ GROS,VLOG,TDL,HLL,DBT,TVOL,BAR,HMERC

      DATA GOL  / 3.0 /
      DATA SPTR /  7,10, 1, 4, 3,
     >             2, 8, 5, 4, 9,
     >            14,15,11, 1,14 /
C
C     TRANSLATE SPECIES CODE TO KOZAK CODE
C
      IS  = SPTR(ISC)

      call dbchk( debug, 'MIN', 3, icyc )
      if (debug) then
          write (jostnd,'('' Entering MIN'')')
          write (jostnd,'('' Parameters:'')')
          write (jostnd,'(t8,''Input Species:'',i10)')   isc
          write (jostnd,'(t8,''Age:          '',i10)')   age
          write (jostnd,'(t8,''FIZ:          '',i10)')   fiz
          write (jostnd,'(t8,''DBH (cm):     '',f10.4)') dbh
          write (jostnd,'(t8,''Height (m):   '',f10.4)') ht
          write (jostnd,'(t8,''Stump Ht (m): '',f10.4)') sh
          write (jostnd,'(t8,''Top Diam (cm):'',f10.4)') td
          write (jostnd,'(t8,''Trunc Ht (m): '',f10.4)') htrunc
      endif
C
CC    Convert the arguments to metric for use in LOG
C
      DBHMET =  DBH    * INtoCM
      HTMET  =  HT     * FTtoM
      SHMET  =  SH     * FTtoM
      TDMET  =  TD     * INtoCM
      HTM    =  HTRUNC * FTtoM
      
      GROSIMP = 0.0
C
CC    Set the maturity code to immature if AGE is less than 121
CC    or less than 81 for lodgepole pine.
C
CC    Note that IM isn't used in any way. Could it be a place-
CC    holder?
C
      IF (ISC .EQ. 7 .AND. AGE .LE. 80) THEN
        IM = 1
      ELSEIF (AGE .GT. 120) THEN
        IM = 2
      ELSEIF (ISC .EQ. 7) THEN
        IM = 2
      ELSE
        IM = 1
      ENDIF

      IFZ = int(FIZ,2)

C     Initialize log length

      if (debug) write (jostnd,1000) IS,IM,FIZ,DBHMET,HTMET,SHMET,
     >                               TDMET,GOL,HTM
1000  FORMAT (' Calling LOG with:',
     >        /T5,'IS     = ',I10,  '  MATURE  = ',I10,
     >        /T5,'FIZ    = ',I10,  '  DBHMET  = ',F10.5,
     >        /T5,'HT     = ',F10.5,'  STUMPHT = ',F10.5,
     >        /T5,'TDMET  = ',F10.5,'  LOGLEN  = ',F10.5,
     >        /T5,'HTRUNC = ',F10.5)
      CALL LOG(IS,IFZ,IM,DBHMET,HTMET,SHMET,TDMET,GOL,NL,
     >         VLOG,TDL,HLL,DBT,TVOL,GROS,BAR,HTM)
      if (debug) write (jostnd,'('' Called LOG...'')' )
      GROSIMP = GROS * M3toFT3
      TVOLIMP = TVOL * M3toFT3
c     HMRIMP  = HMERC * MtoFT
      if (debug) write (jostnd,'('' Leaving MIN'')' )
      RETURN
      END
