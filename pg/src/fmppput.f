      SUBROUTINE FMPPPUT (WK3, IPNT, ILIMIT)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C  Purpose:
C     Put (write) the Fire Model data for a given stand to DA file.
C
C     This is part of the Parallel Processing Extension.
C----------------------------------------------------------------------
C
C  Call list definitions:
C     ILIMIT: (I)  Size of buffer WK3.
C     IPNT:   (IO) Pointer to curent element in print buffer WK3.
C     WK3:    (IO) Work array used as a buffer.
C
C     INTS:   Array of length MXI to hold integer values.
C     LOGICS: Array of length MXL to hold logical values.
C     REALS:  Array of length XMR to hold real values.
C
C**********************************************************************

C     Parameter statements.

      INTEGER MXL,MXR,MXI
      PARAMETER (MXL=14,MXR=54,MXI=92)

C     Parameter and other include files that are not stored here

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'SVDATA.F77'

C     Include files that are stored here

      INCLUDE 'FMPARM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMSVCM.F77'

      LOGICAL LOGICS(MXL)
      INTEGER INTS(MXI), I, ILIMIT, IPNT, NSNAGZ
      REAL    REALS(MXR), WK3(*)


      INTS(  1) = IFMTYP
      INTS(  2) = ACTCBH
      INTS(  3) = ATEMP
      INTS(  4) = BURNYR
      INTS(  5) = COVTYP
      INTS(  6) = FIRTYPE
      INTS(  7) = FMKOD
      INTS(  8) = FTREAT
      INTS(  9) = HARTYP
      INTS( 10) = HARVYR
      INTS( 11) = IBRPAS
      INTS( 12) = IDBRN
      INTS( 13) = IDFLAL
      INTS( 14) = IDFUL
      INTS( 15) = IDMRT
      INTS( 16) = IDPFLM
      INTS( 17) = IDRYB
      INTS( 18) = IDRYE
      INTS( 19) = IFAPAS
      INTS( 20) = IFLALB
      INTS( 21) = IFLALE
      INTS( 22) = IFLPAS
      INTS( 23) = IFMBRB
      INTS( 24) = IFMBRE
      INTS( 25) = IFMFLB
      INTS( 26) = IFMFLE
      INTS( 27) = IFMMRB
      INTS( 28) = IFMMRE
      INTS( 29) = IFMYR1
      INTS( 30) = IFMYR2
      INTS( 31) = IFSTEP
      INTS( 32) = IFTYR
      INTS( 33) = IMRPAS
      INTS( 34) = IPFLMB
      INTS( 35) = IPFLME
      INTS( 36) = IPFPAS
      INTS( 37) = IPFSTP
      INTS( 38) = ISALVC
      INTS( 39) = ISALVS
      INTS( 40) = ISNAGB
      INTS( 41) = ISNAGE
      INTS( 42) = ISNGSM
      INTS( 43) = ISNSTP
      INTS( 44) = JCOUT
      INTS( 45) = JSNOUT
      INTS( 46) = ND
      INTS( 47) = NFMODS
      INTS( 48) = NFMSVPX
      INTS( 49) = NL
      INTS( 50) = NSNAG
      INTS( 51) = OLDCOVTYP
      INTS( 52) = OLDICT
      INTS( 53) = OLDICT2
      INTS( 54) = PBURNYR
      INTS( 55) = FM89YR
      INTS( 56) = ICBHMT
      INTS( 57) = ICANSP
      INTS( 58) = BURNSEAS
      INTS( 59) = IDSHEAT
      INTS( 60) = ISHEATB
      INTS( 61) = ISHEATE
      INTS( 62) = SOILTP
      INTS( 63) = ICFPB
      INTS( 64) = ICFPE
      INTS( 65) = ICFPST
      INTS( 66) = NSNAGSALV
      INTS( 67) = NYRS
C------- Carbon reporting INTEGER variables --------
      INTS( 68) = ICHABT
      INTS( 69) = ICHPAS
      INTS( 70) = ICHRVB
      INTS( 71) = ICHRVE
      INTS( 72) = ICHRVI
      INTS( 73) = ICMETRC
      INTS( 74) = ICMETH
      INTS( 75) = ICRPAS
      INTS( 76) = ICRPTB
      INTS( 77) = ICRPTE
      INTS( 78) = ICRPTI
      INTS( 79) = IDCHRV
      INTS( 80) = IDCRPT
C------- new FFE INTEGER variables --------
      INTS( 81) = IFLOGIC     
      INTS( 82) = IFMSET
C------- new FFE INTEGER variables for down wood reports --------      
      INTS( 83) = IDWPAS
      INTS( 84) = IDWRPB
      INTS( 85) = IDWRPE
      INTS( 86) = IDWRPI
      INTS( 87) = IDCPAS
      INTS( 88) = IDWCVB
      INTS( 89) = IDWCVE
      INTS( 90) = IDWCVI
      INTS( 91) = IDDWRP
      INTS( 92) = IDDWCV      
         
      CALL IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)


      NSNAGZ = MAX(NSNAG,1)
      CALL IFWRIT (WK3, IPNT, ILIMIT, DKRCLS, MAXSP       , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, FLAG, 3             , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, FMDUSR, 4           , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, FMOD, MXFMOD        , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, GROW,  ITRN         , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, IOBJTPTMP, NSVOBJ   , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, IS2FTMP,   NSVOBJ   , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, JFROUT, 3           , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, JLOUT,  3           , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, MPS,    8           , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, PLSIZ,  2           , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, POTSEAS, 2          , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, POTTYP, 2           , 2)      
      CALL IFWRIT (WK3, IPNT, ILIMIT, SPS,    NSNAGZ      , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, SPSSALV, NSNAGZ     , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, SURFVL, MXDFMD*2*4  , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, YRDEAD, NSNAGZ      , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, FMICR, MAXTRE       , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, IFUELMON, MXDFMD    , 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, ISPCC, MAXTRE       , 2)
      
      LOGICS ( 1) = LANHED
      LOGICS ( 2) = LATFUEL
      LOGICS ( 3) = LDHEAD
      LOGICS ( 4) = LDYNFM
      LOGICS ( 5) = LFLBRN
      LOGICS ( 6) = LFMON
      LOGICS ( 7) = LFMON2
      LOGICS ( 8) = LHEAD
      LOGICS ( 9) = LREMT
      LOGICS (10) = LSHEAD
      LOGICS (11) = LUSRFM
      LOGICS (12) = LATSHRB
      LOGICS (13) = LVWEST
      LOGICS (14) = LPRV89
      CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS,   MXL, 2)

      CALL LFWRIT (WK3, IPNT, ILIMIT, HARD, NSNAGZ,     2)
      CALL LFWRIT (WK3, IPNT, ILIMIT, HARDSALV, NSNAGZ, 2)
      CALL LFWRIT (WK3, IPNT, ILIMIT, LFROUT, 3,        2)
      CALL LFWRIT (WK3, IPNT, ILIMIT, LSW, MAXSP,       2)
      
      REALS (  1) = BURNCR
      REALS (  2) = CBD
      REALS (  3) = CRBURN
      REALS (  4) = CWDCUT
      REALS (  5) = DEPTH
      REALS (  6) = DPMOD
      REALS (  7) = EXPOSR
      REALS (  8) = FLAMEHT
      REALS (  9) = FLPART
      REALS ( 10) = FMSLOP
      REALS ( 11) = FWIND
      REALS ( 12) = HTR1
      REALS ( 13) = HTR2
      REALS ( 14) = HTXSFT
      REALS ( 15) = LARGE
      REALS ( 16) = LIMBRK
      REALS ( 17) = MINSOL
      REALS ( 18) = NZERO
      REALS ( 19) = OLARGE
      REALS ( 20) = OSMALL
      REALS ( 21) = PBRNCR
      REALS ( 22) = PBSCOR
      REALS ( 23) = PBSIZE
      REALS ( 24) = PBSMAL
      REALS ( 25) = PBSOFT
      REALS ( 26) = PBTIME
      REALS ( 27) = PERCOV
      REALS ( 28) = PRSNAG
      REALS ( 29) = RFINAL
      REALS ( 30) = SCCF
      REALS ( 31) = SCH
      REALS ( 32) = SLCHNG
      REALS ( 33) = SLCRIT
      REALS ( 34) = SMALL
      REALS ( 35) = TCLOAD
      REALS ( 36) = TONRMC
      REALS ( 37) = TONRMH
      REALS ( 38) = TONRMS
      REALS ( 39) = TOTACR
      REALS ( 40) = CCCHNG
      REALS ( 41) = CCCRIT
      REALS ( 42) = PRV8
      REALS ( 43) = PRV9
      REALS ( 44) = CANMHT
      REALS ( 45) = CBHCUT
C------- Carbon reporting REAL variables --------
      REALS ( 46) = CRDCAY
      REALS ( 47) = BIOLIVE
      REALS ( 48) = BIOSNAG
      REALS ( 49) = BIODDW
      REALS ( 50) = BIOFLR
      REALS ( 51) = BIOSHRB
      REALS ( 52) = BIOROOT
C-------new FFE REAL variables --------
      REALS ( 53) = ULHV
      REALS ( 54) = FOLMC

      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)

      CALL BFWRIT (WK3, IPNT, ILIMIT, ALLDWN,       MAXSP  , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CANCLS,        4     , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CATCHUP,      NFLPTS , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CORFAC,        4     , 2)
      DO I=0,5
         CALL BFWRIT (WK3, IPNT, ILIMIT, CROWNW(1,I), ITRN , 2)
         CALL BFWRIT (WK3, IPNT, ILIMIT, OLDCRW(1,I), ITRN , 2)
      ENDDO
      CALL BFWRIT (WK3, IPNT, ILIMIT, CURKIL, ITRN         , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CWD, 3*MXFLCL*2*5    , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CWD2B,   4*6*TFMAX   , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CWD2B2,  4*6*TFMAX   , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CWDNEW,    2*MXFLCL  , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, DBHS,    NSNAGZ      , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, DBHSSALV, NSNAGZ     , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, DECAYX,  MAXSP       , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, DEND ,   NSNAGZ      , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, DENIH,   NSNAGZ      , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, DENIS,   NSNAGZ      , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, DKR,   MXFLCL*4      , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, DKRDEF, 4            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, DSPDBH, MAXSP*19     , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FALLX, MAXSP         , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FIRACR, 2            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FLIVE, 2             , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FMACRE, 14           , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FMDEP, MXDFMD        , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FMLOAD, MXDFMD*2*7   , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FMTBA, MAXSP         , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FMY1,  NFLPTS        , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FMY2,  NFLPTS        , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FUAREA, 5*4          , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FWG, 2*7             , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FWT, MXFMOD          , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FWTUSR, 4            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, HTDEAD, NSNAGZ       , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, HTDEADSALV, NSNAGZ   , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, HTIH,   NSNAGZ       , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, HTIHSALV, NSNAGZ     , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, HTIS,   NSNAGZ       , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, HTISSALV, NSNAGZ     , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, HTX,  MAXSP*4        , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, LEAFLF, MAXSP        , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, LOWDBH, 7            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, MAXHT, MAXSP*19      , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, MEXT, 3              , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, MINHT, MAXSP*19      , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, MOIS, 2*5            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, MOISEX, MXDFMD       , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, OFFSET, NFLPTS       , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, OLDCRL, ITRN         , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, OLDHT,  ITRN         , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PBFRIH, NSNAGZ       , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PBFRIS, NSNAGZ       , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PFLACR, 4*3          , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PFLAM, 4             , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, POTEMP, 2            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, POTFSR, 4            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, POTKIL, 4            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, POTPAB, 2            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, POTRINT, 2           , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, POTVOL, 2            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PRDUFF, MXFLCL*4     , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PRESVL, 2*8          , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PREWND, 2            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PRPILE, MXFLCL       , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PSOFT, MAXSP         , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SALVSPA, NSNAGZ*2    , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SCBE, 3              , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SFRATE, 3            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SIRXI,  3            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SMOKE,  2            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SNGNEW, NSNAGZ       , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SNPRCL, 6            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SPHIS, 3             , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SRHOBQ, 3            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SSIGMA, 3            , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, SXIR, 3              , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, TCWD,  6             , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, TCWD2, 6             , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, TFALL,   MAXSP*6     , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, TODUFF,  MXFLCL*4    , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, V2T,     MAXSP       , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, BURNED,  3*MXFLCL    , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, BURNLV,  2           , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FIRKIL,  MAXTRE      , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FMPROB,  MAXTRE      , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, OLDICTWT,2           , 2)
C------- Carbon reporting REAL variables --------
      CALL BFWRIT (WK3, IPNT, ILIMIT, CDBRK,   2           , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, BIOCON,  2           , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, BIOREM,  2           , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, FATE,    2*2*MAXCYC  , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CARBVAL, 17          , 2)
C------- new FFE REAL variables --------
      CALL BFWRIT (WK3, IPNT, ILIMIT, USAV,    3           , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, UBD,     2           , 2)            
      CALL BFWRIT (WK3, IPNT, ILIMIT, CWDVOL, 3*10*2*5     , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CWDCOV, 3*10*2*5     , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PREMST,  MAXTRE      , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, PREMCR,  MAXTRE      , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, DBHC,  MAXTRE        , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, HTC,  MAXTRE         , 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, CROWNWC,  MAXTRE*6   , 2)            
      CALL BFWRIT (WK3, IPNT, ILIMIT, SETDECAY, MXFLCL*4   , 2)

      RETURN
      END
