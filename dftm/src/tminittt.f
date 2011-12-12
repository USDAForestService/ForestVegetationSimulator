      SUBROUTINE TMINIT 
      IMPLICIT NONE
C---------- 
C  **TMINIT--TT DATE OF LAST REVISION:  06/30/10 
C---------- 
C     
C     INITIALIZES THE TUSSOCK MOTH SWITCHES AND VARIABLES   
C     
C     THIS ROUTINE IS CALLED FROM **DFTMIN** ONCE PER STAND 
C
C  REVISION HISTORY:
C    15-APR-2002 Lance R. David (FHTET)
C      Random number generator initialization with default seed value.
C      Added local variables LSET and TMSEED.
C----------
COMMONS     
C     
      INCLUDE 'TMCOM1.F77'
C     
      INCLUDE 'TMEGGS.F77'
C     
      INCLUDE 'UPPER.F77'
C     
      INCLUDE 'LOWER.F77'
C     
COMMONS     
C     
      INTEGER IGFCOD,IDFCOD,ITMREP,JOTMDK,JODFTM,ITMSLV
      INTEGER ITMSCH,IPBMT,ITMETH,IBMTYP,NCLAS,NACLAS
      INTEGER I,IEGTYP,JODFEC,IPRBMT
      REAL B0,CNTGF,CNTDF,DFFBIO,DFPNEW,DFREGG,DFEGG,GFFBIO,GFEGG,
     &     GFPNEW,GFREGG
      REAL PRBSCL,Y1,F1,G1,R0,R1,B1,Z1,X1,X0
      REAL TMB0(66),TMB1(36),TMR0(18),TMSEED,TMPN1,TMASHD,
     &     TMPRB,TMDEFL,TOPO,WEIGHT
      LOGICAL LSET

      DATA TMB0 / 
     >        0.  ,   0.        , 0.    ,  0.   ,   0.   ,   0.   
     >    ,   0.  ,   0.        , 0.    ,  0.   ,   0.001,   0.001
     >    ,  .002 ,   .003      , .006  ,  .013 ,   .035 ,   .028 
     >    ,  .025 ,   .028      , .031  ,  .034 ,   .035 ,   .035 
     >    ,   0.  ,   0.        , 0.    ,  0.   ,   0.   ,   0.   
     >    ,   0.  ,   0.        , 0.    ,  0.   ,   .001 ,   .001 
     >    ,  .001 ,   .002      , .003  ,  .010 ,   .016 ,   .042 
     >    ,  .005 ,   .006      , .007  ,  .021 ,   .033 ,   .056 
     >    ,  .50  ,   .62       , .75   ,  .80  ,   200. ,   200. 
     >    ,  150. ,   150.      , 0.    ,  .50  ,   .60  ,   .85  
     >    ,  .90  ,   .25       , 0.    ,  0.   ,   0.   ,   0.  /

      DATA TMR0 / 
     >       .92  ,   .60       , .07   ,  0.   ,   0.   ,   0.   
     >    ,  .95  ,   .7        , .1    ,  .02  ,   0.   ,   0.   
     >    ,  .02  ,   .02       , .02   ,  .02  ,   .02  ,   .02 /

      DATA TMB1 / 
     >       .02  ,   .02       , .02   ,  .02  ,   .02  ,   .02  
     >    ,  5.4  ,   6.25      , 6.25  ,  2.71 ,   2.27 ,   2.2  
     >    ,  5.4  ,   6.25      , 6.25  ,  3.69 ,   3.29 ,   3.2  
     >    ,  1.19 ,   .081      , .1    ,  .1147,   .0886,   .0625
     >    ,  .197 ,   52.0626838, .02   ,  .01  ,   0.   ,.50, 6*0./    

C
C     THE VARIABLES IDFCOD AND IGFCOD ARE VARIANT DEPENDENT.
C     FOR TETON (TT) VARIANT THEY ARE :
C        IDFCOD - 3  (DF SPECIES CODE)
C        IGFCOD - 9  (GF/WF SPECIES IS NOT REPRESENTED IN THIS VARIANT,
C                     SUBALPINE FIR (AF) IS.)
C
      IDFCOD = 3
      IGFCOD = 9
      LNPV2    = .FALSE.
      LNPV3    = .FALSE.
      LRANSD   = .FALSE.
      LTMPRM   = .FALSE.
      LCHEM    = .FALSE.
      TMDEBU   = .FALSE.
      JODFTM   = 10     
      JODFEC   = 0
      JOTMDK   = 0
      ITMREP   = 2
      TMDTRE   = .FALSE.
      LPUNCH   = .FALSE.
      LDF      = .TRUE. 
      LGF      = .TRUE. 
      LREDIS   = .TRUE. 
      IBMTYP   = 4
      NCLAS(1) = 20     
      NCLAS(2) = 20     
      IPRBMT   = 1
      TMEVNT   = 0.1    
      TMWAIT   = 30     
      ITMSCH   = 1
      ITMETH   = 1
      ITMSLV   = 0
      TMDEFL   = 50.0   
      PRBSCL   = 1.0    
      TOPO     = 1.0    
      TMASHD   = 15.93  
      TMPN1    = 0.5    
      TMPAST   = 1492   
      TMSEED   = 55329.0

C.... Initialized random number generator with default seed value.

      LSET = .TRUE.
      CALL TMRNSD (LSET, TMSEED)
C     
C     THESE EGG DEFAULTS ARE SITE SPECIFIC FROM THE BLUE MOUNTAINS,     
C     NORTHEASTERN OREGON.  THEY MAY BE CHANGED BUT SHOULD NEVER  
C     BE DEFAULTED TO ZERO.   
C     
      IEGTYP   = 1
      DFEGG(1) = 11.0   
      DFEGG(2) = 9.0    
      DFEGG(3) = 7.0    
      GFEGG(1) = 15.0   
      GFEGG(2) = 10.0   
      GFEGG(3) =  7.0   
      DFREGG(1) = 9.0   
      DFREGG(2) = 2.0   
      DFREGG(3) = 0.0   
      GFREGG(1) = 11.0  
      GFREGG(2) = 3.0   
      GFREGG(3) = 0.0   

      DO 5 I=1,40 
        TMWORK(I) = .FALSE.   
        TMYRS(I) = 0    
    5 CONTINUE    

      TMYRS(41) = 0     

      DO 10 I=1,66
        B0(I) = TMB0(I) 
   10 CONTINUE    

      DO 15 I=1,18
        R0(I) = TMR0(I) 
   15 CONTINUE    

      DO 20 I=1,36
        B1(I) = TMB1(I) 
   20 CONTINUE    

      DFPNEW(1) = 26.9  
      DFPNEW(2) = 12.6  
      GFPNEW(1) = 35.2  
      GFPNEW(2) = 7.3   
      DFFBIO(1) = 213.8 
      DFFBIO(2) = 64.2  
      GFFBIO(1) = 227.0 
      GFFBIO(2) = 63.7  
      WEIGHT(1) = 1.0   
      WEIGHT(2) = 1.0   

      RETURN
      END   
