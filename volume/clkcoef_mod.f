      MODULE CLKCOEF_MOD

!     Module to store clark equation coefficients

!     Created TDH 11/23/09
!     Revised ... ../../.. 
!     YW 06/03/14 added Q, FIXDI, DX for region 8 Clark equation
      TYPE CLKCOEF
      SEQUENCE
        REAL DBHIB,DIB17,R,C,E,P,B,A,A4,B4,A17,B17,TOTHT,Q,FIXDI,DX
      END TYPE CLKCOEF
   
   
      END MODULE CLKCOEF_MOD