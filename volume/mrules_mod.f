      MODULE MRULES_MOD

!     Module to store merch rules

!     Created TDH 06/17/09
!     Revised ... DD/MM/YY 

!     Class for passing/receiving merch rules in calls from C++
      TYPE MERCHRULES
      SEQUENCE
        INTEGER        EVOD
        INTEGER        OPT     
        
        
        REAL           MAXLEN
        REAL           MINLEN
        REAL           MINLENT
        REAL           MERCHL
        REAL           MTOPP
        REAL           MTOPS
        
        REAL           STUMP
        REAL           TRIM
        REAL           BTR
        REAL           DBTBH
        REAL           MINBFD
        
        CHARACTER*1    COR 
        
      END TYPE MERCHRULES
   

      END MODULE MRULES_MOD