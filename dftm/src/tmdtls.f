      SUBROUTINE TMDTLS(JO)   
      IMPLICIT NONE
C---------- 
C  **TMDTLS DATE OF LAST REVISION:  06/30/10
C---------- 
C     **** This listing of revision dates has NEVER been maintained ****
C
C     TMDTLS WRITES A LISTING OF THE DATE OF THE LAST REVISION    
C     FOR EACH SUBROUTINE.  IF YOU CHANGE A SUBROUTINE YOU MUST   
C     UPDATE THIS LISTING AND THE COMMENT IN LINE 3 OF THE ROUTINE.     
C     FAILURE TO DO SO WILL CAUSE VIOLENT REACTIONS RESULTING     
C     IN PAIN AND SUFFERING.  
C     
      INTEGER JO     

      WRITE(JO,600)     
  600 FORMAT(/' DATE OF LAST REVISION: TUSSOCK MOTH MODEL SUB-',  
     & 'PROGRAMS AND COMMON AREAS'//1X,   
     & ' COM: BIOMAS      00:00:00 12/29/88  '/1X,    
     & ' COM: DFOL        00:00:00 12/29/88  '/1X,    
     & ' COM: GPASS       00:00:00 12/29/88  '/1X,    
     & ' COM: ICOND       00:00:00 12/29/88  '/1X,    
     & ' COM: LIMITS      00:00:00 12/29/88  '/1X,    
     & ' COM: LOWER       00:00:00 12/29/88  '/1X,    
     & ' COM: TMCOM1      00:00:00 12/29/88  '/1X,    
     & ' COM: TMEGGS      00:00:00 12/29/88  '/1X,    
     & ' COM: UPPER       00:00:00 12/29/88  ') 

      WRITE(JO,604)     
  604 FORMAT(1X,  
     & ' SUB: DFOLE8      00:00:00 12/29/88  '/1X,    
     & ' SUB: DFTMGO      00:00:00 12/29/88  '/1X,    
     & ' SUB: DFTMIN      00:00:00 12/29/88  '/1X,    
     & ' SUB: DFTMOD      00:00:00 12/29/88  '/1X,    
     & ' SUB: GARBEL      14:04:00 11/13/81  '/1X,    
     & ' SUB: GFCOMP      00:00:00 12/29/88  '/1X,    
     & ' SUB: GRCLAS      14:04:00 11/13/81  '/1X,    
     & ' SUB: GRPSUM      14:04:00 11/13/81  '/1X,    
     & ' SUB: G0COMP      00:00:00 12/29/88  '/1X,    
     & ' SUB: INSCYC      00:00:00 12/29/88  ') 

      WRITE (JO,601)    
  601 FORMAT(1X,  
     & ' SUB: IQRSRT      14:04:00 11/13/81  '/1X,    
     & ' SUB: REDIST      00:00:00 12/29/88  '/1X,    
     & ' SUB: TMBCHL      14:04:00 11/13/81  '/1X,    
     & ' SUB: TMBMAS      00:00:00 12/29/88  '/1X,    
     & ' SUB: TMCOUP      00:00:00 12/29/88  '/1X,    
     & ' SUB: TMDTLS      00:00:00 12/29/88  '/1X,    
     & ' SUB: TMHED       14:04:00 11/13/81  '/1X,    
     & ' SUB: TMINIT      00:00:00 12/29/88  '/1X,    
     & ' SUB: TMOTPR      00:00:00 12/29/88  '/1X,    
     & ' SUB: TMOUT       00:00:00 12/29/88  '/1X,    
     & ' SUB: TMRANN      14:04:00 11/13/81  ') 

      WRITE(JO,602)     
  602 FORMAT(1X,  
     & ' SUB: TMSCHD      00:00:00 12/29/88  '/1X,    
     & ' SUB: UV1         14:04:00 11/13/81  '/1X,    
     & ' SUB: UV2         14:04:00 11/13/81  '/1X,    
     & ' SUB: Y0COMP      00:00:00 12/29/88  '/1X,    
     & ' SUB: Y1COMP      00:00:00 12/29/88  '/1X,    
     & ' SUB: Z1COMP      00:00:00 12/29/88  ') 

      RETURN
      END   
