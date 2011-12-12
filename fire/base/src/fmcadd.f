      SUBROUTINE FMCADD
      IMPLICIT NONE
C----------
C  **FMCADD  FIRE--DATE OF LAST REVISION:  09/29/09
C----------      
C     CALLED FROM: FMMAIN
C     CALLS        
*  Purpose:
*     This subroutine calculates annual litterfall and crown breakage
*     from each tree, and adds the material to the appropriate debris
*     pools.  It also adds in dead material from crown lifting and the
*     debris-in-waiting from previously killed snags (i.e. CWD2B),
*     including a proportion (=CWDCUT) of debris slated to fall in
*     future years that has been brought down early by a salvage
*     operation.  CWD2B may include some dead material from
*     live-but-scorched trees. Crown foliage and woody materials are not
*     reduced for the losses calculated here: the material is assumed to
*     be replaced by growth (and is re-calculated to take account of
*     changes in tree size at the start of each cycle).  Data on leaf
*     lifespan come from Keane, Arno & Brown 1989.
*     NOTE:  PP leaf lifespan data was used for species 11 and WP
*     NOTE:  DF leaf lifespan data was used for species WH and Cedar
*----------------------------------------------------------------------
*
*  Local variable definitions:
*     DKCL:    decay class
*     DOWN:    pounds per acre of snag material falling in this year. 
*     PDOWN:   the Proportion of material that has come DOWN in each
*              year class of CWD2B
*     MAXYR:   the number of year classes of CWD2B from which material
*              must be removed.
*
*  Common block variables and parameters:
*
***********************************************************************
      
C.... Parameter statements.

C.... Parameter include files. 
  
      INCLUDE 'PRGPRM.F77'
C      INCLUDE 'PPEPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... Common include files.

      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'

C.... Variable declarations. 

      INTEGER I, SIZE, SP, DKCL, IYR, MAXYR
      REAL    DOWN, PDOWN    
      LOGICAL DEBUG	
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMCADD',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
  7   FORMAT(' ENTERING FMCADD CYCLE = ',I2)	

C.... Begin routine.

C     Go through the tree list, calculating debris inputs from each tree.

      DO I=1,ITRN

         IF (FMPROB(I).GT.0.) THEN

            SP = ISP(I)
            DKCL = DKRCLS(SP)
         
C           Litterfall from foliage:              .0005 = 1/2000.

            CWD(1,10,2,DKCL) = CWD(1,10,2,DKCL)
     +        + (CROWNW(I,0) * FMPROB(I)/LEAFLF(SP)) * .0005 
            CWDNEW(1,10) = CWDNEW(1,10) 
     +        + (CROWNW(I,0) * FMPROB(I)/LEAFLF(SP)) * .0005   

            DO SIZE=1,5 
        
C           Random breakage of each woody crown component:
        
               CWD(1,SIZE,2,DKCL) = CWD(1,SIZE,2,DKCL) 
     &           + (LIMBRK * FMPROB(I) * CROWNW(I,SIZE)) * .0005 
               CWDNEW(1,SIZE) = CWDNEW(1,SIZE) 
     &           + (LIMBRK * FMPROB(I) * CROWNW(I,SIZE)) * .0005 
            
C           Dead material from crown lifting (NOTE:  foliage isn't being
C           included here because it's assumed that the leaf-lifespan
C           data used above already incorporates the death of some foliage 
C           due to crown lifting):

               CWD(1,SIZE,2,DKCL) = CWD(1,SIZE,2,DKCL) 
     &           + (FMPROB(I) * OLDCRW(I,SIZE)) * .0005 
               CWDNEW(1,SIZE) = CWDNEW(1,SIZE) 
     &           + (FMPROB(I) * OLDCRW(I,SIZE)) * .0005 

            ENDDO
         ENDIF
      ENDDO
  
  
C     Add in the debris-in-waiting from snags.  In the absence of a 
C     salvage operation this year (CWDCUT=0), it is enough to take 
C     all the material in the year-1 pool of CWD2B.  If there has been
C     a salvage, then also remove a proportion of material from every 
C     other year pool.  NOTE:  this is a slight mis-usage of CWDCUT,
C     because CWD2B also contains dead crown material from live-but-
C     burned trees (not much per tree, or the tree would have died).

      IF (CWDCUT .LE. 0.0) THEN
         MAXYR = 1
      ELSE
         MAXYR = TFMAX
      ENDIF
      
      DO IYR=1,MAXYR
         
        IF (IYR .EQ. 1) THEN
          PDOWN = 1.0
        ELSE
          PDOWN = CWDCUT
        END IF
        
C       Repeat for each decay class:        
      
        DO DKCL=1,4 
      
C          First add the litterfall to down debris.
           
           DOWN = PDOWN * CWD2B(DKCL,0,IYR)
           CWD(1,10,2,DKCL) = CWD(1,10,2,DKCL) + DOWN / 2000.0 
           CWDNEW(1,10) = CWDNEW(1,10) + DOWN / 2000.0
           CWD2B(DKCL,0,IYR) = CWD2B(DKCL,0,IYR) - DOWN
           
C          Then all the sizes of woody material.
        
           DO SIZE=1,5
              DOWN = PDOWN * CWD2B(DKCL,SIZE,IYR)
              CWD(1,SIZE,2,DKCL) = CWD(1,SIZE,2,DKCL) + DOWN / 2000.0
              CWDNEW(1,SIZE) = CWDNEW(1,SIZE) + DOWN / 2000.0
              CWD2B(DKCL,SIZE,IYR) = CWD2B(DKCL,SIZE,IYR) - DOWN
              
           ENDDO
        ENDDO
      ENDDO
        
C     Move all the debris-in-waiting pools of each decay class forward
C     one year, to be ready for next year:

      DO DKCL=1,4 
         DO SIZE=0,5
            DO IYR=1,(TFMAX-1)
               CWD2B(DKCL,SIZE,IYR) = CWD2B(DKCL,SIZE,IYR+1)
            ENDDO
            CWD2B(DKCL,SIZE,TFMAX) = 0.0
         ENDDO
      ENDDO

      IF (DEBUG) THEN
         DO DKCL=1,4 
            DO SIZE=0,5
               DO IYR=1,(TFMAX-1)
                  WRITE (JOSTND,10)DKCL,SIZE,IYR,
     >                 CWD2B(DKCL,SIZE,IYR),CWD2B2(DKCL,SIZE,IYR)
 10               FORMAT(' FMCADD: DKCL,SIZE,IYR=',3I3,' CWD2B=',F12.2,
     >                 ' CWD2B2=',F12.2)
               ENDDO
            ENDDO
         ENDDO
      ENDIF
     
      RETURN
      END

