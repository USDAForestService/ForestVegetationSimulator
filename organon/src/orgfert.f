The following code segment was inserted into base/src/ffert.f by Jeff. It doesn't belong there, but it illustrates
his thinking on what needed to be done. I think it needs to be thought out a little more.
In the initial overhaul of what Jeff did, I'm leaving this out of the OC and OP variants, for the following reasons:
1) In FVS, subroutine FFERT is called from subroutine GRINCR after growth and mortality have been calculated. The
   fertilizer effect is accounted for by looping through the tree list and boosting the diameter and height growth
   estimates. Logic has been inserted to figure out the number of years the fertilizer effect is applied and so forth.
   Since this effect is applied after mortality has been applied, the increased growth rates do not affect mortality. This
   might be wrong and is something FMSC should think about and perhaps the call to FFERT needs to be moved before the
   call to MORTS.
2) In the FVS-Organon variants, the PN array needs to be loaded before the call to subroutine EXECUTE which occurs in
   subroutine DGDRIV. Right now the only way people can trigger a fertilizer effect in the Organon equations is by using
   the FERTILIZ keyword (PN values are not contained in the .INP file, and there isn't an FVS-Organon keyword for them).
   So given that, they just as well fake a fertilizer effect using the FVS logic.

When its time to include a fertilizer effect, then the way to do this might be
1) move the retrieval of the FERTILIZ keyword out of FFERT and into GRINCR, then the parameters can be passed to EXECUTE or FFERT.
2) FVS nitrogen is currently fixed at 200 pounds per acre; it can go up to 400 pounds per acre in Organon. So accound for this in
   the keyword processing.
3) Effect needs to be different depending on IORG(i) values. If it is a valid Organon tree then let the Organon effect be
   implemented. Only apply the FVS logic to non-valid Organon trees. This may necessitate some restructuring of the subroutines
   involved.
4) The Organon effect only gets applied if: a) the stand has at least 80 percent of its basal area in Douglas-fir, b) the stand is 
   even-aged, and c) the stand age is less than 70 years.
   
   
C
C
      INCLUDE 'ORGANON.F77'
C
C
COMMONS
C
      .
      .
      .
C-----------
C  MANAGE THE ORGANON FERTILIZATION VARIABLES. IF FERTILIZATION HAS
C  OCCURRED, SET THE "STAND HAS BEEN FERTILIZED" VARIABLE TO TRUE
C-----------
      IF(LORGANON)THEN
        IF( FFPRMS(1) .GT. 0.0 ) THEN
          DO I=5,2,-1
            YSF(I)  = YSF(I-1)
            PN(I)   = PN(I-1)
          END DO
          YSF(1)  = FLOAT( ( CYCLG - 1 ) * 5 )
          PN(1)   = FFPRMS(1)
          INDS(8)		= 1
        END IF
C----------         
C  WRITE OUT THE VARIABLES TO THE DEBUG/OUTPUT FILE FOR REVIEW LATER.
C----------        
        IF( INDS(8) .EQ. 1 ) THEN
          IF (DEBUG) THEN
            WRITE(JOSTND,123) ICYC, PN(1)
 123        FORMAT(' STAND HAS BEEN FERTILIZED ',
     &           ' ORGANON.DLL, CYCLE=',I2, ' PN= ', F9.3 )
            WRITE(JOSTND,124) ICYC,
     &           YSF(1), 
     &           YSF(2), 
     &           YSF(3), 
     &           YSF(4), 
     &           YSF(5) 
 124        FORMAT(
     &           ' ORGANON.DLL, CYCLE=',I2, 
     &           ', YSF(1)=', F6.2,
     &           ', YSF(2)=', F6.2,
     &           ', YSF(3)=', F6.2,
     &           ', YSF(4)=', F6.2,
     &           ', YSF(5)=', F6.2 )
            WRITE(JOSTND,125) ICYC,
     &           PN(1), 
     &           PN(2), 
     &           PN(3), 
     &           PN(4), 
     &           PN(5) 
 125        FORMAT(
     &           ' ORGANON.DLL, CYCLE=',I2, 
     &           ', PN(1)=', F6.2,
     &           ', PN(2)=', F6.2,
     &           ', PN(3)=', F6.2,
     &           ', PN(4)=', F6.2,
     &           ', PN(5)=', F6.2 )
          END IF
        END IF
        GO TO 100
      ENDIF
