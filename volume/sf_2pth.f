!== last modified  10-4-2002
      SUBROUTINE SF_2PTH(JSP,GEOSUB,SETOPT,DBH,H1,d1,DBTBH,TotalH,
     >                   HEX,ZEX,F,RHFW,RFLW,TAPCOE)
c                                                               SF_2PTH
c                                  separate entry:              SF_2PTHA
c                                  separate entry:              SF_2PTHB

c==klc  June 5, 2000
c     Stem form   Specify the  DBH and some  (height, diameter),
c                 but total height is unknown, and is to be found.     
c                 SP_2PTHA is identical, but has fewer input arguments.
c                 SP_2PTHB same as SP_2PTHB, but SF_2PT is
c                          automatically called by this routine. (Use
c                          if there is one and only one upper diameter meas.)
c
c     DBH_arg   input   R*4    DBH
c     HT_arg    input   R*4    Total tree height
c     ASP       input   A2      Species (and coefficient-set) code.
c                                 HI   (ITT Western Hemlock)
c                                 DI   (ITT Douglas Fir - proprietary)
c                                 DF   1994 Douglas Fir
c                                 WH   1994 Western Hemlock
c                                 RC   1994 Red Cedar   

c     IERR      output  I       0  No Error
c                               1  Unrecognized species code
c                               2  Invalid DBH
c                               3  Invalid height
c                               4  This species not available in metric.
c                               5  Supplemental arguments can not be used
c                                  because regression coef file not
c                                  available for this species.
c                               6  Region code not recognized.
c                               7  H1, D1 are a bad combination.
c                               8  Fail to find a solution.

c     DBT_ARG     input    R*4   Double Bark Thickness at B.H.  (0: unknown)
c     REGION_ARG  input    A2    Region code (blank: unspecified)
c     SUPV_ARG    input    R*4(10) Supplemental arguments
c                                 See documentation. DO NOT PROVIDE all NSUP.
c                                 A value of zero ALWAYS implies missing.
c                                 (1)  Breast height Age
c                                 (2)  Crown Ratio (Fraction of TOTAL HT w crwn) 
c                                 (3)  Site index (in feet, base 50 yrs BH)
c                                 (4)  Stand density index (SDI)
c     H1_ARG       input   R*4    Height with a known diameter
c     D1_ARG       input   R*4    Diameter at H1_ARG
c     IDTYPE       input   I      1 if D1_ARG is o.b., 2 if inside bark
c     HT           output  R*4    Total tree height (precision about 0.1 ft)
c
c     General note:  This entire procedure is based on a FAULTY statistical
c                    assumption: that total height (an input variable) in
c                    regression can now be treated as an output variable.
c                    There is nothing in regression theory that suggests
c                    this is a good idea.
c                    It is IMPERATIVE that the user check for reasonableness.
c                    Depending on cruising methods, TOTAL height should 
c                    never exceed F0  + F1* H1_ARG where  (F0, F1) is
c                    about (10 feet, 1.25). 

      REAL DBH,TOTALH,DBTBH,h1,d1
      REAL htry,eval,HTLOW,HTHI,EVLOW,EVHI
C      REAL*4  SUPV_arg(4)
      REAL*4 HEX(2), ZEX(2),BH,HT,DEL_H,SF_2PTH1,AER,TOL
      REAL*4 RHFW(4),RFLW(6),TAPCOE(12),F 
      INTEGER JSP, SETOPT(6),IDTYPE,FLAG,ITRY,IERROR
C      LOGICAL METRIC
c     character*1 unit
      Character*2 GEOSUB
      
c              idtype = 2 for inside bark top diameter
       IDTYPE= 2
       BH=4.5

      if (h1 .le. 1.33*BH  .or.  d1.ge. dbh) then
c            IERR=7
            HT=0.0
            RETURN
      ENDIF
c
c       find a suitable DEL_H as a search unit.
c
      DEL_H = h1 * d1/(dbh -d1)
      if (del_H  .gt. 0.25*h1)  del_h= 0.25*h1
      if (del_H  .lt. 0.05*h1)  del_h= 0.05*h1
c
c      Find a suitable upper bound on total ht
c
      HTLOW=H1
      EVLOW = D1 * (-1)
      flag = 0
      HTRY = H1+ DEL_H
      DO 100 ITRY=1,25
        EVAL = SF_2PTH1(JSP,GEOSUB,SETOPT,DBH,HTRY,DBTBH,HEX,ZEX,F,RHFW,
     >                                          RFLW,TAPCOE,h1,d1)
        IF (EVAL.gt. 0.0 .OR. FLAG .EQ.1) THEN
c          have bracketed a viable solution. Now find it.
C         check to see if eval is within tolerance
          IF(abs(eval) .lt. 0.01) then
C                we are done
            TOTALH = HTRY
            CALL SF_2PT(JSP,GEOSUB,SETOPT,DBH,TOTALH,DBTBH,F,RHFW,
     >                                             RFLW,TAPCOE)      
            GOTO 120
          ELSE
            flag = 1
            IF(EVAL < 0) THEN
               EVLOW = EVAL
               HTLOW = HTRY
            ELSE
               EVHI = EVAL
               HTHI = HTRY
            ENDIF
            AER = ABS(EVLOW)
C            using ratios, find the next height to try
            HTRY = (AER/(AER+EVHI))*(HTHI-HTLOW) + HTLOW
          ENDIF
        ELSE
           HTLOW=HTRY
           EVLOW = EVAL
           HTRY=HTRY+ DEL_H
        ENDIF
      
 100  CONTINUE
 120  CONTINUE
        TOL = 0.1
        IERROR=0
C         TOTALH = ZBRENT(EVAL, HLAST, HTRY, TOL, izerror)
c        find total height using ratio of htry, hlast
C        TOTALH = HTRATIO(EVAL,EVLAST,HTRY,HLAST,TOL,IZERROR) 

C        IERR=IERROR
C        IF (IZERROR.ne. 0 ) IERR=IZERROR
c                                           Have solution.
c               If sf_2PTHB was called, then call 2pt .
c               Otherwise the user is responsible for making this call.      
c                  We could call SF-3PT, but we won't   
c         NP=1
c         HV(1)= H1_arg
c         DV(2) = D1_ARG
c         CALL SF_3PT(NP, HV, DV, IDTYPE_A, ZV)
C         ENDIF
C      endif
      RETURN
      END


c**********************************************************************
c**********************************************************************
      FUNCTION SF_2PTH1(JSP,GEOSUB,SETOPT,DBH,HTRY,DBTBH,HEX,ZEX,F,RHFW,
     >                                            RFLW,TAPCOE,h1,d1)
c**********************************************************************
c                                                         SF_2PTH1
c      HTRY      IN   R*4    A trial value for total ht.
c      SF_2PTH1  OUT  R*4    Predicted dib at h1 minus measured diameter there
  

      REAL SF_2PTH1
      REAL DBH,HTRY,DBTBH,h1,d1,d2,SLOPE
c      REAL*4  SUPV_arg(4)
      REAL*4 HEX(2), ZEX(2),DOB,DBT,DIFF
      REAL*4 RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
      INTEGER JSP, SETOPT(6),NP
      INTEGER ineedsl,IDTYPE
C      LOGICAL METRIC
c     character*1 unit
      Character*2 GEOSUB
c                                develop taper assuming HT=HTRY      
      CALL SF_2PT(JSP,GEOSUB,SETOPT,DBH,HTRY,DBTBH,F,RHFW,
     >                                             RFLW,TAPCOE)      

C      IF(IERR.gt.0) then
C          SF_2PTH1=0.
C          IERROR=IERR
C          RETURN
C        ENDIF               

c                                find predicted diameter at H1
      INEEDSL = 0

      CALL SF_DS(JSP,geosub,NP,SETOPT,ineedsl,slope,DBH,HTRY,H1,HEX,ZEX,
     >                               RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,D2)

      IF(JSP.GE.22 .AND. JSP.LE.30) THEN
        CALL BRK_UP(JSP,geosub,DBH,HTRY,DBTBH,H1,D2,DOB,DBT)
      ENDIF
c
C                                  calculate the error.
          IDTYPE = 2
      IF (IDTYPE.eq.2) then
c                                  specified diameter is inside bark       
          DIFF= D2 - D1
      else 
c                                  specified diameter is o.b.         
          DIFF = DOB - D1
      ENDIF
      SF_2PTH1 = DIFF
      RETURN
      END 
