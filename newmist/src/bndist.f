      SUBROUTINE BNDIST(M, V, err, UBound, PDF, End) 
      IMPLICIT NONE
C----------
C  $Id$
C----------
C **BNDIST -- NISI  Date of last revision: April 15 1994 
C--------------------------------------------------------------------
C Purpose:
C   Returns the probability distribution associated with a population
C of a given mean and variance. The distributions are taken from the 
C Binomial/Poisson/Negative Binomial family. If the variance (nearly)
C equals the mean, the Poisson is used. If V < M the Binomial is used
C and if V > M the Negative Binomial. The traditional notation is 
C given in the comments sections below. The coding could be enhanced
C to give better error reporting; as it is, the error code is not
C used. This routine is **NOT** able or intended to simulate negative
C sample sizes or continuous distributions.
C--------------------------------------------------------------------
C
C Called by:
C
C     DMNB 
C
C Other routines called:
C
C     GAMMLN [part of this routine's file]
C
C Argument list definitions:                        
C
C     REAL    M       (I) The mean of the distribution.
C     REAL    V       (I) The variance of the distribution.
C     LOGICAL err     (O) Flag indicating if any anomalous conditions
C                          occurred (.TRUE.); .FALSE. otherwise.
C     INTEGER UBound  (I) The length of the array 'PDF()'.
C     REAL    PDF     (O) The array containing the probability
C                          distribution.
C     INTEGER End     (O) The position of the largest non-zero 
C                          probability, ie: the last value of the 
C                          upper tail.      
C
C Local variable definitions:
C
C     INTEGER method      The kind of distribution: 1= Poisson; 
C                          2= Negative Binomial; 3= Binomial.
C     INTEGER jp          Offset counter to get around compiler bug.
C     INTEGER j           Loop counter for the distribution.
C     REAL    k           Computed constant; meaning depends on
C                          distribution type.
C     REAL    p           Like 'k'
C     REAL    t1          Like 'k'.
C     REAL    t2          Like 'k'.
C     REAL    t3          Like 'k'.
C     REAL    sum         Counter for the summation of the
C                          distribution. This is used to insure that
C                          the distribution does not exceed 1.0.
C     REAL    plast       The probability associated with the
C                          previous sample size.
C     REAL    pnow        The probabilitiy of the current sample
C                          size.
C     REAL    z           The logarithm of the probability of the
C                          current sample size. This is the value
C                          that is actually computed, *then* 
C                          converted back to a {0,1} number.
C     REAL    tol         The value used to discriminate between
C                          distribution types; whether the mean and
C                          variance are valid at all; and whether
C                          the upper tail of the distribution has
C                          been reached.
C
C Common block variables and parameters:
C
C     [none]
C
C********************************************************************

C Subroutine arguments.
      
      REAL    M
      REAL    V
      LOGICAL err 
      INTEGER UBound
      REAL    PDF(*)
      INTEGER End           

C Local variables.
      
      INTEGER method, jp, j
      REAL    k, p, t1, t2, t3, sum, plast, pnow, z
      REAL    tol / 1.0e-6 /

C >>>>>>>>>>>>>>>>>>>>>>>>>>>> READ THIS <<<<<<<<<<<<<<<<<<<<<<<<<<<<
C ERROR IN VARIABLE LENGTH ARRAYS: Note that 'jp' is required       <
C because the variable-length array PDF(*) is actually              <
C PDF(0:UBound) in the calling routine. An error in MS PowerStation <
C 1.0 prevents this from being recognized properly. Therefore, the  <
C 'jp' index bumps things up one. The actual 'j' location will be   <
C correct upon return.                                              <
C >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> +++++++++ <<<<<<<<<<<<<<<<<<<<<<<<

C Zero the distribution.

      DO 10 j = 0, UBound          
        jp = j+1
        PDF(jp) = 0.
        End = 1
   10 CONTINUE

C If mean or variance is very small, RETURN.
            
      err = .FALSE.
      IF ((M .LT. tol) .OR. (V .LT. tol)) THEN
        err = .TRUE.
        RETURN 
      END IF      
            
C Choose the method and compute constant terms.

C Poisson.
      
      IF (ABS(V - M) .LT. tol) THEN
      
        method =  1       
        
        t1 = -(M)
        t2 = LOG(M)
                        
C Negative Binomial.

      ELSE IF (V .GT. M) THEN
      
        method = 2
                  
        p = (V / M) - 1.0
        k = M / p        
 		
C kpq = variance
C kp  = mean
C q   = kpq/kp
C q-p = 1
C p   = q-1
      
        t1 = -k * LOG(1. + (M / k))
        t2 = GAMMLN(k)
        t3 = LOG(M / (M + k))

C Binomial.
      
      ELSE 
      
        method = 3
        
        p = 1. - (V / M)
        k = M / p                       
        
C kpq = variance
C kp  = mean
C q+p = 1
C p   = 1-q

        t1 = GAMMLN(k + 1.0)
        t2 = LOG(p)
        t3 = LOG(1.0 - p) 
        
      END IF
      
      sum = 0.
      DO 1000 j = 0, UBound

C The 'jp' indexing circumvents the 0:UBound error in variable length
C arrays.
        
        jp = j + 1
      
        x = FLOAT(j)
        
C The order here is Poisson (100), Negative Binomial (200),
C Binomial (300).

        GOTO (100,200,300), method

C Poisson if mean is nearly equal to the variance. The 'nearly'
C covers a lot of ground, since some of the limiting cases of the B()
C and NB() arise when the variance/mean ratio is 1. The chosen
C distribution is P() near these limits.
C
C     z= -(M) + (x * LOG(M)) - GAMMLN(x + 1.) 

  100   CONTINUE     
        z = t1 + (x * t2) - GAMMLN(x + 1.) 
        GOTO 400

C Negative binomial if variance > mean. Note that many terms have
C been precomputed in the setup call. The old form of the equation
C is:
C
C     z = -k * LOG(1. + (M / k))
C    >    + GAMMLN(k + x) - GAMMLN(x + 1.)  - GAMMLN(k)
C    >    + x * LOG(M / (M + k))

  200   CONTINUE
        z =  t1 + GAMMLN(k + x) - GAMMLN(x + 1.) - t2 + (x * t3)
        GOTO 400
              
C Binomial if if variance < mean. Note precomputed terms. The old
C form of the equation is:
C
C     z = GAMMLN(k + 1.) - GAMMLN(x + 1.) - GAMMLN(k - x + 1.)
C    >    + (x * LOG(p)) + ((k - x) * LOG(1. - p)) 

              
  300	  CONTINUE  
  
        IF ((k - x + 1.0) .LT. 0.0) THEN
          z = -99.
        ELSE
          z = t1 - GAMMLN(x + 1.) - GAMMLN(k - x + 1.)
     >       + (x * t2) + ((k - x) * t3) 
        END IF

  400   CONTINUE
   
C Convert logarithms back to real values. If 'v' is less than -75,
C the result is nearly zero (in single precision), so no assignment
C is made: PDF(j) is 0.0 prior to assignment. The 'jp' indexing gets
C around the 0:UBound problem described above.

        IF (z .GT. -75.) PDF(jp) = EXP(z)
        
        sum = sum + PDF(jp)

C Set 'plast' for the n=0 case
        
        IF (j .EQ. 0) THEN 
        
          plast = PDF(jp) 
          
        ELSE 

C Stop computing if a) the value is less than 1e-6 on the descending
C limb; or b) the sum has passed 1.0 (this happens for pathological
C binomial cases, and some adjustment is needed... even then it is
C biased); or c) the end of the PDF array has been reached.
        
          pnow = PDF(jp)
          
          IF ((pnow .LT. plast) .AND. (pnow .LT. tol)) THEN
            End = j                                     
            GOTO 1001
          END IF
          
          IF (sum .GE. 1.0) THEN
            End = j 
            PDF(jp) = PDF(jp) - (sum - 1.0)
C SHOULD CALL ERROR HANDLER TO SIGNAL UNRELIABLE RESULTS
            GOTO 1001
          END IF       
            
          IF (j .EQ. UBound) THEN
            End = j
C SHOULD CALL ERROR HANDLER TO SIGNAL UNRELIABLE RESULTS
            GOTO 1001
          END IF              
            
        plast = pnow
        
        END IF
            
 1000 CONTINUE
 1001 CONTINUE
 
      RETURN
      END
C
C
      FUNCTION GAMMLN(Arg)
      
C********************************************************************
C **GAMMLN -- NISI  Date of last revision: April 15 1994 
C--------------------------------------------------------------------
C Purpose:
C   Compute the natural logarithm of the gamma function for the
C argument. Taken from Numerical Recipes in C.
C--------------------------------------------------------------------
C
C Called by:
C
C     BNDIST 
C
C Other routines called:
C
C     [none]
C
C Argument list definitions:                        
C
C     REAL    Arg (I) The argument to the gamma function.
C
C Local variable definitions:
C
C     INTEGER j       Loop counter.
C     REAL    SqPI    Square root of PI.
C     REAL    x       Temporary place holder.
C     REAL    tmp     Temporary place holder.
C     REAL    ser     Series to be accumulated.
C     REAL    cof     Coefficients of interpolation function.
C
C Common block variables and parameters:
C
C     [none]
C
C********************************************************************

C Function arguments.

      REAL      Arg

C Local variables
                      
      INTEGER   j
      REAL      SqPI, x, tmp, ser
      REAL      cof(6)
      
      DATA cof / 76.18009173,
     >          -86.50532033,
     >           24.01409822,
     >           -1.231739516,
     >            0.12085003e-2,
     >           -0.536382e-5 /  

      DATA SqPI / 2.50662827465 /

      x = Arg - 1.0
      tmp = x + 5.5
      tmp = tmp - (x + 0.5) * LOG(tmp)
      
      ser = 1.0
      do 10 j = 1, 6
        x = x + 1.0
        ser = ser + cof(j) / x
   10 continue
   
      GAMMLN = (-tmp + LOG(SqPI * ser))
      
      END
