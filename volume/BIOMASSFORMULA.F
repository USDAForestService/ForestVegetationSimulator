      SUBROUTINE BIOMASSFORMULA(EQFORM,DBH,THT,CR, a, b, c, d, e, BMS)
      INTEGER EQFORM
      REAL DBH, THT, CR, BMS, TopD
      REAL a, b, c, d, e

      BMS = 0
C     CR as crown ratio (crown length/total height)      
      IF(CR.GT.1.OR.CR.LE.0) CR = 0.5
C     Default TopD to 4" when no value provided
      TopD = 4      

      CALL BioeqFormula(EQFORM,DBH,THT,CR,TopD,a,b,c,d,e,BMS)
      RETURN
      END
C ***********************************************************************      
      SUBROUTINE BioeqFormula(EQFORM,DBH,THT,CR,TopD,a,b,c,d,e,BMS)
      INTEGER EQFORM
      REAL DBH, THT, CR, BMS, TopD,CL
      REAL a, b, c, d, e
      
      BMS = 0
C     CR as crown ratio (crown length/total height)      
      IF(CR.GT.1.OR.CR.LE.0) CR = 0.5
C     Crown Length
      CL = CR*THT      
C     Default TopD to 4" when no value provided
      IF(TopD.LE.0) TopD = 4      
      IF (EQFORM.EQ.1) THEN
C     log10 biomass = a + b * (log10(dia^c))      
         BMS = a + b * (LOG10(DBH**c))
         BMS = 10**BMS
      ELSE IF (EQFORM.EQ.2) THEN
C     ln biomass = a + b * dia + c * (ln(dia^d)) + e*tht         
         BMS = a + b * DBH + c * (LOG(DBH**d)) + e*THT
         BMS = EXP(BMS)
      ELSE IF (EQFORM.EQ.3) THEN
C     ln biomass = a + b * ln(dia) + c * (d + e * ln(dia))
         BMS = a + b * LOG(DBH) + c * (d + e * LOG(DBH))
         BMS = EXP(BMS)
      ELSE IF (EQFORM.EQ.4) THEN
C     biomass = a + b * dia + c * dia ^ d + e*dia^2*tht
         BMS = a + b * DBH + c * DBH**d + e*DBH**2*THT
      ELSE IF (EQFORM.EQ.5) THEN 
C     biomass = a + b * dia + c * dia ^ 2 + d * dia ^ 3 + e*dia*tht
         BMS = a + b * DBH + c * DBH**2 + d * DBH**3 + e*DBH*THT 
      ELSE IF (EQFORM.EQ.6) THEN
C     biomass = a * (exp(b + c * ln(dia) + d * dia))
         BMS = a * (EXP(b + c * LOG(DBH) + d * DBH))   
      ELSE IF (EQFORM.EQ.7) THEN  
C     biomass = a + (b * dia ^ c)/(dia ^ c + d)
         BMS = a + (b * DBH**c)/(DBH**c + d)  
      ELSE IF (EQFORM.EQ.8) THEN  
C     log100 biomass = a + b * log10(dia)
         BMS = a + b * LOG10(DBH)
         BMS = 100**BMS
      ELSE IF (EQFORM.EQ.9) THEN  
C     ln biomass = ln(a) + (b * ln(dia))
         BMS = LOG(a) + (b * LOG(DBH)) 
         BMS = EXP(BMS)
      ELSE IF (EQFORM.EQ.10) THEN    
C     biomass= exp(a + b*ln(dia))*exp(c + d/dia)
         BMS = EXP(a + b*LOG(DBH))*EXP(c + d/DBH)   
      ELSE IF (EQFORM.EQ.11) THEN
C     biomass = a + b*dia^2*tht + c*dia^3 + d*dia*tht
         BMS = a + b*DBH**2*THT + c*DBH**3 + d*DBH*THT 
      ELSE IF (EQFORM.EQ.12) THEN 
C     ln biomass = a + b*ln(dia) + c*ln(tht)
         BMS = a + b*LOG(DBH) + c*LOG(THT) 
         BMS = EXP(BMS)
      ELSE IF (EQFORM.EQ.13) THEN  
C     biomass = exp(a + b*ln(dia) + d*ln(tht))*c
         BMS = EXP(a + b*LOG(DBH) + d*LOG(THT))*c 
      ELSE IF (EQFORM.EQ.14) THEN  
C     biomass = exp(a + b*ln(dia))/(c + d*dia^e)
         BMS = EXP(a + b*LOG(DBH))/(c + d*DBH**e)
      ELSE IF (EQFORM.EQ.15) THEN
C     biomass = a + b*tht + c*tht^2
         BMS = a + b*THT + c*THT**2
      ELSE IF (EQFORM.EQ.16) THEN
C     biomass = a * dia^b * tht^c
         BMS = a * DBH**b * THT**c
      ELSE IF (EQFORM.EQ.17) THEN
C     biomass = a + b*dbh + c*dbh^2 + d*tht + e*dbh^2*tht
         BMS = a + b*DBH + c*DBH**2 + d*THT + e*DBH**2*THT
      ELSE IF (EQFORM.EQ.18) THEN
C     ln biomass = a + b*ln(dia) + c*ln(CR) + d*ln(CR*tht)
C     CR as crown ratio (crown length/total height)
         BMS = a + b*LOG(DBH) + c*LOG(CR) + d*LOG(CR*THT)
         BMS = EXP(BMS)
      ELSE IF(EQFORM.EQ.19) THEN
C     ratio = 1 - EXP(a*TopD^b*DBH^c)
C     This is the Tip biomass to total stem above stump ratio
         BMS = 1-EXP(a*TopD**b*DBH**c)        
      ELSE IF (EQFORM.EQ.33) THEN 
        IF(DBH.LT.11)THEN
          BMS = a*(DBH**2)**b 
        ELSE
          BMS = c*(DBH**2)**d
        ENDIF
      ELSE IF (EQFORM.EQ.34) THEN
        IF(DBH.LT.11) THEN
          BMS = a*(DBH**2*THT)**b
        ELSE
          BMS = c*(DBH**2)**d*THT**e
        ENDIF
      ELSEIF(EQFORM.EQ.35)THEN
        BMS = a+b*DBH**3+c*DBH**2*CR+d*DBH**2*THT*CR
      ELSEIF(EQFORM.EQ.36)THEN
        BMS = a+b*DBH**2+c*DBH**2*THT+d*DBH*THT*CR+e*DBH**2*THT*CR
      ELSEIF(EQFORM.EQ.37)THEN
        BMS = a+b*LOG(DBH*THT)+c*LOG(THT*CR)
        BMS = EXP(BMS)
      ELSEIF(EQFORM.EQ.38)THEN
        BMS = a+b*THT+c*THT*CR+d*DBH*THT*CR
      ELSEIF(EQFORM.EQ.39)THEN
        BMS = a*DBH**b*THT**c*EXP(d*CR)
      ELSEIF(EQFORM.EQ.40)THEN
        BMS = a+b*DBH**c*THT**d*CL**e
      ELSEIF(EQFORM.EQ.41)THEN
C       ratio equation      
        BMS = 1/(1+a*DBH**b)
      ELSEIF(EQFORM.EQ.42)THEN
        BMS = a*DBH**b*EXP(c*THT)  
      ELSEIF(EQFORM.EQ.43)THEN
        BMS = a*DBH**2*THT*(b+c*TopD/DBH+d*(Topd/DBH)**2)/100.0  
          
      ENDIF 
      END
            