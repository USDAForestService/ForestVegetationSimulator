      SUBROUTINE BMCBKP (ISTD,IYR,OLDGRF)
C----------
C WWPB $Id$
C----------
C CALLED BY:  BMDRV
C CALLS:      GPGET2
C             GPADD
***********************************************************************
*  **BMCBKP  Date of last revision:  09/28/05
*----------------------------------------------------------------------
*  Purpose
*     Calculate the BKP resulting from trees killed in the previous year
*
*  Call list definitions:
*     OLDGRF: last year's GRF
*
*  Local variable definitions:
*     IDSIZ:  Loop counter over dead woody pool size categories
*     IDTYP:  Loop counter over dead woody pool types (Ips)
*     INC:    Array containing "increase" amounts for each pine beetle
*             species and dbh size class
*     ISIZ:   Loop counter over dbh size classes 
*     LBAD:   Logical flag for is this a bad year for reproduction
*     REPRD:  Generation multiplier for non-Ips REPRDuction this year 
*     REPRDI: Generation multiplier for Ips REPRDuctions this year 
*     SLINC:  Increase factor for REPRD for Ips in slash
*
*  Common block variables and parameters:
*     ALLKLL: From BMCOM; Array containing the number of stems killed
*             by Ips in each size class 
*     BADREP: The reproductive value in the bad year, for all trees
*     IBADYR: The year in which reproduction is "bad"
*     IBADBB: The species for which reproduction is "bad" 
*     MSBA:   From BMCOM; BA in each size class
*     MXDWHC: From BMPRM; Max number of Dead Woody pool types (IPS)
*     MXDWSZ: From BMPRM; Max number of Dead Woody Pool Size categories
*     NBGEN:  From BMCOM; the number of MPB/WPB generations/year
*     NIBGEN: From BMCOM; the number of IPS generations/year
*     NSCL:   From BMPRM; the number of dbh size classes 
*     OLDBKP: From BMCOM; array containing the BKP in a stand before dispersal
*     PSLASH: From BMCOM; the proportion slash inhabited by Ips 
*     TOPKLL: From BMCOM; Array containing the proportion of tops attacked
*             by Ips in each size class   
*     WPBA:   From BMCOM; Array containing the BA in each 
*              Dead Woody Pool size class
*
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77' 
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.

      INCLUDE 'PPCNTL.F77'
      INCLUDE 'BMCOM.F77'

C.... Variable declarations.                             

      INTEGER IDSIZ, IDTYP
      INTEGER DUM(1)
      LOGICAL LOK
      REAL    REPRD, REPRDI, SLINC 
      REAL    PRMS(7)                   
      REAL    OLDGRF(NSCL)

      SAVE

      IF (LBMDEB) WRITE (JBMBPR,1) IYR, ISTD
    1 FORMAT (' IN BMCBKP:  IYR=',I4, 'ISTD= ', I6)

C     Increase factor for ips in slash
      SLINC = 5.0
    
C...    REPRD is to account for multiple generations

      IF (NBGEN .EQ. 1) REPRD= 1.0
      IF (NBGEN .EQ. 2) REPRD= 1.5
      IF (NBGEN .EQ. 3) REPRD= 2.0
      IF (NBGEN .GT. 3) REPRD= 2.5
      
C....    turns off Ips as a DV completely

      IF (PBSPEC .NE. 3 .AND. .NOT. IPSON) REPRDI= 0.

C....    REPRDI is for Ips, when Ips is driving var.

      IF (NIBGEN .EQ. 1) REPRDI= 1.0
      IF (NIBGEN .EQ. 2) REPRDI= 1.5
      IF (NIBGEN .EQ. 3) REPRDI= 2.0
      IF (NIBGEN .GT. 3) REPRDI= 2.5

C.... if Ips main pest then make sure REPRD reflects this
C     and make sure there are no other main beetles accidently on

      IF (PBSPEC .EQ. 3) THEN
         IF (REPRD .NE. 0.0) REPRDI = REPRD
         REPRD= 0.0
      ENDIF

C     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.

      IF (ICNT .GE. BMEND) ICNT = 0
      ICNT = ICNT + 1

      IF (ICNT .EQ. 1) THEN

        IYR1= IYR
        NPRMS= 4
        LBAD = .FALSE.

C     FETCH THE MOST RECENTLY SCEHEDULED UNTRIGGERED ACTIVITY. IF THERE
C     ARE ANY SET FOR THIS YEAR (IYR), THEN THEY TAKE OVER ANY CURRENT
C     SET OF PARAMETERS.

        CALL GPGET2 (317, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

        IF (LOK) THEN

          LBAD = .TRUE.

          IYR2= IYR1 + IFIX(PRMS(1)) - 1
          IBADBB = INT(PRMS(2))

          IF (IBADBB .EQ. 4) THEN
            BADREP(PBSPEC) = PRMS(3)
            BADREP(3) = PRMS(4)
          ELSE
            BADREP(IBADBB) = PRMS(3)
          ENDIF

          IF (LBMDEB) WRITE (JBMBPR,101) MICYC, IYR1, IBADBB
  101     FORMAT (/' IN BMSETP: MICYC=', I5, 'IYR1=',I5,' BBTYPE=',I5)

          IF (IYR2.GE.MIY(MICYC)) THEN
            PRMS(1) = IYR2 - MIY(MICYC) + 1
            CALL GPADD (KODE, MIY(MICYC), 317, NPRMS, PRMS(1), 1, DUM)
            IYR2 = MIY(MICYC) - 1
            IF (LBMDEB) WRITE (JBMBPR,103) PRMS(1),IYR2
  103       FORMAT (/' IN BMCBKP: "BAD REPRODUCTION" IS SCHEDULED FOR ',
     >        'THE NEXT MASTER CYCLE. DURATION WILL BE =',F5.0,
     >        '  NEW IYR2=',I5)
          ENDIF
        ELSE
C         Make sure that we will still get into the proper loops.
          IF (IYR .LE. IYR2) LBAD = .TRUE.
        ENDIF
      ENDIF

C.... PBKill was changed to trees/acre, STRIP is trees/acre,
C     TOPKLL is a proportion and ALLKLL was changed to trees/acre.

      DO 100 ISIZ=1,NSCL

        IF (LBAD) THEN

C         If this is a bad climatic year then reproduction of the beetles is reduced
c         and is constant for all size classes. Note that we assume there is no
c         reproduction in strip-killed trees.

          IF (IBADBB .NE. 3 .AND. PBSPEC .NE. 3) THEN

            IF (INT(FINAL(ISTD,2)) .EQ. ISIZ) THEN

C            First, figure out the bkp emerging from the last tree killed
c            if in this size class. final(1)=bkp used, (2)=size class,
c            (3)=tpa killed
              PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) - FINAL(ISTD,3)
              BKP(ISTD)= BKP(ISTD) + FINAL(ISTD,1) * FINAL(ISTD,3)
     >                               * BADREP(PBSPEC)

              FINAL(ISTD,1) = 0.0
              FINAL(ISTD,2) = 0.0
              FINAL(ISTD,3) = 0.0
             ENDIF

             BKP(ISTD)= BKP(ISTD) + PBKILL(ISTD,ISIZ) * MSBA(ISIZ)
     &                              * BADREP(PBSPEC)
             PBKILL(ISTD,ISIZ)= 0.0

          ENDIF
          IF (IBADBB .GE. 3) THEN

            BKPIPS(ISTD)= BKPIPS(ISTD) + BADREP(3) * TOPKLL(ISTD,ISIZ)
     &                                * MSBA(IPSMIN) * TREE(ISTD,ISIZ,1)
            BKPIPS(ISTD)= BKPIPS(ISTD) + BADREP(3) * ALLKLL(ISTD,ISIZ)
     &                                             * MSBA(IPSMIN)
            ALLKLL(ISTD,ISIZ) = 0.0

          ENDIF

        ELSE

C     Note that reproduction is based on the amount of BKP that actually
c     went into a tree

         IF (PBSPEC .NE. 3) THEN

C          First, figure out the bkp emerging from the last tree killed
c          if in this size class. final(1)=bkp used, (2)=size class,
c          (3)=tpa killed.  Set in BMISTD

           IF (INT(FINAL(ISTD,2)) .EQ. ISIZ) THEN

            PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) - FINAL(ISTD,3)

            IF (PBKILL(ISTD,ISIZ).LT.0.0) PBKILL(ISTD,ISIZ)=0.0  !AJM 9/05

            BKP(ISTD)= BKP(ISTD) + INC(PBSPEC,ISIZ) *
     >                             FINAL(ISTD,1) * FINAL(ISTD,3)
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
Cc troubleshoot ajm 9/05
C           write(99,105)istd,iyr,isiz,bkp(istd),pbkill(istd,isiz),
C     >     final(istd,1),final(istd,2),final(istd,3),inc(pbspec,isiz),
C     >     msba(isiz)
C  105  format('istd=',i4,'  iyr=',i4,'  isiz=',i3,'   bkp=',f10.6,
C     >       '  pbkill=',f10.6,'  final1(bkp)=',f10.6,'  final2(sc)=',
C     >        f4.1,'  final3(tpakld)=',f10.6,'  inc=',f5.3,'  msba=',
C     >        f6.3)
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            FINAL(ISTD,1) = 0.0
            FINAL(ISTD,2) = 0.0
            FINAL(ISTD,3) = 0.0
           ENDIF

c          then figure out the reproduction from all completely filled trees
           BKP(ISTD)= BKP(ISTD) + MSBA(ISIZ) * PBKILL(ISTD,ISIZ)
     &                            * INC(PBSPEC,ISIZ)
           PBKILL(ISTD,ISIZ)= 0.0

C....      STRIP attacks produce brood, but not quite as well. Since strip attacks
c          are always last, use the GRF to modify the amount immediately.

           BKP(ISTD)= BKP(ISTD) + MSBA(ISIZ) * INC(PBSPEC,ISIZ)
     &                  * 0.75 * STRIP(ISTD,ISIZ)

C          ******************************************************************
C          It seems to me that there is no reason to use GRF as a modifier so
C          term OLDGRF(ISIZ) is drop here.  S.Zhang, 4/13/98


C           * OLDGRF(ISIZ)
C          *******************************************************************

           STRIP(ISTD, ISIZ)= 0.0

         ENDIF

C        Ips reproduction from topkilled and totally killed trees. Note that
C        since Ips only attacks and colonizes the part of the tree less than
c        3" diameter, all the INCrease factors are the same.

         BKPIPS(ISTD)= BKPIPS(ISTD) + INC(3,ISIZ) * TOPKLL(ISTD,ISIZ)
     &                              * TREE(ISTD,ISIZ,1) * MSBA(IPSMIN)
         BKPIPS(ISTD)= BKPIPS(ISTD) + INC(3,ISIZ) * ALLKLL(ISTD,ISIZ)
     &                                            * MSBA(IPSMIN)


         ALLKLL(ISTD,ISIZ)= 0.0
C            (TOPKLL may be needed in a later routine so is not zeroed here)

        ENDIF
  100 CONTINUE

C.... to account for multiple generations

      BKP(ISTD)= BKP(ISTD) * REPRD
      BKPIPS(ISTD)= BKPIPS(ISTD) * REPRDI

***************************************************************************
c...Modification to BKP if feedback is invoked. AJM. 12/1/99

C      IF (LFDBK) THEN
C        IF (BKP(ISTD) .GE. TFDBK) THEN
C          BKP(ISTD) = BKP(ISTD) * SFDBK
C        END IF
C      END IF

      IF (LFDBK) THEN
        IF (BKP(ISTD) .GT. OLDBKP(ISTD)) THEN
          IF (OLDBKP(ISTD) .GE. TFDBK) BKP(ISTD) = TFDBK
        END IF
      END IF
***************************************************************************
C.... PSLASH() is stems/acre

C.... Ips does very well in slash, irrespective of size, except in bad years

      REPRDI= REPRDI * SLINC
      DO 200 IDTYP=1,MXDWHC
        DO 210 IDSIZ=1,MXDWSZ
          IF (LBAD .AND. IBADBB .GE. 3) THEN
            BKPIPS(ISTD)= BKPIPS(ISTD) + PSLASH(ISTD,IDTYP,IDSIZ)
     &                                   * WPBA(IDSIZ) * BADREP(3)
          ELSE
            BKPIPS(ISTD)= BKPIPS(ISTD) + PSLASH(ISTD,IDTYP,IDSIZ)
     >                                   * WPBA(IDSIZ) * REPRDI
          ENDIF
          PSLASH(ISTD,IDTYP,IDSIZ)= 0.0
  210   CONTINUE
  200 CONTINUE

      IF (PBSPEC .NE. 3) THEN
        OLDBKP(ISTD)= BKP(ISTD)
      ELSE
        OLDBKP(ISTD)= BKPIPS(ISTD)
      ENDIF

      IF (LBMDEB) WRITE (JBMBPR,2) IYR, ISTD
    2 FORMAT (' LEAVING BMCBKP:   IYR=',I4, 'ISTD= ', I6)

      RETURN
      END
