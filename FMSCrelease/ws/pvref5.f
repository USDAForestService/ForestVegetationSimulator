      SUBROUTINE PVREF5 (KARD2,ARRAY2,LPVCOD,LPVREF)
C----------
C WS $Id$
C----------
C
C     MAPS PV/REFERENCE CODES INTO A FVS HABITAT/ECOCLASS CODE
C     CALLED FROM **HABTYP** WHEN REFTMP IS GREATER THAN ZERO
C
C     INPUT VARIABLES
C     KARD2          - FIELD 2 OF STDINFO KEYWORD
C     ARRAY2         - FIELD 2 OF STDINFO (REAL CONTERPART TO KARD2)
C     CPVREF         - FIELD 7 OF STDINFO KEYWORD
C                    - CARRIED IN PLOT.F77
C
C     RETURN VARIABLES
C     KARD2          - MAPPED FVS HABITAT/ECOCLASS CODE
C
C     INTERNAL VARIABLES
C     PVCODE,PVREF   - ARRAYS OF PV CODE/REFERENCE CODE COMBINATIONS
C                      FROM FSVEG DATA BASE
C     HABPVR         - FVS HABITAT/ECOCLASS CODE CORRESPONDING TO
C                      PV CODE/REFERENCE CODE COMBINATION
C  DECLARATIONS
C
      IMPLICIT NONE
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
      INCLUDE 'PLOT.F77'
C
C  DECLARATIONS
      REAL         ARRAY2
      INTEGER      I,NCODES
      PARAMETER    (NCODES=650)
      CHARACTER*10 PVREF(NCODES),PVCODE(NCODES),KARD2,KARD2T
      CHARACTER*10 HABPVR(NCODES)
      LOGICAL      LPVCOD,LPVREF
C
C  DATA STATEMENTS
C
      DATA (HABPVR(I),I=   1,  60)/
     &'CC0411    ','CPJSOD11  ','CPL00000  ','CPLSOH00  ',
     &'CPLSOH11  ','CPLSOH12  ','CPLST000  ','CPLST011  ',
     &'CPSHGC11  ','CPWCFW00  ','CPWCFW11  ','CX000000  ',
     &'CX0D0000  ','CX0FBB11  ','CX0FFS11  ','CX0FRE11  ',
     &'CX0FTP11  ','CX0FWS11  ','CX0GCR11  ','CX0HAW11  ',
     &'CX0HDP00  ','CX0HDP13  ','CX0HDP14  ','CX0HMB12  ',
     &'CX0HOL00  ','CX0HOL15  ','CX0HOL16  ','CX0HOL17  ',
     &'CX0HT000  ','CX0HT011  ','CX0HT012  ','CX0HT013  ',
     &'CX0HT014  ','CX0M0000  ','CX0R0000  ','CX0SAM12  ',
     &'CX0SDA11  ','CX0SE000  ','CX0SE011  ','CX0SE012  ',
     &'CX0SE013  ','CX0SE014  ','CX0SHN12  ','CX0SLS11  ',
     &'CX0SMA11  ','CX0SMA12  ','CX0SMM00  ','CX0SMM11  ',
     &'CX0SMM12  ','CX0SSS13  ','CX0W0000  ','DC1011    ',
     &'DC1012    ','DC1013    ','DC1014    ','DC1015    ',
     &'DC1016    ','DC1017    ','DC1018    ','DC1019    '/
      DATA (HABPVR(I),I=  61, 120)/
     &'DS0911    ','JC0111    ','JC0112    ','MC0211    ',
     &'PG0611    ','PG0612    ','PG0613    ','PG0614    ',
     &'PS0811    ','PS0812    ','PS0813    ','PS0911    ',
     &'QC0211    ','QC0212    ','RC0011    ','RC0331    ',
     &'RC0421    ','RC0511    ','RC0512    ','RC0513    ',
     &'RC0611    ','RC0612    ','RC0613    ','RF0411    ',
     &'RF0412    ','RS0114    ','RS0511    ','WC0413    ',
     &'WC0711    ','WC0712    ','WC1011    ','WC1012    ',
     &'WC1013    ','          ','          ','          ',
     &'          ','43014     ','43015     ','43016     ',
     &'43017     ','43031     ','43061     ','43062     ',
     &'43063     ','43064     ','43065     ','43066     ',
     &'43067     ','43071     ','43072     ','43073     ',
     &'43074     ','43075     ','43076     ','          ',
     &'          ','          ','43106     ','          '/
      DATA (HABPVR(I),I= 121, 180)/
     &'          ','43153     ','43154     ','          ',
     &'43156     ','          ','          ','43246     ',
     &'43247     ','43261     ','43262     ','43263     ',
     &'43264     ','43265     ','43266     ','43272     ',
     &'43273     ','43274     ','43275     ','43276     ',
     &'43282     ','43284     ','43285     ','43287     ',
     &'43288     ','43289     ','43290     ','43291     ',
     &'43292     ','43293     ','43294     ','43304     ',
     &'43325     ','43327     ','43328     ','43329     ',
     &'43351     ','43352     ','43451     ','43500     ',
     &'43554     ','43605     ','43606     ','43651     ',
     &'          ','43803     ','          ','          ',
     &'          ','          ','43811     ','          ',
     &'          ','          ','          ','          ',
     &'43872     ','          ','43883     ','43905     '/
      DATA (HABPVR(I),I= 181, 240)/
     &'43911     ','43915     ','43916     ','          ',
     &'          ','43991     ','43995     ','CC0311    ',
     &'CCOCCO00  ','CCOCCO11  ','CCOCCO12  ','CCOCCO12  ',
     &'CCOCCO13  ','CCOCCO13  ','CCOCCO14  ','CCOCCO14  ',
     &'          ','          ','          ','          ',
     &'CCOCFW00  ','CCOCFW11  ','CCOCFW11  ','CCOCFW12  ',
     &'CCOCFW12  ','CCOCFW13  ','CCOCFW13  ','CCOCFW14  ',
     &'CCOCFW15  ','CCOCFW16  ','CCOCFW17  ','CCOCFW17  ',
     &'CCOCFW18  ','CCOCFW18  ','          ','          ',
     &'CD000000  ','CD0CCI00  ','CD0CCI11  ','CD0CCI11  ',
     &'CD0CPJ00  ','CD0CPJ11  ','CD0CPJ11  ','CD0HAR00  ',
     &'CD0HAR11  ','CD0HBC00  ','CD0HBC11  ','CD0HBC11  ',
     &'CD0HBC12  ','CD0HGC00  ','CD0HGC11  ','CD0HGC12  ',
     &'CD0HGC13  ','CD0HGC14  ','CD0HGC15  ','CD0HGC16  ',
     &'CD0HGC17  ','CD0HMA00  ','CD0HMA11  ','CD0HMA12  '/
      DATA (HABPVR(I),I= 241, 300)/
     &'CD0HMA13  ','CD0HOB00  ','CD0HOB11  ','CD0HOB12  ',
     &'CD0HOB13  ','CD0HOL00  ','CD0HOL11  ','CD0HOL12  ',
     &'CD0HOL13  ','CD0HOO00  ','CD0HOO11  ','CD0HOO12  ',
     &'CD0HT000  ','CD0HT011  ','CD0HT012  ','CD0HT012  ',
     &'CD0SM000  ','CD0SM011  ','CD0SOH00  ','CD0SOH11  ',
     &'CD0SOH11  ','CD0SOH11  ','CD0SOH12  ','CD0SOH12  ',
     &'CD0SOH13  ','CD0SOH13  ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','CN00000   ','CN00000   ','CN00011   ',
     &'CN00011   ','CNF0111   ','CNF0111   ','CNF0211   ',
     &'CNF0211   ','CNF0311   ','CNF0311   ','CNHB011   ',
     &'CNHB011   ','CNHT011   ','CNHT011   ','          ',
     &'          ','CPJ00000  ','CPJCCI00  ','CPJCCI11  '/
      DATA (HABPVR(I),I= 301, 360)/
     &'CPJCCI12  ','CPJCCI13  ','CPJCCI14  ','          ',
     &'          ','CPJCFW11  ','CPJCFW12  ','          ',
     &'          ','          ','          ','CPJGBW11  ',
     &'CPJGBW11  ','CPJGFI00  ','CPJGFI11  ','CPJGFI12  ',
     &'CPJGNG11  ','CPJGNG11  ','          ','CPJSAM11  ',
     &'CPJSAM11  ','CPJSAM12  ','CPJSAM12  ','CPJSBB11  ',
     &'CPJSBB11  ','CPJSBB12  ','CPJSBB12  ','CPJSBB13  ',
     &'CPJSBB13  ','CPJSBB14  ','CPJSBB14  ','CPJSBB15  ',
     &'CPJSBB15  ','CPJSBB16  ','CPJSBB16  ','CPJSBB17  ',
     &'CPJSBB17  ','CPJSBB18  ','CPJSBB18  ','CPJSBB19  ',
     &'CPJSBB19  ','CPJSBB20  ','CPJSBB20  ','CPJSBB21  ',
     &'CPJSBB21  ','CPJSBB23  ','CPJSBB23  ','CPJSMC11  ',
     &'CPJSMC11  ','CPJSMC12  ','CPJSMC12  ','CPJSMC13  ',
     &'CPJSMC13  ','CPJSOH11  ','CPJSOH11  ','CPJSSB11  ',
     &'CPJSSB11  ','CPJSSS12  ','CPJSSS12  ','CPJSSY11  '/
      DATA (HABPVR(I),I= 361, 420)/
     &'CPJSSY11  ','          ','          ','          ',
     &'          ','CPOSMP11  ','CPOSMP11  ','CPOSSY11  ',
     &'CPOSSY11  ','          ','CPPSAM11  ','CPPSAM11  ',
     &'CPPSAM12  ','CPPSAM12  ','CPPSAM13  ','CPPSAM13  ',
     &'CPPSAM14  ','CPPSAM14  ','CPPSAM15  ','CPPSAM15  ',
     &'CPPSAM16  ','CPPSAM16  ','CPPSBB11  ','CPPSBB11  ',
     &'CPPSBB12  ','CPPSBB12  ','CPPSBB13  ','CPPSBB13  ',
     &'CPPSBB14  ','CPPSBB14  ','CPPSBB15  ','CPPSBB15  ',
     &'CPPSBB16  ','CPPSBB16  ','CPPSBB17  ','CPPSBB17  ',
     &'CPPSBB18  ','CPPSBB18  ','CPPSBB19  ','CPPSBB19  ',
     &'CPPSBB20  ','CPPSBB20  ','CPPSBB21  ','CPPSBB21  ',
     &'CPPSBB22  ','CPPSBB22  ','CPPSSB11  ','CPPSSB11  ',
     &'CPPSSS11  ','CPPSSS11  ','CPS00000  ','CPSCPL00  ',
     &'CPSCPL11  ','CPSCPL12  ','CPSCPW00  ','CPSCPW11  ',
     &'CPSHGC00  ','CPW00000  ','CPWCD000  ','CPWCD011  '/
      DATA (HABPVR(I),I= 421, 480)/
     &'CPWCPL00  ','CPWCPL11  ','CPWCPS00  ','CPWCPS11  ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'DC0811    ','DC0812    ','DC0813    ','DC0911    ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          '/
      DATA (HABPVR(I),I= 481, 540)/
     &'          ','          ','          ','          ',
     &'          ','          ','DH0711    ','          ',
     &'          ','HOD00000  ','HOD00000  ','HODGA000  ',
     &'HODGA000  ','HODGA011  ','HODGA011  ','HODGA012  ',
     &'HODGA012  ','HODGA013  ','HODGA013  ','HODGA014  ',
     &'HODGA014  ','HODGA015  ','HODGA015  ','HODGA016  ',
     &'HODGA016  ','HODGA017  ','HODGA017  ','HODGA018  ',
     &'HODGA018  ','HODGA019  ','HODGA019  ','HODGA020  ',
     &'HODGA020  ','HODGA021  ','HODGA021  ','HODGA022  ',
     &'HODGA022  ','HODHOI00  ','HODHOI00  ','HODHOI11  ',
     &'HODHOI11  ','          ','          ','HT000000  ',
     &'HT0CCI00  ','HT0CCI11  ','HT0CCO00  ','HT0CCO11  ',
     &'HT0CCO11  ','HT0CCO12  ','HT0CCO12  ','HT0CCO13  ',
     &'HT0CCO13  ','HT0CCO14  ','HT0CCO14  ','HT0CCO15  ',
     &'HT0CCO15  ','HT0CCO16  ','HT0CCO16  ','HT0CCO17  '/
      DATA (HABPVR(I),I= 541, 600)/
     &'HT0CCO17  ','HT0CCO18  ','HT0CCO18  ','HT0CCO19  ',
     &'HT0CCO19  ','          ','          ','          ',
     &'          ','HT0HBC00  ','HT0HBC11  ','HT0HBC12  ',
     &'HT0HGC00  ','HT0HGC11  ','HT0HGC12  ','HT0HGC13  ',
     &'HT0HGC14  ','HT0HGC15  ','HT0HGC16  ','HT0HM000  ',
     &'HT0HM011  ','HT0HM012  ','HT0HM013  ','HT0HOB00  ',
     &'HT0HOB11  ','HT0HOL00  ','HT0HOL11  ','HT0HOL12  ',
     &'HT0HOL13  ','HT0HOL14  ','HT0HOL15  ','HT0HOL16  ',
     &'HT0SD000  ','HT0SD011  ','HT0SD012  ','HT0SEH00  ',
     &'HT0SEH11  ','HT0SEH12  ','HT0SEH13  ','HT0SM000  ',
     &'HT0SM011  ','HT0SOH00  ','HT0SOH11  ','HT0SSG00  ',
     &'HT0SSG11  ','HT0SSG12  ','HT0SSG13  ','          ',
     &'          ','          ','          ','          ',
     &'          ','PC0611    ','          ','          ',
     &'          ','QS0111    ','          ','          '/
      DATA (HABPVR(I),I= 601, NCODES)/
     &'SA000000  ','SA0SB000  ','SA0SBS00  ','SA0SCC00  ',
     &'SA0SCH00  ','SA0SCT00  ','SA0SCW00  ','SA0SMB00  ',
     &'SA0SME00  ','SB0SSW00  ','SBM00000  ','SCH00000  ',
     &'          ','          ','SMB00000  ','SME00000  ',
     &'SOC00000  ','SOI00000  ','SOISCL00  ','SOISOC00  ',
     &'SOISOS00  ','SOS00000  ','SOSSA000  ','SOSSBM00  ',
     &'SOSSCH00  ','SOSSCL00  ','SR000000  ','SR0SA000  ',
     &'SSC00000  ','SSCSB000  ','SSCSSB00  ','WC0911    ',
     &'WC0912    ','WC0913    ','WC0914    ','WC0915    ',
     &'WC0916    ','WC0917    ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          '/
C
      DATA (PVCODE(I),I=   1,  60)/
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          '/
      DATA (PVCODE(I),I=  61, 120)/
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','42012     ','42228     ','43001     ',
     &'43010     ','43014     ','43015     ','43016     ',
     &'43017     ','43031     ','43061     ','43062     ',
     &'43063     ','43064     ','43065     ','43066     ',
     &'43067     ','43071     ','43072     ','43073     ',
     &'43074     ','43075     ','43076     ','43101     ',
     &'43103     ','43104     ','43106     ','43151     '/
      DATA (PVCODE(I),I= 121, 180)/
     &'43152     ','43153     ','43154     ','43155     ',
     &'43156     ','43222     ','43243     ','43246     ',
     &'43247     ','43261     ','43262     ','43263     ',
     &'43264     ','43265     ','43266     ','43272     ',
     &'43273     ','43274     ','43275     ','43276     ',
     &'43282     ','43284     ','43285     ','43287     ',
     &'43288     ','43289     ','43290     ','43291     ',
     &'43292     ','43293     ','43294     ','43304     ',
     &'43325     ','43327     ','43328     ','43329     ',
     &'43351     ','43352     ','43451     ','43500     ',
     &'43554     ','43605     ','43606     ','43651     ',
     &'43801     ','43803     ','43805     ','43807     ',
     &'43808     ','43809     ','43811     ','43812     ',
     &'43821     ','43822     ','43831     ','43871     ',
     &'43872     ','43882     ','43883     ','43905     '/
      DATA (PVCODE(I),I= 181, 240)/
     &'43911     ','43915     ','43916     ','43921     ',
     &'43931     ','43991     ','43995     ','CC0311    ',
     &'CCOCCO00  ','CCOCCO11  ','CCOCCO12  ','CCOCCO12  ',
     &'CCOCCO13  ','CCOCCO13  ','CCOCCO14  ','CCOCCO14  ',
     &'CCOCDO03  ','CCOCFR01  ','CCOCFR02  ','CCOCFR03  ',
     &'CCOCFW00  ','CCOCFW11  ','CCOCFW11  ','CCOCFW12  ',
     &'CCOCFW12  ','CCOCFW13  ','CCOCFW13  ','CCOCFW14  ',
     &'CCOCFW15  ','CCOCFW16  ','CCOCFW17  ','CCOCFW17  ',
     &'CCOCFW18  ','CCOCFW18  ','CCOCFW19  ','CCOCFW20  ',
     &'CD000000  ','CD0CCI00  ','CD0CCI11  ','CD0CCI11  ',
     &'CD0CPJ00  ','CD0CPJ11  ','CD0CPJ11  ','CD0HAR00  ',
     &'CD0HAR11  ','CD0HBC00  ','CD0HBC11  ','CD0HBC11  ',
     &'CD0HBC12  ','CD0HGC00  ','CD0HGC11  ','CD0HGC12  ',
     &'CD0HGC13  ','CD0HGC14  ','CD0HGC15  ','CD0HGC16  ',
     &'CD0HGC17  ','CD0HMA00  ','CD0HMA11  ','CD0HMA12  '/
      DATA (PVCODE(I),I= 241, 300)/
     &'CD0HMA13  ','CD0HOB00  ','CD0HOB11  ','CD0HOB12  ',
     &'CD0HOB13  ','CD0HOL00  ','CD0HOL11  ','CD0HOL12  ',
     &'CD0HOL13  ','CD0HOO00  ','CD0HOO11  ','CD0HOO12  ',
     &'CD0HT000  ','CD0HT011  ','CD0HT012  ','CD0HT012  ',
     &'CD0SM000  ','CD0SM011  ','CD0SOH00  ','CD0SOH11  ',
     &'CD0SOH11  ','CD0SOH11  ','CD0SOH12  ','CD0SOH12  ',
     &'CD0SOH13  ','CD0SOH13  ','CFRCFR11  ','CFRCFR12  ',
     &'CFRCFW11  ','CFRCPL11  ','CFRCPW11  ','CFRCPW12  ',
     &'CFRCPW13  ','CFRCPW14  ','CFRFME11  ','CFWCPR11  ',
     &'CFWCPR12  ','CHMCHM11  ','CHMCHM12  ','CJOCJO11  ',
     &'CJOCJO12  ','CN00000   ','CN00000   ','CN00011   ',
     &'CN00011   ','CNF0111   ','CNF0111   ','CNF0211   ',
     &'CNF0211   ','CNF0311   ','CNF0311   ','CNHB011   ',
     &'CNHB011   ','CNHT011   ','CNHT011   ','COCPW01   ',
     &'COCPW02   ','CPJ00000  ','CPJCCI00  ','CPJCCI11  '/
      DATA (PVCODE(I),I= 301, 360)/
     &'CPJCCI12  ','CPJCCI13  ','CPJCCI14  ','CPJCCO01  ',
     &'CPJCDO11  ','CPJCFW11  ','CPJCFW12  ','CPJCPJ11  ',
     &'CPJCPJ12  ','CPJCPJ13  ','CPJCPJ14  ','CPJGBW11  ',
     &'CPJGBW11  ','CPJGFI00  ','CPJGFI11  ','CPJGFI12  ',
     &'CPJGNG11  ','CPJGNG11  ','CPJS      ','CPJSAM11  ',
     &'CPJSAM11  ','CPJSAM12  ','CPJSAM12  ','CPJSBB11  ',
     &'CPJSBB11  ','CPJSBB12  ','CPJSBB12  ','CPJSBB13  ',
     &'CPJSBB13  ','CPJSBB14  ','CPJSBB14  ','CPJSBB15  ',
     &'CPJSBB15  ','CPJSBB16  ','CPJSBB16  ','CPJSBB17  ',
     &'CPJSBB17  ','CPJSBB18  ','CPJSBB18  ','CPJSBB19  ',
     &'CPJSBB19  ','CPJSBB20  ','CPJSBB20  ','CPJSBB21  ',
     &'CPJSBB21  ','CPJSBB23  ','CPJSBB23  ','CPJSMC11  ',
     &'CPJSMC11  ','CPJSMC12  ','CPJSMC12  ','CPJSMC13  ',
     &'CPJSMC13  ','CPJSOH11  ','CPJSOH11  ','CPJSSB11  ',
     &'CPJSSB11  ','CPJSSS12  ','CPJSSS12  ','CPJSSY11  '/
      DATA (PVCODE(I),I= 361, 420)/
     &'CPJSSY11  ','CPLCPL11  ','CPLCPL12  ','CPLCPL13  ',
     &'CPLCPL14  ','CPOSMP11  ','CPOSMP11  ','CPOSSY11  ',
     &'CPOSSY11  ','CPPS      ','CPPSAM11  ','CPPSAM11  ',
     &'CPPSAM12  ','CPPSAM12  ','CPPSAM13  ','CPPSAM13  ',
     &'CPPSAM14  ','CPPSAM14  ','CPPSAM15  ','CPPSAM15  ',
     &'CPPSAM16  ','CPPSAM16  ','CPPSBB11  ','CPPSBB11  ',
     &'CPPSBB12  ','CPPSBB12  ','CPPSBB13  ','CPPSBB13  ',
     &'CPPSBB14  ','CPPSBB14  ','CPPSBB15  ','CPPSBB15  ',
     &'CPPSBB16  ','CPPSBB16  ','CPPSBB17  ','CPPSBB17  ',
     &'CPPSBB18  ','CPPSBB18  ','CPPSBB19  ','CPPSBB19  ',
     &'CPPSBB20  ','CPPSBB20  ','CPPSBB21  ','CPPSBB21  ',
     &'CPPSBB22  ','CPPSBB22  ','CPPSSB11  ','CPPSSB11  ',
     &'CPPSSS11  ','CPPSSS11  ','CPS00000  ','CPSCPL00  ',
     &'CPSCPL11  ','CPSCPL12  ','CPSCPW00  ','CPSCPW11  ',
     &'CPSHGC00  ','CPW00000  ','CPWCD000  ','CPWCD011  '/
      DATA (PVCODE(I),I= 421, 480)/
     &'CPWCPL00  ','CPWCPL11  ','CPWCPS00  ','CPWCPS11  ',
     &'CX        ','CXF01     ','CXF0111   ','CXF0112   ',
     &'CXF0113   ','CXF02     ','CXF0211   ','CXF0212   ',
     &'CXF0213   ','CXF03     ','CXF0311   ','CXF0312   ',
     &'CXF0313   ','CXHA11    ','CXHB12    ','CXHD      ',
     &'CXHD12    ','CXHD13    ','CXHL      ','CXHL11    ',
     &'CXHL13    ','CXHL14    ','CXHT      ','CXHT11    ',
     &'CXHT12    ','CXHT13    ','CXHT14    ','CXS05     ',
     &'CXS0511   ','CXS0512   ','CXS0513   ','CXS0514   ',
     &'CXS06     ','CXS0611   ','CXS0612   ','CXS07     ',
     &'CXS0711   ','CXS0712   ','CXS0715   ','CXS0716   ',
     &'DC0811    ','DC0812    ','DC0813    ','DC0911    ',
     &'DF        ','DF1       ','DF2       ','DFJP      ',
     &'DFJP1     ','DFJP2     ','DFJP3     ','DFPP      ',
     &'DFPP1     ','DFPP2     ','DFPP3     ','DFPP4     '/
      DATA (PVCODE(I),I= 481, 540)/
     &'DFPP5     ','DFPP6     ','DFPP7     ','DFPP7A    ',
     &'DFPP8     ','DFPP9     ','DH0711    ','FBLFBL11  ',
     &'FCPFCP11  ','HOD00000  ','HOD00000  ','HODGA000  ',
     &'HODGA000  ','HODGA011  ','HODGA011  ','HODGA012  ',
     &'HODGA012  ','HODGA013  ','HODGA013  ','HODGA014  ',
     &'HODGA014  ','HODGA015  ','HODGA015  ','HODGA016  ',
     &'HODGA016  ','HODGA017  ','HODGA017  ','HODGA018  ',
     &'HODGA018  ','HODGA019  ','HODGA019  ','HODGA020  ',
     &'HODGA020  ','HODGA021  ','HODGA021  ','HODGA022  ',
     &'HODGA022  ','HODHOI00  ','HODHOI00  ','HODHOI11  ',
     &'HODHOI11  ','HQAHQA11  ','HQAHQA12  ','HT000000  ',
     &'HT0CCI00  ','HT0CCI11  ','HT0CCO00  ','HT0CCO11  ',
     &'HT0CCO11  ','HT0CCO12  ','HT0CCO12  ','HT0CCO13  ',
     &'HT0CCO13  ','HT0CCO14  ','HT0CCO14  ','HT0CCO15  ',
     &'HT0CCO15  ','HT0CCO16  ','HT0CCO16  ','HT0CCO17  '/
      DATA (PVCODE(I),I= 541, 600)/
     &'HT0CCO17  ','HT0CCO18  ','HT0CCO18  ','HT0CCO19  ',
     &'HT0CCO19  ','HT0CCO20  ','HT0CCO21  ','HT0CCO22  ',
     &'HT0CCO23  ','HT0HBC00  ','HT0HBC11  ','HT0HBC12  ',
     &'HT0HGC00  ','HT0HGC11  ','HT0HGC12  ','HT0HGC13  ',
     &'HT0HGC14  ','HT0HGC15  ','HT0HGC16  ','HT0HM000  ',
     &'HT0HM011  ','HT0HM012  ','HT0HM013  ','HT0HOB00  ',
     &'HT0HOB11  ','HT0HOL00  ','HT0HOL11  ','HT0HOL12  ',
     &'HT0HOL13  ','HT0HOL14  ','HT0HOL15  ','HT0HOL16  ',
     &'HT0SD000  ','HT0SD011  ','HT0SD012  ','HT0SEH00  ',
     &'HT0SEH11  ','HT0SEH12  ','HT0SEH13  ','HT0SM000  ',
     &'HT0SM011  ','HT0SOH00  ','HT0SOH11  ','HT0SSG00  ',
     &'HT0SSG11  ','HT0SSG12  ','HT0SSG13  ','JPIC      ',
     &'JPIC1     ','JPIC2     ','JPIC3     ','JPIC4     ',
     &'JPIC5     ','PC0611    ','POC       ','POC1      ',
     &'POC2      ','QS0111    ','RF        ','RF1       '/
      DATA (PVCODE(I),I= 601, NCODES)/
     &'SA000000  ','SA0SB000  ','SA0SBS00  ','SA0SCC00  ',
     &'SA0SCH00  ','SA0SCT00  ','SA0SCW00  ','SA0SMB00  ',
     &'SA0SME00  ','SB0SSW00  ','SBM00000  ','SCH00000  ',
     &'SCM00000  ','SCPSBS11  ','SMB00000  ','SME00000  ',
     &'SOC00000  ','SOI00000  ','SOISCL00  ','SOISOC00  ',
     &'SOISOS00  ','SOS00000  ','SOSSA000  ','SOSSBM00  ',
     &'SOSSCH00  ','SOSSCL00  ','SR000000  ','SR0SA000  ',
     &'SSC00000  ','SSCSB000  ','SSCSSB00  ','WC0911    ',
     &'WC0912    ','WC0913    ','WC0914    ','WC0915    ',
     &'WC0916    ','WC0917    ','WFDF      ','WFDF1     ',
     &'WFDF2     ','WFDF3     ','WFDF4     ','WFDF5     ',
     &'WFJP      ','WFJP1     ','WFJP2     ','WFJP3     ',
     &'WFWP      ','WFWP1     '/
C
      DATA (PVREF(I),I=   1,  60)/
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          '/
      DATA (PVREF(I),I=  61, 120)/
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       '/
      DATA (PVREF(I),I= 121, 180)/
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','501       '/
      DATA (PVREF(I),I= 181, 240)/
     &'501       ','501       ','501       ','501       ',
     &'501       ','501       ','501       ','502       ',
     &'510       ','510       ','510       ','512       ',
     &'510       ','512       ','510       ','512       ',
     &'510       ','510       ','510       ','510       ',
     &'510       ','510       ','512       ','510       ',
     &'512       ','510       ','512       ','510       ',
     &'510       ','510       ','510       ','512       ',
     &'510       ','512       ','510       ','510       ',
     &'513       ','513       ','512       ','513       ',
     &'513       ','512       ','513       ','513       ',
     &'513       ','513       ','512       ','513       ',
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','513       '/
      DATA (PVREF(I),I= 241, 300)/
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','512       ','513       ',
     &'513       ','513       ','513       ','507       ',
     &'512       ','513       ','512       ','513       ',
     &'512       ','513       ','506       ','506       ',
     &'506       ','506       ','506       ','506       ',
     &'506       ','506       ','506       ','506       ',
     &'506       ','506       ','506       ','506       ',
     &'506       ','507       ','514       ','507       ',
     &'514       ','507       ','514       ','507       ',
     &'514       ','507       ','514       ','507       ',
     &'514       ','507       ','514       ','510       ',
     &'510       ','512       ','512       ','512       '/
      DATA (PVREF(I),I= 301, 360)/
     &'512       ','512       ','512       ','510       ',
     &'512       ','512       ','512       ','506       ',
     &'506       ','506       ','506       ','502       ',
     &'504       ','512       ','512       ','512       ',
     &'502       ','504       ','504       ','502       ',
     &'504       ','502       ','504       ','502       ',
     &'504       ','502       ','504       ','502       ',
     &'504       ','502       ','504       ','502       ',
     &'504       ','502       ','504       ','502       ',
     &'504       ','502       ','504       ','502       ',
     &'504       ','502       ','504       ','502       ',
     &'504       ','502       ','504       ','502       ',
     &'504       ','502       ','504       ','502       ',
     &'504       ','502       ','504       ','502       ',
     &'504       ','502       ','504       ','502       '/
      DATA (PVREF(I),I= 361, 420)/
     &'504       ','506       ','506       ','506       ',
     &'506       ','502       ','504       ','502       ',
     &'504       ','504       ','502       ','504       ',
     &'502       ','504       ','502       ','504       ',
     &'502       ','504       ','502       ','504       ',
     &'502       ','504       ','502       ','504       ',
     &'502       ','504       ','502       ','504       ',
     &'502       ','504       ','502       ','504       ',
     &'502       ','504       ','502       ','504       ',
     &'502       ','504       ','502       ','504       ',
     &'502       ','504       ','502       ','504       ',
     &'502       ','504       ','502       ','504       ',
     &'504       ','507       ','512       ','512       ',
     &'512       ','512       ','512       ','512       ',
     &'512       ','512       ','512       ','512       '/
      DATA (PVREF(I),I= 421, 480)/
     &'512       ','512       ','512       ','512       ',
     &'509       ','509       ','509       ','509       ',
     &'509       ','509       ','509       ','509       ',
     &'509       ','509       ','509       ','509       ',
     &'509       ','509       ','509       ','509       ',
     &'509       ','509       ','509       ','509       ',
     &'509       ','509       ','509       ','509       ',
     &'509       ','509       ','509       ','509       ',
     &'509       ','509       ','509       ','509       ',
     &'509       ','509       ','509       ','509       ',
     &'509       ','509       ','509       ','509       ',
     &'502       ','502       ','502       ','502       ',
     &'503       ','503       ','503       ','503       ',
     &'503       ','503       ','503       ','503       ',
     &'503       ','503       ','503       ','503       '/
      DATA (PVREF(I),I= 481, 540)/
     &'503       ','503       ','503       ','503       ',
     &'503       ','503       ','502       ','506       ',
     &'512       ','507       ','515       ','507       ',
     &'515       ','507       ','515       ','507       ',
     &'515       ','507       ','515       ','507       ',
     &'515       ','507       ','515       ','507       ',
     &'515       ','507       ','515       ','507       ',
     &'515       ','507       ','515       ','507       ',
     &'515       ','507       ','515       ','507       ',
     &'515       ','507       ','515       ','507       ',
     &'515       ','506       ','506       ','513       ',
     &'513       ','513       ','513       ','510       ',
     &'513       ','510       ','513       ','510       ',
     &'513       ','510       ','513       ','510       ',
     &'513       ','510       ','513       ','510       '/
      DATA (PVREF(I),I= 541, 600)/
     &'513       ','510       ','513       ','510       ',
     &'513       ','510       ','510       ','510       ',
     &'510       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','513       ',
     &'513       ','513       ','513       ','503       ',
     &'503       ','503       ','503       ','503       ',
     &'503       ','502       ','503       ','503       ',
     &'503       ','502       ','503       ','503       '/
      DATA (PVREF(I),I= 601, NCODES)/
     &'511       ','511       ','511       ','511       ',
     &'511       ','511       ','511       ','511       ',
     &'511       ','511       ','511       ','511       ',
     &'511       ','512       ','511       ','511       ',
     &'511       ','511       ','511       ','511       ',
     &'511       ','511       ','511       ','511       ',
     &'511       ','511       ','511       ','511       ',
     &'511       ','511       ','511       ','502       ',
     &'502       ','502       ','502       ','502       ',
     &'502       ','502       ','503       ','503       ',
     &'503       ','503       ','503       ','503       ',
     &'503       ','503       ','503       ','503       ',
     &'503       ','503       '/
C----------
C  MAP PV/REFERENCE CODES INTO A FVS HABITAT/ECOCLASS CODE
C----------
      KODTYP=0
      KARD2T=KARD2
      KARD2='          '
      ARRAY2=0.
C
      DO I=1,NCODES
      IF((ADJUSTL(PVCODE(I)).EQ.ADJUSTL(KARD2T)).AND.(ADJUSTL(PVREF(I))
     &  .EQ.ADJUSTL(CPVREF)))THEN
        KARD2=HABPVR(I)
        LPVCOD=.TRUE.
        LPVREF=.TRUE.
        EXIT
      ENDIF
      IF(ADJUSTL(PVCODE(I)).EQ.ADJUSTL(KARD2T))LPVCOD=.TRUE.
      IF(ADJUSTL(PVREF(I)).EQ.ADJUSTL(CPVREF))LPVREF=.TRUE.
      ENDDO
C
      RETURN
      END