      SUBROUTINE FMPHOTOCODE (FotoRef, CFotoCode, Fotocode, Icall)
      IMPLICIT NONE
C----------
C  **FMPHOTOCODE--FIRE--DATE OF LAST REVISION:  01/11/07
C----------
C
C     TRANSLATES THE CHARACTER PHOTO SERIES PHOTO CODE INTO AN 
C     INTEGER SUBSCRIPT, FOTOCODE. 
C     FOTOREF - THE PHOTO SERIES REFERENCE NUMBER (1 - 32)
C                (SEE FMPHOTOVAL FOR CITATIONS)
C     CFOTOCODE - THE CHARACTER PHOTO CODE ASSOCIATED WITH THAT REFERENCE
C     FOTOCODE - THE INTEGER PHOTO CODE
C     ICALL = 1 MEANS THAT YOU ARE GIVING THE CHARACTER CODE AND WANT THE INTEGER CODE
C     ICALL = 0 MEANS THAT YOU ARE GIVING THE INTEGER CODE AND WANT THE CHARACTER CODE
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C----------
      CHARACTER*13 CFOTOCODE
      INTEGER FOTOCODE, FOTOREF, I, N, ICALL
      LOGICAL DEBUG
      CHARACTER*3  REF1COD(22), REF2COD(59), REF3COD(66), REF32COD(39)
      CHARACTER*6  REF5COD(17), REF16COD(41), REF21COD(25), 
     >             REF22COD(36)        
      CHARACTER*12 REF6COD(27)     
      CHARACTER*4  REF7COD(56), REF14COD(29), REF15COD(29),  
     >             REF17COD(35), REF18COD(43)     
      CHARACTER*10 REF8COD(86), REF29COD(16)    
      CHARACTER*11 REF9COD(26), REF23COD(26), REF30COD(16)      
      CHARACTER*8  REF11COD(26), REF20COD(26), REF13COD(42)   
      CHARACTER*13 REF12COD(90)                   
      CHARACTER*5  REF19COD(34), REF27COD(30), REF24COD(27)                        
      CHARACTER*7  REF25COD(14), REF26COD(16)               
      CHARACTER*9  REF28COD(30)          
      CHARACTER*2  REF31COD(10)    
C----------
      DATA REF1COD / '16 ', '15 ', '63 ', '65 ', '67 ', 
     &               '25 ', '66 ', '9A ', '4A ', '17A',
     &               '3A ', '13A', '6A ', '18A', '10A',
     &               '7A ', '19A', '11A', '5A ', '8A ', 
     &               '16A', '15A' /

      DATA REF2COD / '24 ', '18 ', '23 ', '33A', '30A',
     &              '32A', '17 ', '31A', '29A', '72 ', 
     &              '76 ', '69 ', '80 ', '70 ', '64 ',
     &              '79 ', '75 ', '73 ', '77 ', '84 ',
     &              '74 ', '78 ', '68 ', '31 ', '36A',
     &              '71 ', '14 ', '88 ', '13 ', '9  ',
     &              '89 ', '5  ', '29 ', '30 ', '56 ', 
     &              '33 ', '91 ', '7  ', '32 ', '8  ',
     &              '28 ', '43 ', '14A', '39A', '41A',
     &              '49 ', '28A', '27A', '12A', '37A',
     &              '42A', '38A', '86 ', '43A', '34 ',
     &              '40A', '42 ', '48 ', '95 '/  
     
      DATA REF3COD / '2  ', '26A', '1  ', '35A', '82 ',
     &              '85 ', '25A', '34A', '45A', '47A',
     &              '87 ', '83 ', '92 ', '53 ', '41 ',
     &              '49A', '98 ', '61 ', '60 ', '6  ',
     &              '55 ', '48A', '11 ', '46A', '27 ',
     &              '81 ', '45 ', '1A ', '40 ', '39 ',
     &              '35 ', '2A ', '26 ', '24A', '44 ', 
     &              '37 ', '21 ', '21A', '90 ', '54 ', 
     &              '36 ', '22A', '50 ', '58 ', '19 ', 
     &              '57 ', '96 ', '23A', '46 ', '97 ', 
     &              '20 ', '59 ', '44A', '93 ', '47 ', 
     &              '4  ', '22 ', '51 ', '62 ', '12 ', 
     &              '20A', '94 ', '10 ', '3  ', '38 ', 
     &              '52 '/  
     
      DATA REF5COD /'1WH1TH','2WH1TH','3WH1TH','4WH1TH','5WH1TH',
     &             '6WH1TH','1GF1TH','2GF1TH','3GF1TH','4GF1TH',
     &             '1WC1TH','2WC1TH','3WC1TH','4WC1TH','5WC1TH',
     &             '6WC1TH','7WC1TH'/
     
      DATA REF6COD / '1PP4CC      ', '2PP4CC      ', '1PP4PC      ',
     &              '2PP4PC      ', '3PP4PC      ', '4PP4PC      ',
     &              '5PP4PC      ', '1PP1TH      ', '2PP1TH      ',
     &              '3PP1TH      ', '4PP1TH      ', '5PP1TH      ',
     &              '6PP1TH      ', '1PP&ASSOC4PC', '2PP&ASSOC4PC', 
     &              '3PP&ASSOC4PC', '4PP&ASSOC4PC', '5PP&ASSOC4PC', 
     &              '6PP&ASSOC4PC', '7PP&ASSOC4PC', '8PP&ASSOC4PC', 
     &              '1LP3CC      ', '1LP3PC      ', '2LP3PC      ', 
     &              '3LP3PC      ', '4LP3PC      ', '5LP3PC      '/                      
     
      DATA REF7COD / '1MP4','2MP4','3MP4','4MP4','5MP4',
     &              '1MF4','2MF4','3MF4','4MF4','5MF4',
     &              '1PP2','2PP2','3PP2','4PP2','1PP3',
     &              '2PP3','3PP3','4PP3','1PP4','2PP4',
     &              '3PP4','1LP2','2LP2','3LP2','4LP2',
     &              '5LP2','1LP3','2LP3','3LP3','4LP3',
     &              '1LP4','1WF2','2WF2','3WF2','4WF2',
     &              '1WF3','2WF3','3WF3','4WF3','5WF3',
     &              '1WF4','2WF4','3WF4','4WF4','5WF4',
     &              '1RF3','2RF3','3RF3','4RF3','5RF3',
     &              '1RF4','2RF4','3RF4','4RF4','5RF4',
     &              '1MH4'/
     
      DATA REF8COD / '1DFHD3    ', '2DFHD3    ', '3DFHD3    ', 
     &               '1DFHD4    ', '2DFHD4    ', '3DFHD4    ',
     &               '4DFHD4    ', '5DFHD4    ', '1HD2      ',
     &               '2HD2      ', '1DF2      ', '2DF2      ', 
     &               '1DF3      ', '2DF3      ', '1DF4      ', 
     &               '2DF4      ', '3DF4      ', '4DF4      ', 
     &               '5DF4      ', '6DF4      ', '7DF4      ', 
     &               '1SA1      ', '2SA1      ', '3SA1      ', 
     &               '1SA2      ', '2SA2      ', '1SA3      ', 
     &               '2SA3      ', '3SA3      ', '1SA4      ', 
     &               '2SA4      ', '1MC2      ', '2MC2      ', 
     &               '3MC2      ', '1MC3      ', '2MC3      ', 
     &               '3MC3      ', '1MC4      ', '2MC4      ', 
     &               '1LP1      ', '2LP1      ', '3LP1      ', 
     &               '1LP2      ', '2LP2      ', '3LP2      ', 
     &               '4LP2      ', '1LP3      ', '2LP3      ', 
     &               '3LP3      ', '1PP&ASSOC3', '2PP&ASSOC3', 
     &               '3PP&ASSOC3', '4PP&ASSOC3', '5PP&ASSOC3', 
     &               '1PP&ASSOC4', '2PP&ASSOC4', '3PP&ASSOC4', 
     &               '1PP1      ', '2PP1      ', '3PP1      ', 
     &               '1PP2      ', '2PP2      ', '3PP2      ', 
     &               '4PP2      ', '1PP3      ', '2PP3      ', 
     &               '3PP3      ', '4PP3      ', '5PP3      ', 
     &               '6PP3      ', '7PP3      ', '8PP3      ', 
     &               '1PP4      ', '2PP4      ', '3PP4      ', 
     &               '4PP4      ', '5PP4      ', '6PP4      ', 
     &               '7PP4      ', '8PP4      ', '1BR       ', 
     &               '2BR       ', '1JU2      ', '2JU2      ', 
     &               '1GR       ', '2GR       '/                              
     
      DATA REF9COD / '1DFWHPRE01 ', '1DFWHPRE02 ', '1DFWHPRE03 ', 
     &              '1DFWHPRE04 ', '1DFWHPRE05 ', '1DFWHPRE06 ', 
     &              '1DFWHPRE07 ', '1DFWHPRE08 ', '1DFWHPRE09 ', 
     &              '2WHSSPRE01 ', '3RAPRE01   ', '3RAPRE02   ', 
     &              '3RAPRE03   ', '3RAPRE04   ', '3RAPRE05   ', 
     &              '3RAPRE06   ', '3RAPRE07   ', '4DFWHPOST01', 
     &              '4DFWHPOST02', '4DFWHPOST03', '4DFWHPOST04', 
     &              '5RAPOST01  ', '5RAPOST02  ', '5RAPOST03  ', 
     &              '5RAPOST04  ', '5RAPOST05  '/  
      
      DATA REF11COD / '1PP1TH  ', '2PP1TH  ', '3PP1TH  ', '4PP1TH  ', 
     &               '5PP1TH  ', '6PP1TH  ', '7PP1TH  ', '1PP2PC  ', 
     &               '2PP2PC  ', '3PP2PC  ', '4PP2PC  ', '5PP2PC  ', 
     &               '1PP3PC  ', '2PP3PC  ', '3PP3PC  ', '1PP3CC  ', 
     &               '2PP3CC  ', '1PPSP3PC', '2PPSP3PC', '3PPSP3PC', 
     &               '1SP3PC  ', '2SP3PC  ', '1PP1    ', '1PP2    ', 
     &               '2PP2    ', '1PP3    '/    
     
      DATA REF12COD /'1PP1TH(BH)   ', '2PP1TH(BH)   ', '3PP1TH(BH)   ', 
     &               '4PP1TH(BH)   ', '5PP1TH(BH)   ', '6PP1TH(BH)   ', 
     &               '7PP1TH(BH)   ', '1PP2PC       ', '2PP2PC       ', 
     &               '3PP2PC       ', '4PP2PC       ', '5PP2PC       ', 
     &               '1PP3PC       ', '2PP3PC       ', '3PP3PC       ', 
     &               '1PP3CC       ', '2PP3CC       ', '1PPSP3PC     ', 
     &               '2PPSP3PC     ', '3PPSP3PC     ', '1SP3PC       ', 
     &               '2SP3PC       ', '1PP1(BH)     ', '1PP2(BH)     ', 
     &               '2PP2(BH)     ', '1PP3(BH)     ', '1AZPPSPPRE01 ', 
     &               '1AZPPSPPRE02 ', '1AZPPSPPRE03 ', '1AZPPSPPRE04 ', 
     &               '1MC2         ', '2MC2         ', '3MC2         ', 
     &               '1MC3         ', '2MC3         ', '3MC3         ', 
     &               '1PP&ASSOC3   ', '2PP&ASSOC3   ', '3PP&ASSOC3   ',  
     &               '4PP&ASSOC3   ', '5PP&ASSOC3   ', '1PP&ASSOC4   ', 
     &               '2PP&ASSOC4   ', '3PP&ASSOC4   ', '1PP1         ', 
     &               '2PP1         ', '3PP1         ', '1PP2(PNW-105)', 
     &               '2PP2(PNW-105)', '3PP2(PNW-105)', '4PP2(PNW-105)', 
     &               '1PP3(PNW-105)', '2PP3(PNW-105)', '3PP3(PNW-105)', 
     &               '4PP3(PNW-105)', '5PP3         ', '6PP3         ', 
     &               '7PP3         ', '8PP3         ', '1PP4(PNW-105)', 
     &               '2PP4(PNW-105)', '3PP4(PNW-105)', '4PP4         ', 
     &               '1JU2         ', '2JU2         ', '1PP4PC       ', 
     &               '2PP4PC       ', '3PP4PC       ', '4PP4PC       ', 
     &               '5PP4PC       ', '1PP1TH       ', '2PP1TH       ', 
     &               '3PP1TH       ', '4PP1TH       ', '5PP1TH       ', 
     &               '6PP1TH       ', '1PP2         ', '2PP2         ', 
     &               '3PP2         ', '4PP2         ', '1PP3         ', 
     &               '2PP3         ', '3PP3         ', '4PP3         ', 
     &               '1PP4         ', '2PP4         ', '3PP4         ', 
     &               '3WF2         ', '4WF3         ', '3WF3         '/          
     
      DATA REF13COD / '1DF4CC  ', '2DF4CC  ', '3DF4CC  ', '4DF4CC  ', 
     &               '5DF4CC  ', '6DF4CC  ', '7DF4CC  ', '8DF4CC  ', 
     &               '9DF4CC  ', '10DF4CC ', '1DF4PC  ', '2DF4PC  ', 
     &               '3DF4PC  ', '4DF4PC  ', '5DF4PC  ', '6DF4PC  ', 
     &               '7DF4PC  ', '8DF4PC  ', '9DF4PC  ', '1DF3PC  ', 
     &               '2DF3PC  ', '3DF3PC  ', '4DF3PC  ', '5DF3PC  ', 
     &               '6DF3PC  ', '1DF1TH  ', '2DF1TH  ', '3DF1TH  ', 
     &               '4DF1TH  ', '1DFHD4CC', '2DFHD4CC', '3DFHD4CC', 
     &               '4DFHD4CC', '5DFHD4CC', '6DFHD4CC', '7DFHD4CC', 
     &               '1DFHD4PC', '2DFHD4PC', '3DFHD4PC', '4DFHD4PC', 
     &               '5DFHD4PC', '6DFHD4PC'/
      
      DATA REF14COD /'BG01','BG02','BG03','BG04','MC01',
     &              'MC02','MC03','MC04','MC05','MC06',
     &              'MC07','MC08','MC09','MC10','MC11',
     &              'MC12','MC13','MC14','MC15','MC16',
     &              'MC17','SB01','SB02','SB03','SB04',
     &              'WJ01','WJ02','WJ03','WJ04'/        
     
      DATA REF15COD /'BG01','BG02','BG03','BG04','MC01',
     &              'MC02','MC03','MC04','MC05','MC06',
     &              'MC07','MC08','MC09','MC10','MC11',
     &              'MC12','MC13','MC14','MC15','MC16',
     &              'MC17','SB01','SB02','SB03','SB04',
     &              'WJ01','WJ02','WJ03','WJ04'/
     
      DATA REF16COD / 'AKHD01','AKHD02','AKHD03','AKHD04','AKHD05',
     &               'AKHD06','AKHD07','AKHD08','AKHD09','AKHD10',
     &               'AKHD11','AKHD12','AKHD13','AKHD14','AKHD15',
     &               'BS01  ','BS02  ','BS03  ','BS04  ','BS05  ',
     &               'BS06  ','BS07  ','BS08  ','BS09  ','BS10  ',
     &               'BS11  ','BS12  ','BS13  ','BS14  ','WS01  ',
     &               'WS02  ','WS03  ','WS04  ','WS05  ','WS06  ',
     &               'WS07  ','WS08  ','WS09  ','WS10  ','WS11  ',
     &               'WS12  '/
                                                
      DATA REF17COD / 'GO01','GO02','GO03','GO04','GO05',
     &               'GO06','GO07','GO08','GO09','LP01',
     &               'LP02','LP03','LP04','LP05','LP06',
     &               'LP07','LP08','LP09','LP10','LP11',
     &               'LP12','LP13','QA01','QA02','QA03',
     &               'QA04','QA05','QA06','QA07','QA08',
     &               'QA09','QA10','QA11','QA12','QA13'/
     
     
      DATA REF18COD / 'JP01', 'JP02', 'JP03', 'JP04', 'JP05', 
     &               'JP06', 'JP07', 'JP08', 'JP09', 'JP10', 
     &               'JP11', 'JP12', 'JP13', 'JP14', 'JP15', 
     &               'JP16', 'JP17', 'JP18', 'JP19', 'MO01', 
     &               'MO02', 'MO03', 'MO04', 'MO05', 'MO06', 
     &               'MO07', 'MO08', 'MO09', 'MO10', 'MO11', 
     &               'MP01', 'MP02', 'MP03', 'MP04', 'MP05', 
     &               'MP06', 'MP07', 'MP08', 'MP09', 'MP10', 
     &               'MP11', 'MP12', 'MP13'/ 
                  
     
      DATA REF19COD /'HP01 ', 'HP02 ', 'HP03 ', 'HP04 ', 'HP05 ', 
     &               'HP06 ', 'HP07 ', 'LLP01', 'LLP02', 'LLP03', 
     &               'LLP04', 'LLP05', 'LLP06', 'LLP07', 'LLP08', 
     &               'LLP09', 'LLP10', 'P-W01', 'P-W02', 'SH01 ', 
     &               'SH02 ', 'SH03 ', 'SH04 ', 'SH05 ', 'SH06 ', 
     &               'SH07 ', 'SH08 ', 'SH09 ', 'SH10 ', 'SH11 ', 
     &               'SPS01', 'SPS02', 'SPS03', 'SPS04'/         
     
      DATA REF20COD /'1PP1TH  ', '2PP1TH  ', '3PP1TH  ', '4PP1TH  ', 
     &               '5PP1TH  ', '6PP1TH  ', '7PP1TH  ', '1PP2PC  ', 
     &               '2PP2PC  ', '3PP2PC  ', '4PP2PC  ', '5PP2PC  ', 
     &               '1PP3PC  ', '2PP3PC  ', '3PP3PC  ', '1PP3CC  ', 
     &               '2PP3CC  ', '1PPSP3PC', '2PPSP3PC', '3PPSP3PC', 
     &               '1SP3PC  ', '2SP3PC  ', '1PP1    ', '1PP2    ', 
     &               '2PP2    ', '1PP3    '/      
     
      DATA REF21COD /'PJ01  ', 'PJ02  ', 'PJ03  ', 'PJ04  ', 'PJ05  ', 
     &               'PJ06  ', 'PJ07  ', 'PJ08  ', 'PJ09  ', 'PJ10  ', 
     &               'PJ11  ', 'PJ12  ', 'PJ13  ', 'PJ14  ', 'SWSB01', 
     &               'SWSB02', 'SWSB03', 'SWSB04', 'SWSB05', 'SWSB06', 
     &               'SWSB07', 'SWSB08', 'SWSB09', 'SWSB10', 'SWSB11'/
     
      DATA REF22COD /'HI-F01', 'HI-F02', 'HI-F03', 'HI-F04', 'HI-F05', 
     &               'HI-F06', 'HI-F07', 'HI-F08', 'HI-F09', 'HI-G01', 
     &               'HI-G02', 'HI-G03', 'HI-G04', 'HI-G05', 'HI-G06', 
     &               'HI-G07', 'HI-G08', 'HI-G09', 'HI-G10', 'HI-G11', 
     &               'HI-G12', 'HI-G13', 'HI-S01', 'HI-S02', 'HI-S03', 
     &               'HI-S04', 'HI-S05', 'HI-S06', 'HI-S07', 'HI-W01', 
     &               'HI-W02', 'HI-W03', 'HI-W04', 'HI-W05', 'HI-W06', 
     &               'HI-W07'/
     
      DATA REF23COD / '1DFWHPRE01 ', '1DFWHPRE02 ', '1DFWHPRE03 ', 
     &               '1DFWHPRE04 ', '1DFWHPRE05 ', '1DFWHPRE06 ', 
     &               '1DFWHPRE07 ', '1DFWHPRE08 ', '1DFWHPRE09 ', 
     &               '1DFWHPRE10 ', '1DFWHPRE11 ', '1DFWHPRE12 ', 
     &               '1DFWHPRE13 ', '1DFWHPRE14 ', '1DFWHPRE15 ', 
     &               '1DFWHPRE16 ', '1DFWHPRE17 ', '1DFWHPRE18 ', 
     &               '1DFWHPRE19 ', '2DFWHPOST01', '2DFWHPOST02', 
     &               '2DFWHPOST03', '2DFWHPOST04', '2DFWHPOST05', 
     &               '2DFWHPOST06', '2DFWHPOST07'/
               
      DATA REF24COD / '1LL2N', '2LL2H', '3LL3N', '4LL2H', '5LL1P', 
     &               '6LL3H', '7LL3H', '8LL3N', '9LL3H', '1WP3N', 
     &               '2WP2P', '3WP3N', '4WP3H', '5WP3H', '6WP2H', 
     &               '7WP3N', '1PP1N', '2PP2N', '3PP1N', '4PP1N', 
     &               '5PP2N', '6PP2N', '7PP3H', '1VP2N', '2VP2N', 
     &               '3VP3N', '4VP2N' /
                 
      DATA REF25COD /'1A21N  ', '2A22N  ', '3B21N  ', '4A22N  ', 
     &               '5B12N  ', '6A12N  ', '7B22N  ', '8A22N  ', 
     &               '9A11N  ', '10A22CC', '11B22CC', '12A22CC', 
     &               '13A22CC', '14B23CC'/ 
     
      DATA REF26COD /'FC1PRE ', 'FC1POST', 'FC2PRE ', 'FC2POST',
     &               'FC3PRE ', 'FC3POST', 'FC4PRE ', 'FC4POST', 
     &               'FC5PRE ', 'FC5POST', 'FC6PRE ', 'FC6POST', 
     &               'FC7PRE ', 'FC7POST', 'FC8PRE ', 'FC8POST'/ 
      
      DATA REF27COD / 'CDO01', 'CDO02', 'CDO03', 'CDO04', 'CDO05', 
     &               'CDO06', 'CDO07', 'CDO08', 'CDO09', 'MCS01', 
     &               'MCS02', 'MCS03', 'MCS04', 'MCS05', 'MCS06', 
     &               'MCS07', 'MCS08', 'MCS09', 'MCS10', 'MCS11', 
     &               'WO01 ', 'WO02 ', 'WO03 ', 'WO04 ', 'WO05 ', 
     &               'WO06 ', 'WO07 ', 'WO08 ', 'WO09 ', 'WO10 '/ 
     
      DATA REF28COD / '1-MC-4-RC', '2-MC-4-RC', '3-MC-4-RC', 
     &               '1-MC-4-PC', '2-MC-4-PC', '3-MC-4-PC', 
     &               '4-MC-4-PC', '5-MC-4-PC', '6-MC-4-PC', 
     &               '7-MC-4-PC', '8-MC-4-PC', '1-MC-3-PC', 
     &               '2-MC-3-PC', '3-MC-3-PC', '4-MC-3-PC', 
     &               '5-MC-3-PC', '6-MC-3-PC', '7-MC-3-PC', 
     &               '8-MC-3-PC', '1-TF-4-RC', '2-TF-4-RC', 
     &               '3-TF-4-RC', '4-TF-4-RC', '5-TF-4-RC', 
     &               '6-TF-4-RC', '1-TF-4-PC', '2-TF-4-RC', 
     &               '3-TF-4-PC', '4-TF-4-PC', '5-TF-4-PC'/
                 
      DATA REF29COD /'1-PREBURN ', '1-POSTBURN', '2-PREBURN ', 
     &               '2-POSTBURN', '3-PREBURN ', '3-POSTBURN', 
     &               '4-PREBURN ', '4-POSTBURN', '5-PREBURN ', 
     &               '5-POSTBURN', '6-PREBURN ', '6-POSTBURN', 
     &               '7-PREBURN ', '7-POSTBURN', '8-PREBURN ', 
     &               '8-POSTBURN' /
                 
      DATA REF30COD / '3D-PREBURN ', '3D-POSTBURN', '2A-PREBURN ', 
     &               '2A-POSTBURN', '3B-PREBURN ', '3B-POSTBURN', 
     &               '2C-PREBURN ', '2C-POSTBURN', '2D-PREBURN ', 
     &               '2D-POSTBURN', '1A-PREBURN ', '1A-POSTBURN', 
     &               '1C-PREBURN ', '1C-POSTBURN', '1D-PREBURN ', 
     &               '1D-POSTBURN'/
                 
      DATA REF31COD / '1 ', '2 ', '3 ', '4 ', '5 ', 
     &               '6 ', '7 ', '8 ', '9 ', '10'/
                 
      DATA REF32COD /'1A ', '1B ', '1C ', '2A ', '2B ', '3A ', '3B ', 
     &               '4A ', '4B ', '4C ', '5A ', '5B ', '5C ', '6A ', 
     &               '6B ', '6C ', '7A ', '7B ', '7C ', '8A ', '9A ', 
     &               '9B ', '10A', '10B', '10C', '11A', '12A', '12B', 
     &               '12C', '13A', '13B', '13C', '13D', '13E', '14A', 
     &               '14B', '14C', '14D', '14E'/
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMPHOTOCODE',11,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING FMPHOTOCODE CYCLE = ',I2)
      
      IF (ICALL .EQ. 0) GO TO 70
C----------
C  DECODE FUELS PHOTO CODE FIELD BASED ON THE PHOTO REFERENCE NUMBER
C----------
      FOTOCODE = -1
      
      DO I=1,13
        CALL UPCASE(CFOTOCODE(I:I))
      ENDDO
      
      SELECT CASE (FOTOREF)
      CASE (1)
        N = 22
        DO I = 1, N
          IF (CFOTOCODE(1:3) .EQ. REF1COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
            
      CASE (2)
        N = 59
        DO I = 1, N
          IF (CFOTOCODE(1:3) .EQ. REF2COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
              
      CASE (3)      
        N = 66
        DO I = 1, N
          IF (CFOTOCODE(1:3) .EQ. REF3COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
        
      CASE (5)        
        N = 17
        DO I = 1, N
          IF (CFOTOCODE(1:6) .EQ. REF5COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
              
      CASE (6)     
        N = 27
        DO I = 1, N
          IF (CFOTOCODE(1:12) .EQ. REF6COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  

      CASE (7)     
        N = 56
        DO I = 1, N
          IF (CFOTOCODE(1:4) .EQ. REF7COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  

      CASE (8)     
        N = 86
        DO I = 1, N
          IF (CFOTOCODE(1:10) .EQ. REF8COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
        
      CASE (9)     
        N = 26
        DO I = 1, N
          IF (CFOTOCODE(1:11) .EQ. REF9COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
        
      CASE (11) 
        N = 26
        DO I = 1, N
          IF (CFOTOCODE(1:8) .EQ. REF11COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
                       
      CASE (12) 
        N = 90
        DO I = 1, N
          IF (CFOTOCODE(1:13) .EQ. REF12COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
                       
      CASE (13) 
        N = 42
        DO I = 1, N
          IF (CFOTOCODE(1:8) .EQ. REF13COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
                       
      CASE (14) 
        N = 29
        DO I = 1, N
          IF (CFOTOCODE(1:4) .EQ. REF14COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO   
                      
      CASE (15) 
        N = 29
        DO I = 1, N
          IF (CFOTOCODE(1:4) .EQ. REF15COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
                       
      CASE (16) 
        N = 41
        DO I = 1, N
          IF (CFOTOCODE(1:6) .EQ. REF16COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO 
                       
      CASE (17) 
        N = 35
        DO I = 1, N
          IF (CFOTOCODE(1:4) .EQ. REF17COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO 
                       
      CASE (18) 
        N = 43
        DO I = 1, N
          IF (CFOTOCODE(1:4) .EQ. REF18COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO 
                        
      CASE (19) 
        N = 34
        DO I = 1, N
          IF (CFOTOCODE(1:5) .EQ. REF19COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO 
                        
      CASE (20)
        N = 26
        DO I = 1, N
          IF (CFOTOCODE(1:8) .EQ. REF20COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
        
      CASE (21) 
        N = 25
        DO I = 1, N
          IF (CFOTOCODE(1:6) .EQ. REF21COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
               
      CASE (22) 
        N = 36
        DO I = 1, N
          IF (CFOTOCODE(1:6) .EQ. REF22COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO 
                       
      CASE (23) 
        N = 26
        DO I = 1, N
          IF (CFOTOCODE(1:11) .EQ. REF23COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO 
                       
      CASE (24) 
        N = 27
        DO I = 1, N
          IF (CFOTOCODE(1:5) .EQ. REF24COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO 
                       
      CASE (25) 
        N = 14
        DO I = 1, N
          IF (CFOTOCODE(1:7) .EQ. REF25COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO    
                    
      CASE (26) 
        N = 16
        DO I = 1, N
          IF (CFOTOCODE(1:7) .EQ. REF26COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
                      
      CASE (27) 
        N = 30
        DO I = 1, N
          IF (CFOTOCODE(1:5) .EQ. REF27COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO   
                     
      CASE (28) 
        N = 30
        DO I = 1, N
          IF (CFOTOCODE(1:9) .EQ. REF28COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO       
                     
      CASE (29) 
        N = 16
        DO I = 1, N
          IF (CFOTOCODE(1:10) .EQ. REF29COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
                       
      CASE (30)
        N = 16
        DO I = 1, N
          IF (CFOTOCODE(1:11) .EQ. REF30COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
        
      CASE (31)
        N = 10
        DO I = 1, N
          IF (CFOTOCODE(1:2) .EQ. REF31COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO  
              
      CASE (32)
        N = 39
        DO I = 1, N
          IF (CFOTOCODE(1:3) .EQ. REF32COD(I)) THEN
            FOTOCODE = I
            GO TO 50
          ENDIF
        ENDDO                                                                                
      
      END SELECT
            
   50 CONTINUE

      IF (DEBUG) WRITE(JOSTND,58) FOTOREF,FOTOCODE,CFOTOCODE
   58 FORMAT(' IN FMPHOTOCODE, FOTOREF=',I4,' FOTOCODE=',I4,
     &       ' CFOTOCODE=',A)
  
      IF (FOTOCODE .EQ. -1) THEN
        WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',
     &  'PHOTO REFERENCE OR PHOTO CODE ENTERED.',/1X)")
        CALL RCDSET (2,.TRUE.)
      ENDIF
      RETURN
      
   70 CONTINUE
   
      SELECT CASE (FOTOREF)
      CASE (1)
      CFOTOCODE = REF1COD(FOTOCODE)            

      CASE (2)
      CFOTOCODE = REF2COD(FOTOCODE)
                    
      CASE (3)      
      CFOTOCODE = REF3COD(FOTOCODE)
               
      CASE (5)        
      CFOTOCODE = REF5COD(FOTOCODE)
                      
      CASE (6)     
      CFOTOCODE = REF6COD(FOTOCODE)
        
      CASE (7)     
      CFOTOCODE = REF7COD(FOTOCODE)
        
      CASE (8)     
      CFOTOCODE = REF8COD(FOTOCODE)
        
      CASE (9)     
      CFOTOCODE = REF9COD(FOTOCODE)
        
      CASE (11) 
      CFOTOCODE = REF11COD(FOTOCODE)
                      
      CASE (12) 
      CFOTOCODE = REF12COD(FOTOCODE)
                            
      CASE (13) 
      CFOTOCODE = REF13COD(FOTOCODE)
                          
      CASE (14) 
      CFOTOCODE = REF14COD(FOTOCODE)
                            
      CASE (15) 
      CFOTOCODE = REF15COD(FOTOCODE)
                           
      CASE (16) 
      CFOTOCODE = REF16COD(FOTOCODE)
                      
      CASE (17) 
      CFOTOCODE = REF17COD(FOTOCODE)
                         
      CASE (18) 
      CFOTOCODE = REF18COD(FOTOCODE)
                        
      CASE (19) 
      CFOTOCODE = REF19COD(FOTOCODE)
                     
      CASE (20)
      CFOTOCODE = REF20COD(FOTOCODE)
        
      CASE (21) 
      CFOTOCODE = REF21COD(FOTOCODE)
          
      CASE (22) 
      CFOTOCODE = REF22COD(FOTOCODE)
                 
      CASE (23) 
      CFOTOCODE = REF23COD(FOTOCODE)
              
      CASE (24) 
      CFOTOCODE = REF24COD(FOTOCODE)
                               
      CASE (25)   
      CFOTOCODE = REF25COD(FOTOCODE)
                            
      CASE (26) 
      CFOTOCODE = REF26COD(FOTOCODE)
                    
      CASE (27) 
      CFOTOCODE = REF27COD(FOTOCODE)
                             
      CASE (28) 
      CFOTOCODE = REF28COD(FOTOCODE)
          
      CASE (29) 
      CFOTOCODE = REF29COD(FOTOCODE)
                
      CASE (30)
      CFOTOCODE = REF30COD(FOTOCODE)
                
      CASE (31)
      CFOTOCODE = REF31COD(FOTOCODE)
                      
      CASE (32)                                                                           
      CFOTOCODE = REF32COD(FOTOCODE)
              
      END SELECT

      IF (DEBUG) WRITE(JOSTND,58) FOTOREF,FOTOCODE,CFOTOCODE
   
      RETURN
      END



                                         


            
