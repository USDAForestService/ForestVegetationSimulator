REM
REM  This batch file runs all western variant models from the bin 
REM  directory and moves the output files to the .\output directory
REM
DEL .\output\*.OUT
REM
..\..\bin\FVSAK   <  AK.RSP
MOVE AK.out .\output
..\..\bin\FVSBMC   <  BM.RSP
MOVE BM.out .\output
..\..\bin\FVSCAC   <  CA.RSP
MOVE CA.out .\output
..\..\bin\FVSCIC   <  CI.RSP
MOVE CI.out .\output
..\..\bin\FVSCRC   <  CR.RSP
MOVE CR.out .\output
..\..\bin\FVSECC   <  EC.RSP
MOVE EC.out .\output
..\..\bin\FVSEMC   <  EM.RSP
MOVE EM.out .\output
..\..\bin\FVSIEC   <  IE.RSP
MOVE IE.out .\output
..\..\bin\FVSKTC   <  KT.RSP
MOVE KT.out .\output
..\..\bin\FVSNCC   <  NC.RSP
MOVE NC.out .\output
..\..\bin\FVSPNC   <  PN.RSP
MOVE PN.out .\output
..\..\bin\FVSSOC   <  SO.RSP
MOVE SO.out .\output
..\..\bin\FVSTTC   <  TT.RSP
MOVE TT.out .\output
..\..\bin\FVSUTC   <  UT.RSP
MOVE UT.out .\output
..\..\bin\FVSWCC   <  WC.RSP
MOVE WC.out .\output
..\..\bin\FVSWSC   <  WS.RSP
MOVE WS.out .\output
del *.tls
del *.chp