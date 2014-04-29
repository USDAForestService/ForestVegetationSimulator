
rem StdFVS run on DOS.
rem testing Stuart Johnston's C10 stand against his OSU Supplied 

echo test1.key >  test1.rsp
echo test1.fvs >> test1.rsp
echo test1.out >> test1.rsp
echo test1.trl >> test1.rsp
echo test1.sum >> test1.rsp
echo test1.chp >> test1.rsp

REM c:\fvs\or\FVSpnc_DS6\DEBUG\FVSpnc_DS6.exe < c10.rsp
rem FVSpnc_DS6.exe < c10.rsp

c:\fvs\bin\FVSpncor.exe < test1.rsp
