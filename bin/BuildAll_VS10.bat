echo off
rem Once SLN files have been made with 
rem    cmake -G"Visual Studio 10" .
rem The compilation of all versions can be automated...
rem 
setlocal enabledelayedexpansion
set VS="C:\Program Files\Microsoft Visual Studio 10.0\Common7\IDE\devenv.exe"
rem for %%V in (ak bcc bmc cac cic crc cs ecc emc iec ktc ls ncc ne pnc sn soc ttc utc wcc wsc) do (
for %%V in (bcc) do (
	echo %%V
	set Targ=FVS!Targ!%%V_CMakeDir\FVS.sln
	%VS% !Targ! /Build "Release" /Out "BuildAll_VS10.log"
	copy FVS%%V_CMakeDir\Release\*.dll .
	copy FVS%%V_CMakeDir\Release\*.exe .
	set Targ=
	)
