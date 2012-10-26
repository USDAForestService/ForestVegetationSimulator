setlocal enabledelayedexpansion
set VS="C:\Program Files\Microsoft Visual Studio 10.0\Common7\IDE\devenv.exe"
for %%V in (ak bmc cac cic crc cs ecc emc iec ktc ls ncc ne pnc sn soc ttc utc wcc wsc) do (
	set Targ=FVS!Targ!%%V_CMakeDir\FVS.sln
	%VS% !Targ! /Build "Debug" /Out "BuildAll_VS10.log"
	set Targ=
	)
