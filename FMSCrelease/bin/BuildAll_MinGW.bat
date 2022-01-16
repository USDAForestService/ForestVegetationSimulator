rem Once Cmake has been used to create all the FVS build files:
rem
rem    cmake -G"MinGW Makefiles" .
rem
rem this script will build (using MinGW make) all the FVS variants and 
rem then copy them into the /bin directory, where they can be tested.
rem
echo off
setlocal enabledelayedexpansion
for %%V in (ak bmc cac cic crc cs ecc emc iec ktc ls ncc ne pnc sn soc ttc utc wcc wsc) do (
	echo %%V
	set Targ=FVS%%V_CMakeDir
	cd !Targ!
	mingw32-make
	copy FVS%%V.exe ..
	copy *.dll ..
	cd ..
	set Targ=
	)
