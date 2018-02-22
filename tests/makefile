
#	$Id$	

# runs all the tests where the directory names start with FVS, ignoring errors.

export TestDirs = $(shell ls -d FVS*)
export qTestDirs = $(shell ls -d qFVS*)

all : $(addsuffix .test,$(TestDirs))
FVSQLite : $(addsuffix .test,$(qTestDirs))	

export WINE

%.test : 
	cd $(basename $@); ${MAKE} --ignore-errors
	

