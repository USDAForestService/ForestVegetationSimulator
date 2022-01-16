
#	$Id$	

# runs all the tests where the directory names start with FVS, ignoring errors.

export TestDirs = $(shell ls -d FVS*)

all : $(addsuffix .test,$(TestDirs))

%.test : 
	cd $(basename $@); ${MAKE} --ignore-errors
	

