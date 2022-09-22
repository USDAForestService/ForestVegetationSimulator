
# runs all the tests where the directory names start with FVS, ignoring errors.

export TestDirs = $(shell ls -d FVS*)

all : $(addsuffix .test,$(TestDirs))

clean : $(addsuffix .clean,$(TestDirs))

%.test : 
	cd $(basename $@); ${MAKE} --ignore-errors

%.clean :
	cd $(basename $@); ${MAKE} clean