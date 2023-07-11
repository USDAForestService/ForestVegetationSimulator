# runs all the tests where the directory names start with FVS, ignoring errors.
# Targets are: all (All variants), Canada (Canada variants only), US (US variants only)

export TestDirs = $(shell ls -d FVS*)

CanDirs := FVSbc FVSon

USDirs := FVSak FVSbm FVSca FVSci FVScr FVScs FVSec FVSem FVSie FVSkt FVSls FVSnc FVSne FVSoc FVSop FVSpn FVSsn FVSso FVStt FVSut FVSwc FVSws

all : $(addsuffix .test,$(TestDirs))

Canada : $(addsuffix .test,$(CanDirs))

US : $(addsuffix .test,$(USDirs))

clean : $(addsuffix .clean,$(TestDirs))

clean_US : $(addsuffix .clean,$(USDirs))

%.test : 
	cd $(basename $@); ${MAKE} --ignore-errors

%.clean :
	cd $(basename $@); ${MAKE} clean