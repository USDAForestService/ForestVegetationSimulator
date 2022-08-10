#	$Id$	

# Notes: If OS is defined, then then we are 64 bit windows, otherwise, we are Linix 
# x86_64 or mac osx x86_64. These two are automatically picked as OS is always defined on windows

ifdef OS
 export WIN = -DWINDOWS
 export SLIBSUFX = .dll
 export PIC =      # never use PIC on this OSARCH type
 export PRGSUFX = .exe 
 ifndef OSARCH
   export OSARCH = w64
 endif 
else
#setting WIN as done below is to enable correct processing of fofem (fire) c code.
 export WIN = -Dunix  
 export SLIBSUFX = .so
 export PRGSUFX = 
 ifdef OSTYPE
  ifeq ($(OSTYPE),linux)
    export OSARCH = l64
    export PIC = -fPIC  # always use PIC on this OSARCH type
  endif
  ifeq ($(OSTYPE),linux-gnu)
    export OSARCH = l64
    export PIC = -fPIC  # always use PIC on this OSARCH type
  endif
 else
  ifeq ($(shell uname),Linux)
    export OSARCH = l64
    export PIC = -fPIC
  endif
 endif
 ifndef OSARCH
   export OSARCH = osx
   export PIC =      # never use PIC on this OSARCH type
   $(info OSARCH: $(OSARCH))
 endif
endif                                                                               

	
# FC is the fortran compiler
# FFLAGS are pasted to fortran
# CC is the c compiler and used for c++ code
# CFLAGS are passed to C, of these CMPgcc is used by the fofem routines and WIN is used
# by fvsSQL.c
# LD is the linker

ifdef mingw64
 export FCprf = x86_64-w64-mingw32-
 export CCprf = x86_64-w64-mingw32-
else
 export FCprf = 
 export CCprf = 
endif

$(info OS: $(OS); OSTYPE: $(OSTYPE); WIN: $(WIN); OSARCH: $(OSARCH); SLIBSUFX: $(SLIBSUFX))
$(info PIC: $(PIC); PRGSUFX: $(PRGSUFX))
$(info mingw64: $(mingw64); FCprf: $(FCprf); CCprf: $(CCprf))

export CC = gcc
export FC = gfortran 
export FFLAGS = $(PIC) -g -cpp -DCMP${CC} -Wall -Wno-integer-division -ffpe-trap=invalid,zero,underflow,overflow,denormal -fbacktrace 
# Adding optimization (-O2) does not help execution speed much but slows compile time a lot
export CFLAGS = $(PIC) -DANSI -DCMP${CC} -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_LOAD_EXTENSION $(WIN) 
export LD = gfortran

ifndef sourceFileList
export FVSprgs = $(subst _sourceList.txt,,$(shell ls FVS*_sourceList.txt))
$(info FVSprgs: $(FVSprgs))
endif


# the following variables are not processed until sourceFileList is defined...and that happens
# when the first rule fires (the %.setup rule).
ifdef sourceFileList
 ifndef sourceList
  export sourceFileList
  export sourceList = $(shell cat $(sourceFileList))
  export includes = $(notdir $(filter %.F77 %.INC %.h,$(sourceList)))
  export mods = $(notdir $(filter %_mod.f, $(sourceList)))
  export mods_mods = $(addsuffix .mod,$(basename $(mods)))
  export source = $(notdir $(filter %.c %.f %.F %.for %.cpp, $(sourceList)))
  export object = $(addsuffix .o,$(basename $(source)))
 endif
endif

all : $(addsuffix $(SLIBSUFX),$(FVSprgs)) $(addsuffix $(PRGSUFX),$(FVSprgs))

clean :
	rm -frv *_buildDir $(addsuffix $(SLIBSUFX),$(FVSprgs)) $(addsuffix $(PRGSUFX),$(FVSprgs))

ifndef sourceFileList

FVSak$(SLIBSUFX) FVSak$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSak) $(shell cat $(addsuffix _sourceList.txt,FVSak))
	$(MAKE) --file=makefile FVSak.setup

FVSbc$(SLIBSUFX) FVSbc$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSbc) $(shell cat $(addsuffix _sourceList.txt,FVSbc))
	$(MAKE) --file=makefile FVSbc.setup

FVSbm$(SLIBSUFX) FVSbm$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSbm) $(shell cat $(addsuffix _sourceList.txt,FVSbm))
	$(MAKE) --file=makefile FVSbm.setup

FVSca$(SLIBSUFX) FVSca$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSca) $(shell cat $(addsuffix _sourceList.txt,FVSca))
	$(MAKE) --file=makefile FVSca.setup

FVSci$(SLIBSUFX) FVSci$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSci) $(shell cat $(addsuffix _sourceList.txt,FVSci))
	$(MAKE) --file=makefile FVSci.setup
	
FVScr$(SLIBSUFX) FVScr$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVScr) $(shell cat $(addsuffix _sourceList.txt,FVScr))
	$(MAKE) --file=makefile FVScr.setup

FVScs$(SLIBSUFX) FVScs$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVScs) $(shell cat $(addsuffix _sourceList.txt,FVScs))
	$(MAKE) --file=makefile FVScs.setup

FVSec$(SLIBSUFX) FVSec$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSec) $(shell cat $(addsuffix _sourceList.txt,FVSec))
	$(MAKE) --file=makefile FVSec.setup

FVSem$(SLIBSUFX) FVSem$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSem) $(shell cat $(addsuffix _sourceList.txt,FVSem))
	$(MAKE) --file=makefile FVSem.setup

FVSie$(SLIBSUFX) FVSie$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSie) $(shell cat $(addsuffix _sourceList.txt,FVSie))
	$(MAKE) --file=makefile FVSie.setup

FVSkt$(SLIBSUFX) FVSkt$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSkt) $(shell cat $(addsuffix _sourceList.txt,FVSkt))
	$(MAKE) --file=makefile FVSkt.setup

FVSls$(SLIBSUFX) FVSls$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSls) $(shell cat $(addsuffix _sourceList.txt,FVSls))
	$(MAKE) --file=makefile FVSls.setup

FVSnc$(SLIBSUFX) FVSnc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSnc) $(shell cat $(addsuffix _sourceList.txt,FVSnc))
	$(MAKE) --file=makefile FVSnc.setup
	
FVSne$(SLIBSUFX) FVSne$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSne) $(shell cat $(addsuffix _sourceList.txt,FVSne))
	$(MAKE) --file=makefile FVSne.setup

FVSoc$(SLIBSUFX) FVSoc$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSoc) $(shell cat $(addsuffix _sourceList.txt,FVSoc))
	$(MAKE) --file=makefile FVSoc.setup

FVSon$(SLIBSUFX) FVSon$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSon) $(shell cat $(addsuffix _sourceList.txt,FVSon))
	$(MAKE) --file=makefile FVSon.setup

FVSop$(SLIBSUFX) FVSop$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSop) $(shell cat $(addsuffix _sourceList.txt,FVSop))
	$(MAKE) --file=makefile FVSop.setup

FVSpn$(SLIBSUFX) FVSpn$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSpn) $(shell cat $(addsuffix _sourceList.txt,FVSpn))
	$(MAKE) --file=makefile FVSpn.setup

FVSsn$(SLIBSUFX) FVSsn$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSsn) $(shell cat $(addsuffix _sourceList.txt,FVSsn))
	$(MAKE) --file=makefile FVSsn.setup

FVSso$(SLIBSUFX) FVSso$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSso) $(shell cat $(addsuffix _sourceList.txt,FVSso))
	$(MAKE) --file=makefile FVSso.setup

FVStt$(SLIBSUFX) FVStt$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVStt) $(shell cat $(addsuffix _sourceList.txt,FVStt))
	$(MAKE) --file=makefile FVStt.setup

FVSut$(SLIBSUFX) FVSut$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSut) $(shell cat $(addsuffix _sourceList.txt,FVSut))
	$(MAKE) --file=makefile FVSut.setup

FVSwc$(SLIBSUFX) FVSwc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSwc) $(shell cat $(addsuffix _sourceList.txt,FVSwc))
	$(MAKE) --file=makefile FVSwc.setup

FVSws$(SLIBSUFX) FVSws$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSws) $(shell cat $(addsuffix _sourceList.txt,FVSws))
	$(MAKE) --file=makefile FVSws.setup
	

%.setup : 
	mkdir -pv $(basename $@)_buildDir
	cp -p `cat $(basename $@)_sourceList.txt` $(basename $@)_buildDir
	$(MAKE) --file=../makefile --directory=$(basename $@)_buildDir \
	               buildDir=$(basename $@)_buildDir \
                 sourceFileList=../$(basename $@)_sourceList.txt $(basename $@).prg
endif

.SUFFIXES :

.PRECIOUS : $(object) $(mods_mods) 
 
ifeq (${OSARCH},w64) 
%.prg : $(object) 
	$(FCprf)$(LD) -static -shared -Wl,--export-all-symbols -o $(basename ../$@)$(SLIBSUFX)  $(filter-out  main.o,$(object))
	$(FCprf)$(LD) -static -o $(basename ../$@)$(PRGSUFX) $(object)
else
%.prg : $(object) 
	$(LD) -shared -o $(basename ../$@)$(SLIBSUFX) $(filter-out main.o,$(object)) $(DBLINK) 
	$(LD)         -o $(basename ../$@)$(PRGSUFX)  $(object) $(DBLINK)    
endif

%.mod : %.f $(includes)
	$(FCprf)$(FC) $(FFLAGS) -c $<

%.o : %.f $(includes) $(mods_mods)
	$(FCprf)$(FC) $(FFLAGS) -c -o $@ $<

%.o : %.for $(includes) $(mods_mods)
	$(FCprf)$(FC) $(FFLAGS) -c -o $@ $<

%.o : %.F $(includes) $(mods_mods)
	$(FCprf)$(FC) $(FFLAGS) -c -o $@ $<

%.o : %.c 
	$(CCprf)$(CC) $(CFLAGS) -c -o $@ $<
	
%.o : %.cpp 
	$(CCprf)$(CC) $(CFLAGS) -c -o $@ $<
