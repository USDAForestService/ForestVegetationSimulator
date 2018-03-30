#	$Id$	

# Notes: If OS is defined, then then we are 32 bit windows, otherwise, we are Linix 
# x86_64 or mac osx x86_64. 
# Key switches/flags:
# OSARCH is used to select the ODBC type defs in DBSTYPEDEFS.F77 and in selecting the 
#  ultimate build command. Values are: 
#     "w32" on Windows x86 (32 bit), 
#     "osx" on MAC x86_64, and 
#     "l64" on generic Linux x86_64.
#  When qFVS programs are built, the ODBC type defs and libraries are not used
# To build 64 bit windows, set OSARCH=w64 and OS=OS on the make command line.
# To build using cross compilers, define mingw64 on the command line

ifdef OS
 export WIN = -DWINDOWS
 export SLIBSUFX = .dll
 export PIC =      # never use PIC on this OSARCH type
 export PRGSUFX = .exe 
 ifndef OSARCH
   export OSARCH = w32
 endif 
 ifeq ($(OSARCH), w32)
   export ODBC = -lodbc32
 else
   export ODBC = -lodbc
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
   export ODBC = -liodbc
   $(info OSARCH: $(OSARCH); ODBC: $(ODBC))
 endif
 ifndef ODBC
  export ODBC = -lodbc
 endif
endif                                                                               

	
# FC is the fortran compiler
# FFLAGS are pasted to fortran
# CC is the c compiler
# CFLAGS are pased to C, of these CMPgcc is used by the fofem routines and WIN is used
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
$(info ODBC: $(ODBC); PIC: $(PIC); PRGSUFX: $(PRGSUFX))
$(info mingw64: $(mingw64))
$(info FCprf: $(FCprf))
$(info CCprf: $(CCprf))

export FC = gfortran 
#export FFLAGS = $(PIC) -g -Wall -fbacktrace 
#export FFLAGS = $(PIC) -g -Wall -fbacktrace -ffpe-trap=invalid,zero,denormal -fcheck=bounds
#export FFLAGS = $(PIC) -g -Wall -ffpe-trap=invalid,zero -fbacktrace
#export FFLAGS = $(PIC) -g -Wall -ffpe-trap=invalid,zero,underflow,overflow -fbacktrace -fcheck=all -fcheck=bounds
#export FFLAGS = $(PIC) -g -Wall -ffpe-trap=invalid,zero,underflow,overflow -fcheck=bounds -fbacktrace 
export FFLAGS = $(PIC) -g -Wall -Wno-integer-division -ffpe-trap=invalid,zero,underflow,overflow -fbacktrace 
export CC = gcc
# Adding optimization (-O2 does not help execution speed much but slows compile time a lot
# export CFLAGS = $(PIC) -DANSI -DCMP${CC} $(WIN) -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_LOAD_EXTENSION -O2
export CFLAGS = $(PIC) -DANSI -DCMP${CC} -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_LOAD_EXTENSION $(WIN) 
export LD = gfortran
ifndef sourceFileList
# TODO: See comment below regarding generl rules. We can change to this rule for all
# when we adopt a newer make on windows builds
export FVSprgs  = $(subst _sourceList.txt,,$(shell ls FVS*_sourceList.txt))
export qFVSprgs = $(subst _sourceList.txt,,$(shell ls qFVS*_sourceList.txt))
#  export FVSprgs = FVSbmc FVScrc FVSecc FVSiec FVSne FVSpnc FVSsn FVSwcc FVSwsc
endif

# the following variables are not processed until sourceFileList is defined...and that happens
# when the first rule fires (the %.setup rule).
ifdef sourceFileList
 ifndef sourceList
  export sourceFileList
  export sourceList = $(shell cat $(sourceFileList))
  ifeq (mkdbsTypeDefs,$(findstring mkdbsTypeDefs,$(sourceList)))
   export dbstypedefs = DBSTYPEDEFS.F77
   export DBLINK = $(ODBC)
  else
   export DBLINK = 
  endif
  $(info DBLINK: $(DBLINK))
  export includes = $(notdir $(filter %.F77 %.INC %.h,$(sourceList))) $(dbstypedefs)
  export mods = $(notdir $(filter %_mod.f, $(sourceList)))
  export mods_mods = $(addsuffix .mod,$(basename $(mods)))
  export source = $(filter-out mkdbsTypeDefs.c, $(notdir $(filter %.c %.f, $(sourceList))))
  export object = $(addsuffix .o,$(basename $(source)))
 endif
endif

FVSQLite : $(addsuffix $(PRGSUFX),$(qFVSprgs))

all : $(addsuffix $(SLIBSUFX),$(FVSprgs)) $(addsuffix $(PRGSUFX),$(FVSprgs)) $(addsuffix $(PRGSUFX),$(qFVSprgs))

clean :
	rm -frv *_buildDir libfvsSQL.dll $(addsuffix $(SLIBSUFX),$(FVSprgs)) $(addsuffix $(PRGSUFX),$(FVSprgs)) $(addsuffix $(PRGSUFX),$(qFVSprgs))

ifndef sourceFileList

FVSak$(SLIBSUFX) FVSak$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSak) $(shell cat $(addsuffix _sourceList.txt,FVSak))
	$(MAKE) --file=makefile FVSak.setup

FVSbmc$(SLIBSUFX) FVSbmc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSbm) $(shell cat $(addsuffix _sourceList.txt,FVSbmc))
	$(MAKE) --file=makefile FVSbm.setup

FVScac$(SLIBSUFX) FVScac$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVScac) $(shell cat $(addsuffix _sourceList.txt,FVScac))
	$(MAKE) --file=makefile FVScac.setup

FVScic$(SLIBSUFX) FVScic$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVScic) $(shell cat $(addsuffix _sourceList.txt,FVScic))
	$(MAKE) --file=makefile FVScic.setup
	
FVScrc$(SLIBSUFX) FVScrc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVScrc) $(shell cat $(addsuffix _sourceList.txt,FVScrc))
	$(MAKE) --file=makefile FVScrc.setup

FVScs$(SLIBSUFX) FVScs$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVScs) $(shell cat $(addsuffix _sourceList.txt,FVScs))
	$(MAKE) --file=makefile FVScs.setup

FVSecc$(SLIBSUFX) FVSecc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSecc) $(shell cat $(addsuffix _sourceList.txt,FVSecc))
	$(MAKE) --file=makefile FVSecc.setup

FVSemc$(SLIBSUFX) FVSemc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSemc) $(shell cat $(addsuffix _sourceList.txt,FVSemc))
	$(MAKE) --file=makefile FVSemc.setup

FVSiec$(SLIBSUFX) FVSiec$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSiec) $(shell cat $(addsuffix _sourceList.txt,FVSiec))
	$(MAKE) --file=makefile FVSiec.setup

FVSktc$(SLIBSUFX) FVSktc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSktc) $(shell cat $(addsuffix _sourceList.txt,FVSktc))
	$(MAKE) --file=makefile FVSktc.setup

FVSls$(SLIBSUFX) FVSls$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSls) $(shell cat $(addsuffix _sourceList.txt,FVSls))
	$(MAKE) --file=makefile FVSls.setup

FVSncc$(SLIBSUFX) FVSncc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSncc) $(shell cat $(addsuffix _sourceList.txt,FVSncc))
	$(MAKE) --file=makefile FVSncc.setup
	
FVSne$(SLIBSUFX) FVSne$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSne) $(shell cat $(addsuffix _sourceList.txt,FVSne))
	$(MAKE) --file=makefile FVSne.setup

FVSoc$(SLIBSUFX) FVSoc$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSoc) $(shell cat $(addsuffix _sourceList.txt,FVSoc))
	$(MAKE) --file=makefile FVSoc.setup

FVSop$(SLIBSUFX) FVSop$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSop) $(shell cat $(addsuffix _sourceList.txt,FVSop))
	$(MAKE) --file=makefile FVSop.setup

FVSpnc$(SLIBSUFX) FVSpnc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSpnc) $(shell cat $(addsuffix _sourceList.txt,FVSpnc))
	$(MAKE) --file=makefile FVSpnc.setup

FVSsn$(SLIBSUFX) FVSsn$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSsn) $(shell cat $(addsuffix _sourceList.txt,FVSsn))
	$(MAKE) --file=makefile FVSsn.setup

FVSsoc$(SLIBSUFX) FVSsoc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSsoc) $(shell cat $(addsuffix _sourceList.txt,FVSsoc))
	$(MAKE) --file=makefile FVSsoc.setup

FVSttc$(SLIBSUFX) FVSttc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSttc) $(shell cat $(addsuffix _sourceList.txt,FVSttc))
	$(MAKE) --file=makefile FVSttc.setup

FVSutc$(SLIBSUFX) FVSutc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSutc) $(shell cat $(addsuffix _sourceList.txt,FVSutc))
	$(MAKE) --file=makefile FVSutc.setup

FVSwcc$(SLIBSUFX) FVSwcc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSwcc) $(shell cat $(addsuffix _sourceList.txt,FVSwcc))
	$(MAKE) --file=makefile FVSwcc.setup

FVSwsc$(SLIBSUFX) FVSwsc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSwsc) $(shell cat $(addsuffix _sourceList.txt,FVSwsc))
	$(MAKE) --file=makefile FVSwsc.setup

# the qFVS family, no odbc, just sqlite3.

qFVSak$(SLIBSUFX) qFVSak$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSak) $(shell cat $(addsuffix _sourceList.txt,qFVSak))
	$(MAKE) --file=makefile qFVSak.setup

qFVSbm$(SLIBSUFX) qFVSbm$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSbm) $(shell cat $(addsuffix _sourceList.txt,qFVSbm))
	$(MAKE) --file=makefile qFVSbm.setup

qFVSca$(SLIBSUFX) qFVSca$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSca) $(shell cat $(addsuffix _sourceList.txt,qFVSca))
	$(MAKE) --file=makefile qFVSca.setup

qFVSci$(SLIBSUFX) qFVSci$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSci) $(shell cat $(addsuffix _sourceList.txt,qFVSci))
	$(MAKE) --file=makefile qFVSci.setup
	
qFVScr$(SLIBSUFX) qFVScr$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVScr) $(shell cat $(addsuffix _sourceList.txt,qFVScr))
	$(MAKE) --file=makefile qFVScr.setup

qFVScs$(SLIBSUFX) qFVScs$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVScs) $(shell cat $(addsuffix _sourceList.txt,qFVScs))
	$(MAKE) --file=makefile qFVScs.setup

qFVSec$(SLIBSUFX) qFVSec$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSec) $(shell cat $(addsuffix _sourceList.txt,qFVSec))
	$(MAKE) --file=makefile qFVSec.setup

qFVSem$(SLIBSUFX) qFVSem$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSem) $(shell cat $(addsuffix _sourceList.txt,qFVSem))
	$(MAKE) --file=makefile qFVSem.setup

qFVSie$(SLIBSUFX) qFVSie$(PRGSUFX) : $(addsuffix _sourceList.txt,qFVSie) $(shell cat $(addsuffix _sourceList.txt,qFVSie))
	$(MAKE) --file=makefile qFVSie.setup

qFVSkt$(SLIBSUFX) qFVSkt$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSkt) $(shell cat $(addsuffix _sourceList.txt,qFVSkt))
	$(MAKE) --file=makefile qFVSkt.setup

qFVSls$(SLIBSUFX) qFVSls$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSls) $(shell cat $(addsuffix _sourceList.txt,qFVSls))
	$(MAKE) --file=makefile qFVSls.setup

qFVSnc$(SLIBSUFX) qFVSnc$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSnc) $(shell cat $(addsuffix _sourceList.txt,qFVSnc))
	$(MAKE) --file=makefile qFVSnc.setup
	
qFVSne$(SLIBSUFX) qFVSne$(PRGSUFX) : $(addsuffix _sourceList.txt,qFVSne) $(shell cat $(addsuffix _sourceList.txt,qFVSne))
	$(MAKE) --file=makefile qFVSne.setup

qFVSoc$(SLIBSUFX) qFVSoc$(PRGSUFX) : $(addsuffix _sourceList.txt,qFVSoc) $(shell cat $(addsuffix _sourceList.txt,qFVSoc))
	$(MAKE) --file=makefile qFVSoc.setup

qFVSop$(SLIBSUFX) qFVSop$(PRGSUFX) : $(addsuffix _sourceList.txt,qFVSop) $(shell cat $(addsuffix _sourceList.txt,qFVSop))
	$(MAKE) --file=makefile qFVSop.setup

qFVSpn$(SLIBSUFX) qFVSpn$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSpn) $(shell cat $(addsuffix _sourceList.txt,qFVSpn))
	$(MAKE) --file=makefile qFVSpn.setup

qFVSsn$(SLIBSUFX) qFVSsn$(PRGSUFX) : $(addsuffix _sourceList.txt,qFVSsn) $(shell cat $(addsuffix _sourceList.txt,qFVSsn))
	$(MAKE) --file=makefile qFVSsn.setup

qFVSso$(SLIBSUFX) qFVSso$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSso) $(shell cat $(addsuffix _sourceList.txt,qFVSso))
	$(MAKE) --file=makefile qFVSso.setup

qFVStt$(SLIBSUFX) qFVStt$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVStt) $(shell cat $(addsuffix _sourceList.txt,qFVStt))
	$(MAKE) --file=makefile qFVStt.setup

qFVSut$(SLIBSUFX) qFVSut$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSut) $(shell cat $(addsuffix _sourceList.txt,qFVSut))
	$(MAKE) --file=makefile qFVSut.setup

qFVSwc$(SLIBSUFX) qFVSwc$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSwc) $(shell cat $(addsuffix _sourceList.txt,qFVSwc))
	$(MAKE) --file=makefile qFVSwc.setup

qFVSws$(SLIBSUFX) qFVSws$(PRGSUFX)  : $(addsuffix _sourceList.txt,qFVSws) $(shell cat $(addsuffix _sourceList.txt,qFVSws))
	$(MAKE) --file=makefile qFVSws.setup

	
#FVSbcc$(SLIBSUFX) FVSbcc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSbcc) $(shell cat $(addsuffix _sourceList.txt,FVSbcc))
#	$(MAKE) --file=makefile qFVSbcc.setup

# TODO: These rules work in place of those listed above (they are generated), but only on Linux. 
# when we adopt a newer make on windows builds
#$(1)$(SLIBSUFX) $(1)$(PRGSUFX) : $$(addsuffix _sourceList.txt,$(1)) $$(shell cat $$(addsuffix _sourceList.txt,$(1)))
#	@echo $(MAKE) $(1)$(SLIBSUFX) $(1)$(PRGSUFX) --file=makefile $(1).setup
#	$(MAKE) --file=makefile $(1).setup
#endef
#
#$(foreach prog,$(FVSprgs),$(eval $(call P_template,$(prog))))
#===========

%.setup : 
	mkdir -pv $(basename $@)_buildDir
ifeq (${OSARCH},w32)
# this command resets the acl's on Windows, if it causes problems, delete it!
	-icacls $(basename $@)_buildDir /reset /T /Q
endif
	cp -p `cat $(basename $@)_sourceList.txt` $(basename $@)_buildDir
ifeq (${OSARCH},w32)
	-icacls $(basename $@)_buildDir /reset /T /Q
endif                     
	$(MAKE) --file=../makefile --directory=$(basename $@)_buildDir \
	               buildDir=$(basename $@)_buildDir \
                 sourceFileList=../$(basename $@)_sourceList.txt $(basename $@).prg
endif

.SUFFIXES :

.PRECIOUS : $(object) $(mods_mods) 
 
ifeq (${OSARCH},w32) 
ifeq ($(DBLINK),$(ODBC))
%.prg : $(object) 
	$(CC) fvsSQL.o -shared $(DBLINK) $(WIN) -o   ../libfvsSQL.dll 
	$(LD) -shared -Wl,--export-all-symbols -o $(basename ../$@)$(SLIBSUFX) ../libfvsSQL.dll  $(filter-out fvsSQL.o main.o,$(object))
	$(LD)         -o $(basename ../$@)$(PRGSUFX)  ../libfvsSQL.dll  $(filter-out fvsSQL.o       ,$(object))
else
%.prg : $(object) 
	$(LD) -shared -Wl,--export-all-symbols -o $(basename ../$@)$(SLIBSUFX)  $(filter-out  main.o,$(object))
	$(LD)         -o $(basename ../$@)$(PRGSUFX) $(object)
endif
endif
ifeq (${OSARCH},w64) 
ifeq ($(DBLINK),$(ODBC))
%.prg : $(object)
	$(CCprf)$(CC) fvsSQL.o -shared $(DBLINK) $(WIN) -o   ../libfvsSQL.dll 
	$(FCprf)$(LD) -shared -Wl, --export-all-symbols -o $(basename ../$@)$(SLIBSUFX) ../libfvsSQL.dll  $(filter-out fvsSQL.o main.o,$(object))
	$(FCprf)$(LD)         -o $(basename ../$@)$(PRGSUFX)  ../libfvsSQL.dll  $(filter-out fvsSQL.o       ,$(object))
else
%.prg : $(object) 
	$(FCprf)$(LD) -static -shared -Wl, --export-all-symbols -o $(basename ../$@)$(SLIBSUFX)  $(filter-out  main.o,$(object))
	$(FCprf)$(LD) -static -o $(basename ../$@)$(PRGSUFX) $(object)
endif
else
%.prg : $(object) 
	$(LD) -shared -o $(basename ../$@)$(SLIBSUFX) $(filter-out main.o,$(object)) $(DBLINK) 
	$(LD)         -o $(basename ../$@)$(PRGSUFX)  $(object) $(DBLINK)    
endif

%.mod : %.f $(includes)
	$(FCprf)$(FC) $(FFLAGS) -c $<

%.o : %.f $(includes) $(mods_mods)
	$(FCprf)$(FC) $(FFLAGS) -c -o $@ $<

%.o : %.c $(includes) 
	$(CCprf)$(CC) $(CFLAGS) -c -o $@ $<
	
DBSTYPEDEFS.F77 : mkdbsTypeDefs.c
	$(CCprf)$(CC) mkdbsTypeDefs.c $(WIN) -o mkdbsTypeDefs$(PRGSUFX)
	./mkdbsTypeDefs$(PRGSUFX)
	
