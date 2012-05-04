
#	$Id$	

# Notes: If OS is defined, then then we are 32 bit windows, otherwise, we are Linix 
# x86_64 or mac osx x86_64. 
# Key switches/flags:
# OSARCH is used to select the ODBC type defs in DBSTYPEDEFS.F77 and in selecting the 
#  ultimate build command. Values are: 
#     "w32" on Windows x86 (32 bit), 
#     "osx" on MAC x86_64, and 
#     "l64" on generic Linux x86_64.

ifdef OS
 WIN = -DWINDOWS
 OSARCH = w32
 ODBC = odbc32
 SLIBSUFX = .dll
 PRGSUFX = .exe
 PIC =      # never use PIC on this OSARCH type
#not 32 bit windows, assume 64 bit Linux or mac osx.
else
 WIN = 
 SLIBSUFX = .so
 PRGSUFX = 
 ifdef OSTYPE
  ifeq ($(OSTYPE),linux)
    OSARCH = l64
    PIC = -fPIC  # always use PIC on this OSARCH type
  endif
 endif
 ifndef OSARCH
   OSARCH = osx
   PIC =      # never use PIC on this OSARCH type
 endif
 ODBC = odbc 
endif

# FC is the fortran compiler
# FFLAGS are pasted to fortran
# CC is the c compiler
# CFLAGS are pased to C, of these CMPgcc is used by the fofem routines and WIN is used
# by fvsSQL.c
# LD is the linker

export FC = gfortran 
export FFLAGS = $(PIC) -g -Wall
export CC = gcc
export CFLAGS = $(PIC) -DANSI -DCMP${CC} $(WIN)
export LD = gfortran
ifndef sourceFileList
# TODO: See comment below regarding generl rules. We can change to this rule for all
# when we adopt a newer make on windows builds
# export FVSprgs = $(subst _sourceList.txt,,$(shell ls FVS*_sourceList.txt))
  export FVSprgs = FVSiec FVSpnc FVSne FVSsn
endif

# the following variables are not processed until sourceFileList is defined...and that happens
# when the first rule fires (the %.setup rule).
ifdef sourceFileList
 ifndef sourceList
  export sourceFileList
  export sourceList = $(shell cat $(sourceFileList))
  export includes = $(notdir $(filter %.F77 %.INC %.h,$(sourceList))) DBSTYPEDEFS.F77
  export mods = $(notdir $(filter %_mod.f, $(sourceList)))
  export mods_mods = $(addsuffix .mod,$(basename $(mods)))
  export source = $(filter-out mkdbsTypeDefs.c, $(notdir $(filter %.c %.f, $(sourceList))))
  export object = $(addsuffix .o,$(basename $(source)))
 endif
endif

all : $(addsuffix $(SLIBSUFX),$(FVSprgs)) $(addsuffix $(PRGSUFX),$(FVSprgs))

clean :
	rm -frv *_buildDir libfvsSQL.dll $(addsuffix $(SLIBSUFX),$(FVSprgs)) $(addsuffix $(PRGSUFX),$(FVSprgs))

ifndef sourceFileList

FVSiec$(SLIBSUFX) FVSiec$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSiec) $(shell cat $(addsuffix _sourceList.txt,FVSiec))
	$(MAKE) --file=makefile FVSiec.setup

FVSpnc$(SLIBSUFX) FVSpnc$(PRGSUFX)  : $(addsuffix _sourceList.txt,FVSpnc) $(shell cat $(addsuffix _sourceList.txt,FVSpnc))
	$(MAKE) --file=makefile FVSpnc.setup

FVSne$(SLIBSUFX) FVSne$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSne) $(shell cat $(addsuffix _sourceList.txt,FVSne))
	$(MAKE) --file=makefile FVSne.setup

FVSsn$(SLIBSUFX) FVSsn$(PRGSUFX) : $(addsuffix _sourceList.txt,FVSsn) $(shell cat $(addsuffix _sourceList.txt,FVSsn))
	$(MAKE) --file=makefile FVSsn.setup

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
	cp -p `cat $(basename $@)_sourceList.txt` $(basename $@)_buildDir
	$(MAKE) --file=../makefile --directory=$(basename $@)_buildDir \
	               buildDir=$(basename $@)_buildDir \
                 sourceFileList=../$(basename $@)_sourceList.txt $(basename $@).prg
endif

.PRECIOUS : $(object) $(mods_mods) 

ifeq (${OSARCH},w32) 
%.prg : $(object) 
	$(CC) fvsSQL.o -shared -l $(ODBC) $(WIN) -o   ../libfvsSQL.dll 
	$(LD) -shared -o $(basename ../$@)$(SLIBSUFX) ../libfvsSQL.dll  $(filter-out fvsSQL.o main.o,$(object))
	$(LD)         -o $(basename ../$@)$(PRGSUFX)  ../libfvsSQL.dll  $(filter-out fvsSQL.o       ,$(object))
else
%.prg : $(object) 
	$(LD) -shared -o $(basename ../$@)$(SLIBSUFX) -l $(ODBC) $(filter-out main.o,$(object))
	$(LD)         -o $(basename ../$@)$(PRGSUFX)  -l $(ODBC) $(object)
endif

DBSTYPEDEFS.F77 : mkdbsTypeDefs.c
	$(CC) mkdbsTypeDefs.c $(WIN) -o mkdbsTypeDefs$(PRGSUFX)
	./mkdbsTypeDefs$(PRGSUFX)

%.mod : %.f $(includes)
	$(FC) $(FFLAGS) -c $<

%.o : %.f $(includes) $(mods_mods)
	$(FC) $(FFLAGS) -c -o $@ $<

%.o : %.c $(includes) 
	$(CC) $(CFLAGS) -c -o $@ $<
	
