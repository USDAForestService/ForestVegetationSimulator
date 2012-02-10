
#	$Id$	


# Notes: If OS is defined, then then we are 32 bit windows, otherwise, we are Linix 
# x86_64 or mac osx x86_64. 
# Key switches/flags:
# WIN is set to -DWINDOWS and this is used in the conditional compile of fvsSQL.c (also for CFLAGS)
# OSARCH is used to select the ODBC type defs in DBSTYPEDEFS.F77 (see the copy command below). 
#  Valuses are: "w32" on Windows x86 (32 bit), "osx" on MAC x86_64, and l64 on generic Linux x86_64.

ifdef OS
 WIN = -DWINDOWS
 OSARCH = w32
 ODBC = odbc32
 ifeq (${as.shared},1)
  SHARED = -shared 
  OUTSUFX = .so
  MAIN = 0
  PIC = 
 else
  SHARED = -static
  OUTSUFX = .exe
  MAIN = 1
  PIC = 
 endif
#not 32 bit windows, assume 64 bit Linux or mac osx.
else
 WIN = 
 ifdef OSTYPE
  ifeq ($(OSTYPE),linux)
    OSARCH = l64
  endif
 endif
 ifndef OSARCH
   OSARCH = osx
 endif
 ODBC = odbc 
 ifeq (${as.shared},1)
  SHARED = -shared 
  OUTSUFX = .so
  MAIN = 0
  PIC = -fPIC
 else
  SHARED =
  OUTSUFX =
  MAIN = 1
  PIC = 
 endif
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

# the following variables are not processed until sourceFileList is defined...and that happens
# when the first rule fires (the %.setup rule).
ifdef sourceFileList
 ifndef sourceList
  export sourceFileList
  export sourceList = $(shell cat $(sourceFileList))
  export includes = $(notdir $(filter %.F77 %.INC %.h,$(sourceList))) 
  export src1 = $(notdir $(filter %.c %.f,$(sourceList))) 
  ifeq (${MAIN},0) 
   export source = $(filter-out main.f,$(src1))
  else
   export source = $(src1)
  endif
  export object = $(addsuffix .o,$(basename $(source)))
 endif
endif

clean :
	rm -frv *_buildDir ../dbs/obj/fvsSQL.o libfvsSQL.so FVSiec$(OUTSUFX) FVSpnc$(OUTSUFX) FVSne$(OUTSUFX) FVSsn$(OUTSUFX)

ifndef sourceFileList

FVSiec : FVSiec_sourceList.txt $(shell cat FVSiec_sourceList.txt)
	$(MAKE) --file=makefile $@.setup

FVSpnc : FVSpnc_sourceList.txt $(shell cat FVSpnc_sourceList.txt)
	$(MAKE) --file=makefile $@.setup

FVSne : FVSne_sourceList.txt $(shell cat FVSne_sourceList.txt)
	$(MAKE) --file=makefile $@.setup

FVSsn : FVSsn_sourceList.txt $(shell cat FVSsn_sourceList.txt)
	$(MAKE) --file=makefile $@.setup

# here is the rule that sets up the directories and copies the source, then it calls make
# again to build the target.
%.setup : libfvsSQL.so
	mkdir -pv $(basename $@)_buildDir
	cp -p `cat $(basename $@)_sourceList.txt` $(basename $@)_buildDir
	cp -p $(basename $@)_buildDir/DBSTYPEDEFS_$(OSARCH).F77 $(basename $@)_buildDir/DBSTYPEDEFS.F77
	$(MAKE) --file=../makefile --directory=$(basename $@)_buildDir \
	               buildDir=$(basename $@)_buildDir \
                 sourceFileList=../$(basename $@)_sourceList.txt $(basename $@).prg

ifeq (${OSARCH},l64) 
libfvsSQL.so : ../dbs/src/fvsSQL.c 
	$(CC) $< -shared -fPIC -l $(ODBC) $(WIN) -o $@ 
else
libfvsSQL.so : ../dbs/src/fvsSQL.c 
	$(CC) $< -shared -l $(ODBC) $(WIN) -o $@ 
endif

endif

.PRECIOUS : $(object) 

#TODO: this might work for now on w32 and osx 
ifeq (${OSARCH},l64) 
%.prg : $(object)
	$(LD) $(SHARED) -o $(basename ../$@)${OUTSUFX} $(realpath ../libfvsSQL.so) $(object)
else
%.prg : $(object)
	$(LD) $(SHARED) -o $(basename ../$@)${OUTSUFX} ../libfvsSQL.so $(object)
endif

%.o : %.f $(includes)
	$(FC) $(FFLAGS) -c -o $@ $<

%.o : %.c $(includes)
	$(CC) $(CFLAGS) -c -o $@ $<
	
blmvol.o : blmvol.f debug_mod.mod $(includes) 
	$(FC) $(FFLAGS) -c -o $@ blmvol.f

blmtap.o : blmtap.f debug_mod.mod $(includes) 
	$(FC) $(FFLAGS) -c -o $@ blmtap.f

profile.o : profile.f debug_mod.mod $(includes) 
	$(FC) $(FFLAGS) -c -o $@ profile.f

debug_mod.mod : debug_mod.f $(includes) 
	$(FC) $(FFLAGS) -c debug_mod.f

