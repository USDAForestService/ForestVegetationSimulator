# Fortran compiler
include ../../bin/FC${OS}.make

# FVS common blocks directory
BASE_COM = ../../common/

# Western Spruce Budworm General Defoliator source directory
WWPB_SRC   = ../src/

# Dwarf Mistletoe source directory
MIST_SRC = ../../mistoe/src/

# options for the compiler
FFLAGS = ${I1}${WWPB_SRC}${I2}${BASE_COM}${I2}${MIST_SRC}

WWPB_OBJS = bmagdw.o${BJ} bmatct.o${BJ} bmavg.o${BJ} \
            bmblkdbm.o${BJ} bmblkdnc.o${BJ} bmblkdni.o${BJ} \
            bmblkdso.o${BJ} bmcbet.o${BJ} bmcbkp.o${BJ} bmcgrf.o${BJ} bmcnum.o${BJ} \
            bmcspt.o${BJ} bmcwin.o${BJ} bmdam.o${BJ} bmdbhc.o${BJ} bmdfol.o${BJ} \
            bmdrgt.o${BJ} bmdrv.o${BJ} bmfdbk.o${BJ} \
            bmfint.o${BJ} bmfire.o${BJ} bmfmrt.o${BJ} bmin.o${BJ} bminit.o${BJ} \
            bmips.o${BJ} bmistd.o${BJ} bmkill.o${BJ} bmlite.o${BJ} bmmort.o${BJ} \
            bmobb.o${BJ} bmout.o${BJ} bmpher.o${BJ} bmppgt.o${BJ} bmppin.o${BJ} \
            bmpppt.o${BJ} bmpstc.o${BJ} bmqmrt.o${BJ} bmrann.o${BJ} bmrrgt.o${BJ} \
            bmrrpt.o${BJ} bmrrsr.o${BJ} bmsalv.o${BJ} bmsani.o${BJ} bmsdit.o${BJ} \
            bmsetp.o${BJ} bmslsh.o${BJ} bmsmgt.o${BJ} bmtrip.o${BJ} 

# dependencies for these routines are based on include files only.

wwpb : ${WWPB_OBJS}

bmagdw.o${BJ}:   ${WWPB_SRC}bmagdw.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmagdw.f   

bmatct.o${BJ}:   ${WWPB_SRC}bmatct.f  \
                 ${BASE_COM}PRGPRM.F77  ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmatct.f   

bmavg.o${BJ}:    ${WWPB_SRC}bmavg.f   \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmavg.f    

bmblkdbm.o${BJ}: ${WWPB_SRC}bmblkdbm.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMRCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmblkdbm.f 

# This Central Rockies block data needs compiled only with all other CR code
# bmblkdcr.o${BJ}: ${WWPB_SRC}bmblkdcr.f  \
#                  ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
#                  ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMRCOM.F77
#	${FC} -c ${FFLAGS} ${WWPB_SRC}bmblkdcr.f 

bmblkdnc.o${BJ}: ${WWPB_SRC}bmblkdnc.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMRCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmblkdnc.f 

bmblkdni.o${BJ}: ${WWPB_SRC}bmblkdni.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMRCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmblkdni.f 

# The compile instructions for this routine 33 species version are also in ppeso/obj/makefile
bmblkdso.o${BJ}: ${WWPB_SRC}bmblkdso.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMRCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmblkdso.f 

bmcbet.o${BJ}:   ${WWPB_SRC}bmcbet.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcbet.f   

bmcbkp.o${BJ}:   ${WWPB_SRC}bmcbkp.f  \
                 ${BASE_COM}PRGPRM.F77  ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcbkp.f   

bmcgrf.o${BJ}:   ${WWPB_SRC}bmcgrf.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcgrf.f   

bmcnum.o${BJ}:   ${WWPB_SRC}bmcnum.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcnum.f   

bmcspt.o${BJ}:   ${WWPB_SRC}bmcspt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcspt.f   

bmcwin.o${BJ}:   ${WWPB_SRC}bmcwin.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmcwin.f   

bmdam.o${BJ}:    ${WWPB_SRC}bmdam.f   \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}CONTRL.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdam.f    

bmdbhc.o${BJ}:   ${WWPB_SRC}bmdbhc.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdbhc.f   

bmdfol.o${BJ}:   ${WWPB_SRC}bmdfol.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdfol.f   

bmdrgt.o${BJ}:   ${WWPB_SRC}bmdrgt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdrgt.f   

bmdrv.o${BJ}:    ${WWPB_SRC}bmdrv.f   \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdrv.f    

#bmdvo.o${BJ}:    ${WWPB_SRC}bmdvo.f   \
#                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
#                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
#	${FC} -c ${FFLAGS} ${WWPB_SRC}bmdvo.f    

bmfdbk.o${BJ}:   ${WWPB_SRC}bmfdbk.f  \
                 ${WWPB_SRC}BMPRM.F77 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmfdbk.f   

bmfint.o${BJ}:   ${WWPB_SRC}bmfint.f  \
                 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMFCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmfint.f   

bmfire.o${BJ}:   ${WWPB_SRC}bmfire.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMFCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmfire.f   

bmfmrt.o${BJ}:   ${WWPB_SRC}bmfmrt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMFCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmfmrt.f   

bmin.o${BJ}:   ${WWPB_SRC}bmin.f  \
                 ${BASE_COM}PRGPRM.F77 ${WWPB_SRC}BMPRM.F77 ${BASE_COM}PPEPRM.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77 \
                 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmin.f   

bminit.o${BJ}:   ${WWPB_SRC}bminit.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMRRCM.F77 \
                 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bminit.f   

bmips.o${BJ}:    ${WWPB_SRC}bmips.f   \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmips.f    

bmistd.o${BJ}:   ${WWPB_SRC}bmistd.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmistd.f   

bmkill.o${BJ}:   ${WWPB_SRC}bmkill.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}OUTCOM.F77 \
                 ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmkill.f   

bmlite.o${BJ}:   ${WWPB_SRC}bmlite.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmlite.f   

bmmort.o${BJ}:   ${WWPB_SRC}bmmort.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmmort.f   

bmobb.o${BJ}:    ${WWPB_SRC}bmobb.f   \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmobb.f    

bmout.o${BJ}:   ${WWPB_SRC}bmout.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmout.f   

bmpher.o${BJ}:   ${WWPB_SRC}bmpher.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmpher.f   

bmppgt.o${BJ}:   ${WWPB_SRC}bmppgt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmppgt.f   

bmppin.o${BJ}:   ${WWPB_SRC}bmppin.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77 \
                 ${WWPB_SRC}BMRRCM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmppin.f   

bmpppt.o${BJ}:   ${WWPB_SRC}bmpppt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}ARRAYS.F77 ${BASE_COM}PPCNTL.F77 ${BASE_COM}CONTRL.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmpppt.f   

bmpstc.o${BJ}:   ${WWPB_SRC}bmpstc.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmpstc.f   

bmqmrt.o${BJ}:   ${WWPB_SRC}bmqmrt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmqmrt.f   

bmrann.o${BJ}:   ${WWPB_SRC}bmrann.f ${WWPB_SRC}BMRCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmrann.f   

bmrrgt.o${BJ}:   ${WWPB_SRC}bmrrgt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMRRCM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmrrgt.f   

bmrrpt.o${BJ}:   ${WWPB_SRC}bmrrpt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 ${WWPB_SRC}BMRRCM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmrrpt.f   

bmrrsr.o${BJ}:   ${WWPB_SRC}bmrrsr.f  \
                 ${BASE_COM}PRGPRM.F77 ${WWPB_SRC}BMRRCM.F77 ${BASE_COM}CONTRL.F77 \
                 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmrrsr.f   

bmsalv.o${BJ}:   ${WWPB_SRC}bmsalv.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsalv.f   

bmsani.o${BJ}:   ${WWPB_SRC}bmsani.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsani.f   

bmsdit.o${BJ}:   ${WWPB_SRC}bmsdit.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${BASE_COM}PPCNTL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}OUTCOM.F77 \
                 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${MIST_SRC}MISCOM.F77 \
                 ${WWPB_SRC}BMCOM.F77 ${WWPB_SRC}BMFCOM.F77 ${WWPB_SRC}BMRRCM.F77 \
                 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsdit.f   

bmsetp.o${BJ}:   ${WWPB_SRC}bmsetp.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}ARRAYS.F77 \
                 ${BASE_COM}PPCNTL.F77 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77 \
                 ${WWPB_SRC}BMPCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsetp.f   

bmslsh.o${BJ}:   ${WWPB_SRC}bmslsh.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmslsh.f   

bmsmgt.o${BJ}:   ${WWPB_SRC}bmsmgt.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${BASE_COM}PPCNTL.F77 \
                 ${WWPB_SRC}BMPRM.F77 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmsmgt.f   

bmtrip.o${BJ}:   ${WWPB_SRC}bmtrip.f  \
                 ${BASE_COM}PRGPRM.F77 ${BASE_COM}PPEPRM.F77 ${WWPB_SRC}BMPRM.F77 \
                 ${WWPB_SRC}BMCOM.F77
	${FC} -c ${FFLAGS} ${WWPB_SRC}bmtrip.f   

