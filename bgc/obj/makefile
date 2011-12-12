# Fortran compiler
include ../../bin/FC${OS}.make

# the Prognosis common blocks directory
BASE_COM = ./../../common/

# the bgc model source directory
BGC_SRC = ./../src/

# options for the compiler
FFLAGS = ${I1}${BGC_SRC}${I2}${BASE_COM}

# the BGC object files

BGC_OBJS = \
babsrad.o${BJ} \
balloca.o${BJ} \
bark.o${BJ} \
bcrown.o${BJ} \
bcvts.o${BJ} \
bentyload.o${BJ} \
bentyupdt.o${BJ} \
bgcfvs.o${BJ} \
bgcgo.o${BJ} \
bgcgrow.o${BJ} \
bgcin.o${BJ} \
bgcint.o${BJ} \
bgrown.o${BJ} \
bhtdiam.o${BJ} \
binitial.o${BJ} \
biopen.o${BJ} \
bkill.o${BJ} \
bpenman.o${BJ} \
bsiteload.o${BJ} \
bstndbgcn.o${BJ} \
bstructn.o${BJ} \
btrnovr.o${BJ} \
bupdate.o${BJ} \
bwater.o${BJ} \
bzcomp.o${BJ} \
setleafn.o${BJ} \
sitocn.o${BJ} \
redolai.o${BJ}


bgc: ${BGC_OBJS}

babsrad.o${BJ}: ${BGC_SRC}babsrad.f   ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}babsrad.f  

balloca.o${BJ}: ${BGC_SRC}balloca.f   ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}balloca.f   

bark.o${BJ}: ${BGC_SRC}bark.f
	${FC} -c ${FFLAGS} ${BGC_SRC}bark.f

bcrown.o${BJ}: ${BGC_SRC}bcrown.f    ${BGC_SRC}ENTITY.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bcrown.f

bentyload.o${BJ}: ${BGC_SRC}bentyload.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bentyload.f 

bentyupdt.o${BJ}: ${BGC_SRC}bentyupdt.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bentyupdt.f   

bgcfvs.o${BJ}: ${BGC_SRC}bgcfvs.f  ${BGC_SRC}ENTITY.F77 \
                                  ${BGC_SRC}BGCCOM.F77 \
                                  ${BASE_COM}ARRAYS.F77 \
                                  ${BASE_COM}CONTRL.F77 \
                                  ${BASE_COM}PRGPRM.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bgcfvs.f

bgcgo.o${BJ}: ${BGC_SRC}bgcgo.f ${BGC_SRC}BGCCOM.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bgcgo.f

bgcgrow.o${BJ}: ${BGC_SRC}bgcgrow.f  ${BGC_SRC}BGCCOM.F77 \
                                    ${BASE_COM}PRGPRM.F77 \
                                    ${BASE_COM}PLOT.F77 \
                                    ${BASE_COM}CONTRL.F77 \
                                    ${BASE_COM}PDEN.F77 \
                                    ${BASE_COM}OUTCOM.F77 \
                                    ${BASE_COM}ARRAYS.F77 \
                                    ${BASE_COM}CVCOM.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bgcgrow.f

bgcin.o${BJ}: ${BGC_SRC}bgcin.f  ${BGC_SRC}BGCCOM.F77 \
                                ${BGC_SRC}ENTITY.F77 \
                                ${BASE_COM}PRGPRM.F77 \
                                ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bgcin.f

bgcint.o${BJ}: ${BGC_SRC}bgcint.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77 \
                                  ${BGC_SRC}BGCCOM.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bgcint.f 

bcvts.o${BJ}: ${BGC_SRC}bcvts.f
	${FC} -c ${FFLAGS} ${BGC_SRC}bcvts.f 

bgrown.o${BJ}: ${BGC_SRC}bgrown.f   ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bgrown.f

bhtdiam.o${BJ}: ${BGC_SRC}bhtdiam.f
	${FC} -c ${FFLAGS} ${BGC_SRC}bhtdiam.f

binitial.o${BJ}: ${BGC_SRC}binitial.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77 \
                                      ${BGC_SRC}BGCCOM.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}binitial.f 

biopen.o${BJ}: ${BGC_SRC}biopen.f
	${FC} -c ${FFLAGS} ${BGC_SRC}biopen.f 

bkill.o${BJ}: ${BGC_SRC}bkill.f ${BGC_SRC}ENTITY.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bkill.f

bpenman.o${BJ}: ${BGC_SRC}bpenman.f
	${FC} -c ${FFLAGS} ${BGC_SRC}bpenman.f

bsiteload.o${BJ}: ${BGC_SRC}bsiteload.f ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bsiteload.f  

bstndbgcn.o${BJ}: ${BGC_SRC}bstndbgcn.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77 \
                                      ${BGC_SRC}BGCCOM.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}bstndbgcn.f

bstructn.o${BJ}: ${BGC_SRC}bstructn.f ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bstructn.f  

btrnovr.o${BJ}: ${BGC_SRC}btrnovr.f ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}btrnovr.f  

bupdate.o${BJ}: ${BGC_SRC}bupdate.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bupdate.f   

bwater.o${BJ}: ${BGC_SRC}bwater.f  ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bwater.f 

bzcomp.o${BJ}: ${BGC_SRC}bzcomp.f ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}bzcomp.f

setleafn.o${BJ}: ${BGC_SRC}setleafn.f ${BGC_SRC}ENTITY.F77 
	${FC} -c ${FFLAGS} ${BGC_SRC}setleafn.f

sitocn.o${BJ}: ${BGC_SRC}sitocn.f ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}sitocn.f 

redolai.o${BJ}: ${BGC_SRC}redolai.f ${BGC_SRC}ENTITY.F77 ${BGC_SRC}SITE.F77
	${FC} -c ${FFLAGS} ${BGC_SRC}redolai.f 

