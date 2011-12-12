# Fortran compiler
include ../../bin/FC${OS}.make

# the FVS common blocks directory
BASE_COM = ../../common/

# the White pine blister rust source directory
WPBR_SRC   = ./../src/

# options for the compiler
FFLAGS = ${I1}${WPBR_SRC}${I2}${BASE_COM} 

# the White pine blister rust object files
WPBR_OBJS  = brann.o${BJ} bratv.o${BJ} brblkd.o${BJ}  brblkdso.o${BJ} \
             brcank.o${BJ} brcdel.o${BJ} brcgro.o${BJ} brcini.o${BJ}  \
             brcmpr.o${BJ} brcout.o${BJ} brcred.o${BJ} brcrem.o${BJ}  \
             brcsta.o${BJ} brdam.o${BJ} brdout.o${BJ} brecan.o${BJ}   \
             brestb.o${BJ} brgi.o${BJ} briba.o${BJ} bribes.o${BJ} brical.o${BJ} \
             brin.o${BJ} brinit.o${BJ} brpr.o${BJ} brrout.o${BJ}      \
             brsetp.o${BJ} brsor.o${BJ} brstar.o${BJ} brstat.o${BJ}   \
             brstyp.o${BJ} brsum.o${BJ} brtarg.o${BJ} brtdel.o${BJ}   \
             brtout.o${BJ} brtreg.o${BJ} brtrip.o${BJ} brtsta.o${BJ}  \
             brupdt.o${BJ} i4pbsr.o${BJ} i4psrt.o${BJ}

wpbr:     ${WPBR_OBJS}

brann.o${BJ}: ${WPBR_SRC}brann.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brann.f

bratv.o${BJ}: ${WPBR_SRC}bratv.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}bratv.f

brblkd.o${BJ}: ${WPBR_SRC}brblkd.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brblkd.f

brblkdso.o${BJ}: ${WPBR_SRC}brblkdso.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brblkdso.f

brcank.o${BJ}: ${WPBR_SRC}brcank.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${BASE_COM}HTCAL.F77  \
          ${BASE_COM}ESTREE.F77 ${WPBR_SRC}BRCOM.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brcank.f

brcdel.o${BJ}: ${WPBR_SRC}brcdel.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${WPBR_SRC}BRCOM.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brcdel.f

brcgro.o${BJ}: ${WPBR_SRC}brcgro.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brcgro.f

brcini.o${BJ}: ${WPBR_SRC}brcini.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${WPBR_SRC}BRCOM.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brcini.f

brcmpr.o${BJ}: ${WPBR_SRC}brcmpr.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brcmpr.f

brcout.o${BJ}: ${WPBR_SRC}brcout.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brcout.f

brcred.o${BJ}: ${WPBR_SRC}brcred.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}ARRAYS.F77 ${WPBR_SRC}BRCOM.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brcred.f

brcrem.o${BJ}: ${WPBR_SRC}brcrem.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brcrem.f

brcsta.o${BJ}: ${WPBR_SRC}brcsta.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brcsta.f

brdam.o${BJ}: ${WPBR_SRC}brdam.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
         ${WPBR_SRC}BRCOM.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brdam.f

brdout.o${BJ}: ${WPBR_SRC}brdout.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 \
          ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brdout.f

brecan.o${BJ}: ${WPBR_SRC}brecan.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}PLOT.F77 ${BASE_COM}ARRAYS.F77 ${WPBR_SRC}BRCOM.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brecan.f

brestb.o${BJ}: ${WPBR_SRC}brestb.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${WPBR_SRC}BRCOM.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brestb.f

brgi.o${BJ}: ${WPBR_SRC}brgi.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77      \
        ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brgi.f

briba.o${BJ}: ${WPBR_SRC}briba.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77    \
         ${BASE_COM}PLOT.F77 ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}briba.f

bribes.o${BJ}: ${WPBR_SRC}bribes.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}bribes.f

brical.o${BJ}: ${WPBR_SRC}brical.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brical.f

brin.o${BJ}: ${WPBR_SRC}brin.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77     \
        ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brin.f

brinit.o${BJ}: ${WPBR_SRC}brinit.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brinit.f

brpr.o${BJ}: ${WPBR_SRC}brpr.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77    \
        ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${WPBR_SRC}BRCOM.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brpr.f

brrout.o${BJ}: ${WPBR_SRC}brrout.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 \
          ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brrout.f

brsetp.o${BJ}: ${WPBR_SRC}brsetp.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brsetp.f

brsor.o${BJ}: ${WPBR_SRC}brsor.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77   \
         ${BASE_COM}CONTRL.F77 ${BASE_COM}PLOT.F77 ${WPBR_SRC}BRCOM.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brsor.f

brstar.o${BJ}: ${WPBR_SRC}brstar.f 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brstar.f

brstat.o${BJ}: ${WPBR_SRC}brstat.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77 \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brstat.f

brstyp.o${BJ}: ${WPBR_SRC}brstyp.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${WPBR_SRC}BRCOM.F77  
	${FC} -c ${FFLAGS} ${WPBR_SRC}brstyp.f

brsum.o${BJ}: ${WPBR_SRC}brsum.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77    \
         ${BASE_COM}CONTRL.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brsum.f

brtarg.o${BJ}: ${WPBR_SRC}brtarg.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brtarg.f

brtdel.o${BJ}: ${WPBR_SRC}brtdel.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77  \
          ${BASE_COM}CONTRL.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brtdel.f

brtout.o${BJ}: ${WPBR_SRC}brtout.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brtout.f

brtreg.o${BJ}: ${WPBR_SRC}brtreg.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${WPBR_SRC}BRCOM.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brtreg.f

brtrip.o${BJ}: ${WPBR_SRC}brtrip.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}CONTRL.F77 \
          ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77 ${WPBR_SRC}BRCOM.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brtrip.f

brtsta.o${BJ}: ${WPBR_SRC}brtsta.f ${BASE_COM}PRGPRM.F77 ${BASE_COM}ARRAYS.F77 \
          ${BASE_COM}CONTRL.F77 ${WPBR_SRC}BRCOM.F77 
	${FC} -c ${FFLAGS} ${WPBR_SRC}brtsta.f

brupdt.o${BJ}: ${WPBR_SRC}brupdt.f ${BASE_COM}PRGPRM.F77 ${WPBR_SRC}BRCOM.F77  \
          ${BASE_COM}CONTRL.F77 ${BASE_COM}ARRAYS.F77 ${BASE_COM}PLOT.F77
	${FC} -c ${FFLAGS} ${WPBR_SRC}brupdt.f

i4pbsr.o${BJ}: ${WPBR_SRC}i4pbsr.f 
	${FC} -c ${FFLAGS} ${WPBR_SRC}i4pbsr.f

i4psrt.o${BJ}: ${WPBR_SRC}i4psrt.f
	${FC} -c ${FFLAGS} ${WPBR_SRC}i4psrt.f

