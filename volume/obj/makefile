# Fortran compiler
include ../../bin/FC${OS}.make

# the volume source directory  (Central Rockies Variant)
VOLUME_SRC  = ./../src/

# the volume objectiles
VOLUME_OBJS = debug_mod.o${BJ} \
              clkcoef_mod.o${BJ} blmtap.o${BJ} blmvol.o${BJ} brk_up.o${BJ} \
              doyal78.o${BJ} dvest.o${BJ} f_alaska.o${BJ} f_ingy.o${BJ} f_other.o${BJ} \
              f_west.o${BJ} formclas.o${BJ} fwinit.o${BJ} intl78.o${BJ} mrules.o${BJ}  \
              numlog.o${BJ} profile.o${BJ} \
              r1allen.o${BJ} r1byrne.o${BJ} r1kemp.o${BJ} r1tap.o${BJ} \
              r2oldv.o${BJ} r2tap.o${BJ} r3d2hv.o${BJ}  r4d2h.o${BJ} r4vol.o${BJ} r5harv.o${BJ} \
              r5tap.o${BJ} r6dibs.o${BJ} r6fix.o${BJ} r6vol.o${BJ} r6vol1.o${BJ} r6vol2.o${BJ} \
              r6vol3.o${BJ} r8init.o${BJ} r8vol.o${BJ} r8vol1.o${BJ} r8vol2.o${BJ} r9clark.o${BJ} \
              r9init.o${BJ} r9logs.o${BJ} r9vol.o${BJ} \
              r10d2h.o${BJ} r10tap.o${BJ} r10tapo.o${BJ} r10vol.o${BJ} r10vol1.o${BJ} \
              r10volo.o${BJ} r12tap.o${BJ} r12vol.o${BJ} scrib.o${BJ} segmnt.o${BJ} sf_2pt.o${BJ} \
              sf_2pth.o${BJ} sf_3pt.o${BJ} sf_3z.o${BJ} sf_corr.o${BJ} sf_dfz.o${BJ} sf_ds.o${BJ} \
              sf_hs.o${BJ} sf_shp.o${BJ} sf_taper.o${BJ} sf_yhat.o${BJ} sf_yhat3.o${BJ} \
              sf_zero.o${BJ} voleqdef.o${BJ}

volume: ${VOLUME_OBJS}

debug_mod.o${BJ}:   ${VOLUME_SRC}debug_mod.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}debug_mod.f

clkcoef_mod.o${BJ}:   ${VOLUME_SRC}clkcoef_mod.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}clkcoef_mod.f

blmtap.o${BJ}:   ${VOLUME_SRC}blmtap.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}blmtap.f

blmvol.o${BJ}:   ${VOLUME_SRC}blmvol.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}blmvol.f

brk_up.o${BJ}:   ${VOLUME_SRC}brk_up.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}brk_up.f

doyal78.o${BJ}:   ${VOLUME_SRC}doyal78.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}doyal78.f

dvest.o${BJ}:   ${VOLUME_SRC}dvest.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}dvest.f

f_alaska.o${BJ}:  ${VOLUME_SRC}f_alaska.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}f_alaska.f

f_ingy.o${BJ}:  ${VOLUME_SRC}f_ingy.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}f_ingy.f

f_other.o${BJ}:  ${VOLUME_SRC}f_other.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}f_other.f

f_west.o${BJ}:  ${VOLUME_SRC}f_west.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}f_west.f

fdbt_c2.o${BJ}:  ${VOLUME_SRC}fdbt_c2.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}fdbt_c2.f

formclas.o${BJ}:  ${VOLUME_SRC}formclas.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}formclas.f

fwinit.o${BJ}:   ${VOLUME_SRC}fwinit.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}fwinit.f

intl78.o${BJ}:   ${VOLUME_SRC}intl78.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}intl78.f

mrules.o${BJ}:   ${VOLUME_SRC}mrules.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}mrules.f

numlog.o${BJ}:   ${VOLUME_SRC}numlog.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}numlog.f

profile.o${BJ}:  ${VOLUME_SRC}profile.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}profile.f

r1allen.o${BJ}:    ${VOLUME_SRC}r1allen.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r1allen.f

r1byrne.o${BJ}:    ${VOLUME_SRC}r1byrne.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r1byrne.f

r1kemp.o${BJ}:    ${VOLUME_SRC}r1kemp.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r1kemp.f

r1tap.o${BJ}:    ${VOLUME_SRC}r1tap.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r1tap.f

r2oldv.o${BJ}:    ${VOLUME_SRC}r2oldv.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r2oldv.f

r2tap.o${BJ}:    ${VOLUME_SRC}r2tap.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r2tap.f

r3d2hv.o${BJ}:   ${VOLUME_SRC}r3d2hv.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r3d2hv.f

r4d2h.o${BJ}:    ${VOLUME_SRC}r4d2h.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r4d2h.f

r4vol.o${BJ}:    ${VOLUME_SRC}r4vol.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r4vol.f

r5harv.o${BJ}:   ${VOLUME_SRC}r5harv.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r5harv.f

r5tap.o${BJ}:    ${VOLUME_SRC}r5tap.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r5tap.f

r6dibs.o${BJ}:   ${VOLUME_SRC}r6dibs.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r6dibs.f

r6fix.o${BJ}:   ${VOLUME_SRC}r6fix.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r6fix.f

r6vol.o${BJ}:    ${VOLUME_SRC}r6vol.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r6vol.f

r6vol1.o${BJ}:   ${VOLUME_SRC}r6vol1.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r6vol1.f

r6vol2.o${BJ}:   ${VOLUME_SRC}r6vol2.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r6vol2.f

r6vol3.o${BJ}:   ${VOLUME_SRC}r6vol3.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r6vol3.f

r8init.o${BJ}:    ${VOLUME_SRC}r8init.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r8init.f

r8vol.o${BJ}:    ${VOLUME_SRC}r8vol.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r8vol.f

r8vol1.o${BJ}:   ${VOLUME_SRC}r8vol1.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r8vol1.f

r8vol2.o${BJ}:   ${VOLUME_SRC}r8vol2.f ${VOLUME_SRC}R8DIB.INC
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r8vol2.f

r9clark.o${BJ}:   ${VOLUME_SRC}r9clark.f ${VOLUME_SRC}r9coeff.inc
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r9clark.f

r9init.o${BJ}:    ${VOLUME_SRC}r9init.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r9init.f

r9logs.o${BJ}:    ${VOLUME_SRC}r9logs.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r9logs.f

r9vol.o${BJ}:    ${VOLUME_SRC}r9vol.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r9vol.f

r10d2h.o${BJ}:    ${VOLUME_SRC}r10d2h.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r10d2h.f

r10tap.o${BJ}:   ${VOLUME_SRC}r10tap.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r10tap.f

r10tapo.o${BJ}:   ${VOLUME_SRC}r10tapo.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r10tapo.f

r10vol.o${BJ}:   ${VOLUME_SRC}r10vol.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r10vol.f

r10vol1.o${BJ}:   ${VOLUME_SRC}r10vol1.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r10vol1.f

r10volo.o${BJ}:   ${VOLUME_SRC}r10volo.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r10volo.f

r12tap.o${BJ}:   ${VOLUME_SRC}r12tap.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r12tap.f

r12vol.o${BJ}:   ${VOLUME_SRC}r12vol.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}r12vol.f

scrib.o${BJ}:    ${VOLUME_SRC}scrib.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}scrib.f

segmnt.o${BJ}:   ${VOLUME_SRC}segmnt.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}segmnt.f

sf_2pt.o${BJ}:   ${VOLUME_SRC}sf_2pt.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_2pt.f

sf_2pth.o${BJ}:   ${VOLUME_SRC}sf_2pth.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_2pth.f

sf_3pt.o${BJ}:   ${VOLUME_SRC}sf_3pt.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_3pt.f

sf_3z.o${BJ}:    ${VOLUME_SRC}sf_3z.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_3z.f

sf_corr.o${BJ}:  ${VOLUME_SRC}sf_corr.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_corr.f

sf_dfz.o${BJ}:   ${VOLUME_SRC}sf_dfz.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_dfz.f

sf_ds.o${BJ}:    ${VOLUME_SRC}sf_ds.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_ds.f

sf_hs.o${BJ}:    ${VOLUME_SRC}sf_hs.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_hs.f

sf_shp.o${BJ}:  ${VOLUME_SRC}sf_shp.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_shp.f

sf_taper.o${BJ}: ${VOLUME_SRC}sf_taper.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_taper.f

sf_yhat.o${BJ}:  ${VOLUME_SRC}sf_yhat.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_yhat.f

sf_yhat3.o${BJ}: ${VOLUME_SRC}sf_yhat3.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_yhat3.f

sf_zero.o${BJ}:  ${VOLUME_SRC}sf_zero.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}sf_zero.f

voleqdef.o${BJ}:  ${VOLUME_SRC}voleqdef.f
	${FC} -c ${FFLAGS} ${VOLUME_SRC}voleqdef.f