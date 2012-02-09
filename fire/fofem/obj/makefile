CFLAGS=-c -DANSI -DCMP${CC}
FOFEM_SRC = ./../src/

objects = fof_cm.o${BJ}   \
          fof_mrt.o${BJ}  \
          fof_hsf.o${BJ}  \
          fof_ci.o${BJ}   \
          fof_co.o${BJ}   \
          fof_duf.o${BJ}  \
          fof_lem.o${BJ}  \
          fof_bcm.o${BJ}  \
          bur_bov.o${BJ}  \
          bur_brn.o${BJ}  \
          fof_util.o${BJ} \
          fof_sgv.o${BJ}  \
          fof_sh.o${BJ}   \
          fof_sd.o${BJ}   \
          fof_se.o${BJ}   \
          fof_sha.o${BJ}  \
          fof_soi.o${BJ}  \
          fof_unix.o${BJ} \
          fm_fofem.o${BJ}

fofem: $(objects)

fof_cm.o${BJ}: ${FOFEM_SRC}fof_cm.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_mrt.o${BJ}: ${FOFEM_SRC}fof_mrt.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_hsf.o${BJ}: ${FOFEM_SRC}fof_hsf.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_ci.o${BJ}: ${FOFEM_SRC}fof_ci.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_co.o${BJ}: ${FOFEM_SRC}fof_co.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_duf.o${BJ}: ${FOFEM_SRC}fof_duf.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_lem.o${BJ}: ${FOFEM_SRC}fof_lem.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_bcm.o${BJ}: ${FOFEM_SRC}fof_bcm.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

bur_bov.o${BJ}: ${FOFEM_SRC}bur_bov.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

bur_brn.o${BJ}: ${FOFEM_SRC}bur_brn.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_util.o${BJ}: ${FOFEM_SRC}fof_util.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_sgv.o${BJ}: ${FOFEM_SRC}fof_sgv.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_sh.o${BJ}: ${FOFEM_SRC}fof_sh.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_sd.o${BJ}: ${FOFEM_SRC}fof_sd.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_se.o${BJ}: ${FOFEM_SRC}fof_se.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_sha.o${BJ}: ${FOFEM_SRC}fof_sha.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_soi.o${BJ}: ${FOFEM_SRC}fof_soi.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fof_unix.o${BJ}: ${FOFEM_SRC}fof_unix.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@

fm_fofem.o${BJ}: ${FOFEM_SRC}fm_fofem.c ${FOFEM_SRC}*.h
	$(CC) $(CFLAGS) $< -o $@