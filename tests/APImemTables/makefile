# Memory-mode output tables (fvsSetMemoryTables and the fvsTable* API in
# dbsqlite/dbstables.f), read through the FVSie shared library.
# Each test prints PASS or FAIL and exits non-zero on failure.

all : ctypes R

ctypes :
	@echo Test memtables_ctypes: memory tables match the SQLite file output
	-python3 memtables_ctypes.py ../../bin/FVSie.so
	@echo Test memtables_ctypes ended

R :
	@echo Test memtables_r: read memory tables from R with .Fortran
	-Rscript memtables_r.R ../../bin/FVSie.so
	@echo Test memtables_r ended
