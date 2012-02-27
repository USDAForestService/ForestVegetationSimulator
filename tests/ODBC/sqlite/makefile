
all : ie ne sn fireTest

ie : 
	@echo Test ie database read
	-../../../bin/FVSiec --keywordfile=ie.key 
	@echo Test ie ended
	@echo Test ie2 database read and write
	rm -f FVSieOut.db
	-../../../bin/FVSiec --keywordfile=ie2.key 
	-sqlite3 FVSieOut.db .dump > ie2.db_dump.txt
	@echo This diff should show timestamp differences
	-diff -w ie2.db_dump.txt ie2.db_dump.txt.save
	@echo Test ie2 ended
	
	
ne : 
	@echo Test ne database read
	-../../../bin/FVSne --keywordfile=ne.key 
	@echo Test ne ended
	
sn : 
	@echo Test ie database read
	-../../../bin/FVSsn --keywordfile=sn.key 
	@echo Test sn ended
	 
fireTest : 
	@echo fireTest  database read and write
	rm -f FVSieFireOutput.db
	-../../../bin/FVSiec --keywordfile=fireTest.key 
	-sqlite3 FVSieFireOutput.db .dump > fireTest.db_dump.txt
	@echo This diff should show timestamp differences
	-diff -w fireTest.db_dump.txt fireTest.db_dump.txt.save
	@echo fireTest ended
	
 
 

