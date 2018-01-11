
all : ie ie2 ne sn fireTest

ie : 
	@echo Test ie database read
	-../../bin/FVSQie --keywordfile=ie.key 
	@echo Test ie ended
	
ie2 : 	
	@echo Test ie2 database read and write
	rm -f FVSieOut.db
	-../../bin/FVSQie --keywordfile=ie2.key 
	-sqlite3 FVSieOut.db < sqliteTestFVSieOut.sql > ie2.db_dump.txt
	-diff -w ie2.db_dump.txt ie2.db_dump.txt.save
	@echo Test ie2 ended
	
	
ne : 
	@echo Test ne database read
	-../../bin/FVSne --keywordfile=ne.key 
	@echo Test ne ended
	
sn : 
	@echo Test ie database read
	-../../bin/FVSsn --keywordfile=sn.key 
	@echo Test sn ended
	 
fireTest : 
	@echo fireTest  database read and write
	rm -f FVSieFireOutput.db
	-../../bin/FVSQie --keywordfile=fireTest.key 
	-sqlite3 FVSieFireOutput.db < sqliteTestFireOut.sql > fireTest.db_dump.txt
	-diff -w fireTest.db_dump.txt fireTest.db_dump.txt.save
	@echo fireTest ended
	
 
 

