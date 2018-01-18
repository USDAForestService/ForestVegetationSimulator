
all : ie econ fire
	
ie : 	
	@echo Test ie database read and write
	rm -f FVSieOut.db
	-../../bin/FVSQie --keywordfile=ie.key 
	-sqlite3 FVSieOut.db < sqliteTestFVSieOut.sql > ie.db_dump.txt
	-diff -w ie.db_dump.txt ie.db_dump.txt.save
	@echo Test ie ended

econ :
	@echo Test ie2 database read and write
	rm -f TestDataBase.db
	-../../bin/FVSQie --keywordfile=TestDataBase.key 
	-sqlite3 TestDataBase.db "select Year,Species,Total_Value from FVS_EconHarvestValue;" > TestDataBase.dump
	-sqlite3 TestDataBase.db "select pnv,Mrch_BoardFoot_Volume from FVS_EconSummary;" >> TestDataBase.dump	
	-diff -w TestDataBase.dump TestDataBase.dump.save
	@echo Test econ ended
	
	 
fire : 
	@echo fireTest  database read and write
	rm -f FVSieFireOutput.db
	-../../bin/FVSQie --keywordfile=fireTest.key 
	-sqlite3 FVSieFireOutput.db < sqliteTestFireOut.sql > fireTest.db_dump.txt
	-diff -w fireTest.db_dump.txt fireTest.db_dump.txt.save
	@echo fireTest ended
	
clim : 
	@echo climate database read and write
	rm -f ClimOut.db
	-../../bin/FVSQie --keywordfile=Clim.key  
	-sqlite3 ClimOut.db "Select Species,Count(Species) from FVS_Climate group by Species;" > Clim.db.txt 
	-diff -w Clim.db.txt Clim.db.txt.save
	

