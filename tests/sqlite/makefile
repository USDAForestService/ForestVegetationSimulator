
all : iedb econdb firedb climdb sn
	
iedb : 	
	@echo Test ie database read and write
	rm -f FVSieOut.db
	-../../bin/qFVSie --keywordfile=ie.key 
	-sqlite3 FVSieOut.db < sqliteTestFVSieOut.sql > ie.db_dump.txt
	-diff -w ie.db_dump.txt ie.db_dump.txt.save
	@echo Test ie ended

econdb :
	@echo Test econ database read and write
	rm -f TestDataBase.db
	-../../bin/qFVSie --keywordfile=TestDataBase.key 
	-sqlite3 TestDataBase.db "select Year,Species,Total_Value from FVS_EconHarvestValue;" > TestDataBase.dump
	-sqlite3 TestDataBase.db "select round(pnv,1),Mrch_BoardFoot_Volume from FVS_EconSummary;" >> TestDataBase.dump	
	-diff -w TestDataBase.dump TestDataBase.dump.save
	@echo Test econ ended
	
	 
firedb : 
	@echo fireTest database read and write
	rm -f FVSieFireOutput.db
	-../../bin/qFVSie --keywordfile=fireTest.key 
	-sqlite3 FVSieFireOutput.db < sqliteTestFireOut.sql > fireTest.db_dump.txt
	-diff -w fireTest.db_dump.txt fireTest.db_dump.txt.save
	@echo fireTest ended
	
climdb : 
	@echo climate database read and write
	rm -f ClimOut.db
	-../../bin/qFVSie --keywordfile=Clim.key  
	-sqlite3 ClimOut.db "Select Species,Count(Species) from FVS_Climate group by Species;" > Clim.db.txt 
	-diff -w Clim.db.txt Clim.db.txt.save
	
sn : 
	@echo sn variant database read and write, tests a lot of tables
	rm -f snout.db
	-../../bin/qFVSsn --keywordfile=sn.key  
	-sqlite3 snout.db < snTablesTest.sql > snout.txt
	-diff -w snout.txt snout.txt.save
	

