
all : ie econ fire clim sn
	
ie : 	
	@echo Test ie database read and write
	rm -f FVSieOut.db
	-../../bin/FVSQie --keywordfile=ie.key 
	-sqlite3 FVSieOut.db < sqliteTestFVSieOut.sql > ie.db_dump.txt
	-diff -w ie.db_dump.txt ie.db_dump.txt.save
	@echo Test ie ended

econ :
	@echo Test econ database read and write
	rm -f TestDataBase.db
	-../../bin/FVSQie --keywordfile=TestDataBase.key 
	-sqlite3 TestDataBase.db "select Year,Species,Total_Value from FVS_EconHarvestValue;" > TestDataBase.dump
	-sqlite3 TestDataBase.db "select pnv,Mrch_BoardFoot_Volume from FVS_EconSummary;" >> TestDataBase.dump	
	-diff -w TestDataBase.dump TestDataBase.dump.save
	@echo Test econ ended
	
	 
fire : 
	@echo fireTest database read and write
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
	
sn : 
	@echo sn variant database read and write, tests a lot of tables
	rm -f snout.db
	-../../bin/FVSQsn --keywordfile=sn.key  
	-sqlite3 snout.db ".tables" > snout.txt
	-sqlite3 snout.db "select count(*) from FVS_ATRTList        ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_Down_Wood_Cov   ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_Mortality       ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_BurnReport      ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_Down_Wood_Vol   ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_PotFire_East    ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_Carbon          ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_EconHarvestValue;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_SnagSum         ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_Cases           ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_EconSummary     ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_Summary_East    ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_Consumption     ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_Fuels           ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_TreeList        ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_CutList         ;" >> snout.txt
	-sqlite3 snout.db "select count(*) from FVS_Hrv_Carbon      ;" >> snout.txt
	-diff -w snout.txt snout.txt.save
	

