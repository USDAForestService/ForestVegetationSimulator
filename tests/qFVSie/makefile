
# $Id: makefile 1665 2016-02-01 21:32:22Z nickcrookston $

all : iet01 iet02 iet03 iedb econdb firedb climdb \
 ffe_svs climate climatePutGet maxActs 

iet01 : 
	@echo Test iet01, basic functionality 
	-../../bin/qFVSie < iet01.rsp
	grep -v "\-999" iet01.sum      > iet01.tmp
	grep -v "\-999" iet01.sum.save > iet01.tmp.save
	-diff -w iet01.tmp iet01.tmp.save
	rm iet01.tmp iet01.tmp.save
	@echo Test iet01 ended
	
 
iet02 : 
	@echo Test iet02, test use of stop/restart, same as iet01. 
	-../../bin/qFVSie --keywordfile=iet01.key --stoppoint=2,2040,iet02.stop
	-../../bin/qFVSie --restart=iet02.stop
	grep -v "\-999" iet01.sum      > iet01.tmp
	grep -v "\-999" iet01.sum.save > iet01.tmp.save
	-diff -w iet01.tmp iet01.tmp.save
	rm iet01.tmp iet01.tmp.save
	@echo Test iet02 ended
	
iet03 : 
	@echo Test iet03, simple test of ffe with stop/restart
	-../../bin/qFVSie --keywordfile=iet03.key --stoppoint=1,2003,iet03.stop
	-../../bin/qFVSie --restart=iet03.stop
	grep -v "\-999" iet03.sum      > iet03.tmp
	grep -v "\-999" iet03.sum.save > iet03.tmp.save
	-diff -w iet03.tmp iet03.tmp.save
	rm iet03.tmp iet03.tmp.save
	@echo Test iet03 ended
	
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

ffe_svs:
	@echo ffe_svs: Test FFE with SVS and stop/restart
	-../../bin/qFVSie --keywordfile=ffe_svs.key --stoppoint=1,2003,ffe_svs.stop
	-../../bin/qFVSie --restart=ffe_svs.stop
	-diff -w ffe_svs_007.svs ffe_svs_007.svs.save
	-diff -w ffe_svs_index.svs ffe_svs_index.svs.save
#	rm ffe_svs_*.svs ffe_svs_*.grd
	@echo Test ffe_svs ended
	

climate:
	@echo Test Climate-FVS 
	-../../bin/qFVSie --keywordfile=climate.key
	grep -v "\-999" climate.sum      > climate.tmp
	grep -v "\-999" climate.sum.save > climate.tmp.save
	-diff -w climate.tmp climate.tmp.save
	rm climate.tmp climate.tmp.save
	@echo Test Climate-FVS ended
	
climatePutGet:
	@echo Test Climate-FVS with PutGet
	-../../bin/qFVSie --keywordfile=climate.key --stoppoint=1,2040,climate.stop
	-../../bin/qFVSie --restart=climate.stop
	grep -v "\-999" climate.sum      > climate.tmp
	grep -v "\-999" climate.sum.save > climate.tmp.save
	-diff -w climate.tmp climate.tmp.save
	rm climate.tmp climate.tmp.save
	@echo Test Climate-FVS with PutGet ended

maxActs:
	@echo Test having many compute statements
	-../../bin/qFVSie --keywordfile=maxActs.key
	@echo This grep command should not find the string
	grep "OPTION/ACTIVITY STORAGE AREA IS FULL" maxActs.out 
	rm maxActs.out 
	@echo Test having many compute statements ended

