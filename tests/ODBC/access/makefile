
all : ie ne sn fireTest

ie : 
	@echo Test ie database read and write
	rm -f FVS_Output.accdb
	rm -fr DataDump
	cscript.exe MakeNewAccessDataBase.vbs
	-../../../bin/FVSiec --keywordfile=ie.key
	cscript.exe ExportAccessQueriesToCSV.vbs /databaseFile:FVS_Output.accdb /outputDirectory:DataDump ieTestQueries.txt
	-diff -qsw DataDump ExpectedValues
	@echo Test ie ended
	
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
	rm -f FVS_Output.accdb
	cscript.exe MakeNewAccessDataBase.vbs
	-../../../bin/FVSiec --keywordfile=fireTest.key 
	@echo fireTest ended
	
