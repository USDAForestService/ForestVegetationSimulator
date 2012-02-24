
all : ie ne sn fireTest

ie : 
	@echo Test ie database read
	-../../../bin/FVSiec --keywordfile=ie.key 
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
	rm -f FVS_Output.db
	-../../../bin/FVSiec --keywordfile=fireTest.key 
	@echo fireTest ended
	
 
 

