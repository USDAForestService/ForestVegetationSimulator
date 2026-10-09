
all : acb01 acb02

acb01 : 
	@echo Test acb01, many all-cycle activities over 20 cycles
	../../bin/FVSca --keywordfile=allcycle_budget.key
	grep -v "\-999" allcycle_budget.sum      > allcycle_budget.tmp
	grep -v "\-999" allcycle_budget.sum.save > allcycle_budget.tmp.save
	-diff -w allcycle_budget.tmp allcycle_budget.tmp.save
	rm allcycle_budget.tmp allcycle_budget.tmp.save
	grep "^ *CMPU *XBBA.*DONE IN" allcycle_budget.out > allcycle_budget.xbba
	-diff -w allcycle_budget.xbba allcycle_budget.xbba.save
	rm allcycle_budget.xbba
	@echo Test acb01 ended
	
acb02 : 
	@echo Test acb02, many all-cycle activities, stop/restart
	../../bin/FVSca --keywordfile=allcycle_budget.key --stoppoint=2,2090,acb.stop
	../../bin/FVSca --restart=acb.stop
	grep -v "\-999" allcycle_budget.sum      > allcycle_budget.tmp
	grep -v "\-999" allcycle_budget.sum.save > allcycle_budget.tmp.save
	-diff -w allcycle_budget.tmp allcycle_budget.tmp.save
	rm allcycle_budget.tmp allcycle_budget.tmp.save
	grep "^ *CMPU *XBBA.*DONE IN" allcycle_budget.out > allcycle_budget.xbba
	-diff -w allcycle_budget.xbba allcycle_budget.xbba.save
	rm allcycle_budget.xbba
	@echo Test acb02 ended

clean : 
	rm -rfv acb.stop allcycle_budget.out allcycle_budget.sng allcycle_budget.sum FVS_Data.db FVSOut.db
