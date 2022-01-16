
all : net01 net02

net01 : 
	@echo Test net01, basic functionality 
	../../bin/FVSne --keywordfile=net01.key 
	grep -v "\-999" net01.sum      > net01.tmp
	grep -v "\-999" net01.sum.save > net01.tmp.save
	-diff -w net01.tmp net01.tmp.save
	rm net01.tmp net01.tmp.save
	@echo Test net01 ended
	
net02 : 
	@echo Test net02, stop/restart, output should match net01
	../../bin/FVSne --keywordfile=net01.key --stoppoint=2,2040,net.stop
	../../bin/FVSne --restart=net.stop
	grep -v "\-999" net01.sum      > net01.tmp
	grep -v "\-999" net01.sum.save > net01.tmp.save
	-diff -w net01.tmp net01.tmp.save
	rm net01.tmp net01.tmp.save
	@echo Test net02 ended
		 
 
 

