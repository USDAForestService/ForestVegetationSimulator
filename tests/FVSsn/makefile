
all : snt01 snt02

snt01 : 
	@echo Test snt01, basic functionality 
	-../../bin/FVSsn < snt01.rsp
	grep -v "\-999" snt01.sum      > snt01.tmp
	grep -v "\-999" snt01.sum.save > snt01.tmp.save
	-diff -w snt01.tmp snt01.tmp.save
	rm snt01.tmp snt01.tmp.save
	@echo Test snt01 ended
	
snt02 : 
	@echo Test snt02, stop/restart, output should match snt01
	-../../bin/FVSsn --keywordfile=snt01.key --stoppoint=2,2020,snt.stop
	-../../bin/FVSsn --restart=snt.stop
	grep -v "\-999" snt01.sum      > snt01.tmp
	grep -v "\-999" snt01.sum.save > snt01.tmp.save
	-diff -w snt01.tmp snt01.tmp.save
	rm snt01.tmp snt01.tmp.save
	@echo Test snt02 ended
		 
 
 

