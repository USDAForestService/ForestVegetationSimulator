all : opt01 opt02

opt01 : 
	@echo Test opt01, basic functionality 
	-../../bin/FVSop --keywordfile=opt01.key
	grep -v "\-999" opt01.sum      > opt01.tmp
	grep -v "\-999" opt01.sum.save > opt01.tmp.save
	-diff -w opt01.tmp opt01.tmp.save
	rm opt01.tmp opt01.tmp.save
	@echo Test opt01 ended
	
opt02 : 
	@echo Test opt02, stop/restart, output should match opt01
	-../../bin/FVSop --keywordfile=opt01.key --stoppoint=2,2020,opt.stop
	-../../bin/FVSop --restart=opt.stop
	grep -v "\-999" opt01.sum      > opt01.tmp
	grep -v "\-999" opt01.sum.save > opt01.tmp.save
	-diff -w opt01.tmp opt01.tmp.save
	rm opt01.tmp opt01.tmp.save
	@echo Test opt02 ended
		 
 
 

