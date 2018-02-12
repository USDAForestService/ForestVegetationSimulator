
all : cit01 cit02

cit01 : 
	@echo Test cit01, basic functionality 
	-../../bin/qFVSci --keywordfile=cit01.key
	grep -v "\-999" cit01.sum      > cit01.tmp
	grep -v "\-999" cit01.sum.save > cit01.tmp.save
	-diff -w cit01.tmp cit01.tmp.save
	rm cit01.tmp cit01.tmp.save
	@echo Test cit01 ended

cit02 : 
	@echo Test cit02, stop/restart
	-../../bin/qFVSci --keywordfile=cit01.key --stoppoint=2,2040,cit.stop
	-../../bin/qFVSci --restart=cit.stop
	grep -v "\-999" cit01.sum      > cit01.tmp
	grep -v "\-999" cit01.sum.save > cit01.tmp.save
	-diff -w cit01.tmp cit01.tmp.save
	rm cit01.tmp cit01.tmp.save
	@echo Test cit02 ended
	
