
all : crt01 

crt01 : 
	@echo Test crt01, basic functionality 
	-../../bin/FVScrc --keywordfile=crt01.key
	grep -v "\-999" crt01.sum      > crt01.tmp
	grep -v "\-999" crt01.sum.save > crt01.tmp.save
	diff -w crt01.tmp crt01.tmp.save
	rm crt01.tmp crt01.tmp.save
	@echo Test crt01 ended
	