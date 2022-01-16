# $Id: makefile 734 2013-03-22 22:39:10Z ncrookston.fs@gmail.com $

all : Hardwood

Hardwood :
	@echo "Test Hardwood (1 stands)"
	-../../bin/FVSon --keywordfile=Hardwood.key
	grep -v "\-999" Hardwood.sum      > Hardwood.tmp
	grep -v "\-999" Hardwood.sum.save > Hardwood.tmp.save
	-diff -w Hardwood.tmp Hardwood.tmp.save
	rm Hardwood.tmp Hardwood.tmp.save
	rm Hardwood.out Hardwood.trl
	@echo "Test Hardwood (1 stand) ended"

