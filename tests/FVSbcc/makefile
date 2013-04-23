
# $Id: makefile 734 2013-03-22 22:39:10Z ncrookston.fs@gmail.com $

all : climbc mgmtbc wsbw

climbc : 
	@echo Test ClimBC, basic functionality 
	-../../bin/FVSbcc < ClimBC.rsp
	grep -v "\-999" ClimBC.sum      > ClimBC.tmp
	grep -v "\-999" ClimBC.sum.save > ClimBC.tmp.save
	-diff -w ClimBC.tmp ClimBC.tmp.save
	rm ClimBC.tmp ClimBC.tmp.save
	@echo Test ClimBC ended
	
 
mgmtbc : 
	@echo Test MgmtBC 
	-../../bin/FVSbcc < MgmtBC.rsp
	grep -v "\-999" MgmtBC.sum      > MgmtBC.tmp
	grep -v "\-999" MgmtBC.sum.save > MgmtBC.tmp.save
	-diff -w MgmtBC.tmp MgmtBC.tmp.save
	rm MgmtBC.tmp MgmtBC.tmp.save
	@echo Test MgmtBC ended
	
wsbw : 
	@echo Test WSBW, test of WSBW
	-../../bin/FVSbcc  < WSBW-A.rsp
	grep -v "\-999" WSBW-A.sum      > WSBW-A.tmp
	grep -v "\-999" WSBW-A.sum.save > WSBW-A.tmp.save
	-diff -w WSBW-A.tmp WSBW-A.tmp.save
	rm WSBW-A.tmp WSBW-A.tmp.save
	@echo Test WSBW ended
