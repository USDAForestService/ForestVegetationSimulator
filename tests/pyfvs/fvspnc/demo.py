"""
Demonstrate the basic functionality of the the FVS Python inteface.

Run several iterations of a keyword file that has the random seed disabled
and plot the resulting variable growth curves
"""

import os
import sys
import time
import numpy

try:
    import pylab
    plot = True

except:
    print 'Plotting requires the matplotlib Python package'
    plot = False

##FIXME:
bin = r'C:\workspace\projects\open-fvs\local.dev\fvs\bin'
sys.path.insert(0, bin)
    
import pyfvspnc as fvs

#ensure runs will bomb if the previous call failed, ensure all files are closed
fvs.filclose()

##TODO: make the keyword file a template so some of the variables can be manipulated

#how many reps to run
reps = 10

#what keyword file are we using
##NOTE: Currently the FVS API expects a treelist file with the same basename
#           if the dbs extension is not being used.
kwd = r'C:\workspace\projects\open-fvs\local.dev\fvs\test\pyfvs\fvspnc\pnt01.key'  #implies a matching 'pnt01.tre' treelist

num_cycles = 10  ##TODO: add this to a keyword template
year_zero = 1990  ##TODO: get this from FVS
cycle_len = 10

#Initialize a Numpy integer array to collect the summary records
summary = numpy.zeros((reps,num_cycles + 1, 20), dtype='i')

st = time.clock()

trees = open('trees.txt','w')

#The current FVS API requires using command line style arguments passed to the
#setcommandline subroutine to initialize the FVS arrays
cl = '--keywordfile=%s' % (kwd,)
for cnt in xrange(reps):

    #initialize the run
    i = fvs.fvssetcmdline(cl)
    
    #print 'FVS Returned with exit code %d' % i
    
    cycle = 0
    cycle_year = year_zero
    while 1:
        #set the stop point to the end of the next cycle
        fvs.fvssetstoppointcodes(6,cycle_year)
        
        #call the main FVS grower loop
        rtn = fvs.fvs()
        
        if rtn != 0:
            raise ValueError('FVS return with error code %d' % rtn)
        
        sp = fvs.fvsgetstoppointcodes()
        if sp==(0,0): break
        
        ntrees,ncycles,nplots,maxtrees,maxspecies,maxplots,maxcycles = fvs.fvsdimsizes()
        attrs = numpy.zeros(ntrees)
        fvs.fvstreeattr('dbh',3,'get',attrs)
        trees.write('Year %d\n' % cycle_year)
        trees.write('DBH\n')
        trees.write('\n'.join('%.2f' % a for a in attrs))
        trees.write('\n')
        
        cycle_year += cycle_len
        cycle += 1
        
        if cycle >= num_cycles+1: break
        
    #close all IO files
    #NOTE: this was added to fvssetcmdline in Open-FVS @ r493
    fvs.filclose()
    
    #collect the summary values
    for i in xrange(1, num_cycles + 2):
        #by passing a slice of the numpy array, the f2py wrappers ensure the 
        #array is modified inplace, this could be changed in the future so
        #that summaries, treelists, etc. are returned as variables.  However,
        #this would likely incur some modest and unecessary overhead.
        r = fvs.fvssummary(summary[cnt,i - 1], i)
    
    
    #Print the periodic growth for this iteration to show progress
    print '%3d CUFT %s' % (cnt,' '.join('%5d' % v for v in summary[cnt,:, 3]))
    # print 'BDFT',','.join('%6d' % v for v in summary[:,5])

et = time.clock()

print '%d reps; total elapsed time: %.2f, %.3f second per rep' % (reps, et - st, (et - st) / cnt)

if plot:
    #plot the cubic foot growth curves for each iteration
    mean_curve = numpy.mean(summary[:,:,3],axis=0)
    # for s in summary[:]:
        # pylab.plot(s[:,0],s[:,3])
    pylab.plot(summary[0,:,0],mean_curve)
        
    pylab.boxplot(summary[:,:,3],positions=summary[0,:,0])
    pylab.show()
