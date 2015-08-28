"""
Demonstrate the basic functionality of the the FVS Python inteface.

Run several iterations of a keyword file that has the random seed disabled
and plot the resulting variable growth curves
"""

import os
import sys
import time
import random
import numpy
import matplotlib

import pyfvspnc as fvs

# Matplotlib backend must be specified before importing pyplot
# Comment the following line to use the default backend.
matplotlib.use('Agg')
import matplotlib.pyplot as plt

#ensure runs will bomb if the previous call failed, ensure all files are closed
fvs.filclose()

#how many reps to run
reps = 20

#what keyword file are we using
##NOTE: Currently the FVS API expects a treelist file with the same basename
#           if the dbs extension is not being used.
kwd = os.path.join(os.path.dirname(__file__),'pnt01.key')

cycle_len = 5

st = time.clock()

trees = open('trees.txt','w')

#Initialize a Numpy integer array to collect the summary records
summary = numpy.zeros((reps,40, 20), dtype='i')

#The current FVS API requires using command line style arguments passed to the
#setcommandline subroutine to initialize the FVS arrays
cl = '--keywordfile=%s' % (kwd,)
for rep in xrange(reps):
    
    #initialize the run
    i = fvs.fvssetcmdline(cl)
    
    num_cycles = fvs.contrl.ncyc
    
    # fvs.contrl.ncyc = numpy.int(num_cycles)
    fvs.ransed(True,random.random())
    
    #print 'FVS Returned with exit code %d' % i
    
    cycle = 0
    while 1:
        #set the stop point to the end of the next cycle
        cycle_year = fvs.contrl.iy[cycle]
        fvs.fvssetstoppointcodes(6,cycle_year)
        
        #call the main FVS grower loop
        rtn = fvs.fvs()
        
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
        
        if cycle >= num_cycles: break
        
    #close all IO files
    fvs.filclose()
    
    #collect the summary values
    for i in range(num_cycles):
        summary[rep,i,:] = fvs.fvssummary(i+1) #+1 since Fortran indexes start at 1
    
    #Print the periodic growth for this iteration to show progress
    print '%3d CUFT %s' % (rep,' '.join('%5d' % v for v in summary[rep,:num_cycles, 3]))
    # print 'BDFT',','.join('%6d' % v for v in summary[:,5])

et = time.clock()

print '%d reps; total elapsed time: %.2f, %.3f second per rep' % (reps, et - st, (et - st) / (rep+1))

fig, ax = plt.subplots( nrows=1, ncols=1 )

v = 3
#plot the cubic foot growth curves for each iteration
mean_curve = numpy.mean(summary[:,:num_cycles,v],axis=0)
print summary[0,:num_cycles,0]
ax.plot(summary[0,:num_cycles,0],mean_curve)

if rep>0:
    ax.boxplot(
            summary[:,:num_cycles,v]
            ,positions=summary[0,:num_cycles,0])
    
fig.savefig('demo.png')
plt.close(fig)