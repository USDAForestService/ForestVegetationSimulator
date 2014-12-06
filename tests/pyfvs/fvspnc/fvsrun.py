"""
Demonstrates calling fvs from python as well as a method to bootstrap the plot
records.
"""

import os
import sys
import shutil
import time
import numpy

try:
    import pylab
    plot = True

except:
    print 'Plotting requires the matplotlib Python package'
    plot = False

import pyfvspnc as fvs

def readtrees(treelist='pnt01.tre.save'):
    tl = open(treelist).readlines()
    plots = {}
    for l in tl:
        plotid = l.split()[2]
        if not plots.has_key(plotid):
            plots[plotid] = []
            
        plots[plotid].append(l.replace(plotid,'xidx').rstrip())
    
    return plots

def resampleplots(plots):
    newplots = {}
    plotids = plots.keys()
    resample = numpy.random.choice(plotids,len(plotids),replace=True)
    for i in xrange(len(resample)):
        newplots[i] = plots[resample[i]]
    
    return newplots

def writetrees(plots,treelist='pnt01.tre'):
    with open(treelist,'w') as tl:
        for id,trees in plots.items():
            for treerow in trees:
                treerow = treerow.replace('xidx','%04d' % id)
                tl.write('%s\n' % treerow)
    
    
reps = 10
kwds = ('pnt01.key',)  #'pnt02.key')
#kwds = [os.path.join(root,kwd) for kwd in kwds]

num_cycles = 10
cycle_len = 10
year_zero = 1990

#Create a numpy recarray of summary variables
sumvars = ('year','age','tpa','tcuft','mcuft','mbdft','rtpa','rtcuft'
        ,'rmcuft','rmbdft','baa','ccf','topht','perlen','accr','mort'
        ,'samwt','cvrtype','sizecls','stkcls')

summary = numpy.zeros(
        (reps, num_cycles+1)
        , dtype=zip(sumvars,['I4']*len(sumvars))
        )

#temporary array to hold current growth cycle summary variables
repsum = numpy.zeros(len(sumvars),dtype='I4')

trees = numpy.zeros(1,dtype=[('year','i4'),('cycle','i4'),('stop','i4'),('spp','a2'),('tpa','f4')])

##TODO: build a rec array to store cycle tree values

st = time.clock()
run_id = 0
for kwd in kwds:
    plottrees = readtrees('pnt01.tre.save')

    for x in xrange(reps):
        #bootstrap the plot records
        tls = resampleplots(plottrees)
        writetrees(tls)
        
        #cl = numpy.array((1,),dtype='a100')
        cl = '--keywordfile=%s' % (kwd,)

        #initialize the run
        i = fvs.fvssetcmdline(cl)
        
        #get the size of data arrays
        ## fvsDimSizes(ntrees,ncycles,nplots,maxtrees,maxspecies,maxplots,maxcycles)
        dim_sizes = fvs.fvsdimsizes()
        
        #get a list of species codes for lookup
        ## fvsSpeciesCode(fvs_code,fia_code,plant_code,indx,nchfvs,nchfia,nchplant,rtnCode)
        spp_codes = numpy.array(map(fvs.fvsspeciescode
                ,range(1,dim_sizes[4]+1)))[:,0]

        cycle_year = year_zero

        #get trees
#        tn = fvs.contrl.itrn
#        trees.resize(trees.shape[0]+tn)
#        trees['cycle'][-tn:] = 0
#        trees['year'][-tn:] = cycle_year
#        #trees['tpa'][-tn:] = fvs.arrays.prob[:tn]
#        #trees['spp'][-tn:] = spp_codes[fvs.arrays.isp[:tn]]
#
#        print('{:>6s}{:>5s}{:>10s}'.format('year','spp','tpa'))
#        print('{:>6d}{:>5s}{:>10.3f}'.format(trees[-tn:]['year'][0],trees[-tn:]['spp'][0],trees[-tn:]['tpa'][0]))
#        print('{:>6d}{:>5s}{:>10.3f}'.format(trees[-tn:]['year'][0],trees[-tn:]['spp'][-1],trees[-tn:]['tpa'][-1]))
            
        #loop through growth cycles, collecting summary and tree stats
        for cycle in range(num_cycles+1):
            #set the stop point to the end of the next cycle
            fvs.fvssetstoppointcodes(6,cycle_year)

            #call the FVS grower loop
            rtn = fvs.fvs()
            #print 'FVS Returned with exit code %d' % rtn

            if rtn == 1:
                raise ValueError('FVS return with error code %d' % rtn)

            sp = fvs.fvsgetstoppointcodes()
            cycle_year += cycle_len
            if sp==(0,0): break

            repsum = fvs.fvssummary(cycle+1) #cycle+1 - Fortran is 1 based indexing
            #populate the summary recarray with this cycle
            summary[run_id,cycle] = repsum[:]

            #get trees
            tn = fvs.contrl.itrn
            trees.resize(trees.shape[0]+tn)
            trees['cycle'][-tn:] = cycle+1
            trees['year'][-tn:] = cycle_year
            trees['stop'][-tn:] = sp[0]
            trees['tpa'][-tn:] = fvs.arrays.prob[:tn]
            #trees['spp'][-tn:] = spp_codes[fvs.arrays.isp[:tn]]

            #print sum(trees[-tn:]['tpa']) - summary[run_id,cycle]['tpa']
            print('{:>6s}{:>5s}{:>5s}{:>10s}'.format('year','stop','spp','tpa'))
            print('{:>6d}{:>5d}{:>5s}{:>10.3f}'.format(trees[-tn:]['year'][0],trees[-tn:]['stop'][0],trees[-tn:]['spp'][0],sum(trees[-tn:]['tpa'])))
            print('{:>6d}{:>5d}{:>5s}{:>10.3f}'.format(trees[-tn:]['year'][0],trees[-tn:]['stop'][0],trees[-tn:]['spp'][-1],trees[-tn:]['tpa'][-1]))

#            ntrees,ncycles,nplots,maxtrees,maxspecies,maxplots,maxcycles = fvs.fvsdimsizes()
#            attrs = numpy.zeros(ntrees)
#            fvs.fvstreeattr('tpa',3,'get',attrs)
#
#            print sum(trees[-tn:]['tpa']) - summary[run_id,cycle]['tpa'], sum(attrs) - summary[run_id,cycle]['tpa']

        #close all IO files
        fvs.filclose()

#        print summary[:,:6]
#        print summary[cnt,:]
        print 'Rep: %-4d' % x, ','.join('%6d' % v for v in summary[run_id,:]['tcuft'])
        # print 'BDFT',','.join('%6d' % v for v in summary[:,5])

        # if plot:
            # pylab.plot(summary[:, 3])

        run_id += 1

et = time.clock()

#replace the overwritten tree data file
shutil.copy2('pnt01.tre.save','pnt01.tre')

# #summary_names = ('year','age','tpa_b','gross_cuft_b','merch_cuft_b','gross_bdft_b','net_bdft_b','tpa_r','gross_cuft_r','top_cuft_r','gross_merch_cuft_r','net_merch_cuft_r','gross_bdft_r','net_bdft_r','baa_a','ccf_a','topht_a','per_len','accr_cuft','mort_cuft','sam_wt','for_cvr','size_class','stock_class')
# ##summary = numpy.recarray((num_cycles+1,20),dtype='i',names=summary_names)

print '%d reps; total elapsed time: %.2f, %.3f second per rep' % (reps, et - st, (et - st) / run_id)

if plot:
    xbar_cuft = numpy.mean(summary['tcuft'],axis=0)
    for r in xrange(summary.shape[0]):
        pylab.plot(summary[r,:]['tcuft'],color='blue',alpha=0.25)
    #pylab.boxplot(summary[:,:]['tcuft'])

    pylab.plot(xbar_cuft,linewidth=3,color='green')
    pylab.show()
