## Getting started: Configuring the workflow to fit your data

The folder **config** contains all config files that allow for analysis-specific adjustments, such as defining input-data or which subsets of the data you wish to analyze.

 - [*config.yaml*](#configyaml)
 - [*subset.yaml*](#subsetyaml)
 - [*eems.yaml*](#eemsyaml)
 - [*output.yaml*](#outputyaml)
 - [*plots.yaml*](#plotsyaml)
 - [*cluster.yaml*](#clusteryaml)


#### *config.yaml*

This is the main config file that expects the path to all required executables, input data, and how many principle components you want to include.

**NPC** : number of principal components you want to include


**EXE** : path to all executables

    - plink
    - admixture
    - flashpca
    - eems
    - eems0
    - bed2diffs1
    - bed2diffs2
    - treemix
    - tess


**DATA** : path to input data

    - genotypes in plink format (without extensions)
    - meta data
    - map (for subsetting data)

**sdfactor** : 0.0000001
<font color="red">What does this do?</font>


----

#### *subset.yaml*
Here you define the different subsets of your data you wish to analyze.

If you wish to run an analysis on the full dataset, you must define a subset 'all'.


<font color="red">How does it work exactly? There are default settings for all runs, then I specifty run names. Are these names freely definable by user? Is the general idea here that I have one dataset and then I run the analysis on different subsets of the data? How would the subset 'all' look like?</font>

```
   **__default__**:
    - add_pop: [] : manually add sample
    - exclude_source: [] : manually exclude source for subset
    - extrema: false
    - filter: [] : which filter on samples (from data.yaml) to apply
    - hull: true
    - max_missing: 0.01 : filter for missing data
    - max_missing_ind: 1.0 : filter for missing data
    - max_per_pop: 5000
    - min_area: 0.3
    - min_sample_size: 1
    - region_buffer: 3.0
    - sample_buffer: 3.0
    - source_file: 0 : which genotype file to use (config[DATA][genotypess])

  **run0**:                          
    - region : Asia : region to run stuff on, learned from config[DATA][map], excluding aquatic regions
    - region_buffer: 1.0
    - sample_buffer: 1.0
    - hull : true : use convex hull around area
  **run1**:                          
    - region : Asia
    - region_buffer: 1.0
    - sample_buffer: 1.0
    - hull : true : use convex hull around area
    - extrema : [110, 130, -10, 10]
  **run2b**:                          
    - subsets : run2
```

----

#### *eems.yaml*
Here you set **eems** parameters for the subsets defined in *subset.yaml*. Again, you can set default settings for all subsets and aditionally specify settings for individual subsets. The parameters include eems parameters from the original publication (see [EEMS](http://github.com/dipetkov/eems) for more information):

    - nDemes = 200 : density of the population grid
    - numMCMCIter = 2000000 : number of MCMC iterations
    - numBurnIter = 1000000 : number of burn-in iterations
    - numThinIter = 9999 : thinning interval

and the follwing additional parameters:

    - bed2diffs1 or bed2diffs2 (how missing data is handled in dist matrix)
    - grid : resolution of world-based grid
    - n_runs : number of runs
    - n_pilots : number of pilot runs
    - pilot : separate eems options for pilot run
    - continues : continue previous run


<font color="red">Parameters from eems should be included here. What is density of population grid. Where do I find this? Are there good default values we could set here?</font>


----
#### *output.yaml*
create tables with all pops and where they are used. Format specific to
Peter, Petkova & Novembre (2018) paper

```
paper:
    #revision1nofilter:
    All :
        main : "run1"
        full : false
        abbrev : All
```

<font color="red">This needs more explanation</font>

----
#### *plot.yaml*
various options for plots.

need clean-up and are mainly undocumented. In general, not sure if this is the best way to do things. <font color="red">Can we go through these together?</font>  

----

#### *cluster.yaml*

Defines resources for running on a cluster environment (tested using slurm and
SGE), currently supports time, memory, and number of cpus.

<font color="red">Are these fixed? How do you know how much resources you need to reserve on the cluster?</font>
