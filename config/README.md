## Configuring the VAPE workflow to fit your data

The folder **config** contains all config files that allow for analysis-specific adjustments, such as defining input-data or which subsets of the data you wish to analyze.

 - [*config.yaml*](#configyaml-required)
 - [*subset.yaml*](#subsetyaml-required)
 - [*eems.yaml*](#eemsyaml-optional)
 - [*output.yaml*](#outputyaml-optional)
 - [*plots.yaml*](#plotsyaml-optional)
 - [*cluster.yaml*](#clusteryaml-optional)

----

#### *config.yaml* (required)

This is the main config file that expects the path to all [required executables](https://github.com/NovembreLab/eems-around-the-world/blob/master/README.md#requirements) and [input data](https://github.com/NovembreLab/eems-around-the-world/blob/master/README.md#what-does-my-input-data-have-to-look-like).

```
EXE : paths to all executables

DATA : paths to input data without file type endings

```

----

#### *subset.yaml* (required)
Here you define the different subsets of your data you wish to analyze.
If you wish to run an analysis on the full dataset, you must define a subset 'all'.

```
   __default__:
    add_pop: [] : manually add sample
    exclude_source: [] : manually exclude source for subset
    extrema: false
    filter: [] : which filter on samples (from data.yaml) to apply
    hull: true
    max_missing: 0.01 : filter for missing data
    max_missing_ind: 1.0 : filter for missing data
    max_per_pop: 5000
    min_area: 0.3
    min_sample_size: 1
    region_buffer: 3.0
    sample_buffer: 3.0
    source_file: 0 : which genotype file to use (config[DATA][genotypess])
  run0:                          
    region : Asia : region to run stuff on, learned from config[DATA][map], excluding aquatic regions
    region_buffer: 1.0
    sample_buffer: 1.0
    hull : true : use convex hull around area
  run1:                          
    region : Asia
    region_buffer: 1.0
    sample_buffer: 1.0
    hull : true : use convex hull around area
    extrema : [110, 130, -10, 10]
  run2b:                          
    subsets : run2
```

----

#### *eems.yaml* (optional)
Here you set **eems** parameters for the subsets defined in *subset.yaml*. Again, you can set default settings for all subsets and additionally specify settings for individual subsets. The parameters include EEMS parameters from the original publication (see [EEMS](http://github.com/dipetkov/eems) for more information):

```
  nDemes = 200 : density of the population grid
  numMCMCIter = 2000000 : number of MCMC iterations
  numBurnIter = 1000000 : number of burn-in iterations
  numThinIter = 9999 : thinning interval
```

and the following additional parameters:

```
  bed2diffs1 or bed2diffs2 (how missing data is handled in dist matrix)
  grid : resolution of world-based grid
  n_runs : number of runs
  n_pilots : number of pilot runs
  pilot : separate eems options for pilot run
  continues : continue previous run
```

----
#### *output.yaml* (optional)
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
#### *plots.yaml* (optional)
various options for plots.


----

#### *cluster.yaml* (optional)

Defines resources for running on a cluster environment (tested using slurm and
SGE), currently supports time, memory, and number of cpus.
