## Config folder

This folder contains various config files (in yaml format) that define how 
analyses are run. As of 26. 3. 2018, they will require some revisions and documentation.

Very briefly, the most important files are
 - config.yaml
 - subset.yaml
 - eems.yaml
 - paper.yaml
 - plots.yaml
 - cluster.yaml


###### config.yaml
Primary config file, major options include number of principal components,
paths to all executables (under 'EXE') and DATA. genotypes refers to a 
plink file (without extensions), meta to the meta-data, map to a map for
automatic subsetting

###### subset.yaml
Define subsets of the data for analysis. ALL analyses are configured to run
on subsets, so this is used even on a full dataset (just define a subset 'all')

Major options include:
- region: region to run stuff on, learned from config[DATA][map], for land-
areas only
- source_file : which genotype file to use (config[DATA][genotype])
- max_missing, max_missing_ind : filter for missing data
- extrema, bounding box on [x0, x1, y0, y1], samples outside are tossed
- hull: use convex hull around area
- filter: which filter on samples (from data.yaml) to apply
- exclude_source : manually exclude source for subset
- add_pop : manually add sample

there is also many undocumented ones that need to be annotated

###### eems.yaml
config for eems, all eems parameters (see manual) are passed on, also
 - bed2diffs1 or bed2diffs2 (how missing data is handled in dist matrix)
 - grid : resolution of world-based grid
 - n_runs : number of runs
 - n_pilots : number of pilot runs
 - pilot : separate eems options for pilot run
 - continues : continue previous run

###### cluster.yaml
Defines resources for running on a cluster environment (tested using slurm and
SGE), currently supports time, memory, and number of cpus

###### data.yaml
fir filters and annotations

###### paper.yaml
create tables with all pops and where they are used. Format specific to
Peter, Petkova & Novembre (2018) paper

###### plot.yaml
various options for plots. need clean-up and are mainly undocumented. In
general,
not sure if this is the best way to do things
