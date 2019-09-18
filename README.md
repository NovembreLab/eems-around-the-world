# EEMS-AROUND-THE-WORLD

## Goal
This is a workflow that allows easily using a number of population
structure analysis tools, with the goal to streamline workflows that 
allow easy comparison between methods and approaches. The original reason
was to compare EEMS with other methods.



## Implemented methods
- [EEMS](http://github.com/dipetkov/eems)
- [flashpca](https://github.com/gabraham/flashpca)
- [admixture](https://www.genetics.ucla.edu/software/admixture/)
- [pong](https://pypi.python.org/pypi/pong) visualization of admixture
- [TESS3](https://github.com/cayek/TESS3/)
- [treemix](https://bitbucket.org/nygcresearch/treemix/wiki/Home)
- [Spacemix](https://github.com/gbradburd/SpaceMix)
- [conStruct](https://github.com/gbradburd/conStruct)
- FST (using plink)


## Implementation
Genotypic data is stored in [plink](https://www.cog-genomics.org/plink2) format.
Metadata/location data is stored using John Novembre's
[PopGenStructures](https://docs.google.com/document/d/1wPlI1hLr19JIdM2EzYKlPnzzbR6L2ZOgOGkC6kbhHE4/edit)
data format, with some minor (recommended) changes.
The pipeline is implemented using [Snakemake](https://bitbucket.org/snakemake),
using `python` for most data wrangling and `R` for most plotting


## Reproducing results from Peter et al. 2019
As some of the data used requires permission, we are not free to redistribute it. To
re-generate all figures from the paper, it will be necessary to 
1. acquire access to all data and create master data set as described in the
   merge-pipeline
2. change paths in `config/config.json` to reflect your working environment
3. run `snakemake all`
