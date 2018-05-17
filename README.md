# EEMS-AROUND-THE-WORLD
**Visualizing population structures using Admixture, PCA, and EEMS (vAPE)**


## Overview
1. [Goal](#goal)
2. [What does my input data have to look like?](#what-does-my-input-data-have-to-look-like?)
3. [Getting started: Configuring the workflow to fit your data](config/README.md)


## Goal

The workflow brings together a number visualization tools commonly used in population structure analyses. The goal is to allow for a easy and reliable generation of a number of different plots and graphs from one input dataset, giving you a general overview over your data set.

The main focus lies on the following methods:
- [EEMS](http://github.com/dipetkov/eems)
- [flashpca](https://github.com/gabraham/flashpca)
- [admixture](https://www.genetics.ucla.edu/software/admixture/)

Others include:
- [pong](https://pypi.python.org/pypi/pong) visualization of admixture
- [TESS3](https://github.com/cayek/TESS3/)
- [treemix](https://bitbucket.org/nygcresearch/treemix/wiki/Home)
- [Spacemix](https://github.com/gbradburd/SpaceMix)
- [conStruct](https://github.com/gbradburd/conStruct)
- FST using [plink](https://www.cog-genomics.org/plink/1.9/)


The pipeline is implemented using [Snakemake](https://bitbucket.org/snakemake),
using `python` for most data wrangling and `R` for most plotting.


## What does my input data have to look like?

### Genotypes

Genotypes must be provided in binary [plink](https://www.cog-genomics.org/plink2) format, which is comprised of the following three file types:

1. *filename.bed* (Do not confuse this with the UCSC Genome Browser's BED format, which is completely different.)

2. *filename.bim*

3. *filename.fam*


### Meta data

The meta data - holding information on individuals and populations in your sample - must be provided in an adaption of the  [PopGenStructures](https://docs.google.com/document/d/1wPlI1hLr19JIdM2EzYKlPnzzbR6L2ZOgOGkC6kbhHE4/edit) format as follows:

1. *filename.indiv_meta*:
> `sampleId,source,used,originalId,permissions,popId`

2. *filename.pop_meta*
> `"popId","name","abbrev","color","colorAlt","order","latitude","longitude","accuracy"`



### Maps

This pipeline uses maps from [Natural Earth](http://www.naturalearthdata.com/) to allow you to subset data sets into regional subsets by using words like 'Asia' or 'India'. This implementation is configured to these maps specifically, which is why they are included as files in this package. You can find them under `subsetter/maps`.

It is, however, possible to load your own maps into the workflow, if you require a more detailed resolution. Maps must then be provided in the [shapefile](https://en.wikipedia.org/wiki/Shapefile) format.
