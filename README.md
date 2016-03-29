# EEMS-AROUND-THE-WORLD

## Goal
The goal here is to use various data visualization tools, apply and compare
them on a wide range of human genetic data. This is motivated by Desi Petkova's 
[EEMS](http://github.com/dipetkov/eems) program. Other methods that I am
currently using are

- [flashpca](https://github.com/gabraham/flashpca)
- [admixture](https://www.genetics.ucla.edu/software/admixture/)
- [pong](https://pypi.python.org/pypi/pong)

Other methods I am thinking about implementing:
- TESS
- F-statistics (Reich 2009, Peter2016)
- treemix (Pickrell & Pritchard, 2012)
- treelets (Lee 2007)
- spacemix (Bradburg et al. 2016)

## Data
I combined data from eight different sources:
- Lazaridis et al. 2014
- POPRES (Nelson et al. 2008)
- Xing et al. 2010
- Paschou et al. 2014
- HUGO Consoretium 2009
- Verdu et al 2014
- Reich, Stoneking 2011
- several data sets from the Estonian Biocenter

## Implementation
Genotypic data is stored in [plink](https://www.cog-genomics.org/plink2) format.
Metadata/location data is stored using John's
[PopGenStructures](https://docs.google.com/document/d/1wPlI1hLr19JIdM2EzYKlPnzzbR6L2ZOgOGkC6kbhHE4/edit)
data format, with some minor (recommended) changes.
The pipeline is implemented using [Snakemake](https://bitbucket.org/snakemake),
using python for most data wrangling and R for most plotting

