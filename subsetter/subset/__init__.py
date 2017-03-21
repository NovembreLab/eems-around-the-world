from .polygon import get_subset_area
from ..utils.plink import run_plink
import pandas as pd
import numpy as np

def filter_data(meta_data, bedfile, missing=0.001, plink="plink",
                has_dataset=False, outfile='TMP_PLINK'):
    """filter_data
    filters bed file to only keep the individuals in meta_data, uses plink
    
    the data is read from bedfile, and written to the file in the TMP_PLINK file
    global variable
    
    Parameters
    ----------
    meta_data : pd.DataFrame
        pandas data frame with individuals to keep
    bedfile : path
        the bedfile to be filtered
    plink : path
        the plink executable to be used
    
    """
    include_name = '%s.incl' % outfile

    try:
        fam = pd.read_table("%s.fam" % bedfile, header=None,
                            skipinitialspace=True)
        fam.columns = ['FAM', 'sampleId', 'a', 'b', 'c', 'd']
    except ValueError:
        fam = pd.read_table("%s.fam" % bedfile, header=None,
                            skipinitialspace=True, sep=" ")
        fam.columns = ['FAM', 'sampleId', 'a', 'b', 'c', 'd']

    extract_data = meta_data.merge(fam, on='sampleId', how='inner')
    extract_data.to_csv(include_name, sep=' ',
                        columns=('FAM', 'sampleId'),
                        header=None, index=None)
    extract_data.drop('a', axis=1, inplace=True)
    extract_data.drop('b', axis=1, inplace=True)
    extract_data.drop('c', axis=1, inplace=True)
    extract_data.drop('d', axis=1, inplace=True)
    extract_data.drop('FAM', axis=1, inplace=True)
    extract_data.drop('POINTS', axis=1, inplace=True)
    print(extract_data.columns)

    #if has_dataset:
    #    meta_data = extract_data[['sampleId', 'POP', 'latitude', 'longitude', 'FAM',
    #                              'DATASET']]
    #else:
    #    meta_data = extract_data[['sampleId', 'POP', 'latitude', 'longitude', 'FAM']]
    extract_data = extract_data[pd.notnull(extract_data['latitude'])]
    extract_data = extract_data[pd.notnull(extract_data['longitude'])]

    flags = dict()
    flags['make-bed'] = ''
    #flags['allow-extra-chr'] = ''
    flags['bfile'] = bedfile
    flags['out'] = outfile
    flags['keep'] = include_name
    flags['indiv-sort'] = 'f %s' % include_name
    flags['geno'] = '%s'% missing 


    run_plink(plink, flags)

    return extract_data
