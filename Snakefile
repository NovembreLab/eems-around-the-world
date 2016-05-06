configfile: "config/subset.json"
configfile: "config/eems.json"
configfile: "config/config.json"

include: 'sfiles/utils.snake'
include: 'sfiles/treemix.snake'
include: 'sfiles/eems.snake'
include: 'sfiles/pong.snake'
include: 'sfiles/pca.snake'
include: 'sfiles/spacemix.snake'
include: 'sfiles/paintings.snake'
include: 'sfiles/tess.snake'

PLINK_EXT = ['bed', 'bim', 'fam']
META_EXT = ['pop_geo', 'indiv_meta']
INDIV_META_COLS = ['sampleId', 'wasDerivedFrom', 'used', 
    'originalId', 'permissions', 'popId']
POP_GEO_COLS = ['popId', 'latitude', 'longitude', 'accuracy'] 


PLINK_EXE = config['EXE']['plink']
PLINK_SRC = config['DATA']['genotypes']
META_SRC = config['DATA']['meta']


base = lambda x: os.path.splitext(x)[0]

def snakemake_subsetter(input, output, name):
    """ creates a subset of data based on a geographical region
        see the rule `subset` for an example.
        it assumes that output is in a folder named `subset/`
    input : snakemake.input
        input.plink : a triple of plink format genetic data files
        input.meta : a path to pgs-type meta-data
        input.map : path to a shapefile map
    output : snakemake.output
        output.indiv_meta : indiv_meta file of subset
        output.pop_geo: pop_geo file restricted to subset
        output.polygon: a l x 2 file with latitude and longitude
            of polygon points delineating region
    name : str
        the name of the resulting dataset, also, config is read
        from config['subset'][name]
    """
    from subsetter.load import load_pop_geo, load_indiv_meta
    from subsetter.subset.polygon import _get_subset_area, create_polygon_file
    from subsetter.subset import filter_data
    import numpy as np

    params = config['subset']['__default__']
    params.update(config['subset'][name])
    location_data = load_pop_geo(input.meta[0])
    sample_data = load_indiv_meta(input.meta[1])
    meta_data = sample_data.merge(location_data)

    from collections import Counter
    counter = Counter(meta_data.popId)
    pops_to_keep = [c for c in counter if counter[c] >= params['min_sample_size']]
    inds_to_keep = np.in1d(meta_data.popId, pops_to_keep)
    meta_data = meta_data[inds_to_keep]

    if "population" not in params:
        print("POP NOT FOUND WEEE")
        params['population'] = None

    if "region" not in params:
        print("REGION NOT FOUND WEEE")
        params['region'] = None
    polygon, meta_data = _get_subset_area(meta_data = meta_data,
        region=params['region'],
        sample_buffer=float(params['sample_buffer']),
        region_buffer=float(params['region_buffer']),
        convex_hull=params['hull'],
        population=params['population'],
                _map=input.map)
    bed = os.path.splitext(input.plink[0])[0]
    meta_data = filter_data(meta_data=meta_data,
                            bedfile=bed,
                            missing=float(params['max_missing']), 
                            plink=PLINK_EXE,
                            outfile='subset/' + name)
    
    meta_data[POP_GEO_COLS].drop_duplicates().to_csv(output.pop_geo, index=False)
    meta_data[INDIV_META_COLS].to_csv(output.indiv_meta, index=False)
    create_polygon_file(polygon, output.polygon, add_outer=False)


def subset_all_fun(ext, prefix=''):
    def ss(wildcards):
        subsets = config['subset'].keys()
        infiles = ['%s%s%s' %(prefix, s, ext) for s in subsets 
            if not s == '__default__']
        return infiles
    return ss
    

def subset_all_fun_reps(ext, prefix='', nreps=10):
    def ss(wildcards):
        subsets = config['subset'].keys()
        infiles = expand(["".join([prefix, s, ext])  for s in subsets 
            if not s == '__default__'], i=range(nreps))
        return infiles
    return ss
    



# rules that run important stuff for all subsets

rule subset_all_eems:
    input:
        subset_all_fun(prefix='eemsout/', ext='_runs10.controller')

rule subset_all_eems_plot:
    input:
        subset_all_fun(prefix='eemsout/', ext='_nruns10-mrates01.png')

rule subset_all_pca:
    input:
        subset_all_fun(ext='_dim20_pc2.png', prefix='figures/pca/'),
         subset_all_fun(ext='_dim20_pc1.png', prefix='figures/pca/')

rule subset_all_pong:
    input: subset_all_fun(prefix='pong/run_pong_', ext='-K2-8-nruns3.sh')

rule subset_all_tess:
    input: subset_all_fun(prefix='tess/subset/', ext='_K2-8_nruns3.controller')

rule all:
    input:
        rules.subset_all_eems.input,
        rules.subset_all_pca.input,
        rules.subset_all_pong.input

# rules that run testing or partial stuff for all subsets
rule subset_all_ini0:
    input: subset_all_fun(prefix='eems/', ext='-run0.ini')

rule subset_all_ini10:
    input: subset_all_fun_reps(prefix='eems/', ext='-run{i}.ini', nreps=10)

rule subset_all_diagnostic_mds:
    input: subset_all_fun(ext='-mds.pdf', prefix='eems/figures/')



rule subset_admixture_k2:
    input: subset_all_fun_reps(prefix='admixture/{i}/', ext='.2.P')
        

# rules that do the data partitioning
rule subset:
    input:
        plink=expand("%s.{ext}"% PLINK_SRC, ext=PLINK_EXT),
        meta=expand("%s.{ext}"% META_SRC, ext=META_EXT),
        map=config['DATA']['map']
    output:
        pop_geo='subset/{name}.pop_geo',
        indiv_meta='subset/{name}.indiv_meta',
        polygon='subset/{name}.polygon',
        bed='subset/{name}.bed',
        bim='subset/{name}.bim',
        fam='subset/{name}.fam',
    version: "1"
    run:
        snakemake_subsetter(input, output, wildcards.name)


rule install:
    shell:
        'apt-get install libgeos-dev libgda1-dev python-pandas python-pip '
        ' python-mpltoolkits.basemap ;' #ubuntu repos
        'pip3 install shapely fiona descartes basemap;' #python3 stuff
        'pip install pong;' #python 2
        # R packages: deldir SDMtools, rworldmap, rworldxtra        
        #   mapdata, FNN:q



rule diagnostic_pca:
    input:
        pc='pca/flash_{name}_dim20.pc',
        order='subset/{name}.fam',
        indiv_meta='subset/{name}.indiv_meta',
        pop_display=config['DATA']['meta'] + '.pop_display',
        __script__='scripts/diagnostic_pca.R',
        __lib__='pw_plot.R'
    output:
        pdf='pca/figures/{name}-pca.pdf'
    script: input.__script__

"""
rule run_eems:
    input:
        bed='{name}.bed',
        bim='{name}.bim',
        fam='{name}.fam',

rule run_treemix:
    input:
        bed='{name}.bed',
        bim='{name}.bim',
        fam='{name}.fam',

rule run_treelets:
    input:
        bed='{name}.bed',
        bim='{name}.bim',
        fam='{name}.fam',

rule run_tess:
    input:
        bed='{name}.bed',
        bim='{name}.bim',
        fam='{name}.fam',

rule run_spacemix:
    input:
        bed='{name}.bed',
        bim='{name}.bim',
        fam='{name}.fam',

rule run_f2:
    input:
        bed='{name}.bed',
        bim='{name}.bim',
        fam='{name}.fam',

rule run_f3:
    input:
        bed='{name}.bed',
        bim='{name}.bim',
        fam='{name}.fam',

rule run_f4:
    input:
        bed='{name}.bed',
        bim='{name}.bim',
        fam='{name}.fam',
"""
