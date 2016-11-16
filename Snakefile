configfile: "config/subset.json"
configfile: "config/eems.json"
configfile: "config/config.json"
configfile: "config/data.json"

include: 'sfiles/utils.snake'
include: 'sfiles/treemix.snake'
include: 'sfiles/eems.snake'
include: 'sfiles/pong.snake'
include: 'sfiles/pca.snake'
include: 'sfiles/spacemix.snake'
include: 'sfiles/paintings.snake'
include: 'sfiles/tess.snake'
include: 'sfiles/fst.snake'


subsets_paper = ['africa3', 
    'medi4',
    'europe3', 
    'centralasia3',
    'eastasia2',
    'seasiaB',
    'southafrica2',
    'india0'
#    "ncasia0",
#    "northasia1",
#    "ncasia1",
]
subsets0 = ['africa0', 
    'medi0',
    'europe0', 
    'centralasia0',
    'eastasia0',
    'seasia0'
    'southafrica2',
    'india0'
#    'southafrica0'
#    "ncasia0",
#    "northasia0",
#    "ncasia0",
]

#subsets_paper.extend(subsets0)
#with open("ss_paper.txt", 'w') as f:
#    for s in subsets_paper:
#        f.write("%s\n" % s)

PLINK_EXT = ['bed', 'bim', 'fam']
META_EXT = ['pop_geo', 'indiv_meta']
INDIV_META_COLS = ['sampleId', 'wasDerivedFrom', 'used', 
    'originalId', 'permissions', 'popId']
POP_GEO_COLS = ['popId', 'latitude', 'longitude', 'accuracy'] 


PLINK_EXE = config['EXE']['plink']
PLINK_SRC = config['DATA']['genotypes']
META_SRC = config['DATA']['meta']


base = lambda x: os.path.splitext(x)[0]

def load_subset_config(config, name, verbose=True):
    """ recursively load subset info """
    if verbose:
        print("loading subset %s" % name)

    params = config['__default__'].copy()

    if 'subsets' in config[name]:
        parent_dataset = load_subset_config(config, config[name]['subsets'])
        params.update(parent_dataset)
    params.update(config[name])

    # this bit modifies lists, etc
    if 'modify_parent' in config[name]:
        for k, v in config[name]['modify_parent'].items():
            if k in params:
                params[k] = params[k] + v
            else: 
                params[k] = v
            if verbose:
                print("modifying key %s to value %s" % (k, v))

        
    return params


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

    outname = base(output.bed)

    params = load_subset_config(config['subset'], name)
    location_data = load_pop_geo(input.meta[0], wrap=False)
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

    if "exclude_pop" not in params:
        print("NO POPS EXCLUDED")
        params['exclude_pop'] = None


    polygon, meta_data = _get_subset_area(meta_data = meta_data,
        region=params['region'],
        sample_buffer=float(params['sample_buffer']),
        region_buffer=float(params['region_buffer']),
        convex_hull=params['hull'],
        extrema=params['extrema'],
        population=params['population'],
        exclude_pop=params['exclude_pop'],
        exclude_source=params['exclude_source'],
        min_area=params['min_area'],
        add_pop = params['add_pop'],
                _map=input.map)

    bed = os.path.splitext(input.plink[0])[0]
    meta_data = filter_data(meta_data=meta_data,
                            bedfile=bed,
                            missing=float(params['max_missing']), 
                            plink=PLINK_EXE,
                            outfile=outname)
    
    meta_data[POP_GEO_COLS].drop_duplicates().to_csv(output.pop_geo, index=False)
    meta_data[INDIV_META_COLS].to_csv(output.indiv_meta, index=False)
    create_polygon_file(polygon, output.polygon, add_outer=False)

def subset_paper_fun(ext, prefix='', subset0=False):
    def ss(wildcards):
        #print('subset_all_fun called')
        subsets = subsets_paper
        if subset0: subsets=subsets0
        infiles = ['%s%s%s' %(prefix, s, ext) for s in subsets 
            if not s == '__default__']
        return infiles
    return ss
    

def subset_all_fun(ext, prefix=''):
    def ss(wildcards):
        #print('subset_all_fun called')
        subsets = config['subset'].keys()
        #print(subsets)
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
    

include: 'sfiles/paper_figures.snake'



       

# rules that run important stuff for all subsets
rule subset_all_poly:
    input:
        subset_all_fun(prefix='subset/', ext='.polygon')
rule subset_all_bed1:
    input:
        subset_all_fun(prefix='subset_nopca/', ext='.bim'),
        subset_all_fun(prefix='subset_nopca/', ext='.fam'),
        subset_all_fun(prefix='subset_nopca/', ext='.bed')

rule subset_all_diffs:
    input:
        subset_all_fun(prefix='eems/', ext='.diffs')

rule subset_all_fst:
    input:
        subset_all_fun(prefix='figures/fst/', ext='.fst.png')

rule subset_all_spacemix:
    input:
        subset_all_fun(prefix='spacemix/subset/', ext='.controller')

rule subset_all_eems:
    input:
        subset_all_fun(prefix='eemsout/', ext='_runs4.controller')

rule subset_all_eems_ggplot:
    input:
        subset_all_fun(prefix='eemsout_gg/', ext='_nruns4-mrates01.png')

rule subset_all_eems_plot:
    input:
        subset_all_fun(prefix='eemsout/', ext='_nruns4-mrates01.png')

rule subset_all_pca:
    input:
        subset_all_fun(ext='_pc20.png', prefix='figures/pca/pc1d_'),

rule subset_all_pca_wdf:
    input:
        subset_all_fun(ext='_pc19_wdf.png', prefix='figures/pca/pc2d_'),

rule subset_all_pong:
    input: subset_all_fun(prefix='pong/run_pong_', ext='-K2-8-nruns3.sh')

rule subset_all_treemix:
    input : subset_all_fun(prefix='treemix/subset/', ext='_m2-8_runs4.tree.png')

rule subset_all_tess:
    input: subset_all_fun(prefix='tess/subset/', ext='_K2-8_nruns3.controller')


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
rule subset_nopca:
    input:
        plink=expand("%s.{ext}"% PLINK_SRC, ext=PLINK_EXT),
        meta=expand("%s.{ext}"% META_SRC, ext=META_EXT),
        map=config['DATA']['map']
    output:
        pop_geo='subset/{name}.pop_geo',
        indiv_meta='subset/{name}.indiv_meta',
        polygon='subset/{name}.polygon',
        bed='subset_nopca/{name}.bed',
        bim='subset_nopca/{name}.bim',
        fam='subset_nopca/{name}.fam',
        incl='subset_nopca/{name}.incl'
    version: "2"
    run:
        snakemake_subsetter(input, output, wildcards.name)

rule subset_pca:
    input:
        bed='subset_nopca/{name}.bed',
        bim='subset_nopca/{name}.bim',
        fam='subset_nopca/{name}.fam',
        outliers="subset/{name}_dim10.outlier_snp"
    output:
        bed='subset/{name}.bed',
        bim='subset/{name}.bim',
        fam='subset/{name}.fam',
    shell:
        '{PLINK_EXE} --bfile subset_nopca/{wildcards.name} '
        '--exclude {input.outliers} '
        '--out subset/{wildcards.name} --make-bed'





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




"""
rule all:
    input:
        rules.subset_all_spacemix.input,
        rules.subset_all_pca.input,
        rules.subset_all_eems_plot.input,
        rules.subset_all_pong.input,
        rules.subset_all_treemix.input,
        rules.subset_all_tess.input,
