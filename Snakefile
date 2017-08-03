configfile: "config/config.json"
configfile: "config/subset.json"
configfile: "config/eems.json"
configfile: "config/data.yaml"
configfile: "config/plots.yaml"
configfile: "config/paper.yaml"


subsets_paper = [
    "global2s", #reruning global0b
    "eumedi0sb", #running eumedi4, eumedi2
    "easia1s", #waiting for easiacer20b

    "africa1s", #waiting for 0b/0c
    "seasia3s", #waiting for seasia0c

    "casia0sd", #completely done
    "southafrica1s" #completely done
]
subsets0 = ['africa0', 
    'medi0',
    'europe0', 
    'centralasia0',
    'eastasia0',
    'seasia0',
    'southafrica2',
    'india0'
#    'southafrica0'
#    "ncasia0",
#    "northasia0",
#    "ncasia0",
]

subsets = config['paper']
subsets_names = [k for k,v in subsets.items()]
subsets_abbrev = [v['abbrev'] for k,v in subsets.items()]
subsets_paper = [v['main'] for k,v in subsets.items()]
subsets0 = [v['full'] if v['full'] else v['main'] for k,v in subsets.items()]

excluded_sets = [
'centralasia3pccer11',
'centralasia5pccer11',
"easia1",
"easiacer11",
"seasia4c",
"africa5pc1",
"africa5pc2",
"eumedi0",

#currently running
"global3",
"eumedi3",
"northafrica0b",
"northafrica0c",
"eumedi2",
"seasia0c"
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
_META_ = config['DATA']['meta']
_POP_DISPLAY_ = _META_ + ".pop_display"
_POP_GEO_ = _META_ + ".pop_geo"
_INDIV_META_ = _META_ + ".indiv_meta"


include: 'sfiles/utils.snake'
include: 'sfiles/treemix.snake'
include: 'sfiles/pong.snake'
include: 'sfiles/pca.snake'
include: 'sfiles/spacemix.snake'
include: 'sfiles/paintings.snake'
include: 'sfiles/tess.snake'
include: 'sfiles/fst.snake'
include: 'sfiles/distances.snake'

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
        input.bed/bim/fam : a triple of plink format genetic data files
        input.* : a path to pgs-type meta-data
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
    location_data = load_pop_geo(input.pop_geo, wrap=False)
    sample_data = load_indiv_meta(input.indiv_meta)
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
        params['exclude_pop'] = []

    if "filter" in params:
        for f in params["filter"]:
            filter_set = config["filter"][f]
            print("filtering %s" % f)
            params["exclude_pop"].extend(filter_set)


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

    # exclucde some individuals
    if 'exclude_samples' in params:
        excl = params['exclude_samples']
        print("excluding stuff, from %s rows ..."% meta_data.shape[0])
        meta_data = meta_data[~meta_data['sampleId'].isin(excl)]
        print("to %s rows ..."% meta_data.shape[0])


    bed = os.path.splitext(input.bed)[0]
    meta_data = filter_data(meta_data=meta_data,
                            bedfile=bed,
                            missing=float(params['max_missing']), 
                            per_ind_missing=float(params['max_missing_ind']),
                            plink=PLINK_EXE,
                            max_per_pop=int(params['max_per_pop']),
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
    
include: 'sfiles/eems.snake'
include: 'sfiles/eems0.snake'

def subset_all_fun(ext, prefix='', force=False):
    def ss(wildcards):
        #print('subset_all_fun called')
        subsets = config['subset'].keys()
        #print(subsets)
        local_excluded = excluded_sets
        if(force): local_excluded = []
        for s in subsets:
            if s in local_excluded:
                print("excluded " + s)
        infiles = ['%s%s%s' %(prefix, s, ext) for s in subsets 
            if not (s == '__default__' or s in local_excluded)]
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
rule subset_paper_spacemix:
    input:
        subset_paper_fun(prefix='spacemix/subset/', ext='.controller')

rule subset_all_eems:
    input:
        subset_all_fun(prefix='eemsout/', ext='_runs4.controller')

rule subset_all_eems_ggplot:
    input:
        subset_all_fun(prefix='eemsout_gg/', ext='_nruns4-mrates01.png')
rule subset_paper_eems_ggplot:
    input:
        subset_paper_fun(prefix='eemsout_gg/', ext='_nruns4-mrates02.png'),
        subset_paper_fun(prefix='eemsout_gg/', ext='_nruns4-mrates01.png')
rule subset_paper_synth:
    input:
        subset_paper_fun(prefix='figures/pca/synthmap/', ext='_PC1.png'),
rule subset_paper_map:
    input:
        subset_paper_fun(prefix='eemsout_gg/', ext='_nruns4-map01.png'),
rule subset_paper_figs:
    input:
        subset_paper_fun(prefix='', ext='.figs')

rule subset_all_eems_plot:
    input:
        subset_all_fun(prefix='eemsout/', ext='_nruns4-mrates01.png')

rule subset_all_pca:
    input:
        subset_all_fun(ext='_pc20.png', prefix='figures/pca/pc1d_'),
rule subset_all_loadings:
    input:
        subset_all_fun(ext='_pc20.png', prefix='figures/pca/loadings_'),

rule subset_all_pca_wdf:
    input:
        subset_all_fun(ext='_pc19_wdf.png', prefix='figures/pca/pc2d_'),

rule subset_all_pong:
    input: subset_all_fun(prefix='pong/run_pong_', ext='-K2-8-nruns3.sh')

rule subset_paper_pong:
    input: subset_paper_fun(prefix='pong/run_pong_', ext='-K2-8-nruns3.sh')

rule subset_all_treemix:
    input : subset_all_fun(prefix='treemix/subset/', ext='_m0-8_runs4.tree.png')

rule subset_paper_treemix:
    input : subset_paper_fun(prefix='treemix/subset/', ext='_m0-2_runs3.tree.png')

rule subset_all_tess:
    input: subset_all_fun(prefix='tess/subset/', ext='_K2-8_nruns3.controller')
rule subset_paper_tess:
    input: subset_paper_fun(prefix='tess/subset/', ext='_K2-6_nruns3.controller')


# rules that run testing or partial stuff for all subsets
rule subset_all_ini0:
    input: subset_all_fun(prefix='eems/', ext='-run0.ini')

rule subset_all_ini10:
    input: subset_all_fun_reps(prefix='eems/', ext='-run{i}.ini', nreps=10)

rule subset_all_diagnostic_mds:
    input: subset_all_fun(ext='-mds.pdf', prefix='eems/figures/')

rule subset_paper_newplots:
    input : 
        subset_paper_fun(prefix='figures/paper/', ext='.png'),

rule subset_all_newplots:
    input : 
        subset_all_fun(prefix='figures/paper/', ext='.png'),
#        subset_all_fun(prefix='figures/pca/2d/', ext='_pc1.png'),
#        subset_all_fun(prefix='eemsout_gg/', ext='_nruns4-mrates01.png'),
        subset_all_fun(prefix='eemsout_gg/', ext='_nruns4-map01.png'),


rule subset_admixture_k2:
    input: subset_all_fun_reps(prefix='admixture/{i}/', ext='.2.P')

rule subset_all_error_plot:
    input: subset_all_fun(prefix="eemsout_gg/", ext="_nruns4-error-pop01.png")

rule subset_all_scatter_plot:
    input: subset_all_fun(prefix="figures/dists/", ext=".png")

rule subset_all_excluded:
    input: subset_all_fun(prefix="excl/", ext=".excl", force=True)
        

# rules that do the data partitioning
def subset_inputfn(wildcards):
    d = dict()
    params = load_subset_config(config['subset'], wildcards.name)
    if 'source_file' in params:
        #print("custom source")
        src = config['DATA']['genotypes']
        #print(src, len(src))
        
        source_file = src[params['source_file']]
    else:
        print("default source")
        source_file = PLINK_SRC
            

    for ext in PLINK_EXT:
        d[ext] = "%s.%s" % (source_file, ext)
    for ext in META_EXT:
        d[ext] = "%s.%s" % (_META_, ext)
    d['map']=config['DATA']['map']
    return d

rule subset_nopca:
    input:
        unpack(subset_inputfn)
    output:
        pop_geo='subset/{name}.pop_geo',
        indiv_meta='subset/{name}.indiv_meta',
        polygon='subset/{name}.polygon',
        bed='subset_nopca/{name}.bed',
        bim='subset_nopca/{name}.bim',
        fam='subset_nopca/{name}.fam',
        incl='subset_nopca/{name}.incl'
    version: "3"
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
    run:
        s = '{PLINK_EXE} --bfile subset_nopca/{wildcards.name} '
        s += ' --out subset/{wildcards.name} --make-bed'
        if 'no_pca' in config['subset'][wildcards.name]:
            if config['subset'][wildcards.name]['no_pca']:
                s += ' --exclude {input.outliers} '
        shell(s)





rule install:
    shell:
        'apt-get install libgeos-dev libgda1-dev python-pandas python-pip '
        ' python-mpltoolkits.basemap ;' #ubuntu repos
        'pip3 install shapely fiona descartes basemap;' #python3 stuff
        'pip install pong;' #python 2
        # R packages: deldir SDMtools, rworldmap, rworldxtra        
        #   mapdata, FNN:q



__script__11='scripts/diagnostic_pca.R'
rule diagnostic_pca:
    input:
        pc='pca/flash_{name}_dim20.pc',
        order='subset/{name}.fam',
        indiv_meta='subset/{name}.indiv_meta',
        pop_display=_POP_DISPLAY_,
        __script__='scripts/diagnostic_pca.R',
        __lib__='scripts/pw_plot.R'
    output:
        pdf='pca/figures/{name}-pca.pdf'
    script: __script__11

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
