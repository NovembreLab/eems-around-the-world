from subsetter.load import load_pop_geo, load_indiv_meta
from subsetter.subset.polygon import _get_subset_area, create_polygon_file
from subsetter.subset import filter_data

configfile: "subset.json"
configfile: "eems.json"
configfile: "config.json"

include: 'eems.snake'
include: 'pong.snake'

PLINK_EXT = ['bed', 'bim', 'fam']
META_EXT = ['pop_geo', 'indiv_meta']
INDIV_META_COLS = ['sampleId', 'wasDerivedFrom', 'used', 
    'originalId', 'permissions', 'popLabel']
POP_GEO_COLS = ['popLabel', 'latitude', 'longitude', 'accuracy'] 


PLINK_EXE = config['EXE']['plink']
PLINK_SRC = config['DATA']['genotypes']
META_SRC = config['DATA']['meta']





base = lambda x: os.path.splitext(x)[0]

def snakemake_subsetter(input, output, name):
    params = config['subset']['__default__']
    params.update(config['subset'][name])
    location_data = load_pop_geo(input.meta[0])
    sample_data = load_indiv_meta(input.meta[1])
    meta_data = sample_data.merge(location_data)
    polygon, meta_data = _get_subset_area(meta_data = meta_data,
        region=params['region'],
        sample_buffer=float(params['sample_buffer']),
        region_buffer=float(params['region_buffer']),
        convex_hull=params['hull'],
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
    

rule subset_all_bed:
    input: subset_all_fun(ext='.bed')

rule subset_all_ini0:
    input: subset_all_fun(prefix='eems/', ext='-run0.ini')

rule subset_all_ini10:
    input: subset_all_fun_reps(prefix='eems/', ext='-run{i}.ini')

rule subset_all_diagnostic_mds:
    input: subset_all_fun(ext='-mds.pdf', prefix='eems/figures/')

rule subset_all_pca:
    input: subset_all_fun(ext='-pca.pdf', prefix='pca/figures/')

rule subset_admixture_k2:
    input: subset_all_fun_reps(prefix='admixture/{i}/', ext='.2.P')
        
rule subset_pong:
    input: subset_all_fun_reps(prefix='pong/', ext='-K2-8-nruns5/result_summary.txt')

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
        'pip install pong' #python 2



rule run_flashpca:
    input:
        bed='subset/{name}.bed',
        bim='subset/{name}.bim',
        fam='subset/{name}.fam',
    output:
        pc=protected("pca/flash_{name}_dim{ndim}.pc"),
        load=protected("pca/flash_{name}_dim{ndim}.load"),
        pve=protected("pca/flash_{name}_dim{ndim}.pve")
    params: seed=12
    run:
        infile = base(input.bed)
        s = '%s --bfile %s ' % (config['EXE']['flashpca'], infile)
        s += '--ndim %s ' % wildcards.ndim 
        s += '--outpc {output.pc} '
        s += '--outload {output.load} '
        s += '--outpve {output.pve} '
        s += '--v --mem low --seed {params.seed}'
        shell(s)

rule diagnostic_pca:
    input:
        pc='pca/flash_{name}_dim20.pc',
        order='subset/{name}.fam',
        indiv_meta='subset/{name}.indiv_meta',
        pop_display=config['DATA']['meta'] + '.pop_display',
        __script__='diagnostic_pca.R',
        __lib__='pw_plot.R'
    output:
        pdf='pca/figures/{name}-pca.pdf'
    script: input.__script__
EEMS_FILES=['mcmcxcoord.txt', 'mcmcycoord.txt',
            'mcmczcoord.txt', 'mcmcwcoord.txt', 
            'mcmcmtiles.txt', 'mcmcqtiles.txt']


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
