from .parameters import Parameters
from .load import load_location_file, load_sample_file, load_combined_file
from .load import load_sd_file
from .subset.polygon import get_subset_area
from .eems import write_all_files, run_all
from .subset import filter_data


def run(params):
    if params.ind is not None:
        meta_data = load_combined_file(params.ind, params.ind_header,
                                       has_dataset=params.has_dataset)
    else:
        location_data = load_location_file(params.loc, params.location_header)
        sample_data = load_sample_file(params.sample, params.sample_header,
                                       has_dataset=params.has_dataset)

        meta_data = sample_data.merge(location_data)


    polygon, meta_data = get_subset_area(params, meta_data)
    meta_data = filter_data(meta_data=meta_data, bedfile=params.bed,
                            missing=params.max_missing, 
                            plink=params.plink,
                            outfile=params.plinkfile,
                            has_dataset=params.has_dataset)
    if params.sd is not None:
        sd_data = load_sd_file(params.sd)
        meta_data = meta_data.merge(sd_data)
    write_all_files(params, meta_data, polygon)
    meta_data.to_csv("meta.txt")
    run_all(params)


def run_cli():
    """run_cli

    runs the pipeline from the command line interface.
    see -h flag for arguments
    """
    params = read_params()
    run(params)

def read_params():
    params = Parameters.from_command_line()
    return params


if __name__ == "__main__":
    params = Parameters.from_command_line()
