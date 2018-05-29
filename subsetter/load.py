import pandas as pd
import os

# possible ids for population
allowed_pop_names = ['POP', 'POP_ID', 'POP_NAME', 'POP_ORIG',
                     'POPULATION', 'ECOTYPE_ID', 'POPULATION_ID',
                     "verbose Population ID", 'Subspecies',
                     'popLabel']

# possible ids for individuals
allowed_ind_names = ['SAMPLE', 'IID', 'sampleId', 'sampleIdIVIDUAL',
                     'sampleIdIVIDUALS', 'ID', 'SAMPLE', 'ILLUMINA_ID',
                     'SAMPLE_ID', 'SAMPLEID']

# possible ids for latitude & longitude
allowed_lat_names = ['LAT', 'LATITUDE', 'Y', 'LAT-ITUDE']
allowed_long_names = ['LONG', 'LONGITUDE', 'X', 'LON-GI-TUDE']
allowed_sd_names = ['SD', 'ACCURACY']

allowed_dataset_names = ['DATASET', 'SOURCE', 'source']

CSV_FORMATS = ["csv", "pop_meta", "indiv_label", "indiv_meta"]

def load_combined_file(combined_file, combined_has_header=True, format=None,
                       column_names=None, wrap=True, 
                       has_dataset=False, **kwargs):
    """load_sample_file
    
    loads individuals with sampling coords directly

    Parameters
    ----------
    combined_file : file or filename
        The file to assign samples to populations
    combined_has_header : bool
        does the sample have a header row
    format : str
        the format of the file, allowed formats are xls, xlsx, or csv. If not
        given, it is guessed based on extension. Otherwise, read_table is used.
    column_names : (str, str)
        the names of the columns to be used for sample id and population id,
        respectively.
    
    Returns
    -------

    sample_data : pd.DataFrame
        a data frame with columns 'sampleId' and `POP` 'latitude' 'longitude' 
    """
    read_function = get_read_fun_from_extension(combined_file, format)

    if combined_has_header:
        sample_data = read_function(combined_file, **kwargs)
    else:
        sample_data = read_function(combined_file, header=None, **kwargs)
        sample_data.columns[:3] = ['sampleId', 'latitude' 'longitude']

    header = sample_data.columns.values
    
    if column_names is None:
        POP = get_first_id(header, allowed_pop_names)
        sampleId = get_first_id(header, allowed_ind_names)
        latitude = get_first_id(header, allowed_lat_names)
        longitude = get_first_id(header, allowed_long_names)
        if has_dataset:
            SET = get_first_id(header, allowed_long_names)
    else:
        POP, sampleId = column_names

    if has_dataset:
        sample_data = sample_data[[sampleId, POP, latitude, longitude, SET]]
        sample_data.columns = ['sampleId', 'POP', 'latitude', 'longitude', 'SET']
    else:
        sample_data = sample_data[[sampleId, POP, latitude, longitude]]
        sample_data.columns = ['sampleId', 'POP', 'latitude', 'longitude']

    if wrap:
        sample_data['longitude'] = wrap_america(sample_data['longitude'])

    return sample_data


def load_location_file(location_file, location_has_header=True, format=None,
                       column_names=None, wrap=True, has_accuracy=True,
                       **keywords):
    """load_location_file
    
    this function loads the location data using lat, long and pop_id. 
        The approach here is that I'll draw them from a list of possible ids,
        but the resulting data frame will always have three cols named POP, latitude, longitude.

    Parameters
    ----------
    location_file : path
        the file with the path
    location_has_header : bool
        does the location file has a header row?
    format : str
        the format of the file, allowed formats are xls, xlsx, or csv. If not
        given, it is guessed based on extension. Otherwise, read_table is used.
    wrap : bool
        should coordinates mapping to the americas be wrapped around?
    column_names : (str, str, str)
        the names of the columns to be used for population id, latitude and
        longitude, respectively.
    has_accuracy: bool
        wheter an accuracy col is included
    
    Returns
    -------
    location_data : pandas.DataFrame
        a table containing the location information
    """

    read_function = get_read_fun_from_extension(location_file, format)

    if location_has_header:
        location_data = read_function(location_file, **keywords)
    else:
        location_data = read_function(location_file, header=None, **keywords)
        headers = list(location_data.columns)
        headers[:3] = ['POP', 'LATITUDE', 'LATITUDE']
        location_data.columns = headers

    header = location_data.columns.values
    
    if column_names is None:
        # possible ids for population, possibly add more
        POP = get_first_id(header, allowed_pop_names)
        latitude = get_first_id(header, allowed_lat_names)
        longitude = get_first_id(header, allowed_long_names)
        if has_accuracy:
            SD = get_first_id(header, allowed_sd_names)

    else:
        if has_accuracy:
            POP, latitude, longitude, SD = column_names
        else:
            POP, latitude, longitude = column_names

    if has_accuracy:
        location_data = location_data[[POP, latitude, longitude, SD]]
        location_data.columns = ['POP', 'latitude', 'longitude', 'SD']
    else:
        location_data = location_data[[POP, latitude, longitude]]
        location_data.columns = ['POP', 'latitude', 'longitude']
    
    if wrap:
        location_data['longitude'] = wrap_america(location_data['longitude'])

    return location_data.drop_duplicates()


def load_sample_file(sample_file, sample_has_header=True, format=None,
                     column_names=None, has_dataset=False, **kwargs):
    """load_sample_file
    
    this file loads the sample ids that are used when analyzing VCF/bed files

    Parameters
    ----------
    sample_file : file or filename
        The file to assign samples to populations
    sample_has_header : bool
        does the sample have a header row
    format : str
        the format of the file, allowed formats are xls, xlsx, or csv. If not
        given, it is guessed based on extension. Otherwise, read_table is used.
    column_names : (str, str)
        the names of the columns to be used for sample id and population id,
        respectively.
    
    Returns
    -------

    sample_data : pd.DataFrame
        a data frame with columns 'SAMPLE' and `POP` that assigns samples
        to populations
    """
    read_function = get_read_fun_from_extension(sample_file, format)

    if sample_has_header:
        sample_data = read_function(sample_file, **kwargs)
    else:
        sample_data = read_function(sample_file, header=None, **kwargs)
        if has_dataset:
            sample_data.columns[:3] = ['SAMPLE', 'POP', 'DATASET']
        else:
            sample_data.columns[:2] = ['SAMPLE', 'POP']

    header = sample_data.columns.values
    
    if column_names is None:
        POP = get_first_id(header, allowed_pop_names)
        sampleId = get_first_id(header, allowed_ind_names)
        SET = get_first_id(header, allowed_dataset_names)
    else:
        POP, sampleId, SET = column_names

    sample_data = sample_data[[sampleId, POP, SET]]
    sample_data.columns = ['sampleId', 'POP', 'DATASET']

    return sample_data


def get_read_fun_from_extension(file_name, format):
    """
    gets the proper pd function to read a data set
    """
    if format is None:
        format = os.path.splitext(file_name)[1]

    if format.startswith("."):
        format = format[1:]
    
    if format == "xlsx" or format == "xls":
        read_function = pd.read_excel
    elif format in CSV_FORMATS:
        print("READING AS CSV")
        read_function = pd.read_csv
    else:
        print("READING TABLE")
        read_function = pd.read_table

    return read_function


def get_first_id(header, allowed_names):
    """private function that gets the first match from the possible list of matches"""
    for name in allowed_names:
        for i, h in enumerate(header):
            if str(name).upper() == str(h).upper():
                return h
    raise ValueError('did not find correct header for %s.\
                     Please adjust file/ script'%allowed_names)


def wrap_america(data):
    return [i + 360 if i < -45 else i for i in data]


def unwrap_america(data):
    return [i - 360 if i >= 180 else i for i in data]


def load_sd_file(sd_file, sd_has_header=True, format=None, **kwargs):
    """load_sd_file
    
    loads standard deviation for samples with unknown location
    if this option is used, it requires for each sample to have
    a degree of uncertainty in it's location. Instead of fixed
    sample location, samples will then have a bivariate normal
    distribution with mean given by the coordinates in the other
    file, and sd given here. The sd_file has two columns:
    the first gives the individual id, the second one gives the sd of
    the normal

    Parameters
    ----------
    sd_file : file or filename
        The file that assignes sd to sample distribution
    sd_has_headere : bool
        Does the file have a header line?
    format : str
        the format of the file, allowed formats are xls, xlsx, or csv. If not
        given, it is guessed based on extension. Otherwise, read_table is used.
    
    Returns
    -------

    sd_data : pd.DataFrame
        a data frame with columns 'sampleId' and `SD`
    """
    read_function = get_read_fun_from_extension(sd_file, format)

    if sd_has_header:
        sd_data = read_function(sd_file, **kwargs)
    else:
        sd_data = read_function(sd_file, header=None, **kwargs)
        sd_data.columns[:2] = ['sampleId', 'SD']

    header = sd_data.columns.values
    
    sampleId = get_first_id(header, allowed_ind_names)
    SD = get_first_id(header, ['SD'])

    sd_data = sd_data[[sampleId, SD]]
    sd_data.columns = ['sampleId', 'SD']


    return sd_data


def load_pop_meta(pop_meta, wrap=True):
    f = pd.read_csv(pop_meta)
    if wrap:
        f.longitude = wrap_america(f.longitude)
    return f

def load_indiv_meta(indiv_meta):
    return pd.read_csv(indiv_meta)
