import os


def standardize_bed_file_name(bedfile):
    """ removes the bed extension from bed name if present"""
    fname, ext = os.path.splitext(bedfile)
    if ext == ".bed":
        return fname
    return bedfile


def assert_bed_file_exists(bedfile):
        pass


def assert_vcf_file_exists(bedfile):
        pass

