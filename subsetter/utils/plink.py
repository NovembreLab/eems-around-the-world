import os
import logging 


def run_plink(plink, flags):
    """runs plink. Parameters are handled with the flags command


    
    Parameters
    ----------
    plink : path
        Path to a plink 1.9 executable
    flags : dict
        A dictionary, that contains the arguments for plink. an entry
        flags[k] => v results in "--k v"
    
    """
    
    s = "%s " % plink
    for item in flags.items():
        s += "--%s %s " % item

    # s += " >>plink.log"

    print(s)
    os.system(s)
    # logging.debug("ran plink with the following command\n%s" % s)
