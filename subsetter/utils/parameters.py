import argparse


class Parameters(object):
    """ class that handles parameters I/O

    the main idea is that this class unifies the different ways a project can
    be loaded. Current options are:
        - a dict like object, passed to init
        - from an input file
        - from the command line
        
    the main input format is from the command line
    """

    def __init__(self, **kwargs):
        self.create_parser()
        for k, v in kwargs.items():
            setattr(self, k, v)

    @staticmethod
    def create_from_dict(d, defaults=True):
        """create_from_dict
        
        allows creation of a set of parameters from a dictionary.
        
        
        Parameters
        ----------
        d : dict
            Dictionary with format 'args' => val
        defaults : bool
            if True, then argparse is run once to get default arguments
        
        Returns
        -------
        params : Parameters
            the parameters object
        """

        params = Parameters(**d)
        if defaults:
            defaults = Parameters.parser.parse_args("")
            for k, v in defaults.iteritems():
                if not hasattr(params, k):
                    params.k = v
        return params

    @staticmethod
    def create_parser():
        """
        generates ArgumentParser and reads options from CLI

        use -h flag for details
        """
        raise NotImplementedError()

        if hasattr(Parameters, "parser"):
            return
        args = argparse.ArgumentParser("default parser, override this")
    
        return args

    @staticmethod
    def from_command_line():
        """loads arguments from command line
        
        Returns
        -------
        p : Parameters
            the parameters object read from the command line
        """

        Parameters.create_parser()
        parser = Parameters.parser
        params = parser.parse_args()

        p = Parameters(**params.__dict__)
        p.postprocess_args()
        return p

    def postprocess_args(p):
        """
        processes some args in p
        """
        pass
