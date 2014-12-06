

import os
import logging
import logging.config

# TODO: Look in local path as well as user home path
# Use a config file written as a Python dictionary.
# The config file is used to initialize logging and FVS library paths.
config_path = os.path.join(os.path.split(__file__)[0], 'pyfvs.cfg')

def get_config():
    """
    Return the configuration dict.
    """
    cfg = eval(open(config_path).read())
    return cfg

def init_logging():
    """
    Initialize package wide logging from the configuration file.
    """
    logging.config.dictConfig(get_config()['logging'])

# init_logging()
# log = logging.getLogger('pyfvs')
# log.debug('logging initiated')
