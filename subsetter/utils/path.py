import os


def make_full_path(path, fi):
    if fi is None:
        return fi
    if os.path.isabs(fi):
        return fi
    else:
        return os.path.sep.join((path, fi))


