try:
    from ._version import __version__, __version_tuple__
except Exception as e:
    import warnings
    warnings.warn(f'Failed to read commit hash:\n{e}', RuntimeWarning, stacklevel=2)
    __version__ = 'dev'
    __version_tuple__ = (0, 0, __version__)
def _prev_minor_version_was(version_str):
    if __version_tuple__[0:2] == (0, 0):
        return True
    assert __version_tuple__[0] == 0
    assert isinstance(__version_tuple__[1], int)
    return version_str == f'{__version_tuple__[0]}.{__version_tuple__[1] - 1}'
def _prev_minor_version():
    assert isinstance(__version_tuple__[1], int)
    return f'{__version_tuple__[0]}.{__version_tuple__[1] - 1}'