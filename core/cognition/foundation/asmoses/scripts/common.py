import datetime
def datetime_from_str(time_str):
    fmt = '%Y-%m-%d %H:%M:%S:%f'
    return datetime.datetime.strptime(time_str, fmt)