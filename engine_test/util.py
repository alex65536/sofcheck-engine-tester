def human_time(secs):
    if secs < 60:
        return '{:.2f} sec'.format(secs)
    if secs < 3600:
        return '{:.2f} min'.format(secs / 60)
    if secs < 3600 * 24:
        return '{:.2f} hours'.format(secs / 3600)
    return '{:2f} days'.format(secs / (3600 * 24))
