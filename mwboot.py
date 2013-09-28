
import numpy as np
from scipy.stats import binom
import pandas as pd
from progressbar import ProgressBar

def multiway_boot(df, reps, levels, show_progress=False):
    """
    Generate a sequence of bootstrap samples from a dataframe.

    Parameters
    ----------
    df : a Pandas DataFrame object
    reps : number of reps
    levels : 1-K levels in a list.
    
    The dataframe must have a hierarchical index for these levels.
    See example below.
    """
    
    
    if show_progress:
        iterator = ProgressBar(maxval=reps)(range(reps))
    else:
        iterator = range(reps)

    indexes = [df.groupby(level=l).apply(lambda x:1) for l in levels]
        
    for i in iterator:
        weight = np.prod([
            pd.Series(
                2*binom.rvs(1, 0.5, size=ix.shape[0]),
                index=ix.index,
                name='%s_weight' % level).reindex(df.index, level=level)
            for ix, level in zip(indexes, levels)
            ], axis=0)
        replicate = df[weight > 0]
        replicate['rep'] = i
        replicate['weight'] = weight[weight > 0]
        yield replicate


def example():
    levels = [['bar', 'bar', 'baz', 'baz', 'foo', 'foo', 'qux', 'qux'],
              ['one', 'two', 'one', 'two', 'one', 'two', 'one', 'two']]
    index = pd.MultiIndex.from_tuples(zip(*levels), names=['first', 'second'])
    df = pd.DataFrame(np.random.randn(8, 4), index=index)
    
    means = []
    for rep in multiway_boot(df, 5000, ['first', 'second']):
        if rep: # in case the dataframe is empty
            means.append(rep[0].mean())

    print 'median =', np.median(means)
    print '[%.2f, %.2f]' % (
        np.percentile(means, 2.5),
        np.percentile(means, 97.5),
        )
    
