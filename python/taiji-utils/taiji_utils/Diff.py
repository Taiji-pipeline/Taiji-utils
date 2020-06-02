import numpy as np
import itertools
from scipy.stats import chi2
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import log_loss
import statsmodels.api as sm
from statsmodels.stats.multitest import multipletests
import math
import multiprocessing as mp

from .Utils import InputData, readMatrix

def diff(args):
    fg = readMatrix(args.input1, binary=False)
    fg_depth = np.log(np.sum(fg, axis=1))
    fg_total = np.sum(fg) / 1000000
    (n1,m) = fg.shape

    bg = readMatrix(args.input2, binary=False)
    bg_depth = np.log(np.sum(bg, axis=1))
    bg_total = np.sum(bg) / 1000000
    n2, _ = bg.shape

    X = np.concatenate((fg_depth, bg_depth))
    z = np.array([1] * n1 + [0] * n2)[:, np.newaxis]

    if args.index == None:
        idx_set = range(m)
    else:
        with open(args.index, 'r') as fl:
            idx_set = list(set([int(l.strip()) for l in fl]))

    result_list = []
    '''
    pool = mp.Pool(args.thread)
    for r in chunkIt(idx_set, 20):
        pool.apply_async(process,
            args=(r, fg, bg, idx_set, math.log2(args.fold), X, z, fg_total, bg_total),
            callback = lambda x: result_list.append(x),
            error_callback = lambda x: print(x)
        ) 
    pool.close()
    pool.join()
    '''
    for r in chunkIt(idx_set, 20):
        x = process(r, fg, bg, idx_set, math.log2(args.fold), X, z, fg_total, bg_total)
        result_list.append(x)
    result = list(itertools.chain.from_iterable(result_list))
    np.savetxt( args.output, computeFDR(np.array(result)),
        header='index\tfraction_1\tfraction_2\tlog2_fold_change\tp-value\tFDR',
        fmt='%i\t%.5f\t%.5f\t%.5f\t%1.4e\t%1.4e' )

def process(r, fg, bg, idx_set, fd, X, z, rc_fg, rc_bg):
    print(r)
    result = []
    for ii in range(r[0], r[1]):
        i = idx_set[ii]
        y_fg = fg[:, i].todense()
        y_bg = bg[:, i].todense()

        f_fg = (np.sum(y_fg) + 1) / rc_fg
        f_bg = (np.sum(y_bg) + 1) / rc_bg
        fold = math.log2(f_fg / f_bg)

        if fd == None or abs(fold) >= fd: 
            Y = np.ravel(np.concatenate((np.clip(y_fg,0,1), np.clip(y_bg,0,1))))
            if np.sum(Y) == 0:
                p = 1
            else:
                p = likelihoodTest(X, Y, z)
            result.append([i, f_fg, f_bg, fold, p])
    return result

def computeFDR(X):
    if X.size == 0:
        return X
    else:
        pvals = np.array([multipletests(X[:, 4], method='fdr_bh')[1]]).T
        return np.append(X, pvals, axis=1)

def likelihoodTest(X, Y, z):
    model = LogisticRegression(penalty="none", random_state=0, n_jobs=1,
        solver="lbfgs", multi_class='ovr', tol=1e-3, warm_start=False
        ).fit(X, Y)
    reduced = -log_loss(Y, model.predict_proba(X), normalize=False)

    X = np.concatenate((X, z), axis=1)
    model = LogisticRegression(penalty="none", random_state=0, n_jobs=1,
        solver="lbfgs", multi_class='ovr', tol=1e-3, warm_start=False
        ).fit(X, Y)
    full = -log_loss(Y, model.predict_proba(X), normalize=False)
    chi = -2 * (reduced - full)
    return chi2.sf(chi, 1)

def chunkIt(seq, num):
    n = len(seq)
    step = math.ceil(n / num)
    last = 0
    while last < n:
        i = last + step
        if i <= n:
            yield (last, i)
        else:
            yield (last, n)
        last += step