import numpy as np
import itertools
from scipy.stats import chi2
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import log_loss
import statsmodels.api as sm
from statsmodels.tools import add_constant
from statsmodels.discrete.discrete_model import NegativeBinomial, Logit
from statsmodels.stats.multitest import multipletests
import math
import multiprocessing as mp

from .Utils import InputData, readMatrix

def diff(args):
    fg = readMatrix(args.input1, binary=True)
    (n1,m) = fg.shape
    fg_depth = np.log(np.sum(fg, axis=1))

    bg = readMatrix(args.input2, binary=True)
    n2, _ = bg.shape
    bg_depth = np.log(np.sum(bg, axis=1))

    X = np.concatenate((fg_depth, bg_depth))
    z = np.array([1] * n1 + [0] * n2)[:, np.newaxis]

    if args.index == None:
        idx_set = range(m)
    else:
        with open(args.index, 'r') as fl:
            idx_set = list(set([int(l.strip()) for l in fl]))

    result_list = []
    pool = mp.Pool(args.thread)
    for r in chunkIt(idx_set, 100):
        pool.apply_async(process,
            args=(r, fg, bg, idx_set, args.fold, X, z),
            callback = lambda x: result_list.append(x)
        ) 
    pool.close()
    pool.join()

    result = list(itertools.chain.from_iterable(result_list))
    np.savetxt( args.output, computeFDR(np.array(result)),
        header='index\tfraction_1\tfraction_2\tlog2_fold_change\tp-value\tFDR',
        fmt='%i\t%.5f\t%.5f\t%.5f\t%1.4e\t%1.4e' )

def process(r, fg, bg, idx_set, fd, X, z):
    print(r)
    result = []
    for ii in range(r[0], r[1]):
        i = idx_set[ii]
        y_fg = fg[:, i].todense()
        y_bg = bg[:, i].todense()

        f_fg = fraction(y_fg)
        f_bg = fraction(y_bg)
        fold = math.log2(f_fg / f_bg)

        if fd == None or abs(fold) >= fd: 
            Y = np.ravel(np.concatenate((y_fg, y_bg)))
            if np.sum(Y) == 0:
                p = 1
            else:
                p = likelihoodTest(X, Y, z)
            result.append([i, f_fg, f_bg, fold, p])
    return result

def fraction(X):
    return (np.count_nonzero(X) + 1) / (X.shape[0] + 1)

def computeFDR(X):
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

def fitPoisson(X, Y):
    return sm.GLM(Y, X, family=sm.families.Poisson()).fit()

'''
def fitNB(X, Y):
    poisson_training_results = fitPoisson(X, Y)

    # STEP 2: We will now fit the auxiliary OLS regression model on the data set and use the fitted model to get the value of α.
    X_ = poisson_training_results.mu
    Y_ = np.array(list(((b - a)**2 - b / a) for (a,b) in zip(X_, Y)))
    X_ = add_constant(X_)
    aux_olsr_results = sm.OLS(Y_, X_).fit()

    # STEP 3: We supply the value of alpha found in STEP 2 into the statsmodels.genmod.families.family.NegativeBinomial class, and train the NB2 model on the training data set.
    return sm.GLM(Y, X,family=sm.families.NegativeBinomial(alpha=aux_olsr_results.params[1])).fit()
'''
def fitNB(X, Y):
    return NegativeBinomial(Y, X).fit()

def fitLogistic(X, Y):
    X = add_constant(X)
    return Logit(Y, X).fit(maxiter=100, method="bfgs")


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