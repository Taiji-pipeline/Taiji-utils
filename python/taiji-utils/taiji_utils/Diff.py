import numpy as np
from scipy.stats import chi2
import statsmodels.api as sm
from statsmodels.tools import add_constant
from statsmodels.discrete.discrete_model import NegativeBinomial, Logit
from statsmodels.stats.multitest import multipletests
import math

from .Utils import InputData, readMatrix

def diff(args):
    fg = readMatrix(args.input1, binary=True)
    (n1,m) = fg.shape
    fg_depth = np.log(np.sum(fg, axis=1))
    #np.random.shuffle(fg_depth)

    bg = readMatrix(args.input2, binary=True)
    n2, _ = bg.shape
    bg_depth = np.log(np.sum(bg, axis=1))

    X = np.concatenate((fg_depth, bg_depth))
    z = np.array([1] * n1 + [0] * n2)[:, np.newaxis]

    res = []
    if args.index == None:
        idx_set = range(m)
    else:
        with open(args.index, 'r') as fl:
            idx_set = set([int(l.strip()) for l in fl])
    for i in idx_set:
        y_fg = fg[:, i].todense()
        y_bg = bg[:, i].todense()

        f_fg = fraction(y_fg)
        f_bg = fraction(y_bg)
        fold = math.log2(f_fg / f_bg)

        if args.fold == None or abs(fold) >= args.fold: 
            Y = np.concatenate((y_fg, y_bg))
            if np.sum(Y) == 0:
                p = 1
            else:
                p = likelihoodTest(fitLogistic, X, Y, z)
            res.append([i, f_fg, f_bg, fold, p])
    np.savetxt( args.output, computeFDR(np.array(res)),
        delimiter='\t',
        header='index\tfraction_1\tfraction_2\tlog2_fold_change\tp-value\tFDR',
        fmt='%i %10.5f %10.5f %10.5f %1.4e %1.4e' )

def fraction(X):
    return (np.count_nonzero(X) + 1) / (X.shape[0] + 1)

def computeFDR(X):
    pvals = np.array([multipletests(X[:, 4], method='fdr_bh')[1]]).T
    return np.append(X, pvals, axis=1)

def likelihoodTest(fit, X, Y, z):
    reduced = fit(X, Y).llf
    X = np.concatenate((X, z), axis=1)
    full = fit(X, Y).llf
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