import numpy as np
from statsmodels.tools import add_constant	
from statsmodels.discrete.discrete_model import NegativeBinomial
import statsmodels.api as sm
import math
from scipy import stats

from sklearn.svm import SVR
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import learning_curve
from sklearn.kernel_ridge import KernelRidge
import matplotlib.pyplot as plt

from .Utils import readMatrix

def fitSmooth(X, Y):
    X = X[..., np.newaxis]
    #svr = GridSearchCV(SVR(kernel='rbf', gamma=0.1),
    #    param_grid={"C": [1e0, 1e1, 1e2, 1e3],
    #    "gamma": np.logspace(-2, 2, 5)})
    model = GridSearchCV(KernelRidge(kernel='rbf', gamma=0.1),
        param_grid={"alpha": [1e0, 0.1, 1e-2, 1e-3],
        "gamma": np.logspace(-2, 2, 5)})
    model.fit(X, Y)

    '''
    X_plot = np.linspace(np.min(X), np.max(X), 10000)[:, None]
    y_ = model.predict(X_plot)
    plt.scatter(X, Y, c='k', label='data', zorder=1,
                edgecolors=(0, 0, 0))
    plt.plot(X_plot, y_, c='r', label='SVR')
    plt.xlabel('data')
    plt.ylabel('target')
    plt.title('SVR')
    plt.legend()
    plt.savefig('1.png')
    '''

    return model

def fitNB(X, Y):
    X = add_constant(X)	
    model = NegativeBinomial(Y, X).fit(disp=0, skip_hessian=True, maxiter=500)
    if not model.mle_retvals['converged']:
        model = NegativeBinomial(Y, X).fit_regularized(disp=0, skip_hessian=True, maxiter=500)
    return model.params

def fitNB_(X, Y):
    def convert(y,mu):
        return ((y - mu)**2 - mu) / mu

    poisson_training_results = fitPoisson(X, Y)	
    # STEP 2: We will now fit the auxiliary OLS regression model on the data set and use the fitted model to get the value of α.	
    X_ = poisson_training_results.mu
    Y_ = np.array(list(convert(a,b) for (a,b) in zip(X_, Y)))
    aux_olsr_results = sm.OLS(Y_, X_).fit(disp=0)
    # STEP 3: We supply the value of alpha found in STEP 2 into the statsmodels.genmod.families.family.NegativeBinomial class, and train the NB2 model on the training data set.	
    X = add_constant(X)
    return sm.GLM(Y, X,family=sm.families.NegativeBinomial(alpha=aux_olsr_results.params[0])).fit(disp=0)

def fitPoisson(X, Y):
    X = add_constant(X)	
    return sm.GLM(Y, X, family=sm.families.Poisson()).fit(disp=0)

'''
def sctransform(cellxgene, nFeat=3000):
    r, c = cellxgene.get_shape()
    X = np.log10(cellxgene.sum(axis=1))
    gene_mean = gmeans(cellxgene)
    params = []
    idx = kdeSample(gene_mean)
    for i in idx:
        Y = cellxgene.getcol(i).todense()
        params.append(fitNB(X, Y))
    params = np.array(params)
    gene_mean = np.log10(gene_mean)
    beta0_fit = fitSmooth(gene_mean[idx], params[:, 0]) 
    beta1_fit = fitSmooth(gene_mean[idx], params[:, 1]) 
    alpha_fit = fitSmooth(gene_mean[idx], np.log(params[:, 2]))
    beta0 = beta0_fit.predict(gene_mean[:, None])
    beta1 = beta1_fit.predict(gene_mean[:, None])
    alpha = np.exp(alpha_fit.predict(gene_mean[:, None]))

    mu = np.exp(np.tile(beta0[np.newaxis, :], (r, 1)) + np.matmul(X[:, np.newaxis], beta1[np.newaxis, :]))
    sigma = np.sqrt(mu + np.multiply(np.tile(alpha[np.newaxis, :], (r, 1)),  np.square(mu)))
    cellxgene = cellxgene.todense()
    z = np.clip((cellxgene - mu) / sigma, a_min=-math.sqrt(r), a_max=math.sqrt(r))
    dispersion = np.ravel(np.var(z, axis=0))
    features = dispersion.argsort()[-nFeat:]
    return z[:, features]

def kdeSample(X, nGene=2000):
    weights = np.reciprocal(stats.gaussian_kde(X, bw_method="silverman").evaluate(X))
    prob = weights / np.sum(weights)
    return np.random.choice(X.shape[0], size=nGene, replace=False, p=prob)
'''

# apply column-wise geometric mean
def gmeans(cellxgene):
    return np.ravel(np.expm1(cellxgene.log1p().mean(axis=0)))

def sctransform(cellxgene, cell_reads, gene_mean, new_data):
    _, c = cellxgene.get_shape()

    # fit NB model for each gene
    params = []
    for i in range(c):
        Y = cellxgene.getcol(i).todense()
        params.append(fitNB(cell_reads, Y))
    params = np.array(params)

    gene_mean = np.log10(gene_mean)

    beta0_fit = fitSmooth(gene_mean, params[:, 0]) 
    beta1_fit = fitSmooth(gene_mean, params[:, 1]) 
    alpha_fit = fitSmooth(gene_mean, np.log(params[:, 2]))

    new_data = np.log10(new_data)
    beta0 = beta0_fit.predict(new_data[:, None])
    beta1 = beta1_fit.predict(new_data[:, None])
    alpha = np.exp(alpha_fit.predict(new_data[:, None]))
    return(np.array([beta0, beta1, alpha]))

def normalize(args):
    np.random.seed(0) 
    inputMat = readMatrix(args.input)

    with open(args.genemean, 'r') as fl:
        geneMean = [float(l.strip()) for l in fl]
    with open(args.cellreads, 'r') as fl:
        cellReads = [float(l.strip()) for l in fl]
    with open(args.data, 'r') as fl:
        data = [float(l.strip()) for l in fl]
    np.savetxt(args.output, sctransform(inputMat, cellReads, geneMean, data))

    '''
    v = np.ravel(np.var(z, axis=0))
    plt.scatter(gm, v, c='k', label='data', zorder=1, edgecolors=(0, 0, 0))
    plt.xlabel('data')
    plt.ylabel('target')
    plt.title('SVR')
    plt.legend()
    plt.savefig('1.png')
    '''

    #all_data = np.append(np.array(a), b.reshape(2000,1), 1)