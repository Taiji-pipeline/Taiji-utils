import numpy as np
from statsmodels.nonparametric.kernel_regression import KernelReg

from .Utils import readMatrix

def fitSmooth(X, Y, newX, output=None):
    X = X[..., np.newaxis]
    if output is not None:
        import matplotlib.pyplot as plt
        plt.figure()
        X_plot = np.linspace(np.min(X), np.max(X), 10000)[:, None]
        y_, _ = KernelReg(Y, X, 'c').fit(X_plot)
        plt.scatter(X, Y, c='k', label='data', zorder=1,
                    edgecolors=(0, 0, 0))
        plt.plot(X_plot, y_, c='r', label='fit')
        plt.xlabel('log(gene mean)')
        plt.ylabel('parameter')
        plt.legend()
        plt.savefig(output)
        plt.close()
    return KernelReg(Y, X, 'c').fit(newX)[0]

'''
Y: numpy array, rows are samples, columns are genes 
X: numpy array, columns are covariates, rows are samples.
'''
def fitNB2(X, Y):
    from rpy2.robjects.packages import importr
    import rpy2.robjects as robjects
    from rpy2.robjects import numpy2ri
    numpy2ri.activate()

    glmGamPoi = importr('glmGamPoi')
    robjects.r('''
        fit_glmGamPoi <- function(X, Y) {
            fit <- glmGamPoi::glm_gp(data = t(Y),
                           design = ~ .,
                           col_data = as.data.frame(X),
                           size_factors = FALSE)
            fit$theta <- pmin(1 / fit$overdispersions, rowMeans(fit$Mu) / 1e-4)
            colnames(fit$Beta)[match(x = 'Intercept', colnames(fit$Beta))] <- "(Intercept)"
            return(cbind(fit$Beta, fit$theta))
        }
        ''')

    rfit = robjects.r['fit_glmGamPoi']
    res = np.array(rfit(X, Y))
    numpy2ri.deactivate()
    return(res)

def sctransform(cellxgene, cell_reads, log_gene_mean, new_data, dir):
    params = fitNB2(cell_reads, cellxgene.todense())
    beta0 = fitSmooth(log_gene_mean, params[:, 0], new_data[:, None], output=dir + "/beta0.png")
    beta1 = fitSmooth(log_gene_mean, params[:, 1], new_data[:, None], output=dir + "/beta1.png") 

    theta_proxy = np.log10(1 + 10**log_gene_mean / params[:, 2])
    
    # variance of NB is mu * (1 + mu / theta)
    # (1 + mu / theta) is what we call overdispersion factor here
    od_factor = fitSmooth(log_gene_mean, theta_proxy, new_data[:, None], output=dir + "/theta.png")
    theta = 10**new_data / (10**od_factor - 1)

    return(np.array([beta0, beta1, 1 / theta]))

def normalize(args):
    np.random.seed(0) 
    inputMat = readMatrix(args.input)

    with open(args.genemean, 'r') as fl:
        geneMean = np.array([float(l.strip()) for l in fl])
    with open(args.cellreads, 'r') as fl:
        cellReads = np.array([float(l.strip()) for l in fl])
    with open(args.data, 'r') as fl:
        data = np.array([float(l.strip()) for l in fl])
    np.savetxt(args.output, sctransform(inputMat, cellReads, geneMean, data, args.plot_dir))