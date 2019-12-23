import scrublet as scr
import scipy as sp
import numpy as np
from sklearn.mixture import BayesianGaussianMixture

from .Utils import readMatrix

def detectDoublet(args):
    counts_matrix = readMatrix(args.input, binary=False)
    scrub = scr.Scrublet(counts_matrix,
        expected_doublet_rate = 0.06, sim_doublet_ratio=3, n_neighbors=25)
    doublet_scores, _ = scrub.scrub_doublets(
        min_counts=1, 
        min_cells=3, 
        min_gene_variability_pctl=85,
        mean_center=True,                                                                                                                                                                                                                                                                
        normalize_variance=True,
        n_prin_comps=min(30, counts_matrix.get_shape()[0] // 10)
        )

    # Fit a Gaussian mixture model
    X = scrub.doublet_scores_sim_
    X = np.array([X]).T
    gmm = BayesianGaussianMixture(n_components=2, max_iter=1000,
        random_state=2394).fit(X)
    i = np.argmax(gmm.means_)

    probs_sim = gmm.predict_proba(X)[:,i]
    vals = X[np.argwhere(probs_sim>0.5)].flatten()
    if vals.size == 0:
        threshold = np.amax(X.flatten())
    else:
        threshold = min(vals)

    X = np.array([doublet_scores]).T
    probs = gmm.predict_proba(X)[:,i].tolist()

    with open(args.output, 'w') as fl:
        fl.write('\t'.join(map(str, probs)))
        fl.write("\n")

        fl.write(str(threshold))
        fl.write("\n")
        fl.write('\t'.join(map(str, (doublet_scores.tolist()))))
        fl.write("\n")
        fl.write('\t'.join(map(str, scrub.doublet_scores_sim_)))