import scipy as sp
import numpy as np
import umap

def Viz(args):
    mat = np.loadtxt(args.input)
    embedding = umap.UMAP(random_state=42,
        n_components=2,
        n_neighbors=args.k, 
        min_dist=0
        ).fit_transform(mat)
    np.savetxt(args.output, embedding, delimiter='\t')