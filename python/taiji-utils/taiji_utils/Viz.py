import scipy as sp
import numpy as np
import umap

def Viz(args):
    mat = np.loadtxt(args.input)
    if args.t == None:
        embedding = umap.UMAP(random_state=42,
            n_components=2,
            n_neighbors=args.k, 
            min_dist=0
            ).fit_transform(mat)
    else:
        with open(args.t, 'r') as fl:
            target = [l.strip() for l in fl]
        embedding = umap.UMAP(random_state=42,
            n_components=2,
            n_neighbors=args.k, 
            min_dist=0
            ).fit_transform(mat, y = target)
    np.savetxt(args.output, embedding, delimiter='\t')