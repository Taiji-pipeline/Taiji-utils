import scipy as sp
import numpy as np
import math
from sklearn.neighbors import kneighbors_graph
import igraph as ig
import umap

from .Spectral import spectral
from .Clustering import leiden

def reduceDimension(args):
    if args.method == "spectral":
        spectral(args)
    else:
        print("Unknown method")

"""
args.input: a list of files.
args.k: number of neighbours.
args.output: output file.
args.embed: embeding file.
"""
def mkKNNGraph(args):
    fls = args.input.split(',')
    adj = None
    for fl in fls:
        mat = readCoordinates(fl)
        if (adj == None):
            adj = kneighbors_graph(mat, args.k, mode='distance', n_jobs=args.thread)
        else:
            adj += kneighbors_graph(mat, args.k, mode='distance', n_jobs=args.thread)
    adj = adj / len(fls)
    np.reciprocal(adj.data, out=adj.data)
    sp.sparse.save_npz(args.output, adj)

    if(args.embed):
        print("Create Embedding:")
        data = readCoordinates(fls[0])
        getEmbedding(data, args.embed)

def clustering(args):
    adj = sp.sparse.load_npz(args.input)
    vcount = max(adj.shape)
    sources, targets = adj.nonzero()
    edgelist = list(zip(sources.tolist(), targets.tolist()))
    weights = np.ravel(adj[(sources, targets)])
    gr = ig.Graph(n=vcount, edges=edgelist, edge_attrs={"weight": weights})

    print("Start Clustering...")
    partition = leiden(gr, resolution=args.res, optimizer=args.optimizer)

    n = 0
    with open(args.output, 'w') as f:
        cutoff = max(1, min(50, 0.001*vcount))
        for c in partition:
            if len(c) > cutoff:
                n = n + 1
                print(','.join(map(str, c)), file=f) 
    print(n)

def readCoordinates(fl, n_dim=None, discard=None, scale=None):
    def scaling(xs):
        s = 0
        for x in xs:
            s = s + x * x
        s = math.sqrt(s)
        return np.array([x / s for x in xs])
    data = np.loadtxt(fl)
    if (n_dim):
        data = data[..., :n_dim]
    if (discard):
        data = data[..., 1:]
    if (scale):
        data = np.apply_along_axis(scaling, 1, data)
    return data

def getEmbedding(mat, output):
    e1 = umap.UMAP(random_state=42, n_components=2).fit_transform(mat)
    e2 = umap.UMAP(random_state=42, n_components=3).fit_transform(mat)
    embedding = np.concatenate((e1, e2), axis=1)
    np.savetxt(output, embedding, delimiter='\t')

