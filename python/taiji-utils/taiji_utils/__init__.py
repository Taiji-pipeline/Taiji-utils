import scipy as sp
import numpy as np
import math
from sklearn.neighbors import kneighbors_graph
import igraph as ig
import leidenalg as la
import umap

from .Spectral import spectral

def reduceDimension(args):
    if args.method == "spectral":
        spectral(args)
    else:
        print("Unknown method")

def getEmbedding(mat, output):
    e1 = umap.UMAP(random_state=42, n_components=2).fit_transform(mat)
    e2 = umap.UMAP(random_state=42, n_components=3).fit_transform(mat)
    embedding = np.concatenate((e1, e2), axis=1)
    np.savetxt(output, embedding, delimiter='\t')

def clustering(args):
    fls = args.input.split(',')
    gr = mkKNNGraph(fls, k=args.k)

    print("Start clustering")
    partition = leiden(gr, resolution=args.res)

    print("Clusters: ")
    print(len(partition)) 
    with open(args.output, 'w') as f:
        f.write('\n'.join([','.join(map(str, c)) for c in partition]))

    if(args.embed):
        print("Create Embedding:")
        data = readCoordinates(fls[0], n_dim=args.dim,
           discard=args.discard, scale=args.scale)
        getEmbedding(data, args.embed)

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

def mkKNNGraph(fls, k=25):
    adj = None
    for fl in fls:
        mat = readCoordinates(fl)
        if (adj == None):
            adj = kneighbors_graph(mat, k, mode='distance')
        else:
            adj += kneighbors_graph(mat, k, mode='distance')
    vcount = max(adj.shape)
    sources, targets = adj.nonzero()
    edgelist = list(zip(sources.tolist(), targets.tolist()))

    weights = 1 / (np.ravel(adj[(sources, targets)]) / len(fls))
    gr = ig.Graph(n=vcount, edges=edgelist, edge_attrs={"weight": weights})
    return gr

def leiden(gr, resolution=None):
    weights = gr.es["weight"]
    if (resolution != None):
        partition = la.find_partition(gr, la.RBConfigurationVertexPartition,
            n_iterations=10, seed=12343, resolution_parameter=resolution,
            weights=weights)
    else:
        partition = la.find_partition(gr, la.ModularityVertexPartition,
            n_iterations=10, seed=12343, weights=weights)
    return partition
