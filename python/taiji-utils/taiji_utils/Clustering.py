import scipy as sp
import numpy as np
import math
import leidenalg as la
from sklearn.neighbors import kneighbors_graph
import igraph as ig
import umap
from sklearn.metrics import adjusted_rand_score
import multiprocessing as mp

"""
args.input: a list of files.
args.k: number of neighbours.
args.output: output file.
args.embed: embeding file.
"""
def mkKNNGraph(args):
    fls = args.input.split(',')
    adj = None

    #adj = kneighbors_graph(readCoordinates(fls[0]), args.k, mode='distance', n_jobs=12)
    for fl in fls:
        mat = readCoordinates(fl)
        k = min(args.k, mat.shape[0]-1)
        if (adj == None):
            adj = kneighbors_graph(mat, k, mode='distance', n_jobs=args.thread)
        else:
            adj += kneighbors_graph(mat, k, mode='distance', n_jobs=args.thread)
    adj = adj / len(fls)

    np.reciprocal(adj.data, out=adj.data)
    sp.sparse.save_npz(args.output, adj)

    if(args.embed):
        print("Create Embedding:")
        data = readCoordinates(fls[0])
        getEmbedding(data, args.embed)

def clustering(args):
    partition = runClustering(args.input, args.res, args.optimizer, args.seed)
    nCl = 0
    vcount = 0
    for c in partition:
        vcount = vcount + len(c)
    with open(args.output, 'w') as f:
        cutoff = max(args.min_cells, min(50, 0.001*vcount))
        for c in partition:
            if len(c) >= cutoff:
                nCl = nCl + 1
                print(','.join(map(str, c)), file=f) 
    print(nCl)
    if (args.stability):
        pool = mp.Pool(5)
        result = []
        for _ in range(5):
            pool.apply_async(runClustering,
                args=(args.input, True),
                callback = lambda x: result.append(x)
            ) 
        pool.close()
        pool.join()
        print(ari(result))

def ari(cls):
    n = len(cls)
    scores = []
    for i in range(n):
        for j in range(i+1, n):
            scores.append(adjusted_rand_score(cls[i], cls[j]))
    return sum(scores) / len(scores)

def runClustering(input, res, opti, seed, perturb=False):
    adj = sp.sparse.load_npz(input)
    vcount = max(adj.shape)
    sources, targets = adj.nonzero()
    if (perturb):
        toDelete = set()
        for i in range(vcount):
            _, col = adj.getrow(i).nonzero()
            idx = np.random.choice(col)
            toDelete.add((i, idx))
            toDelete.add((idx, i))
        print("Perturbing ")
        edges = filter(lambda x: x not in toDelete, zip(sources.tolist(), targets.tolist()))
        sources, targets = zip(*edges)
    edgelist = list(zip(list(sources), list(targets)))
    weights = np.ravel(adj[(sources, targets)])
    gr = ig.Graph(n=vcount, edges=edgelist, edge_attrs={"weight": weights})

    print("Start Clustering...")
    return leiden(gr, resolution=res, optimizer=opti, seed=seed)

'''
    n = 0
    with open(args.output, 'w') as f:
        cutoff = max(args.min_cells, min(50, 0.001*vcount))
        for c in partition:
            if len(c) >= cutoff:
                n = n + 1
                print(','.join(map(str, c)), file=f) 
    print(n)
'''

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
    embedding = umap.UMAP(random_state=42, n_components=2).fit_transform(mat)
    np.savetxt(output, embedding, delimiter='\t')

def leiden(gr, resolution=1, optimizer="RB", seed=12343):
    weights = gr.es["weight"]
    if (optimizer == "RB"):
        algo = la.RBConfigurationVertexPartition
    elif (optimizer == "CPM"):
        algo = la.CPMVertexPartition
    partition = la.find_partition(gr, algo,
        n_iterations=10, seed=seed, resolution_parameter=resolution,
        weights=weights)
    return partition