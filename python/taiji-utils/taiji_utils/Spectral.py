import scipy as sp
import numpy as np
import math
from sklearn.linear_model import LinearRegression
from sklearn.metrics.pairwise import cosine_similarity, rbf_kernel

from .Utils import readMatrix, regress

def spectral(args):
    nSample = max(1000, args.sample_size)
    nChunk = nSample

    print("Read Data")
    if (args.input_format == "dense"):
        mat = np.loadtxt(args.input)
    elif (args.distance == "jaccard"):
        mat = readMatrix(args.input, binary=True)
    else:
        mat = readMatrix(args.input, binary=False)

    #n, _ = mat.get_shape()
    n, _ = mat.shape
    if nSample < n:
        np.random.seed(args.seed)
        idx = np.arange(n)
        np.random.shuffle(idx)
        sample = mat[idx[:nSample], :]
        dm = Spectral(sample, n_dim=args.dim, distance=args.distance, sampling_rate=nSample/n)
        i = nSample
        res = [dm.coordinates]
        while i < n:
            data = mat[idx[i:i+nChunk], :]
            res.append(dm.fit(data))
            i = i + nChunk
        res = np.concatenate(res, axis=0)[:, 1:]
        res = res[np.argsort(idx), :]
    else:
        res = Spectral(mat, n_dim=args.dim, distance=args.distance).coordinates[:, 1:]

    np.savetxt(args.output, res, delimiter='\t')
    
class Spectral:
    def __init__(self, mat, n_dim=30, sampling_rate=1, distance="jaccard"):
        self.sample = mat
        self.sampling_rate = sampling_rate
        #self.dim = mat.get_shape()[1]
        self.dim = mat.shape[1]
        self.coverage = mat.sum(axis=1) / self.dim
        self.distance = distance
        if (self.distance == "jaccard"):
            print("Use jaccard distance")
            self.compute_similarity = jaccard_similarity
        elif (self.distance == "cosine"):
            self.compute_similarity = cosine_similarity
        else:
            self.compute_similarity = rbf_kernel

        print("Compute similarity matrix")
        jm = self.compute_similarity(mat)

        if (self.distance == "jaccard"):
            self.normalizer = Normalizer(jm, self.coverage)
            S = self.normalizer.fit(jm, self.coverage, self.coverage)
        else:
            S = jm

        np.fill_diagonal(S, 0)
        print("Normalization")
        self.D = np.diag(1/(self.sampling_rate * S.sum(axis=1)))
        L = np.matmul(self.D, S)

        print("Reduction")
        evals, evecs = sp.sparse.linalg.eigs(L, n_dim+1, which='LR')
        ix = evals.argsort()[::-1]
        self.evals = np.real(evals[ix])
        self.evecs = np.real(evecs[:, ix])
        self.coordinates = self.evecs

    def fit(self, data):
        jm = self.compute_similarity(self.sample, data)

        if (self.distance == "jaccard"):
            S_ = self.normalizer.fit(jm, self.coverage, data.sum(axis=1) / self.dim).T
        else:
            S_ = jm.T

        D_ = np.diag(1/(self.sampling_rate * S_.sum(axis=1)))
        L_ = np.matmul(D_, S_)
        evecs = (L_.dot(self.evecs)).dot(np.diag(1/self.evals))
        return evecs

class Normalizer:
    def __init__(self, jm, c):
        n, _ = jm.shape

        X = 1 / c.dot(np.ones((1,n)))
        X = 1 / (X + X.T - 1)
        X = X[np.triu_indices(n, k = 1)].T
        y = jm[np.triu_indices(n, k = 1)].T

        self.model = LinearRegression().fit(X, y)

    def fit(self, jm, c1, c2):
        X1 = 1 / c1.dot(np.ones((1, c2.shape[1]))) 
        X2 = 1 / c2.dot(np.ones((1, c1.shape[1])))
        X = 1 / (X1 + X2.T - 1)

        y = self.model.predict(X.flatten().T).reshape(jm.shape)
        return np.array(jm / y)

# Similarity metric
""" Compute pair-wise jaccard index
Input:
    mat1: n1 x m
    mat2: n2 x m
Output:
    jm: n1 x n2
"""
def jaccard_similarity(mat1, mat2=None):
    coverage1 = mat1.sum(axis=1)
    if(mat2 != None):
        coverage2 = mat2.sum(axis=1)
        jm = mat1.dot(mat2.T).todense()
        n1, n2 = jm.shape
        c1 = coverage1.dot(np.ones((1,n2)))
        c2 = coverage2.dot(np.ones((1,n1)))
        jm = jm / (c1 + c2.T - jm)
    else:
        n, _ = mat1.get_shape()
        jm = mat1.dot(mat1.T).todense()
        c = coverage1.dot(np.ones((1,n)))
        jm = jm / (c + c.T - jm)
    return jm