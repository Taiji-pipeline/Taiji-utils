import numpy as np
from sklearn.neighbors import KDTree
import itertools
from sklearn.cluster import KMeans

class Projector(object):
    def __init__(self, X, Y):
        self.reference = X
        self.vector = Y - X

    def project(self, X, weight=0.5):
        def project(x):
            P = self.reference
            U = self.vector
            d = np.sqrt(np.sum((P - x)**2, axis=1))
            w = np.exp(-(d/0.005))
            #w = 1/d
            return (x + weight * np.average(U, axis=0, weights=w))
        return np.apply_along_axis(project, 1, X)

def findMNC(X, Y, k, n):
    cX = KMeans(n_clusters=k, random_state=0).fit(X).cluster_centers_
    cY = KMeans(n_clusters=k, random_state=0).fit(Y).cluster_centers_

    treeX = KDTree(cX)
    treeY = KDTree(cY)

    # X by Y matrix
    mX = treeY.query(cX, k=n, return_distance=False)

    # Y by X matrix
    mY_ = treeX.query(cY, k=n, return_distance=False)
    mY = []
    for i in range(mY_.shape[0]):
        mY.append(set(mY_[i,:]))

    iX = []
    iY = []
    for i in range(mX.shape[0]):
        for j in mX[i]:
            if i in mY[j]:
                iX.append(i)
                iY.append(j)
    a = cX[iX,:]
    b = cY[iY,:]
    return (Projector(a, b), Projector(b, a))

def MNCCorrect(dat, k, n):
    X = dat[0]
    for i in range(1, len(dat)):
        Y = dat[i]
        pX, pY = findMNC(X, Y, k, n)
        ratio = X.shape[0] / (X.shape[0] + Y.shape[0])
        X_ = pX.project(X, weight = 1 - ratio)
        Y_ = pY.project(Y, weight = ratio)
        X = np.concatenate((X_, Y_), axis=0)
    return X

def MNCCorrectMain(args):
    mat = np.loadtxt(args.input)
    with open(args.label, 'r') as fl:
        labels = [l.strip() for l in fl]
    
    for _ in range(args.iter):
        batchLabels = list(set(labels))
        batchIndex = []
        batch = []
        for label in batchLabels:
            idx = [i for i, x in enumerate(labels) if x == label]
            batchIndex.append(idx)
            batch.append(mat[idx,:])

        mat_ = MNCCorrect(batch, args.k, args.n)
        new = np.array(mat, copy=True)
        idx = list(itertools.chain.from_iterable(batchIndex))
        for i in range(len(labels)):
            new[idx[i]] = mat_[i,:]
        mat = new
    np.savetxt(args.output, mat, delimiter='\t')