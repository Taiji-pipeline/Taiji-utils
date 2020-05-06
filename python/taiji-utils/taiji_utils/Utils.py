import gzip
import scipy as sp
import numpy as np
from sklearn.linear_model import LinearRegression

class InputData:
    def __init__(self, filename):
        self.filename = filename
        with gzip.open(self.filename, mode='rt') as f:
            header = f.readline().strip()
            (m, n) = header.split(": ")[1].split(" x ")
            self.num_doc = int(m)
            self.num_terms = int(n)

    def __iter__(self):
        def convert(x):
            return (int(x[0]), float(x[1]))
        with gzip.open(self.filename, mode='rt') as f:
            f.readline()
            for line in f:
                yield [convert(item.split(",")) for item in line.strip().split("\t")[1:]]

# regress out a variable
def regress(X, y):
    model = LinearRegression().fit(X, y)
    return model.predict(X)

def readMatrix(fl, binary=False):
    data = InputData(fl)
    indptr = [0]
    indices = []
    mat = []
    for row in iter(data):
        for (i,x) in row:
            indices.append(i)
            if (binary):
                mat.append(1)
            else:
                mat.append(x)
        indptr.append(len(indices))
    if (binary):
        mat = sp.sparse.csr_matrix((mat, indices, indptr), shape=(data.num_doc, data.num_terms), dtype=int)
    else:
        mat = sp.sparse.csr_matrix((mat, indices, indptr), shape=(data.num_doc, data.num_terms), dtype=float)
    return mat
