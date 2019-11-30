import scipy as sp
import numpy as np
import math
import leidenalg as la

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