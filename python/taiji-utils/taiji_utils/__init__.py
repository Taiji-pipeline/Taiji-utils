import argparse

################################################################################
## ARGUMENT PARSER
################################################################################

parser = argparse.ArgumentParser(description="Python scripts for the Taiji pipeline")
subparsers = parser.add_subparsers(title="sub commands")

def reduce(args):
    from .Spectral import spectral
    spectral(args)
# create the parser for the "reduce" command
parser_reduce = subparsers.add_parser('reduce', help='dimension reduction')
parser_reduce.add_argument('input', type=str, help='gzipped input file')
parser_reduce.add_argument('--input-format', default="sparse", type=str)
parser_reduce.add_argument('output', type=str, help='output matrix in .npy format')
parser_reduce.add_argument('--sample-size', default=35000, type=int, help='sampling size')
parser_reduce.add_argument('--dim', default=30, type=int, help='number of dimension')
parser_reduce.add_argument('--seed', default=3484, type=int, help='random seed')
parser_reduce.add_argument('--distance', default="jaccard", type=str, help='distance: jaccard or cosine')
parser_reduce.set_defaults(func=reduce)

def fit(args):
    from .Spectral_v2 import spectral_fit
    spectral_fit(args)
# create the parser for the "fit" command
parser_fit = subparsers.add_parser('fit', help='fit')
parser_fit.add_argument('input', type=str, help='gzipped input file')
parser_fit.add_argument('--input-format', default="sparse", type=str)
parser_fit.add_argument('--sampling-rate', default=1, type=float)
parser_fit.add_argument('output', type=str, help='model')
parser_fit.add_argument('--dim', default=30, type=int, help='number of dimension')
parser_fit.add_argument('--distance', default="jaccard", type=str, help='distance: jaccard or cosine')
parser_fit.set_defaults(func=fit)

def predict(args):
    from .Spectral_v2 import spectral_predict
    spectral_predict(args)
# create the parser for the "predict" command
parser_predict = subparsers.add_parser('predict', help='predict')
parser_predict.add_argument('output', type=str, help='output matrix in .npy format')
parser_predict.add_argument('--input', default=None, type=str, help='gzipped input file')
parser_predict.add_argument('--input-format', default="sparse", type=str)
parser_predict.add_argument('--model', type=str)
parser_predict.set_defaults(func=predict)

def knn(args):
    from .Clustering import mkKNNGraph
    mkKNNGraph(args)
# create the parser for the "knn" command
parser_knn = subparsers.add_parser('knn', help='Make KNN graph')
parser_knn.add_argument('input', type=str, help='input matrix in .npy format')
parser_knn.add_argument('output', type=str, help='adjacency matrix')
parser_knn.add_argument('-k', default=50, type=int, help='neighbors')
parser_knn.add_argument('--embed', help='embedding file')
parser_knn.add_argument('--thread', default=1, type=int, help='number of jobs')
parser_knn.set_defaults(func=knn)

def clust(args):
    from .Clustering import clustering
    clustering(args)
# create the parser for the "clust" command
parser_clust = subparsers.add_parser('clust', help='perform clustering')
parser_clust.add_argument('input', type=str, help='adjacency matrix')
parser_clust.add_argument('output', type=str, help='output file')
parser_clust.add_argument('--res', type=float, default=1, help='resolution')
parser_clust.add_argument('--min-cells', type=int, default=10, help='minimum number of cell in a cluster')
parser_clust.add_argument('--stability', action="store_true", help='compute stability')
parser_clust.add_argument('--seed', type=int, default=12343, help='seed')
parser_clust.add_argument('--optimizer', type=str, default="RB", help='algorithm: RB, CPM')
parser_clust.set_defaults(func=clust)

def doublet(args):
    from .Doublet import detectDoublet
    detectDoublet(args)
# create the parser for the "doublet" command
parser_doublet = subparsers.add_parser('doublet', help='doublet detection')
parser_doublet.add_argument('input', type=str, help='input matrix')
parser_doublet.add_argument('output', type=str, help='output')
parser_doublet.set_defaults(func=doublet)

def correct(args):
    from .BatchCorrect import MNCCorrectMain
    MNCCorrectMain(args)
# create the parser for the "correct" command
parser_correct = subparsers.add_parser('correct', help='batch correction')
parser_correct.add_argument('input', type=str, help='input matrix')
parser_correct.add_argument('output', type=str, help='output')
parser_correct.add_argument('--label', type=str, help='labels')
parser_correct.add_argument('-k', type=int, default=40, help='number of centroids')
parser_correct.add_argument('-n', type=int, default=5, help='number of nearest neighbors')
parser_correct.add_argument('--iter', type=int, default=2, help='number of iterations')
parser_correct.set_defaults(func=correct)

def viz(args):
    from .Viz import Viz
    Viz(args)
# create the parser for the "viz" command
parser_viz = subparsers.add_parser('viz', help='UMAP embedding')
parser_viz.add_argument('input', type=str, help='input matrix')
parser_viz.add_argument('output', type=str, help='output')
parser_viz.add_argument('-k', type=int, default=50, help='number of neighbors')
parser_viz.add_argument('-t', type=str, default=None, help='target')
parser_viz.set_defaults(func=viz)

# 
def diff(args):
    from .Diff import diffAnalysis
    diffAnalysis(args)
parser_diff = subparsers.add_parser('diff', help='atac differential analysis')
parser_diff.add_argument('input1', type=str, help='input matrix 1')
parser_diff.add_argument('input2', type=str, help='input matrix 2')
parser_diff.add_argument('--index', type=str, help='index')
parser_diff.add_argument('--output', type=str, help='output')
parser_diff.add_argument('--fold', type=float, help='fold change cutoff')
parser_diff.add_argument('--thread', type=int, default=10, help='number of threads')
parser_diff.set_defaults(func=diff)

# create the parser for the "normalize" command
def norm(args):
    from .Normalization import normalize
    normalize(args)
parser_normalize = subparsers.add_parser('normalize', help='')
parser_normalize.add_argument('input', type=str, help='')
parser_normalize.add_argument('genemean', type=str, help='')
parser_normalize.add_argument('cellreads', type=str, help='')
parser_normalize.add_argument('data', type=str, help='')
parser_normalize.add_argument('output', type=str, help='')
parser_normalize.add_argument('--plot-dir', type=str, help='')
parser_normalize.set_defaults(func=norm)

# create the parser for the "normalize" command
def barcode(args):
    from .Knee import selectBarcode
    selectBarcode(args)
parser_barcode = subparsers.add_parser('barcode', help='')
parser_barcode.add_argument('input', type=str, help='')
parser_barcode.set_defaults(func=barcode)


def main():
    args = parser.parse_args()
    args.func(args)
