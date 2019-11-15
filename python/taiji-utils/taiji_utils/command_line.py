import argparse
import taiji_utils as sc

from .Doublet import detectDoublet

################################################################################
## ARGUMENT PARSER
################################################################################

parser = argparse.ArgumentParser(description="Fast online algorithms")
subparsers = parser.add_subparsers(title="sub commands")

# create the parser for the "run" command
parser_run = subparsers.add_parser('reduce', help='dimension reduction')
parser_run.add_argument('input', type=str, help='gzipped input file')
parser_run.add_argument('output', type=str, help='output matrix in .npy format')
parser_run.add_argument('--method', help='algorithm: svd, spectral')
parser_run.add_argument('--sample-size', default=35000, type=int, help='sampling size')
parser_run.add_argument('--seed', default=3484, type=int, help='random seed')
parser_run.add_argument('--distance', default="jaccard", type=str, help='distance: jaccard or cosine')
parser_run.set_defaults(func=sc.reduceDimension)

# create the parser for the "clust" command
parser_clust = subparsers.add_parser('clust', help='perform clustering')
parser_clust.add_argument('input', type=str, help='input matrix in .npy format')
parser_clust.add_argument('output', type=str, help='output file')
parser_clust.add_argument('--coverage', help='coverage file')
parser_clust.add_argument('--embed', help='embedding file')
parser_clust.add_argument('--discard', action='store_true', help='remove first dimension')
parser_clust.add_argument('--scale', action='store_true', help='scale to unit ball')
parser_clust.add_argument('--dim', type=int, help='dimension')
parser_clust.add_argument('-k', default=25, type=int, help='neighbors')
parser_clust.add_argument('--res', type=float, help='resolution')
parser_clust.set_defaults(func=sc.clustering)

# create the parser for the "doublet" command
parser_doublet = subparsers.add_parser('doublet', help='doublet detection')
parser_doublet.add_argument('input', type=str, help='input matrix')
parser_doublet.add_argument('output', type=str, help='output')
parser_doublet.set_defaults(func=detectDoublet)

def main():
    args = parser.parse_args()
    args.func(args)