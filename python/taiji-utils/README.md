Auxiliary Python utilities used in the Taiji pipeline
=====================================================

Dimension reduction
-------------------

```
taiji-utils reduce cell_by_feat_sparse.mat.gz reduced.tsv.gz
```

`cell_by_feat_sparse.mat.gz` is a gzipped file containing non-zero entries in the feature matrix, for example:

```
sparse matrix: 3 x 100
rowname1  0,3 10,1  22,2
rowname2  1,1 2,5 99,2
rowname3  0,1 95,4  96,1
```

Construct KNN graph
-------------------

```
taiji-utils knn reduced.tsv.gz knn.npz -k 50 --embed umap.txt
```

Clustering
----------

```
taiji-utils clust knn.npz cluster.txt --res 0.5 --optimizer CPM
```


