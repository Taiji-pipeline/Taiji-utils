Auxiliary Python utilities used in the Taiji pipeline
=====================================================

Dimension reduction
-------------------

```
taiji-utils reduce cell_by_feat_sparse.mat.gz reduced.tsv.gz
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


