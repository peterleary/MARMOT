import pacmap


def pacmap_fit(X,
               init=None,
               save_pairs=True,
               n_components=2,
               n_neighbors=10,
               MN_ratio=0.5,
               FP_ratio=2.0,
               pair_neighbors=None,
               pair_MN=None,
               pair_FP=None,
               distance="euclidean",
               lr=1.0,
               num_iters=450,
               verbose=False,
               apply_pca=True,
               intermediate=False,
               intermediate_snapshots=[0, 10, 30, 60, 100, 120, 140, 170, 200,
                                        250, 300, 350, 450],
               random_state=None,
               save_tree=False):
    """Pairwise Controlled Manifold Approximation.

    Maps a high-dimensional dataset to a low-dimensional embedding.
    See https://www.jmlr.org/papers/volume22/20-1061/20-1061.pdf
    """
    embedding = pacmap.PaCMAP(
        n_components, n_neighbors, MN_ratio, FP_ratio,
        pair_neighbors, pair_MN, pair_FP,
        distance, lr, num_iters, verbose, apply_pca,
        intermediate, intermediate_snapshots, random_state, save_tree,
    )
    return embedding.fit_transform(X, init, save_pairs)
