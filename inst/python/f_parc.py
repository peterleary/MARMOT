import parc

def parc_clust(X,
               true_label=None, 
               dist_std_local=3, 
               jac_std_global='median', 
               keep_all_local_dist='auto',
               too_big_factor=0.4, 
               small_pop=10, 
               ac_weighted_edges=True, 
               knn=30, 
               n_iter_leiden=5, 
               random_seed=42,
               num_threads=-1, 
               distance='l2', 
               time_smallpop=15, 
               partition_type = "ModularityVP", 
               resolution_parameter = 1.0,
               knn_struct=None, 
               neighbor_graph=None, 
               hnsw_param_ef_construction = 150):

  parc1 = parc.PARC(X,
               true_label, 
               dist_std_local, 
               jac_std_global, 
               keep_all_local_dist,
               too_big_factor, 
               small_pop, 
               ac_weighted_edges, 
               knn, 
               n_iter_leiden, 
               random_seed,
               num_threads, 
               distance, 
               time_smallpop, 
               partition_type, 
               resolution_parameter,
               knn_struct, 
               neighbor_graph, 
               hnsw_param_ef_construction)
               
  parc1.run_PARC()
  return parc1.labels 
