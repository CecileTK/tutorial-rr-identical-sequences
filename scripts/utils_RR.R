get_df_RR <- function(df_n_pairs_by_group, metadata_field){
  
  df_n_pairs_by_group %>% 
    rename(n_pairs = count) %>% 
    group_by(across(all_of(paste0(metadata_field, '_1')))) %>% 
    mutate(n_pairs_1_x = sum(n_pairs)) %>% 
    group_by(across(all_of(paste0(metadata_field, '_2')))) %>% 
    mutate(n_pairs_x_2 = sum(n_pairs)) %>% 
    ungroup() %>% 
    mutate(n_pairs_x_x = sum(n_pairs),
           RR = n_pairs / n_pairs_1_x / n_pairs_x_2 * n_pairs_x_x) %>% 
    return()
}

expand_df_RR_with_0 <- function(df_RR, metadata_field){
  vec_groups <- unique(unlist(df_RR[, paste0(metadata_field, '_1')]))
  
  col_1 <- paste0(metadata_field, '_1')
  col_2 <- paste0(metadata_field, '_2')
  
  df_RR_expanded <- expand.grid(var_1 = vec_groups, var_2 = vec_groups) %>% 
    rename('{col_1}' := var_1, '{col_2}' := var_2) %>% 
    left_join(df_RR %>% select(- n_pairs_1_x, -n_pairs_x_x, - n_pairs_x_2)) %>% 
    mutate(RR = replace_na(RR, 0.), n_pairs = replace_na(n_pairs, 0.))
  
  return(df_RR_expanded)
}

