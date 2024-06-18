#!/usr/bin/env Rscript
suppressPackageStartupMessages(library("argparse"))
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("glue"))

source('scripts/utils_RR.R')

# Create parser object
parser <- ArgumentParser(description = "Performs a resampling strategy to compute uncertainty intervals around RR of identical sequences")

# Specify desired options
# Specify desired options
parser$add_argument("--input_df_id_seq", required = T,
                    help = "Input dataframe with list of pairs of identical sequences and associated group information",
                    type = "character", metavar="string")

parser$add_argument("--input_metadata", required = T,
                    help = "Input metadata",
                    type = "character", metavar="string")

parser$add_argument("--output_RR_uncertainty", required = T,
                    help = "Output dataframe for subsampled RR",
                    type = "character", metavar="string")

parser$add_argument("--n_subsamples", required = T,
                    help = "Number of draws used to compute CI",
                    type = "integer", metavar="N") 

parser$add_argument("--prop_subsample", required = T,
                    help = "Subsampling proportion",
                    type = "numeric") 

parser$add_argument("--temp_dir", required = T,
                    help = "Temp directory where subsampled RR replicated are saved",
                    type = "character", metavar="string") 

parser$add_argument("--metadata_field", required = T,
                    help = "Metadata field present in input_df_id_seq used to count pairs of identical sequences",
                    type = "character", metavar="string") 

# Get command line options
args <- parser$parse_args()

n_subsamples <- args$n_subsamples
input_metadata <- args$input_metadata
metadata_field <- args$metadata_field
prop_subsample <- args$prop_subsample
temp_dir <- args$temp_dir
input_file_df_identical_sequences <- args$input_df_id_seq
output_file_RR_uncertainty <- args$output_RR_uncertainty

## Create temp_dir if necessary
if(! dir.exists(temp_dir)){
  dir.create(temp_dir)
}

df_all_RR <- Reduce('bind_rows', lapply(1:n_subsamples, FUN = function(i_replicate){
  ## Subsampled metadata
  output_subsampled_metadata <- paste0(temp_dir, '/subsampled_metadata_', i_replicate, '.tsv')
  system(paste0('tsv-sample -H ', input_metadata, ' --prob ', prop_subsample, ' > ', output_subsampled_metadata))
  
  ## Compute number of pairs of identical sequences between subsampled metadata
  output_df_pairs_count <- paste0(temp_dir, '/df_n_pairs_by_group_', i_replicate, '.tsv')
  
  # Define list of sed command to adjust dataframe
  fields_to_append <- c(metadata_field)
  list_sed_to_change_1 <- lapply(fields_to_append, FUN = function(curr_field){
    paste0("sed -e '1s/", curr_field, "/", curr_field, "_1", "/g'")
  })
  list_sed_to_change_2 <- lapply(fields_to_append, FUN = function(curr_field){
    paste0("sed -e '1s/", curr_field, "/", curr_field, "_2", "/g'")
  })
  list_sed_to_change_2_1 <- lapply(fields_to_append, FUN = function(curr_field){
    paste0("sed -e '1s/", curr_field, "_2_1/", curr_field, "_1", "/g'")
  })
  
  system(
    paste0(
      "sed -e '1s/strain_1/strain/g' ", input_file_df_identical_sequences, " | ",
      
      r"(tsv-join -H --filter-file )", output_subsampled_metadata, r"( --key-fields strain --append-fields )", 
      paste(fields_to_append, collapse = ','), ' | ',
      paste(list_sed_to_change_1, collapse = ' | '),  ' | ',
      
      "sed -e '1s/strain/strain_1/g'", ' | ',
      "sed -e '1s/strain_1_2/strain/g'", ' | ',
      r"(tsv-join -H --filter-file )", output_subsampled_metadata, r"( --key-fields strain --append-fields )", 
      paste(fields_to_append, collapse = ','), ' | ',
      paste(list_sed_to_change_2, collapse = ' | '),  ' | ',
      
      "sed -e '1s/strain/strain_2/g'", ' | ', 
      "sed -e '1s/strain_2_1/strain_1/g'", ' | ', 
      paste(list_sed_to_change_2_1, collapse = ' | '),  ' | ', 
      
      "tsv-summarize -H --group-by ", metadata_field, "_1", ",", metadata_field, "_2", " --count ", 
      
      r"( > )", output_df_pairs_count)
  )
  
  ## As pairs are only present once in the output of pairsnp, we modify the dataframe to aggregate the number of pairs observed in group 1 & 2 and 2 & 1
  df_pairs_count <- read_tsv(output_df_pairs_count, show_col_types = FALSE)
  
  col_1 <- paste0(metadata_field, '_1')
  col_2 <- paste0(metadata_field, '_2')
  
  tmp_df_pairs_count <- df_pairs_count %>% 
    filter(get(paste0(metadata_field, '_1')) != get(paste0(metadata_field, '_2'))) %>% 
    rename(tmp = paste0(metadata_field, '_1'), '{col_1}' := col_2) %>% 
    rename('{col_2}' := tmp)
  
  df_pairs_count <- bind_rows(df_pairs_count, tmp_df_pairs_count) %>% 
    group_by(across(all_of(c(col_1, col_2)))) %>% 
    summarise(count = sum(count), .groups = 'drop')
  
  file.remove(output_df_pairs_count)
  file.remove(output_subsampled_metadata)
  
  df_RR <- get_df_RR(df_pairs_count, metadata_field)
  
  expand_df_RR_with_0(df_RR, metadata_field) %>% 
    mutate(replicate_id = i_replicate)
}))

write_tsv(df_all_RR, output_file_RR_uncertainty)