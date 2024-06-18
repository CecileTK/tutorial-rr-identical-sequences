#!/usr/bin/env Rscript
suppressPackageStartupMessages(library("argparse"))
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("glue"))

# Create parser object
parser <- ArgumentParser(description = "Count the number of pairs of identical sequences between groups from a dataframe with pairs and group metadata")

# Specify desired options
parser$add_argument("--input_df_id_seq", required = T,
                    help = "Input dataframe with list of pairs of identical sequences and associated group information",
                    type = "character", metavar="string")

parser$add_argument("--metadata_field", required = T,
                    help = "Metadata field present in input_df_id_seq used to count pairs of identical sequences",
                    type = "character", metavar="string") 

parser$add_argument("--output_df_pairs_count", required = T,
                    help = "Output dataframe where the number of pairs of identical sequences between metadata_field is saved",
                    type = "character", metavar="string") 
  
# Get command line options
args <- parser$parse_args()

input_df_id_seq <- args$input_df_id_seq
metadata_field <- args$metadata_field
output_df_pairs_count <- args$output_df_pairs_count

# Count number of pairs of identical sequences by metadata_field
system(paste0("tsv-summarize -H --group-by ", metadata_field, "_1", ",", metadata_field, "_2", 
              " --count ", input_df_id_seq, r"( > )", output_df_pairs_count)
       )

# As pairs are only present once in the output of pairsnp, we modify the dataframe to aggregate the number of pairs observed in group 1 & 2 and 2 & 1
df_pairs_count <- read_tsv(output_df_pairs_count, show_col_types = FALSE)

col_1 <- paste0(metadata_field, '_1')
col_2 <- paste0(metadata_field, '_2')

tmp_df_pairs_count <- df_pairs_count %>% 
  filter(get(paste0(metadata_field, '_1')) != get(paste0(metadata_field, '_2'))) %>% 
  rename(tmp = paste0(metadata_field, '_1'), '{col_1}' := col_2) %>% 
  rename('{col_2}' := tmp)

bind_rows(df_pairs_count, tmp_df_pairs_count) %>% 
  group_by(across(all_of(c(col_1, col_2)))) %>% 
  summarise(count = sum(count), .groups = 'drop') %>% 
  write_tsv(output_df_pairs_count)

## Compute RR between groups
