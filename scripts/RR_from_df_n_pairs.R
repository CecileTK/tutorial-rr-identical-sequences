#!/usr/bin/env Rscript
suppressPackageStartupMessages(library("argparse"))
suppressPackageStartupMessages(library("tidyverse"))

source('scripts/utils_RR.R')

# Create parser object
parser <- ArgumentParser(description = "Compute the relative risk of observing identical sequences between two groups")

parser$add_argument("--input_df_pairs_count", required = T,
                    help = "Input dataframe where the number of pairs of identical sequences between metadata_field is saved",
                    type = "character", metavar="string") 

parser$add_argument("--metadata_field", required = T,
                    help = "Metadata field present in input_df_id_seq used to count pairs of identical sequences",
                    type = "character", metavar="string") 

parser$add_argument("--output_df_RR", required = T,
                    help = "Output dataframe where the RR are saved",
                    type = "character", metavar="string")

# Get command line options
args <- parser$parse_args()

# Read input dataframe with pairs count
df_pairs_count <- read_tsv(args$input_df_pairs_count, show_col_types = FALSE)

# Compute RR
df_RR <- get_df_RR(df_pairs_count, args$metadata_field)
df_RR <- expand_df_RR_with_0(df_RR, args$metadata_field)

# Save RR
write_tsv(df_RR, args$output_df_RR)