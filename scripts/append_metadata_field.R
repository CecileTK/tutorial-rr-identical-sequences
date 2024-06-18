#!/usr/bin/env Rscript
suppressPackageStartupMessages(library("argparse"))

# Create parser object
parser <- ArgumentParser(description = "Append metadata fields of interest to pairwise distance dataframe")

# Specify desired options
parser$add_argument("--input_metadata", required = T,
                    help = "Input metadata",
                    type = "character", metavar="string")

parser$add_argument("--input_df_id_seq", required = T, 
                    help = "Input file for dataframe with pairs of identical sequences",
                    type = "character", metavar="string")

parser$add_argument("--output", required = T, 
                    help = "Output file for dataframe with pairs of identical sequences along with fields of interest",
                    type = "character", metavar="string")

parser$add_argument("--metadata_field", required = T,
                    help = 'Metadata fields to be appended to the genetic distance dataframe',
                    type = "character", nargs = '+')

# Get command line options
args <- parser$parse_args()

input_file_metadata <- args$input_metadata
input_file_df_identical_sequences <- args$input_df_id_seq
output_file <- args$output
fields_to_append <- args$metadata_field

# Define list of sed command to adjust dataframe
list_sed_to_change_1 <- lapply(fields_to_append, FUN = function(curr_field){
  paste0("sed -e '1s/", curr_field, "/", curr_field, "_1", "/g'")
})
list_sed_to_change_2 <- lapply(fields_to_append, FUN = function(curr_field){
  paste0("sed -e '1s/", curr_field, "/", curr_field, "_2", "/g'")
})
list_sed_to_change_2_1 <- lapply(fields_to_append, FUN = function(curr_field){
  paste0("sed -e '1s/", curr_field, "_2_1/", curr_field, "_1", "/g'")
})

# Append metadata field to the dataframe
system(
  paste0(
    "sed -e '1s/strain_1/strain/g' ", input_file_df_identical_sequences, " | ",
    
    r"(tsv-join -H --filter-file )", input_file_metadata, r"( --key-fields strain --append-fields )", 
    paste(fields_to_append, collapse = ','), ' | ',
    paste(list_sed_to_change_1, collapse = ' | '),  ' | ',
    
    "sed -e '1s/strain/strain_1/g'", ' | ',
    "sed -e '1s/strain_1_2/strain/g'", ' | ',
    r"(tsv-join -H --filter-file )", input_file_metadata, r"( --key-fields strain --append-fields )", 
    paste(fields_to_append, collapse = ','), ' | ',
    paste(list_sed_to_change_2, collapse = ' | '),  ' | ',
    
    "sed -e '1s/strain/strain_2/g'", ' | ', 
    "sed -e '1s/strain_2_1/strain_1/g'", ' | ', 
    paste(list_sed_to_change_2_1, collapse = ' | '),  
    r"( > )", output_file,"")
)

