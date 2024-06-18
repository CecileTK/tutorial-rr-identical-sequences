# Tutorial - Computing the relative risk of observing identical pathogen sequences 
## Overview

This repository contains a tutorial describing the different steps to compute the relative risk of observing identical sequences between two groups.
Compared with the [repository](https://github.com/blab/phylo-kernel-public/) associated with the [preprint](https://www.medrxiv.org/content/10.1101/2024.05.24.24307811v1) where we introduce the method, this is modified to be able to accomodate very large sequencing datasets.
To do so, the strategy has been to use [pairsnp](https://github.com/gtonkinhill/pairsnp) to compute the pairwise distance matrix and using [tsv-utils](https://github.com/eBay/tsv-utils) to directly analyse tsv files without requiring to open them within R (where file loading can become unnecessarily long for larger files).

## Setup & installation
We use conda to define an environment containing all the required depencencies.
We can then use conda to install the required dependencies.
The environment can be created and activated using the following commands:

```bash
# Install
conda env create -f envs/idseq.yaml
# Activate
source activate idseq
```

If needed, the environment can be deactivated / deleted using the following command:
```bash
# Deactivate 
conda deactivate
# Remove
conda env remove --name idseq
```

## Performing analyses
### 1. Computing pairwise distance matrices within a fasta file
[**pairsnp**](https://github.com/gtonkinhill/pairsnp) can be used to compute a pairwise distance matrix from aligned sequences
```bash
# Creating header in the output file
echo "strain_1\tstrain_2\tn_mutations" > "results/df_genetic_distance.tsv"
# Generating pairwise distance matrix
pairsnp -s "data/synthetic-fasta.fasta" >> "results/df_genetic_distance.tsv"
```

### 2. Downsampling the distance matrix only to pairs of identical sequences
[**tsv-filter**](https://github.com/eBay/tsv-utils/tree/master/tsv-filter) can be used to only keep pairs of sequences that are identical.
```bash
tsv-filter -H --eq n_mutations:0 "results/df_genetic_distance.tsv" > "results/df_pairs_id_seq.tsv"
```

Another option would have been to directly add the flag ```-t 0``` in the ```pairsnp``` command to only save pairs of sequences that are identical.

### 3. Matching metadata to pairs of identical sequences
```append_metadata_field.R``` uses ```tsv-join``` to add metadata fields to a dataframe with pairs of identical sequences. 
The metadata file needs to contain a column called **strain**. 
Metadata fields to be appended to the dataframe with the list of pairs of identical sequences can be indicated by the ```--metadata_field``` flag.
A metadata column named ```field``` will be indicated in the dataframe of pairs of identical sequences using the columns ```field_1``` and ```field_2``` to indicate the corresponding values for ```strain_1``` and ```strain_2```.

At the moment, this script would not work if some metadata field names are *nested* within each other (e.g. **group** and **subgroup**, **region** and **big_region** ...)

The **--input_metadata** flag should refer to a file where strain names indicated by the ```strain``` column name. 
A typical valid metadata file should have the following structure:
| strain   | field_a          | field_b          | field_c          | ... |
| ------   | -----------------| ---------------- | ---------------- | --- |
| strain_A | field_Aa         | field_Ab         | field_Ac         | ... |
| strain_B | field_Ba         | field_Bb         | field_Bc         | ... |
| strain_C | field_Ca         | field_Cb         | field_Cc         | ... |
| strain_D | field_Da         | field_Db         | field_Dc         | ... |
... |

```bash
scripts/append_metadata_field.R \
    --input_metadata data/synthetic-metadata.tsv \
    --input_df_id_seq results/df_pairs_id_seq.tsv \
    --output results/df_pairs_id_seq_with_metadata.tsv \
    --metadata_field group region       
```

### 4. Generating a dataframe with counts of pairs of identical sequences by group
```count_pairs.R``` uses [```tsv-summarize```](https://github.com/eBay/tsv-utils/tree/master/tsv-summarize) to summarise the output of ```append_metadata_field.R``` by counting pairs of identical sequences betwen subgroups.
For this to work well, the metadata column **--metadata_field** shoudl have been added in the ```append_metadata_field.R``` step above.

```bash
scripts/count_pairs.R \
    --input_df_id_seq results/df_pairs_id_seq_with_metadata.tsv \
    --metadata_field region \
    --output_df_pairs_count results/df_n_pairs_by_region.tsv
```

### 5. Computing the relative risk of observing identical sequences between groups from a dataframe containing the number of pairs of identical sequences between groups
```RR_from_df_n_pairs.R``` then enables to compute the relative risk of observing pairs of identical sequences between groups (indicated by the **--metadata_field** flag) by inputting a dataframe obtained from ```count_pairs.R```.

```bash
scripts/RR_from_df_n_pairs.R \
    --input_df_pairs_count results/df_n_pairs_by_region.tsv \
    --metadata_field region \
    --output_df_RR results/df_RR_by_region.tsv
```

### 6. Compute uncertainty around RR using a resampling approach
```uncertainty_RR.R```  enables to compute confidence intervals around relative risk estimates using a subsampling approach with **--n_subsamples** sampling iteration and a **--prop_subsample** sampling probability.
This requires the following other arguments:
- **--input_df_id_seq**: a tsv file with pairs of identical sequences with columns ```strain_1``` and ```strain_2```. Pairs should only be present once in this dataset (typical output from ```pairsnp``` presented in step 2).
- **--input_metadata**: a tsv file with columns ```strain``` and the metadata_field indicated by  **--metadata_field**.
- **--temp_dir**: a temp folder where some of the files produced during intermediate steps will be stores.
- **--output_RR_uncertainty**: a tsv file where the output of the subsampling can be stored.

The output has the following format:
| metadata_field_1 | metadata_field_2 | n_pairs | RR  | replicate_id |
|------------------|------------------|---------|-----|--------------|
| group_A          | group_A          | 100     | 1.1 | 1            |
| group_A          | group_A          | 192     | 1.5 | 2            |
| group_A          | group_A          | 150     | 1.4 | 3            |
|...|
| group_A          | group_B          | 65      | 0.5 | 1            |
| group_A          | group_B          | 72      | 0.8 | 2            |
| group_A          | group_C          | 92      | 0.9 | 3            |
|...|


```bash
scripts/uncertainty_RR.R \
    --input_df_id_seq results/df_pairs_id_seq.tsv \
    --input_metadata data/synthetic-metadata.tsv \
    --n_subsamples 1000 \
    --prop_subsample 0.8 \
    --temp_dir temp \
    --metadata_field region \
    --output_RR_uncertainty results/RR_uncertainty_region.tsv
```

Median values along 95% confidence intervals can then be obtained by computing the median and 2.5% and 97.5% quantiles across ```replicate_id``` using a standard data analysis software (R, Python...) or the following ```tsv-summarize``` command: 

```bash
tsv-summarize -H \
    --group-by region_1,region_2 \
    --quantile RR:0.025,0.5,0.975 \
    results/RR_uncertainty_region.tsv
```