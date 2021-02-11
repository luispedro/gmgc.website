# Downloads

The main catalog was done at species-level (95% nucleotide identity) and
includes 302,655,267 unigenes. Additionally, we make available a 100%
non-redundant catalog (including 966,108,540 genes) and a 90% amino-acid level
catalog (210,478,083 genes). Note that the 90% catalog is a subcatalog of the
main one and identifiers are kept consistent.

Files can also be downloaded from our [GMGC10.data git-annex
repository](https://git.embl.de/coelho/GMGC10.data). This recommended only for
more advanced users, but may be a better interface for settings such as an HPC
cluster (where direct web access is not available) and provides automated data
integrity checks.

## Habitat-specific catalogs

The 14 habitats considered in this version of the catalog give rise to 14
different sub-catalogs. Additionally, for convenience, we provide versions
which exclude rare genes and may be more appropriate for uses such as short
read mapping.

## Clusters

### ORF clustering table

[GMGC.relationships.txt.gz](http://gmgc.embl.de/downloads/v1.0/GMGC.relationships.txt.gz)

This table shows the structure of the clusters at at 95%. Note that this table
requires _ca._ 300 GB to store and contains 8,533,537,889 rows.

This table contains triplets of the form `[ORF1] [REL] [ORF2]`. `[ORF2]` is
always an ORF in the GMGCv1, while `[ORF1]` is one of the original ORFs. `[REL]` is one of

- `=`: the ORFs are identical
- `C`: `ORF1` is **contained** in `ORF2` as a substring
- `R`: `ORF1` can be represented by `ORF2` (_i.e._, `ORF1` is 95% identical to
  a substring of `ORF2`).

### Renaming tables

To understand the ORF structure, you also need to download the renaming tables.

There are two types of ORF used for building the GMGC:
- Metagenomic ORFs. These named according to the format `{SAMPLE}_{#}`.
- [Progenomes2](http://progenomes.embl.de/) ORFs. These were renamed as
  `Fr12_{#}` (Freeze 12 being an internal name for ProGenomes2 as construction
  of `GMGCv1` predated the publication of ProGenomes2). The reason for renaming
  is that it was important to have short names to keep the memory usage down.

At the end of the process, the representative ORF for each cluster is assigned
a `GMGC10` name (`GMGC10` being the abbreviated form of `GMGC 1.0`).

- [ProGenomes2 renaming
  table](http://gmgc.embl.de/downloads/v1.0/metadata/ProGenomes2.rename.tsv.gz)
- Final GMGC10 renaming is part of the [unigene metadata
  table](http://gmgc.embl.de/downloads/v1.0/metadata/GMGC10.meta.tsv.gz)

### Protein clustering tables

_Protein clusters_ were built at 90% amino acid identity, which represent
sequences with the same function. Broader families were built at higher
thresholds too, namely 50%, 30%, and 20% (all subject to e-value thresholding,
see the manuscript for details).

- [Clustering table](http://gmgc.embl.de/downloads/v1.0/GMGC10.protein-clusters-families.tsv.gz)
- [Clustering table (complete ORFs only)](http://gmgc.embl.de/downloads/v1.0/GMGC10.complete-orfs.protein-families.tsv.gz)

## Unigene Metadata & Annotations

The [unigene metadata
table](http://gmgc.embl.de/downloads/v1.0/metadata/GMGC10.meta.tsv.gz) contains
basic information for each unigene: its original name (which indicates which
sample the representative was assembled from, the habitat of this sample, the
size (in nucleotides), and whether the ORF is a complete ORF.

ORFs are annotated to the environment from which they were assembled. Unigenes
aggregate multiple ORFs and can, thus, represent multiple environments. The
filegq
[GMGC10.gene-environment.tsv](http://gmgc.embl.de/downloads/v1.0/metadata/GMGC10.gene-environment.tsv.gz)
contains the link between genes and environments:

- `0`: the unigene does not contain any ORF from the respective environment,
- `1`: the unigene contain at least one ORF from the respective environment,
- `2`: the unigene contain at least one ORF from the respective environment
  **and** was detected in at least 5 samples from the environment.


Genes were assigned taxonomy as described in the manuscript: [Taxonomic
assignments](http://gmgc.embl.de/downloads/v1.0/metadata/GMGC10.taxonomy.tsv.gz)

The GMGC was also annotated with [eggnog-mapper](http://eggnog-mapper.embl.de/):
[GMGC10.emapper2.annotations.tsv.gz](http://gmgc.embl.de/downloads/v1.0/GMGC10.emapper2.annotations.tsv.gz)
(this file is [NGLess](https://ngless.embl.de/)-compatible).

CARD/ResFAM annotations (in [NGLess](https://ngless.embl.de/)-compatible
format) are also available as [GMGC10.card\_resfam.tsv](GMGC10.card_resfam.tsv.gz)

[Per sample
metadata](http://gmgc.embl.de/downloads/v1.0/metadata/GMGC10.sample.meta.tsv.gz)
includes the habitat, GPS coordinates, number of basepairs and other
information on each sample. Note that some samples use an internal name in
addition to the public one.

### Per sample abundances

This is a large table with 6 columns, representing 4 different sparse matrices:

- `unigene`: The unigene 
- `sample`: The sample
- `scaled`: The _scaled_ value
- `raw`: The _raw_ value
- `raw_unique`: The _raw_ value for uniquely mapped reads only
- `normed10m`: The _normed_ value scaled by 10 million

The meaning of `raw`, `scaled`, and `normed10m` are similar to that used in the
[NGLess count function](https://ngless.embl.de/Functions.html#count), except
that `normed10m` is scaled to 10 million reads. Briefly, `raw` is the number of
reads mapped to the gene (distributed using option `{dist1}` in NGLess),
`scaled` takes the size of each gene into account, and `normed10m` takes both
the size of each gene and the number of reads in each sample.

**Warning**: Note that this table file is 396GB (compressed) and contains
35,790,210,719 rows. Make sure you have the resources to handle it prior to
downloading it.

- [GMGC10.sample-abundance.tsv.xz](http://gmgc.embl.de/downloads/v1.0/GMGC10.sample-abundance.tsv.xz)

## GMBC10: Global Microbial Genome Bins 1.0

Genome bins have IDs of the form `GMBC10.NNN\_NNN`. They are ordered by
decreasing quality (defined as `completeness - 5 * contamination`, both metrics
estimated using [checkM](https://genome.cshlp.org/content/25/7/1043.short), so
that the first bin (`GMBC10.000\_000`) is the highest quality genome.

We also provide a table with estimated quality measures from each bin:
[GMBC10.meta.tsv](http://gmgc.embl.de/downloads/v1.0/GMBC10.meta.tsv). Bins
have been classified into three categories:

- `high-quality`: >90% complete & <5% contamination,
- `medium-quality`: >50% complete & <10% contamination,
- `low-quality`: all other bins.

As Unigenes can represent multiple ORFs, an Unigene may be present in multiple
genomic bins (and not all unigenes are in a genome bin).
The file
[GMGC10.GMBC10.tsv](http://gmgc.embl.de/downloads/v1.0/GMGC10.GMBC10.tsv)
associates unigenes with genome bins.

Individual genome FASTA files can be downloaded through the API, e.g.,
[http://gmgc.embl.de/api/v1.0/genome\_bin/GMBC10.001\_023/fasta](http://gmgc.embl.de/api/v1.0/genome_bin/GMBC10.001_023/fasta).

