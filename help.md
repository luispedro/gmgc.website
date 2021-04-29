Global Microbial Gene Catalog (GMGC)
------------------------------------

The Global Microbial Gene Catalog (**GMGC**, currently version 1.0) is an
integrated, consistently-processed, gene catalog of the microbial world,
combining metagenomics and high-quality sequenced isolates (from the
[ProGenomes2 database](https://progenomes.embl.de), Mende et al., 2019).

A total of 2.3 Billion genes were used to build the catalog. After
100%-identity redundancy removal, we obtained a 100% non-redundant catalog with
966 million sequences. Further, species-level (95% nucleotide identity)
resulted in the main GMGC catalog, which includes 302 million unigenes. See the
[Downloads page](https://gmgc.embl.de/download.cgi) for more details and
various habitat-specific subcatalogs that may be more convenient for your
usage.

Available subcatalogs
---------------------

In addition to the stratification by habitat, we also provide versions of the
catalog that exclude fragmentary open reading frames (ORFs). Fragmentary ORFs
arise when due to the incomplete assemblies so that either the start or the
stop codons may be missing (or both). They represent real DNA and should be
accounted for when considering all the extant sequences (\_e.g.\_, when
considering whether and where a particular sequence has been sequenced before).
However, they are not appropriate to use for all analyses. For example,
multiple sequence alignments in protein families which include fragmentary ORFs
need to be interpreted with care as missing portions of a gene may be missing
solely due to the incompleteness of the assembly.

Thus, to cater to users who wish for a higher standard of quality of each
individual sequence (albeit at potential the cost of a loss of coverage), we
also make available a version of the catalog that only includes complete ORFs
(both start and stop codon present).

Similarly, our protein families were built using \_e-value\_ thresholds to
ensure that they are grouping together sequences which share an evolutionary
history, even at the so-called \_twilight zone\_ (<30% amino acid identity, see
Rost 1999). However, for users who want a stricter dataset, we also provide
families with an additional identity threshold of 30% (thus avoiding the
twilight zone) as well as 50% (analogously to what is provided by RefSeq50).
While, as we also show in the manuscript, many analyses will be robust to these
technical choices (and we default to the broader 20% amino acid threshold for
general usage), we offer the higher-threshold versions for users who need
stricter assurances with respect to their analyses.

Identifiers
-----------

Genes in the main catalog and subcatalogs are identified with the scheme
`GMGC10.###_###_###.NAME`. The initial GMGC10 indicates the version of the
catalog (Global Microbial Gene Catalog 1.0). The numeric ID uniquely identifies
the unigene (the underscores are for readability only). Finally, the NAME is the
predicted gene name, obtained from
[eggnog-mapper](http://eggnog-mapper.embl.de/) (Huerta-Cepas et al., 2017).

Searching
---------

Search uses a kmer-based index. Briefly, the query sequence is broken up into
consecutive 7-mers and the 100 sequences in the database which share the
highest number of kmers with the query are retrieved. These are aligned against
the query using a fast implementation of Smith-Waterman (Zhao et al., 2013) and
sorted by alignment score. This method is very efficient at recovering sequence
that are similar to ones in the database, but is less sensitive than BLAST.

References
----------

1.  Burkhard Rost **Twilight zone of protein sequence alignments** in Protein Engineering, Design and Selection, 1999, [doi:10.1093/protein/12.2.85](https://doi.org/10.1093/protein/12.2.85)
2.  Daniel R Mende, Ivica Letunic, Oleksandr M Maistrenko, Thomas S B Schmidt, Alessio Milanese, Lucas Paoli, Ana Hernández-Plaza, Askarbek N Orakov, Sofia K Forslund, Shinichi Sunagawa, Georg Zeller, Jaime Huerta-Ceps, Luis Pedro Coelho and Peer Bork **proGenomes2: an improved database for accurate and consistent habitat, taxonomic and functional annotations of prokaryotic genomes** in Nucleic Acid Research, 2019 [doi:10.1093/nar/gkz1002](https://doi.org/10.1093/nar/gkz1002)
3.  Jaime Huerta-Cepas, Kristoffer Forslund, Luis Pedro Coelho, Damian Szklarczyk, Lars Juhl Jensen, Christian von Mering and Peer Bork **Fast genome-wide functional annotation through orthology assignment by eggNOG-mapper** in Mol Biol Evol, 2017 [doi:10.1093/molbev/msx148](https://doi.org/10.1093/molbev/msx148)
4.  Mengyao Zhao, Wan-Ping Lee, Erik P. Garrison, and Gabor T. Marth **SSW Library: An SIMD Smith-Waterman C/C++ Library for Use in Genomic Applications** in PloS One, 2013 [doi:10.1371/journal.pone.0082138](https://doi.org/10.1371/journal.pone.0082138)
