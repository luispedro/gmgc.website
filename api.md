# GMGC API

The base URL for API calls is `http://gmgc.embl.de/api/v1.0/` and it returns JSON.

Also, note that the resources can all be [downloaded](/download.cgi) for local
processing. For large scale analyses, that will be more efficient.

## Lookup

### Unigenes

- `unigene/\<gene\_id\>`

Example output

```
    {
        "strand": "+" or "-",
        "query": "GMGC10.054_598_380.SCLAV_5304",
        "name":"GMGC10.054_598_380.SCLAV_5304",
        "taxonomy":[
            {"name":"environmental samples",
             "id":"59619",
             "rank":"genus"}
             ],
        "samples": 573,
        "length":"88062",
        "habitat":["human skin","human gut"],
        "complete": 1 or 0
    }
```

- `unigene/\<gene\_id\>/dna\_sequence`

```
    {
        "query":"GMGC10.054_598_380.SCLAV_5304",
        "name":"GMGC10.054_598_380.SCLAV_5304",
        "dna_sequence":"ATGAAGTTAGGGGAG..."

    }
```

- `unigene/\<gene\_id\>/protein\_sequence`

Same as above, but the field is called `protein\_sequence`


- `unigene/\<gene\_id\>/features`

```
    {
        'query': 'GMGC10.054_598_380.SCLAV_5304',
        'name': 'GMGC10.054_598_380.SCLAV_5304',
        'features':
            {
                'intrinsic': [
                    {'feature': 'COIL',
                    'end': 351,
                    'start': 321}],
                'pfam': [
                    {'domain': 'Pfam:RCC1',
                    'evalue': 3.4e-06,
                    'bitscore': 24.2,
                    'end': 891,
                    'start': 834},
                   {'domain': 'Pfam:RCC1_2',
                    'evalue': 4.1e-07,
                    'bitscore': 26.4,
                    'end': 907,
                    'start': 878}],
                'smart': [
                    {'domain': 'PbH1',
                    'evalue': 4989.07360092172,
                    'bitscore': 2.3,
                    'end': 782,
                    'start': 755},
                   {'domain': 'TECPR',
                    'evalue': 102.066040067862,
                    'bitscore': 4.4,
                    'end': 902,
                    'start': 870}],
                'eggnog':
                    {'bigg_reactions': [],
                       'cog_functional_category': 'D, Z',
                       'seed_ortholog_score': 531.2,
                       'go_terms': [],
                       'seed_eggnog_ortholog': '483219.LILAB_18585',
                       'predicted_gene_name': 'SCLAV_5304',
                       'seed_ortholog_evalue': 1.2e-146,
                       'kegg_ko': [],
                       'eggnog_hmm_model_annotation': 'Regulator of chromosome condensation'}
            }
    }
```

- `unigene/\<gene\_id\>/samples`

```
    {
        'query': 'GMGC10.054_598_380.SCLAV_5304',
        'name': 'GMGC10.054_598_380.SCLAV_5304',
        'samples': [
            'SAMEN123000',
            'SAMEN123001',
            ...]
    }
```

- `unigene/\<gene\_id\>/genome\_bins`

```
    {
        'query': 'GMGC10.054_598_380.SCLAV_5304',
        'name': 'GMGC10.054_598_380.SCLAV_5304',
        'genome_bins': [
            'GMBC10.012_002',
            'GMBC10.149_387',
            ...]
    }
```

### Genome bins

- `genome\_bin/\<genome\_bin\_id\>`

```
    {
        "name": "GMBC10.001_023",
        "sample": "SAMEN123000",
        "completeness": 99.0,
        "contamination": 1.0,
        "quality": 94.0,
        "nr_contigs": 123,
        "total_bp_size": 24232323,
        "N50": 6223,
        "min_contig_size": 2506,
        "max_contig_size": 12342,
        "category": "high-quality" or "medium-quality" or "low-quality",
        "taxonomic_prediction": "E. coli K12",
        "taxonomic_confidence": 23.3
    }
```

### Samples

- `sample/\<sample\_id\>`

```
    {
        "name": "SAMEN123000",
        "latitude": 12.2,
        "longitude": -102.2,
        "ena_link": "https://ena..../",
        "habitat": "human gut"
    }
```


### Habitats

- `habitat/\<habitat\_name\>

```
    {
        "name": "human gut",
        "samples":
                [ "SAMEN123000",
                "...",
                ...],
        "subcatalog\_url": "http://gmgc.embl.de/downloads/v1.0/GMGC10.human-gut.95nr.fna.gz",
        "subcatalog\_no\_rare": "http://gmgc.embl.de/downloads/v1.0/GMGC10.human-gut.no-rare.95nr.fna.gz"
    }
```

## Search

### Query by sequence

- query/sequence`

The call is as a `POST` request of up to 50 sequences as an attached FASTA
file. The attachment should be called `fasta`.

For example, create `test.fasta` containing:

    >MySeq
    AALAMSALMALSJLAJLACAOSIJDAOSIJDALAASKJDASLKJALCEMALWPQRODASLKJALCKMALWPQRODASLKJALCCKMALWPQRODASLKJALCKMALWPQROQUPJALSFAASLUFPASUFASFJA

and with `curl`:

    curl -X POST -F 'fasta=@test.fasta' -F 'mode=all' -F 'return_seqs=true' -F 'return_bins=true' http://gmgc.embl.de/api/v1.0/query/sequence

Note that the algorithm will always returns its best matches as _hits_ and it
is the user's responsibility to filter them appropriately (_i.e._, if no good
matches exist in the catalog, the algorithm will still return something, but it
will be returned with a high e-value).

Parameters:

- `mode`: `"all"` or `"besthit"`
- `return\_seqs`: boolean
- `return\_bins`: boolean

```
    {
        "results":
            [{
                "query\_name": "Q1",
                "hits":
                    [ # hits is always a list, but if the request included `mode=besthit`, this will be a list of size one.
                    {"gene\_id": "GMGC10.000_000_000.NAME",
                     "evalue":12e-23,
                     "bitscore":232.2,
                        # sequences are only provided if the request included `return\_seqs=true`
                     "dna\_sequence": "ATTATACAA...",
                     "protein\_sequence": "MEPATA..."
                        # Genome bins are only provided if the request included `return\_bins=true`
                     "genome\_bins":
                        [ "GMBC10.001_023"
                        , "GMBC10.202_232"
                        ]
                    },
                    ...
                    ]
              }, {
                  "query\_name": "Q2",
                  "hits": ...
              }]
    }
```



