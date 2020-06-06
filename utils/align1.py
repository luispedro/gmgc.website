import skbio.alignment
from skbio.sequence import Protein
from BLOSUM import blosum62


def num_alignment(query,target):
    num = 0
    for nucl_q , nucl_t in zip(query ,target):
            if nucl_q == nucl_t:
                num += 1
    return num

def alignment(protein_query, protein_target, length):
    sw_protein = skbio.alignment.local_pairwise_align_ssw(Protein(protein_query), Protein(protein_target),
                                                          substitution_matrix=blosum62, gap_open_penalty=11,
                                                          gap_extend_penalty=1)
    query_align = str(sw_protein[0][0])
    target_aligh = str(sw_protein[0][1])
    align = num_alignment(query_align, target_aligh)
    identity = align / len(target_aligh)
    out = []

    out.append('identity: '+ '{:.2f}%'.format(identity*100))
    for row_index in range(len(query_align)//length + 1):
        if row_index < len(query_align)//length:
            row_mid = ''
            for column_index in range(row_index*length , (row_index+1)*length):
                if query_align[column_index] == '-' or target_aligh[column_index] == '-':
                    row_mid = row_mid+ ' '
                elif query_align[column_index] != target_aligh[column_index]:
                    row_mid = row_mid + '.'
                else:
                    row_mid = row_mid + '|'
            out.append(query_align[row_index*length: (row_index+1)*length])
            out.append(row_mid)
            out.append(target_aligh[row_index*length: (row_index+1)*length] + '\n')
        else:
            row_mid = ''
            for column_index in range(row_index*length , len(target_aligh)):
                if query_align[column_index] == '-' or target_aligh[column_index] == '-':
                    row_mid = row_mid+ ' '
                elif query_align[column_index] != target_aligh[column_index]:
                    row_mid = row_mid + '.'
                else:
                    row_mid = row_mid + '|'
            out.append(query_align[row_index*length:len(query_align)])
            out.append(row_mid)
            out.append(target_aligh[row_index*length: len(target_aligh)])

    for s in out:
        print(s)


if __name__ == '__main__':
    from os import path
    from sys import argv
    from fasta_reader import IndexedFastaReader

    import sys
    index_fname = argv[1]
    query_fname = argv[2]
    unigene_id = argv[3]
    otype = argv[4]
    if not otype.startswith('text/plain'):
        sys.stderr.write("Only text/plain is supported as output type\n")
        sys.exit(1)

    index_base, index_fname  = path.split(path.abspath(index_fname))
    index = IndexedFastaReader(path.join(index_base, index_fname))
    target_aa = index.get(unigene_id).decode('ascii')
    query_aa = None
    for fa in skbio.io.read(query_fname, format='fasta'):
        query_aa = str(fa)
        break
    alignment(query_aa, target_aa, 80)







