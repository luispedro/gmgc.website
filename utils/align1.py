import skbio.alignment
from skbio.sequence import Protein
from BLOSUM import blosum62


HTML_HEADER = '''
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8" />
        <title>Alignment result</title>
        <style type="text/css">
            .match{width:15px; height:19px; display:inline-block;}
            .gap{width:15px; height:19px; display:inline-block;}
            .close{width:15px; height:19px; display:inline-block;}
            .mismatch{width:15px; height:19px; display:inline-block;}
            span{font-family: monospace;text-align:center;font-size:15pt}
        </style>
    </head>
    <body>
'''
HTML_FOOTER = '''
    </body>
</html>
'''




def num_alignment(query,target):
    num = 0
    for nucl_q , nucl_t in zip(query ,target):
            if nucl_q == nucl_t:
                num += 1
    return num

def alignment(protein_query, protein_target, line_length, output_format='text/plain', html_header=False):
    sw_protein = skbio.alignment.local_pairwise_align_ssw(Protein(protein_query), Protein(protein_target),
                                                          substitution_matrix=blosum62, gap_open_penalty=11,
                                                          gap_extend_penalty=1)
    query_align = str(sw_protein[0][0])
    target_aligh = str(sw_protein[0][1])
    align = num_alignment(query_align, target_aligh)
    identity = align / len(target_aligh)
    query_start , query_end = sw_protein[2][0]
    target_start , target_end = sw_protein[2][1]
    query_coverage = (query_end - query_start + 1) / len(protein_query)
    target_coverage = (target_end - target_start + 1) / len(protein_target)

    out_text = []

    out_text.append('identity: '+ '{:.2f}%'.format(identity*100))
    out_text.append('Query coverage: {:.2f}%(positions {}-{}; total length {})'.format(query_coverage*100,query_start,query_end,len(protein_query)))
    out_text.append(
        'Unigene coverage: {:.2f}%(positions {}-{}; total length {})\n'.format(target_coverage * 100, target_start, target_end,
                                                                           len(protein_target)))

    out_html = ['<div class="alignment">']
    out_html.append( '<p class="summary">{}</p>'.format(out_text[0]) )
    out_html.append( '<p class="summary">{}</p>'.format(out_text[1]) )
    out_html.append( '<p class="summary">{}</p><br/>'.format(out_text[2].strip('\n')) )

    out_html.append( '<p class="alignment">' )
    for row_index in range(len(query_align)//length + 1):
        if row_index < len(query_align)//length:
            row_mid = ''
            html_query = ''
            html_mid = ''
            html_target = ''

            for column_index in range(row_index*length , (row_index+1)*length):
                if query_align[column_index] == '-' or target_aligh[column_index] == '-':
                    row_mid = row_mid+ ' '
                    html_query = html_query + '<span class="gap">{}</span>'.format(query_align[column_index])
                    html_mid = html_mid + '<span class="gap">&nbsp</span>'
                    html_target = html_target + '<span class="gap">{}</span>'.format(target_aligh[column_index])
                elif query_align[column_index] != target_aligh[column_index]:
                    row_mid = row_mid + '.'
                    if blosum62[query_align[column_index]][target_aligh[column_index]] >= 0:
                        """
                        close
                        """
                        html_query = html_query + '<span class="close">{}</span>'.format(query_align[column_index])
                        html_mid = html_mid + '<span class="close">&#58</span>'
                        html_target = html_target + '<span class="close">{}</span>'.format(target_aligh[column_index])
                    else:
                        """
                        mismatch
                        """
                        html_query = html_query + '<span class="mismatch">{}</span>'.format(query_align[column_index])
                        html_mid = html_mid + '<span class="mismatch">.</span>'
                        html_target = html_target + '<span class="mismatch">{}</span>'.format(target_aligh[column_index])


                else:
                    row_mid = row_mid + '|'
                    html_query = html_query + '<span class="match", >{}</span>'.format(query_align[column_index])
                    html_mid = html_mid + '<span class="match">|</span>'
                    html_target = html_target + '<span class="match">{}</span>'.format(target_aligh[column_index])
            out_text.append(query_align[row_index*length: (row_index+1)*length])
            out_text.append(row_mid)
            out_text.append(target_aligh[row_index*length: (row_index+1)*length] + '\n')
            out_html.append( html_query + '<br/>' )
            out_html.append( html_mid + '<br/>' )
            out_html.append( html_target + '<br/><br/>' )

        else:
            row_mid = ''
            html_query = ''
            html_mid = ''
            html_target = ''
            for column_index in range(row_index*length , len(target_aligh)):
                if query_align[column_index] == '-' or target_aligh[column_index] == '-':
                    row_mid = row_mid+ ' '
                    html_query = html_query + '<span class="gap">{}</span>'.format(query_align[column_index])
                    html_mid = html_mid + '<span class="gap">&nbsp</span>'
                    html_target = html_target + '<span class="gap">{}</span>'.format(target_aligh[column_index])
                elif query_align[column_index] != target_aligh[column_index]:
                    row_mid = row_mid + '.'
                    if blosum62[query_align[column_index]][target_aligh[column_index]] >= 0:
                        """
                        close
                        """
                        html_query = html_query + '<span class="close">{}</span>'.format(query_align[column_index])
                        html_mid = html_mid + '<span class="close">&#58</span>'
                        html_target = html_target + '<span class="close">{}</span>'.format(target_aligh[column_index])
                    else:
                        """
                        mismatch
                        """
                        html_query = html_query + '<span class="mismatch">{}</span>'.format(query_align[column_index])
                        html_mid = html_mid + '<span class="mismatch">.</span>'
                        html_target = html_target + '<span class="mismatch">{}</span>'.format(target_aligh[column_index])
                else:
                    row_mid = row_mid + '|'
                    html_query = html_query + '<span class="match">{}</span>'.format(query_align[column_index])
                    html_mid = html_mid + '<span class="match">|</span>'
                    html_target = html_target + '<span class="match">{}</span>'.format(target_aligh[column_index])
            out_text.append(query_align[row_index*length:len(query_align)])
            out_text.append(row_mid)
            out_text.append(target_aligh[row_index*length: len(target_aligh)])
            out_html.append( html_query + '<br/>' )
            out_html.append( html_mid + '<br/>' )
            out_html.append( html_target + '</p></div>' )

    if output_format == 'text/plain':
        for s in out_text:
            print(s)
    else:
        if html_header:
            print(HTML_HEADER)
        for s in out_html:
            print(s)

        if html_header:
            print(HTML_FOOTER)


if __name__ == '__main__':
    from os import path
    from sys import argv
    from fasta_reader import IndexedFastaReader

    import sys
    index_fname = argv[1]
    query_fname = argv[2]
    unigene_id = argv[3]
    otype = argv[4]
    if not (otype.startswith('text/plain') or otype.startswith('text/html')):
        sys.stderr.write("Only text/plain and text/html are supported as output types\n")
        sys.exit(1)

    index_base, index_fname  = path.split(path.abspath(index_fname))
    index = IndexedFastaReader(path.join(index_base, index_fname))
    target_aa = index.get(unigene_id).decode('ascii')
    query_aa = None
    for fa in skbio.io.read(query_fname, format='fasta'):
        query_aa = str(fa)
        break
    alignment(query_aa, target_aa, 80, output_format=otype)

