{
  const API = "/api/v1.0/";
  const SEQUENCE = ">MySeq\nAALAMSALMALSJLAJLACAOSIJDAOSIJDALAASKJDASLKJALCEMALWPQRODASLKJALCKMALWPQRODASLKJALCCKMALWPQRODASLKJALCKMALWPQROQUPJALSFAASLUFPASUFASFJA";

  const add_hits_table_rows = (it) => {
    // We need to access the sequences to build the download files
    let tbl = $("#hits_table tbody");

    // Note the API supports querying multiple sequences at a time
    // but we only submit one in the website
    $.data(document.body, "search", it.results[0]);
    console.log(it.results);

    tbl.empty();

    let tmpl = $.templates("#table_row_tmpl");
    tbl.append(tmpl.render(it.results[0].hits));
  };

  const download_sequences = (seq_target) => {
    let tmpl = $.templates(`>{{:gene_id}}\n{{:${seq_target}}}\n`)
    let text = tmpl.render($.data(document.body, "search").hits);

    var element = document.createElement('a');
    element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
    element.setAttribute('download', `${seq_target}s.fasta`);
    element.style.display = 'none';
    document.body.appendChild(element);
    element.click();
    document.body.removeChild(element);
  };

  const startRequests = () => {
    // First replace targets with "Loading..."
    $("#hits_table tbody").empty();

    $.ajax({
      method: "POST",
      url: API.concat("query/sequence"),
      data: {
        fasta: SEQUENCE,
        return_seqs: true,
        return_bins: true
      }
    }).done(function(it) {
      add_hits_table_rows(it);

      $("#protein_download").click(function() {
        download_sequences("protein_sequence");
      });

      $("#dna_download").click(function() {
        download_sequences("dna_sequence");
      });

    }).fail(function() {
      $("#hits_table").replaceWith("Failed to load hits!");
    });
  }

  $(document).ready(function() {
    setTimeout(function () {
      console.log("ready to go");
      startRequests();
    }, 1000);

    // vegaEmbed('#vis-main', "specs/scores.json")
    // vegaEmbed('#vis-homologs', "specs/map.json")
    // vegaEmbed('#vis-genebiome', "specs/genebiome.json")
    // // TODO Krona integration
    // // vegaEmbed('#vis-taxonomy', "specs/taxonomy.json")
    // vegaEmbed('#vis-funccat', "specs/func-cats.json")
    // vegaEmbed('#vis-smart', "specs/smart-doms.json")
    // vegaEmbed('#vis-pfam', "specs/pfam-doms.json")
    // vegaEmbed('#vis-eggnog', "specs/eggnog-ogs.json")

    // vegaEmbed('#vis-eggnog', "spec/...").then(res =>
    //   res.view
    //   .insert("myData", mydata)
    //   .run()
    // )

  });
}
