#' Fetch sequence from NCBI
#'
#' @param term search term
#' @param db_from search db
#' @param db_to default: nuccore
#' @param rettype default: fasta
#' @param retmode text
#' @param file file to write sequence
#'
#' @return NULL, if write successfully
#' @export
#'
#' @examples
#' \dontrun{
#'   ncbi_fetch_seq("GCA_023823755.1")
#' }
ncbi_fetch_seq = function(term,
                          db_from = "assembly",
                          db_to = "nuccore",
                          rettype = c("fasta", "gb"),
                          retmode = "text",
                          file = paste(term, rettype, sep = ".")){
  rettype = match.arg(rettype)
  esearch = rentrez::entrez_search(db = db_from, term = term)
  ids = esearch$ids
  dbname = rentrez::entrez_db_links(db = db_from) %>% names()
  if (!db_to %in% dbname) stop("There is no way to fetch sequence from ", db_from)
  elink = rentrez::entrez_link(dbfrom = db_from, id = ids, db = db_to)
  sequence_id = elink[["links"]][["assembly_nuccore_insdc"]]
  recs = rentrez::entrez_fetch(db = db_to, id = sequence_id, rettype = rettype, retmode = retmode)
  message("Writing sequence to ", file)
  write(recs, file = file)
}
