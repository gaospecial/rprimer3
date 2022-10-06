#' Design Primers
#'
#' @param fastafile sequence in fasta format
#' @param filter_description_pattern a string pattern
#' @param parts separate long template to parts
#' @param part_length length of part
#' @param ... other parameters passed to Primer3
#'
#' @return primers
#' @export
#'
#' @examples
#'   fastafile = system.file("sequence.fa", package = "rPrimer3")
#'   design_primer_from_file(fastafile)
design_primer_from_file = function(fastafile,
                                   filter_description_pattern = NULL,
                                   parts = 3,
                                   part_length = 500,
                                   ...){
  fasta = seqinr::read.fasta(file = fastafile, seqtype = "DNA", as.string = TRUE)
  if (!is.null(filter_description_pattern)){
    fasta = filter_fasta(fasta, annot_pattern = filter_description_pattern)
  }
  if (parts > 1){
    fasta = lapply(fasta, split_chromosome, num = parts, sequence_len = part_length)
    fasta = unlist(fasta, recursive = FALSE)
  }
  nseq = length(fasta)
  if (nseq < 1) stop("No sequence was found in ", fastafile)
  result = lapply(fasta, design_primer, parse = TRUE, ...)
  return(dplyr::bind_rows(result))
}

#' Design Primer from a `SeqFastadna` object
#'
#' @param fasta a `SeqFastadna` object
#' @param parse parse result
#' @param ...  other parameters passed to Primer3
#'
#' @return primer output
#' @export
#'
#' @examples
#' fastafile = system.file("sequence.fa", package = "rPrimer3")
#' fasta = seqinr::read.fasta(file = fastafile, seqtype = "DNA", as.string = TRUE)
#' design_primer(fasta[[1]])
design_primer = function(fasta, parse = TRUE, ...){
  if (!inherits(fasta, "SeqFastadna")) stop("Input is not valid.")
  id = attr(fasta, "name")
  seq = as.character(fasta)
  config = primer3_configure(SEQUENCE_ID = id, SEQUENCE_TEMPLATE = seq, ...)
  output = run_primer3(config)
  if (parse) output = parse_primer3_output(output)
  return(output)
}



