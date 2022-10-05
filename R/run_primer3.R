#' Design Primers
#'
#' @param fastafile sequence in fasta format
#' @param ... other parameters passed to Primer3
#'
#' @return primers
#' @export
#'
#' @examples
#'   fastafile = system.file("sequence.fa", package = "rPrimer3")
#'   design_primer(fastafile)
design_primer = function(fastafile, ...){
  fasta = seqinr::read.fasta(file = fastafile, seqtype = "DNA")
  nseq = length(fasta)
  if (nseq < 1) stop("No sequence was found in ", fastafile)
  result = vector("list", nseq)
  for (i in seq_along(fasta)){
    this = fasta[[i]]
    id = attr(this, "name")
    seq = get_sequence(this)
    config = primer3_configure(SEQUENCE_ID = id, SEQUENCE_TEMPLATE = seq, ...)
    input = write_boulder(config)
    output = run_primer3(input)
    result[[i]] = parse_primer3_output(output)
  }
  return(dplyr::bind_rows(result))
}


run_primer3 = function(input, exec = find_primer3()){
  output <- system(paste(exec, input), intern=TRUE)
  return(output)
}

find_primer3 = function(){
  if (.Platform$OS.type == "unix"){
    ret = system("which primer3_core")
    if (ret == 0) return("primer3_core")
    stop("Primer3 is not found in your system. Please install it and retry.")
  } else if (.Platform$OS.type == "windows"){
    exe = system.file("bin/primer3_core.exe", package = "rPrimer3")
    return(exe)
  } else {
    stop("Something is wrong.")
  }
}

parse_primer3_output = function(output){
  raw = read_boulder(output)
  id = raw[["SEQUENCE_ID"]]
  size = raw[["PRIMER_PAIR_0_PRODUCT_SIZE"]]
  fseq = raw[["PRIMER_LEFT_0_SEQUENCE"]]
  rseq = raw[["PRIMER_RIGHT_0_SEQUENCE"]]
  dplyr::tibble(
    name = paste0(id, c("f", "r")),
    sequence = c(fseq, rseq),
    product_size = size
  )
}
