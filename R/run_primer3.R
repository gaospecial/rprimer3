run_primer3 = function(config, exec = find_primer3()){
  input = write_boulder(config)
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
