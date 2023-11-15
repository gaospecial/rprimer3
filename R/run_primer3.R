run_primer3 = function(config, exec = find_primer3()){
  input = write_boulder(config)
  output <- system(paste(exec, input), intern=TRUE)
  return(output)
}

find_primer3 = function(){
  if (.Platform$OS.type == "unix"){
    ret = system("which primer3_core", ignore.stdout = TRUE)
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
  required_keys = c("SEQUENCE_ID","PRIMER_PAIR_0_PRODUCT_SIZE","PRIMER_LEFT_0_SEQUENCE","PRIMER_RIGHT_0_SEQUENCE")
  if (all(required_keys %in% names(raw))){
    id = raw[["SEQUENCE_ID"]]
    size = raw[["PRIMER_PAIR_0_PRODUCT_SIZE"]]
    fseq = raw[["PRIMER_LEFT_0_SEQUENCE"]]
    rseq = raw[["PRIMER_RIGHT_0_SEQUENCE"]]
    df = dplyr::tibble(
      name = paste0(id, c("f", "r")),
      sequence = c(fseq, rseq),
      product_size = size
    )
    return(df)
  } else {
    return(NULL)
  }
}
