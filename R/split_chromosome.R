split_chromosome = function(fasta,
                            num = 3,
                            sequence_len = 500
                            ){
  if (!inherits(fasta, "SeqFastadna")) stop("Input is not valid.")
  id = attr(fasta, "name")
  chr_seq = as.character(fasta)
  chr_len = stringr::str_length(chr_seq)
  start = seq(1, chr_len, by = ceiling(chr_len/3))
  subseq = stringr::str_sub(chr_seq, start, start + sequence_len)
  fasta_list = lapply(seq_along(subseq), function(i){
    seqinr::as.SeqFastadna(subseq[[i]],
                           name = paste(id, i, sep = "_"),
                           Annot = paste(id, "part", i))
  })
  return(fasta_list)
}

filter_fasta = function(fasta,
                        annot_pattern = "chromosome"){
  annot = lapply(fasta, attr, which = "Annot")
  chr = fasta[grep(annot_pattern, annot)]
  return(chr)
}
