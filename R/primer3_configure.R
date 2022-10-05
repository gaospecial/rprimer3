primer3_configure = function(SEQUENCE_ID,
                             SEQUENCE_TEMPLATE,
                             PRIMER_TASK = "generic",
                             # PRIMER_PICK_LEFT_PRIMER = 1,
                             # PRIMER_PICK_RIGHT_PRIMER = 1,
                             PRIMER_OPT_SIZE = 20,
                             # PRIMER_MIN_SIZE = 18,
                             # PRIMER_MAX_SIZE = 22,
                             PRIMER_PRODUCT_SIZE_RANGE = "75-150",
                             PRIMER_NUM_RETURN = 1,
                             ...){
  config = list(
      SEQUENCE_ID = SEQUENCE_ID,
      SEQUENCE_TEMPLATE = SEQUENCE_TEMPLATE,
      PRIMER_TASK = PRIMER_TASK,
      PRIMER_OPT_SIZE = PRIMER_OPT_SIZE,
      PRIMER_PRODUCT_SIZE_RANGE = PRIMER_PRODUCT_SIZE_RANGE,
      PRIMER_NUM_RETURN = PRIMER_NUM_RETURN,
      ...
    )
  return(config)
}



get_sequence = function(x){
  if (!inherits(x, "SeqFastadna")) stop("Input is not a valid class")
  paste(x, collapse = "")
}
