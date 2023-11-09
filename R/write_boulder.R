#' Write a list config to Boulder-IO format
#'
#' @param x a named list
#' @param file output file
#'
#' @return output file name
#' @export
#'
#' @examples
#'   l = list(A = 1, B = 2)
#'   write_boulder(l)
write_boulder = function(x, file = tempfile()){
  # TODO: check values
  name = names(x)
  out = paste(name, unlist(x), sep = "=", collapse = "\n")
  out = paste(out, "=", sep = "\n" )
  writeLines(text = out, con = file)
  return(file)
}

#' Read boulder to list
#'
#' @param x text vector in Boulder format
#'
#' @return a list
#' @export
#'
#' @examples
#'   read_boulder(x = c("A=1","B=2"))
read_boulder = function(x){
  value = gsub("^[^=]+=", "", x)
  name = gsub("=[^=]+$", "", x)
  names(value) = name
  return(value)
}
