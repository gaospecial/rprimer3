#' Write a list config to Boulder-IO format
#'
#' @param x a named list
#' @param file output file
#'
#' @return
#' @export
#'
#' @examples
write_boulder = function(x, file = tempfile()){
  # TODO: check values
  name = names(x)
  out = paste(name, unlist(x), sep = "=", collapse = "\n")
  out = paste(out, "=", sep = "\n" )
  writeLines(text = out, con = file)
}
