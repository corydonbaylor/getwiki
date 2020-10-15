#' Clean Your Wiki
#'
#' This function accepts a string and removes new line indentations and html tags with regex
#' @param string The string to be cleaned
#' @export
#' @examples
#' clean_wiki("<p>some text</p>")

clean_wiki <- function(string) {
  string = gsub("\n", "", string)
  return(gsub("<.*?>", "", string))
}
