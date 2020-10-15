#' Clean Your Wiki
#'
#' This function accepts a string and removes new line indentations and html tags with regex
#' @param htmlString The string to be cleaned
#' @export
#' @examples
#' clean_wiki()

clean_wiki <- function(htmlString) {
  htmlString = gsub("\n", "", htmlString)
  return(gsub("<.*?>", "", htmlString))
}
