#' Get the Text of a Random Wikipedia Article
#'
#' Get the text of a random wikipedia article
#' @param clean Should getwiki remove html tags from the returned text?
#' @return \code{random_wiki} will return a single named character value whose value is the text of the wikipedia page
#' @examples
#' random_wiki()
#' @export

random_wiki <- function(clean =TRUE){
  result = jsonlite::fromJSON(paste0("https://en.wikipedia.org/w/api.php?action=query&generator=random&format=json&grnnamespace=0&prop=extracts"))

  #the page is usually called  by its article id, we will rename it to content, so the name of the variable
  # doesnt vary
  names(result$query$pages) = "content"

  # if the user selects true for clean then we will use regex to clean the text
  if(clean == TRUE){
    random = clean_wiki(result$query$pages$content$extract)
  }else{
    random = result$query$pages$content$extract
  }

  # naming the chr
  names(random) = result$query$pages$content$title

  return(random)
}
