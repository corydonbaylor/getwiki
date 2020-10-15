#' Get the Text of a Wikipedia Article
#'
#' Get the text of a wikipedia article by submitted a title as a string.
#' @param title The title to be searched
#' @param clean Should getwiki remove html tags from the returned text?
#' @export
#' @examples
#' get_wiki()

get_wiki <- function(title, clean = TRUE){

  # using jsonlite we pull back the wikipedia article
  result = jsonlite::fromJSON(paste0("https://en.wikipedia.org/w/api.php?action=query&titles=", title, "&prop=extracts&redirects=&format=json"))

  # if the query does not bring anything back the page will be titled -1
  # in this case we stop
  if(names(result$query$pages)=="-1"){
    stop("Getwiki could not find the requested page title. Make sure you are entering a valid title.")
  }

  #the page is usually called the article id, we will rename it to content
  names(result$query$pages) = "content"

  # if the user selects true for clean then we will use regex to clean the text
  if(clean == TRUE){
    return(clean_wiki(result$query$pages$content$extract))
  }else{
    return(result$query$pages$content$extract)
  }
}

