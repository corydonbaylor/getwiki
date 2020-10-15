get_wiki <- function(title){
  result = jsonlite::fromJSON(paste0("https://en.wikipedia.org/w/api.php?action=query&titles=", title, "&prop=extracts&redirects=&format=json"))
  names(result$query$pages) = "content"
  return(clean_wiki(result$query$pages$content$extract))
}

