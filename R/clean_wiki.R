clean_wiki <- function(htmlString) {
  htmlString = gsub("\n", "", htmlString)
  return(gsub("<.*?>", "", htmlString))
}
