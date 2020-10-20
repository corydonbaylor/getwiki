#' Search Wikipedia for Articles
#'
#' Search for the top twenty wikipedia pages that match a given query. This function will return a dataframe with the
#' names of the matched articles and the first paragraph of content.
#' @param search_term The search term you would like to use.
#' @param clean Should getwiki remove html tags from the returned text?
#' @return search_term will return a dataframe of the top twenty search results. The "title" column will be the title
#' of the articles and the "content" column will be the first paragraph from those articles.
#' @export
#' @examples
#' search_wiki("Belgrade")

search_wiki = function(search_term, clean = TRUE){

  # cleaning title
  search_term = gsub("\\s", "_", search_term)
  search_term = gsub("–", " ", search_term)

  # querying wikipedia
  result = jsonlite::fromJSON(paste0("https://en.wikipedia.org/w/api.php?action=query&generator=search&gsrlimit=20&prop=extracts&exintro&explaintext&exlimit=max&format=json&gsrsearch=",
                                     search_term))

  # initilizing columns outside loop
  titles = c()
  content = c()

  for(i in 1:length(result$query$pages)){

      # we are going to overwrite the article id with "content"
      # but we dont want to overwrite this for the result
      temp = result$query$pages[i]
      names(temp) = "content"

      # user chooses if they want html tags removed
      if(clean == TRUE){
        item_content = clean_wiki(temp$content$extract)
      }else{
        item_content = temp$content$extract
      }
      # add the values to columns
      content = append(content, item_content)
      titles = append(titles, temp$content$title)
  }

  # return a dataframe
  return(data.frame(titles, content))
}





