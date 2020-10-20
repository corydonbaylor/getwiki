#' Get the Text of a Wikipedia Article
#'
#' Get the text of a wikipedia article by searching a title. For example, entering the search term "France" will return
#' the text of the wikipedia page for France.
#' @param title The title or (titles) of the Wikipedia page to be searched. If you would like to query multiple articles,
#' put the titles in a character vector. The maximum number of titles that can be queried at one time is 50.
#' @param clean Should getwiki remove html tags from the returned text?
#' @return A single title will return the matched wikipedia article in a string. A vector of titles will return
#' a dataframe with one column equal to the searched titles and one column equal to the matched article content
#' @examples
#' get_wiki("United States")
#' get_wiki(c("United States", "France"))
#' @export


get_wiki <- function(title, clean = TRUE){


# Validating Titles for API -----------------------------------------------

  # replace spaces with underscore
  title = gsub("\\s", "_", title)
  # replaces - which is invalid
  title = gsub("–", " ", title)


# Single title to search --------------------------------------------------


  if(length(title)==1){

    # using jsonlite we pull back the wikipedia article
    result = jsonlite::fromJSON(paste0("https://en.wikipedia.org/w/api.php?action=query&titles=", title, "&prop=extracts&redirects=&format=json"))

    # if the query does not bring anything back the page will be titled -1
    # in this case we stop
    if(names(result$query$pages)=="-1"){
      stop("Getwiki could not find the requested page title. Make sure you are entering a valid title.")
    }

    #the page is usually called  by its article id, we will rename it to content, so the name of the variable
    # doesnt vary
    names(result$query$pages) = "content"

    # if the user selects true for clean then we will use regex to clean the text
    if(clean == TRUE){
      return(clean_wiki(result$query$pages$content$extract))
    }else{
      return(result$query$pages$content$extract)
    }

# Multiple titles to search -----------------------------------------------


  }else{
    titles = c()
    content = c()

    for(i in 1:length(title)){
      item = title[i]

      item = gsub("\\s", "_", item)
      # using jsonlite we pull back the wikipedia article
      result = jsonlite::fromJSON(paste0("https://en.wikipedia.org/w/api.php?action=query&titles=", item, "&prop=extracts&redirects=&format=json"))

      # if the query does not bring anything back the page will be titled -1
      # in this case we stop
      if(names(result$query$pages)=="-1"){
        stop(paste0("Getwiki could not find the requested page title for the entry '", item, "'. Make sure you are entering a valid title."))
      }

      #the page is usually called the article id, we will rename it to content
      names(result$query$pages) = "content"

      # if the user selects true for clean then we will use regex to clean the text
      if(clean == TRUE){
        item_content = clean_wiki(result$query$pages$content$extract)
      }else{
        item_content = result$query$pages$content$extract
      }
      content = append(content, item_content)
      titles = append(titles, result$query$pages$content$title)

    }

    return(data.frame(titles, content))
  }

}

