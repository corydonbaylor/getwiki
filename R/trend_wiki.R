#' Find the Page Views for an Article
#'
#' Find the page views for an article for the past sixty days.
#' @param title The title of the Wikipedia article you would like trends for.
#' @return trend_wiki will return a dataframe of the past sixty days of page views for the requested title
#' @export
#' @examples
#' trend_wiki("Belgrade")

trend_wiki = function(title){
  # title test
  if(length(title) != 1){
    stop("Please enter a single value for title")
  }

  # cleaning title
  title = gsub("\\s", "_", title)
  title = gsub("–", " ", title)

  # querying wikipedia
  trend = jsonlite::fromJSON(paste0("https://en.wikipedia.org/w/api.php?action=query&titles=", title, "&prop=pageviews&redirects=&format=json"))

  names(trend$query$pages) = "content"

  # getting the values
  vec = unlist(trend$query$pages$content$pageviews)
  # getting the date value
  dates = names(vec)
  dates = as.Date(dates, "%Y-%m-%d")

  # return a dataframe
  return(data.frame(
    title = trend$query$pages$content$title,
    date = dates,
    value = as.numeric(vec)
    ))
}
