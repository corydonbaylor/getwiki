# Get Wikipedia Data with getwiki
`getwiki` is the Wikipedia api wrapper for R that you definitely knew you needed. The purpose of this package is to import Wikipedia articles into R, quickly and easily. `getwiki` will return articles in a format that plays nice with `tidytext`, simplifying your NLP workflow. Previously, the easiest way to import the text of a Wikipedia article was to highlight everything and then copy paste it into a character vector. 

This has the obvious disadvantages of formatting issues and being a manual, tedious process. With `getwiki` you can simply access Wikipedia using Wikipedia's API. 

Starting with `getwiki` is easy. First install it from github using devtools and then load it into your library. 

```{r setup}

# download from github
devtools::install_github("corydonbaylor/getwiki")
# load into R
library(getwiki)

```

## Use get_wiki to Import the Text of a Wikipedia Article

The first function, `get_wiki`, will return the matched Wikipedia article based on titles. So if you were to search for France then the function would return the Wikipedia article for France as a string. 

```{r getwiki, eval=FALSE}

# will return a character string with the contents of the wikipedia page on France. 
get_wiki("France")

```

If you want to search for more than one article at a time you can! `get_wiki()` will return multiple articles in a data.frame with one column being the title of the article and the other being the content of the article. Just put the needed items in a character vector. 

```{r getwiki_multiple, eval=FALSE}

# will return a character string with the contents of the wikipedia page on France. 
get_wiki(c("France", "United States"))

```

`get_wiki` will try to clean out all html tags returned by the API using regex. However, this can be unreliable as there is no simple regex pattern to only match html tags. If you would like to skip this, set `clean = FALSE`. 

```{r getwiki_clean, eval=FALSE}

# this will keep the html tags from the API results
get_wiki("France", clean = FALSE)

```

## Use search_wiki to Return the Top Twenty Results of a Search Term

Sometimes you may not be exactly sure what article you are looking for. `search_wiki` will return the top twenty matching articles based on a search term. So for example if you were to search for United States, you will retun a data.frame with a column for the returned titles and a column with the content of those articles. 

```{r search_wiki, eval=FALSE}

# this will keep the html tags from the API results
search_wiki("United States")

```

**Note that the content article only has the text of the first paragraph of the article.** If you want to return the full text of those articles you will need to use `get_wiki`. This can be easily accomplished in two lines.

```{r search_wiki_big, eval=FALSE}

# this will keep the html tags from the API results
us = search_wiki("United States")

# this will return the full text of the wikipedia articles
big_us = get_wiki(us$titles)

```

## Return a Random Article with random_wiki

If you would just like to pull in a random article, you can do that as well with `random_wiki`. This function works exactly the same as `get_wiki` except you cannot specify the article, and you can only return one article at a time.  

```{r search_wiki, eval=FALSE}

# returns a random wikipedia article
random_wiki()

```
## Find Trends for a Wikipedia Article with trend_wiki

If you want to find how often a particular page has been viewed, you can use `trend_wiki`. It will return a data.frame with the page views for the last sixty days. 

```{r trend_wiki, eval=FALSE}

# returns a random wikipedia article
trend_wiki("France")

```
