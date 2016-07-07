require(rvest)
require(magrittr)

getTable <- function(page)
{
  url <- paste0("https://www.brewtoad.com/recipes?page=",page,"&sort=rank&view_as_table=true")
  url%>%
    read_html()%>%
    html_nodes("table")%>%
    html_table()%>%
    .[[1]]-> table
  return(table)
}

getDataSet <- function(pages)
{
  dataComplete <- NULL
  for (page in 1:pages) 
   {
     tempTable <- getTable(page)
     dataComplete <- rbind(dataComplete,tempTable)
   }
  return(dataComplete)
}


