require(rvest)
require(magrittr)
require(dplyr)

getTable <- function(page)
{
  url <- paste0("https://www.brewtoad.com/recipes?page=",page,"&sort=rank&view_as_table=true")
  url%>%
    read_html()%>%
    html_nodes("table")%>%
    html_table()%>%
    .[[1]]%>%
    mutate(ABV = as.numeric(gsub("%","",.$ABV)))%>%
    select(-Brewer,-Rating)-> table
  return(table)
}
Test <- getTable(1)

# Beer styles are in numbers encoded
# Weizen/Weissbier - 1
# American IPA - 2
# German Pilsner (Pils) - 3
# Irish Red Ale - 4

getDataSet <- function(pages)
{
  dataComplete <- NULL
  for (page in 1:pages) 
   {
     tempTable <- getTable(page)
     dataComplete <- rbind(dataComplete,tempTable)
     dataComplete%>%
       filter(Style=="Weizen/Weissbier"|Style=="American IPA"|Style=="German Pilsner (Pils)"|Style=="Irish Red Ale")%>%
       mutate(Style = ifelse(.$Style == "Weizen/Weissbier",1,
                             ifelse(.$Style == "American IPA",2,
                                    ifelse(.$Style == "German Pilsner (Pils)",3,
                                           ifelse(.$Style == "Irish Red Ale",4,NA)))))-> dataComplete
     rownames(dataComplete)<- dataComplete$Name
     dataComplete%>%
       select(-Name)-> dataComplete
     
    }
  return(dataComplete)
}

brewerData <- getDataSet(1)

# 1.a 

PCAbrewer = prcomp(brewerData,scale = T)
#data gets scaled
summary(PCAbrewer) #summary stats
PCAbrewer$rotation #loadings
biplot(PCAbrewer)
screeplot(PCA,type="lines")

#
# DISKUSSION!!!
#
# Die ersten drei Komponenten erklären ca.90% der Varianz. 
#
#

# 1.b 

kMeansBrewer <- kmeans(select(brewerData,-Style),4,nstart = 5)

plot(newiris[c("Sepal.Length",
               "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length",
                     "Sepal.Width")], col=1:3, pch=8, cex=2)
table(kMeansBrewer$cluster,brewerData$Style)

#
#
#
#
#

# 1.c 

