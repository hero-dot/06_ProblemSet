require(rvest)
require(magrittr)
require(dplyr)
#install.packages("dendextend")
require(dendextend)

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

# Beer styles are in numbers encoded
# Weizen/Weissbier - 1
# American IPA - 2
# German Pilsner (Pils) - 3
# Irish Red Ale - 4
getDataSet <- function(firstPg,lastPg)
{
  dataComplete <- NULL
  for (page in firstPg:lastPg) 
   {
     tempTable <- getTable(page)
     dataComplete <- rbind(dataComplete,tempTable)
   }
  dataComplete%>%
    filter(Style=="Weizen/Weissbier"|Style=="American IPA"|Style=="German Pilsner (Pils)"|Style=="Irish Red Ale")%>%
    mutate(Style = ifelse(.$Style == "Weizen/Weissbier",1,
                          ifelse(.$Style == "American IPA",2,
                                 ifelse(.$Style == "German Pilsner (Pils)",3,
                                        ifelse(.$Style == "Irish Red Ale",4,NA)))))-> dataComplete
  
  return(dataComplete)
}

brewData1 <- getDataSet(1,100)
brewData2 <- getDataSet(101,200)
brewData3 <- getDataSet(201,300)
brewData <- rbind(brewData1,brewData2,brewData3)
write.csv2(brewData, file = "brewData.csv")

brewData <- read.csv2("brewData.csv")

rownames(brewData)<- make.names(brewData$Name,unique = T)
brewData%>%
  select(-Name,-X)-> brewData

# 1.a 
PCAbrew = prcomp(brewData,scale = T)
#data gets scaled
summary(PCAbrew) #summary stats
PCAbrew$rotation #loadings
biplot(PCAbrew)
screeplot(PCAbrew,type="lines")

#
# DISKUSSION!!!
#
# Die ersten drei Komponenten erklären ca.90% der Varianz. 
#
#

# 1.b 

kMeansBrewer <- kmeans(select(brewData,-Style),4,nstart = 5)

plot(newiris[c("Sepal.Length",
               "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length",
                     "Sepal.Width")], col=1:3, pch=8, cex=2)
table(kMeansBrewer$cluster,brewData$Style)

#
#
#
#
#

# 1.c
# Change style back to a factor variable
newBrewData = brewData
newBrewData$Style= NULL
distances= dist(brewData)
hc= hclust(distances)
plot(hc)
labels=cutree(hc,4)
table(labels,brewData$Style)
hc= as.dendrogram(hc)
dend1 <-color_branches(hc, k = 3)
plot(dend1)
