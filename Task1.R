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

rownames(brewData)<- make.names(brewData$Name,unique = T)
brewData%>%
  select(-Name)-> brewData

write.csv2(brewData, file = "brewData.csv", row.names = T)

brewData <- read.csv2("brewData.csv",row.names = 1)

# Alternative with preserved style and no rownames
getDataSetStyle <- function(firstPg,lastPg)
{
  dataComplete <- NULL
  for (page in firstPg:lastPg) 
  {
    tempTable <- getTable(page)
    dataComplete <- rbind(dataComplete,tempTable)
  }
  dataComplete%>%
    filter(Style=="Weizen/Weissbier"|Style=="American IPA"|Style=="German Pilsner (Pils)"|Style=="Irish Red Ale")-> dataComplete
  
  return(dataComplete)
}
brewDataSt1 <- getDataSetStyle(1,100)
brewDataSt2 <- getDataSetStyle(101,200)
brewDataSt3 <- getDataSetStyle(201,300)
brewDataSt <- rbind(brewDataSt1,brewDataSt2,brewDataSt3)
write.csv2(brewData, file = "brewDataSt.csv")
brewData <- read.csv2("brewDataSt.csv")

# 1.a 
PCAbrew = prcomp(select(brewDataSt,-Name,-Style),scale = T,center = T)
#data gets scaled
summary(PCAbrew) #summary stats
PCAbrew$rotation #loadings
biplot(PCAbrew)
screeplot(PCAbrew,type="lines")

# # Die erste Komponente erklärt 56% der Varianz und kann den ersten vier Variablen zugeordnet werden.
# Die Farbe hat den geringsten Einfluss auf die erste Komponente.
# Die ersten drei Komponenten erklären ca.90% der Varianz. 

# 1.b
newBrewData = brewDataSt
newBrewData$Style= NULL
newBrewData$Name = NULL
head(newBrewData)
kMeansBrewer <- kmeans(newBrewData,4,nstart = 5)

table(kMeansBrewer$cluster,brewDataSt$Style)


plot(newBrewData[c("OG",
               "FG")], col=kMeansBrewer$cluster)
points(kMeansBrewer$centers[,c("OG",
                     "FG")], col=1:4, pch=8, cex=2)
# In this manner all pairs of variables could be visualized.
# There would be 10 visualizations in total. 

# 1.c
newBrewData = brewDataSt
newBrewData$Style= NULL
newBrewData$Name = NULL
distances= dist(select(brewDataSt,-Name,-Style))
hc= hclust(distances)
plot(hc)
labels=cutree(hc,4)
table(labels,brewDataSt$Style)
hc= as.dendrogram(hc)
dend1 <-color_branches(hc, k = 4)
plot(dend1)
