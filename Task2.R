#install.packages("tidyr")
require(cluster)
require(ggplot2)
require(tidyr)
require(dplyr)
require(rvest)
require(magrittr)
require(dendextend)
#install.packages("gplots")
require(gplots)
install.packages("DendSer")
require(dendser)

# a. 

voteData <- votes.repub


lapply(colnames(voteData),function(x)as.numeric(gsub("X","",x)))-> date
data.frame(date)->date

voteData <- rbind(voteData, date)

voteData <- as.data.frame(t(voteData))

voteData%>%
  gather(State,Percentage, Alabama:Wyoming)-> voteDataLong

colnames(voteDataLong)[1] <- "Date"

voteDataLong%>%
  filter(., !is.na(Percentage))%>%
  ggplot(.,aes(Date,Percentage))+
  geom_line(aes(colour = State)) -> graph2a
graph2a
# sehr unübersichtliche Darstellung, da zu viele Linien in einem Plot; zu viele ähnliche Farbnuancen

#b.
url <- "http://www.cdc.gov/std/stats11/census.htm"
url%>%
  read_html()%>%
  html_nodes("table")%>%
  html_table()%>%
  .[[1]]%>%
  gather(Region, State, West:Northeast)%>%
  filter(State != "")%>%
  arrange(State)%>%
  filter(State != "District of Columbia")-> regionTable

#unique(voteDataLong[,"State"]) == regionTable[,"State"]
voteDataLong%>%
  inner_join(.,regionTable)->voteDataRegion

voteDataRegion%>%
  filter(., !is.na(Percentage))%>%
  ggplot(.,aes(Date,Percentage))+
  geom_line(aes(colour = State))+
  facet_grid(Region~.)-> graph2b1
graph2b1

voteDataRegion%>%
  group_by(Region,Date)%>%
  summarise(meanPercent = mean(Percentage,na.rm = T))->voteDataAggr

voteDataAggr%>%
  filter(., !is.na(meanPercent))%>%
  ggplot(.,aes(Date,meanPercent))+
  geom_line(aes(colour = Region))-> graph2b2
graph2b2

# c.
distances= dist(select(voteDataRegion,-State,-Region))
hc= hclust(distances, method = "complete")
plot(hc)
labels=cutree(hc,4)
table(labels,voteDataRegion$Region)
hc= as.dendrogram(hc)
dend1 <-color_branches(hc, k = 4)
plot(dend1)

distances= dist(select(voteDataRegion,-State,-Region))
hc= hclust(distances, method = "average")
plot(hc)
labels=cutree(hc,4)
table(labels,voteDataRegion$Region)
hc= as.dendrogram(hc)
dend1 <-color_branches(hc, k = 4)
plot(dend1)

voteDataRegion%>%
  filter(!is.na(Percentage))%>%
  select(-State,-Region)%>%
  dist(.)%>%
  hclust(.,method = "complete")-> clust
clust = as.dendrogram(clust)
dend <- color_branches(clust, k = 4)
plot(dend)

voteDataRegion%>%
  filter(State=="Alabama")%>%
  select(-State, -Region)%>%
  dist(.)->matrix1

summary(matrix1)

voteDataRegion%>%
  filter(State=="Alabama")%>%
  filter(!is.na(Percentage))%>%
  select(-State, -Region)%>%
  dist(.)->matrix2
summary(matrix2)

# d.
voteMatrix <- as.matrix(votes.repub)
heatmap.2(voteMatrix)
# now let's spice up the dendrograms a bit:
Rowv  <- voteMatrix %>% dist %>% hclust %>% as.dendrogram %>%
  set("branches_k_color", k = 4) %>% set("branches_lwd", 4) %>%
  ladderize
#    rotate_DendSer(ser_weight = dist(x))
Colv  <- voteMatrix %>% t %>% dist %>% hclust %>% as.dendrogram %>%
  set("branches_k_color", k = 4) %>% set("branches_lwd", 4) %>%
  ladderize
#    rotate_DendSer(ser_weight = dist(t(x)))

heatmap.2(voteMatrix, Rowv = Rowv)
          #, Colv = Colv)
