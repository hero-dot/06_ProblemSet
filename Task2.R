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
#install.packages("DendSer")
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
voteMatrix <- as.matrix(votes.repub)
voteMatrix %>% dist %>% hclust(.,method = "complete") %>% as.dendrogram %>%
  set("branches_k_color", k = 4) %>% set("branches_lwd", 2) %>% ladderize -> dend1
plot(dend1)

dend2 <- voteMatrix %>% dist %>% hclust(.,method = "average") %>% as.dendrogram %>%
  set("branches_k_color", k = 4) %>% set("branches_lwd", 2) %>% ladderize
plot(dend2)

# Distance between Alaska and California with NA values 
testSet1 <- rbind(voteMatrix["Alaska",],voteMatrix["California",])
dist(testSet1)

# Distance between Alaska and California with 0 instead of NA
testSet2temp <- mapply(function(x) ifelse(is.na(x),0,x),votes.repub)
row.names(testSet2temp) <- row.names(votes.repub)
testSet2 <- rbind(testSet2["Alaska",],testSet2["California",])
dist(testSet2)

# As seen the distance increases, if the NA values are replaced  by a zero. Which in turn changes the 
# clutering. 

# Just for the curious mind, how the dendogram looks without NA's
dend3 <- testSet2temp %>% dist %>% hclust(.,method = "complete") %>% as.dendrogram %>%
  set("branches_k_color", k = 4) %>% set("branches_lwd", 2) %>% ladderize
plot(dend3)

dend4 <- testSet2temp %>% dist %>% hclust(.,method = "average") %>% as.dendrogram %>%
  set("branches_k_color", k = 4) %>% set("branches_lwd", 2) %>% ladderize
plot(dend4)

# d.
heatmap.2(voteMatrix)
heatmap.2(voteMatrix, Rowv = dend1, dendrogram = "row")

# e. 
# The first three clusters are more or less aligned with the census regions. However, they don't contain all
# the states of the corresponding region. The fourth cluster contains the missing states and is therefore not
# aligned with the census region. 
