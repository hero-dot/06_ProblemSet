install.packages("tidyr")
require(cluster)
require(ggplot2)
require(tidyr)
require(dplyr)
require(rvest)
require(magrittr)


# a. 

voteData <- votes.repub


lapply(colnames(voteData),function(x)as.numeric(gsub("X","",x)))-> date
data.frame(date)->date

voteData <- rbind(voteData, date)

voteData <- as.data.frame(t(voteData))

voteData%>%
  gather(State,Percentage, Alabama:Wyoming)-> voteDataWide

colnames(voteDataWide)[1] <- "Date"

voteDataWide%>%
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
    .[[1]]-> regionTable

regionTable
