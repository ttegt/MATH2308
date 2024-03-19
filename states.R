#this script requires 4 packages. If you don't have them
#installed (check your package list), you can install them 
#here by uncommenting (remove the #)
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("maps")
#You can usually ignore the warnings when packages are
#installed or loaded

library(ggplot2)
library(dplyr)
library(stringr)
library(maps)

#A look at the data and the map dataframe
View(state.x77)
states_map<-map_data("state")
View(states_map)


#'fix' states_map issues
states_map$region<-str_to_title(states_map$region)
states_map2<-subset(states_map,region!="District Of Columbia")

#k-means clustering on income and illiteracy from the state.x77 dataframe
set.seed(123)
state_kmeans<-kmeans(scale(state.x77[,1:2]),centers=3)
state_kmeans 

#create dataframe for clustering
state_clusters<-data.frame(region=rownames(state.x77),
                           cluster=as.factor(state_kmeans$cluster))


#combine data frames
cluster_map<-left_join(states_map2,state_clusters,by="region")

#map it
g<-ggplot(cluster_map,aes(long,lat,group=group))+
  geom_polygon(aes(fill=cluster),color="white")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Contiguous US states, clustered by Population and Income")+
  theme(axis.ticks=element_blank(),
        axis.text=element_blank(),
        plot.title = element_text(size=8,hjust=0.5))
  
g
