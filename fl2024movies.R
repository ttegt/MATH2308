library(ggplot2)
library(ggrepel)
movies<-read.csv("MoviesFL2024.csv")
View(movies)

for(i in 2:ncol(movies)){
    movies[is.na(movies[,i]), i] <- mean(movies[,i], na.rm = TRUE)
}

movies$Cluster<-as.factor(kmeans(movies[,2:11],centers=3)$cluster)
movieplot<-ggplot(movies, aes(Twisters, Inside.Out.2,color=Cluster))+
    geom_point(color="blue", size=2)+
    geom_label_repel(aes(label = User),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'black')
movieplot

users<-movies$User
moviesm<-movies[,2:11]
moviest<-as.data.frame(t(moviesm))
names(moviest)[1:28]<-users

moviest$Cluster<-as.factor(kmeans(moviest[,1:27],centers=3)$cluster)

movietplot<-ggplot(moviest, aes(andre, Sebastian,color=Cluster))+
    geom_point(color="blue", size=2)+
    geom_label_repel(aes(label = rownames(moviest)),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'black')
movietplot
