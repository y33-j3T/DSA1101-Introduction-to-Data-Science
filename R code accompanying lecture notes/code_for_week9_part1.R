resale=read.csv("HDBresale_cluster.csv")
head(resale)

plot(x=resale$floor_area_sqm, y=resale$amenities,
xlab="Floor area in sqm", ylab="Number of amenities", col="red")

kout <- kmeans(resale[,c("floor_area_sqm","amenities")],
      centers=2)

plot(resale$floor_area_sqm, 
     resale$amenities, 
     col=kout$cluster,
     xlab="Floor area in sqm", 
     ylab="Number of amenities")