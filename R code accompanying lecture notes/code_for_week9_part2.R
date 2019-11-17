grade_input = read.csv("grades_km_input.csv")
head(grade_input)

kout <- kmeans(grade_input[,c("English","Math","Science")],centers=3)

kout$withinss


wss <- numeric(15)

for (k in 1:15) { 
   wss[k] <- sum(kmeans(grade_input[,c("English","Math","Science")], 
   centers=k, nstart=25)$withinss)
}


plot(1:15, wss, type="b", xlab="Number of Clusters",  ylab="Within Sum of Squares")
