
sample <- read.table("sample1.csv",header=TRUE,sep=",")
head(sample)
dim(sample)

traindata <- as.data.frame(sample[1:14,])
testdata <- as.data.frame(sample[15,])
testdata

tprior <- table(traindata$Enrolls)
tprior
tprior <- tprior/sum(tprior)
tprior

ageCounts <- table(traindata[,c("Enrolls", "Age")])
ageCounts
ageCounts <- ageCounts/rowSums(ageCounts)
ageCounts


incomeCounts <- table(traindata[,c("Enrolls", "Income")])
incomeCounts <- incomeCounts/rowSums(incomeCounts)
incomeCounts


jsCounts <- table(traindata[,c("Enrolls", "JobSatisfaction")])
jsCounts <- jsCounts/rowSums(jsCounts)
jsCounts

desireCounts <- table(traindata[,c("Enrolls", "Desire")])
desireCounts <- desireCounts/rowSums(desireCounts)
desireCounts



prob_yes <-
ageCounts["Yes",testdata[,c("Age")]]*
incomeCounts["Yes",testdata[,c("Income")]]*
jsCounts["Yes",testdata[,c("JobSatisfaction")]]*
desireCounts["Yes",testdata[,c("Desire")]]*
tprior["Yes"]
prob_no <-
ageCounts["No",testdata[,c("Age")]]*
incomeCounts["No",testdata[,c("Income")]]*
jsCounts["No",testdata[,c("JobSatisfaction")]]*
desireCounts["No",testdata[,c("Desire")]]*
tprior["No"]


library(e1071)

model <- naiveBayes(Enrolls ~
Age+Income+JobSatisfaction+Desire,
traindata, laplace=0)

results <- predict(model,testdata)
results




