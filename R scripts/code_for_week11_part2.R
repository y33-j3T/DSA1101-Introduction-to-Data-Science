churn = read.csv("churn.CSV")
head(churn)

Churn_logistic1 <- glm (Churned~Age + Married + Cust_years +
Churned_contacts, data=churn,
family=binomial(link="logit"))

summary(Churn_logistic1)

###AUC###
#Uncomment the code below to install 'ROCR' package if you haven't already
#install.packages('ROCR')

library(ROCR)
pred = predict(Churn_logistic1, type="response")

predObj = prediction(pred, churn$Churned )
rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc")
plot(rocObj, main = paste("Area under the curve:",
round(aucObj@y.values[[1]] ,4)))


# extract the alpha(threshold), FPR, and TPR values from rocObj
alpha <- round(as.numeric(unlist(rocObj@alpha.values)),4)
fpr <- round(as.numeric(unlist(rocObj@x.values)),4)
tpr <- round(as.numeric(unlist(rocObj@y.values)),4)
# adjust margins and plot TPR and FPR
par(mar = c(5,5,2,5))
plot(alpha,tpr, xlab="Threshold", xlim=c(0,1),
ylab="True positive rate", type="l")
par(new="True")
plot(alpha,fpr, xlab="", ylab="", axes=F, xlim=c(0,1), type="l" )
axis(side=4)
mtext(side=4, line=3, "False positive rate")
text(0.18,0.18,"FPR")
text(0.58,0.58,"TPR")