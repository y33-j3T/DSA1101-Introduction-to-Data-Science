## Plot logistic curve

logistic = function(z) {
  exp(z)/(1+exp(z))
}

z = seq(-10,10,0.1);
plot(z, logistic(z), xlab="z", ylab="f(z)", lty=1, type='l')

## Customer churn example

churn = read.csv("churn.CSV")
head(churn)

Churn_logistic <- glm (Churned~Age + Churned_contacts, data=churn, family=binomial(link="logit"))
summary(Churn_logistic)


