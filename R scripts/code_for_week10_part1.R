install.packages('arules')
install.packages('arulesViz')
library('arules')
library('arulesViz')

data(Groceries)

Groceries@itemInfo[1:10,]
Groceries@data[,100:110]

apply(Groceries@data[,100:105], 2,
      function(r) paste(Groceries@itemInfo[r,"labels"], collapse=", "))

itemsets <- apriori(Groceries, parameter=list(minlen=1, maxlen=1,
support=0.02, target="frequent itemsets"))

summary(itemsets)

inspect(head(sort(itemsets, by = "support"), 10))

itemsets <- apriori(Groceries, parameter=list(minlen=3, maxlen=3,
support=0.02, target="frequent itemsets"))

summary(itemsets)
inspect(sort(itemsets, by ="support"))


itemsets <- apriori(Groceries, parameter=list(minlen=4, maxlen=4,
support=0.02, target="frequent itemsets"))

summary(itemsets)

inspect(sort(itemsets, by ="support"))> itemsets <- apriori(Groceries, parameter=list(minlen=1, support=0.02,
target="frequent itemsets"))

summary(itemsets)

rules <- apriori(Groceries, parameter=list(support=0.001,
confidence=0.6, target = "rules"))


inspect(head(sort(rules, by="lift"), 3))
highLiftRules <- head(sort(rules, by="lift"), 5)
plot(highLiftRules, method="graph", control=list(alpha=1))



