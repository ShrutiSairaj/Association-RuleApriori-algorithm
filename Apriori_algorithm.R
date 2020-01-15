#install.packages(arules)
#install.packages("arules")
library(arules)


data("Groceries")
inspect(Groceries[1:5]) #inspect is to summarize all relevant options, plots and statistics that should be usually considered.
df_ip <- read.csv("C:\\One-Drive\\OneDrive - Tredence\\Walmart ET Merchant Track\\Text Analytics\\groceries.csv")

getwd()
setwd("C:\\Users\\shruti.sairaj\\Desktop\\Shruti\\March\\Openends")
itemFrequencyPlot(Groceries,topN=20,type="absolute") 
#Load the data
openends_1 <- read.transactions("cleaned_openends.csv", sep = " ", rm.duplicates = TRUE)
View(openends_1)

#Sparse Matrix summary
summary(openends_1)

x<-inspect(openends_1)
itemFrequencyPlot(openends,topN=20,type="absolute")

image(sample(openends, 10000))
rules <- apriori(openends,parameter = list(supp = 0.001, conf = 0.75, minlen = 2))
rules
summary(rules)
inspect(rules)

#Rules for specific Items
#rules1 <- apriori(openends_1,parameter = list(supp = 0.0001, conf = 0.2, minlen = 2),appearance = list(rhs = "checkout"))
#rules1 <- apriori(openends_1,parameter = list(supp = 0.001, conf = 0.75, minlen = 2),appearance = list(rhs = "checkout"))
rules1 <- apriori(openends_1,parameter = list(supp = 0.001, conf = 0.15, minlen = 2),appearance = list(default="rhs",lhs="return"))
rules1 <- apriori(openends_1,parameter = list(supp = 0.0001, conf = 0.14, minlen = 3),appearance = list(default="rhs",lhs="checkout"))
rules1 <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.15,minlen=2), appearance = list(default="rhs",lhs="whole milk"), control = list (verbose=F))
#rules1 <- apriori(openends_1,parameter = list(supp = 0.001, conf = 0.75, minlen = 3),appearance = list(rhs = "checkout"))
rules1 <- apriori(openends_1,parameter = list(supp = 0.001, conf = 0.9, minlen = 3),appearance = list(rhs = "checkout"))




View(rules1)
#Sorting
inspect(sort(rules1, by = "lift")[1:5])
#Write
op <- write(rules1,file = "new_checkout_01.csv", sep = ",", quote  = TRUE, row.names = FALSE)

#Convert as dataframe
df <- as(rules1, "data.frame")
dfsplit <-  data.frame(lhs = labels(lhs(rules1), setStart = "", setEnd = ""), 
                       rhs = labels(rhs(rules1), setStart = "", setEnd = ""),
                       c = quality(rules1))

write.csv(dfsplit, "return_keywords_1.csv")

#plot graph
library(arulesViz)
plot(rules1[1:10],
     
     method = "graph",
     
     control = list(type = "items"))
