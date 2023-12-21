library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(readxl)
library(RColorBrewer)

retail <- read_excel('Online_Retail.xlsx')
retail <- retail[complete.cases(retail), ]

retail %>% mutate(Description = as.factor(Description))
retail %>% mutate(Country = as.factor(Country))

retail$Date <- as.Date(retail$InvoiceDate)
retail$Time<- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))

transactionData <- ddply(retail,c("InvoiceNo","Date", "Time"), function(df1)paste(df1$Description, collapse = ","))

transactionData$InvoiceNo <- NULL
transactionData$Time <- NULL
transactionData$Date <- NULL

#Rename column to items
colnames(transactionData) <- "items"

write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = FALSE)

tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')

itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), main="Relative Item Frequency Plot")

# Min Support as 0.002, confidence as 0.95.
association.rules <- apriori(tr, parameter = list(supp=0.002, conf=0.95,maxlen=10))

apriori_results <- inspect(association.rules[1:10])

write.csv(apriori_results,"apriori_results_q.csv", quote = FALSE, row.names = FALSE)