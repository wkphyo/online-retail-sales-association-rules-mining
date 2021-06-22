rm(list=ls())

library('arules')
library('backports')
library('zeallot')
library('arulesViz')
library('dplyr')
library('stringr')
library('chron')

df <- read.csv('OnlineRetail.csv', stringsAsFactors = FALSE)

View(df)
str(df)
summary(df)

sum(is.null(df$InvoiceNo))  # Sum of all records with null InvoiceNo

xrows <- 0
for(i in 1:nrow(df)){
  if(grepl('C', df[i, 'InvoiceNo'])){
    xrows[i] <- i
  }
}

xrows <- xrows[!is.na(xrows)]  # Drop rows with NA; keep only valid row numbers

df <- df[-xrows, ]

df$Description <- trimws(df$Description)  # Remove trailing and leading spaces

df$Description <- gsub(" ", "_", df$Description)

df$InvoiceDate <- as.Date(df$InvoiceDate, format = "%m/%d/%Y")

eire <- df[df$Country == 'EIRE', ]
View(eire)

write.csv(eire, file = '2021-cleaned-eire.csv', row.names = FALSE)

eire <- read.transactions(
  '2021-cleaned-eire.csv', 
  format = c('single'),
  header = TRUE,
  rm.duplicates = FALSE,
  cols = c('InvoiceNo', 'StockCode'),
  sep = ','
)

inspect(eire[1:2])

# Explore support count to determine min value
itemFrequencyPlot(eire, support = 0.1)
itemFrequencyPlot(eire, topN = 15)

# Get rules
eire.rules <- apriori(
  eire, 
  parameter = list(
    confidence = 0.6,
    support = 0.08,
    minlen = 2
  )
)

inspect(eire.rules)
summary(eire.rules)

df.eire.rules <- as(eire.rules, 'data.frame')
View(df.eire.rules)

plot(
  eire.rules, 
  method = 'matrix',
)

subset.rules = eire.rules[quality(eire.rules)$lift > 5]
View(subset.rules)
inspect(subset.rules)
