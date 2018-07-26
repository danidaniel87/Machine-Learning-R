##### Association Rules -------------------
####Step 1 – collecting data####

## Example: Identifying Frequently-Purchased Groceries ----
## Step 2: Exploring and preparing the data ----

# load the grocery data into a sparse matrix
library(arules)
groceries <- read.transactions("C:/Users/Solutio/Desktop/Machine Learning with R/groceries.csv", sep = ",")
summary(groceries)

# look at the first five transactions
inspect(groceries[1:5])

# examine the frequency of items
itemFrequency(groceries[, 1:3])

# plot the frequency of items
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

# a visualization of the sparse matrix for the first five transactions
image(groceries[1:5])

# visualization of a random sample of 100 transactions
image(sample(groceries, 100))

## Step 3: Training a model on the data ----
library(arules)

# default settings result in zero rules learned
apriori(groceries)

# set better support and confidence levels to learn more rules
#The support of an itemset or rule measures how frequently it occurs in the data.
#A rule's confidence is a measurement of its predictive power or accuracy.
  #the confidence tells us the proportion of transactions where the presence
  #of item or itemset X results in the presence of item or itemset Y.
groceryrules <- apriori(groceries, parameter = list(support =
                            0.006, confidence = 0.25, minlen = 2))
groceryrules

## Step 4: Evaluating model performance ----
# summary of grocery association rules
summary(groceryrules)

# look at the first three rules
inspect(groceryrules[1:3])

## Step 5: Improving model performance ----

# sorting grocery rules by lift
inspect(sort(groceryrules, by = "lift")[1:5])

# finding subsets of rules containing any berry items
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

# writing the rules to a CSV file
write(groceryrules, file = "Rulesgroceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# converting the rule set to a data frame
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)