## Code for market basket analysis :

## A Little math : 
#Support: The fraction of which our item set occurs in our dataset.
#Confidence: probability that a rule is correct for a new transaction with items on the left.
#Lift: The ratio by which by the confidence of a rule exceeds the expected confidence. 

install.packages("arules")
install.packages("arulesViz")
install.packages("datasets")
# Load the libraries
library(arules)
library(arulesViz)
library(datasets)

# Load the data set
data(Groceries)


# Create an item frequency plot for the top 20 items
# This gives the items with highest number of occurances in the data
itemFrequencyPlot(Groceries,topN=20,type="absolute")

# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:10])
