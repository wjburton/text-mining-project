#association analysis 

#we are using the association analysis package 'arules'
library(arules)
library(dplyr)


#Start by reading in the food lists, then read in a csv containing recipes.
# we clean the recipes by only keeping words that exist in the food list:

#read in food csv files (contains a list of foods that we'd like to keep for
# the analysis 
food_list1 <- read.csv('../data/food_list.csv', stringsAsFactors = F)
food_list2 <- read.csv('../data/michael_term_list.csv', stringsAsFactors = F)

#only keep single words .. The method i'm using only looks for single word matches
food_list2 <- food_list2[-grep(' ', food_list2)]
food_list <- c(food_list1, food_list2)
food_list <- unlist(unique(food_list))
food_list <- tm::stemDocument(food_list)


#read in a file that has each ingredient in a recipe as a different column.
# This will be used as a transactional dataset, but first it needs to be cleaned
# to convert things like '12 chicken breasts' to 'chicken'
assoc <- read.csv('../data/assoc.csv', stringsAsFactors = F)


#select the ingredient columns
ingreds <- apply(assoc[,11:length(assoc)], 2, clean_text )

#Clean the single ingredients, stem them 
# We are looping over each column and then over each ingredient vector in this section
ingreds <- apply(ingreds, 2, function(text){
  x <- strsplit(text, split = " ")
  x <- lapply(x, function(y) tm::stemDocument(y))
  new_text <- sapply(x, function(x) unique(x[x %in% food_list])[1])
  return(new_text)
})

#write the cleaned ingredients to a csv
write.csv(ingreds,'ingred_transactions.csv', row.names= F)




#read in ingred_transactions
ingredients <- read.transactions("ingred_transactions.csv", sep = ",")

#calculate lift support and confidence on the transactional dataset
rules <- apriori(ingredients, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))

#inspect rules extracts the association rules
associations <- inspect(rules)

#this just adds a name to the one of the columns that didn't have a name
names(associations)[2] <- 'arrow'

#arrange the associations by lift
associations %>% 
  arrange(desc(lift)) -> final

head(final,20)
