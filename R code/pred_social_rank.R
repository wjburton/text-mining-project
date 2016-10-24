 
## on this page we try to predict the social rank of a recipe to learn what makes a recipe popular

df <- read_in_recipe_data()
df$ingredients <- clean_ingredients(df$ingredients)


#Variable creation  :We are only creating the number of unique words
n_unique_words <- trimws(gsub('\\d', " ", df$ingredients))
n_unique_words <-  strsplit(n_unique_words, split = ' ')
n_unique_words <- sapply(n_unique_words, function(x) length(unique(x[x != ''])))
df$n_unique_words <- n_unique_words
df$nchar_title <- nchar(df$title)

#we were going to add the cluster a recipe is in.. but they were not significant


samp <- sample(1:nrow(df), nrow(df)*0.6)
train <- df[samp,]
valid <- df[-samp,]
df$publisher <- as.factor(df$publisher)

linear_model <- lm(social_rank ~ n_unique_words + publisher, data = train)
summary(linear_model)


# The only significant variables were the publisher of the recipe and the number of unique terms
#in the recipe