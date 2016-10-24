#text analysis
library(tm)
library(lsa)
library(ggplot2)
library(tibble)
library(dplyr)
library(topicmodels)


################################################################################
#                                                                              #
#       additional functions used in this script are listed in the order       #
#              they appear below in the utils R file                           #
#                                                                              #
################################################################################

#Read in data
df <- read_in_recipe_data()


#Clean the ingredients column
clean_recipes <- clean_ingredients(df$ingredients)
df <- df[names(clean_recipes),]

#do this temporarily to see results
set.seed(100)
rand_samp <- sample(1:length(clean_recipes), 1000)
clean_recipes <- clean_recipes[rand_samp]
clean_recipes <- clean_recipes[clean_recipes != ""]
sampled_df <- df[names(clean_recipes),] %>% select(-c(f2f_url, source_url, 
                                                      image_url, page,
                                                      publisher_url))


#Create a text corpus of all the recipe ingredients
recipe_corpus <- create_corpus(clean_recipes)


#Create a document term frequency for the ingredients 
all_recipes <- paste(clean_recipes, collapse = '')
title_word_freq <- table(unlist(strsplit(all_recipes, split = " ")))
title_word_freq <- title_word_freq[!(names(title_word_freq) %in% c(stopwords(),'amp'))]
title_word_freq <- data.frame(title_word_freq)
names(title_word_freq) <- c('ingredient', 'freq')
title_word_freq <- title_word_freq %>% arrange(desc(freq))

#look at the 30 mot frequent and least frequent items
ggplot(title_word_freq[1:30,], aes(x=reorder(ingredient,freq), y=freq)) + 
  geom_point(size=5, colour="red") + coord_flip() +
  ggtitle("Recipe Popularity of Top 30 Ingredients") +
  xlab('Ingredient')+
  ylab('Frequency')+
  theme(axis.text.x=element_text(size=13,face="bold", colour="black"), 
        axis.text.y=element_text(size=13,colour="black",
                                 face="bold"), axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14,face="bold"),
        plot.title=element_text(size=24,face="bold"))



#create similarity matrix
temp <- as.matrix(TermDocumentMatrix(recipe_corpus))
similarity_cos <- cosine(temp)
similarity_cos <- 1 - similarity_cos
recipe_cluster <- kmeans(similarity_cos, centers = 20)

#create clusters from the ingredients
sampled_df$cluster <- recipe_cluster$cluster
sampled_df$ingredients <- clean_recipes

#look at 5 tightest clusters
most_similar <- order(recipe_cluster$withinss)[1:5]
head(sampled_df[sampled_df$cluster == most_similar[1],c(3,5,6)],60)