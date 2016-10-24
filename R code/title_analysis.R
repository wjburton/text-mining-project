
#recipe titles 
library(tm)
library(ggplot2)

#Read in data
all_recipes <- read.csv('../data/all_recipes.csv', stringsAsFactors = F)
all_recipes <- all_recipes[!duplicated(all_recipes$title),]

#Clean the ingredients column
clean_titles <- clean_text(all_recipes$title)


#Create a text corpus of all the recipe titles
all_titles <- paste(clean_titles, collapse = '')
title_word_freq <- table(unlist(strsplit(all_titles, split = " ")))
title_word_freq <- title_word_freq[!(names(title_word_freq) %in% c(stopwords(),'amp'))]
title_word_freq <- data.frame(title_word_freq)
names(title_word_freq) <- c('word', 'freq')
title_word_freq <- title_word_freq %>% arrange(desc(freq))



ggplot(title_word_freq[1:30,], aes(x=reorder(word,freq), y=freq)) + 
  geom_point(size=5, colour="red") + coord_flip() +
  ggtitle("Top 30 Recipe Title Word Counts") +
  xlab('Ingredient')+
  ylab('Frequency')+
  theme(axis.text.x=element_text(size=13,face="bold", colour="black"), 
        axis.text.y=element_text(size=13,colour="black",
                                 face="bold"), axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14,face="bold"),
        plot.title=element_text(size=24,face="bold"))


#identify titles that contain the major food groups, does any term correspond to a higher 
#social score
titles <- strsplit(clean_titles, split = ' ')
chicken <- sapply(titles, function(x) 'chicken' %in% x)
chocolate <- sapply(titles, function(x) 'chocolate' %in% x)
sweet <- sapply(titles, function(x) 'sweet' %in% x)
bacon <- sapply(titles, function(x) 'bacon' %in% x)
cheese <- sapply(titles, function(x) 'cheese' %in% x)
all_recipes$chicken <- chicken
all_recipes$chocolate <- chocolate
all_recipes$sweet <- sweet
all_recipes$bacon <- bacon
all_recipes$cheese <- cheese
#It doesn't look like there is a very significant association between food names
# and 

mean(all_recipes$social_rank[all_recipes$chicken == 1])
mean(all_recipes$social_rank[all_recipes$chicken == 0])
mean(all_recipes$social_rank[all_recipes$chocolate == 1])
mean(all_recipes$social_rank[all_recipes$chocolate == 0])
mean(all_recipes$social_rank[all_recipes$sweet == 1])
mean(all_recipes$social_rank[all_recipes$sweet == 0])
mean(all_recipes$social_rank[all_recipes$bacon == 1])
mean(all_recipes$social_rank[all_recipes$bacon == 0])
mean(all_recipes$social_rank[all_recipes$cheese == 1])
mean(all_recipes$social_rank[all_recipes$cheese == 0])








