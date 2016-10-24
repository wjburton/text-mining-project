#needed funcitons

#Reads in the data needed for analysis
read_in_recipe_data <- function(){
ingredients <- read.csv('../data/all_ingredients.csv', stringsAsFactors = F)
recipes <- read.csv('../data/all_recipes.csv', stringsAsFactors = F)
recipes <- recipes[!duplicated(recipes$recipe_id),]
df <- merge(recipes, ingredients, by = 'recipe_id')
}


# clean only keep terms in the term list
clean_ingredients <- function(ingredients, pred_score = F){
  clean_ingreds <- clean_text(ingredients)
  #read in the food list and stem the words
  food_list <- read.csv('../data/food_list.csv')$unique.x.
  food_list <- Corpus(VectorSource(food_list))
  food_list <- tm_map(food_list, stemDocument)
  food_list <- textreg::convert.tm.to.character(food_list)
  
  #only keep words that appear in the food list
  clean_ingreds <- as.character(clean_ingreds)
  recipe_corpus <- Corpus(VectorSource(clean_ingreds))
  recipe_corpus <- tm_map(recipe_corpus, stemDocument)
  clean_ingreds <- textreg::convert.tm.to.character(recipe_corpus)
  splt_clean_ingreds <- strsplit(clean_ingreds, split = ' ')
  clean_ingreds <- sapply(splt_clean_ingreds, function(x){
    x <- x[x %in% food_list]
    x <- paste(x, collapse = ' ')
    return(x)
  })
  
  if(pred_score == T){
  clean_ingreds <- clean_ingreds[clean_ingreds != ""]
  }
  
  return(clean_ingreds)
}


# cleans the text within an ingredient
clean_text <- function(ingredients, pred_score = F){
  temp <- gsub('\\d',"",ingredients) #remove numbers
  temp <- gsub('[[:punct:]]', " ", temp) # remove special characters
  temp <- gsub(" *\\b[[:alpha:]]{1}\\b *", " ", temp) #remove single letters
  temp <- gsub('\\s+', " ", temp) #remove extra white space
  temp <- tolower(temp) #to lowercase
  return(temp)
}

#Create a corpus of recipes removing stopwords and stemming applied
create_corpus <-function(clean_ingredients){
  recipe_corpus <- Corpus(VectorSource(clean_ingredients))
  recipe_corpus <- tm_map(recipe_corpus, removeWords, stopwords('english'))
  recipe_corpus <- tm_map(recipe_corpus, stemDocument)
  return(recipe_corpus)
}



# This was used to generate the food list we are filtering the data on

#Clean ingredients
#remove numbers, single letters, special characters,
# clean_food_list <- function(food_list){
#   food_list <- food_list[,-1]
#   food_col <- unlist(lapply(food_list, function(x){
#     x <- x[x != ""]
#     x <- unlist(strsplit(x, split = " "))
#   }))
#   food_col <- unique(food_col)
# }
# 

# food_list1 <- read.csv('food_list1.csv',stringsAsFactors = F, header = F)
# food_list2 <-  read.csv('food_list2.csv',stringsAsFactors = F, header = F)
# food_list3 <-  read.csv('food_list3.csv',stringsAsFactors = F, header = F)
# 
# food_list1 <- clean_food_list(food_list1)
# food_list2 <- clean_food_list(food_list2)
# food_list3 <- clean_food_list(food_list3)
# food_list <- unique(c(food_list3, food_list2, food_list1))
# food_list <- data.frame(food_list)
# write.csv(food_list, 'food_list.csv', row.names = F)



