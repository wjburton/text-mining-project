

# Make API call data from food

library(jsonlite)

keys<- c('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',
         'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',
         'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',
         'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',
         'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',
         'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',
         'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX', 
         'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',
         'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',
         'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',
         'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',
         'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')

#sort can either be done by score (r) or trending (t)
#google drive app for windows, can sync folders
#


call_f2f_api_recipe <- function(search_term = "", sort = "", page = 1, api_key = ""){
  sort <- ifelse(sort %in%c('r','t'), sort, 'r')
  sort <- paste0('&sort=',sort)
  page <- ifelse(is.numeric(page),page,1)
  page <- paste0('&page=',page)
  search_term <- paste(unlist(strsplit(search_term," ")), collapse = '%20')
  search_term <- paste0('&q=',search_term)
  
  
  #This api link will pull names of recipes
  api_recipe_url <- 'http://food2fork.com/api/search?key='
  
  
  request <- paste0(api_recipe_url, api_key, search_term, sort, page)
  
  recipe_df <- fromJSON(request, simplifyDataFrame = T)$recipes

  return(recipe_df)
}




call_f2f_api_ingredient <- function(recipe_id, key, ingredient_only = TRUE){
  
  #this api link will grab the ingredients associated with a recipe ID
  api_ingredient_url <- 'http://food2fork.com/api/get?key='
  
  recipe_id <- paste0('&rId=',recipe_id)
  
  #make url that will pull the correct recipe
  request <- paste0(api_ingredient_url,key,recipe_id)
  
  ingredient_list <- fromJSON(request, simplifyDataFrame = T)$recipe
  if(is.null(ingredient_list)){
    return(NULL)
    break
  }
  ingredient_list <- lapply(ingredient_list, function(x) paste(x, collapse = '_|_'))
  ingredient_df <- data.frame(matrix(unlist(ingredient_list), nrow=1, byrow=T),stringsAsFactors=FALSE)
  names(ingredient_df) <- names(ingredient_list)
  if(ingredient_only == TRUE){
    ingredient_df <- data.frame(recipe_id = ingredient_df$recipe_id, 
                                ingredients = ingredient_df$ingredients)
  }
  return(ingredient_df)
}




#Collect recipe data
#the final page, when collected on 10/7/2016 was 4853
page <- c(1:6000)
index <- 1
#all_recipes <- NULL
for(i in page){
  index <- ifelse(i/index >= 500, index + 1,index)
  recipe_df <- call_f2f_api_recipe(page = i, api_key = keys[index])
  recipe_df$page <- i
  all_recipes <- rbind(all_recipes,recipe_df)
  print(i)
}



write.csv(all_recipes,'all_recipes.csv', row.names = FALSE)


#collect ingredient data using all keys
all_recipes <- read.csv('../data/all_recipes.csv', stringsAsFactors = F)
all_recipes <- all_recipes[!duplicated(all_recipes$recipe_id),]
all_ingredients <- read.csv('../data/all_ingredients.csv', stringsAsFactors = F)
set.seed(1)
all_recipes <- all_recipes[sample(1:nrow(all_recipes),50000),]
remaining_recipes <- all_recipes$recipe_id[!(all_recipes$recipe_id %in% all_ingredients$recipe_id)]
i = 1
for(recipe_id in remaining_recipes){
    ingredient <- call_f2f_api_ingredient(recipe_id, keys[i])
    
    if(is.null(ingredient)){
      i <- i + 1
      key <- keys[i]
      print('Next key')
      ingredient <- call_f2f_api_ingredient(recipe_id, key)
    }
    
    all_ingredients <- rbind(all_ingredients, ingredient)
    print(recipe_id)
}




write.csv(all_ingredients, 'all_ingredients.csv', row.names = F)



# Below I attempted to create a food list by pulling foods from USDA
# .... This was not successful. The food list was pretty messy
##################################################################################
##

#Use the USDA api to pull food names
#data.gov api key 49KRSr3dQCQnAPPmHYKpXcz7NFmxpPHUUfgUzz3


#food api pull
food_api <- function(q, max, offset){
  base_url <- 'http://api.nal.usda.gov/ndb/search/?format=json&q='
  end_of_request <- '&sort=n&api_key=49KRSr3dQCQnAPPmHYKpXcz7NFmxpPHUUfgUzz3l'
  url <- paste0(base_url,q,'&offset=',offset,'&max=',max, end_of_request)
  recipe_df <- fromJSON(url, simplifyDataFrame = T)$list$item
  return(recipe_df)
}

foods <- seq(from = 0, to = 1400000, by = 1500)

food_df <- NULL
for(offset in foods[21:200]){
  foods <- food_api('', 1500, offset)
  food_df <- rbind(food_df,foods)
}

food_df <- read.csv('usda_data.csv')
library(dplyr)
library(tm)
food_df %>% 
  filter(!(group %in% c('Branded Food Products Database',
                        'Fast Foods', 'Snacks', 'Restaurant Foods',
                        'Sweets','Breakfast Cereals'))) -> foods

food_names <- foods$name
food_names <- paste(food_names, collapse = ' ')
food_names <- clean_ingredients(food_names)
#food_corpus <- Corpus(VectorSource(food_names))
food_corpus <- create_recipe_corpus(food_names)
food_tf <- data.frame(as.matrix(TermDocumentMatrix(food_corpus)))
food_tf <- tibble::rownames_to_column(food_tf, 'food') %>% arrange(desc(X1))
