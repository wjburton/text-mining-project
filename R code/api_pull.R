

# Make API call data from food

library(jsonlite)

keys<- c('02ae1ac9a6bad199327f3b1e03e840b8',
         '9257d002012de5a486d0b86abd697f14',
         'b0a17dc4f0ecdd50b251f421f52cc942',
         '093255a649a121e5566b75bb70e4cb5e',
         'e8267530675c57b5197179cd2425cf53',
         '4829942e0ad4f61c3474666f549a7140',
         '6f083f7fc112171a32510b9351c788b1', 
         'd06e7250674884a2cc2731c8a49456a3',
         '1e422d544325f08bc314151c3c652b1b',
         '0c305016af2fa2a1dd04436e3118249a',
         '743b7fdb8d9f056c67e444278f3087aa',
         'fab83ae18ef040ed193764a8b698d614',
         'c4855c7983f6a738b5a49ca1111d8741',
         'a3defceab1d4840b2bbd79f01d70927f',
         '2c2b0ccd621d3811c699478c4ad1756e',
         'a4ec4326cf9f6645c6c5ac70bb08893d',
         'f59d8bd9809714310db2ef9666f86d17',
         '57246440589fe5cad71f760ce200e81f',
         'd08804d4c6c1bd9f336b30098db50a6c',
         'd6d630c1e10671449e774948b51b899d',
         '1331631dc7e0ca7d28650e63871f5104', 
         '25716d5fa5df151504ad7abf42a32736',
         '71f305eee40649b415ecb2882c647013',
         '9bdc40a9e9cff234dfa653578211a146')

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
