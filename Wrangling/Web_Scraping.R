#Import Murder Statistics from Wikipedia into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

#Import Guacamole Recipe from Food Network
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle_a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo_a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients_a-Ingredient") %>% html_text()

guacamole <- list(recipe, prep_time, ingredients)
guacamole

get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle_a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo_a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients_a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
}
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

#Assessment 1 - Baseball
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
length(nodes) #Determine How total number of tables harvested
html_text(nodes[[8]])
html_table(nodes[[8]])

tab_1 <- html_table(nodes[[10]])
tab_1$X1 <- NULL
tab_1 <- tab_1[-c(1),]
tab_1 <- setNames(tab_1, c("Team", "Payroll", "Average"))
tab_1

tab_2 <- html_table(nodes[[19]])
tab_2 <- tab_2[-c(1),]
tab_2 <- setNames(tab_2, c("Team", "Payroll", "Average"))
tab_2

tab_combined <- full_join(tab_1, tab_2, "Team")
tab_combined #Discrepancy in team name formatting precludes complete merge

#Assessment 2 - Brexit
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
tab <- html_nodes(h, "table")
length(tab)
length(html_table(tab[[5]], fill = TRUE))
html_table(tab[[5]], fill = TRUE)