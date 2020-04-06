#Determining the number of unique menu options - INCORRECT...
entrees <- as.character(1:6)
sides <- as.character(1:6)
drinks <- as.character(1:2)

n < - seq(1, 20)
select_meal <- function(n) {
  prod(length((n, 1)), length(combinations(6, 2)), length(combinations(3, 1)))
  } #TWICE AS LARGE AS IT SHOULD BE...
menu <- sapply(n, select_meal)
menu
plot(n, menu)