#Create an urn with 2 red and 3 blue beads then draw one at random
beads <- rep(c("red", "blue"), times = c(2,3))
sample(beads, 1)

#Construct a Monte Carlo Simulation by repeating the draw 10,000 times
B <- 10000
events <- replicate(B, sample(beads, 1))
tab <- table(events)
tab #View Count Table
prop.table(tab) #View Table of Outcome Proportions

#Draw beads with replacement (without is the default condition)
events <- sample(beads, B, replace = TRUE)
prop.table(table(events))

#Set the Random Number Seed for Consistent Responses in Online Coursework
set.seed(1)
set.seed(1, sample.kind="Rounding") #Forces R3.6 to generate R3.5 seed for HarvardX

#using mean() to determine probability of logical vector
mean(beads=="blue")

#Constructing a Deck of Cards#
suits <- c("Diamond", "Clubs", "Hears", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

#Probability of drawing a King
kings <- paste("King", suits)
mean(deck %in% kings)

#Permutation - Drawking a second King after one was already drawn
library(gtools)
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)
sum(first_card %in%  kings & second_card %in% kings) / sum(first_card %in% kings)
#Equivalent Method using Probabilities instead of totals
mean(first_card %in% kings & second_card %in% kings) / mean(first_card %in% kings)

#Combination - Probability of natural 21 in Blackjack (Ace and King is equivalent to King and Ace)
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52,2, v=deck)
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

#Estimating the above combination calculation with a Monte Carlo Simulation
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

#Checking for duplicated birthdays in a n person group
#Monte Carlo Simulation with B = 10000 Replicates
compute_prob <- function(n, B = 10000) {
same_day <- replicate(B, {
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays)) #logical function returning TRUE if any birthdays in group are duplicated
})
mean(same_day) #Calculates proportion of groups with duplicated birthdays
}
n <- seq(1,60)

#Computing exact probability using sapply
prob <- sapply(n, compute_prob)
exact_prob <- function(n) {
  prob_unique <- seq(365, 365-n+1)/365
  1-prod(prob_unique) #calculates shared probability by subtracting the probability of no shared birthdays from one
}
eprob <- sapply(n, exact_prob)

#Plot Monte Carlo simulation against mathematical probability
plot(n, prob)
lines(n, eprob, col = "red")

#Monte Carlo Simulation with varied values of B; what is the minimum required number of experiments?
B <- 10^seq(1, 5, len = 100)
#Illustrates how many experiments are required for an estimate of shared birthdays among 22 people to stabilize
compute_prob <- function(B, n = 22){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
prob <- sapply(B, compute_prob)
plot(log10(B), prob, type = "l")

#Sporting Event Simulation
B <- 10000
celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games=="win")
})
mean(celtic_wins)

#Monte Caro Simulation of Monte Hall Problem (Sticking)
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1) #Open door that neither has prize nor is chosen
  stick <- my_pick
  stick == prize_door
})
  mean(stick)
  
#Monte Caro Simulation of Monte Hall Problem (Switch)
B <- 10000
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1) #Open door that neither has prize nor is chosen
  switch <- doors[!doors %in% c(my_pick, show)] #Switch to door that wasn't chosen first or opened
  switch == prize_door
})
mean(switch)

#Simulating Six Game Championship Possibilities
# Assign a variable 'n' as the number of remaining games.
n <- 6
# Assign a variable `outcomes` as a vector of possible game outcomes
outcomes <- c(0, 1)
# Assign a variable `l` to a list of all possible outcomes in all remaining games
l <- rep(list(outcomes), n)
# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities <- expand.grid(l)
# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins
results <- rowSums(possibilities)
# Calculate the proportion of 'results' in which the Cavs win the series
mean(results>=4)

#Monte Carlo Simulation of Six Game Championship
B <- 10000
results <- replicate(B, {
  games <- sample(c(1, 0), 6, replace = TRUE, C(.5, .5))
})
win_count <- colSums(results)
mean(win_count>=4)

#Monte Carlo Simulation of Jamaican Sweep in Olympic Race
set.seed(1)
B <- 10000
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
sweep <- replicate(B, {
  winners <- sample(runners, 3)
  (winners[1] %in% "Jamaica" & winners[2] %in% "Jamaica" & winners[3] %in% "Jamaica")
})
mean(sweep)

#Determining the number of unique menu options
entree <- combinations(6, 1)
sides <- combinations(6, 3)
drink <- combinations(3, 1) 
menu_options <- prod(nrow(entree), nrow(sides), nrow(drink))
menu_options

n <- seq(2, 12)
select_meal <- function(n) {
  prod(nrow(combinations(n,1)), nrow(sides), nrow(drink))
}
menu <- sapply(n, select_meal)
plot(n, menu)