# Code written by Nicholas Brooking for the purpose of supplementing a submission
# for the Bachelor Thesis for a Bachelor of Statistics at the University of Vienna

# Code to generate practise data for the accompanying script, 'find-solution.R'
# For more information see the README attached with this download

save.location = "test_data_1_1.RData"

# PROGRAM CONSTANTS
UNAVAILABLE <- 0
LEAST_DESIRE <- 1
MOST_DESIRE <- 10
SHIFT_ID_LOWER <- 100000
SHIFT_ID_UPPER <- 999999


# # TESTING CONSTANTS test_data_1_1
# seed <- 15
# PEOPLE_LOWER <- 7
# PEOPLE_UPPER <- 8
# SHIFT_LOWER <- 4
# SHIFT_UPPER <- 8
# NUM_GROUPS <- 1
# SHIFT_WEIGHT_MIN <- 1
# SHIFT_WEIGHT_MAX <- 5
# GROUP.LIKELIHOOD <- 5
# GROUP.DISLIKELIHOOD <- 2 # ratio with GROUP.LIKELIHOOD: person has x out of x + y chance of being in given group
# PERSON_SHIFT_MIN <- 0.4
# PERSON_SHIFT_MAX <- 2.2
# UNAVAILABLE_RATIO <- .1 # x to 1 ratio of being unavailable, high number mean very unavailable

# # TESTING CONSTANTS test_data_2_1
# seed <- 16
# PEOPLE_LOWER <- 10
# PEOPLE_UPPER <- 12
# SHIFT_LOWER <- 6
# SHIFT_UPPER <- 7
# NUM_GROUPS <- 2
# SHIFT_WEIGHT_MIN <- 1
# SHIFT_WEIGHT_MAX <- 5
# GROUP.LIKELIHOOD <- 5
# GROUP.DISLIKELIHOOD <- 2 # ratio with GROUP.LIKELIHOOD: person has x out of x + y chance of being in given group
# PERSON_SHIFT_MIN <- 0.4
# PERSON_SHIFT_MAX <- 2.2
# UNAVAILABLE_RATIO <- .3 # x to 1 ratio of being unavailable, high number mean very unavailable

# TESTING CONSTANTS test_data_3_1
seed <- 12
PEOPLE_LOWER <- 8
PEOPLE_UPPER <- 9
SHIFT_LOWER <- 4
SHIFT_UPPER <- 5
NUM_GROUPS <- 2
SHIFT_WEIGHT_MIN <- 1
SHIFT_WEIGHT_MAX <- 5
GROUP.LIKELIHOOD <- 5
GROUP.DISLIKELIHOOD <- 2 # ratio with GROUP.LIKELIHOOD: person has x out of x + y chance of being in given group
PERSON_SHIFT_MIN <- 0.4
PERSON_SHIFT_MAX <- 2.2
UNAVAILABLE_RATIO <- .7 # x to 1 ratio of being unavailable, high number means very unavailable

set.seed(seed)

# generate practice inputs

# number of people
p <- sample(PEOPLE_LOWER:PEOPLE_UPPER, size = 1)

# number of shifts
s <- sample(SHIFT_LOWER:SHIFT_UPPER, size = 1)

# number of groups
g <- NUM_GROUPS

# generate shift names - names are irrelevant, need only be unique
shift.names <- sample(SHIFT_ID_LOWER:SHIFT_ID_UPPER, size = s)

# init1
# a matrix with colomns of 'workers' and rows 'shifts'
# 0 in cell expresses worker is unavailable for the shift
# non 0 is the desire of the worker to do shift from LEAST_DESIRE to MOST_DESIRE

init1 <- cbind(replicate(p, sample(c(rep(UNAVAILABLE, times = length(LEAST_DESIRE:MOST_DESIRE) * UNAVAILABLE_RATIO), 
                                     LEAST_DESIRE:MOST_DESIRE), size = s, replace = TRUE))) # cbind does nothing?
colnames(init1) <- paste0("P", 1:p)
rownames(init1) <- shift.names

# init2
# matrix with columns of groups and rows 'shifts'
# each cell expresses total number of workers from that group needed for that shift
# if a shift could use a worker from group A or B then create a new group C and 
# classify all A's and B's additionally as C's

init2 <- replicate(g, rpois(s, .5) + 1)
colnames(init2) <- paste0(LETTERS[1:g])
rownames(init2) <- shift.names

# init3
# bool matrix of colomns groups and rows 'workers'
# specifying whether the 'worker' can do shifts of type [group]
# all are in group A to help generator create vaulable data

init3 <- cbind(replicate(g, sample(c(rep(TRUE, times = GROUP.LIKELIHOOD), 
                                     rep(FALSE, times = GROUP.DISLIKELIHOOD)), size = p, replace = TRUE)))
# all are in group A
init3[ , 1] <- TRUE
colnames(init3) <- paste0(LETTERS[1:g])
rownames(init3) <- paste0("P", 1:p)

# generate shift weights

weights <- runif(s, min = SHIFT_WEIGHT_MIN, max = SHIFT_WEIGHT_MAX)
names(weights) <- shift.names
times.vec <- rowSums(init2)
expanded.weights <- rep(weights, times = times.vec)

# init4
# matrix of rows 'workers'
# coloumns min, max, optimal total worked for time period

init4 <- matrix(0, ncol = 3, nrow = p)

init4[ ,3] <- rpois(p, 10)
init4[ ,3] <- init4[ ,3] * (sum(expanded.weights) / sum(init4[ ,3]))
init4[ ,1] <- init4[ ,3] * PERSON_SHIFT_MIN
init4[ ,2] <- init4[ ,3] * PERSON_SHIFT_MAX

# save files for loading into program
save(init1, init2, init3, init4, weights, file = save.location)










