# Nurse Scheduling Problem

save.location = "~/ws-r/nsp/gen9.RData"

# PROGRAM CONSTANTS
UNAVAILABLE <- 0
LEAST_DESIRE <- 1
MOST_DESIRE <- 10
SHIFT_ID_LOWER <- 100000
SHIFT_ID_UPPER <- 999999

# TESTING CONSTANTS
PEOPLE_LOWER <- 12
PEOPLE_UPPER <- 25
SHIFT_LOWER <- 5
SHIFT_UPPER <- 15
GROUP_LOWER <- 3
GROUP_UPPER <- 5
SHIFT_WEIGHT_MIN <- 1
SHIFT_WEIGHT_MAX <- 5
MIN_MIN <- 2
MIN_MAX <- 5
ADD_MIN <- 12
ADD_MAX <- 40
UNAVAILABLE_RATIO <- .1 # .7 to 1 ratio of being unavailable, high number mean very unavailable

# TESTING CONSTANTS gen5
PEOPLE_LOWER <- 5
PEOPLE_UPPER <- 7
SHIFT_LOWER <- 4
SHIFT_UPPER <- 6
GROUP_LOWER <- 1
GROUP_UPPER <- 3
SHIFT_WEIGHT_MIN <- 1
SHIFT_WEIGHT_MAX <- 5
MIN_MIN <- 1
MIN_MAX <- 6
ADD_MIN <- 35
ADD_MAX <- 57
UNAVAILABLE_RATIO <- .1 # .7 to 1 ratio of being unavailable, high number mean very unavailable

# TESTING CONSTANTS gen6
PEOPLE_LOWER <- 3
PEOPLE_UPPER <- 5
SHIFT_LOWER <- 2
SHIFT_UPPER <- 4
GROUP_LOWER <- 1
GROUP_UPPER <- 3
SHIFT_WEIGHT_MIN <- 1
SHIFT_WEIGHT_MAX <- 5
MIN_MIN <- 1
MIN_MAX <- 6
ADD_MIN <- 35
ADD_MAX <- 47
UNAVAILABLE_RATIO <- .1 # .7 to 1 ratio of being unavailable, high number mean very unavailable

# TESTING CONSTANTS gen7 seed 5
PEOPLE_LOWER <- 6
PEOPLE_UPPER <- 7
SHIFT_LOWER <- 2
SHIFT_UPPER <- 4
GROUP_LOWER <- 1
GROUP_UPPER <- 3
SHIFT_WEIGHT_MIN <- 1
SHIFT_WEIGHT_MAX <- 5
MIN_MIN <- 1
MIN_MAX <- 6
ADD_MIN <- 35
ADD_MAX <- 47
UNAVAILABLE_RATIO <- .1 # .7 to 1 ratio of being unavailable, high number mean very unavailable

# TESTING CONSTANTS gen9 seed 8
PEOPLE_LOWER <- 3
PEOPLE_UPPER <- 5
SHIFT_LOWER <- 4
SHIFT_UPPER <- 8
GROUP_LOWER <- 2
GROUP_UPPER <- 3
SHIFT_WEIGHT_MIN <- 1
SHIFT_WEIGHT_MAX <- 5
MIN_MIN <- 1
MIN_MAX <- 6
ADD_MIN <- 35
ADD_MAX <- 47
UNAVAILABLE_RATIO <- .1 # .7 to 1 ratio of being unavailable, high number mean very unavailable

#gen 8 seed 7

# generate practice inputs

set.seed(8)
p <- sample(PEOPLE_LOWER:PEOPLE_UPPER, size = 1)
s <- sample(SHIFT_LOWER:SHIFT_UPPER, size = 1)
g <- sample(GROUP_LOWER:GROUP_UPPER, size = 1)

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

init2 <- cbind(replicate(g, rpois(s, 1.5)))
colnames(init2) <- paste0(LETTERS[1:g])
rownames(init2) <- shift.names

# init3
# bool matrix of colomns groups and rows 'workers'
# specifying whether the 'worker' can do shifts of type [group]
# all are in group A to help generator create vaulable data

init3 <- cbind(replicate(g, sample(c(TRUE, FALSE), size = p, replace = TRUE)))
init3[ , 1] <- TRUE
colnames(init3) <- paste0(LETTERS[1:g])
rownames(init3) <- paste0("P", 1:p)


# init4
# matrix of rows 'workers'
# coloumns min, max, optimal total worked for time period

init4 <- cbind(replicate(2, runif(p, min = MIN_MIN, max = MIN_MAX)))
init4[ ,2] <- init4[ ,2] + runif(p, min = ADD_MIN, max = ADD_MAX)
init4 <- cbind(init4, ((init4[ , 2] - init4[ , 1]) * runif(p, min = 0, max = 1)) + init4[ , 1])

# weights
# shift weights

weights <- runif(s, min = SHIFT_WEIGHT_MIN, max = SHIFT_WEIGHT_MAX)
names(weights) <- shift.names

# save files for loading into program
save(init1, init2, init3, init4, weights, file = save.location)






































