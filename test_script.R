# Nurse Scheduling Problem

# PROGRAM CONSTANTS
UNAVAILABLE <- 0
LEAST_DESIRE <- 1
MOST_DESIRE <- 10
SHIFT_ID_LOWER <- 100000
SHIFT_ID_UPPER <- 999999

# TESTING CONSTANTS
PEOPLE_LOWER <- 5
PEOPLE_UPPER <- 15
SHIFT_LOWER <- 5
SHIFT_UPPER <- 15
GROUP_LOWER <- 3
GROUP_UPPER <- 5
SHIFT_WEIGHT_MIN <- 1
SHIFT_WEIGHT_MAX <- 5
MIN_MIN <- 2
MIN_MAX <- 5
ADD_MIN <- 8
ADD_MAX <- 20

# generate practice inputs

set.seed(1)
p <- sample(PEOPLE_LOWER:PEOPLE_UPPER, size = 1)
s <- sample(SHIFT_LOWER:SHIFT_UPPER, size = 1)
g <- sample(GROUP_LOWER:GROUP_UPPER, size = 1)

shift.names <- sample(SHIFT_ID_LOWER:SHIFT_ID_UPPER, size = s)

# init1
# a matrix with colomns of 'workers' and rows 'shifts'
# 0 in cell expresses worker is unavailable for the shift
# non 0 is the desire of the worker to do shift from LEAST_DESIRE to MOST_DESIRE

init1 <- cbind(replicate(p, sample(c(UNAVAILABLE, LEAST_DESIRE:MOST_DESIRE), size = s, replace = TRUE)))
colnames(init1) <- paste0("P", 1:p)
rownames(init1) <- shift.names

# init2
# matrix with columns of groups and rows 'shifts'
# each cell expresses total number of workers from that group needed for that shift
# if a shift could use a worker from group A or B then create a new group C and 
# classify all A's and B's additionally as C's

init2 <- cbind(runif(s, min = SHIFT_WEIGHT_MIN, max = SHIFT_WEIGHT_MAX), replicate(g, rpois(s, 1.5)))
colnames(init2) <- c("WEIGHT", paste0(LETTERS[1:g]))
rownames(init2) <- shift.names

# init3
# bool matrix of colomns groups and rows 'workers'
# specifying whether the 'worker' can do shifts of type [group]

init3 <- cbind(replicate(g, sample(c(TRUE, FALSE), size = p, replace = TRUE)))
colnames(init3) <- paste0(LETTERS[1:g])
rownames(init3) <- paste0("P", 1:p)

# init4
# matrix of rows 'workers'
# coloumns min, max, optimal total worked for time period

init4 <- cbind(replicate(2, runif(p, min = MIN_MIN, max = MIN_MAX)))
init4[ ,2] <- init4[ ,2] + runif(p, min = ADD_MIN, max = ADD_MAX)
init4 <- cbind(init4, ((init4[ , 2] - init4[ , 1]) * runif(p, min = 0, max = 1)) + init4[ , 1])








































