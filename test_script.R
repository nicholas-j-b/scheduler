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

# generate practice inputs

set.seed(1)
p <- sample(PEOPLE_LOWER:PEOPLE_UPPER, size = 1)
s <- sample(SHIFT_LOWER:SHIFT_UPPER, size = 1)
g <- sample(GROUP_LOWER:GROUP_UPPER, size = 1)

# init1
# a matrix with colomns of 'workers' and rows 'shifts'
# 0 in cell expresses worker is unavailable for the shift
# non 0 is the desire of the worker to do shift from LEAST_DESIRE to MOST_DESIRE

init1 <- cbind(replicate(p, sample(c(UNAVAILABLE, LEAST_DESIRE:MOST_DESIRE), size = s, replace = TRUE)))

colnames(init1) <- paste0("P", 1:p)
shift.names <- sample(SHIFT_ID_LOWER:SHIFT_ID_UPPER, size = s)
rownames(init1) <- shift.names

# init2
# matrix with columns of groups and rows 'shifts'
# each cell expresses total number of workers from that group needed for that shift
# if a shift could use a worker from group A or B then create a new group C and 
# classify all A's and B's additionally as C's

init2 <- cbind(runif(s, min = SHIFT_WEIGHT_MIN, max = SHIFT_WEIGHT_MAX), replicate(g, rpois(s, 1.5)))
init2


























































