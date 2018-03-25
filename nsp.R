#Nurse Scheduling Problem

#PROGRAM CONSTANTS
UNAVAILABLE <- 0
LEAST_DESIRE <- 1
MOST_DESIRE <- 10
SHIFT_ID_LOWER <- 100000
SHIFT_ID_UPPER <- 999999

#TESTING CONSTANTS
PEOPLE_LOWER <- 5
PEOPLE_UPPER <- 15
SHIFT_LOWER <- 5
SHIFT_UPPER <- 15

#generate practice inputs

set.seed(1)
p <- sample(PEOPLE_LOWER:PEOPLE_UPPER, size = 1)
s <- sample(SHIFT_LOWER:SHIFT_UPPER, size = 1)


init1 <- cbind(sample(SHIFT_ID_LOWER:SHIFT_ID_UPPER, size = s, replace = TRUE), 
               replicate(p, sample(c(UNAVAILABLE, LEAST_DESIRE:MOST_DESIRE), size = s, replace = TRUE)))


