# Nurse Scheduling Problem

# PROGRAM CONSTANTS
UNAVAILABLE <- 0
LEAST_DESIRE <- 1
MOST_DESIRE <- 10
SHIFT_ID_LOWER <- 100000
SHIFT_ID_UPPER <- 999999


# load generated data

load("~/ws-r/nsp/gen1.RData")

# generate variables
p <- ncol(init1)
s <- nrow(init1)
g <- ncol(init2)

# produce workable matrix

work.mat <- matrix(data = FALSE, nrow = sum(init2), ncol = p)




slot.names <- list()
for(i in 1:s){
  slot.names <- c(slot.names, colnames(init2)[rep(1:g, times = init2[i, ])])
}
slot.names <- unlist(slot.names)
slot.ref.names <- rep(rownames(init2), times = rowSums(init2))
slot.names <- make.unique(paste0(slot.ref.names, slot.names))
slot.names









































