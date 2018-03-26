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

slot.names.list <- list()
for(i in 1:s){
  slot.names.list <- c(slot.names.list, colnames(init2)[rep(1:g, times = init2[i, ])])
}
slot.names <- unlist(slot.names.list)
slot.ref.names <- rep(rownames(init2), times = rowSums(init2))
slot.titles <- make.unique(paste0(slot.ref.names, slot.names))

rownames(work.mat) <- slot.titles

# desire.mat <- matrix(0, nrow = sum(init2), ncol = p)


#########################
# establish check.mat
# check.mat is a lookup table that has value true when worker is available and in correct group

# person availability / person in group using check.mat
# generate at begin
group.mat <- apply(init3, MARGIN = 1, FUN = function(x) x[slot.names]) # maybe R isn't so bad afterall
times.vec <- unlist(lapply(inames, FUN = function(x) length(grep(x, wnames))))
rep.vec <- rep.vec <- rep(1:s, times = times.vec)
availabilities.mat <- (init1 != 0)[rep.vec, ]
check.mat <- availabilities.mat & group.mat
rownames(check.mat) <- slot.titles

expanded.weights <- rep(weights, times = times.vec)

# all entries in work.mat fit in check.mat
sum(work.mat & check.mat) == sum(work.mat)

# check only 1 slot / person / shift
all(apply(work.mat, MARGIN = 2, FUN = tapply, INDEX = rep.vec, sum) <= 1)

# check min / max / optimal

weight.slots.worked <- expanded.weights %*% work.mat

# check between min and max
(init4[ ,1] < weight.slots.worked) & (weight.slots.worked < init4[ ,2])



















