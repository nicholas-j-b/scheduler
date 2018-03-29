# Nurse Scheduling Problem

# license?

# warning when row in init3 is all false
# optimums lie between min and max
# all(init4[, 1] <= init4[,3] & init4[,2] >= init4[,3])
# perhaps don't slit slots of same group eg slot1 needs 2 A's and a B
# then slot1A needs two TRUEs in work.mat row and slot1B needs one

# PROGRAM CONSTANTS
UNAVAILABLE <- 0
LEAST_DESIRE <- 1
MOST_DESIRE <- 10
SHIFT_ID_LOWER <- 100000
SHIFT_ID_UPPER <- 999999

# PARAMETER MULTIPLIER
work.opt.multiplier <- 100 # should maybe depend on s or p?

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

#########################
# establish check.mat
# check.mat is a lookup table that has value true when worker is available and in correct group

# person availability / person in group using check.mat
# generate at begin (only once)
group.mat <- apply(init3, MARGIN = 1, FUN = function(x) x[slot.names])
times.vec <- unlist(lapply(rownames(init1), FUN = function(x) length(grep(x, slot.titles))))
rep.vec <- rep(1:s, times = times.vec)
desires.mat <- (init1)[rep.vec, ]
availabilities.mat <- desires.mat != 0
check.mat <- availabilities.mat & group.mat
rownames(check.mat) <- slot.titles

expanded.weights <- rep(weights, times = times.vec)

############################
# get initial work.mat

# random init
work.mat <- t(replicate(sum(init2), sample(c(rep(FALSE, times = p - 1), TRUE), size = p, replace = FALSE)))


#########################
# check permissibility
# a value between 0 and 1 : 1 is given only to permissible solutions

# all entries in work.mat fit in check.mat
fit.check.mat <- sum(work.mat & check.mat) / sum(work.mat)

# check only 1 slot / person / shift
fit.slot.lim <- sum(apply(work.mat, MARGIN = 2, FUN = tapply, INDEX = rep.vec, sum) <= 1) / (p * s)

# check between min and max
weight.slots.worked <- expanded.weights %*% work.mat
fit.min.max <- sum((init4[ ,1] < weight.slots.worked) & (weight.slots.worked < init4[ ,2])) / p

# permissibility rating
permissibility <- (fit.check.mat + fit.slot.lim + fit.min.max) / 3


###########################
# evaluate

# triangle density calculated with opt
score <- sum(ifelse(weight.slots.worked < init4[ , 3], (weight.slots.worked - init4[ , 1])/(init4[ , 3] - init4[ , 1]), 
       (init4[ , 2] - weight.slots.worked)/(init4[ , 2] - init4[ , 3]))) * work.opt.multiplier

score = score + sum(desires.mat * work.mat)


###########################
# get neighbour

# all

# without swap

row.to.slide <- 1 # vector, how this is generated will depend on control flow #TODO
potential.cols <- (1:p)[-which(work.mat[row.to.slide, ])]
# one by one set each entry in potential.cols to TRUE
# implementation depends on control flow

# with swap

rows.to.swap <- 1:(length(slot.titles))
# potential.swaps <- i #loop# :length(slot.titles)
# swap



















































