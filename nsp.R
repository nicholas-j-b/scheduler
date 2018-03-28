# Nurse Scheduling Problem

# warning when row in init3 is all false
# optimums lie between min and max
# all(init4[, 1] <= init4[,3] & init4[,2] >= init4[,3])

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
# generate at begin
group.mat <- apply(init3, MARGIN = 1, FUN = function(x) x[slot.names]) # maybe R isn't so bad afterall
times.vec <- unlist(lapply(rownames(init1), FUN = function(x) length(grep(x, slot.titles))))
rep.vec <- rep(1:s, times = times.vec)
desires.mat <- (init1)[rep.vec, ]
availabilities.mat <- desires.mat != 0
check.mat <- availabilities.mat & group.mat
rownames(check.mat) <- slot.titles

expanded.weights <- rep(weights, times = times.vec)

#########################
# check permissibility

# all entries in work.mat fit in check.mat
sum(work.mat & check.mat) == sum(work.mat)

# check only 1 slot / person / shift
all(apply(work.mat, MARGIN = 2, FUN = tapply, INDEX = rep.vec, sum) <= 1)

# check min / max / optimal

weight.slots.worked <- expanded.weights %*% work.mat

# check between min and max
(init4[ ,1] < weight.slots.worked) & (weight.slots.worked < init4[ ,2])

###########################
# evaluate

# triangle density calculated with opt
score <- sum(ifelse(weight.slots.worked < init4[ , 3], (weight.slots.worked - init4[ , 1])/(init4[ , 3] - init4[ , 1]), 
       (init4[ , 2] - weight.slots.worked)/(init4[ , 2] - init4[ , 3]))) * work.opt.multiplier

score = score + sum(desires.mat * work.mat)

############################
# get initial work.mat

# random init
work.mat <- 
  rbind(replicate(sample(c(rep(FALSE, times = p - 1), TRUE), size = p, replace = FALSE), times = sum(init2)))



















































