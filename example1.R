

load("~/ws-r/nsp/test_data_1_1.RData")


greedy.local <- local.local <- anneal.anneal <- list()
number.of.trials <- 10


for(i in 1:number.of.trials){
  greedy.local[[i]] <- find.solution(init1, init2, init3, init4, weights, init.process = "greedy" , 
                             algorithm = "local.search", tolerance = 1)
}


for(i in 1:number.of.trials){
  local.local[[i]] <- find.solution(init1, init2, init3, init4, weights, init.process = "local.search", 
                             algorithm = "local.search", tolerance = 1)
}


for(i in 1:number.of.trials){
  anneal.anneal[[i]] <- find.solution(init1, init2, init3, init4, weights, init.process = "simulated.annealing" , 
                             algorithm = "simulated.annealing", tolerance = 1)
}

# set graphic colours
cols <- c("goldenrod3", "royalblue3", "olivedrab3")
cols2 <- c("goldenrod3", "royalblue3")
cols3 <- colorRampPalette(c("white", "orangered3"))

# extract success of init function
local.search.init.p <- sum(unlist(sapply(local.local, `[`, "init.permissibility")) == 1) / number.of.trials
simulated.annealing.init.p <- sum(unlist(sapply(anneal.anneal, `[`, "init.permissibility")) == 1) / number.of.trials
greedy.search.init.p <- sum(unlist(sapply(greedy.local, `[`, "init.permissibility")) == 1) / number.of.trials

# plot success of init funciton
barplot(c(local.search.init.p, simulated.annealing.init.p, greedy.search.init.p),
        names.arg = c("local search", "simulated annealing", "greedy search" ),
        main = "Proportion of Initialisations with Permissible Solutions", col = cols,
        ylab = "Proportion of trials")

# extract time of init function 
local.search.init.t <- unlist(sapply(local.local, `[`, "init.time"))[(1:number.of.trials) * 5 - 4]
simulated.annealing.init.t <- unlist(sapply(anneal.anneal, `[`, "init.time"))[(1:number.of.trials) * 5 - 4]
greedy.search.init.t <- unlist(sapply(greedy.local, `[`, "init.time"))[(1:number.of.trials) * 5 - 4]

# plot time of init function
barplot(c(local.search.init.t, simulated.annealing.init.t, greedy.search.init.t),
        main = "Times taken for Initialisation Processes", col = cols[rep(1:3, each = number.of.trials)],
        ylab = "Time in Seconds")

# extract scores
local.search.opt.score <- unlist(sapply(local.local, `[`, "score"))
simulated.annealing.opt.score <- unlist(sapply(anneal.anneal, `[`, "score"))

# plot scores
barplot(c(local.search.opt.score, simulated.annealing.opt.score),
        main = "Scores", col = cols2[rep(1:2, each = number.of.trials)],
        ylab = "Scores")

# extract times of optimisation function
local.search.opt.t <- unlist(sapply(local.local, `[`, "optimise.time"))[(1:number.of.trials) * 5 - 4]
simulated.annealing.opt.t <- unlist(sapply(anneal.anneal, `[`, "optimise.time"))[(1:number.of.trials) * 5 - 4]

# plot times of optimisation function
barplot(c(local.search.opt.t, simulated.annealing.opt.t),
        main = "Times Taken for Optimisation Process", col = cols2[rep(1:2, each = number.of.trials)],
        ylab = "Time in seconds")

# must have correct init files loaded
# create matrix for heatmap of init1 showing worker preferences
init1.expanded <- init1[rep(1:4, times = rowSums(init2)), ]
rownames(init1.expanded) <- rownames(local.local[[1]]$solution)

# plot parameter for heatmaps
par(oma = c(1, 1, 1, 1))

# plot init1 preferences
heatmap(init1.expanded, Rowv = NA, Colv = NA, col = cols3(10), main = "Worker Preferences as Expressed in 'init1'",
        xlab = "Workers")

# plot first annealing solution
heatmap(anneal.anneal[[1]]$solution * 1, Rowv = NA, Colv = NA, col = cols3(2), main = "Example of Final Timetable",
        xlab = "Workers")
mtext("Shift Names", 4)

# prepare mats for heatmap plots
local.mat <- anneal.mat <- matrix(0, ncol = ncol(local.local[[1]]$solution), nrow = nrow(local.local[[1]]$solution))

for(i in 1:length(local.local)){
  local.mat <- store.mat + local.local[[i]]$solution
}
for(i in 1:length(anneal.anneal)){
  anneal.mat <- anneal.mat + anneal.anneal[[i]]$solution
}

# plot heatmaps of solutions discovered
heatmap(local.mat, Rowv = NA, Colv = NA, col = cols3(7), main = "Distribution of Local Search Solutions",
        xlab = "Workers")
mtext("Shift Names", 4)

heatmap(anneal.mat, Rowv = NA, Colv = NA, col = cols3(7), main = "Distribution of Simulated Annealing Solutions",
        xlab = "Workers")
mtext("Shift Names", 4)












