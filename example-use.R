# Code written by Nicholas Brooking for the purpose of supplementing a submission
# for the Bachelor Thesis for a Bachelor of Statistics at the University of Vienna

# Code to analyse the performance of 'find.solution' as found in 'find-solution-test.R'
# For more information see the README attached with this download

# load test data
# for example:
# load(".../test_data_1_1.RData")

load("test_data_1_1.RData")


greedy.local <- local.local <- anneal.anneal <- list()
number.of.trials <- 10

# run tests
# be sure to use the function from 'find-solution-test.R'
# for(i in 1:number.of.trials){
#   greedy.local[[i]] <- find.solution(init1, init2, init3, init4, weights, init.process = "greedy" ,
#                                      algorithm = "local.search", tolerance = 1)
# }

run = FALSE

if(run == TRUE){
  print("using local search")
  for(i in 1:number.of.trials){
    print(paste("run:", i))
    local.local[[i]] <- find.solution(init1, init2, init3, init4, weights, init.process = "local.search",
                                      algorithm = "local.search", tolerance = 1)
  }

  print("using simulated annealing")
  for(i in 1:number.of.trials){
    print(paste("run:", i))
    anneal.anneal[[i]] <- find.solution(init1, init2, init3, init4, weights, init.process = "simulated.annealing" ,
                                        algorithm = "simulated.annealing", tolerance = 1)
  }
}


# saving recommended
# for example
# save(local.local, anneal.anneal, file = "current_test_data.RData")
load(file = "current_test_data.RData")

# set graphic colours
cols <- c("goldenrod3", "royalblue3", "olivedrab3")
cols2 <- c("goldenrod3", "royalblue3")
cols3 <- colorRampPalette(c("white", "orangered3"))

algorithm.names <- c("Local Search", "Simulated Annealing")

# extract success of init function
local.search.init.p <- sum(unlist(sapply(local.local, `[`, "init.permissibility")) == 1) / number.of.trials
simulated.annealing.init.p <- sum(unlist(sapply(anneal.anneal, `[`, "init.permissibility")) == 1) / number.of.trials
#greedy.search.init.p <- sum(unlist(sapply(greedy.local, `[`, "init.permissibility")) == 1) / number.of.trials

# plot success of init funciton
barplot(c(local.search.init.p, simulated.annealing.init.p),
        names.arg = algorithm.names,
        main = "Proportion of Initialisations with Permissible Solutions", col = cols2,
        ylab = "Proportion of trials")

# extract time of init function
local.search.init.t <- unlist(sapply(local.local, `[`, "init.time"))[(1:number.of.trials) * 5 - 4]
simulated.annealing.init.t <- unlist(sapply(anneal.anneal, `[`, "init.time"))[(1:number.of.trials) * 5 - 4]
#greedy.search.init.t <- unlist(sapply(greedy.local, `[`, "init.time"))[(1:number.of.trials) * 5 - 4]

# plot time of init function
barplot(c(mean(local.search.init.t), mean(simulated.annealing.init.t)),
        main = "Times taken for Initialisation Processes", col = cols2,
        ylab = "Time in Seconds", xlab = "Algorithm", names.arg = algorithm.names)

# extract scores
local.search.opt.score <- unlist(sapply(local.local, `[`, "score"))
simulated.annealing.opt.score <- unlist(sapply(anneal.anneal, `[`, "score"))

# plot scores
barplot(c(mean(local.search.opt.score), mean(simulated.annealing.opt.score)),
        main = "Scores", col = cols2,
        ylab = "Scores", names.arg = algorithm.names)

# extract times of optimisation function
local.search.opt.t <- unlist(sapply(local.local, `[`, "optimise.time"))[(1:number.of.trials) * 5 - 4]
simulated.annealing.opt.t <- unlist(sapply(anneal.anneal, `[`, "optimise.time"))[(1:number.of.trials) * 5 - 4]

# plot times of optimisation function
barplot(c(mean(local.search.opt.t), mean(simulated.annealing.opt.t)),
        main = "Times Taken for Optimisation Process", col = cols2,
        ylab = "Time in seconds", names.arg = algorithm.names)

# must have correct init files loaded
# create matrix for heatmap of init1 showing worker preferences
init1.expanded <- init1[rep(1:nrow(init2), times = rowSums(init2)), ]
rownames(init1.expanded) <- rownames(local.local[[1]]$solution)

# plot parameter for heatmaps
par(oma = c(1, 1, 1, 1))

# plot init1 preferences
heatmap(init1.expanded, Rowv = NA, Colv = NA, col = cols3(10), main = "Worker Preferences",
        xlab = "Workers")#, add.expr = text(1, 1, "White unavailable, darker = more prefered", adj = c(0, 0)))
title(sub = "White = Unavailable, Darker = Prefered", adj = 1, line = 3, font = 1)

# plot first local solution
heatmap(local.local[[2]]$solution * 1, Rowv = NA, Colv = NA, col = cols3(2), main = "Final Timetable Local Search",
        xlab = "Workers")

# plot first annealing solution
heatmap(anneal.anneal[[2]]$solution * 1, Rowv = NA, Colv = NA, col = cols3(2), main = "Final Timetable Simulated Annealing",
        xlab = "Workers")

# prepare mats for heatmap plots
local.mat <- anneal.mat <- matrix(0, ncol = ncol(local.local[[1]]$solution), nrow = nrow(local.local[[1]]$solution))


for(i in 1:length(local.local)){
  local.mat <- local.mat + local.local[[i]]$solution
}
for(i in 1:length(anneal.anneal)){
  anneal.mat <- anneal.mat + anneal.anneal[[i]]$solution
}


# plot heatmaps of solutions discovered
heatmap(local.mat, Rowv = NA, Colv = NA, col = cols3(7), main = "Distribution of Local Search Solutions",
        xlab = "Workers")

heatmap(anneal.mat, Rowv = NA, Colv = NA, col = cols3(7), main = "Distribution of Simulated Annealing Solutions",
        xlab = "Workers")


#y.local <- sort(local.mat)
#y.anneal <- sort(anneal.mat)
#x <- 1:length(y.local)
#
#lo.mod <- loess(y.local~x)
#an.mod <- loess(y.anneal~x)
#
#plot(predict(lo.mod), type = 'l', col = cols2[1])
#lines(predict(an.mod), col = cols2[2])
