# Code written by Nicholas Brooking for the purpose of supplementing a submission
# for the Bachelor Thesis for a Bachelor of Statistics at the University of Vienna

# A function that assists the solving of an NSP
# For more information see the README attached with this download

find.solution <- function(init1, init2, init3, init4, weights, work.opt.multiplier = 1, 
                          algorithm = "simulated.annealing", init.process = "simulated.annealing", 
                          num.temperatures = 21, rand.gen.tolerance = .65,
                          tolerance = .75, greedy.limit = 100,
                          num.neighbours.considered = 10, temp.exponent = 4){
  #------------------------------------------------------------------------------------------------
  # arguments
  
  # init1
  # a matrix with columns of 'workers' and rows 'shifts'
  # 0 in cell expresses worker is unavailable for the shift
  # higher numbers indicate greater preference of worker for the shift
  
  # init2
  # a matrix with columns of groups and rows 'shifts'
  # each cell expresses total number of workers from that group needed for that shift
  # if a shift could use a worker from group A or B then create a new group C and 
  # classify all A's and B's additionally as C's
  
  # init3
  # bool matrix with columns of groups and rows 'workers'
  # specifying whether the 'worker' can do shifts of type [group]
  
  # init4
  # matrix of rows 'workers'
  # columns min, max, optimal total worked for time period
  
  # weights
  # vector with length nrow(init1)
  # 'size' or 'length' of shift
  # perhaps how hours are credited to the worker if they work the shift
  
  # work.opt.multiplier
  # number indicating the importance of the preferences is init1
  
  # algorithm
  # string - algorithm for finding solution, can be "simulated.annealing" or "local.search"
  # algorithm tests against 'evaluate' function
  
  # init.process
  # string - algorithm for generating initial solution
  # can be "simulated.annealing", "local.search", "random" or "greedy"
  # init.process tests against 'check.permissibility' function
  
  # num.temperatures
  # number indicating how many iterations the algorithm 'simulated.annealing' should use
  
  # rand.gen.tolerance
  # number between 0 and 1, indicates the worst possible value for an initial solution if "random"
  # for 'init.process' was chosen
  
  # tolerance
  # number between 0 and 1, indicates the lowest score on permissibility test allowed in solution
  # indicates flexibility of the 'evaluate' function
  
  # greedy.limit
  # number of partial restarts greedy algorithm is allowed when searching for permissible starting point
  
  # num.neighbours.considered
  # number of top neighbours 'simulated.annealing' algorithm could potentially choose
  
  # temp.exponent
  # number expressing the change in function as temperature drops for 'simulated.annealing'
  # a large number indicates 'simulated.annealing' will often choose a better option
  # lower numbers involve more chance, especially noticeable temperatures drop
  
  #------------------------------------------------------------------------------------------------
  # checks
  
  if(!is.matrix(init1)){
    stop("init1 must be matrix")
  }
  if(!is.matrix(init2)){
    stop("init2 must be matrix")
  }
  if(!is.matrix(init3)){
    stop("init3 must be matrix")
  }
  if(!is.matrix(init4)){
    stop("init4 must be matrix")
  }
  if(!is.vector(weights)){
    stop("weights must be vector")
  }
  if(nrow(init1) != nrow(init2)){
    stop("init1 and init2 row lengths differ")
  }
  if(ncol(init1) != nrow(init3)){
    stop("Number of columns in init1 must equal number of rows in init3")
  }
  if(nrow(init4) != nrow(init3)){
    stop("Number of rows in init3 must equal number of rows in init4")
  }
  if(ncol(init4) != 3){
    stop("Number of columns in init4 must be 3: Minimum, Maximum, Optimal")
  }
  if(length(weights) != nrow(init1)){
    stop("Lenght of weights must equal number of rows in init1")
  }
  if(!all(init4[ ,1] <= init4[ ,3] & init4[, 3] <= init4[ ,2])){
    stop("Problem in init4: comparison all(init4[ ,1] <= init4[ ,3] & init4[, 3] <= 
         init4[ ,2]) failed\n init4: Minimum, Maximum, Optimal")
  }
  if(!is.numeric(init1)){
    stop("init1 must be numeric")
  }
  if(!is.numeric(init2)){
    stop("init2 must be numeric")
  }
  if(!is.logical(init3)){
    stop("init1 must be logical")
  }
  if(!is.numeric(init4)){
    stop("init4 must be numeric")
  }
  if(!is.numeric(weights)){
    stop("weights must be numeric")
  }
  if(!all(rowSums(init2) >= 1)){
    stop("A worker is in no group")
  }
  if(!is.numeric(num.temperatures)){
    stop("num.temperatures must be number")
  }
  if(!is.numeric(rand.gen.tolerance)){
    stop("rand.gen.tolerance must be number")
  }
  if(rand.gen.tolerance < 0 || rand.gen.tolerance > 1){
    stop("rand.gen.tolerance must be between 0 and 1")
  }
  if(!is.numeric(tolerance)){
    stop("tolerance must be number")
  }
  if(tolerance < 0 || tolerance > 1){
    stop("tolerance must be between 0 and 1")
  }
  if(!is.numeric(greedy.limit)){
    stop("greedy.limit must be number")
  }
  if(!is.numeric(num.neighbours.considered)){
    stop("num.neighbours.considered must be number")
  }
  if(!is.numeric(temp.exponent)){
    stop("temp.exponent must be number")
  }

  
  #------------------------------------------------------------------------------------------------
  # initialise function
  
  # generate size variables
  p <- ncol(init1) # number of people
  s <- nrow(init1) # number of shifts
  g <- ncol(init2) # number of groups
  sl <- sum(init2) # number of slots
  
  # establish environment
  work <- new.env()
  
  # produce work.mat matrix to store the current solution
  work$mat <- matrix(data = FALSE, nrow = sl, ncol = p)
  slot.names.list <- list()
  for(i in 1:s){
    slot.names.list <- c(slot.names.list, colnames(init2)[rep(1:g, times = init2[i, ])])
  }
  slot.names <- unlist(slot.names.list)
  slot.ref.names <- rep(rownames(init2), times = rowSums(init2))
  slot.titles <- make.unique(paste0(slot.ref.names, slot.names))
  rownames(work$mat) <- slot.titles
  
  #initialise copy for manipulation
  work$mat.copy <- work$mat
  
  # establish check.mat (const)
  # check.mat is a lookup table that has value true where worker is available and in correct group
  group.mat <- apply(init3, MARGIN = 1, FUN = function(x) x[slot.names])
  times.vec <- unlist(lapply(rownames(init1), FUN = function(x) length(grep(x, slot.titles))))
  work$rep.vec <- rep(1:s, times = times.vec)
  work$desires.mat <- (init1)[work$rep.vec, ]
  availabilities.mat <- work$desires.mat != 0
  work$check.mat <- availabilities.mat & group.mat
  rownames(work$check.mat) <- slot.titles
  work$expanded.weights <- rep(weights, times = times.vec)
  
  #------------------------------------------------------------------------------------------------
  # more checks
  
  if(sum(work$expanded.weights) < colSums(init4)[1]){
    stop("Not enough available shifts")
  }
  if(sum(work$expanded.weights) > colSums(init4)[2]){
    stop("Not enough available work time (worker max too low)")
  }
  if(!all(rowSums(check.mat) >= 1)){
    stop("At least one shift has no available worker")
  }
  
  #------------------------------------------------------------------------------------------------
  # generate initial mat with chosen function
  
  gen.random <- function(){
    # start with random mat that has one TRUE per row
    work$mat <- t(replicate(sl, sample(c(rep(FALSE, times = p - 1), TRUE), size = p, replace = FALSE)))
    work$weight.slots.worked <- work$expanded.weights %*% work$mat
    # if randomly generated mat is very poor, start again
    if(check.permissibility() < rand.gen.tolerance){
      gen.random()
    }
    return(TRUE)
  }
  
  gen.local.search <- function(){
    # start with random mat
    gen.random()
    local.search(check.permissibility)
    return(TRUE)
  }
  
  gen.simulated.annealing <- function(){
    # start with random mat
    gen.random()
    simulated.annealing(check.permissibility)
    return(TRUE)
  }
  
  gen.greedy <- function(){
    failure <- TRUE
    # initialise empty mat
    work$mat <- matrix(FALSE, ncol = p, nrow = sl)
    # loop over rows
    i <- 1
    counts <- 1
    while(i <= sl){
      if(i < 1 || counts > greedy.limit){
        stop("greedy algorithm has failed, try another init method or increase greedy.limit")
      }
      # ensure matching to check.mat
      cols <- sample((1:p)[work$check.mat[i, ]])
      for(j in cols){
        # initialise row to be tested
        work$mat[i, ] <- FALSE
        work$mat[i, j] <- TRUE
        
        # check only 1 slot / worker / shift
        if(sum(apply(work$mat, MARGIN = 2, FUN = tapply, INDEX = work$rep.vec, sum) <= 1) == (p * s)){
          # check shifts worked less than max for worker
          work$weight.slots.worked <- work$expanded.weights %*% work$mat
          if(all(work$weight.slots.worked < init4[ ,2])){
            failure <- FALSE
            break
          }
        }
        failure <- TRUE
      }
      # if no solutions for the row can be found, move up one layer and try again
      if(failure){
        work$mat[i, ] <- FALSE
        i <- i - 1
        counts <- counts + 1
      } else {
        i <- i + 1
      }
    }
    return(TRUE)
  }
  
  #------------------------------------------------------------------------------------------------
  # a function to check the permissibility of a matrix
  
  check.permissibility <- function(){
    # a value between 0 and 1 : 1 is given only to permissible solutions
    
    # calculate weight of slots worked for each worker
    work$weight.slots.worked <- work$expanded.weights %*% work$mat
    
    # all entries in work.mat fit in check.mat
    fit.check.mat <- sum(work$mat & work$check.mat) / sl
    
    # check only 1 slot / worker / shift
    fit.slot.lim <- sum(apply(work$mat, MARGIN = 2, FUN = tapply, INDEX = work$rep.vec, sum) <= 1) / (p * s)
    
    # check lower than max
    fit.max <- sum(work$weight.slots.worked < init4[ ,2]) / p
    
    # check greater than min
    fit.min <- sum(init4[ ,1] < work$weight.slots.worked) / p
    
    # permissibility rating
    permissibility <- (fit.check.mat + fit.slot.lim + fit.max + fit.min) / 4
    
    return(permissibility)
  }
  
  #------------------------------------------------------------------------------------------------
  # a function to find the value of a given matrix
  
  evaluate <- function(){
    # check permissibility
    permissibility <- check.permissibility()
    if(permissibility < tolerance){
      return(0)
    }
    
    # calculate nearness to optimum and consider work.opt.multiplier, the relative importance of workers' preferences
    score <- sum(ifelse(work$weight.slots.worked < init4[ , 3], 
                        (work$weight.slots.worked - init4[ , 1])/(init4[ , 3] - init4[ , 1]), 
                        (init4[ , 2] - work$weight.slots.worked)/(init4[ , 2] - init4[ , 3]))) * work.opt.multiplier
    
    score = score + sum(work$desires.mat * work$mat)
    
    return(score * permissibility)
  }
  
  #------------------------------------------------------------------------------------------------
  # a function to use the local search algorithm
  # takes an evaluation function and a matrix to be evaluated
  
  local.search <- function(eval.fun){
    # set up places to save evaluated scores and 'escape' to see when finished
    escape <- FALSE
    neighbour.slide.vals <- matrix(0, ncol = p, nrow = sl)
    neighbour.swap.vals <- matrix(0, ncol = sl, nrow = sl)
    prev.best <- 0
    while(!escape){
      # update copy
      work$mat.copy <- work$mat
      
      # evaluate all possible slides
      for(i in 1:sl){
        potential.cols <- (1:p)[-which(work$mat[i, ])]
        for(j in potential.cols){
          # make slide
          work$mat[i, ] <- logical(p)
          work$mat[i, j] <- TRUE
          
          # store score
          neighbour.slide.vals[i, j] <- eval.fun()
        }
        # reset row
        work$mat[i, ] <- work$mat.copy[i, ]
      }
      
      # evaluate all possible swaps
      for(i in 1:sl){
        potential.rows <- (1:sl)[-i]
        for(j in potential.rows){
          # make swap
          work$mat[c(i, j), ] <- work$mat.copy[c(j, i), ]
          
          # store score
          neighbour.swap.vals[i, j] <- eval.fun()
          
          #reset rows
          work$mat[c(i, j), ] <- work$mat.copy[c(i, j), ]
        }
      }      
      
      # compare to find best neighbour
      best.slide <- max(neighbour.slide.vals, na.rm = TRUE) 
      best.swap <- max(neighbour.swap.vals, na.rm = TRUE)
      
      if(best.slide > best.swap){
        if(best.slide <= prev.best) {
          # no improvement can be made
          escape <- TRUE
        }
        else{
          # save slide
          chosen.slide.pos <- which(neighbour.slide.vals == best.slide)[1]
          
          i <- ((chosen.slide.pos - 1) %% sl) + 1
          j <- ((chosen.slide.pos - 1) %/% sl) + 1
          
          work$mat[i, ] <- logical(p)
          work$mat[i, j] <- TRUE
          
          #update previous best
          prev.best <- best.slide
        }
      } else {
        if(best.swap <= prev.best){
          # no improvement can be made
          escape <- TRUE
        }
        else{
          # save swap
          chosen.swap.pos <- which(neighbour.swap.vals == best.swap)[1]
          
          i <- ((chosen.swap.pos - 1) %% sl) + 1
          j <- ((chosen.swap.pos - 1) %/% sl) + 1
          
          work$mat[c(i, j), ] <- work$mat.copy[c(j, i), ]
          
          # update previous best
          prev.best <- best.swap
        }
      }
    }
    return(prev.best)
  } 
  
  #------------------------------------------------------------------------------------------------
  # a function to use an adaptation of the simulated annealing algorithm
  # takes an evaluation function and a matrix to be evaluated
  
  simulated.annealing <- function(eval.fun){
    # initialise function variables
    neighbour.slide.vals <- matrix(0, ncol = p, nrow = sl)
    neighbour.swap.vals <- matrix(0, ncol = sl, nrow = sl)
    work$mat.copy <- work$mat
    prev.best <- 0
    # set temp
    temp.func <- seq(100, 0, length.out = num.temperatures)
    bool.swap <- FALSE
    for(temperature in temp.func){
      # update copy
      work$mat.copy <- work$mat
      
      # get either all slide neighbours or swap neighbours
      if(bool.swap){
        #slides
        for(i in 1:sl){
          potential.cols <- (1:p)[-which(work$mat[i, ])]
          for(j in potential.cols){
            # make slide
            work$mat[i, ] <- logical(p)
            work$mat[i, j] <- TRUE
            
            # store score
            neighbour.slide.vals[i, j] <- eval.fun()
          }
          # reset row
          work$mat[i, ] <- work$mat.copy[i, ]
        }
        
        # find best slides, choose randomly from them
        best.slides <- order(neighbour.slide.vals, decreasing = TRUE)[1:num.neighbours.considered]
        chosen.slide.pos <- sample(x = best.slides, size = 1, 
                                   prob = (1:num.neighbours.considered)^((temperature * temp.exponent)/100 - temp.exponent))
        chosen <- neighbour.slide.vals[chosen.slide.pos]
        
        # make slide
        i <- ((chosen.slide.pos - 1) %% sl) + 1
        j <- ((chosen.slide.pos - 1) %/% sl) + 1
        work$mat[i, ] <- logical(p)
        work$mat[i, j] <- TRUE
        
      } else {
        #swaps
        for(i in 1:sl){
          potential.rows <- (1:sl)[-i]
          for(j in potential.rows){
            work$mat[c(i, j), ] <- work$mat.copy[c(j, i), ]
            
            # store score
            neighbour.swap.vals[i, j] <- eval.fun()
            
            # reset rows
            work$mat[c(i, j), ] <- work$mat.copy[c(i, j), ]
          }
        }
        
        # find best swaps, chose randomly from them
        best.swaps <- order(neighbour.swap.vals, decreasing = TRUE)[1:num.neighbours.considered]
        chosen.swap.pos <- sample(x = best.swaps, size = 1, 
                                  prob = (1:num.neighbours.considered)^((temperature * temp.exponent)/100 - temp.exponent))
        chosen <- neighbour.swap.vals[chosen.swap.pos]
        
        # make swap
        i <- ((chosen.swap.pos - 1) %% sl) + 1
        j <- ((chosen.swap.pos - 1) %/% sl) + 1
        work$mat[c(i, j), ] <- work$mat.copy[c(j, i), ]
      }
      
      # update previous best and prepare to alternate between swaps and slides
      prev.best <- chosen
      bool.swap <- !bool.swap
    }
    return(prev.best)
  }
  
  #------------------------------------------------------------------------------------------------
  # main control flow of function
  
  # initialise mat using chosen algorithm
  value.init.process <- switch(init.process,
                               random = gen.random(),
                               simulated.annealing = gen.simulated.annealing(),
                               local.search = gen.local.search(),
                               greedy = gen.greedy(),
                               NULL
  )
  if(is.null(value.init.process)){
    stop("init.process not recognised")
  }
  
  # optimise using chosen function
  score <- switch(algorithm,
                  simulated.annealing = simulated.annealing(evaluate),
                  local.search = local.search(evaluate),
                  NULL
  )
  
  if(is.null(score)){
    stop("algorithm not recognised")
  }
  
  # names for work$mat
  rownames(work$mat) <- slot.titles
  colnames(work$mat) <- colnames(init1)
  
  # return score and solution
  return(list(score = score, permissibility = check.permissibility(), solution = work$mat))
}
