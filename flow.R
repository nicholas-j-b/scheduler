


function(init1, init2, init3, init4, work.opt.multiplier = 1, algorithm = "local", init.process = "random"){
  
  # generate size variables
  p <- ncol(init1)
  s <- nrow(init1)
  g <- ncol(init2)
  sl <- sum(init2)
  
  # produce work.mat matrix to store the current solution
  work.mat <- matrix(data = FALSE, nrow = sl, ncol = p)
  slot.names.list <- list()
  for(i in 1:s){
    slot.names.list <- c(slot.names.list, colnames(init2)[rep(1:g, times = init2[i, ])])
  }
  slot.names <- unlist(slot.names.list)
  slot.ref.names <- rep(rownames(init2), times = rowSums(init2))
  slot.titles <- make.unique(paste0(slot.ref.names, slot.names))
  rownames(work.mat) <- slot.titles
  
  # establish check.mat (const)
  # check.mat is a lookup table that has value true where worker is available and in correct group
  group.mat <- apply(init3, MARGIN = 1, FUN = function(x) x[slot.names])
  times.vec <- unlist(lapply(rownames(init1), FUN = function(x) length(grep(x, slot.titles))))
  rep.vec <- rep(1:s, times = times.vec)
  desires.mat <- (init1)[rep.vec, ]
  availabilities.mat <- desires.mat != 0
  check.mat <- availabilities.mat & group.mat
  rownames(check.mat) <- slot.titles
  expanded.weights <- rep(weights, times = times.vec)
  
  work.mat <- switch(init.process,
    random = t(replicate(sl, sample(c(rep(FALSE, times = p - 1), TRUE), size = p, replace = FALSE))),
    random.permiss = #TODO,
    greedy = #TODO,
    NULL  
  )
  
  if (is.null(work.mat)){
    warning("init.process not recognised. Using 'random.permiss'")
    work.mat <- NULL #TODO
  }
  
  check.permissibility <- function(){
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
    
    # consecutiveness 
    # how many shifts can be done consecutively 
    #TODO
    
    return(permissibility)
  }
  
  evaluate <- function(){
    # evaluate
    
    # triangle density calculated with optimum
    score <- sum(ifelse(weight.slots.worked < init4[ , 3], (weight.slots.worked - init4[ , 1])/(init4[ , 3] - init4[ , 1]), 
                        (init4[ , 2] - weight.slots.worked)/(init4[ , 2] - init4[ , 3]))) * work.opt.multiplier
    
    score = score + sum(desires.mat * work.mat)
    
    # spread
    # test how well the shifts are spread across the timeframe
    #TODO
    
    return(score)
  }
  
  #TODO strictness?
  
  # LOCAL SEARCH
  if(algorithm == "local"){
    escape <- TRUE
    neighbour.slide.vals <- numeric(sl * (p -1))
    neighbour.swap.vals <- numeric(also)
    while(escape){
      # set temp
      work.temp <- work.mat
      
      #get all neighbours
      row.to.slide <- 1:sl
      for(i in 1:sl){
        potential.cols <- (1:p)[-which(work.mat[i, ])]
        for(j in potential.cols){
          new.row <- logical(p)
          new.row[j] <- TRUE
          work.temp[i, ] <- new.row
          
        }
      }

      
      
      
      
      
      
      
      #swaps
      rows.to.swap <- 1:(length(slot.titles))
      # potential.swaps <- i #loop# :length(slot.titles)
      # swap
      
      
      
      
    }
  } 
  
  # SIMULATED ANNEALING
  else if (algorithm == "sim a"){
    escape <- TRUE
    while(escape){
      
    }
  }
  
  
  
  
  
  
  
  
}




















