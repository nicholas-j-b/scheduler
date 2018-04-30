

load("~/ws-r/nsp/gen2_4.RData")



optimise <- function(init1, init2, init3, init4, work.opt.multiplier = 1, 
                     algorithm = "simulated.annealing", init.process = "simulated.annealing", 
                     no.temperatures = 41, rand.gen.tolerance = .65,
                     tolerance = .65, num.random.gens = 1000, greedy.limit = 250,
                     num.neighbours.considered = 10, temp.exponent = 4){
  
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
  # generate initial mat with chosen function
  
  gen.random.mat <- function(){
    # start with random mat that has one TRUE per row
    work$mat <- t(replicate(sl, sample(c(rep(FALSE, times = p - 1), TRUE), size = p, replace = FALSE)))
    work$weight.slots.worked <- work$expanded.weights %*% work$mat
    # if randomly generated mat is very poor, start again
    if(check.permissibility() < rand.gen.tolerance){
      gen.random.mat()
    }
  }
  
  gen.local.search <- function(){
    # start with random mat
    gen.random.mat()
    local.search(check.permissibility)
  }
  
  gen.simulated.annealing <- function(){
    # start with random mat
    gen.random.mat()
    simulated.annealing(check.permissibility)
  }
  
  gen.greedy <- function(){
    failure <- TRUE
    # initialise empty mat
    work$mat <- matrix(FALSE, ncol = p, nrow = sl)
    # loop over rows
    i <- 1
    counts <- 1
    while(i <= sl){
      cat("i : ", i, "\n")
      if(i < 1 || counts > greedy.limit){
        stop("greedy algorithm has failed, try another init method")
      }
      # ensure matching to check.mat
      cols <- sample((1:p)[work$check.mat[i, ]])
      for(j in cols){
        # initialise row to be tested
        work$mat[i, ] <- FALSE
        work$mat[i, j] <- TRUE
        
        # check only 1 slot / person / shift
        if(sum(apply(work$mat, MARGIN = 2, FUN = tapply, INDEX = work$rep.vec, sum) <= 1) == (p * s)){
          # check shifts worked less than max for person
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
  }
  
  #------------------------------------------------------------------------------------------------
  # a function to check the permissibility of a matrix
  
  check.permissibility <- function(){
    # a value between 0 and 1 : 1 is given only to permissible solutions
    
    # all entries in work.mat fit in check.mat
    fit.check.mat <- sum(work$mat & work$check.mat) / sl
    
    # check only 1 slot / person / shift
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
    
    # triangle density calculated with optimum and consider work.opt.multiplier, the relative importance of workers' preferences
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
          work$weight.slots.worked <- work$expanded.weights %*% work$mat
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
          work$weight.slots.worked <- work$expanded.weights %*% work$mat
          neighbour.swap.vals[i, j] <- eval.fun()

          #reset row
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
    temp.func <- seq(100, 0, length.out = no.temperatures)
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
            work$weight.slots.worked <- work$expanded.weights %*% work$mat
            neighbour.slide.vals[i, j] <- eval.fun()
          }
          # reset row
          work$mat[i, ] <- work$mat.copy[i, ]
        }
        
        # find best slides, chose randomly from them
        best.slides <- order(neighbour.slide.vals, decreasing = TRUE)[1:num.neighbours.considered]
        chosen.slide.pos <- sample(x = best.slides, size = 1, 
                                   prob = (1:num.neighbours.considered)^(temperature/(100 / temp.exponent) - temp.exponent))
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
            work$weight.slots.worked <- work$expanded.weights %*% work$mat
            neighbour.swap.vals[i, j] <- eval.fun()
            
            # reset rows
            work$mat[c(i, j), ] <- work$mat.copy[c(i, j), ]
          }
        }
      
        # find best swaps, chose randomly from them
        best.swaps <- order(neighbour.swap.vals, decreasing = TRUE)[1:num.neighbours.considered]
        chosen.swap.pos <- sample(x = best.swaps, size = 1, 
                                  prob = (1:num.neighbours.considered)^(temperature/(100 / temp.exponent) - temp.exponent))
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
  switch(init.process,
        random = gen.random.mat(),
        simulated.annealing = gen.simulated.annealing(),
        local.search = gen.local.search(),
        greedy = gen.greedy(),
        NULL  
  )
  
  # optimise using chosen function
  score <- switch(algorithm,
        simulated.annealing = simulated.annealing(evaluate),
        local.search = local.search(evaluate),
        NULL
  )
  
  # return score and solution
  return(list(score = score, solution = work$mat))
}


# optimise(init1, init2, init3, init4, init.process = "greedy" , algorithm = "simulated.annealing", tolerance = 1)

# optimise(init1, init2, init3, init4, init.process = "local.search" , algorithm = "local.search")

ans <- optimise(init1, init2, init3, init4, init.process = "simulated.annealing" , algorithm = "local.search")

# optimise(init1, init2, init3, init4, init.process = "simulated.annealing" , algorithm = "simulated.annealing")













