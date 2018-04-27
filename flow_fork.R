

load("~/ws-r/nsp/gen2_1.RData")



optimise <- function(init1, init2, init3, init4, work.opt.multiplier = 1, 
                     algorithm = "simulated.annealing", init.process = "simulated.annealing", 
                     no.temperatures = 41, rand.gen.tolerance = .65,
                     tolerance = .65){
  
  
  #------------------------------------------------------------------------------------------------
  # initialise function
  
  
  # generate size variables
  p <- ncol(init1)
  s <- nrow(init1)
  g <- ncol(init2)
  sl <- sum(init2)
  
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
  work$mat.copy <- work$mat
  
  # establish check.mat (const)
  # check.mat is a lookup table that has value true where worker is available and in correct group
  group.mat <- apply(init3, MARGIN = 1, FUN = function(x) x[slot.names])
  times.vec <- unlist(lapply(rownames(init1), FUN = function(x) length(grep(x, slot.titles))))
  rep.vec <- rep(1:s, times = times.vec)
  desires.mat <- (init1)[rep.vec, ]
  availabilities.mat <- desires.mat != 0
  work$check.mat <- availabilities.mat & group.mat
  rownames(work$check.mat) <- slot.titles
  expanded.weights <- rep(weights, times = times.vec)

  
  
  #------------------------------------------------------------------------------------------------
  # generate initial work.mat
  
  gen.random.mat <- function(){
    work$mat <- t(replicate(sl, sample(c(rep(FALSE, times = p - 1), TRUE), size = p, replace = FALSE)))
    weight.slots.worked <- expanded.weights %*% work$mat
    if(check.permissibility(weight.slots.worked) < rand.gen.tolerance){
      work$mat <- gen.random.mat()
    } else {
      return(work$mat)
    }
  }
  
  gen.local.search <- function(){
    work$mat <- gen.random.mat()
    return(local.search(check.permissibility))
  }
  
  gen.simulated.annealing <- function(){
    work$mat <- gen.random.mat()
    return(simulated.annealing(check.permissibility))
  }
  
  
  #------------------------------------------------------------------------------------------------
  # a function to check the permissibility of a matrix
  
  check.permissibility <- function(weight.slots.worked){
    # a value between 0 and 1 : 1 is given only to permissible solutions
    
    # all entries in work.mat fit in check.mat
    fit.check.mat <- sum(work$mat & work$check.mat) / sl
    
    # check only 1 slot / person / shift
    fit.slot.lim <- sum(apply(work$mat, MARGIN = 2, FUN = tapply, INDEX = rep.vec, sum) <= 1) / (p * s)
    
    # check between min and max
    fit.min.max <- sum((init4[ ,1] < weight.slots.worked) & (weight.slots.worked < init4[ ,2])) / p

    
    # permissibility rating
    permissibility <- (fit.check.mat + fit.slot.lim + fit.min.max) / 3
    
    # consecutiveness 
    # how many shifts can be done consecutively 
    #TODO
    
    return(permissibility)
  }
  
  #------------------------------------------------------------------------------------------------
  # a function to find the value of a given matrix
  
  evaluate <- function(weight.slots.worked){
    # check permissibility
    permissibility <- check.permissibility(weight.slots.worked)
    if(permissibility < tolerance){
      return(0)
    }
    
    # evaluate
    
    # triangle density calculated with optimum
    score <- sum(ifelse(weight.slots.worked < init4[ , 3], 
                        (weight.slots.worked - init4[ , 1])/(init4[ , 3] - init4[ , 1]), 
                        (init4[ , 2] - weight.slots.worked)/(init4[ , 2] - init4[ , 3]))) * work.opt.multiplier
    
    score = score + sum(desires.mat * work$mat)
    
    # spread
    # test how well the shifts are spread across the timeframe
    #TODO
    
    return(score * permissibility)
  }
  
  #------------------------------------------------------------------------------------------------
  # a function to use the local search algorithm
  # takes an evaluation function and a matrix to be evaluated
  
  local.search <- function(eval.fun){
    print("begin local search")
    escape <- FALSE
    neighbour.slide.vals <- numeric(sl * (p - 1))
    neighbour.swap.vals <- numeric(sl * (sl - 1))
    prev.best <- 0
    while(!escape){
      print(prev.best)
      # set temp
      work$mat.copy <- work$mat

      #slide
      for(i in 1:sl){
        potential.cols <- (1:p)[-which(work$mat[i, ])]
        for(j in potential.cols){
          # make slide
          work$mat[i, ] <- logical(p)
          work$mat[i, j] <- TRUE
          
          # check permissibility
          weight.slots.worked <- expanded.weights %*% work$mat
          
          neighbour.slide.vals[((i - 1) * (p - 1)) + which(j == potential.cols)] <- eval.fun(weight.slots.worked)
          
        }
        # reset row
        work$mat[i, ] <- work$mat.copy[i, ]
      }

      #swaps
      for(i in 1:sl){
        potential.rows <- (1:sl)[-i]

        for(j in potential.rows){
          work$mat[c(i, j), ] <- work$mat.copy[c(j, i), ]
          
          # check permissibility
          weight.slots.worked <- expanded.weights %*% work$mat
          neighbour.swap.vals[((i - 1) * (sl - 1)) + j - (j > i)] <- eval.fun(weight.slots.worked)

          work$mat[c(i, j), ] <- work$mat.copy[c(i, j), ]

        }
      }      

      best.slide <- max(neighbour.slide.vals, na.rm = TRUE) # shouldn't be NAs!! bug.
      best.swap <- max(neighbour.swap.vals, na.rm = TRUE) # same ^^

      
      if(best.slide > best.swap){
        best.pos <- which(neighbour.slide.vals == best.slide)[1] #maybe pick one at random?
        i <- (best.pos %/% (p - 1)) + 1
        j <- best.pos %% (p - 1)
        j <- (1:p)[-which(work$mat[i, ])][j]

        work$mat[i, ] <- logical(p)
        work$mat[i, j] <- TRUE
        if(best.slide <= prev.best) {
          escape <- TRUE
          #TODO
          # end loop conditions, return etc
        }
        else{
          prev.best <- best.slide
        }
      } else {
        best.pos <- which(neighbour.swap.vals == best.swap)[1] #maybe pick one at random?
        i <- (best.pos %/% (sl - 1)) + 1
        j <- best.pos %% (sl - 1)
        j <- j + (j <= i)

        work$mat[c(i, j), ] <- work$mat.copy[c(j, i), ]
        if(best.swap <= prev.best){
          escape <- TRUE
          #TODO
        }
        else{
          prev.best <- best.swap
        }
      }
    }
    return(work$mat)
  } 
  
  
  #------------------------------------------------------------------------------------------------
  # a function to use the simulated annealing algorithm
  # takes an evaluation function and a matrix to be evaluated
  
  simulated.annealing <- function(eval.fun){
    print("begin simulated.annealing")
    neighbour.slide.vals <- numeric(sl * (p - 1))
    neighbour.swap.vals <- numeric(sl * (sl - 1))
    work$mat.copy <- work$mat
    prev.best <- 0
    # set temp
    temp.func <- seq(100, 0, length.out = no.temperatures)
    bool.swap <- FALSE
    for(temperature in temp.func){
      print(prev.best)
      
      work$mat.copy <- work$mat
      
      # get all neighbours
      if(bool.swap){
        #slide
        for(i in 1:sl){
          potential.cols <- (1:p)[-which(work$mat[i, ])]
          for(j in potential.cols){
            # make slide
            work$mat[i, ] <- logical(p)
            work$mat[i, j] <- TRUE
            
            # check permissibility
            weight.slots.worked <- expanded.weights %*% work$mat
            
            neighbour.slide.vals[((i - 1) * (p - 1)) + which(j == potential.cols)] <- eval.fun(weight.slots.worked)
            
          }
          # reset row
          work$mat[i, ] <- work$mat.copy[i, ]
        }
        
        
        best.slides <- order(neighbour.slide.vals, decreasing = TRUE)[1:10]
        chosen.slide.pos <- sample(x = best.slides, size = 1, prob = (1:10)^(temperature/25 - 4))
        chosen <- neighbour.slide.vals[chosen.slide.pos]
        
        i <- (chosen.slide.pos %/% (p - 1)) + 1
        j <- chosen.slide.pos %% (p - 1)
        j <- (1:p)[-which(work$mat[i, ])][j]
        work$mat[i, ] <- logical(p)
        work$mat[i, j] <- TRUE
        prev.best <- chosen
        
      } else {
        
        #swaps
        for(i in 1:sl){
          potential.rows <- (1:sl)[-i]
          for(j in potential.rows){
            work$mat[c(i, j), ] <- work$mat.copy[c(j, i), ]
            
            # check permissibility
            weight.slots.worked <- expanded.weights %*% work$mat
            neighbour.swap.vals[((i - 1) * (sl - 1)) + j - (j > i)] <- eval.fun(weight.slots.worked)
            
            # reset rows
            work$mat[c(i, j), ] <- work$mat.copy[c(i, j), ]

          }
        }      
      }

      
      best.swaps <- order(neighbour.swap.vals, decreasing = TRUE)[1:10]
      chosen.swap.pos <- sample(x = best.swaps, size = 1, prob = (1:10)^(temperature/25 - 4))
      chosen <- neighbour.swap.vals[chosen.swap.pos]
      

      i <- (chosen.swap.pos %/% (sl - 1)) + 1
      j <- chosen.swap.pos %% (sl - 1)
      j <- j + (j <= i)

      work$mat[c(i, j), ] <- work$mat.copy[c(j, i), ]
      prev.best <- chosen
      bool.swap <- !bool.swap
    }
    return(work$mat)
  }
  
  

  
  
  
  #------------------------------------------------------------------------------------------------
  # main control flow of function
  
  
  
  work$mat <- switch(init.process,
                     random = gen.random.mat(),
                     simulated.annealing = gen.simulated.annealing(),
                     local.search = gen.local.search(),
                     #greedy = #TODO,
                     NULL  
  )
  
  work$mat <- switch(algorithm,
                     simulated.annealing = simulated.annealing(evaluate),
                     local.search = local.search(evaluate),
                     NULL
  )
  
  
  # if (is.null(work.mat)){
  #   warning("init.process not recognised. Using 'random.permiss'")
  #   work.mat <- NULL #TODO
  # }
  
  
  
  #TODO strictness?
  

  
  
}


optimise(init1, init2, init3, init4, init.process = "simulated.annealing" , algorithm = "simulated.annealing")

# optimise(init1, init2, init3, init4, init.process = "local.search" , algorithm = "local.search")














