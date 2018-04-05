


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
  
  check.permissibility <- function(work.mat, weight.slots.worked){
    # a value between 0 and 1 : 1 is given only to permissible solutions
    
    # all entries in work.mat fit in check.mat
    fit.check.mat <- sum(work.mat & check.mat) / sl
    
    # check only 1 slot / person / shift
    fit.slot.lim <- sum(apply(work.mat, MARGIN = 2, FUN = tapply, INDEX = rep.vec, sum) <= 1) / (p * s)
    
    # check between min and max
    fit.min.max <- sum((init4[ ,1] < weight.slots.worked) & (weight.slots.worked < init4[ ,2])) / p
    
    # permissibility rating
    permissibility <- (fit.check.mat + fit.slot.lim + fit.min.max) / 3
    
    # consecutiveness 
    # how many shifts can be done consecutively 
    #TODO
    
    return(permissibility)
  }
  
  evaluate <- function(work.mat, weight.slots.worked){
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
    escape <- FALSE
    neighbour.slide.vals <- numeric(sl * (p - 1))
    neighbour.swap.vals <- numeric(sl * (sl - 1))
    while(!escape){
      # set temp
      work.mat.temp <- work.mat
      
      # get all neighbours
      
      #slides
      for(i in 1:sl){
        potential.cols <- (1:p)[-which(work.mat[i, ])]
        for(j in potential.cols){
          # make slide
          new.row <- logical(p)
          new.row[j] <- TRUE
          work.temp[i, ] <- new.row
          
          # check permissibility
          weight.slots.worked <- expanded.weights %*% work.mat.temp
          permiss <- permissibility(work.mat.temp, weight.slots.worked)
          
          # evaluate if necessary
          if(permiss == 1){
            neighbour.slide.vals[((i - 1) * p) + j] <- evaluate(work.mat.temp, weight.slots.worked)
          }
        }
        # reset row
        work.mat.temp[i, ] <- work.mat[i, ]
      }

      #swaps
      for(i in 1:sl){
        potential.rows <- (1:sl)[-i]
        for(j in potential.rows){
          work.mat.temp[c(i, j), ] <- work.mat[c(j, i), ]
          
          # check permissibility
          weight.slots.worked <- expanded.weights %*% work.mat.temp
          permiss <- permissibility(work.mat.temp, weight.slots.worked)
          
          # evaluate if necessary
          if(permiss == 1){
            neighbour.swap.vals[((i - 1) * sl) + j] <- evaluate(work.mat.temp, weight.slots.worked)
          }
        }
        # reset rows
        work.mat.temp[c(i, j), ] <- work.mat[c(i, j)]
      }      
      
      best.slide <- max(neighbour.slide.vals)
      best.swap <- max(neighbour.swap.vals)
      
      if(best.slide > best.swap){
        best.pos <- which(neighbour.slide.vals == best.slide)
        i <- (best.pos %/% (p - 1)) + 1
        j <- best.pos %% (p - 1)
        new.row <- logical(p)
        new.row[j] <- TRUE
        work.mat.new[i, ] <- new.row
        if(best.slide > prev.best) {
          escape <- TRUE
          #TODO
          # end loop conditions, return etc
        }
      } else {
        best.pos <- which(neighbour.swap.vals == best.swap)
        i <- (best.pos %/% (sl - 1)) + 1
        j <- best.pos %% (sl - 1)
        work.mat.new[c(i, j), ] <- work.mat[c(j, i), ]
        if(best.swap > prev.best){
          escape <- TRUE
          #TODO
        }
      }
      
      
      
    }
  } 
  
  # SIMULATED ANNEALING
  else if (algorithm == "sim a"){
    escape <- FALSE
    while(!escape){
      
    }
  }
  
  
  
  
  
  
  
  
}



















