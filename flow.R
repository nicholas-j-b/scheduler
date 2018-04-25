

load("~/ws-r/nsp/gen7.RData")



optimise <- function(init1, init2, init3, init4, work.opt.multiplier = 1, 
                     algorithm = "local", init.process = "random.permiss", rand.gen.tolerance = .75){
  
  
  #------------------------------------------------------------------------------------------------
  # initialise function
  
  
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
  
  
  #------------------------------------------------------------------------------------------------
  # create 'permissible' mat with local search

  gen.random.mat <- function(){
    work.mat <- t(replicate(sl, sample(c(rep(FALSE, times = p - 1), TRUE), size = p, replace = FALSE)))
    weight.slots.worked <- expanded.weights %*% work.mat
    if(check.permissibility(work.mat, weight.slots.worked) < rand.gen.tolerance){
      work.mat <- gen.random.mat()
    } else {
    return(work.mat)
    }
  }
  
  gen.permiss.mat <- function(){
    work.mat <- gen.random.mat()
    # print("restult of gen.random.mat")
    # print(work.mat)
    return(local.search(check.permissibility, work.mat))
  }
  
  
  #------------------------------------------------------------------------------------------------
  # a function to check the permissibility of a matrix
  
  check.permissibility <- function(work.mat, weight.slots.worked){
    # a value between 0 and 1 : 1 is given only to permissible solutions
    
    # all entries in work.mat fit in check.mat
    fit.check.mat <- sum(work.mat & check.mat) / sl
    
    # check only 1 slot / person / shift
    fit.slot.lim <- sum(apply(work.mat, MARGIN = 2, FUN = tapply, INDEX = rep.vec, sum) <= 1) / (p * s)

    # check between min and max
    fit.min.max <- sum((init4[ ,1] < weight.slots.worked) & (weight.slots.worked < init4[ ,2])) / p

    if(is.na(fit.check.mat)){
      print("fit.check.mat NA")
      print(work.mat)
      print(weight.slots.worked)
    }
    if(is.na(fit.slot.lim)){
      print("fit.slot.lim NA")
      print(work.mat)
      print(weight.slots.worked)
    }
    if(is.na(fit.min.max)){
      print("fit.min.max NA")
      print(work.mat)
      print(weight.slots.worked)
    }
    
    # debug
    # print(check.mat)
    # print("fit ratings")
    # print(fit.check.mat)
    # print(fit.slot.lim)
    # print(fit.min.max)
    # print("p")
    # print(p)
    # print(init4[ , 1])
    # print(init4[ , 2])
    # print(weight.slots.worked)
    
    # permissibility rating
    permissibility <- (fit.check.mat + fit.slot.lim + fit.min.max) / 3
    
    # consecutiveness 
    # how many shifts can be done consecutively 
    #TODO
    
    return(permissibility)
  }
  
  #------------------------------------------------------------------------------------------------
  # a function to find the value of a given matrix
  
  evaluate <- function(work.mat, weight.slots.worked){
    # check permissibility
    if(check.permissibility(work.mat, weight.slots.worked) < 1){
      return(0)
    }
    
    # evaluate
    
    # triangle density calculated with optimum
    score <- sum(ifelse(weight.slots.worked < init4[ , 3], 
                        (weight.slots.worked - init4[ , 1])/(init4[ , 3] - init4[ , 1]), 
                        (init4[ , 2] - weight.slots.worked)/(init4[ , 2] - init4[ , 3]))) * work.opt.multiplier
    
    score = score + sum(desires.mat * work.mat)
    
    # spread
    # test how well the shifts are spread across the timeframe
    #TODO
    
    return(score)
  }
  
  
  #------------------------------------------------------------------------------------------------
  # a function to use the local search algorithm
  # takes an evaluation function and a matrix to be evaluated
  
  
  local.search <- function(eval.fun, work.mat){
    escape <- FALSE
    neighbour.slide.vals <- numeric(sl * (p - 1))
    neighbour.swap.vals <- numeric(sl * (sl - 1))
    work.mat.new <- work.mat
    prev.best <- 0
    while(!escape){
      print("best so far")
      print(prev.best)
      # set temp
      work.mat.temp <- work.mat.new
      work.mat <- work.mat.new
      # print("in local.search, beginning of loop")
      # print(work.mat)
      
      # get all neighbours
      print("-----------------------")
      print(sl)
      print(p)
      #slide
      for(i in 1:sl){
        potential.cols <- (1:p)[-which(work.mat[i, ])]
        for(j in potential.cols){
          # make slide
          # new.row <- logical(p)
          # new.row[j] <- TRUE
          # work.mat.temp[i, ] <- new.row
          work.mat.temp[i, ] <- logical(p)
          work.mat.temp[i, j] <- TRUE
          
          # check permissibility
          weight.slots.worked <- expanded.weights %*% work.mat.temp

          neighbour.slide.vals[((i - 1) * (p - 1)) + which(j == potential.cols)] <- eval.fun(work.mat.temp, weight.slots.worked)

        }
        # reset row
        work.mat.temp[i, ] <- work.mat[i, ]
      }
      
      # print("finish slides")
      
      
      
      #swaps
      for(i in 1:sl){
        # print("in swaps")
        # print(i)
        # print(work.mat.temp)
        potential.rows <- (1:sl)[-i]
        # print("potenial.rows")
        # print(potential.rows)
        for(j in potential.rows){
          work.mat.temp[c(i, j), ] <- work.mat[c(j, i), ]
          
          # check permissibility
          weight.slots.worked <- expanded.weights %*% work.mat.temp
          neighbour.swap.vals[((i - 1) * (sl - 1)) + j - (j > i)] <- eval.fun(work.mat.temp, weight.slots.worked)
          
      
          # reset rows
          # print("before workmat reset")
          # print(work.mat.temp)
          work.mat.temp[c(i, j), ] <- work.mat[c(i, j), ]
          # print("after")
          # print(work.mat.temp)
          # print("work.mat")
          # print(work.mat)
        }
      }      
      
      print("neighbout.slide")
      print(neighbour.slide.vals)
      print("neighbour.swap")
      print(neighbour.swap.vals)
      
      best.slide <- max(neighbour.slide.vals, na.rm = TRUE) # shouldn't be NAs!! bug.
      best.swap <- max(neighbour.swap.vals, na.rm = TRUE) # same ^^
      
      # print("best slide")
      # print(best.slide)
      # print("best swap")
      # print(best.swap)
      
      if(best.slide > best.swap){
        best.pos <- which(neighbour.slide.vals == best.slide)[1] #maybe pick one at random?
        i <- (best.pos %/% (p - 1)) + 1
        j <- best.pos %% (p - 1)
        j <- (1:p)[-which(work.mat[i, ])][j]
        # new.row <- logical(p)
        # new.row[j] <- TRUE
        # work.mat.new[i, ] <- new.row
        work.mat.new[i, ] <- logical(p)
        work.mat.new[i, j] <- TRUE
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
        # print("best pos")
        # print(best.pos)
        i <- (best.pos %/% (sl - 1)) + 1
        j <- best.pos %% (sl - 1)
        j <- j + (j <= i)
        # print("i, j")
        # print(i)
        # print(j)
        work.mat.new[c(i, j), ] <- work.mat[c(j, i), ]
        if(best.swap <= prev.best){
          escape <- TRUE
          #TODO
        }
        else{
          prev.best <- best.swap
        }
      }
    }
    return(work.mat)
  } 
  
  
  
  
  
  
  
  
  #------------------------------------------------------------------------------------------------
  # a function to use the simulated annealing algorithm
  # takes an evaluation function and a matrix to be evaluated
  
  
  ##############################
  # STILL NEEDS WRITING
  
  simulated.annealing <- function(eval.fun, work.mat){
    neighbour.slide.vals <- numeric(sl * (p - 1))
    neighbour.swap.vals <- numeric(sl * (sl - 1))
    work.mat.new <- work.mat
    prev.best <- 0
    temp.func <- seq(100, 0, length.out = 21)
    for(temp in temp.func){  
        print("best so far")
        print(prev.best)
        # set temp
        work.mat.temp <- work.mat.new
        work.mat <- work.mat.new
        # print("in local.search, beginning of loop")
        # print(work.mat)
        
        # get all neighbours
        print("-----------------------")
        print(sl)
        print(p)
        #slide
        for(i in 1:sl){
          potential.cols <- (1:p)[-which(work.mat[i, ])]
          for(j in potential.cols){
            # make slide
            # new.row <- logical(p)
            # new.row[j] <- TRUE
            # work.mat.temp[i, ] <- new.row
            work.mat.temp[i, ] <- logical(p)
            work.mat.temp[i, j] <- TRUE
            
            # check permissibility
            weight.slots.worked <- expanded.weights %*% work.mat.temp
            
            neighbour.slide.vals[((i - 1) * (p - 1)) + which(j == potential.cols)] <- eval.fun(work.mat.temp, weight.slots.worked)
            
          }
          # reset row
          work.mat.temp[i, ] <- work.mat[i, ]
        }
        
        # print("finish slides")
        
        
        
        #swaps
        for(i in 1:sl){
          # print("in swaps")
          # print(i)
          # print(work.mat.temp)
          potential.rows <- (1:sl)[-i]
          # print("potenial.rows")
          # print(potential.rows)
          for(j in potential.rows){
            work.mat.temp[c(i, j), ] <- work.mat[c(j, i), ]
            
            # check permissibility
            weight.slots.worked <- expanded.weights %*% work.mat.temp
            neighbour.swap.vals[((i - 1) * (sl - 1)) + j - (j > i)] <- eval.fun(work.mat.temp, weight.slots.worked)
            
            
            # reset rows
            # print("before workmat reset")
            # print(work.mat.temp)
            work.mat.temp[c(i, j), ] <- work.mat[c(i, j), ]
            # print("after")
            # print(work.mat.temp)
            # print("work.mat")
            # print(work.mat)
          }
        }      
        
        print("neighbout.slide")
        print(neighbour.slide.vals)
        print("neighbour.swap")
        print(neighbour.swap.vals)
        
        best.slides <- max(neighbour.slide.vals)
        best.swaps <- max(neighbour.swap.vals)
        
        # print("best slide")
        # print(best.slide)
        # print("best swap")
        # print(best.swap)
        
        if(best.slide > best.swap){
          best.pos <- which(neighbour.slide.vals == best.slide)[1] #maybe pick one at random?
          i <- (best.pos %/% (p - 1)) + 1
          j <- best.pos %% (p - 1)
          j <- (1:p)[-which(work.mat[i, ])][j]
          # new.row <- logical(p)
          # new.row[j] <- TRUE
          # work.mat.new[i, ] <- new.row
          work.mat.new[i, ] <- logical(p)
          work.mat.new[i, j] <- TRUE
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
          # print("best pos")
          # print(best.pos)
          i <- (best.pos %/% (sl - 1)) + 1
          j <- best.pos %% (sl - 1)
          j <- j + (j <= i)
          # print("i, j")
          # print(i)
          # print(j)
          work.mat.new[c(i, j), ] <- work.mat[c(j, i), ]
          if(best.swap <= prev.best){
            escape <- TRUE
            #TODO
          }
          else{
            prev.best <- best.swap
          }
        }
      }
      return(work.mat)
    } 
      
      
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #------------------------------------------------------------------------------------------------
  # main control flow of function
  
  
  
  work.mat <- switch(init.process,
    random = gen.random.mat(),
    random.permiss = gen.permiss.mat(),
    #greedy = #TODO,
    NULL  
  )
  
  
  if (is.null(work.mat)){
    warning("init.process not recognised. Using 'random.permiss'")
    work.mat <- NULL #TODO
  }
  

  
  #TODO strictness?
  


  

  
  
  
  
  
  
}


optimise(init1, init2, init3, init4)
















