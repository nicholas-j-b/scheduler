


local.search <- function(eval.fun){
  escape <- FALSE
  neighbour.slide.vals <- numeric(sl * (p - 1))
  neighbour.swap.vals <- numeric(sl * (sl - 1))
  work.mat.new <- work.mat
  while(!escape){
    # set temp
    work.mat.temp <- work.mat.new
    work.mat <- work.mat.new
    print(work.mat)
    
    # get all neighbours
    
    #slide
    for(i in 1:sl){
      potential.cols <- (1:p)[-which(work.mat[i, ])]
      for(j in potential.cols){
        # make slide
        new.row <- logical(p)
        new.row[j] <- TRUE
        work.mat.temp[i, ] <- new.row
        
        # check permissibility
        weight.slots.worked <- expanded.weights %*% work.mat.temp
        neighbour.slide.vals[((i - 1) * p) + j] <- eval.fun(work.mat.temp, weight.slots.worked)
        
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
        neighbour.swap.vals[((i - 1) * sl) + j] <- eval.fun(work.mat.temp, weight.slots.worked)
        
      }
      # reset rows
      work.mat.temp[c(i, j), ] <- work.mat[c(i, j)]
    }      
    
    print(1)
    print(neighbour.slide.vals)
    print(neighbour.swap.vals)
    
    best.slide <- max(neighbour.slide.vals)
    best.swap <- max(neighbour.swap.vals)
    
    print(2)
    print(best.slide)
    print(best.swap)
    
    if(best.slide > best.swap){
      best.pos <- which(neighbour.slide.vals == best.slide)[1] #maybe pick one at random?
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
      best.pos <- which(neighbour.swap.vals == best.swap)[1] #maybe pick one at random?
      print(3)
      print(best.pos)
      i <- (best.pos %/% (sl - 1)) + 1
      j <- best.pos %% (sl - 1)
      print(4)
      print(i)
      print(j)
      work.mat.new[c(i, j), ] <- work.mat[c(j, i), ]
      if(best.swap > prev.best){
        escape <- TRUE
        #TODO
      }
    }
  }
} 




















# LOCAL SEARCH
# if(algorithm == "local"){
#   escape <- FALSE
#   neighbour.slide.vals <- numeric(sl * (p - 1))
#   neighbour.swap.vals <- numeric(sl * (sl - 1))
#   work.mat.new <- work.mat
#   while(!escape){
#     # set temp
#     work.mat.temp <- work.mat.new
#     work.mat <- work.mat.new
#     print(work.mat)
#     
#     # get all neighbours
#     
#     #slide
#     for(i in 1:sl){
#       potential.cols <- (1:p)[-which(work.mat[i, ])]
#       for(j in potential.cols){
#         # make slide
#         new.row <- logical(p)
#         new.row[j] <- TRUE
#         work.mat.temp[i, ] <- new.row
#         
#         # check permissibility
#         weight.slots.worked <- expanded.weights %*% work.mat.temp
#         permiss <- check.permissibility(work.mat.temp, weight.slots.worked)
#         # evaluate if necessary
#         if(permiss == 1){
#           neighbour.slide.vals[((i - 1) * p) + j] <- evaluate(work.mat.temp, weight.slots.worked)
#         }
#       }
#       # reset row
#       work.mat.temp[i, ] <- work.mat[i, ]
#     }
# 
#     #swaps
#     for(i in 1:sl){
#       potential.rows <- (1:sl)[-i]
#       for(j in potential.rows){
#         work.mat.temp[c(i, j), ] <- work.mat[c(j, i), ]
#         
#         # check permissibility
#         weight.slots.worked <- expanded.weights %*% work.mat.temp
#         permiss <- check.permissibility(work.mat.temp, weight.slots.worked)
#         
#         # evaluate if necessary
#         if(permiss == 1){
#           neighbour.swap.vals[((i - 1) * sl) + j] <- evaluate(work.mat.temp, weight.slots.worked)
#         }
#       }
#       # reset rows
#       work.mat.temp[c(i, j), ] <- work.mat[c(i, j)]
#     }      
#     
#     print(1)
#     print(neighbour.slide.vals)
#     print(neighbour.swap.vals)
#     
#     best.slide <- max(neighbour.slide.vals)
#     best.swap <- max(neighbour.swap.vals)
#     
#     print(2)
#     print(best.slide)
#     print(best.swap)
#     
#     if(best.slide > best.swap){
#       best.pos <- which(neighbour.slide.vals == best.slide)[1] #maybe pick one at random?
#       i <- (best.pos %/% (p - 1)) + 1
#       j <- best.pos %% (p - 1)
#       new.row <- logical(p)
#       new.row[j] <- TRUE
#       work.mat.new[i, ] <- new.row
#       if(best.slide > prev.best) {
#         escape <- TRUE
#         #TODO
#         # end loop conditions, return etc
#       }
#     } else {
#       best.pos <- which(neighbour.swap.vals == best.swap)[1] #maybe pick one at random?
#       print(3)
#       print(best.pos)
#       i <- (best.pos %/% (sl - 1)) + 1
#       j <- best.pos %% (sl - 1)
#       print(4)
#       print(i)
#       print(j)
#       work.mat.new[c(i, j), ] <- work.mat[c(j, i), ]
#       if(best.swap > prev.best){
#         escape <- TRUE
#         #TODO
#       }
#     }
#   }
# } 
