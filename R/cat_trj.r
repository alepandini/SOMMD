#' Function to read a set of xtc file, and concatenate them.

#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param FILES the xtc files to be read
#'
#' @return TRJ a list, with $coords being the coordinates of the simulation and $starts being the starting frame of each replica (usefull to have it for some analysis)
#' @export
#'
#' @examples
#' #TODO Not added yet

cat_trj <-  function(trj1, ...){
    traj_list <- list(...)
    #Append all the simulations subsequently
    trj <- trj1
    N=1
    print(traj_list)
    for(T in traj_list){
        N=N+1
        #Check that the two simulations have the same number of atoms
        if(dim(trj$coord)[1] != dim(T$coord)[1]){
            stop(sprintf("Number of atoms of simulation %d (%d) is not consistent with the number of atoms of previous simulations %d", N, dim(T$coord)[1], dim(trj$coord)))
        }
        #Check if the two simulations have the same topology, otherwise print a warning
        if(all(trj$top[,c(1:3)] == T$top[,c(1:3)])==FALSE){
            warning(sprintf("Topology of trajectory 1 and %d are not the same. Be sure that the trj files you are merging are consistent", N))
        }
        #Check if the two simulations have the same trjformat, otherwise print a warning
        if(trj$trjformat != T$trjformat){
            warning("Simulations you are merging come from differet file format. Be sure that the trj files you are merging are consistent")
        }
        if(trj$topformat != T$topformat){
            warning("Simulations you are merging use different top file format. Be sure that the trj files you are merging are consistent.")
        }
        trj$trjfile <- c(trj$trjfile, T$trjfile)
        trj$start <- c(trj$start, tail(trj$end, 1)+1)
        trj$end   <- c(trj$end, tail(trj$start,1)+dim(trj$coord)[3]) 
        trj$coord <- abind::abind(trj$coord, T$coord, along = 3)
        trj$call <- sys.call()
    }
}

