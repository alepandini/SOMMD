

#' distance root-mean-square deviation
#' @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#'
#'
#'
#' To calculate the root-mean-square deviation between
#' the atoms in two configurations
#'
#'
#'\deqn{dRMSD = \sqrt{ 1/N(N-1) * \sum{(X_i - X_j)^2L}
#'
#'     where i != j}}
#' X_i coorodinate of Atom i
#'
#'
#' X_j coorodinate of Atom j
#'
#' N number of atoms
#'
#' @param VEC1   the reference vector
#' @param VEC2  the second Conformation vector
#' @param N_atm the number of atom
#'
#' @return
#' @export
#'
#' @examples

dRMSD <- function(VEC1, VEC2, N_atm){
  return(sqrt(sum((VEC1 - VEC2)**2)/(N_atm*(N_atm-1))))
}
