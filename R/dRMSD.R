dRMSD <- function(VEC1, VEC2, N_atm){
  return(sqrt(sum((VEC1 - VEC2)**2)/(N_atm*(N_atm-1)))*10)
}
