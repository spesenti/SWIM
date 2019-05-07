# this function combines the subfunctions: 
# stress_VaR, stress_VaR_ES, stress_prob

# = c("VaR", "VaR ES", "prob", "user")

stress <- function(type = c("VaR", "VaR ES", "prob", "user"), ...){

  if(type == "VaR") SWIM <- stress_VaR(...)
  if(type == "VaR ES") SWIM <- stress_VaR_ES(...)
  if(type == "prob") SWIM <- stress_prob(...)
  if(type == "user") SWIM <- stress_user(...)
  if(type == "moments") SWIM <- stress_moments(...)
  return(SWIM)
}

