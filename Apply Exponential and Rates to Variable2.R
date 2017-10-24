#FUNCTION TO APPLY EXPONENTIAL RATES TO VARIABLES
#LIMITATION IS THAT IT DOES NOT SEEM TO WORK FOR LARGER FUNCTIONS

exp1 <- function (data, variable, rate)
{ 
  x <- data [,get(variable)]
  Exped <- sapply(rate, function(rate)  {
    exp(x*rate)
  })
  out <- as.data.table(Exped)
  setnames(out, names(out), paste(variable, "Exp", as.character(rate), sep = "."))
  
  return(out[])
}