
fon_g <-function(x, a){
  g = exp(-a*x)
  return(g)
}


fun_y <- function(x, a, b){
  y = fon_g(x, a) + b*rnorm(length(x), 0, 1)
  return(y)
}

data <- fun_y(x, a, b)
plot(data, col="blue", main = "données simulées")

data_f<-data.frame(data )

# fonction de cout f
fonc_f <-  function(x, y, a){
   f = sum (( y - exp(-a * x))^2)
    return(f)

}
fonc_f(x, y, a)


# fonction de grad

fonc_grad <- function(x, y, a){
  
  for(i in 1:length(x)){
    grad <-0
    grad[i+1] <- grad[i] +  sum(y[i]- exp(-a*x[i]))*exp(-a*x[i])
    return(grad)
  }
}

fonc_grad(x, y, a)  

###
a=2
b=0.01
x <- seq(0, 3, 0.01)
y = fon_g(x, a) + b*rnorm(length(x), 0, 1)




