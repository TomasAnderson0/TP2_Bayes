#Metropolis-Hastings



MH=function(posterior, inicio, n,fun){
  theta = c()
  theta[1] = inicio
  for (i in 1:n) {
    propuesta = fun(theta[i])
    f_actual = posterior(theta[i])
    f_propuesta = posterior(propuesta)
    alpha = min(c(1, f_propuesta / f_actual))
    decision = sample(c("salto", "no salto"),  size = 1, prob = c(alpha, 1-alpha))
    if(decision=="salto") {
      theta[i+1] <- propuesta 
    } else {
      theta[i+1] <- theta[i]
    }
    }
  return(as.data.frame(theta))
}

asd = function(x) exp(-(x^2)/2)


set.seed(123)

data = MH(asd, 1, 100,fun)


plot(1:101,data$theta)



