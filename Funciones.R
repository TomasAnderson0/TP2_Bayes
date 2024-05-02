

#Calculo del punto actual y el del salto

valor_salto_1d = function(muestras, r_propuesta, i){
  p_actual = muestras[i-1]
  p_nuevo = r_propuesta(p_actual)
  puntos = c(p_actual, p_nuevo)
  return(puntos)
}

#Propuesta de salto

salto_1d = function(p_actual, p_nuevo, d_objetivo, d_propuesta) {
  f_nuevo = d_objetivo(p_nuevo)
  f_actual = d_objetivo(p_actual)
  q_actual = d_propuesta(p_actual, p_nuevo)
  q_nuevo = d_propuesta(p_nuevo, p_actual)
  prob = (f_nuevo*q_actual) / (q_nuevo*f_actual)
  alfa = min(1, prob)
  return(alfa)
}


#Saltamos?

test_salto_1d = function(alfa){
  result = rbinom(1,1,alfa)
  return(result)
}


# Punto generado

p_gen_1d = function(result, p_actual, p_nuevo) {
  if (result){
    return(p_nuevo)
  } else {
    return(p_actual)
  }
}



sample_mh_1d = function(n, p_inicial = 0, d_objetivo, r_propuesta = rnorm, d_propuesta = dnorm, v_random = NA){
  
  stopifnot(n > 0)
  muestras = numeric(n)

  if (length(v_random) == 2){
    muestras[1] = runif(1,v_random[1],v_random[2])
  } else {
    muestras[1] = p_inicial
  }
  
  for (i in 2:n) {
    puntos = valor_salto_1d(muestras, r_propuesta, i)
    
    p_actual = puntos[1]
    
    p_nuevo = puntos[2]
    
    prob = salto_1d(p_actual, p_nuevo, d_objetivo, d_propuesta)
    
    salto = test_salto_1d(prob)
    
    muestras[i] = p_gen_1d(salto, p_actual, p_nuevo)
  }
    return(muestras)
}
































#Calculo del punto actual y el del salto

valor_salto_2d = function(muestras, matrix_var, i){
  p_actual = muestras[i-1,]
  p_nuevo = rmvnorm(1, mean = p_actual, sigma = matrix_var)
  puntos = c(p_actual, p_nuevo)
  return(puntos)
}

#Propuesta de salto

salto_2d = function(p_actual, p_nuevo, d_objetivo, matrix_var) {
  f_nuevo = d_objetivo(p_nuevo)
  f_actual = d_objetivo(p_actual)
  q_actual = dmvnorm(p_actual, p_nuevo, sigma = matrix_var)
  q_nuevo = dmvnorm(p_nuevo, p_actual, sigma = matrix_var)
  prob = (f_nuevo*q_actual) / (q_nuevo*f_actual)
  alfa = min(1, prob)
  return(alfa)
}

#Saltamos?

test_salto_2d = function(alfa){
  result = rbinom(1,1,alfa)
  return(result)
  }

# Punto generado

p_gen_2d = function(result, p_actual, p_nuevo) {
  if (result){
     return(p_nuevo)
  } else {
    return(p_actual)
  }
  }



sample_mh_2d = function(n, p_inicial , d_objetivo, matrix_var = diag(1,2)){
  
  stopifnot(n > 0)
  muestras = matrix(nrow = n,ncol = 2)
  muestras[1,] = p_inicial
  
  for (i in 2:n) {
    puntos = valor_salto_2d(muestras, matrix_var, i)
    
    p_actual = puntos[1:2]
    
    p_nuevo = puntos[3:4]
    
    prob = salto_2d(p_actual, p_nuevo, d_objetivo, matrix_var)
    
    salto = test_salto_2d(prob)
    
    muestras[i,] = p_gen_2d(salto, p_actual, p_nuevo)
  }
  if (length(p_inicial) == 1) {
    return(as.vector(muestras))
  }
  return(muestras)
}




