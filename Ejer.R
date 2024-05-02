library(ggplot2)
library(mvtnorm)

#Punto 2

kumaraswamy = function(a,b,x) {
  stopifnot(a > 0)
  stopifnot(b > 0)
  valor = a*b*(x^(a-1))*((1-(x^a))^(b-1))
  return(valor)
}

grilla = seq(0,1,.01)


runif(2,1,100)

parametros = matrix(c(2,2,10,2,5,15,1/2,2,1/2,1/3), nrow = 5, ncol = 2, byrow = T)

Valores = matrix(0,nrow = 101, ncol = 5)
for (i in 1:5) {
  Valores[,i] = kumaraswamy(parametros[i,1],parametros[i,2],grilla)
}

data_kuma = as.data.frame(Valores)

ggplot(data_kuma)+geom_line(aes(x = grilla, y = V1), color = "red")+
  geom_line(aes(x = grilla, y = V2), color = "yellow")+
  geom_line(aes(x = grilla, y = V3), color = "blue")+
  geom_line(aes(x = grilla, y = V4), color = "green")+
  geom_line(aes(x = grilla, y = V5), color = "brown")



# Muy facilmente se puede representar la creencia a priori 
# de parametros que esten entre 0 y 1.



#Punto 3 


r_beta_by_mean = function(n, media, con){
  alfa = media * con
  beta = con - alfa
  rbeta(n, alfa, beta)
}

d_beta_by_mean = function(x, media, con){
  alfa = media * con
  beta = con - alfa
  dbeta(x, alfa, beta)
}


d_objetivo_kuma = function(x) kumaraswamy(6, 2, x)

r_propuesta_kuma_1 = function(x) r_beta_by_mean(1, x, 2)

d_propuesta_kuma_1 = function(x, media) d_beta_by_mean(x, media, 2)

muestras_kuma_1 = sample_mh_1d(5000, v_random = c(0,1), d_objetivo = d_objetivo_kuma, 
                             d_propuesta = d_propuesta_kuma_1, 
                             r_propuesta = r_propuesta_kuma_1)

r_propuesta_kuma_2 = function(x) r_beta_by_mean(1, x, 10)

d_propuesta_kuma_2 = function(x, media) d_beta_by_mean(x, media, 10)

muestras_kuma_2 = sample_mh_1d(5000, v_random = c(0,1), d_objetivo = d_objetivo_kuma, 
                               d_propuesta = d_propuesta_kuma_2, 
                               r_propuesta = r_propuesta_kuma_2)

r_propuesta_kuma_3 = function(x) r_beta_by_mean(1, x, 1)

d_propuesta_kuma_3 = function(x, media) d_beta_by_mean(x, media, 1)

muestras_kuma_3 = sample_mh_1d(5000, v_random = c(0,1), d_objetivo = d_objetivo_kuma, 
                               d_propuesta = d_propuesta_kuma_3, 
                               r_propuesta = r_propuesta_kuma_3)



muestras_kuma = data.frame(obs = c(muestras_kuma_1, muestras_kuma_2, muestras_kuma_3),
                           orden = rep(1:5000,3),
                           cadena = as.factor(rep(1:3, each = 5000)))

ggplot(muestras_kuma) + geom_line(aes(x = orden, y = obs)) + facet_grid(vars(cadena))

1-mean(muestras_kuma_1[1:4999]-muestras_kuma_1[2:5000] == 0)
1-mean(muestras_kuma_2[1:4999]-muestras_kuma_2[2:5000] == 0)
1-mean(muestras_kuma_3[1:4999]-muestras_kuma_3[2:5000] == 0)


hist(muestras_kuma_1)
hist(muestras_kuma_2)
hist(muestras_kuma_3)

acf(muestras_kuma_1, lag.max = Inf)
acf(muestras_kuma_2, lag.max = Inf)
acf(muestras_kuma_3, lag.max = Inf)

mean(muestras_kuma_1)
mean(muestras_kuma_2)
mean(muestras_kuma_3)


mean(sort(muestras_kuma_1)[250:251])
mean(sort(muestras_kuma_2)[250:251])
mean(sort(muestras_kuma_3)[250:251])

mean(sort(muestras_kuma_1)[4750:4751])
mean(sort(muestras_kuma_2)[4750:4751])
mean(sort(muestras_kuma_3)[4750:4751])

logit_kuma_1 = log(muestras_kuma_1/(1-muestras_kuma_1))
logit_kuma_2 = log(muestras_kuma_2/(1-muestras_kuma_2))
logit_kuma_3 = log(muestras_kuma_3/(1-muestras_kuma_3))


mean(logit_kuma_1)
mean(logit_kuma_2)
mean(logit_kuma_3)


mean(sort(logit_kuma_1)[250:251])
mean(sort(logit_kuma_2)[250:251])
mean(sort(logit_kuma_3)[250:251])

mean(sort(logit_kuma_1)[4750:4751])
mean(sort(logit_kuma_2)[4750:4751])
mean(sort(logit_kuma_3)[4750:4751])

#Ejer 7

d_objetivo_normal = function (x) dmvnorm(x, mean = c(.4, .75), sigma = matrix(c(1.35, .4, .4, 2.4), nrow = 2))


muestra_normal = sample_mh_2d(1000, p_inicial = c(1,1), d_objetivo = d_objetivo_normal,
                              matrix_var = matrix(c(1, .3, .3, 1), nrow = 2))

muestra_normal_df = data.frame(obs = c(muestra_normal[,1],muestra_normal[,2]),
                           orden = rep(1:1000,2),
                           cadena = rep(c("x", "y"), each = 1000))

ggplot(muestra_normal_df) + geom_line(aes(x = orden, y = obs)) + facet_grid(vars(cadena))


apply(muestra_normal, 2, FUN = neef)


#Ejer 8
Prob_mh_norm_1 = mean(muestra_normal[, 1] > 1 & muestra_normal[, 2] < 0)
Prob_mh_norm_2 = mean(muestra_normal[, 1] > 1 & muestra_normal[, 2] > 2)
Prob_mh_norm_3 = mean(muestra_normal[, 1] > .4 & muestra_normal[, 2] > .75)


rmv = rmvnorm(100000, mean = c(.4, .75), sigma = matrix(c(1.35, .4, .4, 2.4), nrow = 2))
Prob_rmv_1 = mean(rmv[, 1] > 1 & rmv[, 2] < 0)
Prob_rmv_2 = mean(rmv[, 1] > 1 & rmv[, 2] > 2)
Prob_rmv_3 = mean(rmv[, 1] > .4 & rmv[, 2] > .75)


abs(Prob_mh_norm_1 - Prob_rmv_1)/Prob_rmv_1
abs(Prob_mh_norm_2 - Prob_rmv_2)/Prob_rmv_2
abs(Prob_mh_norm_3 - Prob_rmv_3)/Prob_rmv_3



#Ejer 9



rosenbrock = function (x) ((.5 - x(1))^2) + 5 * ((x(2) - (x(1)^2))^2)

d_objetivo_rosenbrock = function (x) exp((-1)*(((.5 - x[1])^2) + 5 * ((x[2] - (x[1]^2))^2)))

muestra_rosenbrock_1 = sample_mh_2d(1000, p_inicial = c(1,1), d_objetivo = d_objetivo_rosenbrock,
                              matrix_var = matrix(c(1, .7, .7, 1), nrow = 2))
muestra_rosenbrock_2 = sample_mh_2d(1000, p_inicial = c(1,1), d_objetivo = d_objetivo_rosenbrock,
                                    matrix_var = matrix(c(1, .3, .3, 1), nrow = 2))
muestra_rosenbrock_3 = sample_mh_2d(1000, p_inicial = c(1,1), d_objetivo = d_objetivo_rosenbrock,
                                    matrix_var = matrix(c(1,0,0,2), nrow = 2))


muestra_rosenbrock_df_1 = data.frame(obs = c(muestra_rosenbrock_1[,1],muestra_rosenbrock_1[,2]),
                               orden = rep(1:1000,2),
                               cadena = rep(c("x", "y"), each = 1000))

muestra_rosenbrock_df_2 = data.frame(obs = c(muestra_rosenbrock_2[,1],muestra_rosenbrock_2[,2]),
                                     orden = rep(1:1000,2),
                                     cadena = rep(c("x", "y"), each = 1000))

muestra_rosenbrock_df_3 = data.frame(obs = c(muestra_rosenbrock_3[,1],muestra_rosenbrock_3[,2]),
                                     orden = rep(1:1000,2),
                                     cadena = rep(c("x", "y"), each = 1000))


ggplot(muestra_rosenbrock_df_1) + geom_line(aes(x = orden, y = obs)) + facet_grid(vars(cadena))

ggplot(muestra_rosenbrock_df_2) + geom_line(aes(x = orden, y = obs)) + facet_grid(vars(cadena))

ggplot(muestra_rosenbrock_df_3) + geom_line(aes(x = orden, y = obs)) + facet_grid(vars(cadena))

#Ejer 10

Prob_mh_rosen_1_1 = mean(muestra_rosenbrock_1[, 1] < 1 & muestra_rosenbrock_1[, 1] > 0 & muestra_rosenbrock_1[, 2] > 0 & muestra_rosenbrock_1[, 2] < 1)
Prob_mh_rosen_1_2 = mean(muestra_rosenbrock_1[, 1] < 0 & muestra_rosenbrock_1[, 1] > -1 & muestra_rosenbrock_1[, 2] > 0 & muestra_rosenbrock_1[, 2] < 1)
Prob_mh_rosen_1_3 = mean(muestra_rosenbrock_1[, 1] < 2.2 & muestra_rosenbrock_1[, 1] > 1 & muestra_rosenbrock_1[, 2] > 2 & muestra_rosenbrock_1[, 2] < 3)


# r_rosen = ?
Prob_rmv_1 = mean(r_rosen[, 1] > 1 & r_rosen[, 2] < 0)
Prob_rmv_2 = mean(r_rosen[, 1] > 1 & r_rosen[, 2] > 2)
Prob_rmv_3 = mean(r_rosen[, 1] > .4 & r_rosen[, 2] > .75)


abs(Prob_mh_1 - Prob_rmv_1)/Prob_rmv_1
abs(Prob_mh_2 - Prob_rmv_2)/Prob_rmv_2
abs(Prob_mh_3 - Prob_rmv_3)/Prob_rmv_3
