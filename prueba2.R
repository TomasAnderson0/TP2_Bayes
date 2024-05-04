set.seed(69)
d_objetivo_normal = function (x) dmvnorm(x, mean = c(.4, .75), sigma = matrix(c(1.35, .4, .4, 2.4), nrow = 2))


muestra_normal = sample_mh_2d(1000, p_inicial = c(1,1), d_objetivo = d_objetivo_normal,
                              matrix_var = matrix(c(1.5, .4, .4, 2.4), nrow = 2))

muestra_normal_df = data.frame(obs = c(muestra_normal[,1],muestra_normal[,2]),
                               orden = rep(1:5000,2),
                               cadena = rep(c("x", "y"), each = 5000))

ggplot(muestra_normal_df) + geom_line(aes(x = orden, y = obs)) + facet_grid(vars(cadena))


nef = apply(muestra_normal, 2, FUN = neef)



var1 = seq(.85,1.85, by =.25)
var2 = seq(1.9, 2.9, by =.25)
cova = seq(.2, .6, by = .1)
var = expand.grid(var1, var2, cova)

nef = matrix(0,nrow = nrow(var), ncol = 2)
for (i in 1:nrow(var)) {
  set.seed(69)
  muestra_normal = sample_mh_2d(1000, p_inicial = c(.4, .75), d_objetivo = d_objetivo_normal,
                                matrix_var = matrix(c(var[i,1], var[i,3], var[i,3],
                                                      var[i,2]), nrow = 2, ncol = 2))
  nef[i,] = apply(muestra_normal, 2, FUN = neef)
}

which.max(nef[,1]+nef[,2])
var[85,]

