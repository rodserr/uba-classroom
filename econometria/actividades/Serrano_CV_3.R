library(pder)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)   # Para el reporte

# Carga de Datos
data('Grunfeld', package = "plm")
data('Gasoline', package = "plm")
data("Produc", package = "plm")

grunfeld <- pdata.frame(Grunfeld, index = c('firm', 'year'))
gasoline <- pdata.frame(Gasoline, index = c('country', 'year'))
produc <- pdata.frame(Produc, index = c('state', 'year'))

# Se pide que se determine, con base en
# tests de especificación (F, LM y Hausman) , la adecuación o no de los modelos:
#   Pooling - Efectos Fijos de una y dos vías - Efectos Aleatorios de una y dos vías.

grun_formula <- "inv ~ value + capital"
grun_pool <- plm(grun_formula, grunfeld, model = 'pooling')
grun_w <- plm(grun_formula, grunfeld, model = 'within')
grun_w_two <- plm(grun_formula, grunfeld, model = 'within', effect = "twoways")
grun_rand <- plm(grun_formula, grunfeld, model = 'random')
grun_rand_two <- plm(grun_formula, grunfeld, model = 'random', effect = "twoways")

gasoline_formula <- 'lgaspcar~lincomep+lrpmg+lcarpcap'
gas_pool <- plm(gasoline_formula, gasoline, model = 'pooling')
gas_w <- plm(gasoline_formula, gasoline, model = 'within')
gas_w_two <- plm(gasoline_formula, gasoline, model = 'within', effect = "twoways")
gas_rand <- plm(gasoline_formula, gasoline, model = 'random')
gas_rand_two <- plm(gasoline_formula, gasoline, model = 'random', effect = "twoways")

prod_fromula <- 'log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp'
prod_pool <- plm(prod_fromula, produc, model = 'pooling')
prod_w <- plm(prod_fromula, produc, model = 'within')
prod_w_two <- plm(prod_fromula, produc, model = 'within', effect = "twoways")
prod_rand <- plm(prod_fromula, produc, model = 'random')
prod_rand_two <- plm(prod_fromula, produc, model = 'random', effect = "twoways")

# Tests--------
### F
pFtest(grun_w, grun_pool) # Rechaza, existe efectos infividuales
pFtest(grun_w_two, grun_w) # No Rechaza, no existe efectos temporales
pFtest(grun_w_two, grun_pool) # Rechaza, existe efectos infividuales

pFtest(gas_w, gas_pool) # Rechaza, existe efectos infividuales
pFtest(gas_w_two, gas_w) # Rechaza, existe efectos infividuales
pFtest(gas_w_two, gas_pool) # Rechaza, existe efectos infividuales

pFtest(prod_w, prod_pool) # Rechaza, existe efectos infividuales
pFtest(prod_w_two, prod_w) # Rechaza, existe efectos temporales
pFtest(prod_w_two, prod_pool) # Rechaza, existe efectos infividuales

### LM
# Probamos con honda porque corrige el error de rechazar por ambos lados de la distribucion 
plmtest(grun_pool, type = 'honda')
plmtest(grun_pool, effect="twoways", type = 'honda')

plmtest(gas_pool, effect="twoways")
plmtest(gas_pool, effect="twoways", type = 'honda')

plmtest(prod_pool, effect="twoways")
plmtest(prod_pool, effect="twoways", type = 'honda')

# Los datos reflejan la existencia de efectos fijos individuales y temporales. Habria que elegir entre
# dos estimadores: Within y GLS

### Hausman
# Si existe endogeneidad no puedo usar GLS

phtest(grun_w, grun_rand) # no rechaza
phtest(grun_w_two, grun_rand_two) # Rechaza

phtest(gas_w, gas_rand) # Rechaza
phtest(gas_w_two, gas_rand_two) # Rechaza

phtest(prod_w, prod_rand) # Rechaza
phtest(prod_w_two, prod_rand_two) # Rechaza

# No se puede usar GLS porque hay una diferencia significativa con respecto al modelo de efectos fijos
# debemos quedarnos con efectos fijos

# Comparacion de modelos
stargazer(grun_pool, grun_w, grun_w_two, grun_rand, grun_rand_two,
          column.labels = c("MCO", "Within", "Within-twoways", "GLS", "GLS-twoways"),
          title= "Comparacion de estimadores", type="text",
          digits=3)

stargazer(gas_pool, gas_w, gas_w_two, gas_rand, gas_rand_two,
          column.labels = c("MCO", "Within", "Within-twoways", "GLS", "GLS-twoways"),
          title= "Comparacion de estimadores", type="text",
          digits=3)

stargazer(prod_pool, prod_w, prod_w_two, prod_rand, prod_rand_two,
          column.labels = c("MCO", "Within", "Within-twoways", "GLS", "GLS-twoways"),
          title= "Comparacion de estimadores", type="text",
          digits=3)


