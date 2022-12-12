# install.packages("pglm")
# install.packages("splm")
library(pglm)
library(splm)
library(plm)
library(pder)
library(sandwich)
library(lmtest) #for `coeftest()` and `bptest()`.

#*********************************************************
# SELECCIÓN DE MODELOS UTILIZANDO TESTS DE ESPECIFICACIÓN
#*********************************************************
data("RiceFarms", package = "splm")
Rice <- pdata.frame(RiceFarms, index = c("id","time"))
pdim(Rice)
rice.w <- plm(log(goutput) ~ log(seed) + log(totlabor) + log(size), Rice)
rice.p <- plm(log(goutput) ~ log(seed) + log(totlabor) + log(size), Rice, model = "pooling")
rice.wd <- plm(log(goutput) ~ log(seed) + log(totlabor) + log(size), Rice,
               effect = "twoways")

# Tests de efectos individuales - F 
# Aplica en el contexto de Efectos Fijos
# Compara within one way con pooling
pFtest(rice.w, rice.p)

# compara within two ways con within one way:
pFtest(rice.wd, rice.w)

# compara within two ways con pooling:
pFtest(rice.wd, rice.p)

# Breusch and Pagan (1980) LM test 
# Aplica en el contexto de Efectos Aleatorios
# El primer argumento es el modelo pooling, 
# El segundo el tipo de efecto testeado
# effect = c("individual", "time", "twoways"),
# El tercero el tipo de test (BP, Honda, etc.)
# type = c("honda", "bp", "ghm", "kw"), 

plmtest(rice.p, effect="individual", type="bp")
plmtest(rice.p, effect="time", type="bp")
plmtest(rice.p, effect="twoways", type="bp")

plmtest(rice.p, effect="individual", type="honda")
plmtest(rice.p, effect="time", type="honda")
plmtest(rice.p, effect="twoways", type="honda")

plmtest(rice.p, effect="individual", type="kw")
plmtest(rice.p, effect="time", type="kw")
plmtest(rice.p, effect="twoways", type="kw")

# ghm sólo para two ways
plmtest(rice.p, effect="twoways", type="ghm")

# Tests por efectos individuales correlacionados con las explicativas
# Hausman test
rice.r <- update(rice.w, model = "random")
rice.rd <- update(rice.w, model = "random", effects="twoways")
phtest(rice.w, rice.r)
phtest(rice.wd, rice.rd)


#*********************************************************
#     ERRORES ESTÁNDAR ROBUSTOS EN DATOS DE PANEL
#*********************************************************
#******************************************************
#  IMPACTO DE LA OBRA PÚBLICA EN LA ACTIVIDAD ECONÓMICA
#******************************************************
library("plm")
data("Produc", package = "plm")
# Fórmula de la regresión
fm <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp

# Estimación con MCO y SE robustos
lmmod <- lm(fm, Produc)
coeftest(lmmod, vcov = vcovHC)

# Idem con plm pooling
plmmod <- plm(fm, Produc, model = "pooling")
summary(plmmod, vcov = vcovHC)

# Los errores estándar son diferentes, a pesar de
# que la matriz vcov invocada es la misma. Se debe
# al diferente contexto. plm ya indica panel

# Si se quisiera replicar los errores robustos de 
# MCO habría que indicar vcovHC(x, method = "white1", type="HC3")
summary(plmmod, vcov = vcovHC(plmmod, method = "white1",type="HC3"))

# Para tener en cuenta la autocorrelación temporal, pero también 
# la espacial (entre estados) según Driscoll y Kray
coeftest(plmmod, vcov=vcovSCC)

#******************************************************
# Creación de una función para obtener todos los SE posibles
#******************************************************
Vw <- function(x) vcovHC(x, method = "white1")
Vcx <- function(x) vcovHC(x, cluster = "group", method = "arellano")
Vct <- function(x) vcovHC(x, cluster = "time", method = "arellano")
Vcxt <- function(x) Vcx(x) + Vct(x) - Vw(x)
Vct.L <- function(x) vcovSCC(x, wj = function(j, maxlag) 1)
Vnw.L <- function(x) vcovNW(x)
Vscc.L <- function(x) vcovSCC(x)
Vcxt.L <- function(x) Vct.L(x) + Vcx(x) - vcovNW(x, wj = function(j, maxlag) 1)

vcovs <- c(vcov, Vw, Vcx, Vct, Vcxt, Vct.L, Vnw.L, Vscc.L, Vcxt.L)
names(vcovs) <- c("OLS", "Vw", "Vcx", "Vct", "Vcxt", "Vct.L", "Vnw.L",
                  "Vscc.L", "Vcxt.L")

#******************************************************
#*              Función
#******************************************************
cfrtab <- function(mod, vcovs, ...) {
  cfrtab <- matrix(nrow = length(coef(mod)), ncol = 1 + length(vcovs))
  dimnames(cfrtab) <- list(names(coef(mod)),
                           c("Coefficient", paste("s.e.", names(vcovs))))
  cfrtab[,1] <- coef(mod)
  for(i in 1:length(vcovs)) {
    myvcov = vcovs[[i]]
    cfrtab[ , 1 + i] <- sqrt(diag(myvcov(mod)))
  }
  return(t(round(cfrtab, 4)))
}
#******************************************************
# Obtengo el cuadro completo
cfrtab(plmmod, vcovs)

#**********************************************************
#  APLICACIÓN DE ERRORES ESTÁNDAR ROBUSTOS A SERIES
#   DE CORTES TRANSVERSALES
#**********************************************************
#*PRECIOS HEDONICOS DE LAS PROPIEDADES
data("Hedonic", package = "plm")
# Fórmula
hfm <- mv ~ crim + zn + indus + chas + nox + rm + age + dis +
  rad + tax + ptratio + blacks + lstat
# Se corre con lm y los errores robustos se calculan así
hlmmod <- lm(hfm, Hedonic)
coeftest(hlmmod, vcov = vcovHC)

hplmmod <- plm(hfm, Hedonic, model = "pooling", index = "townid")

sign.tab <- cbind(coef(hlmmod), coeftest(hlmmod, vcov = vcovHC)[,4],
                  coeftest(hplmmod, vcov = vcovHC)[, 4])
dimnames(sign.tab)[[2]] <- c("Coefficient", "p-values, HC", "p-val., cluster")
round(sign.tab, 3)

#**********************************************************
# MOdelo de Inversion Q de Tobin Schaller(1990)
#**********************************************************
#*  Analizar cuál es el modelo adecuado para ambas fórmulas
#*  Calcular los errores robustos (arellano)
#*  
data("TobinQ",package="pder")
pTobinQ <- pdata.frame(TobinQ, index = c('cusip', 'year'))
pdim(pTobinQ)

# Regresiones Broad
Qeqb=ikb~qb

# Regresiones Narrow
Qeqn <- ikn ~ qn
