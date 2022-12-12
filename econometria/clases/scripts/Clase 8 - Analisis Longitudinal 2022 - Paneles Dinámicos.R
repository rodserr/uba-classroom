# install.packages("plm")
# install.packages("pder")
# install.packages("stargazer")

library("plm")
library("pder")
library(stargazer)   # Para el reporte

data("DemocracyIncome", package = "pder")
pdim(DemocracyIncome)
View(DemocracyIncome)
Demo_5=pdata.frame(DemocracyIncome,index = c("country", "year"))

# Antes de hacer la regresión dinámica con GMM voy a setear las 
# cotas  Within <= GMM <= MCO 

# MCO con dummies temporales y sin intercepto solo cuando sample == 1
ols1 <- plm(democracy ~ lag(democracy) + lag(income) + year - 1,
           Demo_5, model = "pooling", subset = sample == 1)
coeftest(ols1)

# Idem within con efecto time (da lo mismo)
ols2 <- plm(democracy ~ lag(democracy) + lag(income), Demo_5, 
            model = "within", effect = "time", subset = sample == 1)
coeftest(ols2)

# Ahora regresamos con Within (twoways para mantener las dummies temporales)
within <- update(ols2, effect = "twoways")
coef(summary(within))

stargazer(ols2,within, title= "Cotas rho", digits=6,
          column.labels=c("MCO","Within"), type="text")

###############################################################
# Regresión en diferencias de Anderson y Hsiao
# Arellano ha demostrado que el nivel rezagado es mucho mejor
# instrumento que la diferencia rezagada

ahsiao_1 <- plm(diff(democracy) ~ lag(diff(democracy)) +
                lag(diff(income)) + year - 1 |
                lag(democracy, 2) + lag(income, 2) + year - 1,
              Demo_5, model = "pooling", subset = sample == 1)

coef(summary(ahsiao_1))[1:2,]    # Para que no reporte las dummies

# Instrumentando con la diferencia rezagada (instrumento débil)
ahsiao_2 <- plm(diff(democracy) ~ lag(diff(democracy)) +
                lag(diff(income)) + year - 1 |
                  lag(diff(democracy), 2) + lag(income, 2) + year - 1,
              Demo_5, model = "pooling", subset = sample == 1)

stargazer(ahsiao_1,ahsiao_2, title= "Estimaciones de Anderson y Hsiao", digits=6,
          column.labels=c("Inst. Nivel","Inst. Dif."), omit=c(seq=3:11),type="text")

##############################################################
# Estimador GMM de un paso
diff1 <- pgmm(democracy ~ lag(democracy) + lag(income) |
                lag(democracy, 2:99)| lag(income, 2),
              Demo_5,model="onestep", effect="twoways", subset = sample == 1)

coef(summary(diff1))

# Estimador GMM de dos pasos (Arellano - Bond)
diff2 <- update(diff1, model = "twosteps")
coef(summary(diff2))

##################################################################

# install.packages("plm")
# install.packages("pder")
# install.packages("stargazer")

library("plm")
library("pder")
library(stargazer)   # Para el reporte

###################################################################

# Ahora trabajamos con otro panel de 25 países con datos cada 25 años  
data("DemocracyIncome25", package = "pder")
View(DemocracyIncome25)
Demo_25=pdata.frame(DemocracyIncome25,index = c("country", "year"))
pdim(Demo_25)

# Estimamos OLS y luego Within para establecer las cotas
ols_25_1 <- plm(democracy ~ lag(democracy) + lag(income)+year-1 ,
                Demo_25, model = "pooling")
coef(summary(ols_25_1))

ols_25_2 <- plm(democracy ~ lag(democracy) + lag(income) ,
                Demo_25, model = "within", effect="time")
coef(summary(ols_25_2))

with_25 <- plm(democracy ~ lag(democracy) + lag(income) ,
               Demo_25, model = "within", effect="twoways")
coef(summary(with_25))


# Estimamos Arellano - Bond sin límite de lags
diff25_1 <- pgmm(democracy ~ lag(democracy) + lag(income) |
                   lag(democracy, 2:99) + lag(income, 2:99),
                 Demo_25, model = "onestep", effect = "twoways")
coef(summary(diff25_1))

diff25_2 <- pgmm(democracy ~ lag(democracy) + lag(income) |
                   lag(democracy, 2:99) + lag(income, 2:99),
                 Demo_25, effect = "twoways",model = "twosteps")
coef(summary(diff25_2))

# Reducimos lags hasta el cuarto
diff25lim <- pgmm(democracy ~ lag(democracy) + lag(income) |
                    lag(democracy, 2:4) | lag(income,2),
                  Demo_25, model="twosteps", effect="twoways")
coef(summary(diff25lim))

# Colapsamos los momentos
diff25coll <- pgmm(democracy ~ lag(democracy) + lag(income) |
                     lag(democracy, 2:99)+ lag(income, 2:99),
                   Demo_25, model="twosteps", effect="twoways",
                   collapse = TRUE)
coef(summary(diff25coll))

sapply(list(diff25_2, diff25lim, diff25coll), function(x) coef(x)[1:2])

#***************************************************
#*Modelo en diferencias y niveles
#*Primero para el caso de la base de datos Demo_5
sys25_1 <- pgmm(democracy ~ lag(democracy) + lag(income) |
                  lag(democracy, 2:99)| lag(income, 2:99),
                Demo_5, model = "twosteps", effect = "twoways",
                transformation = "ld", subset = sample == 1)

coef(summary(sys25_1))

# Ahora para los datos de 25 países
sys25_2 <- pgmm(democracy ~ lag(democracy) + lag(income) |
                  lag(democracy, 2:99)| lag(income, 2),
                Demo_25,  model = "twosteps", effect = "twoways",
                transformation = "ld")
coef(summary(sys25_2))

# **********************************************************
# Inferencia
# **********************************************************
# Test de Sargan
sargan(diff2)

sargan(sys2)

# Test de autocorrelación 
mtest(diff2, order = 2)

# Práctica

