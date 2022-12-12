library(pder)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)   # Para el reporte

# Carga de Datos
data("Gasoline")
gasoline <- pdata.frame(Gasoline, index = c('country', 'year'))

# a) Regrese un modelo Pooling (MCO)
summary(pool <- plm(lgaspcar~lincomep+lrpmg+lcarpcap, gasoline, model = 'pooling'))
coeftest(pool, vcov = vcovHC)

# b) Regrese modelos Within con efectos individuales.
summary(within_model <- plm(lgaspcar~lincomep+lrpmg+lcarpcap, gasoline, model = 'within', effect = "individual"))
coeftest(within_model, vcov=vcovHC)

# c) Implemente tests de hipotesis para la presencia de estos efectos y en función del resultado de los mismos 
#    decida entre los modelos estimados cuál resultaría el más adecuado.
pFtest(within_model, pool)
# Rechazo, Hay diferencia entre los modelos por lo tanto debemos considerar los efectos individuales.
# El mas adecuado seria el modelo within con efectos individuales

# d) Incorpore la posibilidad de modelar los efectos encontrados con un modelo de efectos aleatorios (GLS). 
#    Regrese dicho modelo utilizando las alternativas provistas por el software (random.method). 
#    Utilice todas las opciones de estimación que provee el software como “random.method”.

rand_model_swar=plm(lgaspcar~lincomep+lrpmg+lcarpcap, gasoline, model = "random", effect="individual", random.method="swar")
summary(rand_model_swar, vcov=vcovHC)

rand_model_amemiya=plm(lgaspcar~lincomep+lrpmg+lcarpcap, gasoline, model = "random", effect="individual", random.method="amemiya")
summary(rand_model_amemiya, vcov=vcovHC)

rand_model_walhus=plm(lgaspcar~lincomep+lrpmg+lcarpcap, gasoline, model = "random", effect="individual", random.method="walhus")
summary(rand_model_walhus, vcov=vcovHC)

rand_model_nerlove=plm(lgaspcar~lincomep+lrpmg+lcarpcap, gasoline, model = "random", effect="individual", random.method="nerlove")
summary(rand_model_nerlove, vcov=vcovHC)

# e) Decida, en el contexto de efectos aleatorios, entre el estimador MCO y el estimador GLS
plmtest(pool)
# Rechazo, en el contexto aleatorio hay efectos individuales significativos por lo tanto se debe utilizar el estimador GLS

# f) Decida entre el modelo Within y el modelo GLS considerando la posible endogeneidad de las variables explicativas,
#    utilizando el test de Hausman. Intente utilizar la versión corregida por heterocedasticidad 
#    (en ocasiones esta opción no puede calcularse).
phtest(within_model, rand_model_swar)
# El modelo de efectos aleatorios es inconsistente, por lo tanto nis quedamos con el modelo within

# robust Hausman test (regression-based con ajuste por heterocedasticidad)
phtest(lgaspcar~lincomep+lrpmg+lcarpcap, data = gasoline, method = "aux", vco = vcovHC)
# El test corregido por heterocedasticidad tambien rechaza, lo que confirma la desicion anterior

# g) Regrese el modelo con el estimador Between (sólo tiene que consignar model=”between”).
summary(betw_model <- plm(lgaspcar~lincomep+lrpmg+lcarpcap, gasoline, model = 'between'))

# h) Fíjese la interpretación que se le da en el paper a los estimadores within y between respectivamente en relación con
#    el corto y el largo plazo. Comente

# El coeficiente del ingreso per capita varia considerablemente entre los modelos between y within. 
# El modelo within solo toma la data historica dentro de cada pais correspondiente, y no logra establecer 
# una relacion tan fuerte como el modelo between que toma en cuenta las relaciones entre paises.
# 
# Esto hace que el modelo between se relacione con efectos a largo plazo, ya que logra captar la relacion 
# entre las variables en varios individuos, en este caso paises.
# Mientras que el modelo within al solo contemplar datos de la serie de tiempo dentro del pais correspondiente
# se relaciona a efectos a corto plazo.

# i) Confeccione una tabla resumen con los parámetros obtenidos y sus errores estándar robustos mediante el paquete “stargazer” u otra herramienta,
#    que incluya los modelos Pooling, Within, Between y GLS en sus distintas versiones, tal como se hace en el paper (ver Tabla 2)
#    y en el libro de Baltagi (ver Tabla 2.5)
rob_p=sqrt(diag(vcovHC(pool, type = "HC0")))
rob_Wi=sqrt(diag(vcovHC(within_model, type = "HC0")))
rob_Bi=summary(betw_model)$coefficients[,'Std. Error'] # El modelo Between no aplica errores robustos
rob_Ri_swar=sqrt(diag(vcovHC(rand_model_swar, type = "HC0")))
rob_Ri_amemiya=sqrt(diag(vcovHC(rand_model_amemiya, type = "HC0")))
rob_Ri_walhus=sqrt(diag(vcovHC(rand_model_walhus, type = "HC0")))
rob_Ri_nearlove=sqrt(diag(vcovHC(rand_model_nerlove, type = "HC0")))
stargazer(pool, within_model, betw_model, rand_model_swar, rand_model_amemiya, rand_model_walhus, rand_model_nerlove,
          column.labels = c("MCO","Within","Between", "GLS-swar", "GLS-amemiya", "GLS-walhus", "GLS-nearlove"),
          se = list(rob_p, rob_Wi, rob_Bi, rob_Ri_swar, rob_Ri_amemiya, rob_Ri_walhus, rob_Ri_nearlove),
          title= "Comparacion de estimadores", type="text",
          digits=3)


# j) Comente las diferencias y similitudes de sus resultados con los que obtuvieron Baltagi y Griffin y elabore sus propias conclusiones

# Los resultados son los mismos obtenidos por Baltagi y Griffin. Por lo que las conclusiones tambien son las 
# compartidas en el paper mencionado. Todos los modelos arrojan el mismo signo pero varian en cuanto a la magnitud de los coeficientes,
# siendo la variable del precio de la gasolina la que arroja mas variacion entre los modelos.
# Vemos que el ingreso per capita tiene efecto positivo en el consumo de gasolina, caso contrario 
# con el  precio. Igualmente incrementar los autos per capita reduce el uso de los mismos, por lo que el
# efecto es negativo, mas autos por persona equivale a una disminucion del consumo de gasolina
