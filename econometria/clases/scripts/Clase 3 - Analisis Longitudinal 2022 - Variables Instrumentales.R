  ###########################################################
  #             Variables Instrumentales
  ###########################################################
  # install.packages("AER")
  library(AER)
  
#  install.packages("lmtest")
  library(lmtest)
  
#  install.packages("sandwich")
  library(sandwich)
  
#  install.packages("stargazer")
  library(stargazer)
  
  #------------------------------------------------------------
  #   Ejemplo 1
  #------------------------------------------------------------
  

  # Cargar archivo STATA "educ_padres.dta"
  mco_p=lm(logsal~educa+expe+expe2+hombre, data=educ_padres)
  summary(mco_p)

  # Test de Heterocedasticidad Breusch Pagan Godfrey
  bptest(mco_p)
  
  # Para coeftest necesito lmtest y sandwich
  coeftest(mco_p, vcov = vcovHC(mco_p, type="HC1"))
  # NO hay mucha diferencia en este caso en los std. err.
  # El consejo es usar siempre errores robustos en corte 
  # transversal

  iv_p=ivreg(logsal~educa+expe+expe2+hombre|educa_p+
               expe+expe2+hombre, data=educ_padres)
  summary(iv_p, diagnostics=T)
  summary(iv_p, vcov = sandwich, diagnostics = TRUE)
  
  
  # Lo mismo en 2 etapas
  reg_et1=lm(educa~educa_p+expe+expe2+hombre, data=educ_padres)
  educ_padres$educa_hat=reg_et1$fitted.values
  educ_padres$educa_res=reg_et1$residuals
  
  reg_et2=lm(logsal~educa_hat+expe+expe2+hombre, data=educ_padres)
  summary(reg_et2)
  ## OJO, la inferencia no es válida cuando lo hago "artesanal"
  
  # Test de Hausman
  reg_haus=lm(logsal~educa+expe+expe2+hombre+educa_res, data=educ_padres)
    # Ver que usamos X y no X_hat
  summary(reg_haus)
  summary(reg_haus,vcov=vcovHC(reg_et1, type="HC1"), test="F")
  # El estadístico F es t^2 muy aproximadamente y el p-value es casi igual
  
  # Test de instrumentos débiles
  summary(reg_et1)
  waldtest(reg_et1,c(1))
  waldtest(reg_et1,c(1), vcov=vcovHC(reg_et1, type="HC1"), test="F")
  # Con la matriz de White da muy parecido 
  
  #------------------------------------------------------------
  #   Ejemplo 2
  #------------------------------------------------------------
  # Ejercicio de Stock y Watson  "Introducción a la Econometría"
  # Se desea determinar si los impuestos sobre los cigarrillos 
  # tienden a desalentar el consumo. 
  # Con ese fin se recaban datos de los 48 estados de EEUU para el año 1995.
  
  # packs: Consumo de paquetes de cigarrillos per cápita
  # Pop: Población del estado
  # rprice: Precio promedio del paquete (centavos de dólar de 1980)
  # rincome: Ingreso per cápita anual en miles de dólares de 1980
  # tdiff: Impuestos extra sobre cada paquete de cigarrillos, 
  # por sobre el nivel general de impuestos (centavos de dólar de 1980)
  # Rtax: Impuestos nivel general para cada paquete de cigarrillos (centavos de dólar de 1980) 
  
  data("CigarettesSW", package = "AER") 
  # Solo trabajamos con el año 1995
  cigs = subset(CigarettesSW, year==1995)
  cigs$rprice = cigs$price/cigs$cpi  # [centavos/paquete]
  cigs$rincome = (cigs$income/cigs$population)/cigs$cpi #[miles U$S/año]
  cigs$tdiff = (cigs$taxs - cigs$tax)/cigs$cpi
  
  # Regresiones MCO y IV
  mco_c = lm(log(packs) ~ log(rprice), data = cigs)  
  summary(mco_c) 
  
  # Test de Heterocedasticidad Breusch Pagan Godfrey
  bptest(mco_c)
  
  # Para coeftest necesito lmtest y sandwich
  coeftest(mco_c, vcov = vcovHC(mco_c, type="HC1"))
  

  # Busco errores estándar robustos
  coeftest(mco_c, vcov = vcovHC(mco_c, type="HC1"))
  
  # Ahora la regresión con IV
  iv_c1 = ivreg(log(packs) ~ log(rprice) | tdiff , data = cigs)
  summary(iv_c1, diagnostics=T)
  summary(iv_c1, vcov = sandwich, diagnostics = TRUE)
  
  # Ojo, por default carga los errores no robustos
  stargazer(mco_c, iv_c1,type="text")

  # Agregamos otro instrumento (impuestos generales)
  
  iv_c2 = ivreg(log(packs) ~ log(rprice) +log(rincome)| log(rincome) +
                  tdiff + I(tax/cpi), data = cigs)   
  summary(iv_c2, vcov = sandwich, diagnostics=T)
  
  # El test J de Sargan
  aux=lm(iv_c2$residuals~log(rincome)+ tdiff + I(tax/cpi),data=cigs)
  waldtest(aux, c(2,3),test="F")
  # J=2*Ftest
  # gl=2-1=1
  
  stargazer(mco_c, iv_c1,iv_c2,type="text")
  
  
  
  #------------------------------------------------------------
  #   Ejemplo 3  (Stock y Watson)
  #------------------------------------------------------------
  # 0)  Cargar STATA file fertility_small
  
  # Para ver la influencia de la maternidad sobre el 
  # trabajo femenino regresar semanas trabajadas por año sobre
  # mas de dos hijos (Ver paper Angrist y Evans)
  
  #  1) regresar mco weeksm1 contra morekids
  
  # Entendiendo que Hijos -> Trabajo , pero también
  # Trabajo -> Hijos 
  #  2) se instrumenta morekids con samesex
  
  # Se verifica si es un instrumento débil y se verifica 
  # Hausman
  
  # 3) Se agregan variables de control: black, hispan, othrace y 
  #    agem1 (edad de la madre en el año del censo 1979) y se 
  #    reestima por IV
  