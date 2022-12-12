  # Importar Table_7_9_Modificada.xls
  pollos=Table_7_9_Modificada
  names(pollos)=c("anio", "q_pollo", "ingreso", "p_pollo" , "p_cerdo", "p_vac", "p_subs") 
  reg_p=lm(q_pollo~p_pollo+ p_cerdo+p_vac, data=pollos)
  summary(reg_p)
  # Trabajaremos con la regresión final
  reg_pF=lm(log(q_pollo)~log(p_pollo)+log(ingreso), data=pollos)
  summary(reg_pF)

  ##**********************************************************
  #                      HETEROCEDASTICIDAD
  ##**********************************************************
  ##*
  # install.packages("lmtest")
  library(lmtest)

  # Test de Heterocedasticidad Breusch Pagan Godfrey
  # Utilizar el Studentizado (es el default)
  # Dos formas equivalentes
  bptest(reg_pF)
  bptest(log(q_pollo)~log(p_pollo)+log(ingreso), data=pollos) 
  
  
  # Test de White (No hay un comando ya construído, por lo que se 
  # realizará modificando el BPG, agregando las variables al cuadrado)
  bptest(log(q_pollo)~log(p_pollo)+I(log(p_pollo)^2)+log(ingreso)+
           I(log(ingreso)^2), data=pollos) 
  
  # Con términos cruzados
  bptest(log(q_pollo)~log(p_pollo)+I(log(p_pollo)^2)+log(ingreso)+
           I(log(ingreso)^2)+I(log(ingreso)*log(p_pollo)), data=pollos) 
  
  # Errores estándar robustos a la heterocedasticidad
  # Es el que llamamos "Plan B"

  # install.packages("sandwich")
  library(sandwich)
   
  summary(reg_pF)
  coeftest(reg_pF, vcov = vcovHC(reg_pF, type="HC1"))
  # vcovHC es Heteroscedasticity consistent
  # vcovHAC es Heteroscedasticity and correlation consistent
  
  ##**********************************************************
  #                      AUTOCORRELACIÓN
  ##**********************************************************
  ##*# Durbin - Watson test
# Volvemos a la función de demanda de pollos
  dwtest(reg_pF, alternative="greater")
  dwtest(reg_pF, alternative="two.sided")
  dwtest(reg_pF, alternative="less")
  dwtest(reg_pF)
  # Fijarse que el estadístico es el mismo. Sólo cambia el p-valor
  # Porque las pruebas se hacen a cola izq. , dos colas o cola der.
  # respectivamente. El default es cola izquierda
  
  # Breusch y Godfrey BP test
  bgtest(reg_pF, order=1)
  bgtest(reg_pF, order=1,type ="Chisq")
  bgtest(reg_pF, order=1,type ="F")
  bgtest(reg_pF)
  # Usamos el orden 1 que también es default y la distr. chi cuadrado
  # Igual existe la posibilidad de hacer un test F (indicativo)

# MINIMOS CUADRADOS GENERALIZADOS

  # install.packages("nlme")
  library(nlme)
  
  reg_pGLS <- gls(log(q_pollo)~log(p_pollo)+log(ingreso), 
                  data=pollos, correlation=corAR1(), method="ML")
  summary(reg_pGLS)
  # Es una regresión por Máxima Verosimilitud, donde se estiman conjuntamente
  # los beta y el rho. Comparar con MCO
  summary(reg_pF)
  
  # Errores estándar robustos a la autocorrelación y la heterocedasticidad
  # Es el que llamamos "Plan B"

  coeftest(reg_pF, vcov = vcovHAC(reg_pF, type="HC1"))
  # vcovHC es Heteroscedasticity consistent
  # vcovHAC es Heteroscedasticity and correlation consistent
  



