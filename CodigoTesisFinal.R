
# libraries ---------------------------------------------------------------


library(tidyverse) # manejo general
library(lavaan) # análisis factorial
library(openxlsx) # leer archivos de excel
library(psych)
library(skimr) # explorar datos


# import data -------------------------------------------------------------

datos <- openxlsx::read.xlsx("data/ResultadosX.xlsx")

datos_corregidos <- datos %>% 
  dplyr::mutate(across(7:34, as.numeric))

# análisis exploratorio ---------------------------------------------------

## Exploración inicial 
##Analiza los datos puros para ver como se comportan las preguntas y si las personas lo llenaron o no. 

skimr::skim(datos_corregidos)

## Matriz de correlación

### Opción  1

corrplot::corrplot(cor(datos_corregidos[, -c(1:6)], use="complete.obs"), 
                   method = "square",   ## circle, square, shade, pie 
                   type = "lower",
                   tl.col = "black", 
                   bg = "white",  
                   title = "",    
                   col = NULL)

### Opción 2

## psych::corPlot(datos_corregidos[, -c(1:6)])  Otro grafico mas pesado. 


# reliability  ------------------------------------------------------------

alpha_1 <- psych::alpha(datos_corregidos[, -c(1:6)], check.keys = TRUE)  ## Le estamos quitando las primeras 6 columnas
## Check.key se le a vuelta a cp5
alpha_1


## Se sugiere eliminar CP5  Me dice 

alpha_2 <- psych::alpha(datos_corregidos[, -c(1:6, 15)], check.keys = TRUE)  ##Le estamos quitando el CP5

alpha_2

## Se sugiere eliminar CP5  Me dice 

alpha_3 <- psych::alpha(datos_corregidos[, -c(1:6, 11, 15)], check.keys = TRUE)  ##Le estamos quitando el CP5

alpha_3


##KMO  Este es kayser mayer olking    (Forro fn f1) Me puede decir que cosas puedo quitar.Adecuacion a analisis de factores 
psych::KMO(cor(datos_corregidos[, -c(1:6)], use="complete.obs"))

psych::KMO(cor(datos_corregidos[, -c(1:6,15,24)], use="complete.obs"))


# modelos -----------------------------------------------------------------

## Modelo 01: Con CP5

# datos_corregidos2 <- datos_corregidos %>%  ##Le damos vuelta a CP5. Para ver si converge
#   dplyr::mutate(CP5 = case_when(CP5 == 5 ~ 1, 
#                                 CP5 == 4 ~ 2,
#                                 CP5 == 3 ~ 3,
#                                 CP5 == 2 ~ 4,
#                                 CP5 == 1 ~ 5,
#                                 TRUE ~ NA))
# 
 estructura_01 <- 'sa  =~ SA1 + SA2 + SA3 + SA4 
               cp =~ CP1 + CP2 + CP3 + CP4 + CP5
               ce =~ CE1 + CE2 + CE3 + CE4 
               ci =~ CI1 + CI2 + CI3 + CI4 
               p =~ P1 + P2 + P3 + P4 + P5
               e =~ E1 + E2 + E3 + E4 + E5+ E6'
 
 mod_01 <- cfa(estructura_01, data = datos_corregidos, estimator="uls")  

summary(mod_01)
 
fitMeasures(mod_01)

## Modelo 02: Sin CP5, por que alpha lo dice, y ademas vimos en el campo que se enredaban. 

estructura_02 <- 'sa  =~ SA1 + SA2 + SA3 + SA4 
              cp =~ CP1 + CP2 + CP3 + CP4
              ce =~ CE1 + CE2 + CE3 + CE4 
              ci =~ CI1 + CI2 + CI3 + CI4 
              p =~ P1 + P2 + P3 + P4 + P5
              e =~ E1 + E2 + E3 + E4 + E5+ E6'

mod_02 <- cfa(estructura_02, data = datos_corregidos)  #No esta convergiendo. Por las varianzas negativas. 

summary(mod_02, standardized= TRUE)

fitMeasures(mod_02)

## Modelo 03: Sin CP5 ni  ni P1  
##EL ELEGIDO!
### Necesario por problemas de convergencia

estructura_03 <- 'sa  =~ SA1 + SA2 + SA3 + SA4 
              cp =~  CP1 + CP2 + CP3 + CP4
              ce =~ CE1 + CE2 + CE3 + CE4 
              ci =~ CI1 + CI2 + CI3 + CI4 
              p =~ P2  +P3+ P4 + P5
              e =~ E1 + E2 + E3 + E4 + E5+ E6'

mod_03 <- cfa(estructura_03, data = datos_corregidos)

summary(mod_03,fit.measures = TRUE, standardized=TRUE)

fitMeasures(mod_03)

anova(mod_03, mod_02)  ##con estos comparamos modelo 2 NO convergente con 3
##Busco que AIC y BIC sea el menor lrt test de razon de verosimilitudes


## Para comparar modelos busquen y utilicen al menos dos absolutos y
## dos comparativos o relativos (ahorita no preciso la diferencia)


# diagrama ----------------------------------------------------------------

semPlot::semPaths(mod_03, exoCov = FALSE, 
                  whatLabels = "std", 
                  label.prop = 0.8,
                  what = "cons", style = "lisrel",
                  sizeMan = 4,
                  sizeLat = 7, thresholds = FALSE,
                  edge.label.cex = 0.75, 
                  residuals = TRUE,
                  rotation = 1)


citation("corrplot")
citation("semPlot")

citation("blavaan")
