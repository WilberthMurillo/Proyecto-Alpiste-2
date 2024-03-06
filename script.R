
# libraries ---------------------------------------------------------------

library(tidyverse) # manejo general
library(lavaan) # análisis factorial
library(openxlsx) # leer archivos de excel
library(psych)
library(skimr) # explorar datos
library(blavaan)
library(rjags)


# import data -------------------------------------------------------------

datos <- openxlsx::read.xlsx("data/ResultadosX.xlsx")

datos_corregidos <- datos %>% 
  dplyr::mutate(across(7:34, as.numeric))

# análisis exploratorio ---------------------------------------------------

## Exploración inicial

skimr::skim(datos_corregidos)

## Matriz de correlación

### Opción  1

corrplot::corrplot(cor(datos_corregidos[, -c(1:6)], use="complete.obs"), 
                   method = "shade",
                   type = "lower",
                   tl.col = "black", 
                   bg = "white",  
                   title = "",    
                   col = NULL)

# reliability  ------------------------------------------------------------

alpha_1 <- psych::alpha(datos_corregidos[, -c(1:6)], check.keys = TRUE)

alpha_1

## Se sugiere eliminar CP5

alpha_2 <- psych::alpha(datos_corregidos[, -c(1:6, 15)], check.keys = TRUE)

alpha_2

## KMO
### Con CP5
psych::KMO(cor(datos_corregidos[, -c(1:6)], use="complete.obs"))

### Sin CP5

psych::KMO(cor(datos_corregidos[, -c(1:6, 15)], use="complete.obs"))

# modelos -----------------------------------------------------------------
##### No converge
## Modelo 01: Con CP5

estructura_01 <- 'sa  =~ SA1 + SA2 + SA3 + SA4 
              cp =~ CP1 + CP2 + CP3 + CP4 + CP5
              ce =~ CE1 + CE2 + CE3 + CE4 
              ci =~ CI1 + CI2 + CI3 + CI4 
              p =~ P1 + P2 + P3 + P4 + P5
              e =~ E1 + E2 + E3 + E4 + E5'

## Máxima verosimilitud (no converge)

mod_01 <- cfa(estructura_01, data = datos_corregidos)

## Bayesiano (No converge - No correr)

mod_01_B <- blavaan::bcfa(estructura_01, data = datos_corregidos,
                          burnin = 10000,
                          target = "jags", convergence = "auto", 
                          seed = c(1, 2, 3))

summary(mod_01)

fitMeasures(mod_01)

## Modelo 02: Sin CP5

estructura_02 <- 'sa  =~ SA1 + SA2 + SA3 + SA4 
              cp =~ CP1 + CP2 + CP3 + CP4
              ce =~ CE1 + CE2 + CE3 + CE4 
              ci =~ CI1 + CI2 + CI3 + CI4 
              p =~ P1 + P2 + P3 + P4 + P5
              e =~ E1 + E2 + E3 + E4 + E5'

## ML (varianzas negativas)

mod_02 <- cfa(estructura_02, data = datos_corregidos)

## Bayesiano (da bien - no correr, dura mucho)

# mod_02_B <- blavaan::bcfa(estructura_02, data = datos_corregidos,
#                           burnin = 10000,
#                           target = "jags", convergence = "auto", 
#                           seed = c(1, 2, 3))

# saveRDS(mod_02_B, file = "Modelo_Alpiste.RDS")

modelo_final <- readRDS("Modelo_Alpiste.RDS")

summary(modelo_final, standardized=TRUE)

fitMeasures(modelo_final)

blavFitIndices(modelo_final)

# diagrama ----------------------------------------------------------------

semPlot::semPaths(modelo_final, exoCov = FALSE, 
                  whatLabels = "std", 
                  intercepts = FALSE,
                  label.prop = 0.8,
                  what = "cons", style = "lisrel",
                  sizeMan = 4,
                  sizeLat = 7, thresholds = FALSE,
                  edge.label.cex = 0.75, 
                  residuals = TRUE,
                  rotation = 1)


# convergencia ------------------------------------------------------------

blavInspect(modelo_final, 'rhat')

blavInspect(modelo_final, 'neff')

plot(modelo_final, pars = 1:10)

plot(modelo_final, pars = 11:20)

plot(modelo_final, pars = 21:30)

plot(modelo_final, pars = 31:40)

plot(modelo_final, pars = 41:50)

plot(modelo_final, pars = 51:60)

plot(modelo_final, pars = 61:70)

plot(modelo_final, pars = 71:80)

plot(modelo_final, pars = 81:93)
