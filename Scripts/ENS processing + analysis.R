# ELECTIVO: CIENCIA ABIERTA Y SOFTWARE LIBRE.
# MAGÍSTER EN CCSS UNIVERSIDAD DE CHILE - 2020 
# Eduardo Peña Ortiz
# Encuesta Nacional de Salud 2016-2017

# Instalación de paquetes para el espacio de trabajo
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse,  
               arsenal,
               summarytools,
               survey)

# ---- 1. CARGAR BASE DE DATOS A RSTUDIO Y PREPARAR VARIABLES ----

# Cargar Paquete
library(haven)

# Cargar Bases de datos

#Base de datos Formulario 1 y 2
ensf1_2dta <- read_dta("Data/InputData/ENS_F1_2_.dta")
ensf1_2sav <- read_spss("Data/InputData/ENS_F1_2_.sav")

#Base de datos con variables complejas de Formulario 1 y 2
ens_complejo <- read_spss("Data/InputData/ENS_F1_2_Comuna.sav")

#Explorar bases de datos
names(ensf1_2dta)
names(ensf1_2sav)
names(ens_complejo)

#Comparar variables y Unir DataFrames 
library(arsenal) #cargar paquete para compara variables
comparedf(ensf1_2sav,ens_complejo)
ens <- merge(ensf1_2sav,ens_complejo, all=TRUE) 

#Seleccionar variables de interés
library(dplyr) #cargar paquete
ENS_analisis <- select(ens, factor1 = Fexp_F1p_Corr, factor2 = Fexp_F2p_Corr, 
                       factor1x2 = Fexp_F1F2p_Corr, sexo = Sexo, 
                       comuna = Comuna, edad = Edad_Codificada, 
                       plmnr_cronc=m9p17A, asma=m9p18A, hta=h2, tbq= ta3)


# RECODIFICACIÓN

# Sexo (nominal)

table(ENS_analisis$sexo) #1 = hombre, 2 = mujer
class(ENS_analisis$sexo)
ENS_analisis$sexo <- as.numeric(ENS_analisis$sexo) 

ENS_analisis <- mutate(ENS_analisis, sexo_factor = factor(ENS_analisis$sexo,labels = c("Hombre", "Mujer"))) #queda como objeto factor
ENS_analisis$sexo_factor #visualizar datos 
table(ENS_analisis$sexo_factor)

# Comuna (nominal)

#No se realizan modificaciones
table(ENS_analisis$comuna)
class(ENS_analisis$comuna)

#Edad: Menor a 65 o 65+ (ordinal)

#1. 15-24 años / 2. 25-44 años / 3. 45-64años / 4. 65 o más.
#Recodificar en menor de 65 / 65+
table(ENS_analisis$edad)
class(ENS_analisis$edad)
ENS_analisis$edad <- as.numeric(ENS_analisis$edad)

#Recodificar según tramos

ENS_analisis <- mutate(ENS_analisis, edad_rec65 = car::recode(ENS_analisis$edad, "1:3 = 1;4 = 2"))
table(ENS_analisis$edad_rec65)

#Convertir a factor para poner etiquetas
ENS_analisis$edad_rec65factor <- factor(ENS_analisis$edad_rec65, labels= c("menor de 65", "65+"))
table(ENS_analisis$edad_rec65factor)

#Filtrar casos con edad 65+ 
e65mas <- ENS_analisis %>%
  filter(edad_rec65 == 2)
dim(e65mas)


#Enfermedad pulmonar crónica (nominal)

#¿Alguna vez un doctor o médico le ha dicho que tiene o que padece de Bronquitis crónica, enfisema pulmonar, enfermedad pulmonar obstructiva crónica o EPOC? -8888 NO SABE /  1 SÍ  / 2 NO
#recodificar en 1.Sí 0.No 
summary(ENS_analisis$plmnr_cronc)
table(ENS_analisis$plmnr_cronc)
class(ENS_analisis$plmnr_cronc)
ENS_analisis$plmnr_cronc <- as.numeric(ENS_analisis$plmnr_cronc)

ENS_analisis$plmnr_cronc <- car::recode(ENS_analisis$plmnr_cronc, "1=1;2=0; -8888=NA")

#Convertir a factor para poner etiquetas
ENS_analisis$plmnr_cronc_factor<- factor(ENS_analisis$plmnr_cronc, labels= c("No", "Sí"))
table(ENS_analisis$plmnr_cronc_factor)

#Filtrar casos con Enfermdad pulmonar crónica 
EPC <- ENS_analisis %>%
  filter(plmnr_cronc == 1)
  dim(EPC)

#Asma (Nominal)

#¿Alguna vez un doctor o médico le ha dicho que tiene o que padece de Asma? -8888 NO SABE /  1 SÍ  / 2 NO
#recodificar en 1.Sí 0.No 
summary(ENS_analisis$asma)
table(ENS_analisis$asma)
class(ENS_analisis$asma)
ENS_analisis$asma <- as.numeric(ENS_analisis$asma)

ENS_analisis$asma <- car::recode(ENS_analisis$asma, "1=1;2=0; -8888=NA")

#Convertir a factor para poner etiquetas
ENS_analisis$asma_factor<- factor(ENS_analisis$asma, labels= c("No", "Sí"))
table(ENS_analisis$asma_factor)

#Filtrar casos con Asma 
ASMA <- ENS_analisis %>%
  filter(asma == 1)
dim(ASMA)

#Hipertensión Arterial (nominal)
#ENS: "¿Alguna vez un profesional de la salud le ha diagnosticado presión alta?": 1 SÍ, UNA SOLA VEZ 2 SÍ, MÁS DE UNA VEZ  3 NO, NUNCA ME LO HAN DICHO 4 NO RECUERDO, NO ESTOY SEGURO(A)
#Recordificar en números asociados a subgrupos de respuesta 1:2 en 1, 3:4 en 0.
summary(ENS_analisis$hta)
table(ENS_analisis$hta)
class(ENS_analisis$hta)
ENS_analisis$hta <- as.numeric(ENS_analisis$hta)

ENS_analisis <- mutate(ENS_analisis, hta_rec = car::recode(ENS_analisis$hta, "1:2=1;3:4=0; -8888=NA"))
table(ENS_analisis$hta_rec)

#Convertir a factor para poner etiquetas
ENS_analisis$hta_factor<- factor(ENS_analisis$hta_rec, labels= c("No", "Sí"))
table(ENS_analisis$hta_factor)

#Filtrar casos con Asma 
HTA <- ENS_analisis %>%
  filter(hta_rec == 1)
dim(HTA)

#Guardar Base de Datos
saveRDS(ENS_analisis, file = "Data/AnalysisData/ENS20162017_seleccion.RDS")

# ---- 2. RESULTADOS  ----

library(summarytools)

#Ponderadores
#Si las variables que se están analizando son todas de F1, se debe usar factor1 
#Si las variables que se están analizando son todas de F2, se debe usar factor2 
#Si las variables que se están analizando son cruces de F1 con F2, se debe usar factor 1x2
#sexo, comuna, edad, hipertensión arterial: f1
#enfermedad pulmonar crónica, Asma: f2

# RESULTADO 1: Frecuencias ponderadas con "summarytools"
# 10 Comunas con mayor cantidad de observaciones
freq(ENS_analisis$comuna, report.nas = FALSE, headings = FALSE, 
     cumul = FALSE, order = "freq", rows = 1:10, 
     weights = ENS_analisis$factor1)

# RESULTADO 2:  Frecuencias ponderadas con "summarytools" por comuna

# Frecuencia de individuos con edad: 65 o + en cada comuna
freq(e65mas$comuna, report.nas = FALSE, 
     order = "freq", weights = e65mas$factor1)

# Frecuencia de individuos con edad: 65 o + en 10 comunas con mayor cantidad de casos
freq(e65mas$comuna, report.nas = FALSE, 
     order = "freq", rows = 1:10, weights = e65mas$factor1)

#Frecuencia de individuos con Enfermedad Pulmonar Crónica autoreportada en 10 comunas con mayor cantidad de casos
freq(EPC$comuna, report.nas = FALSE,  
     order = "freq", rows = 1:10, weights = EPC$factor1x2)

#Frecuencia de individuos con Asma en 10 comunas con mayor cantidad de casos
freq(ASMA$comuna, report.nas = FALSE,  
     order = "freq", rows = 1:10, weights = ASMA$factor1x2)

#Frecuencia de individuos con Hipertensión Arterial Autoreportada en 10 comunas con mayor cantidad de casos
freq(HTA$comuna, report.nas = FALSE,  
     order = "freq", rows = 1:10, weights = HTA$factor1)

#Resultado 3 Tablas de doble entrada con "summarytools", ponderados

#Para los casos de tablas cruzadas de Enfermedad Pulmonar Crónica y Asma 
#es necesario comparar datos de F1 y F2. 

#Población mayor a 65 años según comuna, perfil fila
ctable(ENS_analisis$comuna, ENS_analisis$edad_rec65factor, 
       prop = "r",weights = ENS_analisis$factor1)

#Prevalencia de Enfermedad Pulmonar Crónica según sexo, perfil columna
ctable(ENS_analisis$sexo_factor, ENS_analisis$plmnr_cronc_factor, 
       prop = "c", weights = ENS_analisis$factor1x2)

#Prevalencia de Enfermedad Pulmonar Crónica según comuna, perfil fila
ctable(ENS_analisis$comuna, ENS_analisis$plmnr_cronc_factor, 
       prop = "r", order = "freq", weights = ENS_analisis$factor1x2)

#Prevalencia de asma según sexo, perfil columna
ctable(ENS_analisis$sexo_factor, ENS_analisis$asma_factor, 
       prop = "c", weights = ENS_analisis$factor1x2)

#Prevalencia de asma según comuna, perfil fila
ctable(ENS_analisis$comuna, ENS_analisis$asma_factor, 
       prop = "r", weights = ENS_analisis$factor1x2)

#Prevalencia de hipertención arterial según sexo, perfil columna
ctable(ENS_analisis$sexo_factor, ENS_analisis$hta_factor, 
       prop = "c", weights = ENS_analisis$factor1)

#Prevalencia de hipertención arterial según comuna, perfil fila
ctable(ENS_analisis$comuna, ENS_analisis$hta_factor, 
       prop = "r", weights = ENS_analisis$factor1)

# Bases de datos ponderadas para uso posterior

library(survey)

ENS_ponderadaF1 <- svydesign(data = ENS_analisis, id=~1, weights = ~factor1)

ENS_paraF1X2 <- subset( ENS_analisis , !is.na( factor1x2 ) )

ENS_ponderadaF1X2 <- svydesign(data = ENS_paraF1X2, id=~1, weights = ~factor1x2)
