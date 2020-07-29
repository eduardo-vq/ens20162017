# Cargar base de datos
ENS_analisis<-readRDS("Datos/Datos de Análisis/ENS2017_analisis.RDS")

#Ponderadores
#Si las variables que se están analizando son todas de F1, se debe usar factor1 
#Si las variables que se están analizando son todas de F2, se debe usar factor2 
#Si las variables que se están analizando son cruces de F1 con F2, se debe usar factor 1x2
#sexo, comuna, edad, ingresos, nivel educacional, pertenencia a pueblo indígena, hipertensión arterial: f1
#enfermedad pulmonar crónica, asma: f2

# ---- 1. ANÁLISIS ESTADÍSTICO DESCRIPTIVO ----

# Tabla 1. Estadísticos Descriptivos Factores de Riesgo

tabla1 <- with(ENS_analisis, stby(data = Risk, INDICES = sexo_factor, 
                                  FUN = descr,transpose = TRUE, stats = c("mean", "med", "sd", "cv"),
                                  weights = ENS_analisis$factor1x2, style = 'rmarkdown'))

tabla2 <- with(ENS_analisis, stby(data = Risk, INDICES = niveleduc, 
                                  FUN = descr,transpose = TRUE, stats = c("mean", "med", "sd", "cv"),
                                  weights = ENS_analisis$factor1x2, style = 'rmarkdown'))

tabla3 <- with(ENS_analisis, stby(data = Risk, INDICES = pind_dic, 
                                  FUN = descr,transpose = TRUE, stats = c("mean", "med", "sd", "cv"),
                                  weights = ENS_analisis$factor1x2, style = 'rmarkdown'))

tabla4 <- with(ENS_analisis, stby(data = Risk, INDICES = tramos_ingpc, 
                                  FUN = descr,transpose = TRUE, stats = c("mean", "med", "sd", "cv"),
                                  weights = ENS_analisis$factor1x2, style = 'rmarkdown'))

# ---- 2. GRÁFICOS ----

grafico1 <- ENS_analisis %>% 
  filter(!is.na(sexo_factor)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = sexo_factor, y = (..count..)/sum(..count..),
                         fill = factor(Risk), weight = factor1x2), 
           position="fill") + 
  scale_y_continuous(labels=scales::percent) +
  xlab("Sexo") +
  ylab("Porcentaje")
grafico1 <- grafico1 + labs(fill = "Cantidad de 
Factores de Riesgo") 

ggsave(grafico1, filename = "Salida/Figura1.png",
       dpi = 350, width = 7, height = 4)


grafico2 <- ENS_analisis %>% 
  filter(!is.na(niveleduc)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = niveleduc, y = (..count..)/sum(..count..),
                         fill = factor(Risk), weight = factor1x2), 
           position="fill") + 
  scale_y_continuous(labels=scales::percent) +
  xlab("Nivel Educacional") +
  ylab("Porcentaje")
grafico2 <- grafico2 + labs(fill = "Cantidad de 
Factores de Riesgo") 

ggsave(grafico2, filename = "Salida/Figura2.png",
       dpi = 350, width = 7, height = 4)

grafico3 <- ENS_analisis %>% 
  filter(!is.na(pind_dic)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = pind_dic, y = (..count..)/sum(..count..),
                         fill = factor(Risk), weight = factor1x2), 
           position="fill") + 
  scale_y_continuous(labels=scales::percent) +
  xlab("Pertenencia a pueblo indígena") +
  ylab("Porcentaje")
grafico3 <- grafico3 + labs(fill = "Cantidad de 
Factores de Riesgo") 

ggsave(grafico3, filename = "Salida/Figura3.png",
       dpi = 350, width = 6, height = 5)

grafico4 <- ENS_analisis %>% 
  filter(!is.na(tramos_ingpc)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = tramos_ingpc, y = (..count..)/sum(..count..),
                         fill = factor(Risk), weight = factor1x2), 
           position="fill") + 
  scale_y_continuous(labels=scales::percent) +
  xlab("Tramos Ingreso per cápita ($)") +
  ylab("Porcentaje")
grafico4 <- grafico4 + labs(fill = "Cantidad de 
Factores de Riesgo") 

ggsave(grafico4, filename = "Salida/Figura4.png",
       dpi = 350, width = 8, height = 4)


#Tablas Anexos

anexo1 <- ctable(ENS_analisis$sexo_factor, ENS_analisis$Risk, 
                 prop = "r", weights = ENS_analisis$factor1x2, 
                 style = 'rmarkdown', headings = F)

anexo2 <- ctable(ENS_analisis$niveleduc, ENS_analisis$Risk, 
                 prop = "r", weights = ENS_analisis$factor1x2, 
                 style = 'rmarkdown', headings = F)

anexo3 <- ctable(ENS_analisis$pind_dic, ENS_analisis$Risk, 
                 prop = "r", weights = ENS_analisis$factor1x2, 
                 style = 'rmarkdown', headings = F)

anexo4 <- ctable(ENS_analisis$tramos_ingpc, ENS_analisis$Risk, 
                 prop = "r", weights = ENS_analisis$factor1x2, 
                 style = 'rmarkdown', headings = F)

# ---- 3. COMPILAR RESULTADOS EN UN SÓLO OBJETO (LISTA) Y GUARDAR COMO ARCHIVO ----

resultados <- list(tabla1, tabla2, tabla3, tabla4, anexo1, anexo2, anexo3, anexo4)

saveRDS(resultados, file = "Datos/Datos de Análisis/resultados-reporte.rds")

# Limpiar entorno de trabajo
rm(list=ls())