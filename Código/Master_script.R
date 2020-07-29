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
               survey,
               haven)

# Ejecuta código 1: lectura base datos
# Expulsa archivo de datos para editar
source("Código/Procesamiento/1-lectura-base.R")

# Ejecuta código 2: preparación de datos
# Carga datos para editar, expulsa archivo de datos editados
source("Código/Procesamiento/2-Preparación-Datos.R")

# Ejecuta código 3: construcción de resultados
# Carga datos editados, expulsa lista de resultados como archivo
source("Código/Análisis/3-construcción-resultados.R")

# Ejecuta código 4: construcción de reporte reproducible
# En base a lista de resultados, crea archivo de reporte en PDF
rmarkdown::render('Reporte Reproducible.Rmd')

# Limpiar entorno de trabajo
rm(list=ls())




