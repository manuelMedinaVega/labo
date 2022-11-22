# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "hibridacion_semillerios_13"
PARAM$exp_input <- "resultados_semillerios"

PARAM$cortes  <- seq( from=  8000,
                      to=    15000,
                      by=        500 )

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

base_dir <- "~/buckets/b1/"

setwd(paste0(base_dir, "exp/", PARAM$experimento, "/")) # Establezco el Working Directory DEL EXPERIMENTO

path_resultados <- paste0(base_dir, "exp/", PARAM$exp_input)
archivos <- list.files(path = path_resultados, pattern = "_ensamble_semillerio.csv")

# Levantar dataset C4
# leo el dataset a partir del cual voy a calcular las ganancias
arch_dataset <- paste0(base_dir, "datasets/competenciaFINAL_2022.csv.gz")
dataset <- fread(arch_dataset)

dataset_final <- dataset[foto_mes == 202109]
rm(dataset)

# Tabla que contendrá los rankings de todos los clientes para todas las semillas
tb_ranking_semillerio <- data.table(numero_de_cliente = dataset_final[, numero_de_cliente])

for (archivo in archivos) {
  
  # cols: numero_de_cliente,foto_mes,prob,rank
  tb_prediccion <- fread(paste0(path_resultados, '/', archivo))
  
  tb_ranking_semillerio[, archivo := tb_prediccion$prediccion]
  
  # Esta es la predicción del semillerio para la semilla i-esima
  tb_prediccion_final <- data.table(
    tb_ranking_semillerio[, list(numero_de_cliente)],
    prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
  )
  
}

setorder(tb_prediccion_final, prediccion) # Esto es un ranking, entonces de menor a mayor

for (corte in PARAM$cortes)
{
  nom_corte_final = paste0("hibridacion_",sprintf("%d", corte),".csv")
  tb_prediccion_final[, Predicted := 0]
  tb_prediccion_final[1:corte, Predicted := 1L]
  # Guardo el submit con rank
  fwrite(tb_prediccion_final[, list(numero_de_cliente, Predicted)],
         file = nom_corte_final,
         sep = ",")
}
