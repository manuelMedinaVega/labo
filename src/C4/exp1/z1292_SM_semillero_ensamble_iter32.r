# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "ZZ1292_ensamble_semillerio_iter32"
PARAM$exp_input <- "ZZ9430_iter32"

#PARAM$corte <- 11000 # cantidad de envios
PARAM$cortes  <- seq( from=  7000,
                      to=    13000,
                      by=        500 )
# FIN Parametros del script

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

base_dir <- "~/buckets/b1/"

# creo la carpeta donde va el experimento
dir.create(paste0(base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0(base_dir, "exp/", PARAM$experimento, "/")) # Establezco el Working Directory DEL EXPERIMENTO

path_experimento_semillerio <- paste0(base_dir, "exp/", PARAM$exp_input)
archivos <- list.files(path = path_experimento_semillerio, pattern = "_resultados.csv")

# Levantar dataset C4
# leo el dataset a partir del cual voy a calcular las ganancias
arch_dataset <- paste0(base_dir, "datasets/competenciaFINAL_2022.csv.gz")
dataset <- fread(arch_dataset)

dataset_final <- dataset[foto_mes == 202109]
rm(dataset)

# Tabla que contendrá los rankings de todos los clientes para todas las semillas
tb_ranking_semillerio <- data.table(numero_de_cliente = dataset_final[, numero_de_cliente])

for (archivo in archivos) {
  
  ksemilla <- strtoi(sapply(strsplit(archivo, "_"), "[", 2))
  
  # cols: numero_de_cliente,foto_mes,prob,rank
  tb_prediccion <- fread(paste0(path_experimento_semillerio, '/', archivo))
  
  # Generamos predicción del semillerio
  tb_ranking_semillerio[, paste0("rank_", ksemilla) := tb_prediccion$rank]
  
  # Generamos predicción individual
  setorder(tb_prediccion, -prob)
  
  
  # Esta es la predicción del semillerio para la semilla i-esima
  tb_prediccion_semillerio <- data.table(
    tb_ranking_semillerio[, list(numero_de_cliente)],
    prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
  )
  setorder(tb_prediccion_semillerio, prediccion) # Esto es un ranking, entonces de menor a mayor
  
}


for (corte in PARAM$cortes)
{
  nom_corte_sem = paste0("semillerio_",sprintf("%d", corte),".csv")
  tb_prediccion_semillerio[, Predicted := 0]
  tb_prediccion_semillerio[1:corte, Predicted := 1L]
  # Guardo el submit con rank
  fwrite(tb_prediccion_semillerio[, list(numero_de_cliente, Predicted)],
         file = nom_corte_sem,
         sep = ",")
}

