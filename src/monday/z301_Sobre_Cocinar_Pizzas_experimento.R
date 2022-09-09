# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")
require("mlrMBO")

# Poner la carpeta de la materia de SU computadora local
setwd(gsub(" ", "", paste(gsub('/', '\\\\', gsub("/m_d_m/dmef", "", getwd())), "\\m_d_m\\dmef")))

# Poner sus semillas
semillas <- c(309367, 149521, 690467, 699191, 795931)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202101]

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
  )
}

#predicted
#actual     noevento  evento
#noevento   0         1
#evento     39         0

modelo_rpart <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10) {
  modelo <- rpart(clase_binaria ~ ., data = train,
                  parms=list(
                    loss=matrix(c(0,1,39,0), byrow = TRUE, nrow = 2)),
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  pred_testing <- predict(modelo, test, type = "prob")
  ganancia(pred_testing[, "evento"], test$clase_binaria) / 0.3
}

experimento_rpart <- function(ds, semillas, cp = -1, ms = 20, mb = 1, md = 10) {
  ganancia <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
                                              list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
    r <- modelo_rpart(train, test, 
                        cp = cp, ms = ms, mb = mb, md = md)
    ganancia <- c(ganancia, r)
  }
  mean(ganancia)
}

obj_fun_md_ms_mb <- function(x) {
  experimento_rpart(dataset, semillas
                      , md = x$maxdepth
                      , ms = x$minsplit
                      , mb = floor(x$minsplit * x$minbucket))
}
#minbucket debería ser menor a minsplit

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms_mb,
  par.set = makeParamSet(
    makeIntegerParam("maxdepth",  lower = 4L, upper = 20L),
    makeIntegerParam("minsplit",  lower = 1L, upper = 1000L),
    makeNumericParam("minbucket", lower = 0L, upper = 1L) 
  ),
  noisy = TRUE,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 100L)
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
  # sacar parámetro opt.focussearch.points en próximas ejecuciones
  #opt.focussearch.points = 20
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms_mb <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms_mb)
