## this code trains all full models used in experiments

source("drifter.R")
source("init.R")

output_dir <- "models"
datapath   <- "../data"


## -----------------------------------------------------------------------------
## Get command-line arguments
## -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

param_set_id     <- as.numeric(args[1])

model_f_ids <- seq.int(length(model_list))
dataset_ids <- seq.int(length(dataset_list))


param_set_list     <- expand.grid("model_f_id"=model_f_ids, "dataset_id"=dataset_ids)

model_f_id <- param_set_list[param_set_id, "model_f_id"]
dataset_id <- param_set_list[param_set_id, "dataset_id"]

dataset_fname <- dataset_list[[dataset_id]]
dataset_name  <- names(dataset_list)[dataset_id]
model_f      <- model_list[[model_f_id]]
model_f_name <- names(model_list)[model_f_id]
data          <- readRDS(file.path(datapath, dataset_fname))

## use first half as training data
index <- floor(0.5 * length(data$y))
tr_data             <- train_test_split(data, index)

res <- train_full_model(model_f, NULL, tr_data$train$x, tr_data$train$y)

saveRDS(res, file = file.path(output_dir, paste0(model_f_name, "_", dataset_name,".rds")), compress = "xz")
