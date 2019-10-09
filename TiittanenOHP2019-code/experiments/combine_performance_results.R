## -----------------------------------------------------------------------------
##
## Combine experiment_performance data into a signle data.frame
##
## -----------------------------------------------------------------------------
## Example usage:
##
##        Rscript --vanilla combine_performance.R <results_dir> <dataset_id> <output_dir>
##
##        Rscript --vanilla combine_performance.R results/performance/ <dataset_id> results 
##
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Get command-line arguments
## -----------------------------------------------------------------------------
args       <- commandArgs(trailingOnly = TRUE)

data_path      <- as.character(args[1])
dataset_id      <- as.numeric(args[2])
output_dir <- as.character(args[3])

## --------------------------------------------------
## Libraries
## --------------------------------------------------

library(ggplot2)
library(dplyr)
library(scales)
library(zoo)
library(corrplot)
source("drifter.R")
source("init.R")

## --------------------------------------------------
## Data
## --------------------------------------------------

data_name  <- names(dataset_list)[dataset_id]
# load data

files <- list.files(path = data_path, pattern = paste0("[[:print:]]*", data_name, "[[:print:]]*", ".rds"), full.names = TRUE)

res <- list()
for (i in seq(1, length(files))){
    data <- readRDS(files[[i]])

    df_roc  <- subset(data$output, data$output$window > data$test_index)
    max_f1_point <- calc_perf(df_roc, thr_p = data$ind_max_f1, thr_e = data$error_thr)
    error_point <- calc_perf(df_roc, thr_p = data$ind_tr, thr_e = data$error_thr)


    max_f1_perf <- calc_perf(df_roc, thr_p = data$ind_max_f1, thr_e = data$error_thr, counts=TRUE)
    perf <- calc_perf(df_roc, thr_p = data$ind_tr, thr_e = data$error_thr, counts=TRUE)

    # c: mean(tr_ind)+c*sd(tr_ind) = d
    out <- data$output
    tr_index  <- subset(out, out$window < data$test_index)
    indices <- tr_index$z

    c_max_f1 <- (data$ind_max_f1 - mean(indices))/sd(indices)
    c_own <- (data$ind_ind_tr - mean(indices))/sd(indices)

    res[[i]] <- list("model_f_name"   = unlist(data$model_f_name),
                 "model_f2_name"   = unlist(data$model_f2_name),
                 "dataset_name" = unlist(data$dataset_name),
                 "error_thr" = unlist(data$error_thr),
                 "k" = unlist(data$k),
                 "ind_c" = unlist(data$ind_c),
                 "n_ind"=data$n_ind,
                 "segmentation"=data$segmentation,
                 "ind_tr" = unlist(data$ind_tr),
                 "ind_max_f1" = unlist(data$ind_max_f1),
                 "f1"= unlist(data$max_f1),
                 "max_tp_fp"= error_point[1:2],
                 "tp_fp"= max_f1_point[1:2],
                 "max_f1"=unlist(data$f1),
                 "max_f1_perf"=max_f1_perf,
                 "c_max_f1"=c_max_f1,
                 "c_own"=c_own,
                 "perf"=perf)
}

res <-data.frame(do.call(rbind, res))
res$f1 <- unlist(res$f1)
res$max_f1 <- unlist(res$max_f1)
saveRDS(res, file = file.path(output_dir, paste0(data_name, ".rds")), compress = "xz")

