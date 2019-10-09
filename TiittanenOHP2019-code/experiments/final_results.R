# -----------------------------------------------------------------------------
##
##     Construct F1 score vs. k result tables for all datasets
##
# -----------------------------------------------------------------------------

library(dplyr)

datapath <- "results"
output_dir <- "results/tables"
files <- list.files(path = datapath, pattern = paste0(".rds"), full.names = TRUE)

main_models <- list(airquality="svm", airline="randomforest", bike_current="lm", bike="lm")


construct_table <- function(data, main_model){
    # fix n_ind,
    data <- subset(data, data$n_ind == 2)
    # fix  overlapping segments
    data <- subset(data, data$segmentation == 0)
    # fix main model
    data <- subset(data, data$model_f_name ==  main_model)
    data <- subset(data, data$ind_c == 2)
    data <- subset(data, (data$model_f2_name ==  main_model) | (data$model_f2_name == "lm"))

    # construct results table
    res <- select(data, "k", "max_f1", "model_f_name", "model_f2_name",
                    "c_max_f1", "c_own", "max_f1_perf", "perf")
    res$k <- unlist(res$k)

    res_main <- subset(res, (res$model_f2_name ==  main_model))
    res_main <- res_main[order(as.numeric(res_main$k)),]

    res_lm <- subset(res, (res$model_f2_name ==  "lm"))
    res_lm <- res_lm[order(as.numeric(res_lm$k)),]

    filename <- paste0(data$dataset_name[[1]],"_","table") 
    saveRDS(list("dataset_name"      = data$dataset_name,
                 "res_main" = res_main,
                 "res_lm" = res_lm),
            file = file.path(output_dir, paste0(filename, "_", main_model, ".rds")), compress = "xz")
}

for (file in files){
    # load data
    data <- readRDS(file)
    data$ind_c <- unlist(data$ind_c)
    #select main model
    main_model = main_models[data$dataset_name[[1]]][[1]]
    if (identical(data$dataset_name[[1]], "synthetic_04")){
        construct_table(data, "lm")
        construct_table(data, "svm")
        construct_table(data, "randomforest")
    } else{
        construct_table(data, main_model)
    }
}
