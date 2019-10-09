# -----------------------------------------------------------------------------
##
## Analysis of drifter performance
##
##     This file trains and evaluates the drifter function for a particular model,
##     data and paramter configuration
##     
##     It returns the trained drifter object, its predictions on training and
##     test data and roc curve plots.
##
## -----------------------------------------------------------------------------
## Example usage:
##
##        Rscript --vanilla experiment_test_drifters.R <model_f_id> <param_set_id> <output_dir>
##
##        E.g.: Rscript --vanilla experiment_test_drifters.R 1 1 results
##
## -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
## Libraries
## -----------------------------------------------------------------------------
set.seed(42)
library(e1071)
library(rpart)
library(randomForest)
source("drifter.R")
source("init.R")


## -----------------------------------------------------------------------------
## Get command-line arguments
## -----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

datapath <-    as.character(args[1])  ## data location
dataset_id     <- as.numeric(args[2])  ## dataset_id
param_set_id <- as.numeric(args[3])    ## which parameter set to use
output_dir   <- as.character(args[4])  ## path to the directory where the created drifter object should be stored


## -----------------------------------------------------------------------------
## Unpack arguments and set parameters
## -----------------------------------------------------------------------------

ks <- c(1, 2, 3, 4, 5)
model_f2_ids <- c(1, 2, 3)
model_f_ids <- c(1, 2, 3)
ind_cs <- c(2, 3, 5) ## multiplication constant for indicator thr selection
k_mins <- c(2, 3, 10) 
segmentations <- c(0,1) # overlapping or nonoverlapping segmentation


## -----------------------------------
## Parameter set
## -----------------------------------
param_set_list     <- expand.grid("k" = ks, "model_f2_id"=model_f2_ids,
    "ind_c"=ind_cs, "model_f_id"=model_f_ids, "k_min"=k_mins, "segmentation"=segmentations)

k_id <- param_set_list[param_set_id, "k"]
ind_c <- param_set_list[param_set_id, "ind_c"]
k_min <- param_set_list[param_set_id, "k_min"]
model_f2_id <- param_set_list[param_set_id, "model_f2_id"]
model_f_id <- param_set_list[param_set_id, "model_f_id"]
segmentation <- param_set_list[param_set_id, "segmentation"]

model_f_name <- names(model_list)[model_f_id]
dataset_name  <- names(dataset_list)[dataset_id]
dataset_fname <- dataset_list[[dataset_id]]

t <- readRDS(file.path("models", paste0(model_f_name, "_", dataset_name, ".rds")))
model_f <- t$f0
error_thr <- t$thr

model_f2         <- model_list[[model_f2_id]]
model_f2_name <- names(model_list)[model_f2_id]

k <- k_list[,as.character(dataset_id)][k_id]

if (k_min <= k){

    ## -----------------------------------------------------------------------------
    ## Split the data into training and testing
    ## -----------------------------------------------------------------------------
    D          <- readRDS(file.path(datapath, dataset_fname))
    ## -----------------------------------------------------------------------------
    ## Train the drifter object
    ## -----------------------------------------------------------------------------

    dr <- drifter_experiment(D, f0=model_f, f_modelfamily=model_f2, k=k, seg=segmentation)

    ## -----------------------------------------------------------------------------
    ## Evaluate results
    ## -----------------------------------------------------------------------------

    ## Test segmantation
    size_te <- 15
    seg<- make_segmentation_even(n = length(D$y), seglen = size_te)


    ## Evalate the drifter for each segment in the training and testing data
    out <- data.frame(t(apply(seg, 1, function(i) drifter_eval(dr, x = D$x[ i[1]:i[2], ], y = D$y[
    i[1]:i[2] ], k_min=k_min))))
    out$window        <- seq.int(nrow(out))

    ## Calculate ROC curve

    # the last half of the data used as testing data
    test_index <- ceiling(floor(length(D$y)/2)/size_te)


    df_roc  <- subset(out, out$window > test_index)

    index_range <- df_roc$z
    index_range <- c(index_range[order(index_range)], max(index_range)+1)
    res_roc <- t(sapply(index_range, function(i) calc_perf(df_roc, thr_p = i, thr_e = error_thr)))

    ## Plot results

    ########################
    ## calculate the index threshold maximizing the f1 score
    df_roc  <- subset(out, out$window > test_index)
    index_range <- df_roc$z
    index_range <- c(index_range[order(index_range)], max(index_range)+1)
    f1 <- res_roc[,3]
    ind_max_f1 <- max(index_range[which(f1 == max(f1))])


    ## then calculate from the error threshold 
    tr_data  <- subset(out, out$window <= test_index)
    ind_tr <- mean(tr_data$z)+ind_c*sd(tr_data$z)
    ind_tr <- index_range[which.min(abs(index_range - ind_tr))]

    # evaluate results
    max_f1_point <- calc_perf(df_roc, thr_p = ind_max_f1, thr_e = error_thr)
    error_point <- calc_perf(df_roc, thr_p = ind_tr, thr_e = error_thr)
    #################################

    filename <- paste0(model_f_name, "_",
                       model_f2_name, "_",
		        dataset_name, "_", k, "_", segmentation, "_", k_min, "_", ind_c)

    pdf(file=file.path(output_dir, paste0(filename, ".pdf")))

    layout(matrix(c(1,3,2,3), 2, 2, byrow = TRUE))
    plot(out$window, out$error, type = "b", col = "red", ylim = c(0, max(out$error)), main = "Ground truth error", xlab = "window", ylab = "error")
    abline(h = error_thr, col = "black", lty = 2)
    abline(v = test_index, col = "black", lty = 2)

    plot(out$window, out$z, type = "l", col = "blue", main = "Index", xlab = "window", ylab = "z")
    ##############################
    abline(h = ind_tr, col = "black", lty = 2)
    abline(h = ind_max_f1, col = "red", lty = 2)
    ##############################
    abline(v = test_index, col = "black", lty = 2)

    plot(res_roc[, 2], res_roc[, 1], type = "b", col = "blue", xlab = "FP-rate", ylab = "TP-rate", main = "ROC", xlim = c(0,1), ylim=c(0,1))
    abline(0, 1, lty=2)
    ############################
    points(max_f1_point[2], max_f1_point[1], col="red", pch=20, cex=5)
    points(error_point[2], error_point[1], col="black", pch=20, cex=5)
    ############################dev
    dev.off()


    ## Save results
    saveRDS(list("drifter"      = dr,
                 "model_f_name"   = model_f_name,
                 "model_f2_name"   = model_f2_name,
                 "dataset_name" = dataset_name,
                 "test_index" = test_index,
                 "error_thr" = error_thr,
                 "output" = out,
                 "k" = k,
                 "n_ind"=k_min,
                 "segmentation"=segmentation,
                 "ind_c" = ind_c,
                 "ind_tr" = ind_tr,
                 "ind_max_f1" = ind_max_f1,
                 "res_roc" = res_roc,
                 "f1" = max_f1_point[3],
                 "max_f1" = error_point[3]),
            file = file.path(output_dir, paste0(filename, ".rds")), compress = "xz")

    ## -----------------------------------------------------------------------------
}
