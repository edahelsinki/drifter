## -----------------------------------------------------------------------------
##
## Analysis of scalability
##
##     The scalability experiment with respect to dataset
##     size, dimensionality, and the number of training segments k.
##     It returns training and testing runtimes for the specified model
##
## -----------------------------------------------------------------------------
## Example usage:
##
##        Rscript --vanilla experiment_scalability.R <model_f_id>  <param_set_id> <output_dir>
##
##        E.g.: Rscript --vanilla experiment_scalability.R 2 1 results
##
## -----------------------------------------------------------------------------

source("init.R")
source("drifter.R")

## -----------------------------------------------------------------------------
## Get command-line arguments
## -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
 
model_f_id     <- as.numeric(args[1])    ## main model index
param_set_id <- as.numeric(args[2])    ## which parameter set (n, d, k) to use
output_dir   <- as.character(args[3])  ## path to the directory where the results should be saved
#output_dir <- "./newresults"

## -----------------------------------------------------------------------------
## Unpack arguments and set parameters
## -----------------------------------------------------------------------------

model_f         <- model_list[[model_f_id]]
model_f_name <- names(model_list)[model_f_id]

ns <- c(5000, 10000, 20000, 40000, 80000, 160000)
ds <- c(5, 10, 50, 100, 250)
ks <- c(5, 10, 25, 50, 100, 200)
 
model_f2_ids <- c(1, 2, 3)

## -----------------------------------
## Parameter set
## -----------------------------------
param_set_list     <- expand.grid("n" = ns, "d" = ds, "k"=ks, "model_f2_id"=model_f2_ids)

n <- param_set_list[param_set_id, "n"]
d <- param_set_list[param_set_id, "d"]
k <- param_set_list[param_set_id, "k"]
model_f2_id <- param_set_list[param_set_id, "model_f2_id"]

model_f2         <- model_list[[model_f2_id]]
model_f2_name <- names(model_list)[model_f2_id]

## -----------------------------------------------------------------------------
## Set the random seed
## -----------------------------------------------------------------------------

set.seed(42)

## -----------------------------------------------------------------------------
## Experiment
## -----------------------------------------------------------------------------

## -----------------------------------
## Set parameters for synthetic data
## -----------------------------------
g         <- sin
a_vec     <- rep(1,d)
iid_n     <- 0.3
h1_tr     <- 150
h1_te     <- h1_tr
h2_tr     <- 0
h2_te     <- h2_tr
amp_x_tr  <- 1
amp_x_te  <- 5*amp_x_tr
amp_xh_tr <- 0
amp_xh_te <- 0

## train the full model (accuracy not important here so use just small amount of data 
## to make trainining fast
data_mg <- generate_synthetic_data(size=n, n_covariates=d, g=g, iid_n=iid_n,
                                   a=a_vec, h1=h1_tr, h2=h2_tr, 
                                   amp_xh=amp_xh_tr, amp_x=amp_x_tr)
t <- train_full_model(model_f, NULL, data_mg$x[1:500,], data_mg$y[1:500])
f <- t$f0

## generate segmentation
seg <- make_segmentation_overlapping(n=n, k=k)
size_te <- 15

t_train <- rep(0,5)
t_test <- rep(0,5)

## Repeat 5 times
for (i in 1:5) {
  ## initialize drifter
  dr <- drifter()
  dr$set_f0(f)

  ## generate training data (from same distribution that was used to train f)
  data_tr <- generate_synthetic_data(size=n, n_covariates=d, g=g, iid_n=iid_n,
              a=a_vec, h1=h1_tr, h2=h2_tr, 
              amp_xh=amp_xh_tr, amp_x=amp_x_tr)

  ## generate testing data (different amplitude)
  data_te <- generate_synthetic_data(size=size_te, n_covariates=d, g=g, iid_n=iid_n,
             a=a_vec, h1=h1_te, h2=h2_te, 
             amp_xh=amp_xh_te, amp_x=amp_x_te)

  # train and test
  t_train[i] <- system.time(dr$train(X=data_tr$x, Y=data_tr$y,
             f=makemodelf(model_f2), segments=seg))[[3]]

  t_test[i]  <- system.time(tmp <- dr$test(data_te$x, ind=2, thr=1.5))[[3]]
  
  i <- i+1

}

out <- list(size_tr=n, size_te=size_te, k=k, d=d, t_train=median(t_train), t_test=median(t_test), t_train_all=t_train, t_test_all=t_test,
       t_tot = median(t_train+t_test), model_f = model_f_name, model_f2 = model_f2_name, stringsAsFactors = FALSE)

## -----------------------------------------------------------------------------
## Save results
## -----------------------------------------------------------------------------
saveRDS(out, file = file.path(output_dir, paste0(model_f_name, "_", model_f2_name,"_", as.character(param_set_id), "_scalability.rds")), compress = "xz")
## -----------------------------------------------------------------------------
