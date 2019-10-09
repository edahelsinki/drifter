## -----------------------------------------------------------------------------
##
## This file contains utility functions needed in the experimental evaluation.
##
## -----------------------------------------------------------------------------

#' Partition integers into random equally sized chunks.
#'
#' @param n number of integers
#' @param k number of chunks
#' @return List of k items each containing about n/k random integers.
#'
#' @export
kpart <- function(n,k,random=TRUE) {
    i <- if(random) sample.int(n) else 1:n
    j <- floor(seq(from=0,to=n,length.out=k+1))
    mapply(function(a,b) i[(a+1):b],j[-k-1],j[-1],SIMPLIFY=FALSE)
}

#' Modifies formula based modelfunction to use explicit matrices
#'
#' @param f modeling function such as lm or svm that takes formula inputs.
#' @param params list of modeling function parameters
#' @return Modeling function that takes matrix x (covariates) and vector y (independent variables) as inputs.
#'
#' @export
makemodelf <- function(f=lm, params=NULL) {
    function(x,y) {
        colnames(x) <- 1:NCOL(x)
                                        # f(y~.,data.frame(cbind(y=y,x)))# , ntree=50)
        do.call(f, c(list(formula=y~., data=data.frame(cbind(y=y,x))), params))
    }
}

#' Predict function to use with models output by makemodelf
#'
#' @param object as in predict method
#' @param newdata as in predict method
#' @return Returns estimated dependent parameters given by model when applied to the covaraites in newdata.
#'
#' @export
predictmodelf <- function(object,newdata) {
    newdata <- data.frame(newdata)
    colnames(newdata) <- 1:NCOL(newdata)
    predict(object,data.frame(newdata))
}

## ------------------------------------------------------------------------------------------
## Functions for segmenting data
## ------------------------------------------------------------------------------------------

#' Segment data into 2*k overlapping segments
#'
#' @param n Length of the data sequence
#' @param k Number of segments / 2.
#' @return Matrix of size kX2 with the endpoints of the segments.
#'
#' @export
make_segmentation_overlapping <- function(n, k) {
    i <- floor(seq(from=0, to=n, length.out=2*(k+1)))
    a <- i[1:(2*k)]+1
    b <- i[3:(2*(k+1))]
    matrix(c(a, b), nrow = length(a), ncol = 2, byrow = FALSE)
}

#' Segment data into segments of lengths ns
#'
#' @param n Length of the data sequence
#' @param seglen Length of the segment
#' @return Matrix of size kX2 with the endpoints of the esegments.
#'
#' @export
make_segmentation_even <- function(n, seglen) {
    nseg   <- floor(n / seglen)
    m      <- matrix(NA, nrow = nseg, ncol = 2)
    m[, 1] <- seq.int(from = 1, to = n, by = seglen)[1:nseg]
    m[, 2] <- m[, 1] + seglen - 1
    m
}

#' Segment data into k segments
#'
#' @param n Length of the data sequence
#' @param nseg Number of esgments
#' @return Matrix of size kX2 with the endpoints of the esegments.
#'
#' @export
make_segmentation_k <- function(n, nseg) {
    make_segmentation_even(n, seglen = floor(n / nseg))
}


#' Euclidean distance
#'
#' Evaluate the Euclidan distance between x1 and x2
#'
#' @param x1 A vector
#' @param x2 A vector
#'
#' @return The Euclidean distance between x1 and x2.
#' 
#' @export
distance_euclidean <- function(x1, x2) {
    sqrt(sum((x1 - x2)^2))
}


#' Root Mean Squared Error (RMSE)
#'
#' Evaluate the RMSE between x1 and x2
#'
#' @param x1 A vector
#' @param x2 A vector
#'
#' @return The RMSE between x1 and x2.
#' 
#' @export
distance_rmse <- function(x1, x2) {
    sqrt(mean((x1 - x2)^2))
}


#' Baseline drift detection based on KS test
#'
#' @param data_x x of the training data
#' @param xx testing data
#' @return p-value, if small, indicating concept drift.
#'
#' @export
baseline_test <- function(data_x_tr, data_x_te) {
    if(ncol(data_x_tr) != ncol(data_x_te))
        stop("baseline_test: wrong dimensionality.")
    
    min(sapply(seq.int(ncol(data_x_tr)),
               function(i) ks.test(data_x_tr[, i], data_x_te[, i])$p.value))
}


#' Split a dataset intro training and testing sets.
#'
#' @param data A data frame containing the elements "x" and "y".
#' @param i_split Index of the split point used to divide the data into training and testing.
#' @return List containing training and testings sets
#'
#' @export
train_test_split <- function(data, i_split) {
    list("train" = list("x" = data$x[1:i_split,,drop=FALSE],
                        "y" = data$y[1:i_split]),
         "test"  = list("x" = data$x[(i_split+1):length(data$y),,drop=FALSE],
                        "y" = data$y[(i_split+1):length(data$y)]))
}


#' Evaluate drifter
#'
#' Return test segment true error and indicator values
#' 
#' @param dr Drifter object
#' @param x Test data x
#' @param y Test data y
#' @param k_min selects the k_min'th smallest element
#'
#' @return Array containing the true error and indicator values
#' 
#' @export
drifter_eval <- function(dr, x, y, k_min) {
    x   <- as.matrix(x)
    tmp <- dr$test(x, k_min, thr = 0) #thr is  not needed here
    c("z" = tmp$z, "error" = distance_rmse(predictmodelf(dr$get_f0(), x), y))
}


#' Performance metrics
#'
#' Calculate performance metrics by varying the threshold for z
#' (i)  true positive rate
#' (ii) false positive rate
#' 
#' @param df A datafame containing error and z columns
#' @param thr_p The indicator threshold 
#' @param thr_e The error threshold 
#'
#' @return True posive rate & false positive rate
#' 
#' @export
calc_perf <- function(df, thr_p, thr_e, counts=FALSE) {
    true_p   <- sum((df$z > thr_p) & (df$error > thr_e))
    false_p  <- sum((df$z > thr_p) & (! df$error > thr_e))

    true_n   <- sum((df$z <= thr_p) & (! df$error > thr_e))
    false_n  <- sum((df$z <= thr_p) & (df$error > thr_e))

    if (counts){
        return(list(tp=true_p, fp=false_p, tn=true_n, fn=false_n))
    }
    c("tp_rate" = true_p / (true_p + false_n),
      "fp_rate" = false_p / (false_p + true_n),
      "f1"      = (2 * true_p) / ((2 * true_p) + false_p + false_n))
}


#' Drifter
#'
#' Used to detect concept drift.
#'
#' @export
drifter <- function() {

    ## Initialise variables
    F_list        <- NULL
    f0       <- NULL

    ## Set the full model f
    set_f0 <- function(f) {
        f0 <<- f
    }

    ## Return the full model
    get_f0 <- function() {
        f0
    }

    ## Return the list of segment models
    get_F <- function() {
        F_list
    }

    ## Train one model for each segment
    ##
    ## X        : training data, covariates
    ## Y        : training data, dependent variable
    ## f        : regression function (from makemodelf) to be trained for each segment
    ## segments : nX2 matrix with each row containing start/stop indices of a segment
    train <- function(X, Y, f, segments) {
        F_list <<- mapply(function(a, b) f(x=X[a:b,,drop=FALSE], y=Y[a:b]),
                     segments[, 1], segments[, 2], SIMPLIFY=FALSE)
    }

    ## Test for concept drift
    ##
    ## X        : testing data, covariates
    ## ind      ; indicator index, i.e., which indicator value to use in an ordered list of values
    ## thr      : threshold of indicator variable used to declare concept drift
    ## dfun     : distance function to use when comparing full model to segment models
    test <- function(X, ind, thr, dfun=distance_rmse) {
        Y0   <- predictmodelf(f0, X)
        z    <- unlist(lapply(F_list, function(f) dfun(predictmodelf(f, X), Y0)))
        zval <- sort(z, decreasing=FALSE)[ind]

        list("z"=zval,
             "concept_drift"=ifelse(zval > thr, TRUE, FALSE))
    }

    ## return functions
    list("train"  = train,
         "test"   = test,
         "set_f0" = set_f0,
         "get_f0" = get_f0,
         "get_F"  = get_F)
}

#' k-fold crossvalidation
#'
#' @param x covariates (nXm matrix)
#' @param y dependent variables (n vector)
#' @param f regression model
#' @param k number of folds (integer)
#' @return Vector of estimates (n vector).
#'
#' @export
crossval <- function(x,y,f,k=10,random=TRUE) {
    a <- rep(NA, length(y))
    for(i in kpart(length(y), k, random=random)) {
        a[i] <- predictmodelf(f(x=x[-i,,drop=FALSE], y=y[-i]), newdata=x[i,,drop=FALSE])
    }
    a
}

#' Return the trained full model and the corresponding error threshold
#' calculated from the full model error on the validation data
#' 
#' @param modelf Model family
#' @param params Parameters used to construct the model
#' @param x Training data (covariates)
#' @param y Traininng data (dependent variable)
#'
#' @export
train_full_model <- function(modelf, params, x, y){
    f0m              <- makemodelf(modelf, params)
    f0               <- f0m(x=x, y=y)

    ## calculate the error threshold
    cv_estimates <- crossval(x=x,y=y, f=f0m, k=5, random=FALSE)

    # thr <- as.numeric(quantile(distance_rmse(cv_estimates, y), probs=c(0.9)))
    thr <- 2*distance_rmse(cv_estimates, y)
    list(f0=f0, thr=thr)
 }


drifter_experiment <- function(data,
                               f0=NULL,
                               f0_modelfamily=svm,
                               f0_params=NULL,
                               f_modelfamily=lm,
                               f_params=NULL,
                               seg=0,
                               k=10) {

    ## split data into train and test
    data_s <- train_test_split(data, i_split = floor(0.5 * length(data$y)))

    ## segment the data
    if (seg == 0){
        seg  <- make_segmentation_overlapping(n=length(data_s$train$y), k=k)
    } else{
        seg  <- make_segmentation_k(n=length(data_s$train$y), nseg=k)
    }
    ## create drifter object
    dr <- drifter()
    
    ## full model
    if (is.null(f0)) {
        t         <- train_full_model(f0_modelfamily, f0_params, data_s$train$x, data_s$train$y)
        f0        <- t$f0
        error_thr <- t$thr
    }

    dr$set_f0(f0)
    
    ## segment models
    func <- makemodelf(f_modelfamily, f_params)
    dr$train(X=data_s$train$x, Y=data_s$train$y, f=func, segments=seg)

    dr
}

## ------------------------------------------------------------------------------------------
## Functions for generating synthetic data
## ------------------------------------------------------------------------------------------

#' AR(1) data generation
#'
#' @param n size of the dataset
#' @param amp amplitude (standard deviation)
#' @param h correlation length
#' @param a parameter
#' @param b parameter
#' @param amp noise parameter
#' @return AR(1) data
ar1cc <- function(n,h=10,amp=1,a=0,b=if(h>0) 2^(-1/h) else 0,s=amp*sqrt(1-b^2),x0=rnorm(1,mean=a/(1-b),sd=s/sqrt(1-b^2))) {
    Reduce(function(x,w) a+b*x+w,c(x0,rnorm(n-1,sd=s)),accumulate=TRUE)
}

#' Synthetic data generation
#'
#' @param a weight vector
#' @param h1 covaraite correlation length
#' @param h2 hidden covaraite correlation length
#' @param size size of the dataset
#' @param n_covariates number of covariates
#' @param g function
#' @param iid_n iid noise amplitude
#' @param amp covariate noise amplitude
#' @param iid_n iid noise amplitude
#' @return list containing data_x and data_y and xh
generate_synthetic_data <- function(a, h1, h2, size = 2000, n_covariates = 5, g=identity, iid_n=0.5,
                                    noise_x, noise_xh, amp_xh=1, amp_x=1){

    XH    <- ar1cc(size, h2, amp=amp_xh)
    out_x <- replicate(n_covariates, ar1cc(size, h1, amp=amp_x))
    list(x = as.data.frame(out_x),
         y = c(g(out_x) %*% c(a))  + XH + rnorm(size, 0, iid_n))
}

## ------------------------------------------------------------------------------------------
## Functions for processing results
## ------------------------------------------------------------------------------------------

#' Combine results from multiple rds-files into one rds file.
#'
#' @param data_path Path to the data
#' @param file_pattern Pattern to use for file matchine
#' @param file_out File in which to save the results
#'
#' @return Nothing.
combine_results <- function(data_path, file_pattern, file_out) {
    res <- lapply(list.files(path = data_path, pattern = file_pattern, full.names = TRUE), function(f) readRDS(f))
    res <- do.call(rbind, res)
    saveRDS(res, file = file_out, compress = "xz")
}

## ------------------------------------------------------------------------------------------
