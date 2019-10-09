## -----------------------------------------------------------------------------
## 
## Retrieve datasets used for the analysis.
## 
##      This script prepares the datasets used in the experimental evaluation.
##
##
## -----------------------------------------------------------------------------
## Example usage:
##
##        Rscript --vanilla retrieve_data.R
##
## -----------------------------------------------------------------------------
source("../experiments/init.R")
source("../experiments/drifter.R")


## -----------------------------------------------------------------------------
## UCI Air Quality Dataset
## -----------------------------------------------------------------------------
AQ           <- read.csv("./AQ_cleaned_version.csv", header=TRUE)
rownames(AQ) <- AQ[, 1]
AQ           <- AQ[, -1]

data <- list("x" = as.matrix(AQ[, 4:14]),
             "y" = AQ[, 3])

saveRDS(data, file = "uci_airquality.rds", compress = "xz")

## -----------------------------------------------------------------------------
## Airline data
## -----------------------------------------------------------------------------
# https://www.kaggle.com/usdot/flight-delays
data = read.csv("./flights.csv")

####### construct x, y (take every 150th sample)
data2 = data[seq(1,dim(data)[1], 150),]
cols = c("DEPARTURE_DELAY", "DAY_OF_WEEK",
  "ORIGIN_AIRPORT", "AIRLINE",
  "DEPARTURE_TIME", "DESTINATION_AIRPORT",
  "DISTANCE", "SCHEDULED_ARRIVAL",
  "ARRIVAL_DELAY")
data2 = data.frame(data2[,cols])
data2 = na.omit(data2)
y = data2$ARRIVAL_DELAY
data2$ARRIVAL_DELAY <- NULL
x = data2

###### 
f_cols = c("ORIGIN_AIRPORT", "AIRLINE", "DESTINATION_AIRPORT", "DISTANCE")
numfac <- function(x) as.numeric(as.factor(x))
x[f_cols] <- lapply(x[f_cols], numfac)

###### save restult
saveRDS(list(x=x, y=y), "airline.rds")

## -----------------------------------------------------------------------------
## Bike data
## -----------------------------------------------------------------------------
D <- read.csv("./bike_day.csv",
              header=TRUE,stringsAsFactors=FALSE)

D[,"dteday"] <- as.Date(D[,"dteday"])
D[,"season"] <- factor(as.factor(D[,"season"]),
                       levels=1:4,labels=c("spring","summer","fall","winter"))

covariates <- c("holiday","weekday","workingday","weathersit","temp",
                "atemp","hum","windspeed")


data_x <- as.matrix(D[,covariates])
data_y <- D[,"cnt"]

## Task: predict count of rentals based on current conditions
## (day, weather - see above)

saveRDS(list(x=data_x,y=data_y),"bike_current.rds")

## remove trend from the bike data
D <- list()
D$x <- data_x
D$y <- data_y
iii <- 1:(length(data_y) %/% 2)
kkk <- mean(data_y[iii])/mean(data_y[-iii])
D$y[-iii] <- data_y[-iii]*kkk
saveRDS(list(x=D$x,y=D$y),"bike_trend_removed.rds")


## -----------------------------------------------------------------------------
## Synthetic data
## -----------------------------------------------------------------------------

generate_data <- function(dim, drift_location, output_file){
  set.seed(42)
  
  size_te <- drift_location[2] - drift_location[1]
  size_tr <- 2000 - size_te
  
  ## Parameters
  d         <- dim
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
  
  data_tr <- generate_synthetic_data(size=size_tr, n_covariates=d, g=g, iid_n=iid_n,
                                       a=a_vec, h1=h1_tr, h2=h2_tr, 
                                       amp_xh=amp_xh_tr, amp_x=amp_x_tr)
  
  data_te <- generate_synthetic_data(size=size_te, n_covariates=d, g=g, iid_n=iid_n,
                                       a=a_vec, h1=h1_te, h2=h2_te, 
                                       amp_xh=amp_xh_te, amp_x=amp_x_te)
  
  
  
  x <- rbind(as.matrix(data_tr$x[1:drift_location[1],]), as.matrix(data_te$x), as.matrix(data_tr$x[drift_location[1]:dim(data_tr$x)[1],]))
  y <- c(data_tr$y[1:drift_location[1]], data_te$y, data_tr$y[drift_location[1]:length(data_tr$y)])
  
  data_tt <- list(x=x, y=y)
  saveRDS(data_tt, file = output_file, compress = "xz")
}

generate_data(1, c(1000, 2000), "synthetic_01.rds")
generate_data(5, c(1000, 2000), "synthetic_02.rds")
generate_data(1, c(1700, 1800), "synthetic_03.rds")
generate_data(5, c(1700, 1800), "synthetic_04.rds")
