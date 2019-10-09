## -----------------------------------------------------------------------------
##
## Analysis of scalability
##
##     Plot results from the scalability experiment.
##
## -----------------------------------------------------------------------------
## Example usage:
##
##        Rscript --vanilla analyse_scalability.R <results_dir> <output_dir>
##
##        E.g.: Rscript --vanilla analyse_scalability.R results figures
##
## -----------------------------------------------------------------------------
## Get command-line arguments
## -----------------------------------------------------------------------------
args       <- commandArgs(trailingOnly = TRUE)

datapath      <- as.character(args[1])
output_dir <- as.character(args[2])

## --------------------------------------------------
## Libraries
## --------------------------------------------------

source("utilities_analysis.R")
source("drifter.R")

library(ggplot2)
library(dplyr)
library(scales)

## --------------------------------------------------
## Data
## --------------------------------------------------

new_path <-file.path(datapath, paste0("scalability_results.rds"))
combine_results(datapath, "_scalability.rds", new_path)

# Just to get a dataframe of right type for plotting script
l <- readRDS(new_path)[,-c(7,8)]
l_num <- l[,1:7]
l_fac <- l[,8:10]

data_num <- data.frame(matrix(unlist(l_num), nrow=dim(l)[1]))
data_fac <- data.frame(matrix(unlist(l_fac), nrow=dim(l)[1]),stringsAsFactors = FALSE)
data <- cbind(data_num, data_fac)
colnames(data) <- colnames(l)

data_t_n <- subset(data, (d == 100) & (k == 100) & (model_f == "svm"))
data_t_d <- subset(data, (size_tr == 80000) & (k == 100)& (model_f == "svm"))
data_t_k <- subset(data, (d == 100) & (size_tr == 80000)& (model_f == "svm"))

data_t_n_test <- subset(data, (d == 100) & (k == 100))
data_t_d_test <- subset(data, (size_tr == 80000) & (k == 100))
data_t_k_test <- subset(data, (d == 100) & (size_tr == 80000))

show_reg_line <- FALSE
#
# ## --------------------------------------------------
# ## (1) Running time vs n
# ## --------------------------------------------------

p1 <- make_scalability_plot(data_t_n, "size_tr", "t_train", "Number of samples", "Time (s)", 
                            show_regression_line = show_reg_line, phase="train")

p1_test <- make_scalability_plot(data_t_n_test, "size_tr", "t_test", "Number of samples", "Time (s)", 
                                 show_regression_line = show_reg_line, phase="test")

# ## --------------------------------------------------
# ## (2) Running time vs d
# ## --------------------------------------------------

p2 <- make_scalability_plot(data_t_d, "d", "t_train", "Number of dimensions", "Time (s)", 
                            show_regression_line = show_reg_line, phase="train")

p2_test <- make_scalability_plot(data_t_d_test, "d", "t_test", "Number of dimensions", "Time (s)", 
                                 show_regression_line = show_reg_line, phase="test")

# ## --------------------------------------------------
# ## (3) Running time vs k
# ## --------------------------------------------------

p3 <- make_scalability_plot(data_t_k, "k", "t_train", "Number of segments", "Time (s)", 
                            show_regression_line = show_reg_line, legend=TRUE, phase="train")
p3_test <- make_scalability_plot(data_t_k_test, "k", "t_test", "Number of segments", "Time (s)", 
                                 show_regression_line = show_reg_line, legend=TRUE, phase="test")


# ## --------------------------------------------------

ggsave(plot=p1, filename = file.path(output_dir, paste0("scalability_t_n.pdf")), width = 80, height = 50, units = "mm")
ggsave(plot=p2, file = file.path(output_dir, paste0("scalability_t_d.pdf")), width = 80, height = 50, units = "mm")
ggsave(plot=p3, file = file.path(output_dir, paste0("scalability_t_k.pdf")), width = 100, height = 50, units = "mm")

ggsave(plot=p1_test, filename = file.path(output_dir, paste0("scalability_t_n_test.pdf")), width = 80, height = 50, units = "mm")
ggsave(plot=p2_test, file = file.path(output_dir, paste0("scalability_t_d_test.pdf")), width = 80, height = 50, units = "mm")
ggsave(plot=p3_test, file = file.path(output_dir, paste0("scalability_t_k_test.pdf")), width = 100, height = 50, units = "mm")

## --------------------------------------------------
