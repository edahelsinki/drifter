# -----------------------------------------------------------------------------
##
## Draw better subfigures for figure 3
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

cols <- c("#1b9e77","#d95f02","black")


## -----------------------------------------------------------------------------
## Get command-line arguments
## -----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

datafile <-    as.character(args[1])  ## data location
output_file   <- as.character(args[2])  ## path to the directory where the created drifter object should be stored
output_file2   <- as.character(args[3])
output_file3   <- as.character(args[4])
isbike <- as.character(args[5])

data <- readRDS(file.path(datafile))

# evaluate results
out <- data$output
test_index <- data$test_index
segmentation <- data$segmentation
#################################

ind_tr <- data$ind_tr  
ind_max_f1 <- data$ind_max_f1
error_thr <- data$error_thr 

#################################
model_f_name <- data$model_f_name
model_f2_name <- data$model_f2_name
segmentation <- data$segmentation
dataset_name = data$dataset_name
k <- data$k
k_min <- data$ind_k
ind_c <- data$ind_c
res_roc <- data$res_roc

df_roc  <- subset(out, out$window > test_index)
max_f1_point <- calc_perf(df_roc, thr_p = ind_max_f1, thr_e = error_thr)
error_point <- calc_perf(df_roc, thr_p = ind_tr, thr_e = error_thr)
filename <- paste0(model_f_name, "_",
                   model_f2_name, "_",
                   dataset_name, "_", k, "_", segmentation, "_", k_min, "_", ind_c)

mz <- mean(out$z[out$window<test_index])
sdz <- sd(out$z[out$window<test_index])
par(mai=c(0,0,0,0))


pdf(output_file2)
par(mar=c(5,5.5,4,1)+.1)
plot(c(0,1),c(0,1),type="n",xlab = "false positive rate", ylab = "true positive rate", main = "", cex.axis = 1.5, cex.lab = 2)
##mtext("false positive rate",side=1,cex=2)
##mtext("true positive rate",side=2,cex=2)

lines(c(0,1),c(0,1),lty="dashed")

aux <- data.frame(error=out$error[test_index:length(out$error)]>error_thr,
                  index=out$z[test_index:length(out$error)],z=NA,tp=NA,fp=NA,fn=NA,fpr=NA,tpr=NA,f1=NA,c=NA)
aux <- aux[order(aux$index),]
aux[-1,"z"] <- (aux[-1,"index"]+aux[-dim(aux)[1],"index"])/2
delta <- (aux[dim(aux)[1],"index"]-aux[1,"index"])/(2*(dim(aux[1])[1]-1))
aux[1,"z"] <- aux[1,"index"]-delta
aux <- rbind(aux,NA)
aux[dim(aux)[1],"z"] <- aux[dim(aux)[1]-1,"index"]+delta

aux[-dim(aux)[1],"tp"] <- rev(cumsum(rev(aux[-dim(aux)[1],"error"])))
aux[dim(aux)[1],"tp"] <- 0
aux[-dim(aux)[1],"fp"] <- rev(cumsum(rev(!aux[-dim(aux)[1],"error"])))
aux[dim(aux)[1],"fp"] <- 0
aux[-1,"fn"] <- cumsum(aux[-dim(aux)[1],"error"])
aux[1,"fn"] <- 0
aux[,"tpr"] <- aux[,"tp"]/aux[1,"tp"]
aux[,"fpr"] <- aux[,"fp"]/aux[1,"fp"]
aux[,"f1"] <- 2*aux[,"tp"]/(2*aux[,"tp"]+aux[,"fp"]+aux[,"fn"])
aux[,"c"] <- (aux[,"z"]-mz)/sdz
lines(aux[,c("fpr","tpr")],lwd=3)
i <- which.max(aux[,"f1"])
points(aux[i,c("fpr","tpr")],col=cols[3],pch=19,cex=3)
text(aux[i,c("fpr","tpr")],sprintf("F1 = %.3f",aux[i,"f1"]),col=cols[3],adj=c(-0.1,1.2),cex=2)
text(0.75,0.25,sprintf("AUC = %.3f",sum((aux[-dim(aux)[1],"fpr"]-aux[-1,"fpr"])*(aux[-1,"tpr"]+aux[-dim(aux)[1],"tpr"])/2)),cex=2)
if(isbike=="1") {
  ind_tr <- max(out$z)+delta
} else {
  ind_tr <- aux[i,"z"]
}

dev.off()

pdf(output_file3,width=7,height=3)
par(mar=c(5,5.5,4,1)+.1)
plot(c(0,20),c(0,1.1),type="n",bty="n",main="",xlab="c",ylab="F1",cex.axis = 1.5, cex.lab = 2)
lines(aux[-dim(aux)[1],c("c","f1")],lwd=2)
points(aux[i,c("c","f1")],col=cols[3],pch=19,cex=2)
text(x=aux[i,"c"]+.5,y=0,sprintf("F1 = %.3f",aux[i,"f1"]),col=cols[3],pos=3,cex=2)
dev.off()


pdf(output_file,width=12,height=4)
hi <- 3
hi0 <- 0

if (identical(dataset_name,"airline")) {
  
  hi0 <- 0.15
  prng <- range(out$window)
  prng[1] <- prng[1]-0.02*(prng[2]-prng[1])
  plot(prng,c(-hi-hi0-0.2,hi+hi0+0.2),type="n",bty="n",xlab="",ylab="",xaxt="n",yaxt="n")
  arrows(0,0,0,-hi-hi0,col=cols[1],length=0.1,lwd=1.2)
  arrows(0,0,0,hi+hi0,col=cols[2],length=0.1,lwd=1.2)
  abline(v=test_index,lty="dashed",lwd=1.2)
  abline(h=0,col="gray50",lwd=1.2)
  lines(c(0,max(out$window)),c(-1-hi0,-1-hi0),lty="dotted",col=cols[1],lwd=1.5)
  lines(c(0,max(out$window)),c(1+hi0,1+hi0),lty="dotted",col=cols[2],lwd=1.5)
  
  i_positives <- which(out$error>=error_thr)
  i_ipositives <- which(out$z>=ind_tr)
  ##points(out$window[i_positives],pmin(hi,out$z/ind_tr)[i_positives],col=cols[2],cex=0.5)
  ##points(out$window[i_ipositives],-pmin(hi,out$error/error_thr)[i_ipositives],col=cols[1],cex=0.5)
  for(i in setdiff(i_positives,i_ipositives)) {
    lines(c(out$window[i],out$window[i]),c(-min(hi+hi0,out$error[i]/error_thr+hi0),min(hi+hi0,out$z[i]/ind_tr+hi0)),col=cols[1],lty="longdash",lwd=1.2)
  }
  
  for(i in setdiff(i_ipositives,i_positives)) {
    lines(c(out$window[i],out$window[i]),c(-min(hi+hi0,out$error[i]/error_thr+hi0),min(hi+hi0,out$z[i]/ind_tr+hi0)),col=cols[2], lty="dashed",lwd=1.2)
  }
  for(i in intersect(i_positives,i_ipositives)) {
    lines(c(out$window[i],out$window[i]),c(-min(hi+hi0,out$error[i]/error_thr+hi0),min(hi+hi0,out$z[i]/ind_tr+hi0)),col="gray50",lwd=1.2)
  }
  
  lines(out$window,-pmin(hi+hi0,out$error/error_thr+hi0),col=cols[1],lwd=2)
  lines(out$window,pmin(hi+hi0,out$z/ind_tr+hi0),col=cols[2],lwd=2)
  text(test_index,-hi-0.2-hi0,"training",pos=2)
  text(test_index,-hi-0.2-hi0,"testing",pos=4)
  text(0,-hi-hi0,"generalisation error (RMSE)",pos=4,col=cols[1])
  text(0,hi+hi0,"d (Alg. 2)",pos=4,col=cols[2])
  text(0,-1-hi0,expression(sigma),pos=2,col=cols[1])
  text(0,1+hi0,expression(delta),pos=2,col=cols[2])
  text(0,-1.2-hi0,"positive",col=cols[1],cex=0.75,pos=4)
  text(0,-0.8-hi0,"negative",col=cols[1],cex=0.75,pos=4)
  text(0,1.2+hi0,"classified positive",col=cols[2],cex=0.75,pos=4)
  text(0,0.8+hi0,"classified negative",col=cols[2],cex=0.75,pos=4)

  } else {

prng <- range(out$window)
prng[1] <- prng[1]-0.02*(prng[2]-prng[1])
plot(prng,c(-hi-hi0-0.2,hi+hi0+0.2),type="n",bty="n",xlab="",ylab="",xaxt="n",yaxt="n")
arrows(0,0,0,-hi,col=cols[1],length=0.1,lwd=1.2)
arrows(0,0,0,hi,col=cols[2],length=0.1,lwd=1.2)
abline(v=test_index,lty="dashed",lwd=1.2)
abline(h=0,col="gray50",lwd=1.2)
lines(c(0,max(out$window)),c(-1,-1),lty="dotted",col=cols[1],lwd=1.2)
lines(c(0,max(out$window)),c(1,1),lty="dotted",col=cols[2],lwd=1.2)

i_positives <- which(out$error>=error_thr)
i_ipositives <- which(out$z>=ind_tr)
##points(out$window[i_positives],pmin(hi,out$z/ind_tr)[i_positives],col=cols[2],cex=0.5)
##points(out$window[i_ipositives],-pmin(hi,out$error/error_thr)[i_ipositives],col=cols[1],cex=0.5)
for(i in setdiff(i_positives,i_ipositives)) {
  lines(c(out$window[i],out$window[i]),c(-min(hi,out$error[i]/error_thr),min(hi,out$z[i]/ind_tr)),col=cols[1],lty="longdash",lwd=1.2)
}

for(i in setdiff(i_ipositives,i_positives)) {
  lines(c(out$window[i],out$window[i]),c(-min(hi,out$error[i]/error_thr),min(hi,out$z[i]/ind_tr)),col=cols[2], lty="dashed",lwd=1.2)
}
for(i in intersect(i_positives,i_ipositives)) {
  lines(c(out$window[i],out$window[i]),c(-min(hi,out$error[i]/error_thr),min(hi,out$z[i]/ind_tr)),col="gray50",lwd=1.2)
}

# if(length(setdiff(i_positives,i_ipositives))>0) {
#   idx <- setdiff(i_positives,i_ipositives)
#   i <- idx[which.min(out$error[idx])]
#   text(out$window[i],-min(hi,out$error[i]/error_thr),"FN",col=cols[1],cex=0.75,srt=90,adj=c(1.2,0))
#   cat(sprintf("false negatives: %d\n",length(which(out$window[idx]>=test_index))))
# }
# if(length(setdiff(i_ipositives,i_positives))>0) {
#   idx <- setdiff(i_ipositives,i_positives)
#   i <- idx[which.min(out$z[idx])]
#   text(out$window[i],min(hi,out$z[i]/ind_tr),"FP",col=cols[2],cex=0.75,srt=90,adj=c(-0.1,0))
#   cat(sprintf("false positives: %d\n",length(which(out$window[idx]>=test_index))))
# }
# if(length(intersect(i_positives,i_ipositives))>0) {
#   idx <- intersect(i_positives,i_ipositives)
#   i <- idx[which.min(out$z[idx])]
#   text(out$window[i],min(hi,out$z[i]/ind_tr),"TP",col="gray50",cex=0.75,srt=90,adj=c(-0.1,0))
#   cat(sprintf("true positives: %d\n",length(which(out$window[idx]>=test_index))))
# }

lines(out$window,-pmin(hi,out$error/error_thr),col=cols[1],lwd=2)
lines(out$window,pmin(hi,out$z/ind_tr),col=cols[2],lwd=2)
text(test_index,-hi-0.2,"training",pos=2)
text(test_index,-hi-0.2,"testing",pos=4)
text(0,-hi-hi0,"generalisation error (RMSE)",pos=4,col=cols[1])
text(0,hi+hi0,"d (Alg. 2)",pos=4,col=cols[2])
text(0,-1,expression(sigma),pos=2,col=cols[1])
text(0,1,expression(delta),pos=2,col=cols[2])
text(0,-1.2,"positive",col=cols[1],cex=0.75,pos=4)
text(0,-0.8,"negative",col=cols[1],cex=0.75,pos=4)
text(0,1.2,"classified positive",col=cols[2],cex=0.75,pos=4)
text(0,0.8,"classified negative",col=cols[2],cex=0.75,pos=4)
}
dev.off()