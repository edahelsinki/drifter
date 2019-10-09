## --------------------------------------------------
## Plot scalability results
## --------------------------------------------------

make_scalability_plot <- function(df, xstr, ystr, xlabel, ylabel, show_regression_line = TRUE, legend=FALSE, 
                                    phase="train") {
    options(scipen=10000)
    point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

    t <- cbind(df)
    t[t=="lm"] <- "LM"
    t[t=="svm"] <- "SVM"
    t[t=="randomforest"] <- "RF"
    names(t)[names(t) == 'model_f2'] <- 'Model'
    p <- ggplot(t)
    p <- p + geom_line(aes_string(x = xstr, y = ystr, group = "Model", linetype = "Model"))
    p <- p + geom_point(aes_string(x = xstr, y = ystr, group = "Model", shape = "Model"), size = 1.5)

    if (show_regression_line)
        p <- p + geom_smooth(aes_string(x = xstr, y = ystr, group = "method", linetype = "method"), method = "lm", se = FALSE, colour = "blue", size = 0.25)

    p <- p + xlab(xlabel)
    p <- p + ylab(ylabel)

   if (identical(phase,"train")){
       lim <- c(0.1, 20000)
   }else{
       lim <- c(0.01, 10)
   }
   p <- p + scale_x_log10(labels = point)
   p <- p + scale_y_log10(limits=lim,  expand = c(0, 0))

    lt=c("solid", "dashed", "dotted", 
         "dotdash", "longdash", "twodash", "1F", 
         "F1", "4C88C488")

    p <- p + theme_classic()
    p <- p + scale_linetype_manual(values =  lt)# c(1, 2, 3, 4, 5, 6))


    if (legend){
      # p <- p + theme(legend.position=c(0.2, 0.85),
      #               legend.title = element_blank(),
      #               legend.background = element_rect(fill = "white", colour = "gray"),
      #               legend.key.height=unit(0.3, "line"),
      #               legend.key.width=unit(1.5, "line")
      #               )
    } else{
        p <- p + theme(legend.position="none")
    }

    p <- p + theme(axis.text.x  = element_text(size = 7))
    p <- p + theme(axis.text.y  = element_text(size = 7))

    p <- p + theme(panel.border = element_blank())
    p <- p + theme(axis.line = element_line(colour = "black"))

    p
}
