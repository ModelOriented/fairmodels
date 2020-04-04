#' Plot Two Models
#'
#' @param x two_models object
#' @param ... other parameters
#'
#' @return ggplot object
#' @export
#'
#' @rdname plot_two_models

plot.two_models <- function(x, ...){

  fairness_data <- x$fairness
  performance_data <- x$performance
  print(x$labels)
  # Don't touch order here!

  fairness_data$to_vjust <- rep(0,nrow(fairness_data))
  for (i in seq_along(fairness_data$x)){
    # if is lowest in it's group than 1, else 0
    fairness_data$to_vjust[i] <- ifelse(fairness_data[i,"y"] == min(fairness_data[fairness_data$x == fairness_data$x[i],"y"]), 1.5, -0.5)
  }



plot1 <- ggplot(fairness_data, aes(x, y, fill = order)) +
            geom_bar(stat="identity", position = "identity")  +
            geom_text(aes(label=round(y,2)),
                      vjust = fairness_data$to_vjust,
                      color="black",
                      size=3 ,
                      fontface = "bold") +
            theme_drwhy()+
            theme(axis.text.x=element_text(angle=70, hjust=0.7),
                  legend.position = "none") +
            ylab(x$fairness_metric) +
            xlab("Models metrics in groups") +
            scale_fill_manual(values=c("#9a53c9", "#86aff0", "gray")) +
            ggtitle("2 models plot", subtitle = paste("Created with",x$labels[1],"and", x$labels[2]))

plot2 <- ggplot(performance_data, aes(x,y, fill = x)) +
        geom_bar(stat = "identity", width = 0.4) +
        geom_text(aes(label=round(y,3)), vjust=-1, color="black", size=3, fontface = "bold") +
        ylab("Accuracy") +
        theme_drwhy() +
        theme(legend.title = element_blank(),
              axis.text.x=element_text(angle=0, hjust=0.3)) +
        scale_fill_manual(values=c("#9a53c9", "#789ffa")) +
        xlab("Models") +
        ylab(x$performance_metric)

plot1 + plot2

}
