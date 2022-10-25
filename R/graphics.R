#' Plot a ROC object
#' 
#' @export
#' @param x ROC class object - see \code{\link{ROC}}
#' @param xlab char, x axis label
#' @param ylab char, y axis label
#' @param title char, plot title
#' @param use char one of 'base', 'ggplot' or 'ggplot2'
#' @param ... other arguments for \code{plot} or \code{ggplot2::labs}
#' @return if \code{use} is 'ggplot' or 'ggplot2' a ggplot object otherwise NULL
plot.ROC <- function(x,
                     xlab = "False Positive Rate (Specificity)",
                     ylab = "True Positive Rate (Sensitivity)", 
                     title = "ROC",
                     use = c("base", "ggplot2", "ggplot")[2],
                     ...){
  
  if (tolower(use[1]) %in% c("ggplot", "ggplot2")) {
    return(qplot.ROC(x, xlab = xlab, ylab = ylab, title = title, ...))
  }
  
  on.exit({
    par(opar)
  })
  opar <- par(no.readonly = TRUE)
  plot(x$fpr, x$tpr, type = "S",
       xlab = xlab, ylab = ylab, main = title, 
       asp = 1,
       xlim = c(0,1),
       ylim = c(0,1), 
       xaxs = "r", 
       yaxs = "r", ...)
  segments(0, 0, 1, 1, col = 'grey')
  text(1, 0, sprintf("AUC: %0.3f", AUC(x)), adj = c(1,0))
  
}


#' Plot a ROC using ggplot2
#' 
#' @export
#' @param x ROC class object - see \code{\link{ROC}}
#' @param xlab char, x axis label
#' @param ylab char, y axis label
#' @param title char, plot title
#' @param ... other arguments for  \code{\link[ggplot2]{labs}}
#' @return ggplot2 object
qplot.ROC <- function(x,
                      xlab = "False Positive Rate (Specificity)",
                      ylab = "True Positive Rate (Sensitivity)", 
                      title = "ROC",
                      ...){
  
  auc <- dplyr::tibble(
    tx = 1,
    ty = 0, 
    label = sprintf("AUC: %0.3f", AUC(x)))
  ggplot2::ggplot(data = x,
                  ggplot2::aes_string(x = "fpr", y = "tpr")) + 
    ggplot2::geom_step(colour = "black") + 
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1), colour = "grey") +
    ggplot2::labs(x = xlab, y = ylab, title = title, ...) +
    ggplot2::coord_fixed() + 
    ggplot2::annotate("text", x = auc$tx, y=auc$ty, label = auc$label, 
                      hjust = 1, vjust = 0) 
  
  
}

