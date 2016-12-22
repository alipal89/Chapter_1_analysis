PCA_plot_function<- function (contrib, eigen,title) 
{
  if (ncol(contrib) == 1) {
    h <- c(unlist(contrib))
    n <- row.names(contrib)
    barplot(h, space = 0, names.arg = n)
    title(main = "variable contribution")
  }
  if (ncol(contrib) == 2) {
    s.corcircle(contrib[, 1:2]/max(abs(contrib[, 1:2])), 
                grid = FALSE)
    title(main = title, sub = paste("axis1 = ", 
                                                   round(eigen[1]/sum(eigen) * 100, 2), "%", "axis2 = ", 
                                                   round(eigen[2]/sum(eigen) * 100, 2), "%"))
  }
}

PCA_plot_function
