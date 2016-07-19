## Function for plotting mirrored histograms
# see http://stackoverflow.com/questions/29928712/how-to-create-mirrored-histograms
multhist <- function(..., bin.width, col, dir, xlab = NULL, ylab = NULL,
                     main = NULL, legends=NULL) {
  
  
  vals <- list(...)
  vals = lapply(vals,function(X){X[!is.na(X)]})
  vrng <- range(vals)
  
  brks <- seq(vrng[1] - abs(vrng[1]*0.1), 
              vrng[2] + abs(vrng[2]*0.1), 
              by = bin.width)
  
  yrng <- max(sapply(lapply(vals, hist, breaks = brks, plot=F), "[[", "counts"))
  yrng <- 1.2*c(-1*yrng, yrng)
  
  plot.new()
  plot.window(ylim = yrng, xlim = vrng)
  
  addhist <- function(x, col, dir) {
    par(new = TRUE)  
    hist(x = x, ylim = dir*yrng, col = col, xlab = "", ylab = "", 
         main = "", axes = FALSE, breaks = brks)
  }
  
  mapply(addhist, x = vals, col = col, dir = dir)
  
  py <- pretty(yrng)
  py <- py[py >= yrng[1] & py <= yrng[2]]
  axis(side = 2, at = py, labels = abs(py))
  axis(side = 1)
  title(main = main, xlab = xlab, ylab = ylab)
  if(!is.null(legend)){
    legend(0.8,yrng[1], legend = legends, col=col, pch=15)
  }
}