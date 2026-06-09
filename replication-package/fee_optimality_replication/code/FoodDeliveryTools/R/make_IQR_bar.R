make.IQR.bar <- function(k, x, h = 0.25, colour = 'goldenrod3'){
    rect(xleft   = k - h,
         xright  = k + h,
         ybottom = x[2],
         ytop    = x[4],
         col = colour)
    segments(x0 = k - h, x1 = k + h, y0 = x[3], lwd = 3)
    segments(x0 = k, y0 = x[4], y1 = x[5])
    segments(x0 = k, y0 = x[1], y1 = x[2])
    segments(x0 = k - h/3, x1 = k + h/3, y0 = x[1])
    segments(x0 = k - h/3, x1 = k + h/3, y0 = x[5])
}
