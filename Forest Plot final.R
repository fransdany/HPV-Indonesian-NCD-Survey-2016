#Coba buat forest plot

library(forestplot)


#Praktik Forest PLot untuk 10 besar asosiasi terkuat
forplot_strongest <- read.table(file = "Top10_strongest_forplot.txt", header = TRUE)


tabss <- as.matrix(forplot_strongest)

OR2 <- with(forplot_strongest, cbind(OR))
lo2 <- with(forplot_strongest, cbind(low))
hi2 <- with(forplot_strongest, cbind(up))
class(OR2)
class(lo2)
class(hi2)

OR2 <- as.matrix(OR2)
lo2 <- as.matrix(lo2)
hi2 <- as.matrix(hi2)

class(OR2)
class(lo2)
class(hi2)

library(forestplot)
forestplot(tabss,
           title="Top 10 strongest HPV Type Associations",
           fn.ci_norm=fpDrawDiamondCI,
           grid = structure(c(1), cex=1.5, gp = gpar(col = "steelblue", 
           lty=3)), boxsize=0.005, xticks = xt, clip = c(-0.5, 4.0),
           col=fpColors(box="orange", line="turquoise"), size=0.5,
           xlab="OR estimates", vertices = TRUE, y.offset=0.3,
           new_page = TRUE, txt_gp = fpTxtGp(label= gpar (cex=0.5), 
           ticks= gpar (cex=0.8), xlab= gpar (cex = 0.8),
           title= gpar (cex = 1.2)))

