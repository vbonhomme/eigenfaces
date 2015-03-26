source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")


library(EBImage)
library(microbenchmark)
library(jpeg)
path <- "~/Research/R-packages/eigenfaces/pain_crops/f01_a.jpg"
microbenchmark(xxx <- readImage(path))
microbenchmark(xx <- readJPEG(path))

colorMode(xxx) <- Grayscale
display(xxx)
#
# xxx <- face(pain.p, c(0, 0))
# plot(xxx)
#
# posPC1 <- cbind(seq(-100, 100, 10), 0)
# xxx2 <- face(pain.p, pos=posPC1, xax = 3, yax=4)
#
# for (i in 1:dim(xxx2)[3]){
# plot(xxx2, i)
# Sys.sleep(0.1)
# }
#
# library(animation)
# saveGIF({
#   for (i in 1:dim(xxx2)[3]){
#     plot(xxx2, i)
#   }
# })
#
# ########
pain.a <- import_faces("~/Research/R-packages/eigenfaces/pain_crops/")
pain.a2 <- vflip(pain.a)
fac <- lf_structure(pain.a2, names=c("ind", "expr"))
pain.p <- PCA(pain.a2, fac=fac)
pain.p
class(pain.p)
plot(pain.p, "expr")
plot(pain.p, "ind")
scree_plot(pain.p)
