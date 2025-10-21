library(ggplot2) 
library(ggord)

input <- read.table("Supplementary_file_11.txt",sep="\t",header=T,quote="",row.names=1)

tmp <- t(input[2:ncol(input)])
cleandata <- tmp[,colSums(tmp != 0) > 0] 
data.pca <-prcomp(cleandata, center= T,scale.= F)

class <- factor( rep( c( 'AF','AN','CF','CN' ), each=3 ) )
p1 <- ggord(data.pca, grp_in=class,  arrow=0, vec_ext =0,txt=NULL)  

