library(corrplot)

input <- read.table("input.txt",sep="\t",header=T,quote="",row.names=1)
cor_matrix <- cor(input[,6:ncol(input)])
col1=colorRampPalette(colors =c("Cyan1","white","#FFBBFF"),space="Lab") 

corrplot(cor_matrix, method = "color",
                     tl.col = 'black',  
                     #pch.cex = 0.5,  
                     is.corr = FALSE, 
					           #type='upper',
					           col = col1(100),
                     rect.col = "black",
                     order='hclust',addrect=1, 
                     addCoef.col = 'black', 
                     number.cex=0.75,
                     col.lim = c(0.8, 1)) 

