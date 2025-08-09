library(corrplot)

input <- read.table("input.txt",sep="\t",header=T,quote="",row.names=1)
cor_matrix <- cor(input[,6:ncol(input)])
col1=colorRampPalette(colors =c("Cyan1","white","#FFBBFF"),space="Lab") # space参数选择使用RGB或者CIE Lab颜色空间

corrplot(cor_matrix, method = "color",
                     tl.col = 'black',  #坐标轴标签字体颜色
                     #pch.cex = 0.5,   #调整标签字体大小，此处不起作用
                     is.corr = FALSE, #当此参数设置成true时，col.lim只是更改色度条，不会更改起始颜色
					           #type='upper',
					           col = col1(100),
                     rect.col = "black",
                     order='hclust',addrect=1, #添加矩形边框，1则标外边框 
                     addCoef.col = 'black',  #显示相关系数数值
                     number.cex=0.75, #相关系数字体大小调节
                     col.lim = c(0.8, 1)) #还可以加很多参数

