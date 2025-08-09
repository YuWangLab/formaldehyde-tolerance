library(ggplot2)
library(RColorBrewer)

all_files <- grep("-diffgene.txt",list.files(path),value=TRUE)
for(i in 1:length(all_files)){
	input <- read.table(all_files[i],header = T,sep="\t",quote="",stringsAsFactors=F, na.strings = "NA")
	
	logFC <-input$log2FoldChange
	# adj <- input$pvalue
	adj <- input$padj
	
	data <- data.frame(logFC=logFC,padj=adj)
	data$sig[(data$padj > 0.05|data$padj=="NA")|(data$logFC < 1)& data$logFC > -1] <- "no"
	data$sig[data$padj <= 0.05 & data$logFC >= 1] <- "up"
	data$sig[data$padj <= 0.05 & data$logFC <= -1] <- "down"
	
	
	# 选最大值作为xlim的上下边界
	x_lim <- max(logFC,-logFC)
	# 绘制火山图
	
	#pdf(file = name,width=8,height=8)
	#theme_set(theme_bw())
	#par(new=T)
	x_title <- expression("log"[2]~"FoldChange")
	y_title<- expression("-log"[10]~"(FDR)")

	p <- ggplot(data,aes(logFC,-1*log10(padj),
					color = sig))+geom_point()+
	xlim(-x_lim,x_lim) +  labs(x=x_title,y=y_title) +theme_classic() 
	p <- p + scale_color_manual(values =c("#0072B5","grey","#BC3C28"))+
	geom_hline(yintercept=-log10(0.05),linetype=4)+
	geom_vline(xintercept=c(-1,1),linetype=4)
	p <- p +theme(panel.grid =element_blank())+
		theme(axis.line = element_line(size=0))+ylim(0,280)
	p <- p  +guides(colour = FALSE)
	p <- p +theme(axis.text=element_text(size=20),axis.title=element_text(size=20))
	p <- p +theme(axis.text=element_text(vjust=1,color="black"))+ #坐标轴标签字体变大，加粗
		theme(axis.line=element_line(linetype=1,color="black",size=2))+  #坐标轴加粗
		theme(axis.text.x=element_text(color='black',face="bold",angle=0,hjust=0.5, vjust=0.5, size=20),
			axis.text.y=element_text(color='black',face="bold",angle=0,hjust=0.5, vjust=0.5, size=20))+  #横坐标标签字体旋转45度
		theme( axis.ticks=element_line(colour="black",size=2,linetype=1,lineend=1),#坐标轴刻度线的设置 
				  axis.ticks.length = unit(0.6,"lines")#设置刻度线的高度
				  ) +
       theme(axis.title.x=element_text(vjust=1,size=25,face = "bold",color='black')) + #横坐标轴标题字体变大，加粗
       theme(axis.title.y=element_text(vjust=1,size=25,face = "bold",color='black')) #纵坐标轴标题字体变大，加粗
	#print(p)
	#dev.off()
	name <- sub("-diffgene.txt","\\.pdf",all_files[i])
	ggsave( name,p,width = 10,height=10 )
}
