DESeq2Method<-function(path,outputDir,count,compare_order,control_repeat_num,case_repeat_num){
	library(DESeq2)
	setwd(path)
	dir.create(outputDir)
	#all_files <- grep("count",list.files(path),value=TRUE)   #只提取包含count的文件名
	
	all_samplefiles <- compare_order #样本名，不含重复
	
	for(i in 1:(length(all_samplefiles)-1)){
		for(j in (i+1):length(all_samplefiles)){
			sampleCount <- as.matrix(cbind(count[,grep(all_samplefiles[i],colnames(count),value=TRUE) ],count[,grep(all_samplefiles[j],colnames(count),value=TRUE) ]))   #将差异分析的文件合并在一起

			condition <- factor(c(rep("control",times=control_repeat_num),rep("treat",times=case_repeat_num)))  #设置条件，将待分析的文件中的重复标记出来
			sampleCondition <- data.frame(row.names=colnames(sampleCount),condition)
			#sampleCondition <- sub("_[0-9].count","\\1",sampleFiles)
			dds <- DESeqDataSetFromMatrix(sampleCount,sampleCondition, design = ~condition)

			dds <- DESeq(dds)
			#res <- results(dds)
			res <- results(dds, contrast = c("condition", "treat", "control"))   #提取结果时，按照treat比上control的结果提取

			# head(res)
	
			output_filename<-paste0(all_samplefiles[j]," vs ",all_samplefiles[i],"_4sample-diffgene.txt")  #输出文件命名，treat在前control在后
			write.table(res,paste0(outputDir,"/",output_filename),quote=F,sep="\t",col.names=T,row.names=T)
		}
	}
}

path <- 'D:\\work\\22-王钰\\0-2-张知慧\\6-4株菌有无甲醛转录组数据\\2-表达量'
setwd( path )
outputDir<-"all_result" 
control_repeat_num <- 3  #对照组平行个数，最少2
case_repeat_num <- 3   #处理组平行个数，最少2
compare_order <- c( 'AF','AN','CF','CN' )   #样本间比较的顺序，control在前,不含重复
count<-read.table("sample12-count.txt",header=T,quote="",row.names=1,sep="\t",stringsAsFactors=FALSE)
result<-DESeq2Method(path,outputDir,count,compare_order,control_repeat_num,case_repeat_num)
