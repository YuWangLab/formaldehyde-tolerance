DESeq2Method<-function(path,outputDir,count,compare_order,control_repeat_num,case_repeat_num){
	library(DESeq2)
	setwd(path)
	dir.create(outputDir)
	#all_files <- grep("count",list.files(path),value=TRUE)   
	
	all_samplefiles <- compare_order 
	
	for(i in 1:(length(all_samplefiles)-1)){
		for(j in (i+1):length(all_samplefiles)){
			sampleCount <- as.matrix(cbind(count[,grep(all_samplefiles[i],colnames(count),value=TRUE) ],count[,grep(all_samplefiles[j],colnames(count),value=TRUE) ]))  

			condition <- factor(c(rep("control",times=control_repeat_num),rep("treat",times=case_repeat_num))) 
			sampleCondition <- data.frame(row.names=colnames(sampleCount),condition)
			#sampleCondition <- sub("_[0-9].count","\\1",sampleFiles)
			dds <- DESeqDataSetFromMatrix(sampleCount,sampleCondition, design = ~condition)

			dds <- DESeq(dds)
			#res <- results(dds)
			res <- results(dds, contrast = c("condition", "treat", "control"))  

			# head(res)
	
			output_filename<-paste0(all_samplefiles[j]," vs ",all_samplefiles[i],"_4sample-diffgene.txt")  
			write.table(res,paste0(outputDir,"/",output_filename),quote=F,sep="\t",col.names=T,row.names=T)
		}
	}
}

path <- './'
setwd( path )
outputDir<-"all_result" 
control_repeat_num <- 3 
case_repeat_num <- 3   
compare_order <- c( '3F','3N','1F','1N' )   
count<-read.table("Supplementary file 2.txt",header=T,quote="",row.names=1,sep="\t",stringsAsFactors=FALSE)
result<-DESeq2Method(path,outputDir,count,compare_order,control_repeat_num,case_repeat_num)
