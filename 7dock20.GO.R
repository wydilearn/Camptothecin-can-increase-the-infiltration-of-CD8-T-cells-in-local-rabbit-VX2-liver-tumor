######Video source: https://ke.biowolf.cn
######生信自学网: https://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：biowolf@foxmail.com
######答疑微信: 18520221056

#install.packages("colorspace")
#install.packages("stringi")
#install.packages("ggplot2")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("DOSE")
#BiocManager::install("clusterProfiler")
#BiocManager::install("enrichplot")


#引用包
library("clusterProfiler")
library("org.Hs.eg.db")
library("enrichplot")
library("ggplot2")

pvalueFilter=0.05         #p值过滤条件
qvalueFilter=0.05         #矫正后的p值过滤条件

setwd("D:\\biowolf\\dock\\20.GO")             #设置工作目录
rt=read.table("id.txt",sep="\t",header=T,check.names=F)     #读取id.txt文件
rt=rt[is.na(rt[,"entrezID"])==F,]                           #去除基因id为NA的基因
gene=rt$entrezID

#定义颜色类型
colorSel="qvalue"
if(qvalueFilter>0.05){
	colorSel="pvalue"
}

#GO富集分析
kk=enrichGO(gene = gene,OrgDb = org.Hs.eg.db, pvalueCutoff =1, qvalueCutoff = 1, ont="all", readable =T)
GO=as.data.frame(kk)
GO=GO[(GO$pvalue<pvalueFilter & GO$qvalue<qvalueFilter),]
#保存富集结果
write.table(GO,file="GO.txt",sep="\t",quote=F,row.names = F)

#定义显示Term数目
showNum=10
if(nrow(GO)<30){
	showNum=nrow(GO)
}

#柱状图
pdf(file="barplot.pdf",width = 10,height =7)
bar=barplot(kk, drop = TRUE, showCategory =showNum,split="ONTOLOGY",color = colorSel) + facet_grid(ONTOLOGY~., scale='free')
print(bar)
dev.off()
		
#气泡图
pdf(file="bubble.pdf",width = 10,height =7)
bub=dotplot(kk,showCategory = showNum, orderBy = "GeneRatio",split="ONTOLOGY", color = colorSel) + facet_grid(ONTOLOGY~., scale='free')
print(bub)
dev.off()


######Video source: https://ke.biowolf.cn
######生信自学网: https://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：biowolf@foxmail.com
######答疑微信: 18520221056
