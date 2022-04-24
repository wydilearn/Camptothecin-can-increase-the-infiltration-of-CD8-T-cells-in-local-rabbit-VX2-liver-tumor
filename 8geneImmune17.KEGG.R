#install.packages("colorspace")
#install.packages("stringi")
#install.packages("ggplot2")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("org.Hs.eg.db")
#BiocManager::install("DOSE")
#BiocManager::install("clusterProfiler")
#BiocManager::install("enrichplot")
#BiocManager::install("pathview")


#引用包
library("clusterProfiler")
library("org.Hs.eg.db")
library("enrichplot")
library("ggplot2")
library("pathview")

pvalueFilter=0.05     #p值过滤条件
qvalueFilter=1        #矫正后的p值过滤条件
setwd("C:\\Users\\lexb4\\Desktop\\geneImmune\\17.KEGG")      #设置工作目录

#读取输入文件
rt=read.table("corResult.txt", header=T, sep="\t", check.names=F)

#基因名字转换为基因id
genes=as.vector(rt[,2])
entrezIDs=mget(genes, org.Hs.egSYMBOL2EG, ifnotfound=NA)
entrezIDs=as.character(entrezIDs)
rt=cbind(rt, entrezIDs)
gene=entrezIDs[entrezIDs!="NA"]        #去除基因id为NA的基因
Cor=as.vector(rt[,3])
names(Cor)=gene

#定义颜色类型
colorSel="qvalue"
if(qvalueFilter>0.05){
	colorSel="pvalue"
}

#kegg富集分析
kk <- enrichKEGG(gene = gene, organism = "hsa", pvalueCutoff =1, qvalueCutoff =1)
KEGG=as.data.frame(kk)
KEGG$geneID=as.character(sapply(KEGG$geneID,function(x)paste(rt$Gene[match(strsplit(x,"/")[[1]],as.character(rt$entrezID))],collapse="/")))
KEGG=KEGG[(KEGG$pvalue<pvalueFilter & KEGG$qvalue<qvalueFilter),]
#保存富集结果
write.table(KEGG,file="KEGG.txt",sep="\t",quote=F,row.names = F)

#定义显示Term数目
showNum=30
if(nrow(KEGG)<showNum){
	showNum=nrow(KEGG)
}

#柱状图
pdf(file="barplot.pdf",width = 9,height = 7)
barplot(kk, drop = TRUE, showCategory = showNum, color = colorSel)
dev.off()

#气泡图
pdf(file="bubble.pdf",width = 9,height = 7)
dotplot(kk, showCategory = showNum, orderBy = "GeneRatio",color = colorSel)
dev.off()

#绘制通路图
keggId="hsa04115"       #选择通路，进行通路图绘制(可以修改)
pv.out=pathview(gene.data=Cor, pathway.id = keggId, species = "hsa", out.suffix = "pathview")


######生信自学网: https://www.biowolf.cn/
######课程链接1: https://shop119322454.taobao.com
######课程链接2: https://ke.biowolf.cn
######课程链接3: https://ke.biowolf.cn/mobile
######光俊老师邮箱：seqbio@foxmail.com
######光俊老师微信: eduBio
