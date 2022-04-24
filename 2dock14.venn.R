######Video source: https://ke.biowolf.cn
######生信自学网: https://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：biowolf@foxmail.com
######答疑微信: 18520221056

#install.packages("venn")


library(venn)               #引用包
drugFile="allTargets.symbol.txt"   #药物靶点文件
diseaseFile="Disease.txt"          #疾病相关基因文件
setwd("C:\\Users\\Administrator\\Desktop\\网络药理学\\疾病中药交集")   #设置工作目录
geneList=list()

#读取药物靶点文件
rt=read.table(drugFile,header=T,sep="\t",check.names=F)      #读取输入文件
geneNames=as.vector(rt[,4])                 #提取基因名称
geneNames=gsub("^ | $","",geneNames)        #去掉基因首尾的空格
uniqGene=unique(geneNames)                  #基因取unique
geneList[["Drug"]]=uniqGene
uniqLength=length(uniqGene)
print(paste("Drug",uniqLength,sep=" "))

#读取疾病相关基因文件
rt=read.table(diseaseFile,header=F,sep="\t",check.names=F)    #读取输入文件
geneNames=as.vector(rt[,1])                 #提取基因名称
geneNames=gsub("^ | $","",geneNames)        #去掉基因首尾的空格
uniqGene=unique(geneNames)                  #基因取unique
geneList[["Disease"]]=uniqGene
uniqLength=length(uniqGene)
print(paste("Disease",uniqLength,sep=" "))


#绘制venn图
mycol=c("#029149","#E0367A","#5D90BA","#431A3D","#91612D","#FFD121","#D8D155","#223D6C","#D20A13","#088247","#11AA4D","#7A142C","#5D90BA","#64495D","#7CC767")
pdf(file="venn.pdf",width=5,height=5)
venn(geneList,col=mycol[1:length(geneList)],zcolor=mycol[1:length(geneList)],box=F)
dev.off()

#保存药物和疾病的交集基因
intersectGenes=Reduce(intersect,geneList)
write.table(file="Drug_Disease.txt",intersectGenes,sep="\t",quote=F,col.names=F,row.names=F)


######Video source: https://ke.biowolf.cn
######生信自学网: https://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：biowolf@foxmail.com
######答疑微信: 18520221056
