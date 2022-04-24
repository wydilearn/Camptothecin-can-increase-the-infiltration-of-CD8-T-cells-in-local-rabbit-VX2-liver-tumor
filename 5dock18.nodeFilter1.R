######Video source: https://ke.biowolf.cn
######生信自学网: https://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：biowolf@foxmail.com
######答疑微信: 18520221056

scoreFile1="score1.txt"       #原始score文件
scoreFile2="score2.txt"       #过滤1次后的score文件
setwd("D:\\biowolf\\dock\\18.CytoNCA\\filter1")    #设置工作目录

score1=read.table(scoreFile1, header=T, sep="\t",check.names =FALSE,row.names=1)    #读取输入文件
nCol=ncol(score1)      #多少个过滤条件

#对score1每个过滤条件循环，找出每个条件大于中位值的基因
mat=data.frame()      #新建数据框
for(i in colnames(score1)){
	print(paste0("filter1,",i,": ",median(score1[,i])))
	value=ifelse(score1[,i]>median(score1[,i]),1,0)
	mat=rbind(mat,value)
}
mat=t(mat)
colnames(mat)=colnames(score1)
row.names(mat)=row.names(score1)
#统计score1哪些基因所有条件都大于中位值,结果保存在score2
geneName=row.names(mat[rowSums(mat)==nCol,])
score2=score1[geneName,]
score2out=cbind(name=row.names(score2),score2)
write.table(score2out, file=scoreFile2, sep='\t', quote=F, row.names=F)
write.table(geneName, file="score2.gene.txt", sep='\t', quote=F, row.names=F,col.names=F)


######Video source: https://ke.biowolf.cn
######生信自学网: https://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：biowolf@foxmail.com
######答疑微信: 18520221056
