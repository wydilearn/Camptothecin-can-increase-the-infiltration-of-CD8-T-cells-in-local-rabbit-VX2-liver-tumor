######Video source: https://ke.biowolf.cn
######生信自学网: https://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：biowolf@foxmail.com
######答疑微信: 18520221056

scoreFile2="score2.1.txt"       #原始score文件
scoreFile3="score3.txt"       #过滤2次后的score文件
setwd("C:\\Users\\Administrator\\Desktop\\网络药理学\\PPI网络核心")    #设置工作目录
score2=read.table(scoreFile2, header=T, sep="\t",check.names =FALSE,row.names=1)    #读取输入文件
nCol=ncol(score2)        #多少个过滤条件

#对score2每个过滤条件循环，找出每个条件大于中位值的基因
mat=data.frame()      #新建数据框
for(i in colnames(score2)){
	print(paste0("filter2,",i,": ",median(score2[,i])))
	value=ifelse(score2[,i]>median(score2[,i]),1,0)
	mat=rbind(mat,value)
}
mat=t(mat)
colnames(mat)=colnames(score2)
row.names(mat)=row.names(score2)
#统计score2哪些基因所有条件都大于中位值,结果保存在score3
geneName=row.names(mat[rowSums(mat)==nCol,])
score3=score2[geneName,]
score3out=cbind(name=row.names(score3),score3)
write.table(score3out, file=scoreFile3, sep='\t', quote=F, row.names=F)
write.table(geneName, file="score3.gene.txt", sep='\t', quote=F, row.names=F,col.names=F)


######Video source: https://ke.biowolf.cn
######生信自学网: https://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：biowolf@foxmail.com
######答疑微信: 18520221056
