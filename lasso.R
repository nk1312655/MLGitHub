indus<-read.csv('30ValueWeightedIndustry.csv',header=T)
tbillrt<-read.csv('tbillrt.csv',header=T)
tbillrt<-tbillrt[,c(1,5)]
colnames(tbillrt)<-c('Date','rf')
#head(tbillrt)
#head(indus)
df<-merge(indus,tbillrt,by='Date')
#head(df)
#tail(df)
df[,2:31]<-df[,2:31]-df[,32]
df<-df[,1:31]
df<-subset(df,Date>=195912 & Date<=201612)
#head(df)

#============================Table 1====================================
#Summary statistics, industry portfolio excess returns, 1959:12-2016:12
monthmean<-apply(df[,2:31],2,mean)
Ann.mean<-round(monthmean*12,2)
Ann.volatilily<-round(apply(df[,2:31],2,sd)*sqrt(12),2)
Minimum<-round(apply(df[,2:31],2,min),2)
Maximum<-round(apply(df[,2:31],2,max),2)
Ann.Sharperatio<-round(monthmean*12/apply(df[,2:31],2,sd)/sqrt(12),2)

table1<-data.frame(Ann.mean,Ann.volatilily,Minimum,Maximum,Ann.Sharperatio)

train<-subset(df,Date>=195912 & Date<=201611)
train<-train[,2:31]
row.names(train)<-1:nrow(train)
Y<-subset(df,Date>=196001 & Date<=201612)
Y<-Y[,2:31]
row.names(Y)<-1:nrow(Y)


#head(train)
#tail(train)
#head(Y)
#tail(Y)

#==========================Table A2==================================
#LASSO predictive regression estimation results, 1960:01-2016:12

#glmnet
library(Matrix)
library(foreach)
library(glmnet)

X<-as.matrix(train)
result<-data.frame(col_name1 = 0)
result<-result[,-1]
lasso.Rsquared<-c()
CT<-c()
for (i in 1:length(Y)){
y<-as.vector(Y[,i])
cv.fit<-cv.glmnet(X,y,nlambda=100,nfolds=10,intercept=F)
cv.fit.pre<-predict(cv.fit, X, s=cv.fit$lambda)
cv.fit.sigma<-colSums((cv.fit.pre-y)^2)/length(y)
cv.aicc<-log(cv.fit.sigma)+2*(cv.fit$nzero+1)/(length(y)-cv.fit$nzero-2)
fit<-glmnet(X,y,lambda=cv.fit$lambda[which(cv.aicc==min(cv.aicc))],intercept=T)
coefs<-as.data.frame(as.matrix(round(coef(fit,exact=T),2)))
result<-cbind(result,coefs)
fit.pre<-predict(fit,X)
R.squared<-1-sum((fit.pre-y)^2)/sum((y-mean(y))^2)
lasso.Rsquared<-c(lasso.Rsquared,round(R.squared*100,2))
Si<-mean(Y[,i])/sd(Y[,i])
CTi<-R.squared*(1+Si^2)/(1-R.squared)/Si^2
CT<-c(CT,round(CTi,2))
}

names<-rownames(result)[-1]
colnames(result)<-names

result<-rbind(result[-1,],lasso.Rsquared,CT)
row.names(result)<-c(names,'R-squared','CT')

for (m in 1:length(result)){
for (n in 1:nrow(result)){
if (result[n,m]==0) 
result[n,m]='-'
}
}

result 

#=============================Table 2(Table A1)====================================
library(stringr)
library(installr)

df1<-result[1:30,]
R.squared<-c()
CT<-c()
OLSpostLASSO.coefs<-data.frame(row.names=names,stringsAsFactors=F)
for (i in 1:30){
	cols<-which(df1[,i]!='-')
	if (!is.empty(cols)){
		fmla<-as.formula(paste('Y[,i]~',paste(names[cols],collapse='+')))
		OLSpostLASSO.fit<-lm(fmla,data=train)
		coefs<-as.data.frame(coefficients(OLSpostLASSO.fit)[-1])
		coefs1<-c()
		for (j in 1:30){
			if (names[j] %in% row.names(coefs)) {
				coefs1[j]=coefs[names[j],]
			}else{coefs1[j]=99999}
		}
		OLSpostLASSO.coefs<-cbind(OLSpostLASSO.coefs,coefs1)
		R2<-summary(OLSpostLASSO.fit)$r.squared
		R.squared<-c(R.squared,R2*100)
		Si<-mean(Y[,i])/sd(Y[,i])
		CTi<-R2*(1+Si^2)/(1-R2)/Si^2
		CT<-c(CT,CTi)
	}else{
		OLSpostLASSO.coefs<-cbind(OLSpostLASSO.coefs,rep(99999,30))
		R.squared<-c(R.squared,99999)
		CT<-c(CT,99999)
	}
}

colnames(OLSpostLASSO.coefs)<-names


OLSpostLASSO.coefs<-rbind(OLSpostLASSO.coefs,R.squared,CT)


for (i in 1:nrow(OLSpostLASSO.coefs)){
for (j in 1:length(OLSpostLASSO.coefs)){
		if (round(OLSpostLASSO.coefs[i,j],2)==0){
			OLSpostLASSO.coefs[i,j]=round(OLSpostLASSO.coefs[i,j],3)
		}else{
			OLSpostLASSO.coefs[i,j]=round(OLSpostLASSO.coefs[i,j],2)
		}
}
}

for (i in 1:nrow(OLSpostLASSO.coefs)){
for (j in 1:length(OLSpostLASSO.coefs)){
	if (OLSpostLASSO.coefs[i,j]==99999) OLSpostLASSO.coefs[i,j]='-'
}
}

row.names(OLSpostLASSO.coefs)=c(names,'R.squared','CT')

OLSpostLASSO.coefs 

#==============================Table A3====================================
#OLS predictive regression estimation results, 1960:01-2016:12

#calculate coefficients
ols.coefs<-data.frame(col_name1=0)
ols.coefs<-ols.coefs[,-1]
for (i in 1:30){
ols.fit<-lm(Y[,i]~.,data=train)
ols.coefs<-cbind(ols.coefs,coefficients(ols.fit)[-1])
}

#rename the column name
names<-rownames(ols.coefs)
colnames(ols.coefs)<-names

#calculate R-squared
rsquared<-c()
CT<-c()
for (j in 1:30){
ols.fit<-lm(Y[,j]~.,data=train)
R2<-summary(ols.fit)$r.squared
rsquared<-c(rsquared,R2*100)
Sj<-mean(Y[,j])/sd(Y[,j])
CTj<-R2*(1+Sj^2)/(1-R2)/Sj^2
CT<-c(CT,CTj)
}
ols.coefs<-rbind(ols.coefs,rsquared,CT)

#rounding
for (m in 1:nrow(ols.coefs)){
for (n in 1:length(ols.coefs)){
if (round(ols.coefs[m,n],2)==0) {
ols.coefs[m,n]=round(ols.coefs[m,n],3)
}else if(round(ols.coefs[m,n],2)!=0){
ols.coefs[m,n]=round(ols.coefs[m,n],2)
}
}
}


#rename the row name
row.names(ols.coefs)<-c(colnames(ols.coefs),'R-squared','CT')

#Table A3
ols.coefs 


