indus<-read.csv('30ValueWeightedIndustry.csv',header=T)  #30 value-weighted industry portfolios from Kenneth French's Data Library
tbillrt<-read.csv('tbillrt.csv',header=T)		
tbillrt<-tbillrt[,c(1,5)]		#one-month Treasury bill return data
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

#=================================functions===================================
#Keep two decimal places
roundtable<-function(df){
	for (i in 1:nrow(df)){
	for (j in 1:length(df)){
		if (round(df[i,j],2)==0){
			df[i,j]=round(df[i,j],3)
		}else{
			df[i,j]=round(df[i,j],2)
		}
	}
	}
	df
}

#convert num to '-', in order to make the tables
zerotobar<-function(df,num=0){
	for (i in 1:nrow(df)){
	for (j in 1:length(df)){
		if (df[i,j]==num)
			df[i,j]='-'
	}
	}
	df
}

#mark significance at the 10%(5%) level, Significance codes: 5% '**' 10% '*'
significance<-function(df1,dfp){
	for (i in 1:30){
	for (j in 1:30){
		if(dfp[i,j]<0.05){
			df1[i,j]	<-paste0(df1[i,j],'**')
		}else if(dfp[i,j]>=0.05 & dfp[i,j]<0.1){
			df1[i,j]<-paste0(df1[i,j],'*')
		}		
	}
	}
	df1
}

#convert num1 to num2 in table dfn
convertNumtoNum<-function(dfn,num1,num2){
	for (i in 1:nrow(dfn)){
	for (j in 1:length(dfn)){
		if (dfn[i,j]==num1){
			dfn[i,j]=num2
		}
	}
	}
	dfn
}

#print to latex
tolatex<-function(dfn){
	print(xtable(dfn[,1:10],align=rep('c',length(dfn[,1:10])+1)))
	print(xtable(dfn[,11:20],align=rep('c',length(dfn[,11:20])+1)))
	print(xtable(dfn[,21:30],align=rep('c',length(dfn[,21:30])+1)))
}
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
train<-train[,2:31]      # dataframe of regressors
row.names(train)<-1:nrow(train)

Y<-subset(df,Date>=196001 & Date<=201612)		
Y<-Y[,2:31]		# dataframe of regressand
row.names(Y)<-1:nrow(Y)


#head(train)
#tail(train)
#head(Y)
#tail(Y)

#==========================Table A2==================================
#LASSO predictive regression estimation results, 1960:01-2016:12

#use the glmnet package
library(Matrix)
library(foreach)
library(glmnet)


X<-as.matrix(train)
X<-scale(X,center=T,scale=F)  #before running glmnet x should be centered

result<-data.frame(col_name1 = 0)
result<-result[,-1]
lasso.Rsquared<-c()
lasso.lambda<-c()
CT<-c()
for (i in 1:length(Y)){
y<-as.vector(Y[,i])
cv.fit<-cv.glmnet(X,y,standardize=T,nlambda=1000,nfolds=10,intercept=T)
cv.fit.pre<-predict(cv.fit, X, s=cv.fit$lambda,exact=TRUE)		#calculate the corrected AIC (AICc)
cv.fit.sigma<-colSums((cv.fit.pre-y)^2)/length(y)
cv.aicc<-log(cv.fit.sigma)+2*(cv.fit$nzero+1)/(length(y)-cv.fit$nzero-2)
cv.fit.lambda<-cv.fit$lambda[which(cv.aicc==min(cv.aicc))]		#choose the lambda with minimum AICc  
lasso.lambda<-c(lasso.lambda,cv.fit.lambda)		#dataframe of 30 lambda for later use
fit<-glmnet(X,y,standardize=T,lambda=cv.fit.lambda,intercept=T)	#lasso regression
coefs<-as.data.frame(as.matrix(coef(fit,exact=T)))
result<-cbind(result,coefs)
fit.pre<-predict(fit,X,exact=T)
R.squared<-1-sum((fit.pre-y)^2)/sum((y-mean(y))^2) 	#calculate the R-squared
lasso.Rsquared<-c(lasso.Rsquared,R.squared*100)
Si<-mean(Y[,i])/sd(Y[,i])		#calculate CT
CTi<-R.squared*(1+Si^2)/(1-R.squared)/Si^2
CT<-c(CT,CTi)
}

names<-rownames(result)[-1]
colnames(result)<-names

result<-rbind(result[-1,],lasso.Rsquared,CT)
row.names(result)<-c(names,'R-squared','CT')

lasso.result<-zerotobar(roundtable(result))

#Table A2
lasso.result
#lasso.lambda
tolatex(lasso.result)

#=========================================Table A6=====================================
#OLS post-ENet predictive regression estimation results, 1960:01-2016:12

enet.coefs<-data.frame(col_name1 = 0)	
enet.coefs<-enet.coefs[,-1]
for (i in 1:length(Y)){
	cv.enet<-cv.glmnet(X,Y[,i],standardize=T,alpha=0.5,nfolds=10,nlambda=100,intercept=T)
	cv.enet.pre<-predict(cv.enet, X, s=cv.enet$lambda,exact=TRUE)
	cv.enet.sigma<-colSums((cv.enet.pre-Y[,i])^2)/length(Y[,i])
	cv.enet.aicc<-log(cv.enet.sigma)+2*(cv.enet$nzero+1)/(length(Y[,i])-cv.enet$nzero-2)
	cv.enet.lambda<-cv.enet$lambda[which(cv.enet.aicc==min(cv.enet.aicc))]		#choose the lambda with minimum AICc
	enet.fit<-glmnet(X,Y[,i],standardize=T,alpha=0.5,lambda=cv.enet.lambda,intercept=T)
	coefs<-as.data.frame(as.matrix(coef(enet.fit,exact=T)))
	enet.coefs<-cbind(enet.coefs,coefs)	
}
colnames(enet.coefs)<-names
enet.coefs<-enet.coefs[-1,]		#ENet coefficients

OLSpostENet.coefs<-data.frame(row.names=names)	#dataframe of coefficients of OLS post-ENet
OLSpostENet.pvalue<-data.frame(row.names=names)	#dataframe of p-value of OLS post-LASSO according to conventional OLS post-ENet t-statistics
R.squared<-c()
CT<-c()
for (i in 1:30){
	cols<-which(enet.coefs[,i]!=0)		#index of regressors selected by ENet
	if (!is.empty(cols)){
		y<-Y[,i]
		fmla<-as.formula(paste('y~',paste0(names[cols],collapse='+')))		#re-estimate the coefficients using OLS for the selected predictor variables by ENet
		OLSpostENet.fit<-lm(fmla,data=train)
		coefs<-as.data.frame(coefficients(OLSpostENet.fit))
		p.value<-summary(OLSpostENet.fit)$coefficients		#p-value according to conventional OLS t-statistics
		coefs1<-c()		#make coefficients of predictor variables not selected by ENet equal to 99
		for (j in 1:30){
			if (names[j] %in% row.names(coefs)){
			coefs1[j]=coefs[names[j],]
			}else{coefs1[j]=99}
		}
		pvalues1<-c()		#make p-value(conventional OLS t-statistics) of predictor variables not selected by ENet equal to 99
		for (m in 1:30){
			if (names[m] %in% row.names(p.value)){
				pvalues1[m]=p.value[names[m],4]
			}else{pvalues1[m]=99}
		}
		OLSpostENet.coefs<-cbind(OLSpostENet.coefs,coefs1)
		OLSpostENet.pvalue<-cbind(OLSpostENet.pvalue,pvalues1)
		R2<-summary(OLSpostENet.fit)$r.squared 	#calculate R-squared
		print(i);print(R2)
		R.squared<-c(R.squared,R2*100)
		Si<-mean(Y[,i])/sd(Y[,i])		#calculate CT
		CTi<-R2*(1+Si^2)/(1-R2)/Si^2
		CT<-c(CT,CTi)
	}else{	  #if no predictor variables is selected, make coefficients&p-value equal to 99
		OLSpostENet.coefs<-cbind(OLSpostENet.coefs,rep(99,30))
		OLSpostENet.pvalue<-cbind(OLSpostENet.pvalue,rep(99,30))
		R.squared<-c(R.squared,99)
		CT<-c(CT,99)
	}
}

OLSpostENet.coefs<-rbind(OLSpostENet.coefs,R.squared,CT)
colnames(OLSpostENet.coefs)<-names
row.names(OLSpostENet.coefs)<-c(names,'R.squared','CT')

OLSpostENet.coefs<-zerotobar(roundtable(OLSpostENet.coefs),num=99)
OLSpostENet.coefs<-significance(OLSpostENet.coefs,OLSpostENet.pvalue)

#Table A6
OLSpostENet.coefs
tolatex(OLSpostENet.coefs)
#================================Table 2 and Table A1====================================
#Table 2: OLS post-LASSO predictive regression estimation results, 1960:01-2016:12
#Table A1: OLS post-LASSO predictive regression estimation results using White (1980) heteroskedasticity-robust standard errors, 1960:01-2016:12

library(stringr)
library(installr)
library(sandwich)
library(lmtest)

df1<-lasso.result[1:30,]
R.squared<-c()
CT<-c()
OLSpostLASSO.coefs<-data.frame(row.names=names,stringsAsFactors=F)	#dataframe of coefficients of OLS post-LASSO
OLSpostLASSO.pvalue<-data.frame(row.names=names)		#dataframe of p-value of OLS post-LASSO according to conventional OLS post-LASSO t-statistics
OLSpostLASSO.White.pvalue<-data.frame(row.names=names) 	#dataframe of p-value of OLS post-LASSO according to White heteroskedasticity-robust standard errors

for (i in 1:30){
	cols<-which(df1[,i]!='-') 	#index of regressors selected by LASSO
	if (!is.empty(cols)){
		fmla<-as.formula(paste('Y[,i]~',paste(names[cols],collapse='+'))) #re-estimate the coefficients using OLS for the selected predictor variables by LASSO
		OLSpostLASSO.fit<-lm(fmla,data=train)
		coefs<-as.data.frame(coefficients(OLSpostLASSO.fit))
		p.value<-summary(OLSpostLASSO.fit)$coefficients		#p-value according to conventional OLS t-statistics
		White.pvalue<-coeftest(OLSpostLASSO.fit,vcov=vcovHC(OLSpostLASSO.fit,type='HC')) #p-value according to White heteroskedasticity-robust standard errors


		coefs1<-c() 		#make coefficients of predictor variables not selected by LASSO equal to 99
		for (j in 1:30){
			if (names[j] %in% row.names(coefs)) {
				coefs1[j]=coefs[names[j],]
			}else{coefs1[j]=99}
		}

		pvalues1<-c()	#make p-value(conventional OLS t-statistics) of predictor variables not selected by LASSO equal to 99
		for (m in 1:30){
			if (names[m] %in% row.names(p.value)){
				pvalues1[m]=p.value[names[m],4]
			}else{pvalues1[m]=99}
		}

		Whitepvalues1<-c()  	#make p-value(White heteroskedasticity-robust standard errors) of predictor variables not selected by LASSO equal to 99
		for (q in 1:30){
			if (names[q] %in% row.names(White.pvalue)){
				Whitepvalues1[q]=White.pvalue[names[q],4]
			}else{Whitepvalues1[q]=99}
		}

		OLSpostLASSO.coefs<-cbind(OLSpostLASSO.coefs,coefs1)
		OLSpostLASSO.pvalue<-cbind(OLSpostLASSO.pvalue,pvalues1)
		OLSpostLASSO.White.pvalue<-cbind(OLSpostLASSO.White.pvalue,Whitepvalues1)
		R2<-summary(OLSpostLASSO.fit)$r.squared 	#calculate R-squared
		R.squared<-c(R.squared,R2*100)
		Si<-mean(Y[,i])/sd(Y[,i])		#calculate CT
		CTi<-R2*(1+Si^2)/(1-R2)/Si^2
		CT<-c(CT,CTi)
	}else{	  #if no predictor variables is selected, make coefficients&p-value equal to 99
		OLSpostLASSO.coefs<-cbind(OLSpostLASSO.coefs,rep(99,30))
		OLSpostLASSO.pvalue<-cbind(OLSpostLASSO.pvalue,rep(99,30))
		OLSpostLASSO.White.pvalue<-cbind(OLSpostLASSO.White.pvalue,rep(99,30))
		R.squared<-c(R.squared,99)
		CT<-c(CT,99)
	}
}

OLSpostLASSO.coefs<-rbind(OLSpostLASSO.coefs,R.squared,CT)
row.names(OLSpostLASSO.coefs)=c(names,'R.squared','CT')

colnames(OLSpostLASSO.coefs)<-names
colnames(OLSpostLASSO.pvalue)<-names
colnames(OLSpostLASSO.White.pvalue)<-names

OLSpostLASSO.coefs1<-zerotobar(roundtable(OLSpostLASSO.coefs),num=99)

#OLSpostLASSO.coefs1 

#Table 2
OLSpostLASSO.conventional.tstatistics<-significance(OLSpostLASSO.coefs1,OLSpostLASSO.pvalue)  
OLSpostLASSO.conventional.tstatistics	
tolatex(OLSpostLASSO.conventional.tstatistics)


#Table A1
OLSpostLASSO.White.coefs<-significance(OLSpostLASSO.coefs1,OLSpostLASSO.White.pvalue)        
OLSpostLASSO.White.coefs		
tolatex(OLSpostLASSO.White.coefs)


#====================================Table A4=========================================
#OLS post-LASSO predictive regression estimation results using Lee et al. (2016) post-selection confidence intervals, 1960:01-2016:12

library(selectiveInference)  #use the selectiveInference package

OLSpostLASSO.betas<-convertNumtoNum(OLSpostLASSO.coefs,99,0)[-(31:32),]  #dataframe of coefficients of OLS post-LASSO
OLSpostLASSO.selectiveInference.pvalue<-data.frame(row.names=names)
for (i in 1:30){	
	selectiveInference.beta<-OLSpostLASSO.betas[,i]
	coefs.names<-names[which(selectiveInference.beta!=0)]	#list of predictor variables' names selected by LASSO
	if (sum(selectiveInference.beta)!=0){
		selectiveInference.out<-fixedLassoInf(X,Y[,i],beta=selectiveInference.beta,lambda=lasso.lambda[i]/nrow(Y),sigma=sigma)
		pvalues<-data.frame(selectiveInference.out$pv,row.names=coefs.names)		#p-values according to Lee et al. (2016) post-selection confidence intervals
		pvalues1<-c()
		for (j in 1:30){		#make p-value(Lee et al. (2016) post-selection confidence) of predictor variables not selected by LASSO equal to 99
			if (names[j] %in% coefs.names){
				pvalues1[j]=pvalues[names[j],1]
			}else{pvalues1[j]=99}
		}
		OLSpostLASSO.selectiveInference.pvalue<-cbind(OLSpostLASSO.selectiveInference.pvalue,pvalues1)
	}else{
		OLSpostLASSO.selectiveInference.pvalue<-cbind(OLSpostLASSO.selectiveInference.pvalue,rep(99,30))
	}
}


colnames(OLSpostLASSO.selectiveInference.pvalue)<-names

#Table A4
OLSpostLASSO.selectiveInference<-significance(OLSpostLASSO.coefs1,OLSpostLASSO.selectiveInference.pvalue)
OLSpostLASSO.selectiveInference
tolatex(OLSpostLASSO.selectiveInference)

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
ols.coefs<-roundtable(ols.coefs)

#rename the row name
row.names(ols.coefs)<-c(colnames(ols.coefs),'R-squared','CT')

#Table A3
ols.coefs 

#significance
ols.pvalue<-data.frame(row.names=names)
for (i in 1:length(Y)){
	fit<-lm(Y[,i]~.,data=train)
	fit.pvalue<-as.data.frame(summary(fit)$coefficients)[-1,4]
	ols.pvalue<-cbind(ols.pvalue,fit.pvalue)
}
colnames(ols.pvalue)<-names

significance(ols.coefs,ols.pvalue)
tolatex(significance(ols.coefs,ols.pvalue))









