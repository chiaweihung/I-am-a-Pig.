##########----------environment setting----------##########
  #clear
  rm(list=ls())
  
  #package
  library(glmnet)
  
  #import raw data
  data<-read.table("C:/Users/MSI/Desktop/chika/chin/Final_edition_1102/timeadjust_20171102.csv",header=TRUE,sep=",")
##########----------environment setting----------##########
  
##########------------trend variable-------------##########
  #F1
  n<-1
  tmp1<-0
  F1<-matrix(ncol=ncol(data),nrow=nrow(data))
  for ( n in 1:(ncol(data)-1)){
    stage<-1
    for (stage in 1:(nrow(data)-1)){
      if(is.na(data[stage,n])+is.na(data[stage+1,n])!=0){
        tmp1<-NA
      }else if(as.numeric(data[stage,n])<as.numeric(data[stage+1,n])){
        tmp1<-1
      }else if(as.numeric(data[stage,n])>as.numeric(data[stage+1,n])){
        tmp1<-0
      }else{
        tmp<-0
      }
      F1[stage+1,n]<-tmp1
      stage=stage+1
    }
    n=n+1
  }
  F1[,ncol(data)]<-data[,ncol(data)]
  colnames(F1)<-c(1:(ncol(data)-1),"index")
  
  #F2
  n<-1
  tmp1<-0
  F2<-matrix(ncol=ncol(data),nrow=nrow(data))
  for ( n in 1:(ncol(data)-2)){
    stage<-1
    for (stage in 1:(nrow(data)-2)){
      if(is.na(data[stage,n])+is.na(data[stage+1,n])+is.na(data[stage+2,n])!=0){
        tmp1<-NA
      }else if(as.numeric(data[stage,n])<as.numeric(data[stage+1,n]) & as.numeric(data[stage+1,n])<as.numeric(data[stage+2,n])){
        tmp1<-1
      }else if(as.numeric(data[stage,n])>as.numeric(data[stage+1,n]) & as.numeric(data[stage+1,n])>as.numeric(data[stage+2,n])){
        tmp1<-0
      }else{
        tmp<-0
      }
      F2[stage+2,n]<-tmp1
      stage=stage+1
    }
    n=n+1
  }
  F2[,ncol(data)]<-data[,ncol(data)]
  colnames(F2)<-c(1:(ncol(data)-1),"index")
  
  #F3
  n<-1
  tmp1<-0
  F3<-matrix(ncol=ncol(data),nrow=nrow(data))
  for ( n in 1:(ncol(data)-3)){
    stage<-1
    for (stage in 1:(nrow(data)-3)){
      if(is.na(data[stage,n])+is.na(data[stage+1,n])+is.na(data[stage+2,n])+is.na(data[stage+3,n])!=0){
        tmp1<-NA
      }else if(as.numeric(data[stage,n])<as.numeric(data[stage+1,n]) & as.numeric(data[stage+1,n])<as.numeric(data[stage+2,n]) & as.numeric(data[stage+2,n])<as.numeric(data[stage+3,n])){
        tmp1<-1
      }else if(as.numeric(data[stage,n])>as.numeric(data[stage+1,n]) & as.numeric(data[stage+1,n])>as.numeric(data[stage+2,n]) & as.numeric(data[stage+2,n])>as.numeric(data[stage+3,n])){
        tmp1<-0
      }else{
        tmp<-0
      }
      F3[stage+3,n]<-tmp1
      stage=stage+1
    }
    n=n+1
  }
  F3[,ncol(data)]<-data[,ncol(data)]
  colnames(F3)<-c(1:(ncol(data)-1),"index")
##########------------trend variable-------------##########
  
##########---------------win rate----------------##########
  ###win rate
  win.rate<-function(f,Rolling.Y,Predict.M){
  if(f==1){
    F<-F1
  }else if(f==2){
    F<-F2
  }else{
    F<-F3
  }
  Rolling.M<-12*Rolling.Y
    ##predict value of each time
    lasso=function(t,Rolling.M){
      #data split
      i<-1
      tmp<-matrix(nrow=Rolling.M,ncol=1)
      skip<-matrix(nrow=Rolling.M,ncol=1)
      rolling<-matrix(nrow=Rolling.M)
      for (i in 1:dim(F)[2]){
        if(sum(is.na(F[t:(t+Rolling.M),i]))==0){
          tmp<-as.numeric(F[t:(t+Rolling.M-1),i])
          tmp<-as.data.frame(tmp)
          rolling<-cbind(rolling,tmp)
          colnames(rolling)[ncol(rolling)]<-i
        }else{
          skip<-F[t:(t+Rolling.M-1),i]
        }
        i=i+1
      }
      rolling<-rolling[,-1]
      colnames(rolling)[ncol(rolling)]<-'Index'
      
      #model-building
      x=model.matrix(Index~.,rolling)[,-1]
      y=rolling[,ncol(rolling)]
      grid=10^seq(10,-2,length =100)
      
      #train & test
      train=c(1:(Rolling.M*.8))
      test =(-train)
      y.test=y[test]
      
      #process of caculate
      lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
      cv.out=cv.glmnet(x[train,],y[train],alpha=1)
      bestlam=cv.out$lambda.min
      lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
      out=glmnet(x,y,alpha=1,lambda=grid)
      lasso.coef=predict(out,type="coefficients",s=bestlam)[1:ncol(rolling),]
      result<-as.matrix(lasso.coef[lasso.coef!=0])
      
      #feature selection
      selected<-if(dim(result)[1]==1){
        0
        }else{
          as.numeric(substr(rownames(result)[2:nrow(result)],2,nchar(rownames(result)[2:nrow(result)])-1))
        }
      value<-sum(F[(Rolling.M+t),selected]*result[2:dim(result)[1]])+result[1]
      value
    }

      #compare the predict and realistic value
      predict<-matrix(nrow=(nrow(F)))
      t<-2
      for (t in (f+1):(nrow(F)-Rolling.M)){
        a<-lasso(t,Rolling.M)
        predict[Rolling.M+t,1]<-a
        t=t+1
        compare<-na.omit(cbind(F[,'index'],predict)[(Rolling.M+f+1):(nrow(F)),])
      }
      
      #win rate
      n=1;try=0
      for (n in 1:(nrow(compare)-Predict.M)){
        if(compare[n,1]>compare[n+1,1] & compare[(n+Predict.M-1),2]>compare[(n+Predict.M),2]){
          try1=1
        }else if(compare[n,1]<compare[n+1,1] & compare[(n+Predict.M-1),2]<compare[(n+Predict.M),2]){
          try1=1
        }else{
          try1=0
        }
        try=try+try1
        n=n+1
      }
      (rate=try/(nrow(compare)-Predict.M))
  }
  
  ###win rate with threshold 0.01
  win.rate<-function(f,Rolling.Y,Predict.M){
    if(f==1){
      F<-F1
    }else if(f==2){
      F<-F2
    }else{
      F<-F3
    }
    Rolling.M<-12*Rolling.Y
    ##predict value of each time
    lasso=function(t,Rolling.M){
      #data split
      i<-1
      tmp<-matrix(nrow=Rolling.M,ncol=1)
      skip<-matrix(nrow=Rolling.M,ncol=1)
      rolling<-matrix(nrow=Rolling.M)
      for (i in 1:dim(F)[2]){
        if(sum(is.na(F[t:(t+Rolling.M),i]))==0){
          tmp<-as.numeric(F[t:(t+Rolling.M-1),i])
          tmp<-as.data.frame(tmp)
          rolling<-cbind(rolling,tmp)
          colnames(rolling)[ncol(rolling)]<-i
        }else{
          skip<-F[t:(t+Rolling.M-1),i]
        }
        i=i+1
      }
      rolling<-rolling[,-1]
      colnames(rolling)[ncol(rolling)]<-'Index'
      
      #model-building
      x=model.matrix(Index~.,rolling)[,-1]
      y=rolling[,ncol(rolling)]
      grid=10^seq(10,-2,length =100)
      
      #train & test
      train=c(1:(Rolling.M*.8))
      test =(-train)
      y.test=y[test]
      
      #process of caculate
      lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
      cv.out=cv.glmnet(x[train,],y[train],alpha=1)
      bestlam=cv.out$lambda.min
      lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
      out=glmnet(x,y,alpha=1,lambda=grid)
      lasso.coef=predict(out,type="coefficients",s=bestlam)[1:ncol(rolling),]
      result<-as.matrix(lasso.coef[lasso.coef!=0])
      
      #feature selection
      selected<-if(dim(result)[1]==1){
        0
      }else{
        as.numeric(substr(rownames(result)[2:nrow(result)],2,nchar(rownames(result)[2:nrow(result)])-1))
      }
      value<-sum(F[(Rolling.M+t),selected]*result[2:dim(result)[1]])+result[1]
      value
    }
    
    #compare the predict and realistic value
    predict<-matrix(nrow=(nrow(F)))
    t<-2
    for (t in (f+1):(nrow(F)-Rolling.M)){
      a<-lasso(t,Rolling.M)
      predict[Rolling.M+t,1]<-a
      t=t+1
      compare<-na.omit(cbind(F[,'index'],predict)[(Rolling.M+f+1):(nrow(F)),])
    }
    
    #win rate with threshold 0.01
    n=1;try=0
    for (n in 1:(nrow(compare)-Predict.M)){
      if(compare[(n+Predict.M-1),2]-compare[(n+Predict.M),2]>0.01){
        if(compare[n,1]>compare[(n+1),1] & compare[(n+Predict.M-1),2]>compare[(n+Predict.M),2]){
          try1=1
        }else{
          try1=0
        }
      }
      else if(compare[(n+Predict.M-1),2]-compare[(n+Predict.M),2]<(-0.01)){
        if(compare[n,1]<compare[n+1,1] & compare[(n+Predict.M-1),2]<compare[(n+Predict.M),2]){
          try1=1
        }else{
          try1=0
        }
      }
      else{
        try1=0
      }
    try=try+try1
    n=n+1
    }
    sign<-sum(diff(compare[(Predict.M+1):nrow(compare),2])<(-0.01) | diff(compare[(Predict.M+1):nrow(compare),2])>0.01)
    (rate=try/sign)
  } 
  
  ###win rate with threshold 0.02
  win.rate<-function(f,Rolling.Y,Predict.M){
    if(f==1){
      F<-F1
    }else if(f==2){
      F<-F2
    }else{
      F<-F3
    }
    Rolling.M<-12*Rolling.Y
    ##predict value of each time
    lasso=function(t,Rolling.M){
      #data split
      i<-1
      tmp<-matrix(nrow=Rolling.M,ncol=1)
      skip<-matrix(nrow=Rolling.M,ncol=1)
      rolling<-matrix(nrow=Rolling.M)
      for (i in 1:dim(F)[2]){
        if(sum(is.na(F[t:(t+Rolling.M),i]))==0){
          tmp<-as.numeric(F[t:(t+Rolling.M-1),i])
          tmp<-as.data.frame(tmp)
          rolling<-cbind(rolling,tmp)
          colnames(rolling)[ncol(rolling)]<-i
        }else{
          skip<-F[t:(t+Rolling.M-1),i]
        }
        i=i+1
      }
      rolling<-rolling[,-1]
      colnames(rolling)[ncol(rolling)]<-'Index'
      
      #model-building
      x=model.matrix(Index~.,rolling)[,-1]
      y=rolling[,ncol(rolling)]
      grid=10^seq(10,-2,length =100)
      
      #train & test
      train=c(1:(Rolling.M*.8))
      test =(-train)
      y.test=y[test]
      
      #process of caculate
      lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
      cv.out=cv.glmnet(x[train,],y[train],alpha=1)
      bestlam=cv.out$lambda.min
      lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
      out=glmnet(x,y,alpha=1,lambda=grid)
      lasso.coef=predict(out,type="coefficients",s=bestlam)[1:ncol(rolling),]
      result<-as.matrix(lasso.coef[lasso.coef!=0])
      
      #feature selection
      selected<-if(dim(result)[1]==1){
        0
      }else{
        as.numeric(substr(rownames(result)[2:nrow(result)],2,nchar(rownames(result)[2:nrow(result)])-1))
      }
      value<-sum(F[(Rolling.M+t),selected]*result[2:dim(result)[1]])+result[1]
      value
    }
    
    #compare the predict and realistic value
    predict<-matrix(nrow=(nrow(F)))
    t<-2
    for (t in (f+1):(nrow(F)-Rolling.M)){
      a<-lasso(t,Rolling.M)
      predict[Rolling.M+t,1]<-a
      t=t+1
      compare<-na.omit(cbind(F[,'index'],predict)[(Rolling.M+f+1):(nrow(F)),])
    }
    
    #win rate with threshold 0.02
    n=1;try=0
    for (n in 1:(nrow(compare)-Predict.M)){
      if(compare[(n+Predict.M-1),2]-compare[(n+Predict.M),2]>0.02){
        if(compare[n,1]>compare[(n+1),1] & compare[(n+Predict.M-1),2]>compare[(n+Predict.M),2]){
          try1=1
        }else{
          try1=0
        }
      }
      else if(compare[(n+Predict.M-1),2]-compare[(n+Predict.M),2]<(-0.02)){
        if(compare[n,1]<compare[n+1,1] & compare[(n+Predict.M-1),2]<compare[(n+Predict.M),2]){
          try1=1
        }else{
          try1=0
        }
      }
      else{
        try1=0
      }
      try=try+try1
      n=n+1
    }
    sign<-sum(diff(compare[(Predict.M+1):nrow(compare),2])<(-0.02) | diff(compare[(Predict.M+1):nrow(compare),2])>0.02)
    (rate=try/sign)
  }  
  
  ###win rate with threshold 0.03
  win.rate<-function(f,Rolling.Y,Predict.M){
    if(f==1){
      F<-F1
    }else if(f==2){
      F<-F2
    }else{
      F<-F3
    }
    Rolling.M<-12*Rolling.Y
    ##predict value of each time
    lasso=function(t,Rolling.M){
      #data split
      i<-1
      tmp<-matrix(nrow=Rolling.M,ncol=1)
      skip<-matrix(nrow=Rolling.M,ncol=1)
      rolling<-matrix(nrow=Rolling.M)
      for (i in 1:dim(F)[2]){
        if(sum(is.na(F[t:(t+Rolling.M),i]))==0){
          tmp<-as.numeric(F[t:(t+Rolling.M-1),i])
          tmp<-as.data.frame(tmp)
          rolling<-cbind(rolling,tmp)
          colnames(rolling)[ncol(rolling)]<-i
        }else{
          skip<-F[t:(t+Rolling.M-1),i]
        }
        i=i+1
      }
      rolling<-rolling[,-1]
      colnames(rolling)[ncol(rolling)]<-'Index'
      
      #model-building
      x=model.matrix(Index~.,rolling)[,-1]
      y=rolling[,ncol(rolling)]
      grid=10^seq(10,-2,length =100)
      
      #train & test
      train=c(1:(Rolling.M*.8))
      test =(-train)
      y.test=y[test]
      
      #process of caculate
      lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
      cv.out=cv.glmnet(x[train,],y[train],alpha=1)
      bestlam=cv.out$lambda.min
      lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
      out=glmnet(x,y,alpha=1,lambda=grid)
      lasso.coef=predict(out,type="coefficients",s=bestlam)[1:ncol(rolling),]
      result<-as.matrix(lasso.coef[lasso.coef!=0])
      
      #feature selection
      selected<-if(dim(result)[1]==1){
        0
      }else{
        as.numeric(substr(rownames(result)[2:nrow(result)],2,nchar(rownames(result)[2:nrow(result)])-1))
      }
      value<-sum(F[(Rolling.M+t),selected]*result[2:dim(result)[1]])+result[1]
      value
    }
    
    #compare the predict and realistic value
    predict<-matrix(nrow=(nrow(F)))
    t<-2
    for (t in (f+1):(nrow(F)-Rolling.M)){
      a<-lasso(t,Rolling.M)
      predict[Rolling.M+t,1]<-a
      t=t+1
      compare<-na.omit(cbind(F[,'index'],predict)[(Rolling.M+f+1):(nrow(F)),])
    }
    
    #win rate with threshold 0.03
    n=1;try=0
    for (n in 1:(nrow(compare)-Predict.M)){
      if(compare[(n+Predict.M-1),2]-compare[(n+Predict.M),2]>0.03){
        if(compare[n,1]>compare[(n+1),1] & compare[(n+Predict.M-1),2]>compare[(n+Predict.M),2]){
          try1=1
        }else{
          try1=0
        }
      }
      else if(compare[(n+Predict.M-1),2]-compare[(n+Predict.M),2]<(-0.03)){
        if(compare[n,1]<compare[n+1,1] & compare[(n+Predict.M-1),2]<compare[(n+Predict.M),2]){
          try1=1
        }else{
          try1=0
        }
      }
      else{
        try1=0
      }
      try=try+try1
      n=n+1
    }
    sign<-sum(diff(compare[(Predict.M+1):nrow(compare),2])<(-0.03) | diff(compare[(Predict.M+1):nrow(compare),2])>0.03)
    (rate=try/sign)
  } 
  
  ###summary of win rate
  a1<-c(win.rate(1,5,1),win.rate(1,5,2),win.rate(1,5,3),win.rate(1,10,1),win.rate(1,10,2),win.rate(1,10,3))
  a2<-c(win.rate(2,5,1),win.rate(2,5,2),win.rate(2,5,3),win.rate(2,10,1),win.rate(2,10,2),win.rate(2,10,3))
  a3<-c(win.rate(3,5,1),win.rate(3,5,2),win.rate(3,5,3))
  a4<-c(win.rate(3,10,1),win.rate(3,10,2),win.rate(3,10,3))
  a<-c(a1,a2,a3,a4)
  b<-array(a,dim=c(3,2,3),dimnames=list(Predict=c("1","2","3"),Rolling=c("5","10"),Trend=c("F1","F2","F3")))
  b
##########---------------win rate----------------##########
  
##########----------------each X-----------------##########
  ###function of each x
  each.x<-function(f,Rolling.Y,Predict.M){
    if(f==1){
      F<-F1
    }else if(f==2){
      F<-F2
    }else{
      F<-F3
    }
    
    Rolling.M<-12*Rolling.Y
    
    ##function of lasso
    lasso=function(t,Rolling.M){
      
      #data split
      i<-1
      tmp<-matrix(nrow=Rolling.M,ncol=1)
      skip<-matrix(nrow=Rolling.M,ncol=1)
      rolling<-matrix(nrow=Rolling.M)
      for (i in 1:dim(F)[2]){
        if(sum(is.na(F[t:(t+Rolling.M),i]))==0){
          tmp<-as.numeric(F[t:(t+Rolling.M-1),i])
          tmp<-as.data.frame(tmp)
          rolling<-cbind(rolling,tmp)
          colnames(rolling)[ncol(rolling)]<-i
        }else{
          skip<-F[t:(t+Rolling.M-1),i]
        }
        i=i+1
      }
      rolling<-rolling[,-1]
      colnames(rolling)[ncol(rolling)]<-'Index'
      
      #model-building
      x=model.matrix(Index~.,rolling)[,-1]
      y=rolling[,ncol(rolling)]
      grid=10^seq(10,-2,length =100)
      
      #train & test
      train=c(1:(Rolling.M*.8))
      test =(-train)
      y.test=y[test]
      
      #Lasso
      lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
      cv.out=cv.glmnet(x[train,],y[train],alpha=1)
      bestlam=cv.out$lambda.min
      lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
      out=glmnet(x,y,alpha=1,lambda=grid)
      lasso.coef=predict(out,type="coefficients",s=bestlam)[1:ncol(rolling),]
      result<-as.matrix(lasso.coef[lasso.coef!=0])
    }
    
    ##summary
    summary<-matrix(nrow=nrow(data)+1,ncol=ncol(data)-1)
    colnames(summary)<-colnames(1:ncol(data)-1)
    
    for(t in (f+1):(nrow(F)-Rolling.M)){
      a<-lasso(t,Rolling.M)
      selected<-if(dim(a)[1]==1){
        0
      }else{
        as.numeric(substr(rownames(a)[2:nrow(a)],2,nchar(rownames(a)[2:nrow(a)])-1))
      }
      ifelse(selected==0,summary[1,1]<-NA,summary[Rolling.M+t,selected]<-a[2:nrow(a),])
      t=t+1
    }
    count<-1
    for(count in 1:ncol(summary)){
      summary[nrow(summary),count]<-nrow(summary)-1-sum(is.na(summary[1:(nrow(summary)-1),count]))
      count=count+1
    }
    summary[nrow(summary),]
  }
  
  ###result of each x
  each.x_F1R5P1<-as.data.frame(each.x(1,5,1),ncol=ncol(data)-1)
  each.x_F1R10P1<-as.data.frame(each.x(1,10,1),ncol=ncol(data)-1)
  each.x_F2R5P2<-as.data.frame(each.x(2,5,2),ncol=ncol(data)-1)
  each.x_F2R10P2<-as.data.frame(each.x(2,10,2),ncol=ncol(data)-1)
  each.x_F3R5P2<-as.data.frame(each.x(3,5,2),ncol=ncol(data)-1)
  each.x_F3R10P2<-as.data.frame(each.x(3,10,2),ncol=ncol(data)-1)
  
  summary_each.x<-cbind(each.x_F1R5P1,each.x_F1R10P1,each.x_F2R5P2,each.x_F2R10P2,each.x_F3R5P2,each.x_F3R10P2)
  write.table(summary_each.x,file="C:/Users/MSI/Desktop/chika/chin/summary_eachx.csv",sep = ",",row.names=FALSE)
##########----------------each X-----------------##########
  
##########------determinate sign of each X-------##########
  dete.sign<-function(f,Rolling.Y,Predict.M){
    if(f==1){
      F<-F1
    }else if(f==2){
      F<-F2
    }else{
      F<-F3
    }
    
    Rolling.M<-12*Rolling.Y
    
    ##function of lasso
    lasso=function(t,Rolling.M){
      
      #data split
      i<-1
      tmp<-matrix(nrow=Rolling.M,ncol=1)
      skip<-matrix(nrow=Rolling.M,ncol=1)
      rolling<-matrix(nrow=Rolling.M)
      for (i in 1:dim(F)[2]){
        if(sum(is.na(F[t:(t+Rolling.M),i]))==0){
          tmp<-as.numeric(F[t:(t+Rolling.M-1),i])
          tmp<-as.data.frame(tmp)
          rolling<-cbind(rolling,tmp)
          colnames(rolling)[ncol(rolling)]<-i
        }else{
          skip<-F[t:(t+Rolling.M-1),i]
        }
        i=i+1
      }
      rolling<-rolling[,-1]
      colnames(rolling)[ncol(rolling)]<-'Index'
      
      #model-building
      x=model.matrix(Index~.,rolling)[,-1]
      y=rolling[,ncol(rolling)]
      grid=10^seq(10,-2,length =100)
      
      #train & test
      train=c(1:(Rolling.M*.8))
      test =(-train)
      y.test=y[test]
      
      #Lasso
      lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
      cv.out=cv.glmnet(x[train,],y[train],alpha=1)
      bestlam=cv.out$lambda.min
      lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
      out=glmnet(x,y,alpha=1,lambda=grid)
      lasso.coef=predict(out,type="coefficients",s=bestlam)[1:ncol(rolling),]
      result<-as.matrix(lasso.coef[lasso.coef!=0])
    }
    
    ##summary
    summary<-matrix(nrow=nrow(data),ncol=ncol(data)-1)
    colnames(summary)<-colnames(1:ncol(data)-1)
    
    for(t in (f+1):(nrow(F)-Rolling.M)){
      a<-lasso(t,Rolling.M)
      selected<-if(dim(a)[1]==1){
        0
      }else{
        as.numeric(substr(rownames(a)[2:nrow(a)],2,nchar(rownames(a)[2:nrow(a)])-1))
      }
      ifelse(selected==0,summary[1,1]<-NA,summary[Rolling.M+t,selected]<-a[2:nrow(a),])
      t=t+1
    }
    summary
  }
  
  write.table(dete.sign(1,5,1),file="C:/Users/MSI/Desktop/chika/chin/dete.sign_F1R5P1.csv",sep = ",",row.names=FALSE)
  write.table(dete.sign(1,10,1),file="C:/Users/MSI/Desktop/chika/chin/dete.sign_F1R10P1.csv",sep = ",",row.names=FALSE)
  write.table(dete.sign(2,5,2),file="C:/Users/MSI/Desktop/chika/chin/dete.sign_F2R5P2.csv",sep = ",",row.names=FALSE)
  write.table(dete.sign(2,10,2),file="C:/Users/MSI/Desktop/chika/chin/dete.sign_F2R10P2.csv",sep = ",",row.names=FALSE)
  write.table(dete.sign(3,5,2),file="C:/Users/MSI/Desktop/chika/chin/dete.sign_F3R5P2.csv",sep = ",",row.names=FALSE)
  write.table(dete.sign(3,10,2),file="C:/Users/MSI/Desktop/chika/chin/dete.sign_F3R10P2.csv",sep = ",",row.names=FALSE)
  