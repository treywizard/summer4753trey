#' Random number histogram generator
#'
#' @param n Number of rows in data matrix
#' @param iter Number of columns in data matrix
#' @param a Lower bound
#' @param b Upper bound
#'
#' @return Histogram of data matrix
#' @importFrom grDevices rainbow
#' @importFrom graphics curve
#' @importFrom graphics hist
#' @importFrom stats dnorm runif
#' @export
#'
#' @examples myclt(n=50,iter=10000,a=5,b=10)

myclt=function(n,iter,a=0,b=5){
  x = NULL
  y=runif(n*iter,a,b)
  data=matrix(y,nrow = n,ncol = iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}

#' Myboot2
#'
#' @param iter Number of columns
#' @param x How many samples are taken
#' @param fun Function that we are implementing
#' @param alpha Range of error
#' @param cx Location of text compared to density
#' @param ... Extra implmentation
#'
#' @return Plot of data related to fun
#' @importFrom stats quantile
#' @importFrom graphics segments text
#' @export
#'
#' @examples myboot2(x = 20)
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow = n,ncol = iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow =length(x),ncol =1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}

#' Myboot1
#'
#' @param iter Number of columns
#' @param x Sample data
#' @param fun function we are analyzing
#' @param alpha confidence interval alpha
#' @param ... Extra implementation
#'
#' @return Plot of data related to fun
#' @importFrom stats qt sd
#' @export
#'
#' @examples myboot(x = c(10, 20, 30))
myboot<-function(iter=10000,x,fun="mean",alpha=0.05,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  t = qt(p = 0.05/2, df = length(x) - 1)
  cit = c(mean(xstat) + t * (sd(xstat)/sqrt(length(x))), mean(xstat) - t * (sd(xstat)/sqrt(length(x))))
  color = rainbow(length(unique(xstat)))
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line

  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=3)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=3)
  text(ci[1],0.2,paste("(",round(cit[1],2),sep=""),col="Blue",cex=3)
  text(ci[2],0.2,paste(round(cit[2],2),")",sep=""),col="Blue",cex=3)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=3)

  return(list(fun=fun,x=x, t=t, ci=ci, cit=cit))# Some output to use if necessary
}

#' Maximum parameter finder
#'
#' @param lfun function we are analyzing
#' @param x parameter that we provide values for
#' @param param single parameter that we are looking to find the max value for
#' @param ... extra
#'
#' @return Graph that shows where max value is on the graph
#' @importFrom graphics axis points
#' @export
#'
#' @examples mymaxlik(lfun = (ex = function(x, param){ mean(x)/param}), x=c(3,4,5,2,3,4,5,5,5,1), param = seq(0,1,length=1000))
mymaxlik=function(lfun,x,param,...){
  # how many param values are there?
  np=length(param)
  # outer -- notice the order, x then param
  # this produces a matrix -- try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand
  z=outer(x,param,lfun)
  # z is a matrix where each x,param is replaced with the function evaluated at those values
  y=apply(z,2,sum)

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  plot(param,y,col="Blue",type="l",lwd=2,...)
  # which gives the index for the value of y == max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i=max(which(y==max(y)))
  abline(v=param[i],lwd=2,col="Red")

  # plots a nice point where the max lik is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  #check slopes. If it is a max the slope shoud change sign from + to
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
