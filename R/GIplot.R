
#' @title Gaussian Interval Plot (GIplot)
#' @description The Gaussian Interval Plot (GIplot) is a pictorial representation of the mean
#'              and the standard deviation of a quantitative variable. It also flags
#'              potential outliers (together with their frequencies) that are c standard deviations away from the mean.
#' @param x a numeric vector or a single list or a data frame
#' @param ... more numeric vectors for the GIplot
#' @param formula a formula, such as x ~ grp, where x is a numeric vector of data values to be split into groups according to the grouping variable grp (usually a factor). Note that ~ g1 + g2 is equivalent to g1:g2.
#' @param dataset a data.frame from which the variables in formula should be taken.
#' @param horizontal Logical.TRUE (Default) for horizontal GIPlot and FALSE for vertical.
#' @param names names of the sub-groups for which separate GIPlots are drawn on the same scale.
#' @param add Logical. TRUE adds a new GIplot to the existing plot. FALSE (Default) will create a new plot.
#' @param at If add = TRUE, the position at which the new GIplot should be placed.
#' @param valueOfc the multiplier of sd to determine extreme bounds beyond which values are flagged as outliers. To flag alpha proportion of data in each tail use c = qnorm(1-alpha). When alpha = 0.01, c = qnorm(0.99) = 2.32
#' @param axisLabel label for the axis
#' @param main title of the GIplot.
#' @param spsize Logical. TRUE (Default) adds a sample size to the GIplot.
#' @return displays the GIplot
#' @importFrom graphics arrows axis segments text mtext title
#'
#' @importFrom stats model.frame na.omit quantile
#' @export
#' @examples
#'   #For vectors
#'   x<- rnorm(90,30,10)
#'   GIplot(x)
#'
#'   #For Formula Class
#'   groupA <- rep(c(1,2,3),30)
#'   GIplot(x~groupA)
#'

GIplot <- function(x,...){
  #if((typeof(x)=="list")||(typeof(x)=="double")||(typeof(x)=="data.frame")||(typeof(x)=="integer")){
  #  class(x)<- "default"
  #  UseMethod("GIplot",x)
  #}else if (typeof(x)=="language"){
  #  class(x) <- "formula"
  #  UseMethod("GIplot",x)
  #}
  UseMethod("GIplot")
}


#' @export
#' @rdname GIplot
GIplot.default <- function(x,...,horizontal= TRUE,names=c(),add=FALSE,at=0,valueOfc=2.33,axisLabel="",main=paste("GI Plot of ",axisLabel),spsize=T) {
  h<- 0 #height at which the GI plot is shown.

  #creating a list of the datasets
  arguments <- list(...)
  if ((typeof(x)=="double")||(typeof(x)=="integer")){
    arguments <- c(list(x),arguments)
  }
  if (typeof(x)=="list"){
    arguments <- c(x,arguments)
  }
  if (typeof(x)=="data.frame"){
    li <- list()
    nOfCol = ncol(x)
    for(h in 1:nOfCol){
      li <- c(li,x[h])
    }
    arguments <- c(li,arguments)
  }

  #finding the maximum and minimum value in the whole list of datasets
  n <- length(arguments)
  xlimitmin <- 0
  xlimitmax <- 0
  for(u in 1:n){
    g <- unlist(arguments[u])
    if (u==1){
      xlimitmin <- min(g)
      xlimitmax <- max(g)
    }
    if (u>1){
      if(min(g)<xlimitmin){
        xlimitmin <- min(g)
      }
      if(max(g)>xlimitmax){
        xlimitmax <- max(g)
      }
    }
  }

  #creating GIplot for every data-set in the list.
  for(j in 1:n){
    h<- h + 1
    if(add==TRUE){
      h = at
    }
    x<- unlist(arguments[j])
    c <- valueOfc #multiple of sd() for the outlier selection
    x<-na.omit(x) #omiting any na values
    y<- rep(h,length(x))
    xlimit <- c(xlimitmin-2,(xlimitmax*5-xlimitmin)/4) #for plot function
    ylimit <- c(0,(n*1+1))
    ht <- 0
    sideOfAxis <- 1
    #x1 <- as.integer(min(x))
    #x2 <- as.integer(max(x))
    xOne <- quantile(x,0.5)
    xTwo <- quantile(x,0.25)
    xThree <- quantile(x,0.75)
    xFour <- mean(x)-c*sd(x)
    xFive <- mean(x)+c*sd(x)

    sortedx <- sort(x)
    #xfour is the value which is just above the mean(x)-c*sd(x)
    for(k in sortedx){
      if (k >= xFour){
        xFour <- k
        break
      }
    }
    #xfive is the value which is just below the mean(x)+c*sd(x)
    for(l in length(sortedx):1){
      if (sortedx[l] <= xFive){
        xFive <- sortedx[l]
        break
      }
    }


    yOne <- h
    yTwo <- h
    yThree <- h
    yFour <- h
    yFive <- h
    angle <- 0
    mean <- mean(x)
    sd <- sd(x)

    #For Vertical GIplot; interchanging the values.
    if(horizontal==FALSE){
      #interchanging xlimit and ylimit
      ylimit <- c(xlimitmin-2,(xlimitmax*5-xlimitmin)/4)
      xlimit <- c(0,(n*1+1))

      #interchanging x and y
      tempy <- y
      y <- x
      x <- tempy

      #all the other values adjusted for vertical GIplot
      sideOfAxis <- 2
      angle <- 90
      yOne <- xOne
      yTwo <- xTwo
      yThree <- xThree
      yFour <- xFour
      yFive <- xFive
      xOne <- h
      xTwo <- h
      xThree <- h
      xFour <- h
      xFive <- h
    }

    #plot function and title of the plot used only once if add ==FALSE
    if((j==1)&(add==FALSE)){
      plot(x,y,frame.plot = F,axes =FALSE,xlab="",ylab="",type="n"
           ,xlim = xlimit ,ylim = ylimit,las=1)
      if(horizontal==T){
        title(main = main, line = 2)
      } else{
        title(main = main, line = 3)
      }
    }

    #horizontal or vertical axis, axislabel and names of variables if there is more than one GIplot.
    if (add==FALSE){
      if (j==n){
        axis(sideOfAxis,pos=ht,xpd=TRUE,las=1)
        mtext(text=axisLabel,side = sideOfAxis,line=2)
        if (sideOfAxis==1){
          if(n>1){
            axis(2,at=c(seq(1,h,1)),labels = names,las=1,xpd=TRUE,padj = 0.5,hadj=1)
          }
        } else {
          if(n>1){
            axis(side = 1,at=c(seq(1,h,1)),labels = names,xpd=TRUE,padj = 1,las =1,hadj=0.5)
          }
        }
      }
    }

    #braces and notches for the quartiles, median, and the outliers' boundary
    text(xOne,yOne,labels = "|",srt=angle,col = "black",xpd=add)
    text(xTwo,yTwo,labels = "[",srt=angle,col = "black",xpd=add)
    text(xThree,yThree,labels = "]",srt=angle,col = "black",xpd=add)
    text(xFour,yFour,labels = "(",srt=angle,col = "black",xpd=add)
    text(xFive,yFive,labels = ")",srt=angle,col = "black",xpd=add)

    #dashed straight line, arrow from the mean, and sample size.
    segments(xFour,yFour,xFive,yFive,lty = 2,xpd=add)
    if (horizontal==TRUE){
      arrows(mean(x),h,mean(x)+sd(x),h,length = 0.08,lty = 1,lwd = 2,xpd=add)
      if(spsize==T){
        text(max(x)+0.1*(max(x)-min(x)),yFive,labels = paste("n =",length(x)),col = "brown",xpd=T,adj=0)
      }
    } else {
      arrows(h,mean(y),h,mean(y)+sd(y),length = 0.08,lty = 1,lwd = 2,xpd=add)
      if(spsize==T){
        text(xFive,max(y)+0.2*(max(y)-min(y)),labels = paste("n =",length(x)),col = "brown",xpd=T,adj=c(0.5,0))
      }
    }


    #for outliers


    #displays the number of outlier diagram in the plot.
    display<-function(nOut,valOfx,valOfy){
      h <- valOfy
      if(nOut == 1){
        if (horizontal==TRUE){
          text(valOfx,h,labels=expression("I"),col="black",xpd=add)
        } else {
          text(h,valOfx,labels=expression("I"),srt=angle,col="black",cex = 1.2,xpd=add)
        }
      } else if(nOut == 2){
        if(horizontal==TRUE){
          text(valOfx,h,labels = expression("v"),col="black",xpd=add)
        } else {
          text(h,valOfx,labels = expression(">"),col="black",cex = 1.2,xpd=add)
        }
      } else if(nOut == 3){
        if (horizontal==TRUE){
          text(valOfx,h,labels = expression("v"),col="black",xpd=add,adj=c(0.5,1))
          text(valOfx,h,labels = expression("I"), col="black",xpd=add)
        } else{
          text(h,valOfx,labels = expression(">"),col="black",cex = 1.2,xpd=add,adj=c(0,0.5))
          text(h,valOfx,labels = expression("I"),srt=90,col="black",cex=1.2,xpd=add )
        }

      } else if(nOut == 4){
        if (horizontal==TRUE){
          text(valOfx,h,labels = expression("v"),col="black",xpd=add,adj=c(0.5,0))
          text(valOfx,h,labels = expression("I"), col="black",xpd=add)
          text(valOfx,h,labels = expression("v"),col="black",xpd=add,adj=c(0.5,1))
        } else {
          text(h,valOfx,labels=expression(">"),col="black",cex = 1.2,xpd=add,adj=c(0,0.5))
          text(h,valOfx,labels=expression(">"),col="black",cex = 1.2,xpd=add,adj=c(1,0.5))
          text(h,valOfx,labels = expression("I"),srt=90,col="black",cex=1.2,xpd=add )
        }
      } else if(nOut == 5){
        if (horizontal==TRUE){
          text(valOfx,h,labels = expression("v"),col="black",xpd=add,adj=c(0.5,1))
          text(valOfx,h,labels = expression("I"), col="black",xpd=add)
          text(valOfx,h,labels = expression("v"),col="black",xpd=add,adj=c(0.5,0))
          text(valOfx,h,labels = expression("I"),col="black",xpd=add,adj = c(0.5,0))
        } else {
          text(h,valOfx,labels=expression(">"),col="black",cex = 1.2,xpd=add)
          text(h,valOfx,labels = expression("|"),srt=90,col="black",cex=1.2,xpd=add)
          text(h+(0.01*(n+1)),valOfx,labels = expression(">"),col="black",cex = 1.2,xpd=add)
        }
      } else if(nOut > 5){
        display(5,valOfx,h)
        if(horizontal==TRUE){
          display(nOut-5,valOfx, h + (0.4*(1)))
        }else if (horizontal==FALSE){
          display(nOut-5,valOfx, h - (0.25*(1)))
        }

      }
    }

    #finds the outliers and count their numbers.
    i <- 1
    while(i<=length(sortedx)){
      count <- 0
      if(sortedx[i] < mean-c*sd || sortedx[i] > mean+ c*sd){
        count <- 1
        if(i < length(sortedx)){
          while(sortedx[i+1]==sortedx[i]){
            count <- count +1
            i<- i + 1
            if(i == length(sortedx)){
              break
            }
          }
        }
      }
      display(count,sortedx[i],h)
      i<- i+1
    }
  }
}


#' @export
#' @rdname GIplot
GIplot.formula <- function(formula, dataset = NULL,horizontal= TRUE,names=c(),add=FALSE,at=0,valueOfc=2.33,axisLabel="",main=paste("GIPlot of ",axisLabel),spsize=T,...){
  df <- model.frame(formula,data = dataset)
  numberOfColumns<- ncol(df)
  vec <- c()
  for(d in 2:numberOfColumns){
    vec <- c(vec,df[d])
  }
  e<- split(df[1],vec)
  if(length(names)==0){
    names = names(e)
  }
  GIplot.default(e,names = names,horizontal=horizontal,add=add,at=at,
                 valueOfc=valueOfc,axisLabel=axisLabel,main=paste("GIPlot of ",axisLabel),spsize = spsize)

}



