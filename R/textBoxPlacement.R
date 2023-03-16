#' axesRange
#' 
#' @import graphics
#' @import stats
#' @import grDevices
#' 
#' @description compute correct axes ranges for a set of overlain plots
#' 
#' @param xList list of numerical vectors containing axis points

#' @examples axesRange(yList1)

#' @returns xlim

#' @export
axesRange<-
  function(xList) {
    n<-length(xList)
    minX<-min(xList[[1]])
    maxX<-max(xList[[1]])
    if(n==1)
      return(c(minX,maxX))
    
    for(i in 2:n) {
      minX<-min(minX,xList[[i]])
      maxX<-max(maxX,xList[[i]])
    } # for(i in 1:n)
    return(c(minX,maxX))
  }

#' positionTextBoxDriverDriverDriver

#' @description Driver invoke positionTextBoxDriverDriver() using the original user input data
#'  If that fails to produce an uncluttered plot, then invokes stagger() to reshape the data
#'  before re-running positionTextBoxDriverDriver().

#' @param xList (optional) list whose components are numeric vectors of the x values for overlaid curves
#' @param yList list whose components are numeric vectors of the y values for overlaid curves
#' @param textList (optional) list of character string texts to insert in plot
#' @param xPos (optional) numeric vector x position for text box
#' @param nApprox (optional) integer number of intervals to interpolate between x data points
#' @param labs (optional) list of labels annotating a graph
#'  \itemize{
#'    \item main character string main title
#'    \item xlab character string x axis label
#'    \item ylab character string y axis label
#'  }
#' @param sortB Boolean if TRUE staggered curves reordered, with
#'   largest range curve on bottom of graph
#' @param verbose Boolean if TRUE print informative or diagnostic messages to console 
#'  
#' @details if xList or textList is missing, it is constructed from elements in yList
#' @details hint: to prevent conflicts, run the following line manually before running
#'    positionTextBoxDriverDriverDriver()  
#' @details rm(list=ls())
#' @details see https://stackoverflow.com/questions/27253376/different-results-from-rscript-and-r-cmd-batch
#'
#' @examples
#' # There is not enough space for text boxes in original graph.
#' # The package automatically adds offsets to the curves,
#' # keeping the curves in the original order,
#' # and successfully adds text boxes
#'
#' positionTextBoxDriverDriverDriver(xList=xList1,yList=yList1,
#'   textList=textList,xPos=c(1,1,1),nApprox=10,labs=labs,sortB=FALSE)
#'
#' # data set contains some negative values
#'
#' positionTextBoxDriverDriverDriver(xList=xList2,yList=yList2,
#'   textList=textList,xPos=c(1,1,1),nApprox=10,labs=labs,sortB=FALSE)
#'
#' # show the difference when we sort the order of the curves
#' # to position the curve with the largest range on the bottom
#'
#' positionTextBoxDriverDriverDriver(xList=xList2,yList=yList2,
#'   textList=textList,xPos=c(1,1,1),nApprox=10,labs=labs,sortB=TRUE,verbose=TRUE)
#'   
#' @returns returns no values, but has side effect of generating a graph.
#'
#' @export
positionTextBoxDriverDriverDriver<-
  function(xList,yList,textList,xPos,nApprox=10,labs,sortB,verbose=FALSE) {
    if(length(dev.list())==0)
      dev.new()
    
    #https://stackoverflow.com/questions/38758156/r-check-if-any-missing-arguments
    passed <- names(as.list(match.call())[-1])
    
    # if labs is missing, then set it to reasonable default
    if(!("labs" %in% passed)) {
      if(verbose)print("Constructing labs . . .")
      labs<-list()
      labs$main<-"Generic Title"
      labs$xlab<-"Generic X Axis"
      labs$ylab<-"Generic Y Axis"
    } # if(!("labs" %in% passed))
    
    # if xList is missing, then make it match number of elements in yList
    if(!("xList" %in% passed)) {
      if(verbose)print("Constructing missing xList from elements in yList . . .")
      xList<-list()
      for(i in 1:length(yList))
        xList[[i]]<-c(1:length(yList[[i]]))
    } # if(!("xList" %in% passed))
    
    # if xPos is not missing, make sure it has valid length
    if("xPos" %in% passed) {
      if(length(xPos) != length(xList)) {
        if(verbose)print(c("positionTextBoxDriverDriverDriver: Auto Correcting Invalid xPos"))
        xPos<-vector("numeric")
        for(i in 1:length(xList)) {
          mean<-as.integer(mean(xList[[i]]))
          xPos[i]<-xList[[i]][mean]
        } # for(i in 1:length(xList))
      } # if(length(xPos) != length(xList))
    } # if("xPos" %in% passed)
    
    # if xPos is missing, then construct it
    if(!("xPos" %in% passed)) {
      xPos<-vector("numeric")
      for(i in 1:length(xList)) {
        mean<-as.integer(mean(xList[[i]]))
        xPos[i]<-xList[[i]][mean]
      } # for(i in 1:length(xList))
    } # if(!("xPos" %in% passed))
    
    # make sure xPos is in valid range
    xPos<-xPosCheck(xPos,xList,verbose)
    
    # if textList is missing, then make it match names of elements in yList
    if(!("textList" %in% passed)) {
      if(verbose)print("Constructing missing textList from elements in yList . . .")
      textList<-vector()
      for(i in 1:length(yList)) {
        t<-names(yList[i])
        if(nchar(t)>0) # if list element has name, use the name
          textList[i]<-t
        else
          textList[i]<-as.character(i)
      }
    }
    
    # prepend extra blank line to each textList element to improve spacing
    for(i in 1:length(textList))
      textList[i]<-sprintf("\n%s",textList[i])
    
    xylim<-stagger(xList,yList,textList,sortB,verbose)
    
    if(verbose)print("FIRST TRYING ORIGINAL UNSTAGGERED OVERLAY PLOT:")
    result<-positionTextBoxDriverDriver(xList,yList,textList,xPos,nApprox,labs,
          stag=FALSE,offset=0,ystart=0,xylim$ylim,verbose)
    
    if(length(result)>0) {
      if(verbose)print("OOPS BAD RESULT:")
      if(verbose)print(result)
    } # if(length(result)>0)
    
    
    if(verbose)print(c("TEXTLIST::",length(textList),length(textList)<2))
    if(verbose)print(textList)
    
    if(length(textList)>1) {
      if(verbose)print("NOW TRYING STAGGERED OVERLAY PLOT WITH OFFSET CURVES:")
      l<-permuteCurves(xList,yList,textList,xylim$textBoxHeights,xylim$permInd)
      
      result<-positionTextBoxDriverDriver(l$xList,l$yList,l$tList,xPos,nApprox,labs,
            stag=TRUE,l$offset,l$ystart,xylim$ylim,verbose)
      if(length(result)==0)
        if(verbose)print("SUCCESS!!")
    } # if(length(labs)>1)
    
    return(result) 		
  }

#' xPosCheck

#' @description is the value of xPos within a valid range?
#' @param xPos integer specifying x position to try to place text box
#' @param xList list whose components are numeric vectors of the x values for overlaid curves
#' @param verbose Boolean if TRUE print informative or diagnostic messages to console
#' 
#' @examples
#' # replace incorrect xPos with reasonable value
#' 
#' xPosCheck(c(1,1,-5),xList2,verbose=TRUE)

#' @returns numeric vector valid values of xPos
#' @export
xPosCheck<-
  function(xPos,xList,verbose) {
    for(i in 1:length(xList)) {
      if(!(xPos[i] %in% xList[[i]])) {
        if(verbose)print(c("xPosCheck xPos ",i,xPos[i]," out of range"))
        mean<-as.integer(mean(xList[[i]]))
        xPos[i]<-xList[[i]][mean]
        if(verbose)print(c("replaced with ",xPos[i]))
      } # for(i in 1:length(xList))
    } # for(i in 1:length(xList))
    return(xPos)
  }

#' positionTextBoxDriverDriver

#' @description Driver to compute x and y coordinates for placement of text box
#'    based upon the y values of the function
#'    to avoid running into the graph line
#'    and avoid overlapping with other overlay curves 

#' @param xList list whose components are numeric vectors of the x values for overlaid curves
#' @param yList list whose components are numeric vectors of the y values for overlaid curves
#' @param textList list of character string texts to insert in plot
#' @param xPos numeric vector x position for text box 
#' @param nApprox integer number of intervals to interpolate between x data points
#' @param labs list of labels annotating a graph
#'  \itemize{
#'    \item main character string main title
#'    \item xlab character string x axis label
#'    \item ylab character string y axis label
#'  }
#' @param stag Boolean TRUE if this plot has staggering added to curves
#' @param offset numeric vector of offsets added to each curve
#' @param ystart numeric vector of starting positions
#' @param ylim numeric vector ylim parameter for plot()
#' @param verbose Boolean if TRUE print informative or diagnostic messages to console
#' 
#' @details if the length of the return value is not 0,
#'  then additional processing might be needed for the
#'  bad curves, such as adding an offset to their y values,
#'  plotting them in a different color or symbol,
#'  and keying them to a second y axis on the right of the graph  
#'
#' @examples # the text box for the second curve cannot fit,
#' # as it is sandwiched between two curves that are too close
#' 
#' plot(xList1[[1]],yList1[[1]],type="l")
#' positionTextBoxDriverDriver(xList=xList1,yList=yList1,
#'   textList=textList,xPos=c(1,1,1),nApprox=10,labs=labs,
#'   stag=FALSE,offset=0,ystart=0,ylim=axesRange(yList1),verbose=TRUE)
#'
#' @returns returns a vector of integers indicating curves whose 
#'   text box could not be drawn
#'
#' @export
positionTextBoxDriverDriver<-
  function(xList,yList,textList,xPos,nApprox=10,labs,stag=FALSE,offset=0,ystart,ylim,verbose) {  
    # create a blank plot with the correct axes ranges
    xlim<-axesRange(xList)

    oldpar <- par(no.readonly = TRUE) # code line i
    on.exit(par(oldpar))
    # increase right margin to make room for alternate y axis labels
    new_mar<-par("mar")
    new_mar[4]<-4.5
    par(mar=new_mar)
    
    if(stag) {
      main2<-"Staggered Curves with Added Offset"
      ylab<-sprintf("Offset %s",labs$ylab)
    }
    else {
      main2<-"Original Curves"
      ylab<-sprintf("Original %s",labs$ylab)
    }
    main<-sprintf("%s\n%s",labs$main,main2)
    
    #if(!stag)
    # https://stat.ethz.ch/pipermail/r-help/2008-July/168294.html
    plot(x=NA,xlim=xlim,ylim=ylim,main=main,xlab=labs$xlab,ylab=ylab,type="n",
         xaxs = "i", yaxs = "i")
    # plot(x=NA,xlim=xlim,ylim=ylim,main=main,xlab=labs$xlab,ylab=ylab,type="n")
    
    n<-length(xList)
    bad<-vector()
    
    for(i in 1:n) {
      result<-positionTextBoxDriver(textList[[i]],xList[[i]],yList[[i]],
                                    xPos[i],nApprox,as.list(xList[-i]),as.list(yList[-i]),
                                    stag,verbose=verbose)
      # result is c(yReal,yAdj)
      # where yReal is c(ymin,ymax,strheight(text),xPos1)
      # and yAdj is either 0 or 1
      if(length(result)==5) {
        lines(xList[[i]],yList[[i]])
        if(result[5]==0)
          y<-result[2]
        else
          y<-result[1]
        if(length(offset)>1) {
          txt<-sprintf("%s\noffset=%.2f",textList[[i]],offset[i])
          thresh<-min(xList[[i]])-offset[i]
          range<-max(xList[[i]])-min(xList[[i]])
          
          if(i>1 & stag) {
            abline(h=offset[i],col="blue")
            mtext(as.character(round(ystart[i],4)),side=4,at=offset[i])
            if(i==2) 
              mtext("Minimum Y Values for Offset Curves",side=4,line=2,cex=1.5)
          }
        } # if(length(offset)>1)
        else
          txt<-textList[[i]]
        # fine tune adj[2]
        if(result[5]==0)
          result[5]<--.1
        if(result[5]==1)
          result[5]<-1.1
        text(result[4],y,txt,adj=c(0,result[5]))
      } # if(length(result)==5)
      else
        bad<-c("bad",bad,i)
    } # for(i in 1:n)
    
    return(bad)
  }


#' positionTextBoxDriver

#' @description Driver to compute x and y coordinates for placement of text box
#'    based upon the values of the function
#'    to avoid running into the graph line
#'    and avoid overlapping with other overlay curves 

#' @param text character string text to insert in plot
#' @param x numeric vector of x values
#' @param y numeric vector of y values
#' @param xPos numeric x position for text box
#' @param nApprox integer number of intervals to interpolate between x data points
#' @param xList list whose components are numeric vectors of the x values for overlaid curves
#' @param yList list whose components are numeric vectors of the y values for overlaid curves
#' @param stag Boolean TRUE if this plot has staggering added to curves
#' @param offset numeric vector of offsets added to each curve
#' @param verbose Boolean if TRUE print informative or diagnostic messages to console 
#' 
#' @examples positionTextBoxDriver(text="TEXT ME",x=xList1[[1]],y=yList1[[1]],
#'   xPos=1,nApprox=10,xList=xList1[-1],yList=yList1[-1],stag=FALSE,offset=0,verbose=TRUE)
#'
#' @returns returns a numeric vector c(yReal,yAdj) where xTry is an x value at which the text box will fit
#'  without overlapping another overlay curve, or returns -1000000 for failure
#'
#' @export
positionTextBoxDriver<-
  function(text,x,y,xPos,nApprox=10,xList,yList,stag=FALSE,offset=0,verbose) {
    # adj = (0,1) text box is to the right and below the data point
    # adj = (0,0) text box is to the right and above the data point
    # adj = not set text box is to centered on the data point
    for(yAdj in c(0,1)) { # see if the text box will fit above the curve, if not try below the curve. yAdj = 0 is above, yAdj > 0 is below
      adj<-c(0,yAdj)
      
      # first try the x value that you want. if text box overlaps other curves, then try other x values
      for(xTry in c(xPos,x[1:length(x)-1])) {
        yReal<-positionTextBox(text,x,y,xTry,adj=adj,nApprox=10,reallyText=TRUE)
        # yReal is c(ymin,ymax,strheight(text),xPos1) in the interval between xTry and xTry + text width
        # the proposed y range of the text box is yReal[2] (height of curve) to yReal[2] +/- yReal[3] (height of text box above/below the curve)
        if(yAdj==0) { # text box above the curve
          yRealMin<-yReal[2]  			
          yRealMax<-yReal[2]+yReal[3]
        } # if(yAdj==0)
        else { # yAdj==1 => text box below the curve
          yRealMin<-yReal[1]-yReal[3]
          yRealMax<-yReal[1]
        } # else  				
        
        # now for each additional curve, check for overlap (of text box with points of additional curve) at the proposed x position
        OVERLAP<-FALSE
        if(length(xList)>0) {
          
          for(i in 1:length(xList)) {
            OVERLAP<-FALSE # assume no overlap until/unless overlap is detected
            # call positionTextBox() for the additional curve, to retrieve the return values
            # also useful to overlay plot during development, but not during real run
            
            yBadRange<-positionTextBox(text,xList[[i]],yList[[i]],xTry,adj=adj,nApprox=10,reallyText=FALSE)
            
            # does yBadRange intersect with the proposed y range of the text box?
            if(!((yBadRange[1]>yRealMax) | (yRealMin>yBadRange[2]))) {
              OVERLAP<-TRUE
              break
            } # if(!((yBadRange[1]>yRealMax) | (yRealMin>yBadRange[2])))          
          } # for(i in 1:length(xList))
          
        } # if(length(xList)>0)
        
        if(!OVERLAP) {
          # yReal is c(ymin,ymax,strheight(text),xPos1)
          return(c(yReal,yAdj))
        } # if(!OVERLAP)
        
      } # for(xTry in c(xPos,x))
    } # for(yAdj in c(0,3.8))
    if(verbose)print("FAILED TO FIND LOCATION FOR TEXT BOX . . .")
    if(verbose)print("WILL NOW TRY TO RE-PROCESS AFTER STAGGERING CURVES . . .")
    return(-1000000)
  }

#' positionTextBox

#' @description compute x and y coordinates for placement of text box
#'    based upon the values of the function
#'    to avoid running into the graph line 

#' @param text character string text to insert in plot
#' @param x numeric vector of x values
#' @param y numeric vector of y values
#' @param xPos numeric x position for text box
#' @param adj numeric vector param passed to text()
#' @param nApprox integer number of intervals to interpolate between x data points
#' @param reallyText Boolean if TRUE then execute text() command
#'
#' @examples x<-1:10
#' @examples y<-1:10
#' @examples plot(x,y,type="l")
#' @examples positionTextBox(text="TEXT ME",x=x,y=y,xPos=1,
#'   adj=c(0,0),nApprox=10,reallyText=TRUE)
#'
#' @returns returns a list c(ymin,ymax,strheight(text),xPos1)
#'
#' @export
positionTextBox<-
  function(text,x,y,xPos,adj,nApprox=10,reallyText) {
    n<-length(x)
    
    # https://stackoverflow.com/questions/26765182/how-to-get-the-width-and-height-of-a-character-in-usr-coordinates
    
    # These functions compute the width or height, respectively, of the given strings or
    # mathematical expressions s[i] on the current plotting device in
    # user coordinates (default), inches or as fraction of the figure width par("fin").
    
    # the ‘height’ of a string is determined only by the number of linefeeds
    # ("\n", aka “newline”s) it contains.
    
    text2<-sprintf("%s\noffset=00.000",text)
    xPos2<-min(xPos + strwidth(text2),x[n])
    
    # https://stat.ethz.ch/R-manual/R-devel/library/stats/html/approxfun.html
    
    # interpolate between xPos and xPos + width(text)

    xyApprox<-approx(x,y,seq(xPos,xPos2,len=nApprox))
    
    ymin<-min(xyApprox$y)
    ymax<-max(xyApprox$y)
    
    return(c(ymin,ymax,strheight(text),xPos))
  }

#' stagger

#' @description unclutter the overlay plot by staggering the graphs

#' @param xList list whose components are numeric vectors of the x values for overlaid curves
#' @param yList list whose components are numeric vectors of the y values for overlaid curves
#' @param tList list of character string texts to insert in plot
#' @param sortB Boolean if TRUE staggered curves reordered, with
#'   largest range curve on bottom of graph
#' @param verbose Boolean if TRUE print informative or diagnostic messages to console
#'  
#' @details In order to unclutter the overlay plot, we need to stagger the graphs
#' 	the offset for each graph will be the sum of the max values for all
#' 	of the preceding graphs.
#' 	So the stack of staggered graphs will have max y (ie, ymax) equal to
#' 	the sum of the max's.

#' @examples
#' # demonstrate effect of sorting the curves
#' 
#' plot.new()
#' 
#' stagger(xList2,yList2,textList,sortB=FALSE,verbose=TRUE)
#' 
#' stagger(xList2,yList2,textList,sortB=TRUE,verbose=TRUE)
#'
#' @returns returns a list whose components are:
#' \itemize{
#'   \item textBoxHeights return value of textBoxUserUnits()
#'   \item permInd return value of permInd()
#'   \item xlim numeric vector parameter for plot()
#'   \item ylim numeric vector parameter for plot()
#' }
#' 
#' @export
stagger<-
  function(xList,yList,tList,sortB=FALSE,verbose) {
    xylim<-list()
    yrange<-yrange(yList)
    xylim$textBoxHeights<-textBoxUserUnits(tList,yrange,verbose)
    xylim$permInd<-permInd(yrange)
    xylim$xlim<-xlim(xList)
    xylim$ylim<-ylim(yList,yrange,xylim$textBoxHeights,sortB,xylim$permInd)
    return(xylim)
  }

#' permuteCurves

#' @description reorder multiple curves so that the curve with the largest y range is on the bottom
#'  of the staggered graph

#' @param xList list whose components are numeric vectors of the x values for overlaid curves
#' @param yList list whose components are numeric vectors of the y values for overlaid curves
#' @param tList list of character string texts to insert in plot
#' @param textBoxHeights return value of textBoxUserUnits()
#' @param permInd return value of permInd()

#' @examples
#'
#' units<-textBoxUserUnits(textList,yrange(yList1),verbose=TRUE)
#' permuteCurves(xList1,yList1,textList,units,permInd(yrange(yList1)))
#' 
#'
#' @returns returns a list whose (re-ordered) components are:
#' \itemize{
#'   \item xList a list of numeric vector for x values
#'   \item yList a list of numeric vector for y values - re-ordered and offset-adjusted
#'   \item tList a list of character strings for text boxes to label the curves
#'   \item offset a numeric vector offset to add to each staggered curve
#'   \item ystart a numeric vector of starting positions
#' }
#' @export
permuteCurves<-
  function(xList,yList,tList,textBoxHeights,permInd) {
    l<-list()
    
    ymax<-vector("numeric")
    ymin<-vector("numeric")
    yrange<-vector("numeric")
    tmax<-vector("numeric")
    
    xList2<-list()
    yList2<-list()
    tList2<-list()
    tmax2<-vector("numeric")
    ystart<-vector("numeric")
    offset<-vector("numeric")
    
    n<-length(xList)
    
    # permute based on sorted order of curves
    for(j in 1:n) {
      i<-permInd[j]
      xList2[[j]]<-xList[[i]]
      yList2[[j]]<-yList[[i]]
      tList2[[j]]<-tList[[i]]
      tmax2[j]<-textBoxHeights[i]
      ystart[j]<-min(yList[[i]])
    }
    
    # compute the offsets for the permuted curves
    offset[1]<-0
    if(n>1)
      for(i in 2:n) {
        mn<-min(yList2[[i]])
        mx<-max(yList2[[i-1]])
        offset[i]<-(mx+tmax2[i-1])
        yList2[[i]]<-yList2[[i]]+offset[i]-mn
      }
    
    l$xList<-xList2
    l$yList<-yList2
    l$tList<-tList2
    l$offset<-offset
    l$ystart<-ystart
    
    return(l)
  }

#' xlim

#' @description compute the numeric vector xlim for a set of curves

#' @param xList list whose components are numeric vectors of the x values for overlaid curves

#' @examples xlim(xList1)

#' @returns numeric vector xlim
#' @export
xlim<-
  function(xList) {
    xmin<-vector("numeric")
    xmax<-vector("numeric")
    
    for(i in 1:length(xList)) {
      xmin[i]<-min(xList[[i]])
      xmax[i]<-max(xList[[i]])
    }
    return(c(min(xmin),max(xmax)))
  }  

#' yrange

#' @description compute the staggered y values for the overlay plot

#' @param yList list whose components are numeric vectors of the y values for overlaid curves

#' @examples
#' yrange(yList1)

#' @returns numeric vector yrange (max-min) for vector of y values
#' @export
yrange<-
  function(yList) {
    ymax<-vector("numeric")
    ymin<-vector("numeric")
    yrange<-vector("numeric")		
    for(i in 1:length(yList)) {
      ymax[i]<-max(yList[[i]])
      ymin[i]<-min(yList[[i]])
      yrange[i]<-ymax[i]-ymin[i]
    }
    
    return(yrange)
  }

#' textBoxUserUnits

#' @description compute the heights of the text boxes in user units

#' @param tList a list of character strings for text boxes to label the curves
#' @param yrange numeric vector (max-min) for vector of y values
#' @param verbose Boolean if TRUE print informative or diagnostic messages to console

#' @examples 
#' textBoxUserUnits(textList,yrange(yList1),verbose=TRUE)

#' @returns numeric vector of the heights of the text boxes in user units
#' @export
textBoxUserUnits<-
  function(tList,yrange,verbose) {
    dev_inch<-dev.size("in")[2]
    marg_inch<-par("mai") # c(bottom, left, top, right)
    window_inch<-dev_inch - (marg_inch[1] + marg_inch[3])
    t_inch<-0 # total height (in inches) of all text boxes
    n<-length(tList)
    for(i in 1:n) {
      fake<-sprintf("%s\nfake",tList[[i]]) # "\noffset= " line is added to text in tList, so we need to add one more line for strheight
      t_inch<-t_inch+strheight(fake,units="in")      
    }

    plot_inch<-window_inch - t_inch
    scale<-(sum(yrange))/plot_inch # user units per inch

    if(verbose) {
      print("",quote=FALSE)
      print(c("DEVICE INCHES",dev_inch),quote=FALSE)
      print(c("MARG_INCH",marg_inch),quote=FALSE)
      print(c("WINDOW_INCH",window_inch),quote=FALSE)
      print(c("T_INCH",t_inch),quote=FALSE)
      print(c("sum(yrange)",sum(yrange)),quote=FALSE)
      print(c("PLOT_INCH",plot_inch),quote=FALSE)
      print(c("SCALE = user units per inch",scale),quote=FALSE)
      print("",quote=FALSE)
    }
    
    # convert text box heights from inches to user units
    t_user<-vector("numeric")
    for(i in 1:n)
    {
      fake<-sprintf("%s\nfake",tList[[i]]) # "\noffset= " line is added to text in tList, so we need to add one more line for strheight
      t_user[i]<-strheight(fake,units="in")*scale
    }
    return(t_user)
  }

#' ylim

#' @description compute the numeric vector ylim

#' @param yList list whose components are numeric vectors of the y values for overlaid curves
#' @param yrange numeric vector (max-min) for vector of y values 
#' @param textBoxHeights return value of textBoxUserUnits()
#' @param sortB Boolean if TRUE staggered curves are reordered, with
#'   largest range curve on bottom of graph
#' @param permInd return value of permInd()
#'  
#' @examples
#' # demonstrate effect of sorting the curves
#' 
#' plot.new()
#' ylim(yList1,yrange(yList1),textBoxUserUnits(textList,yrange(yList1),verbose=TRUE),
#'   FALSE,permInd(yrange(yList1)))
#'
#' @returns returns a numeric vector ylim
#' @export
ylim<-
  function(yList,yrange,textBoxHeights,sortB,permInd) {
    # first compute ylim[1]
    # bottom-most curve (and therefore ylim[1]) depends on sortB
    ylim<-vector("numeric",2)
    if(sortB)    		
      ylim[1]<-min(yList[[permInd[1]]])
    else
      ylim[1]<-min(yList[[1]])
    
    # second compute ylim[2]
    ylim[2]<-ylim[1]+sum(yrange)+sum(textBoxHeights)
    
    return(ylim)
  }

#' permInd

#' @description compute the indices of sorting yrange in decreasing order

#' @param yrange numeric vector (max-min) for vector of y values
#' 
#' @examples
#' permInd(yrange(yList1))
#' 
#' @return returns the integer vector indices of sorting yrange in decreasing order
#' @export 
permInd<-
  function(yrange) {
    return(sort.list(yrange,decreasing=TRUE))
  }
