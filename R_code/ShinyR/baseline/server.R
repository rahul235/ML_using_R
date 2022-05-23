library(shiny)
library(fitdistrplus)     #distribution fitting
library(MASS)
library(stats)
library(xts)              #to convert dataframe to xts format for dygraphs use
library(ggplot2)          #box plot, histogram, scatter plot
library(gdata)             #to read .xlsx file
library(lubridate)        #to check is.date
library(psych)            #describeBy summary
library(reshape2)         #to melt the corelation matrix


options(shiny.maxRequestSize = 30 * 1024 ^ 2) #shiny allows 5 mb upload only. Inc to 30 MB
options(java.parameters = "-Xmx1000m")  #allocate more sapce to read the file. Jave heap error.

shinyServer(function(input, output, session) {
  dataUpload = reactive({
    infile = input$file
    if (is.null(infile))
      return(NULL)
    infile_read = read.csv(infile$datapath, 1)
    numericcol <- c()
    charactercol <- c()
    #dtcol <- c()
    #to update the Column names in ui.R based on excel input
    for (clnames in colnames(infile_read)) {
      if (is.numeric(infile_read[,clnames])) {
        numericcol <- c(numericcol, clnames)
      }
      else {
        charactercol <- c(charactercol, clnames)
      }
      
    }
    numericcol <- c(numericcol, "None")
    charactercol <- c(charactercol, "None")
    
    updateSelectInput(session , "Xvar", choices = numericcol)
    updateSelectInput(session , "Yvar", choices = numericcol)
    updateSelectInput(session , "Zvar", choices = charactercol)
    #updateSelectInput(session , "Xvar", choices = numericcol) "hist_var"
    #updateSelectInput(session , "Zvar", choices = charactercol) "hist_Gvar"
    #updateSelectInput(session , "Xvar", choices = numericcol) "box_Xvar"
    #updateSelectInput(session , "Zvar", choices = charactercol) "box_Yvar"
    #updateSelectInput(session , "Xvar", choices = numericcol) "scatter_Xvar"
    #updateSelectInput(session , "Yvar", choices = numericcol) "scatter_Yvar"
    #updateSelectInput(session , "Zvar", choices = charactercol) "scatter_Zvar"
    return(infile_read)
  })
  
  
  #Show the first 100 selected data in Data tabbn,d
  output$tabl <-
    renderDataTable(dataUpload(), options = list(pageLength =
                                                   10))
  
  #printing outliers in the Outliers tab
  output$otlrsummary <- renderPrint({
    infile = input$file
    if (is.null(infile))
      return(NULL)
    dsBase.iqr <- dataUpload()
    # Create a variable/vector/collection of the column names you want to remove outliers on.
    vars <- c(colnames(dsBase.iqr))
    len <- length(vars)
    # Create a variable to store the row id's of outlier
    otlier = c()
    
    # Loop through the list of columns you specified
    for (i in vars) {
      # Get the Min/Max values
      if (is.numeric(dsBase.iqr[,i])) {
        max <-
          quantile(dsBase.iqr[,i],0.75, na.rm = TRUE) + (IQR(dsBase.iqr[,i], na.rm =
                                                               TRUE) * 1.5)
        min <-
          quantile(dsBase.iqr[,i],0.25, na.rm = TRUE) - (IQR(dsBase.iqr[,i], na.rm =
                                                               TRUE) * 1.5)
        # Get the id's using which
        idx <- which(dsBase.iqr[,i] < min | dsBase.iqr[,i] > max)
        otlier <- list(idx)
        names(otlier) = i
        print(paste(i, "outlier at row no: ", otlier, sep = " "))
      }
    }
  })
  
  
  #print the summary in summary Tab
  output$summary <- renderPrint({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    
    #function to compute stats not being used
    #myFun <- function(x) {
    #  c(min = min(x), max = max(x),
    #    mean = mean(x), median = media(x),
    #    std = sd(x))
    #}
    
    if (input$Zvar == "None") {
      summary(dataUpload())
    }
    else {
      for (c in colnames(dataUpload())) {
        if (is.numeric(dataUpload()[,c])) {
          #s <- tapply(dataUpload()[,c], dataUpload()[,input$Zvar], myFun)
          s <-
            describeBy(dataUpload()[,c], dataUpload()[,input$Zvar])
          cat("\n\n")
          cat(paste("Summary of ", c, " by ", input$Zvar,sep = ""))
          cat("\n\n")
          print(s)
          cat("\n\n")
        }
      }
    }
    
  })
  
  
  #Plot the histogram in histrogram Tab
  output$histplot <- renderPlot({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    if (input$Xvar == "None")
      return(NULL)
    
    # generate an histogram and plot for the column selected
    if (input$Zvar == "None") {
      x <- as.data.frame(dataUpload()[, c(input$Xvar)])
      x = x[!is.na(x)]
      print(x)
      #for interactive histrogram
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      #print(bins)
      
      ggplot(dataUpload(), aes_string(x = input$Xvar)) +
        geom_histogram(breaks = bins,aes(fill = ..count..)) +
        labs(title = paste("Histogram plot for ", input$Xvar, sep = "-"))
    }
    else {
      x <- as.data.frame(dataUpload()[, c(input$Xvar,input$Zvar)])
      x <- x[complete.cases(x),]
      #for interactive histrogram
      bins <- seq(min(x[,1]), max(x[,1]), length.out = input$bins + 1)
      
      ggplot(x, aes_string(x = colnames(x)[1], fill = colnames(x)[2])) +
        geom_histogram(breaks = bins) +
        labs(title = paste("Histogram plot group by ", input$Zvar, sep = " "))
    }
  }, width = 400, height = 320)
  
  
  #density plot to determine shape
  output$denplot <- renderPlot({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    
    #same hist variable used for density plot
    if (input$Xvar == "None")
      return(NULL)
    # generate an density plot for the column selected
    if (input$Zvar == "None") {
      ggplot(dataUpload(), aes_string(x = input$Xvar)) +
        geom_density(aplha = 0.5, color = "blue", fill = "blue") +
        labs(title = paste("Density plot for ", input$Xvar, sep = "-"))
    }
    else {
      ggplot(dataUpload(), aes_string(x = input$Xvar, fill = input$Zvar)) +
        geom_density() +
        labs(title = paste("Density plot group by ", input$Zvar, sep = ""))
    }
    #  plot(density(dataUpload()[ , input$Xvar]),
    #       main = paste("Shape for ", colnames(dataUpload()[input$Xvar]), sep = " "),
    #       col="Red",
    #       xlab = colnames(dataUpload()[input$Xvar])
    #polygon(dataUpload()[ , input$Xvar], col="red", border="blue")
    #  )
  }, width = 400, height = 320)
  
  
  #logistic distribution fitting
  output$logisticplot <- renderPlot({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    
    #logistic distribution stats (KS goodness of fit)
    x <- dataUpload()[,input$Xvar]
    x = x[!is.na(x)]                #removing missing values
    fd_lg <- fitdistr(x,densfun = "logistic")
    est_loct = fd_lg$estimate[[1]]
    est_scal = fd_lg$estimate[[2]]
    ks <- ks.test(x, "plogis")
    results1 <- c(
      paste("loc",round(est_loct, digits = 2),""),
      paste("scale",round(est_scal,digits = 2),""),
      paste("KS",round(ks$statistic,digits = 3),""),
      paste("p", round(ks$p.value,digits = 3),"")
    )
    
    # QQ plot starts
    n <- length(x)
    r <-
      rank(x)              # a) ranks using fractional ranking (averaging ties)
    #r <- order(order(x))     # ordinal ranking which will rank even ties seperately
    if (n > 10) {
      p <- (r - 1 / 2) / n      # assign to ranks using Blom's method
    }
    if (n <= 10) {
      p <- (r - 3 / 8) / (n + 1 / 4)
    }
    y = qlogis(p, est_loct, est_scal)
    x <- sort(x)
    y <- sort(y)
    df <- data.frame(x, y)
    ggplot(df) +
      geom_point(aes(x,y), shape = 1, size = 3, color = "red") +
      geom_abline(
        intercept = 0, slope = 1, alpha = 0.5, color = "Blue"
      ) +
      xlab("Observed value") +
      ylab("Expected logistic value") +
      ggtitle(paste("Logistic Plot for ", input$Xvar, sep = ""))
    
  }, width = 370, height = 320)
  
  
  #normal distribution fitting
  output$normalplot <- renderPlot({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    
    #normal distribution stats (KS goodness of fit)
    x <- dataUpload()[,input$Xvar]
    x = x[!is.na(x)]                #removing missing values
    fd_n <- fitdistr(x,densfun = "normal")
    est_mean = fd_n$estimate[[1]]
    est_stdv = fd_n$estimate[[2]]
    ks <- ks.test(x, "pnorm")
    results2 <- c(
      paste("mean",round(est_mean, digits = 2),""),
      paste("stdv",round(est_stdv,digits = 2),""),
      paste("KS",round(ks$statistic,digits = 3),""),
      paste("p", round(ks$p.value,digits = 3),"")
    )
    
    # QQ plot starts
    n <- length(x)
    r <-
      rank(x)              # a) ranks using fractional ranking (averaging ties)
    #r <- order(order(x))     # ordinal ranking which will rank even ties seperately
    if (n > 10) {
      p <- (r - 1 / 2) / n      # assign to ranks using Blom's method
    }
    if (n <= 10) {
      p <- (r - 3 / 8) / (n + 1 / 4)
    }
    y = qnorm(p, est_mean, est_stdv)
    x <- sort(x)
    y <- sort(y)
    df <- data.frame(x, y)
    ggplot(df) +
      geom_point(aes(x,y), shape = 1, size = 3, color = "red") +
      geom_abline(
        intercept = 0, slope = 1, alpha = 0.5, color = "Blue"
      ) +
      xlab("Observed value") +
      ylab("Expected normal value") +
      ggtitle(paste("Normal Plot for ", input$Xvar, sep = ""))
  }, width = 370, height = 320)
  
  
  #exponential distribution fitting
  output$expplot <- renderPlot({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    
    #exponential distribution stats (KS goodness of fit)
    x <- dataUpload()[,input$Xvar]
    x = x[!is.na(x)]                #removing missing values
    fd_e <- fitdistr(x,densfun = "exponential")
    est_rate = fd_e$estimate[[1]]
    ks <- ks.test(x, "pexp")
    results3 <- c(paste("rate",round(est_rate, digits = 2),""),
                  paste("KS",round(ks$statistic,digits = 3),""),
                  paste("p", round(ks$p.value,digits = 3),""))
    
    # QQ plot starts
    n <- length(x)
    r <-
      rank(x)              # a) ranks using fractional ranking (averaging ties)
    #r <- order(order(x))     # ordinal ranking which will rank even ties seperately
    if (n > 10) {
      p <- (r - 1 / 2) / n      # assign to ranks using Blom's method
    }
    if (n <= 10) {
      p <- (r - 3 / 8) / (n + 1 / 4)
    }
    y = qexp(p,est_rate)
    x <- sort(x)
    y <- sort(y)
    df <- data.frame(x, y)
    ggplot(df) +
      geom_point(aes(x,y), shape = 1, size = 3, color = "red") +
      geom_abline(
        intercept = 0, slope = 1, alpha = 0.5, color = "Blue"
      ) +
      xlab("Observed value") +
      ylab("Expected exponential value") +
      ggtitle(paste("Exponential Plot for ", input$Xvar, sep = ""))
  }, width = 370, height = 320)
  
  
  #weibull distribution fitting
  output$weibullplot <- renderPlot({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    
    #weibull distribution stats (KS goodness of fit)
    x <- dataUpload()[,input$Xvar]
    x = x[!is.na(x)]                #removing missing values
    fd_w <- fitdistr(x,densfun = "weibull")
    est_shap = fd_w$estimate[[1]]
    est_scal = fd_w$estimate[[2]]
    
    ks <- ks.test(x, "pweibull", shape = est_shap, scale = est_scal)
    results4 <- c(
      paste("shape",round(est_shap, digits = 2),""),
      paste("scale",round(est_scal,digits = 2),""),
      paste("KS",round(ks$statistic,digits = 3),""),
      paste("p", round(ks$p.value,digits = 3),"")
    )
    
    # QQ plot starts
    n <- length(x)
    r <-
      rank(x)              # a) ranks using fractional ranking (averaging ties)
    #r <- order(order(x))     # ordinal ranking which will rank even ties seperately
    if (n > 10) {
      p <- (r - 1 / 2) / n      # assign to ranks using Blom's method
    }
    if (n <= 10) {
      p <- (r - 3 / 8) / (n + 1 / 4)
    }
    y = qweibull(p, est_shap, est_scal)
    x <- sort(x)
    y <- sort(y)
    df <- data.frame(x, y)
    ggplot(df) +
      geom_point(aes(x,y), shape = 1, size = 3, color = "red") +
      geom_abline(
        intercept = 0, slope = 1, alpha = 0.5, color = "Blue"
      ) +
      xlab("Observed value") +
      ylab("Expected weibull value") +
      ggtitle(paste("Weibull Plot for ", input$Xvar, sep = ""))
    
  }, width = 370, height = 320)
  
  
  #lognormal distribution fitting
  output$lognormplot <- renderPlot({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    
    #lognormal distribution stats (KS goodness of fit)
    x <- dataUpload()[,input$Xvar]
    x = x[!is.na(x)]                #removing missing values
    fd_ln <- fitdistr(x,densfun = "lognormal")
    est_mean = fd_ln$estimate[[1]]
    est_stdv = fd_ln$estimate[[2]]
    ks <- ks.test(x, "plnorm")
    results5 <- c(
      paste("lmean",round(est_mean, digits = 2),""),
      paste("lstdv",round(est_stdv,digits = 2),""),
      paste("KS",round(ks$statistic,digits = 3),""),
      paste("p", round(ks$p.value,digits = 3),"")
    )
    #print(results5)
    # QQ plot starts
    n <- length(x)
    r <-
      rank(x)              # a) ranks using fractional ranking (averaging ties)
    #r <- order(order(x))     # ordinal ranking which will rank even ties seperately
    if (n > 10) {
      p <- (r - 1 / 2) / n      # assign to ranks using Blom's method
    }
    if (n <= 10) {
      p <- (r - 3 / 8) / (n + 1 / 4)
    }
    y = qlnorm(p, est_mean, est_stdv)
    x <- sort(x)
    y <- sort(y)
    df <- data.frame(x, y)
    ggplot(df) +
      geom_point(aes(x,y), shape = 1, size = 3, color = "red") +
      geom_abline(
        intercept = 0, slope = 1, alpha = 0.5, color = "Blue"
      ) +
      xlab("Observed value") +
      ylab("Expected lognormal value") +
      ggtitle(paste("Lognormal Plot for ", input$Xvar," (Stats", results5, sep = ""))
  }, width = 370, height = 320)
  
  
  #Boxplot for the selected data
  output$bxplot <- renderPlot({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    
    if (input$Xvar == "None")
      return(NULL)
    # generate an box plot and plot it
    if (input$Zvar == "None") {
      ggplot(dataUpload(),
             aes_string(x = factor(0), y = input$Xvar)) +
        geom_boxplot() +
        ggtitle("Box Plot")
    }
    else{
      ggplot(dataUpload(),
             aes_string(
               x = input$Zvar, y = input$Xvar, color = input$Zvar
             )) +
        geom_boxplot() +
        ggtitle("Box Plot")
    }
    
    #boxplot(dataUpload()[ , input$box_var],
    #       outline = input$outliers,
    #       main = paste("Boxplot for ", colnames(dataUpload()[input$box_var]), sep = " "),
    #       xlab = colnames(dataUpload()[input$box_var]),
    #       col="blue",
    #       horizontal =FALSE)
    
  }, width = 400, height = 320)
  
  #Jitterplot for the selected data
  output$jtplot <- renderPlot({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    if (input$Xvar == "None")
      return(NULL)
    # generate jitter plot and plot it
    if (input$Zvar == "None") {
      ggplot(dataUpload(),
             aes_string(x = factor(0), y = input$Xvar)) +
        geom_jitter() +
        ggtitle("Jitter Plot")
    }
    else{
      ggplot(dataUpload(),
             aes_string(
               x = input$Zvar, y = input$Xvar, color = input$Zvar
             )) +
        geom_jitter() +
        ggtitle("Jitter Plot")
    }
  }, width = 400, height = 320)
  
  #show outlier values
  #output$outlierplot <- renderTable({
  #if no file selected return NULL
  #  if(is.null(input$file))
  #    return(NULL)
  # generate an box plot and plot it
  #  output$boxplot=renderPlot(ggplot(dataset(),aes_string(x="Species",y=input$dvar))+
  #                             geom_boxplot()+ ggtitle("Box Plot"))
  #  bplot <- boxplot(dataUpload()[ , input$box_var],
  #                   outline = input$outliers,
  #                   main = paste("Boxplot for ", colnames(dataUpload()[input$box_var]), sep = " "),
  #                   xlab = colnames(dataUpload()[input$box_var]),
  #                   col="blue",
  #                   horizontal =FALSE)
  #  data.frame(bplot$out)
  #}, width = 370, height = 320)
  
  
  #scatter plot for the selected data
  output$scatplot <- renderPlot({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    if (input$Xvar == "None" | input$Yvar == "None")
      return(NULL)
    if (input$Zvar == "None") {
      ggplot(dataUpload(),
             aes_string(
               x = input$Xvar, y = input$Yvar, color = input$Xvar
             )) +
        geom_point() +
        ggtitle("Scatter Plot")
    }
    else{
      ggplot(dataUpload(),
             aes_string(
               x = input$Xvar, y = input$Yvar, color = input$Zvar
             )) +
        geom_point() +
        ggtitle("Scatter Plot")
    }
    #using google vis for visualization (chnage renderPlot to renderGvis)
    #library(googleVis)
    #  scatp = data.frame((dataUpload()[,input$Xvar]), dataUpload()[,input$Yvar])
    #  m <- gvisScatterChart(scatp,
    #                   options = list(title = "scatter plot",
    #                             hAxis = "{title:'input$Xvar'}",
    #                             vAxis="{title:'input$Yvar'}",
    #                             legend = "none",
    #                             dataOpacity = 0.8,
    #                             width = 500, height = 300))
    #                             #trendlines="{0:{type:'linear', visibleInLegend: true
    #                             #showR2: true}}"))
    #  return(m)
  },width = 400, height = 320)
  
  
  #Rug plot for the selected data
  output$rgplot <- renderPlot({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    #scatter variable used for rug plot
    if (input$Xvar == "None" | input$Yvar == "None")
      return(NULL)
    if (input$Zvar == "None") {
      ggplot(dataUpload(),
             aes_string(
               x = input$Xvar, y = input$Yvar, color = input$Xvar
             )) +
        geom_point() + geom_smooth(method = lm, fullrange = FALSE) +
        geom_rug(col = "darkred",aplha = 0.1) +
        ggtitle("Rug Plot")
    }
    else{
      ggplot(dataUpload(),
             aes_string(
               x = input$Xvar, y = input$Yvar, color = input$Zvar
             )) +
        geom_point() + geom_smooth(method = lm, fullrange = FALSE) +
        geom_rug(col = "darkred",aplha = 0.1) +
        ggtitle("Rug Plot")
    }
  },width = 400, height = 320)
  
  output$corelplot <- renderPlot({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    if (input$Zvar == "None") {
      return(NULL)  #need group to perform corelation
    }
    else {
      # calculate the mean for each variable using the group (Zvar)
      cols = sapply(dataUpload(), is.numeric)
      cols = names(cols)[cols]
      a <-
        aggregate(dataUpload()[,c(cols)], list(dataUpload()[, input$Zvar]),
                  function(x)
                    c(mean = mean(
                      x[is.numeric(x)],na.action = na.pass,na.rm = TRUE
                    )))
    }
    #print(a)
    #convert list a to matrix o
    myData <- t(do.call(rbind,a))
    # give row names to matrix using the Zvar (group)
    #rownames(myData) <- names(a$Group.1)
    #remove columns with all NA values. This will happen if all categorical col.
    myData <- myData[,colSums(is.na(myData)) == 0]
    myData <- myData[,-1]
    #myData <- mtcars[, c(1,3,4,5,6,7)]
    #print(myData)
    
    #create corelation matrix
    cormat <- round(cor(myData),2)
    #print(cormat)
    
    # Reorder the correlation matrix
    dd <- as.dist((1 - cormat) / 2)
    hc <- hclust(dd)
    cormat <- cormat[hc$order, hc$order]
    
    #get upper_tri
    cormat[lower.tri(cormat)] <- NA
    
    # Melt the correlation matrix
    melted_cormat <- melt(cormat)
    #print(melted_cormat)
    melted_cormat <- na.omit(melted_cormat)
    
    # Create a ggheatmap
    ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "blue", high = "red", mid = "white",
        midpoint = 0, limit = c(-1,1), name = "Pearson\nCorrelation"
      ) +
      theme_minimal() + # minimal theme
      theme(axis.text.x = element_text(
        angle = 45, vjust = 1,
        size = 12, hjust = 1
      )) +
      coord_fixed() +
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal"
      ) +
      guides(fill = guide_colorbar(
        barwidth = 7, barheight = 1,
        title.position = "top", title.hjust = 0.5
      ))
  })
  
  output$coreltext <- renderTable({
    #if no file selected return NULL
    if (is.null(input$file))
      return(NULL)
    if (input$Zvar == "None") {
      return(NULL)  #need group to perform corelation
    }
    else {
      # calculate the mean for each variable using the group (Zvar)
      cols = sapply(dataUpload(), is.numeric)
      cols = names(cols)[cols]
      a <-
        aggregate(dataUpload()[,c(cols)], list(dataUpload()[, input$Zvar]),
                  function(x)
                    c(mean = mean(
                      x[is.numeric(x)],na.action = na.pass,na.rm = TRUE
                    )))
    }
  })
})
