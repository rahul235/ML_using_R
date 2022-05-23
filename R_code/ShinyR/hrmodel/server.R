library(shiny)
library(MASS)
library(DT)
library(stats)    #for binary logistic without wald statistics
library(Deducer)  #ROC plot
library(ResourceSelection) #Hosmer lemeshow GoF test
library(rJava)
library(iplots)



options(shiny.maxRequestSize=30*1024^2) #shiny allows 5 mb upload only. Inc to 30 MB
options(java.parameters = "-Xmx1000m")  #allocate more sapce to read the file. Jave heap error.

split_data = function(datasplit){
dat <<- na.omit(datasplit) # listwise deletion of missing values
#------creating training & test data---------------------------------
bound <- floor((nrow(dat)/4)*3)           #define % of training and test set
set.seed(12345)
dat <<- dat[sample(nrow(dat)), ]           #sample rows '<<-' used for global use of this variable
dat.train <<- dat[1:bound, ]               # training dataset
dat.test <<- dat[(bound+1):nrow(dat), ]    # test dataset
#--------------------------------------------------------------------
}

shinyServer(function(input, output, session) {

dataUpload = reactive({
   infile = input$file
   if (is.null(infile))
     return(NULL)
   infile_read = read.csv(infile$datapath, 1)
   split_data(infile_read)
   return(infile_read)
 })

scoreData = reactive({
  scrfile = input$scrfile
  if (is.null(scrfile))
    return(NULL)
  scrfile_read = read.csv(scrfile$datapath, 1)
  return(scrfile_read)
})

output$choose_column <- renderUI({
  # If missing input, return to avoid error later in function
  if(is.null(input$file))
    return()
  # Get the data set with the appropriate name
  #dat <- dataUpload()
  colnames <- colnames(dataUpload())
  # Create the checkboxes and select them all by default
  checkboxGroupInput("column", "Y= Status. Select Xs:", 
                     choices  = colnames,
                     selected = colnames[2:3])
})

#Show the first 100 selected data in Data tabbn,d
output$tabl <- renderDataTable(
  dataUpload(), options=list(pageLength=10)
)

output$seltbl <- renderDataTable(
  dataUpload()[, input$column], options=list(pageLength=10)
)

#logistic regression
runLogistic = reactive({
  dat.train$Status <- relevel(dat.train$Status, ref = 1) #set the base category for response variable
  glm(as.formula(paste("Status"," ~ ", paste(input$column,collapse="+"))),
      data=dat, family = binomial)
})

#model summary
output$logregsum <- renderTable({
  if(!is.null(input$column)){
    model <- runLogistic()
    summary(model)
  } else {
    print(data.frame(Warning="Please select Model Parameters."))
  }
})

#Hosmer lemeshow test
output$logreghltest <- renderPrint({
  if(!is.null(input$column)){
    print(paste("Training data set:", length(dat.train[,1])))
    print(paste("Test data set:", length(dat.test[,1])))
    model <- runLogistic()
    hoslem.test(model$y, fitted(model))
    #hl.df <- as.data.frame(do.call(rbind, hl_test))
  }
})

#ROC plot
output$logregroc <- renderPlot({
  if(!is.null(input$column)){
    model <- runLogistic()
    rocplot(model)
  }
})

#model summary
output$predictlogreg <- renderPrint({
  if(!is.null(input$column)){
    #prediction on the test data
    model <- runLogistic()
    model_pred_probs = predict.glm(model, dat.test, type = "response")
    dat_test_Prob <- data.frame(dat.test,model_pred_probs)
    
    #variable with all the values as joined
    n <- length(dat.test$Status)
    model_prediction = rep("Joined", n)
    # defining log odds in favor of not joining
    model_prediction[model_pred_probs > input$cutoff] = "Not Joined"
    print(paste("Cut off value: ", input$cutoff))
    #add the model_precition in the data (not being used here)
    dat_test_Prob <- data.frame(dat.test,model_pred_probs, model_prediction)
    
    # Create the confusion matrix, and compute the misclassification rate 
    tbl <- table(dat.test$Status, model_prediction)
    addmargins(tbl) #sum the values
  }
})

#Score candidate
output$scoredata <- renderDataTable({
  if(is.null(input$scrfile))
    return()
    model <- runLogistic()
    srcdata <- scoreData()  #input new candidate data to srcdata
    srcdata <- na.omit(srcdata) # listwise deletion of missing values
    Model.pred.probs = predict.glm(model, srcdata, type = "response")
    Score.candidate <- data.frame(srcdata,Model.pred.probs)
    
    #variable with all the values as joined
    n <- length(srcdata[,1])
    Model.prediction = rep("Highly likely to join", n)
    # defining log odds in favor of not joining
    Model.prediction[Model.pred.probs > input$cutoff-0.1] = "Less likely to join"
    
    Model.prediction[Model.pred.probs > input$cutoff] = "Unlikely to join"
    
    #add the model_precition in the data (not being used here)
    Score.candidate <- data.frame(srcdata,Model.pred.probs, Model.prediction)
    
    #datatable(Score.candidate) format with color code.
    datatable(Score.candidate, options = list(pageLength = 25)) %>%
    formatStyle(
      'Model.prediction',
      color = styleEqual(c("Highly likely to join","Less likely to join","Unlikely to join"), 
                         c('white','black','white')),
      backgroundColor = styleEqual(c("Highly likely to join","Less likely to join","Unlikely to join"),
                                   c('green','yellow','red'))
    )

    #------------------------write to csv---------------------------------------
    #library(xlsx)
    #write.csv(Score.candidate, "NewCandidateScore.csv") #export to excel
    #--------------------------------------End------------------------------------
})

})