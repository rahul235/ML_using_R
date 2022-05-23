library(shiny)


shinyUI(fluidPage(titlePanel(
  img(
    src = "newLogo_without_tagline.jpg.jpg", height = 80, width = 225
  )
),

sidebarLayout(
  #content on the sidebar
  sidebarPanel(
    wellPanel(
      h4("Build model"),
      fileInput(
        "file", 'Choose csv file',
        accept=c('text/csv',
                 'text/comma-separated-values,text/plain',
                 '.csv'))
      ),
    uiOutput("choose_column"),
    br(),
    wellPanel(
      h4("Score candidate"),
      fileInput(
        "scrfile", 'Choose csv file',
        accept = c('sheetName', 'header'), multiple =
          FALSE)
    ),
    br(),br(),
    wellPanel(
      p(
        "Model parameters does not encompass interactions. May not be an optimized model.For
        customization as per your data:"
      ),
      br(),
      h4("Contact us:"),
      p(span("info@arimaresearch.com", style = "color:blue")),
      br(),
      p("For Demo only. Not for commercial use.")
      )
  ),
  
  #content on the center page
  mainPanel(
    h2("Predicting Renege"),
    p(
      "Select csv files for model building and scoring a candidate."
    ),
    tabsetPanel(
      tabPanel("Data",
               dataTableOutput("tabl")
              ),
      tabPanel("Selected data",
        dataTableOutput("seltbl")
              ),
      tabPanel("Model building & validation",
                p("Prediction stats for model"),
               fluidRow(
                 column(6, plotOutput("logregroc")),
                 br(),
                 column(6,p("Goodness of fit test"),
                 verbatimTextOutput("logreghltest"),
                 p("Confusion matrix on test data"),
                 column(3,numericInput("cutoff", "Cut-off",
                              value = 0.5, min = 0, max = 1,step = 0.1)),
                 verbatimTextOutput("predictlogreg"))
               ),
               tableOutput("logregsum")
      ),
      tabPanel("Score new candidate",
               p(""),
               dataTableOutput("scoredata")
      )
    )
    )
)))