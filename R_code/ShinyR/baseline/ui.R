library(shiny)
library(dygraphs)

shinyUI(fluidPage(titlePanel(
  img(
    src = "Arima-research-logo.jpg", height = 80, width = 225
  )
),

sidebarLayout(
  #content on the sidebar
  sidebarPanel(fluidRow(
    column(
      10,h4("Input data"),
      fileInput(
        "file", 'Choose XLSX File',
        accept = c('sheetName', 'header'), multiple =
          FALSE
      ),
      #fluidRow(
      selectInput("Xvar", "X Variable: ",""),
      selectInput("Yvar", "Y Variable: ",""),
      selectInput("Zvar", "Group by (Optional): ",""),
      #selectizeInput("Zvar", "Group by (Optional): ", choices = NULL, selected = NULL, multiple = TRUE,
      #                options = list(placeholder = 'select value')),
      #),
      br(),
      br(),
      p(
        "Visualize data through various charts. For
        customization as per your data:"
      ),
      br(),
      h4("Contact us:"),
      p(span("info@arimaresearch.com", style = "color:blue")),
      br(),
      p("For Demo only. Not for commercial use.")
      )
  )),
  
  #content on the center page
  mainPanel(
    h2("Visualization of data"),
    p(
      "Select an excel file with numeric value in the first sheet.
      Visualize data through different charts"
    ),
    
    
    #create tabs to show different outputs/charts.
    tabsetPanel(
      tabPanel("Data",
               dataTableOutput("tabl")),
      tabPanel("Outliers",
               p("Outlier without any grouping"),
               verbatimTextOutput("otlrsummary")),
      
      tabPanel("Summary",
               p("Summary without any outlier removed"),
               verbatimTextOutput("summary")),
      tabPanel(
        "Basic plot",
        h5(
          "Box plot & Jitter plot: Select ", span("X variable ", style = "color:blue"),
          " and ", span("Group by", style = "color: blue"), "variable"
        ),
        #fluidRow(
        #  column(4,offset = 1, selectInput("box_Xvar", "Select variable: ","")),
        # column(4,selectInput("box_Yvar", "Select categorical group (Optional): ",""))
        #),
        fluidRow(column(4,plotOutput("bxplot")),
                 column(4, offset = 2, plotOutput("jtplot"))),
        #fluidRow(
        # column(4,selectInput("scatter_Xvar", "X Variable: ","")),
        # column(4, selectInput("scatter_Yvar", "Y Variable: ","")),
        # column(4,selectInput("scatter_Zvar", "Group by (Optional): ",""))
        #),
        h5(
          "Scatter plot & Rug plot: Select ", span("X variable, Y variable ", style = "color:blue"),
          " and ", span("Group by", style = "color: blue"), "variable"
        ),
        fluidRow(column(4,plotOutput("scatplot")),
                 column(4,offset = 2,plotOutput("rgplot")))
      ),
      tabPanel(
        "Best fit",
        h5(
          "Select ", span("X variable ", style = "color:blue"),
          " and ", span("Group by", style = "color: blue"), "variable"
        ),
        fluidRow(column(4,h5(
          "Histogram and distribution fit:"
        )),
        column(
          4,sliderInput(
            "bins",
            "Bins for histogram control:",
            min = 1,
            max = 50,
            value = 30
          )
        )),
        #fluidRow(
        #         column(4,selectInput("hist_var", "Select variable: ","")),
        #         column(4,selectInput("hist_Gvar", "Select categorical group (Optional): ","")),
        #         column(4, sliderInput("bins",
        #                              "Bins for histogram control:",
        #                              min = 1,
        #                              max = 50,
        #                              value = 30))),
        fluidRow(column(4,plotOutput("histplot")),
                 column(4,offset = 2,plotOutput("denplot"))),
        fluidRow(column(4,plotOutput("logisticplot")),
                 column(4,offset = 2,plotOutput("normalplot"))),
        fluidRow(column(4,plotOutput("expplot")),
                 column(4,offset = 2,plotOutput("weibullplot"))),
        fluidRow(column(4,plotOutput("lognormplot")))
      ),
      tabPanel("Heat map",
               h5(
                 "Select ", span("Group by", style = "color:blue"), "variable"
               ),
               plotOutput("corelplot"),
               tableOutput("coreltext"))
    )
    )
)))