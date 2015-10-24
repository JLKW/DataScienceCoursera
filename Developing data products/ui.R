library(shiny)


# Loading the required datasets -------------------------------------------

salary_raw <- read.csv("data/Adult.csv",header=TRUE,na.strings=" ?")
salary_visualisation <- salary_raw[complete.cases(salary_raw),-c(3,5,11,12,14)]
salary_prediction <- salary_raw[complete.cases(salary_raw),c(1,4,10,13,15)]

# get the label names for the UI from the data sets
xyValueNames <- names(salary_visualisation[,-10])
educationLabels <- as.character(unique(salary_prediction$education))
genderLabels<-as.character(unique(salary_prediction$sex))

# remove the variables to save memory
rm(salary_raw)
rm(salary_visualisation)
rm(salary_prediction)


# Shiny UI code starts here -----------------------------------------------------------

shinyUI(
  
  navbarPage("Salary Prediction App",
      tabPanel("Documentation",
          h3("Data source for this App"),
          tags$ul(
            tags$li("The data is provided by the UCI Machine Learning Repository:",
            tags$a("http://archive.ics.uci.edu/ml/datasets/Adult",href="http://archive.ics.uci.edu/ml/datasets/Adult")),
            tags$li("The data set is known as a 'Census Income' and the attribute of interest is 
                    whether a person's salary is >50k a year or <= 50k a year"),
            tags$li("There are 14 predictors of salary in this data set. For example, Age, education, sex etc.
                    For the purposes of our prediction model, we will only make use of 4 predictors namely age, education, sex and number of hours worked per week")
          ),
          
          h3("App features"),
          p("This app has 2 features:"),
          tags$ul( 
            tags$li("The first feature is to allow the user to visualise the data in the form of a scatterplot"),
            tags$li("The second feature allows the user to see what the model predicted annual salary is based on certain values of the 4 predictors")
          ),
          
          h3("How to use the App"),
          tags$ul(
            tags$li("Upon initialising the App, please wait a while for the app to load"),
            tags$li("To use the scatter plot feature, please select the variables to plot on the x and y axis"),
            tags$li("To use the prediction feature, please select the input values for the 4 predictors. The predicted annual salary would be either >50k or <= 50k")
          )
          
      ),     
             
      tabPanel("Visualise data",
        tabsetPanel(
          tabPanel("Scatter plot", 
             
             #Scatter plot that is linked to server.R
             plotOutput("myChart"),
    
             fluidRow(
                column(3,offset=3,
                h4("Value1"),
                selectInput('xvalue',
                            label = "X variable",
                            xyValueNames,
                            selected = "Age")
                ),
                
                column(3,
                h4("Value2"),
                selectInput('yvalue',
                             label = "Y variable",
                            xyValueNames,
                             selected = "education")
                )
              )
          ),
          tabPanel("Data table",
             #Data table that is linked to server.R
             fluidRow(dataTableOutput("datatable"))
                   
          )
          
        )
      ),
      
      tabPanel("Prediction",
            
         sidebarPanel(h3("Predictors"),
           sliderInput("age",label=h5("Age"),
                       min=0,max=100,value=50),
           
           selectInput("education",label="Education",
                       sort(educationLabels)
           ),
           
           selectInput("sex",label="Sex",
                       genderLabels
           ),
           
           sliderInput("hours",label=h5("Working hours per week"),
                       min=0,max=80,value=40)
                      
         ), 
         
         mainPanel(
            h3('Results of prediction'),
            
            h5('You have entered:'),
            #Data table that is linked to server.R
            tableOutput("myTable"),
            
            h5('The predicted annual salary is:'),
            #Prediction result that is linked to server.R
            verbatimTextOutput("myPrediction")
      
         )
      
      )
  )      
)
   



