library(shiny)
library(ggplot2)
require(randomForest)


# Loading the required data sets ------------------------------------------

salary_raw <- read.csv("data/Adult.csv",header=TRUE,na.strings=" ?")
salary_visualisation <- salary_raw[complete.cases(salary_raw),-c(3,5,11,12,14)]
salary_prediction <- salary_raw[complete.cases(salary_raw),c(1,4,10,13,15)]

# random forest prediction model 
model_RF <- randomForest(salary~.,data=salary_prediction,ntree=5)


# Shiny server code starts here -------------------------------------------

shinyServer(function(input, output) {

 #Scatter plot output  
 output$myChart <- renderPlot({
    x <- input$xvalue
    y <- input$yvalue
    print(ggplot(data=salary_visualisation,aes_string(x,y),colour=salary)+
            geom_point(aes(color=salary))+
            theme(axis.text.x = element_text(angle = 45,hjust=1))
          )
  })
  
 #Data table output
 output$datatable <- renderDataTable({salary_raw})
 
 #Reactive statements to consolidate the user's selected values into a data table
 inputTable <- reactive({model_input <- salary_prediction[1,-5]
                          model_input <- rbind(model_input,
                                   data.frame(Age = as.integer(input$age),
                                              education = as.factor(input$education),
                                              sex = as.factor(input$sex),
                                              hours.per.week = as.integer(input$hours)))
                          return(model_input[2,])
  }) 
 
 #Reactive statements to predict the salary based on the user's selected values
 prediction_result <- reactive({
                         model_input <- salary_prediction[1,-5]
                         model_input <- rbind(model_input,
                                              data.frame(Age = as.integer(input$age),
                                                         education = as.factor(input$education),
                                                         sex = as.factor(input$sex),
                                                         hours.per.week = as.integer(input$hours)))
                         prediction <- predict(model_RF,newdata=model_input[2,])
                         if(as.character(prediction)==" >50K"){return("Annual Salary: >50K")}
                         else{return("Annual Salary: <=50K")}
 })

 #Data table output to display to the user what he selected
 output$myTable <- renderTable(inputTable())
 
 #Prediction result showed to the user
 output$myPrediction <- renderPrint(prediction_result())
   
})

