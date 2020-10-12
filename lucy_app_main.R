library(ggplot2)
library(ggcorrplot)
library(grid)
library(gridExtra)
library(dplyr)
library(class)
library(plyr)
library(pROC)
library(caret)
library(e1071)
library(caTools)
library(gbm, quietly=TRUE)
library(shiny)
library(data.table)
library(shinythemes)
library(shinyBS)
library(shinyjs)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Predicting COVID-19 Mortality using Machine Learning",
                  tabPanel("Prediction Results",
                           sidebarLayout(
                             sidebarPanel(
                               h3('Please enter patient information:'),
                               numericInput('age', 'Age', value = '', width = 75),
                               radioButtons('sex2', 'Sex:', c('Female' = 1, 'Male' = 0)),
                               tags$b('Symptoms or Preconditions:'),
                               checkboxInput('pneumonia1', 'Pneumonia'),
                               checkboxInput('diabetes1', 'Diabetes'),
                               checkboxInput('copd1', 'Chronic Obstructive Pulmonary Disease (COPD)'),
                               checkboxInput('asthma1', 'Asthma'),
                               checkboxInput('inmsupr1', 'Immuosuppressed'),
                               checkboxInput('hypertension1', 'Hypertension'),
                               checkboxInput('cardiovascular1', 'Other cardiovascular disease'),
                               checkboxInput('obesity1', 'Obesity'),
                               checkboxInput('renal_chronic1', 'Chronic kidney disease'),
                               checkboxInput('tobacco1', 'Tobacco use'),
                               #checkboxInput('pregnancy1', 'Pregnancy'),
                               checkboxInput('other_disease1', 'Other chronic conditions'),
                    
                               actionButton('predict', 'Predict', class = "btn btn-primary"),
                             ), # sidebarPanel
                          
                           mainPanel(
                             #hiding 'warning when input is NA'
                             tags$style(
                               type = "text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"
                             ),
                             h3('Risk results:'),
                             h3('\n'),
                             h4(textOutput("content")),
                             h3('\n'),
                             tags$p(
                               htmlOutput("prediction", container = tags$div)
                             ),
                             htmlOutput("riskplotbutton"),
                             h3('\n'),
                             h3('\n'),
                             plotOutput('riskplot', width = '100%', height = '500px'),
                           ) # mainPanel
                        )
                           
                  ), # Navbar 1, tabPanel
                  tabPanel('Model Information',
                           h3('Dataset:'),
                           tags$line('The model was trained using publicly-available '),
                           tags$a(href = 'https://www.kaggle.com/tanmoyx/covid19-patient-precondition-dataset', 'anonymized data'),
                           tags$line(
                             'released by the Government of Mexico. The dataset contains over 200,000 confirmed individual COVID-19 cases, along with variables representing age, sex, various preconditions, and death date.'
                           ),
                           h3('Preprocessing:'),
                           tags$line(
                             'The dataset was curated by Complete Case Analysis, in which an observation was removed if one or more of its predictor variables was missing, thus resulting in the removal of <1% of total observations. It was assumed that these data were Missing Completely At Random (MCAR).'
                           ),
                           h3('Model Selection'),
                           tags$line(
                             'The following machine learning classifiers were trained on the data:'
                           ),
                           tags$div(tags$ul(
                             tags$li(tags$span('Logistic Regression')),
                             tags$li(tags$span('Random Forest')),
                             tags$li(tags$span('Decision Tree')),
                             tags$li(tags$span('Gradient Boost'))
                           )),
                           tags$line(
                             'The Random Forest model cross validated with k = 10 folds and ROSE sampling had the highest overall accuracy and balanced accuracy (91% for both). However, due to its large size, it could not be deployed successfully to the shinyapps.io cloud. Ultimately, we chose to deploy with Logistic Regression for its interpretability and low memory usage.'
                           ),
                           h3('Model Validations:'),
                           tags$line(
                             'The logistic regression model has an overall accuracy of 88.7%, specificity (true negative rate) of 91.2%, and sensitivity (true positive rate) of 56.3%. While the model is fairly accurate overall, it tends to underpredict cases where death occurred. Thus, this application is only meant to showcase ML-application as a proof-of-concept and should not be used to aid clinical decision-making
.'
                           ),
                           tags$line(withMathJax("$$\\text{Logistic Regression }X_n=X_{n-1}-\\varepsilon$$"))
                  ),
                  tabPanel("About the Authors", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage
#######################################################################################
#server
#######################################################################################
# Define server logic
library(ggplot2)

risk <- read.csv('covid_risk.csv')
check_percentile <- function(prediction) {
  if (prediction > 93)
  {
    return('100')
  }
  else
    (for (num in seq(0, 1, 0.01)) {
      if (unname(quantile(risk$x, num)) > prediction) {
        return(gsub('%', '', names(quantile(
          risk$x, num
        ))))
      }
    })
  
}

server<- function(input, output) {
  
  # Input Data

    inputdf <- reactive({
      data.frame(
        'sex2' = as.integer(input$sex2),
        'pneumonia1' = as.integer(input$pneumonia1),
        'age' = as.integer(input$age),
        'diabetes1' = as.integer(input$diabetes1),
        'copd1' = as.integer(input$copd1),
        'asthma1' = as.integer(input$asthma1),
        'inmsupr1' = as.integer(input$inmsupr1),
        'hypertension1' = as.integer(input$hypertension1),
        'other_disease1' = as.integer(input$other_disease1),
        'cardiovascular1' = as.integer(input$cardiovascular1),
        'obesity1' = as.integer(input$obesity1),
        'renal_chronic1' = as.integer(input$renal_chronic1),
        'tobacco1' = as.integer(input$tobacco1)
      )
    })
  hideElement(id = "moreInfo")
  output$content <- renderText({
    if (input$predict > 0) {
      isolate('Calculation complete, here is your personal COVID report.')
    }else{
      return("Server is ready for calculation")
    }
  })
  output$prediction <- renderText({
    if (input$predict > 0) {
      
      value <-
        isolate(local(round(as.numeric(
          exp(
            2.255304 * inputdf()$pneumonia1 - 5.80836 - 0.45264 * inputdf()$sex2 + 0.052454 *
              inputdf()$age + 0.32902 * inputdf()$diabetes1 + 0.159049 * inputdf()$copd1 -
              0.1237 * inputdf()$asthma1  + 0.308425 * inputdf()$inmsupr1 + 0.185287 *
              inputdf()$hypertension1 + 0.483032 * inputdf()$other_disease1 - 0.0927 *
              inputdf()$cardiovascular1 + 0.243465 * inputdf()$obesity1 + 0.746113 * inputdf()$renal_chronic1 -
              0.07554 * inputdf()$tobacco1
          ) / (
            1 + exp(
              -5.80836 - 0.45264 * inputdf()$sex2 + 2.255304 * inputdf()$pneumonia1 + 0.052454 *
                inputdf()$age + 0.32902 * inputdf()$diabetes1 + 0.159049 * inputdf()$copd1 -
                0.1237 * inputdf()$asthma1  + 0.308425 * inputdf()$inmsupr1 + 0.185287 *
                inputdf()$hypertension1 + 0.483032 * inputdf()$other_disease1 - 0.0927 *
                inputdf()$cardiovascular1 + 0.243465 * inputdf()$obesity1 + 0.746113 * inputdf()$renal_chronic1 -
                0.07554 * inputdf()$tobacco1
            )
          )
          * 100
        ), 2)))
      percentile <- isolate(check_percentile(value))
      suffix <-
        isolate(if (substring(percentile, nchar(percentile)) == '1') {
          suffix = 'st'
        } else if (substring(percentile, nchar(percentile)) == '2') {
          suffix = 'nd'
        } else if (substring(percentile, nchar(percentile)) == '3') {
          suffix = 'rd'
        } else {
          suffix = 'th'
        })
      percentile <- as.numeric(percentile)
      risk <-
        if (percentile > 0 &&
            percentile < 20) {
          'This can be considered a relatively low risk case.'
        } else if (percentile > 20 &&
                   percentile < 80) {
          'This can be considered an intermediate risk case'
        } else {
          'This can be considered a relatively high risk case.'
        }
      text <-
        isolate(local(
          paste(
            'The individual\'s risk of death due to COVID-19 is:',
            '<b>',
            value,
            '</b>',
            '%',
            ', which lies at the',
            '<b>',
            percentile,
            '<sup>',
            suffix,
            '</sup>',
            '</b>',
            ' percentile compared to confirmed positive cases within the general population. ',
            risk,
            '</br>',
            '</br>'
          )
        ))
      
      isolate(local(HTML(paste(text))))
      
    }
  })
  output$resetbutton <- renderText({
    if (input$moreInfo > 0){
      button <- isolate(bsButton('resetbutton', 'Reset',
                                 class = "btn btn-primary", 
                                 type = "toggle",
                                 value = TRUE,
                                 block = TRUE))
      isolate(paste(button))
    }
  })
  observeEvent(input$resetbutton, {
    # Reset data_next and data_all to 0
    output$riskplotbutton <- renderText({})
    output$prediction <- renderText({})
    output$riskplot <- renderPlot({})
  })
  
  output$riskplotbutton <- renderText({
    if (input$predict > 0){
    button <- isolate(actionButton('moreInfo', 'Check Your Risk Plot',
                                               class = "btn btn-primary"))
    isolate(paste(button))
    }
  })
  output$riskplot <- renderPlot({
   if (input$moreInfo > 0){
    input$predict
    value <-
      isolate(round(as.numeric(
        exp(
          2.255304 * inputdf()$pneumonia1 - 5.80836 - 0.45264 * inputdf()$sex2 + 0.052454 *
            inputdf()$age + 0.32902 * inputdf()$diabetes1 + 0.159049 * inputdf()$copd1 -
            0.1237 * inputdf()$asthma1  + 0.308425 * inputdf()$inmsupr1 + 0.185287 *
            inputdf()$hypertension1 + 0.483032 * inputdf()$other_disease1 - 0.0927 *
            inputdf()$cardiovascular1 + 0.243465 * inputdf()$obesity1 + 0.746113 * inputdf()$renal_chronic1 -
            0.07554 * inputdf()$tobacco1
        ) / (
          1 + exp(
            -5.80836 - 0.45264 * inputdf()$sex2 + 2.255304 * inputdf()$pneumonia1 + 0.052454 *
              inputdf()$age + 0.32902 * inputdf()$diabetes1 + 0.159049 * inputdf()$copd1 -
              0.1237 * inputdf()$asthma1  + 0.308425 * inputdf()$inmsupr1 + 0.185287 *
              inputdf()$hypertension1 + 0.483032 * inputdf()$other_disease1 - 0.0927 *
              inputdf()$cardiovascular1 + 0.243465 * inputdf()$obesity1 + 0.746113 * inputdf()$renal_chronic1 -
              0.07554 * inputdf()$tobacco1
          )
        )
        * 100
      )))
    isolate(
      ggplot(risk, aes(x = x)) +
        
        geom_density(
          alpha = .7,
          fill = '#004885',
          color = '#004885'
        ) +
        geom_vline(
          aes(xintercept = value),
          color = '#b81900',
          linetype = 'dashed',
          size = 1
        ) +
        labs(x = 'Death probability (%)', y = 'Proportion') +
        ggtitle(
          'Predicted Distribution of COVID-19 Death Probabilities within the Population'
        ) +
        annotate(
          'text',
          x = value + 2,
          y = 0.22,
          label = paste('Predicted %'),
          colour = '#b81900',
          size = 5
        )
      
      +             theme(
        plot.title = element_text(size = 16),
        panel.background = element_rect('grey98'),
        panel.grid = element_line('white'),
        panel.border = element_rect(
          colour = 'grey90',
          fill = NA,
          size = 1
        )
      )
    )
    }
    
  })
  
  observeEvent(input$moreInfo, {
    hideElement(id = "riskplotbutton", time = 0.5, anim = TRUE, animType = "fade")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
