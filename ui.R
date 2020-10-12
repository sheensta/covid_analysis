library(shiny)
library(shinythemes)
library(car)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme('cerulean'),
    # Application title
    
    navbarPage(
        'Predicting COVID-19 Mortality using Machine Learning',
        tabPanel(
            'Prediction Results',
            sidebarPanel(
                h3('Please enter patient information:'),
                numericInput(
                    'age',
                    'Age',
                    value = '',
                    width = 75,
                    min = 0
                ),
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
                actionButton('predict', 'Predict', class = "btn btn-primary")
            ),
            mainPanel(
                #hiding 'warning when input is NA'
                tags$style(
                    type = "text/css",
                    ".shiny-output-error { visibility: hidden; }",
                    ".shiny-output-error:before { visibility: hidden; }"
                ),
                h3('Risk results:'),
                h3('\n'),
                h3('\n'),
                htmlOutput('prediction'),
                h3('\n'),
                h3('\n'),
                plotOutput('riskplot', width = '100%', height = '500px')
            )
        ),
        tabPanel(
            'Model Information',
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
        tabPanel('About the Authors',)
    )
    
)
