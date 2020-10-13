library(shiny)
library(shinythemes)

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
                'The dataset was curated by Complete Case Analysis, in which an observation was removed if one or more of its predictor variables was missing, thus resulting in the removal of ~1% of total observations. It was assumed that these data were Missing Completely At Random (MCAR).'
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
                'The Random Forest model cross validated with k = 10 folds and ROSE sampling had the highest overall accuracy and balanced accuracy (91% for both). However, due to its large size, it could not be deployed successfully to the shinyapps.io cloud. Ultimately, we chose to deploy with Logistic Regression for its interpretability and low memory usage. Below is the generalized equation showing the log-odds:'
            ),
            tags$line(
                withMathJax("$$\\text{ℓ}=β_{0}+β_{1}x_{1}+β_{2}x_{2} + ... + β_{n}x_{n}$$")
            ),
            tags$line('where ℓ represents the log-odds and β represents the parameters of the model (i.e. age, sex, and preconditions). Probability is then calculated by the following:'),
            tags$line(withMathJax(('$$\\rho = \\frac{e^ℓ}{1+e^ℓ}$$'))),
            h3('Model Validations:'),
            tags$line(
                'The logistic regression model has an overall accuracy of 88.7%, specificity (true negative rate) of 91.2%, and sensitivity (true positive rate) of 56.3%. While the model is fairly accurate in general, it tends to underpredict for cases where death occurred. As such, this application is only meant to showcase the ML model as a proof-of-concept and should not be used to aid clinical decision-making.
.'
            )
            
        ),
        tabPanel('About the Authors', 
                 h3('Sean X. Zhang'),
                 tags$line('Sean works as an Analyst specializing in health data privacy at a global analytics and consulting company. He completed his BSc. Neuroscience and MSc. Experimental Medicine (with a focus on Biomedical ethics) both at McGill University and is completing a Certificate in Machine Learning from York University. Coming from an interdisiplinary background of biomedical science, data science, and ethics, Sean aims to apply his diverse skillset to improve data-driven solutions within the healthcare sector.'),
                 h3('Lucy Zhang'),
                 tags$line('Lucy completed her degree in Software Engineering at McMaster University.'),
                 h3('Colin Green'),
                 tags$line('Colin Green holds a Bachelor of Science with a major in Physics and a minor in Business from the University of Ottawa and is currently a student a York University studying machine learning.  As an aspiring data scientist, he hopes to one day contribute to the solution of the climate crisis with the use of machine learning.'),
                 h3('Albina Cako'),
                 tags$line('Education: Bachelor of Science (Honors with Distinction) Ryerson University. Currently on my 4th year of Doctor of Naturopathic Medicine Degree at the Canadian College of Naturopathic Medicine. As well, doing a Certificate on Machine learning at York University and a Masters of Biomedical Engineering at Ryerson University focusing on Machine Learning. Interests: Machine learning in the medical field, as well as use of Machine learning in technological innovation, marketing, social media and business development.'),
                 )
    )
    
)
