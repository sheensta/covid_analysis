# Define server logic
library(ggplot2)
library(shinyBS)
library(shinyjs)


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

server <- function(input, output) {
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
    
    output$prediction <- renderText({
        if (input$predict == 0) {
            paste('Server is ready for calculation.')
        }
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
                    ' percentile compared to all predicted COVID-19 positive cases within the entire dataset. ',
                    risk
                )
            ))
        isolate(local(HTML(paste(text))))
        
    })
    
    output$riskplot <- renderPlot({
        if (input$predict == 0)
            return()
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
                    'Population Distribution of Predicted Probabilities'
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
        


        
    })
    
}
