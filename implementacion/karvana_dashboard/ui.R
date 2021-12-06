
navbarPage(title = "Karvana Dashboard",
           theme = bs_theme(primary = "#e9832d", font_scale = NULL, bootswatch = "sandstone"),
           tags$head(tags$link(href = "style.css", rel = "stylesheet")),
           # Main Plots----
           tabPanel("Main",
                    sidebarLayout(
                      # Sidebar----
                        sidebarPanel(
                            width = 2,
                            pickerInput(
                                "picker_maker", 
                                label = "Select Maker: ",
                                choices = choices_maker,
                                selected = unique(automobile$make),
                                multiple = T,
                                options = pickerOptions(liveSearch = T, actionsBox = T)
                            ),
                            shiny::sliderInput(
                                'slider_horsepower',
                                label = 'Horsepower:',
                                min = min(automobile$horsepower), 
                                max = max(automobile$horsepower),
                                value = c(min(automobile$horsepower), max(automobile$horsepower))
                            ),
                            shiny::sliderInput(
                              'slider_curb_weight',
                              label = 'Curb Weight:',
                              min = min(automobile$curb_weight), 
                              max = max(automobile$curb_weight),
                              value = c(min(automobile$curb_weight), max(automobile$curb_weight))
                            ),
                            shiny::checkboxGroupInput(
                                'check_engine_type',
                                label = 'Engine Type:',
                                choices = unique(automobile$engine_type),
                                selected = unique(automobile$engine_type)
                            ),
                            shiny::checkboxGroupInput(
                                'check_drive_wheels',
                                label = 'Drive Wheels:',
                                choices = unique(automobile$drive_wheels),
                                selected = unique(automobile$drive_wheels)
                            )
                        ),
                      # Plots----
                        mainPanel(
                            width = 10,
                            fluidRow(
                                column(3, wellPanel(htmlOutput('card_total_units')) ),  
                                column(3, wellPanel(htmlOutput('card_mean_price')) ),  
                                column(3, wellPanel(htmlOutput('card_mae')) ),  
                                column(3, wellPanel(htmlOutput('card_rmse')) )  
                            ),
                            br(),
                            tabsetPanel(
                              tabPanel(
                                'Portfolio',
                                fluidRow(
                                  column(6, h4('Portfolio Composition: Units'), 
                                         plotOutput("portfolio_composition_plot") %>% withSpinner()),   
                                  column(6, h4('Price by Tier'), 
                                         plotOutput("price_tier_plot") %>% withSpinner())   
                                )
                              ),
                              tabPanel(
                                'Model Evaluation',
                                fluidRow(
                                  column(6, h4('Price vs Prediction'), 
                                         plotOutput("price_prediction_plot") %>% withSpinner()),   
                                  column(6, h4('Variable Importance'), 
                                         plotOutput("vip") %>% withSpinner())   
                                ),
                                fluidRow(
                                  column(6, h4('Error by Tier'), 
                                         plotOutput("error_tier_plot") %>% withSpinner()),   
                                  column(6, h4('Error vs Curb Weight'), 
                                         plotOutput("error_curb_plot") %>% withSpinner())   
                                )
                              )
                            )
                        )
                    )
           ),
           # Price Prediction----
           tabPanel("Price Prediction",
                    fluidRow(
                      column(
                        5,
                        h5('Set the inputs or randomize them to get predictions based on a Random Forest model: ')
                      ),
                      column(
                        3,
                        wellPanel(htmlOutput('card_prediction'))
                      )
                    ),
                    br(),
                    wellPanel(uiOutput('prediction_features')),
                    br(),
                    fluidRow(
                      column(
                        12,
                        plotOutput("prediction_plot", height = "280px") %>% withSpinner()
                      )
                    )
           ),
           # Info----
           tabPanel('Info',
                    fluidRow(
                      column(12, wellPanel(htmltools::includeMarkdown('md/info.md')))
                    )
           )
)