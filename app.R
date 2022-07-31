# 01 Libraries ----

library(tidyverse)
library(scales)  
library(shiny)
library(kableExtra)
library(bslib)

# 02 Control options ----

# Features
feature_names <- c("Sale Price", "Lot Frontage", "Lot Area", 
                   "Basement Area", "Garage Area", "Age of House", 
                   "Porch and Decking Area", "Above Ground Area")
feature_choices <- unique(ames_plots$variables)
feature_list <- as.list(feature_choices)
names(feature_list) <- feature_names

# Metrics
metric_names <- c("None", "Natural Logorithm", "Square Root", 
                  "Yeo-Johnson", "Best Normalize")
metric_choices <- unique(ames_plots$metrics)
metric_list <- as.list(metric_choices)
names(metric_list) <- metric_names

# Fill options
colour_names <- c("Light", "Medium", "Dark")
colour_choices <- c("#a1adb6", "#b0afa0", "#7a8477")
colour_list <- as.list(colour_choices)
names(colour_list) <- colour_names

# Outline options 
outline_names <- c("Black", "White")
outline_choices <- c("black", "white")
outline_list <- as.list(outline_choices)
names(outline_list) <- outline_names


# 03 User interface ----

ui <- fluidPage(
   
   theme = bs_theme(
      bg = "#1D5667",
      fg = "#EFF7F7",
      primary = "#F9BC00"
   ), 
   
   tags$style(".recalculating { opacity: inherit !important; }"),
   
   titlePanel("Ames Housing Plotting Application"),
   
   tabsetPanel(
      type = "tabs",
      tabPanel(
         title = "Histograms",
         fluidRow(
            column(
               width = 2,
               selectInput(
                  inputId = "plot_features",
                  label = "Select a Variable",
                  choices = feature_list,
                  selected = "sale_price"
               ),
               selectInput(
                  inputId = "plot_metrics",
                  label = "Select a Transformation",
                  choices = metric_list,
                  selected = "none"
               ),
               sliderInput(
                  inputId = "plot_bins",
                  label = "Select the Number of Bins",
                  min = 5, max = 50, value = 25, step = 5
               ),
               
               selectInput(
                  inputId = "plot_fill",
                  label = "Select a Colour",
                  choices = colour_list,
                  selected = "Light"
               ),
               selectInput(
                  inputId = "plot_outline",
                  label = "Select an Outline Colour",
                  choices = outline_list,
                  selected = "black"
               ),
               
               selectInput(
                  inputId = "skew_tbl",
                  label = "Expand / Collapse Table",
                  choices = c("Active Variable", "All Variables"),
                  selected = "Active Variable"
                  
               )
            ),
            column(
               width = 5,
               plotOutput("ames_hist_1"),
               h5("Showing the measure of Skew for each variable"),
               wellPanel(
                  width = 5,
                  style = "overflow-y:scroll; max-height: 212px",
                  htmlOutput("ames_skew_tbl")
               )   
            ),
            column(
               width = 5,
               plotOutput("ames_hist_2"),
               h5("Guidance on how to use the app"),
               wellPanel(
                  width = 5, 
                  p("The first plot shows the distribution of the selected variable. 
                  The number of bins can be changed, 
                  as can the colours."),
                  
                  p("The second plot is different in so far as we can also transform 
                    the selected variable."),
                  
                  p("The table to the left shows the measure of skew for each of the variables under 
                  all transformations. When collapsed it shows only the active variable")
               )
            )
         )
      ),
      tabPanel(
         title = "About",
         
         br(),
         
         p("This application is meant to accompany the analysis of the Ames Housing dataset 
           which can be found ", tags$a(href = "https://rich1707.github.io/AmesHousing/", 
                                        "on my GitHub Pages.", target = "blank")),
         
         p("Although the app is very basic, I found it helped me find the best transformations
           without cluttering up my script with histograms.")
      )
   )
)


server <- function(input, output, session) {
   
   # First Plot ----
   
   output$ames_hist_1 <- renderPlot({
      ames_plots %>% 
         filter(variables == input$plot_features, metrics == "none") %>% 
         ggplot(aes(x = values)) +
         geom_histogram(bins = input$plot_bins, fill = input$plot_fill, 
                        colour = input$plot_outline) +
         labs(x = NULL, y = NULL,
              title = paste(
                 "Showing the distribution of the",
                 feature_names[feature_choices == input$plot_features], 
                 "variable"),
              subtitle = "Transformation applied = None"
         ) +
         scale_x_continuous(labels = label_comma()) +
         theme(
            axis.text.x = element_text(face = "bold", size = 12),
            axis.text.y = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold", size = 18),
            plot.subtitle = element_text(face = "bold", size = 14)
         )
   })
   
   # Second Plot ----
   
   output$ames_hist_2 <- renderPlot({
      ames_plots %>% 
         filter(variables == input$plot_features, metrics == input$plot_metrics) %>% 
         ggplot(aes(x = values)) + 
         geom_histogram(bins = input$plot_bins, fill = input$plot_fill, 
                        colour = input$plot_outline) +
         labs(x = NULL, y = NULL,
              title = paste(
                 "Showing the distribution of the",
                 feature_names[feature_choices == input$plot_features], 
                 "variable"),
              subtitle = paste(
                 "Transformation applied = ",
                 metric_names[metric_choices == input$plot_metrics]) 
         ) + 
         scale_x_continuous(labels = label_comma()) +
         theme(
            axis.text.x = element_text(face = "bold", size = 12),
            axis.text.y = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold", size = 18),
            plot.subtitle = element_text(face = "bold", size = 14)
         )
   })
    
   
   # Table showing skew ----
   
   output$ames_skew_tbl <- renderText({
      if(input$skew_tbl == "All Variables") {
         ames_skew %>% 
            kable(format = "html") %>% 
            kable_styling(bootstrap_options = c("striped", "condensed"), full_width = TRUE)
      } else if(input$skew_tbl == "Active Variable") {
         ames_skew %>% 
            filter(
               Variable == feature_names[feature_choices == input$plot_features]
            ) %>% 
            kable(format = "html") %>% 
            kable_styling(bootstrap_options = c("striped", "condensed"), full_width = TRUE)
      }
      
   })
}


shinyApp(ui, server)
