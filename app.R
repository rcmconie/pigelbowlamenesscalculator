library(shiny)
library(bslib)
library(emmeans)

final <- readRDS("data/OA Model.RDS")

# Define UI for dataset viewer app ----
ui <- page_sidebar(
  
  # App title ----
  title = "Pig Lameness Probability Calculator",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(helpText("Select the values for the pig"),
                    
                    # Input: Selector for pig size ----
                    selectInput(
                      inputId = "Size",
                      label = "Pig Size:",
                      choices = c("Small (<45 kg)"="Small", "Medium (45-79 kg)"="Medium", "Large (80+ kg)"="Large")
                    ),
                    
                    # Input: Selector for pig sex ----
                    selectInput(
                      inputId = "Sex",
                      label = "Pig Sex:",
                      choices = c("Female Intact"="F", "Female Spayed"="FS", "Male Neutered"="MN")
                    ),
                    
                    
                    # Input: Selector for OA severity ----
                    selectInput(
                      inputId = "OASev",
                      label = "Pig OA Severity:",
                      choices = c("None","Mild","Moderate","Severe")
                    )
                    
  ),
  
  # Output: Table ----
  
  card(
    card_header("Estimated Probability and Confidence Interval"),
    htmlOutput("estimate"))
  
)

# Define server logic ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of input values to use in model ----
  pigValues <- reactive({
    data.frame(
      Weight_Cat=as.character(
        input$Size),
      Sex=as.character(input$Sex),
      OA_severity=as.character(input$OASev),
      stringsAsFactors = TRUE
    )
  })
  
  
  output$estimate<-renderText({
    ### use the values
    estout<-as.data.frame(emmeans(final,~Weight_Cat*Sex*OA_severity,
                                  at=list(Weight_Cat=pigValues()$Weight_Cat,
                                          Sex=pigValues()$Sex,
                                          OA_severity=pigValues()$OA_severity),type="response"))
    
    paste0("<p>The estimated probability of lameness is <strong>",format(round(estout$prob,3),nsmall=3),"</strong>; 95% CI = <strong>(",format(round(estout$lower.CL,3),nsmall=3),",",format(round(estout$upper.CL,3),nsmall=3),")</strong>.</p>")
    
  })

}

# Create Shiny app ----
shinyApp(ui, server)