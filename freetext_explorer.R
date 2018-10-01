
library(shiny)
library(ggplot2)
library(dplyr)

load("180903_wordratios.rda")

# Make pretty names for filter boxes

ctchoice <- unique(wordratios$ct)
names(ctchoice) <- gsub("cat_","",unique(wordratios$ct))

outchoice <- unique(wordratios$out)
names(outchoice) <- c("Amb - Transport", "Amb - Prio 1 in", "Amb - Läkemedel", "Amb - Syrgas", "Amb - EKG Skickat", 
                      "Amb - Immobilisering", "Amb - PVK etablerad", "Amb - Förband", "Amb - HLR utförd", "Amb - Kritisk patient", 
                      "Amb - Förvarning", "Amb - Någon", "NEWS >= 7", "NEWS <= 4", "Cosmic - Akutmottagning", "Cosmic - Inskrivning",
                      "Cosmic - Intensivvård", "Cosmic - Radiologi", "Cosmic - Kirurigi", "Cosmic - 3-dagars mortalitet")

# Function for making pretty axis labels

scalepct <- function(x) paste0(round(as.numeric(x)*100,3),"%")

# Define UI
ui <- fluidPage(
  titlePanel("Uppsala EMD free-text explorer"),
  
  # Define filter boxes
    fluidRow(
      column(12, align = "center",
        div(style="display: inline-block; vertical-align: top;", selectInput("calltype",
                  "Call Type:",
                  choices = ctchoice,
                  selected = "cat_Full")),
        div(style="display: inline-block; vertical-align: top;",selectInput("outcome",
                  "Outcome:",
                  choices = outchoice,
                  selected = "amb_any")),
        div(style="display: inline-block; vertical-align: top;",textInput("text",
                                                      "Match text:",
                                                      value = ""))
      )
  ),
  
  # Define plots
  fluidRow(fluidRow(
             plotOutput("plotMain",
                               brush = brushOpts(
                                 id = "plot_brush",
                                 resetOnNew = TRUE
                               )
                    ),
             plotOutput("plotZoom")
             )
           )
)

# Define server functions
server <- function(input, output) {
  
  #Zoom range variable
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  #Function to define filtered datasets
  dfInput <- reactive({
    wordratios %>%
      filter(ct == input$calltype,out == input$outcome) %>%
      mutate(match = grepl(input$text,term)) %>%
      arrange(match)
  })
  
  #Plot main (top) graph
  output$plotMain <- renderPlot({
    df <- dfInput()
    ggplot(df, aes(`0`, `1`, color = match)) +
      geom_point() +
      scale_x_log10(labels = scalepct) +
      scale_y_log10(labels = scalepct) +
      geom_abline(color = "red") +
      scale_color_manual(values = c("black","red"), guide=FALSE) +
      theme(plot.margin = margin(rep(20,4)),
            text = element_text(size = 18)) +
      labs(title = "Draw a rectangle on this plot...",
           x = "",
           y = "Term ocurrance in calls with outcome")
  })
  
  #Plot zoomes text graph
  output$plotZoom <- renderPlot({
    
    df <- dfInput()
    ggplot(arrange(df,desc(match)), aes(`0`, `1`, color = match)) +
      geom_text(aes(label = term), size = 6, check_overlap = T,
                position = position_jitter(height = 0.01)) +
      scale_x_log10(labels = scalepct) +
      scale_y_log10(labels = scalepct) +
      geom_abline(color = "red") +
      scale_color_manual(values = c("black","red"), guide=FALSE) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
      theme(plot.margin = margin(rep(20,4)),
            text = element_text(size = 18)) +
      labs(title = "... To zoom in this plot.", 
           x = "Term ocurrance in calls without outcome",
           y = "Term ocurrance in calls with outcome",
           caption = "Red line represents terms with equal prevalence in calls with and without outcome")
  })
  
  # When a click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

