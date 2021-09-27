library(shiny)
library(tidyverse)
library(apivalm)

df <- valmyndigheten_get(c(2010, 2014, 2018), c("riksdag", "county", "municipal"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Title panel
    titlePanel("Swedish elections"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(

            radioButtons(
                inputId = "no_pct",
                label = "Show:",
                choiceNames = c("Number of votes", "Percentage"),
                choiceValues = c(" no", " pct")
            ),

            checkboxGroupInput(
                inputId = "selectYears",
                label = "Select year(s):",
                choices = unique(df$Year)
            ),

            checkboxGroupInput(
                inputId = "selectElection",
                label = "Select type of election(s):",
                choices = unique(df$`Election type`)
            ),

            checkboxGroupInput(
                inputId = "selectCounty",
                label = "Select counties:",
                choices = unique(df$County)
            ), width = 2

        ),

        # Main panel
        mainPanel(
            plotOutput("hist"),
            tableOutput("data"), width = 10
        )
    )
)

# Server
server <- function(input, output) {

    output$hist <- renderPlot({
        req(length(input$selectElection) > 0 & length(input$selectCounty) > 0 & length(input$selectYears) > 0,
            cancelOutput = TRUE)
        df[,c(1:2, 5:6, grep(input$no_pct, colnames(df)))] %>%
            rename_with(~str_remove(., input$no_pct)) %>%
            filter(`Election type` %in% input$selectElection) %>%
            filter(County %in% input$selectCounty) %>%
            filter(Year %in% input$selectYears) %>%
            gather(Party, Votes, -c(Year, `Election type`, County, Municipality)) %>%
            ggplot(aes(x = Party, y = Votes, fill = County)) +
            geom_bar(position = "dodge", stat="identity") +
            facet_wrap(.~Year+`Election type`, ncol = 3)

            })

    output$data <- renderTable({
        df[,c(1:2, 5:6, grep(input$no_pct, colnames(df)))] %>%
            filter(`Election type` %in% input$selectElection) %>%
            filter(County %in% input$selectCounty) %>%
            filter(Year %in% input$selectYears) %>%
            rename_with(~str_remove(., input$no_pct))
    }, rownames = FALSE, digits = 0)


}

# Run the application
shinyApp(ui = ui, server = server)
