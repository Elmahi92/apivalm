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
