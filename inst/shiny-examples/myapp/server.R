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
