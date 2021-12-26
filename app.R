
library(shiny)
library(shinythemes)
library(dplyr)
library(rvest)
library(stringr)

df = read_html("https://leagueoflegends.fandom.com/wiki/Augment_(Teamfight_Tactics)") %>%
    html_node("#mw-content-text > div.mw-parser-output > table") %>%
    html_table() %>%
    .[-1,]

names(df) = c("First Hextech", "Second Hextech", "Third Hextech", "Probability")

df = df %>%
    select(Probability, `First Hextech`, `Second Hextech`, `Third Hextech`) %>%
    mutate(`First Hextech` = str_to_title(`First Hextech`),
           `Second Hextech` = str_to_title(`Second Hextech`),
           `Third Hextech` = str_to_title(`Third Hextech`),
           Probability = str_replace_all(Probability, "%", "") %>%
               as.numeric()/100)

print_prob <- function(tab) {
    tab$Probability = paste0(round(100*tab$Probability, 2), "%")
    return(tab)
}

hextech <- function(first = "?", second = "?") {
    
    if(first == "?" & second == "?") {
        df %>%
            print_prob() %>%
            return()
    } else if(second == "?") {
        ret = df %>%
            filter(`First Hextech` == first) %>%
            select(-`First Hextech`)
        ret$Probability = ret$Probability / sum(ret$Probability)
        ret %>%
            arrange(desc(Probability)) %>%
            print_prob() %>%
            return()
    } else {
        ret = df %>%
            filter(`First Hextech` == first,
                   `Second Hextech` == second) %>%
            select(-`First Hextech`, -`Second Hextech`)
        ret$Probability = ret$Probability / sum(ret$Probability)
        ret %>%
            arrange(desc(Probability)) %>%
            group_by(`Third Hextech`) %>%
            summarise(Probability = sum(Probability)) %>%
            select(Probability, `Third Hextech`) %>%
            print_prob() %>%
            return()
    }
}

ui <- fluidPage(theme = shinytheme("slate"),
    
    titlePanel("Hextech Augment"),
    
    #HTML('<img src="https://media.discordapp.net/attachments/899654736263929857/924161499101032508/unknown.png" alt="">'),
    
    sidebarLayout(
        sidebarPanel(
            radioButtons("first", label = h3("First Augment"),
                         choices = list("?" = "?", "Silver" = "Silver", "Gold" = "Gold", "Prismatic" = "Prismatic")),
            radioButtons("second", label = h3("Second Augment"),
                         choices = list("?" = "?", "Silver" = "Silver", "Gold" = "Gold", "Prismatic" = "Prismatic")),
        ),
        mainPanel(
            tableOutput('table')
        )
    )
)

server <- function(input, output) {
    output$table <- renderTable(hextech(input$first, input$second))
}

shinyApp(ui = ui, server = server)
