
library(shiny)
library(dplyr)

probability = c(30, 9, 15, 10, 4, 4, 2, 1, 4, 2, 10, 4, 4, 1) / 100

first_hextech = c("Silver", "Silver", "Silver", "Gold", "Gold", "Gold", "Prismatic", 
                  "Prismatic", "Silver", "Prismatic", "Gold", "Silver", "Gold", "Prismatic")

second_hextech = c("Silver", "Silver", "Silver", "Silver", "Gold", "Silver", "Silver",
                   "Prismatic", "Prismatic", "Gold", "Silver", "Gold", "Gold", "Silver")

third_hextech = c("Gold", "Prismatic", "Gold", "Gold", "Prismatic", "Prismatic", "Gold",
                  "Prismatic", "Silver", "Gold", "Silver", "Prismatic", "Gold", "Prismatic")

df = tibble(Probability = probability, 
            `First Hextech` = first_hextech, 
            `Second Hextech` = second_hextech, 
            `Third Hextech` = third_hextech) %>%
    arrange(desc(Probability))

print_prob <- function(tab) {
    tab$Probability = paste0(round(100*tab$Probability, 1), "%")
    return(tab)
}

hextech <- function(first = "?", second = "?") {
    
    if(first == "?" & second == "?") {
        df %>%
            print_prob() %>%
            return()
    } else if(second == "?") {
        print(paste0(first, second))
        ret = df %>%
            filter(`First Hextech` == first) %>%
            select(-`First Hextech`)
        print(ret)
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
            print_prob() %>%
            return()
    }
}

ui <- fluidPage(
    
    titlePanel("Hextech Augment"),
    
    sidebarLayout(
        sidebarPanel(
            radioButtons("first", label = h3("First Augment"),
                         choices = list("?" = "?", "Silver" = "Silver", "Gold" = "Gold", "Prismatic" = "Prismatic")),
            radioButtons("second", label = h3("Second Augment"),
                         choices = list("?" = "?", "Silver" = "Silver", "Gold" = "Gold", "Prismatic" = "Prismatic"))
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
