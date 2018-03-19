library(shiny)
library(htmlwidgets)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sortableList <- function(inputId, value) {
    tagList(
        singleton(tags$head(
            tags$script(src = "http://rubaxa.github.io/Sortable/Sortable.js")
        )),
        singleton(tags$head(tags$script(src = "sortableList.js"))),
        tags$div(id = inputId, class = "sortableList list-group",
                 tagList(
                     sapply(value, function(x) {
                         tags$div(class = "list-group-item", "data-id" = x, x)
                     }, simplify = F)
                 ))
    )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(
    # Application title
    titlePanel("Sortable list"),

    sortableList('sortable_list', c(2, 3, 4)),
    sortableList('sortable_list2', c(5, 6, 7)),
    textOutput('test'),
    textOutput('test2')
)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
server <- function(input, output, session) {
    output$test <- renderText({input$sortable_list})
    output$test2 <- renderText({input$sortable_list2})
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shinyApp(ui = ui, server = server)