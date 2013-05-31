if (!require(quantmod)) {
stop("This app requires the quantmod package. To install it, run 'install.packages(\"quantmod\")'.\n")
}
 
# Download data for a stock if needed, and return the data
require_symbol <- function(symbol, envir = parent.frame()) {
if (is.null(envir[[symbol]])) {
envir[[symbol]] <- getSymbols(symbol, auto.assign = FALSE)
}
 
envir[[symbol]]
}
 
 
shinyServer(function(input, output) {
 
# Create an environment for storing data
symbol_env <- new.env()
 
# Make a chart for a symbol, with the settings from the inputs
make_chart <- function(symbol) {
symbol_data <- require_symbol(symbol, symbol_env)
 
chartSeries(symbol_data,
name = symbol,
type = input$chart_type,
subset = paste("last", input$time_num, input$time_unit),
log.scale = input$log_y,
theme = "white")
}
 
output$plot_aapl <- renderPlot({ make_chart("AAPL") })
output$plot_msft <- renderPlot({ make_chart("MSFT") })
output$plot_ibm <- renderPlot({ make_chart("IBM") })
output$plot_goog <- renderPlot({ make_chart("GOOG") })
output$plot_yhoo <- renderPlot({ make_chart("YHOO") })
})