library(rCharts)

shinyUI(pageWithSidebar(
    headerPanel("US Crime in 2013"),
    sidebarPanel(
    p('This web application generates interactive visualizations of crime levels in the United States in 2013. All data was collected from the FBI\'s website. You may enter a State and/or City, then click "Generate Figures" to see visualizations for a specific region. Spelling errors are automatically corrected.'),
    textInput(inputId="text1",label="Enter a US State"),
    textInput(inputId="text2",label="Enter a US City"),
    #checkboxGroupInput("id2","test", c("Use rates per capita" = "1")),
    radioButtons("radio", label = "Crime Format",
                 choices = list("Total number of offences" = 1, "Offenses per capita (times 100000)" = 2), 
                 selected = 1),
    actionButton("goButton", "Generate Figures"),
    p(''),
    textOutput('text1')
    ),
    mainPanel(
        h4(textOutput('text2')),
        htmlOutput('gvis1'),
        p(''),
        showOutput('chart1', lib='nvd3')
    )
))