
library(dplyr)
library(readr)
library(ggplot2)
library(kableExtra)
library(gtsummary)
library(choroplethr)
library(choroplethrMaps)
library(performance)
library(shiny)

Income <- readr::read_delim("https://raw.githubusercontent.com/catfoodlover/Data607/main/Income.csv", delim = ';', show_col_types = FALSE)
Obese <- readr::read_delim("https://raw.githubusercontent.com/catfoodlover/Data607/main/Obese.csv", delim = ';', show_col_types = FALSE)
Diagnosed <- readr::read_delim("https://raw.githubusercontent.com/catfoodlover/Data607/main/Diagnosed.csv", delim = ';', show_col_types = FALSE)

data("county.regions")
Diagnosed <- rename(Diagnosed, County = "County Name")
Income <- rename(Income, County = "Place of Residence")
Income <- rename(Income, Year = "Tax Year")
Obese <- rename(Obese, County = "County Name")
Income$County <- as.character(Income$County)


Income <-mutate(Income, County=ifelse(County=="New York City - Bronx", "Bronx", County))
Income <-mutate(Income, County=ifelse(County=="New York City - Kings", "Kings", County))
Income <-mutate(Income, County=ifelse(County=="New York City - Manhattan", "New York", County))
Income <-mutate(Income, County=ifelse(County=="New York City - Queens", "Queens", County))
Income <-mutate(Income, County=ifelse(County=="New York City - Richmond", "Richmond", County))


Income09 <- filter(Income, Year == 2009)
Income09 <- mutate(Income09, County = ifelse(County == "new york city", "new york", County))


Obese$County <- tolower(Obese$County)
Income09$County <- tolower(Income09$County)
Diagnosed$County <- tolower(Diagnosed$County)


DiaDiagnosed <- filter(Diagnosed, Diagnosed$"Health Topic" %in% "Cirrhosis/Diabetes Indicators")


DiaDiagnosed <- rename(DiaDiagnosed, value = "Percent/Rate")
Income09 <- rename(Income09, value = "Average NY AGI of All Returns")

Obese <- rename(Obese, value = "Percentage/Rate")

Temp <- filter(county.regions, state.name %in% "new york")

#Income09 <- Income09 %>% mutate(County = as.character(County))

Temp_Obese <- left_join(Temp, Obese, by = c("county.name" = "County"))
Temp_Income <- left_join(Temp, Income09, by = c("county.name" = "County"))
Temp_Diab <- left_join(Temp, DiaDiagnosed, by = c("county.name" = "County"))


#fix missing decimals
Temp_Diab <- Temp_Diab %>% mutate(value = ifelse(value > 50, value/10, value), Input = 'Diabetes')
Temp_Obese <- Temp_Obese %>% mutate(value = ifelse(value > 100, value/10, value), Input = 'Obesity')
Temp_Income <- Temp_Income %>% mutate(Input = 'Income')

Final <- bind_rows(Temp_Diab, Temp_Obese, Temp_Income)

full_data <- bind_rows(Temp_Diab %>% select(region, value, Input), Temp_Obese %>% select(region, value, Input))
full_data <- inner_join(full_data, Temp_Income %>% select(region, income = value))


ui <- navbarPage(title = "NY State Income and Comorbidity Data",
                 tabPanel(title = 'Disease Explorer',
                          plotOutput("plot1"),
                          selectInput('Type', 'Input', choices = unique(Final$Input), selected='Diabetes')
                 ),

                 tabPanel(title = 'Map Explorer',
                          plotOutput("plot2"),
                          selectInput('Type2', 'Input', choices = unique(Final$Input), selected='Diabetes')),

                  tabPanel(title = 'Relation Explorer',
                           plotOutput("plot3"),
                           selectInput('Type3', 'Input', choices = unique(full_data$Input), selected='Diabetes')))




server <- function(input, output, session) {
    mskRvis::set_msk_ggplot()
    data1 <- reactive({
        dfSlice <- Final %>%
            filter(Input == input$Type)
    })
    
    data2 <- reactive({
        dfSlice <- Final %>%
            filter(Input == input$Type2)
    })
    
    data3 <- reactive({
      dfSlice <- full_data %>%
        filter(Input == input$Type3)
    })
    
    
    output$plot1 <- renderPlot({
        
        ggplot(data1(), aes(x = reorder(county.name, desc(value)), y = value)) +
            geom_bar(stat="identity", color=NA, fill = mskRvis::msk_colors["msk_blue"]) + 
        scale_y_continuous(labels = gtsummary::style_number,n.breaks = 7) +
        labs(x = "County") + 
        theme(axis.text.x = element_text(angle = 90))
    })
    
    output$plot2 <- renderPlot({
        
        progress = shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Creating image. Please wait.", value = 0)
        
        county_choropleth(data2(),
                          title      = "2009 New York State Rates",
                          legend     = "% Population",
                          num_colors = 9,
                          state_zoom = c("new york")) + scale_fill_brewer(palette="RdPu")
    })
    
    output$plot3 <- renderPlot({
      
      progress = shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating image. Please wait.", value = 0)
      
      ggplot(data3(), aes(y = value, x = income)) + geom_point() + geom_smooth(method = "loess", se =
                                                                                  TRUE, alpha = 0.2) + labs(title = "Income vs Comorbidity in NY State") + xlab("Mean Taxable Income by County") + ylab("Comorbidity Rate by County")
    })
    
    
    
}



shinyApp(ui = ui, server = server)
