library(dplyr)
library(reshape2)
library(reshape)
library(lubridate)
library(plotly)
library(shiny)

options(warn = -1)

server <- function(input, output) {
    data <- read.csv("data/ks-data.csv", stringsAsFactors = F)
    
    data$date <- sapply(strsplit(data$launched, " "), head, 1)
    
    cleaned <- data %>%
      mutate(date = as.Date(date, format = "%Y-%m-%d"))

    cleaned$year <- as.numeric(format(cleaned$date, "%Y"))
    
    cleaned <- cleaned %>%
      filter(year > "2011" & year <= "2017")
    
    popularity <- cleaned %>%
      group_by(year, main_category) %>%
      count() %>%
      arrange(year)
    
    shaped_popularity <- dcast(popularity, main_category ~ year)
    
    colnames(shaped_popularity) <- c("category",
                                     "proj_2012",
                                     "proj_2013",
                                     "proj_2014",
                                     "proj_2015",
                                     "proj_2016",
                                     "proj_2017")
    
    output$popularity <- renderPlotly({
      p <- plot_ly(shaped_popularity, 
                 x = ~category,
                 y = ~get(paste0("proj_", input$first_year)),
                 name = input$first_year, 
                 type = "bar", color = I("#52AA5E")) %>%
      add_trace(y = ~get(paste0("proj_", input$second_year)),
                name = input$second_year, color = I("#388659")) %>%
      layout(title = "Popularity of Kickstarter Categories", 
             xaxis = list(title = "Category"), 
             yaxis = list(title = "# of Projects"))
    })
    
    success <- cleaned %>%
      mutate(pct_funded = (as.numeric(usd_pledged_real) / 
               as.numeric(usd_goal_real) + 0.000001) * 100) %>%
      group_by(year, main_category) %>%
      summarise(Mean = mean(pct_funded), Median = median(pct_funded))
    
    output$success <- renderPlotly({
      success <- success %>%
        select(main_category, year, input$med_or_mean)
      
      shaped_success <- dcast(success, main_category ~ year)
      
      colnames(shaped_success) <- c("category", 
                                    "pct_funded_2012",
                                    "pct_funded_2013",
                                    "pct_funded_2014",
                                    "pct_funded_2015",
                                    "pct_funded_2016",
                                    "pct_funded_2017")
      
      p <- plot_ly(shaped_success, 
                   x = ~category, 
                   y = ~get(paste0("pct_funded_", input$first_year)),
                   name = input$first_year,
                   type = "bar", color = I("#779CAB")) %>%
        add_trace(y = ~get(paste0("pct_funded_", input$second_year)),
                   name = input$second_year, color = I("#35524A")) %>%
        layout(title = paste(input$med_or_mean, "% Funded per Category"),
               xaxis = list(title = "Category"),
               yaxis = list(title = paste(input$med_or_mean, "% Funded")))
      
    })
    
    goal_info <- function(category) {
      avg_by_category <- cleaned %>%
        group_by(main_category) %>%
        summarise(Mean = mean(usd_goal_real), Median = median(usd_goal_real))
      
      avg <- avg_by_category[avg_by_category$main_category == category, "Mean"]
      med <- avg_by_category[avg_by_category$main_category == category, "Median"]
      
      phrase <- paste0("mean, median goal for ", category, ": $", round(avg), ", $", round(med))
      phrase
    }
    
    output$technology <- renderText({ goal_info("Technology") })
    output$comics <- renderText({ goal_info("Comics") })
}