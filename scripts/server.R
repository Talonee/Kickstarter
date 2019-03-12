library(dplyr)
library(reshape2)
library(reshape)
library(lubridate)
library(plotly)
library(shiny)
library(jsonlite)

options(warn = -1)

server <- function(input, output) {
  ########################## Cynthia ##########################

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

  colnames(shaped_popularity) <- c(
    "category",
    "proj_2012",
    "proj_2013",
    "proj_2014",
    "proj_2015",
    "proj_2016",
    "proj_2017"
  )

  output$popularity <- renderPlotly({
    p <- plot_ly(shaped_popularity,
      x = ~category,
      y = ~ get(paste0("proj_", input$first_year)),
      name = input$first_year,
      type = "bar", color = I("#52AA5E")
    ) %>%
      add_trace(
        y = ~ get(paste0("proj_", input$second_year)),
        name = input$second_year, color = I("#388659")
      ) %>%
      layout(
        title = "Popularity of Kickstarter Categories",
        xaxis = list(title = "Category"),
        yaxis = list(title = "# of Projects")
      )
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

    colnames(shaped_success) <- c(
      "category",
      "pct_funded_2012",
      "pct_funded_2013",
      "pct_funded_2014",
      "pct_funded_2015",
      "pct_funded_2016",
      "pct_funded_2017"
    )

    p <- plot_ly(shaped_success,
      x = ~category,
      y = ~ get(paste0("pct_funded_", input$first_year)),
      name = input$first_year,
      type = "bar", color = I("#779CAB")
    ) %>%
      add_trace(
        y = ~ get(paste0("pct_funded_", input$second_year)),
        name = input$second_year, color = I("#35524A")
      ) %>%
      layout(
        title = paste(input$med_or_mean, "% Funded per Category"),
        xaxis = list(title = "Category"),
        yaxis = list(title = paste(input$med_or_mean, "% Funded"))
      )
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

  output$technology <- renderText({
    goal_info("Technology")
  })
  output$comics <- renderText({
    goal_info("Comics")
  })


  ########################## Talon ##########################

  dataset <- data %>%
    filter(state == "live" | state == "successful")
  
  # Convert ISO2 to ISO3 and combine into the dataset
  iso_json <- as.data.frame(fromJSON("http://country.io/iso3.json"), stringsAsFactors = F)
  iso_convert <- data.frame("ISO2" = colnames(iso_json), "ISO3" = unname(unlist(iso_json[1, ])))
  dataset <- suppressWarnings(
    left_join(dataset, iso_convert, by = c("country" = "ISO2"))
  )

  # Convert and combine ISO3 into country's full name
  country_names <- fromJSON(
    "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/slim-3/slim-3.json"
  )
  country_names <- country_names %>%
    dplyr::rename(country_full = "name") %>%
    select(country_full, `alpha-3`)
  dataset <- suppressWarnings(
    left_join(dataset, country_names, by = c("ISO3" = "alpha-3"))
  )

  dataset <- dataset %>%
    mutate(country_full = ifelse(is.na(country_full), "Other", country_full)) %>%
    select(
      name, category, main_category, state, backers,
      usd_pledged_real, usd_goal_real, ISO3, country_full
    )
  
  col_scheme <- "Spectral"
  
  # Select relevant features
  sum_by_country <- dataset %>%
    group_by(country_full) %>%
    summarize(
      backers = sum(backers),
      pledged = sum(usd_pledged_real),
      goal = sum(usd_goal_real)
    )

  output$sumplot <- renderPlotly({
    if (!input$usa) {
      sum_by_country <- sum_by_country %>% 
        filter(country_full != "United States of America")
      col_scheme <- "RdPu"
    }
    
    p <- plot_ly(
      sum_by_country,
      x = ~ get(input$sum),
      type = "bar",
      color = ~country_full,
      colors = col_scheme,
      text = ~paste0(
        "Country: ", country_full,
        "\nSum value of: ", get(input$sum)
      )
    ) %>%
      layout(
        title = paste0("Kickstarter Sum Statistic"),
        xaxis = list(title = "Sum Value")
      )
    p
  })

  mean_by_country <- dataset %>%
    group_by(country_full) %>%
    summarize(
      backers = mean(backers),
      pledged = mean(usd_pledged_real),
      goal = mean(usd_goal_real)
    )

  output$meanplot <- renderPlotly({
    if (!input$usa) {
      mean_by_country <- mean_by_country %>% 
        filter(country_full != "United States of America")
      col_scheme <- "GnBu"
    }
    
    p <- plot_ly(
      mean_by_country,
      x = ~ get(input$mean),
      type = "bar",
      color = ~country_full,
      colors = col_scheme,
      text = ~paste0(
        "Country: ", country_full,
        "\nMean value of: ", get(input$mean)
      )
    ) %>%
      layout(
        title = paste0("Kickstarter Mean Statistic"),
        xaxis = list(title = "Mean Value")
      )
  })

  per_met <- dataset %>%
    group_by(country_full) %>%
    summarize(perc = round(sum(usd_pledged_real) / sum(usd_goal_real) * 100, 2))

  output$percent <- renderPlotly({
    if (!input$usa) {
      per_met <- per_met %>% 
        filter(country_full != "United States of America")
      col_scheme <- "OrRd"
    }
    
    plot_ly(
      per_met,
      x = ~perc,
      type = "bar",
      color = ~country_full,
      colors = col_scheme,
      text = ~paste0(
        "Percentage funding: ", perc, "%"
      )
    ) %>%
      layout(
        title = paste0("Funding Ratio of Successful Projects"),
        xaxis = list(title = "Percent")
      )
  })

  success_amt <- dataset %>%
    group_by(country_full) %>%
    summarize(num = n())

  output$highest <- renderPlotly({
    if (!input$usa) {
      success_amt <- success_amt %>% 
        filter(country_full != "United States of America")
      sum_by_country <- sum_by_country %>% 
        filter(country_full != "United States of America")
      col_scheme <- "Greens"
    }
    
    plot_ly(
      success_amt,
      x = ~num,
      type = "bar",
      color = ~country_full,
      colors = col_scheme,
      text = ~paste0(
        "Country: ", country_full,
        "\nTotal successful projects: ", num,
        "\nTotal number of backers: ", sum_by_country$backers,
        "\nTotal amount pledged (in USD): $", sum_by_country$pledged,
        "\nTotal goal amount (in USD): $", sum_by_country$goal
      )
    ) %>%
      layout(
        title = paste0("Number of Successful Projects"),
        xaxis = list(title = "Total Amount")
      )
  })

  ########################## Ruthvik ##########################
  output$table <- renderTable({
    display_data <- cleaned %>%
      filter(main_category == input$main_category, 
             category == input$category, 
             year >= input$year[1] & year <= input$year[2], 
             backers >= input$backers[1] & backers <= input$backers[2], 
             usd_pledged_real >= input$pledged[1] & 
               usd_pledged_real <= input$pledged[2],
             usd_goal_real >= input$goal[1] & 
               usd_goal_real <= input$goal[2]) %>%
      select(name, category, main_category, backers, country, 
             usd_pledged_real, usd_goal_real, launched, deadline)
  })
}
