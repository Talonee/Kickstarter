library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)

data <- read.csv("data/ks-data.csv", stringsAsFactors = F)

data$date <- sapply(strsplit(data$launched, " "), head, 1)

cleaned <- data %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

cleaned$year <- as.numeric(format(cleaned$date, "%Y"))

latest_data <- cleaned %>% filter(year != 1970)

choose_maincateg <- function(input_name){
  selectInput(input_name, 
              label = "Choose the Main Category",
              choices = unique(latest_data$main_category))
}
  
choose_categ <- selectInput("category", label = "Choose the Category",
                            choices = unique(latest_data$category))

ui <- navbarPage(
  theme = shinytheme("sandstone"),
  "Kickstarter Success",
  tabPanel(
    "Overview",
    tags$h2("What Makes a Successful Kickstarter?"),
    tags$em("Ruthvik, Cynthia, & Talon"),
    p(),
    p(),
    tags$p("Crowdfunding: a relatively new concept, but progressively more
             popular, and powerful. GoFundMe is used for everything from
             plastic surgery to funeral arrangements, Patreon pays the bills of
             numerous
             online creators, and Facebook charity fundraisers are onmipresent.
             Perhaps most influential of all is Kickstarter, which -- with the
             success of
             electronics like the Pebble, and video games like Pillars of
             Eternity --
             is an increasingly legitimate avenue for small businesses to reach
             large audiences."),
    p(),
    tags$p("But: what", tags$em("makes"), "a Kickstarter successful?
              No doubt, this is an important question for startups &
              other small businesses whose dreams ride on an algorithm
              and the generosity of an anonymous audience. Is it possible
              to ensure -- or at least increase -- the likelihood of
              success? Let's find out."),
    p(),
    tags$h3("Our Data"),
    tags$p("We are using a very large dataset -- more than 300,000 rows -- of
              Kickstarter data, published on Kaggle by Mikael Mouille, with
              assistance from Anton Savchenko. While the data is technically
              through 2018, it was collected early in the year, so the last
              complete year of data is 2017. Using this dataset, we hope to
              answer the following."),
    tags$h3("Questions:"),
    tags$li("What types -- categories -- of Kickstarter are most popular in
               each year? Does a category's popularity by number of projects
               effect its funding?"),
    tags$li("Does the country from which a Kickstarter is launched
              significantly affect its project success rate 
            (as measured by funding)?"),
    tags$li("Are the number of backers or the amount pledged affected by the
            time duration of the project?")
  ),
  tabPanel(
    "Category",
    titlePanel(
      "Kickstarter by Category"
    ),
    sidebarLayout(
      sidebarPanel(
        # interactable goes here
        selectInput(
          inputId = "first_year",
          label = "First year:",
          choices = c("2012", "2013", "2014", "2015", "2016", "2017")
        ),

        selectInput(
          inputId = "second_year",
          label = "Second year:",
          choices = c("2012", "2013", "2014", "2015", "2016", "2017"),
          selected = "2017"
        ),

        radioButtons("med_or_mean",
          "% Funded:",
          c("Mean", "Median"),
          selected = "Median"
        )
      ),
      mainPanel(
        # viz goes here
        plotlyOutput("popularity"),
        p(),
        p(),
        plotlyOutput("success"),
        p(),
        p(),
        p("Kickstarter projects are organized into 15 main categories --
            things like film, games, and food. We are interested in the
            popularity of these categories, and their influence
            on funding status. We wonder: do projects which see high
            amounts of success influence the popularity of projects of that
            category on the platform? Do the most popular types of projects
            also see the most success, or the most reliable success?"),
        p(),
        p(
          "To this end, we have designed two bar graphs. Both separate their
            data by year; we want to know if certain categories on the
            Kickstarter
            platform have increased in popularity over time. The first shows
            the total",
          tags$em("count"), "of projects in that category in that year;
            the second shows the mean
            or median funding % of projects in that category in that year."
        ),
        p(),
        p(
          "Our graphs show some interesting results! As anyone familiar with
             Kickstarter
             knows, some projects are funded tens of thousands of times -- and
             these are
             typically the most well-known and publicized. They also",
          tags$em("significantly"), "skew the mean, or average,
             of our calculations; for example, in 2017, Music-type projects
             were funded
             an", tags$em("average"), "of 779%, but only 27% by median. It is
             very common to see extremely high averages
             like this; however, medians cap out at around 100%."
        ),
        p(),
        p(
          "Interestingly, more popular (by count) categories often have lower
             median funding percentages. This makes sense if we think about it:
             if there are more projects, it's likely that funding for these
             projects is more competitive; however, this does suggest that
             creators are not thinking about market saturation when they
             announce their products. Some of the", tags$em("least"),
          "popular categories -- Theater, Comics, and Dance -- have the
             highest median funding percentages, hovering between 50% and
             100%; by comparison, Technology-type projects
             achieve under 5% median funding in the majority of years --
             and achieved a high of 30% in 2012,
             when Technology was much less popular on Kickstarter."
        ),
        p(),

        p(
          "There are many reasons more popular categories like Film and Technology
             see less success. Market saturation is a concern, but also, these types
             of projects are usually much more expensive
             than the typical Comic-type project
             (", textOutput("technology", inline = T), "; ",
          textOutput("comics", inline = T), "). Creators would be prudent to
             note this before launching an expensive,
             high-risk product in a saturated category."
        )
      )
    )
  ),
  tabPanel(
    "Country",
    titlePanel(
      "Kickstarter by Country"
    ),
    sidebarLayout(
      sidebarPanel(
        style = "position:fixed;width:30%;",
        checkboxInput(
          "usa",
          label = strong("Include the United States of America*"),
          value = TRUE
        ),
        radioButtons(
          "sum",
          label = strong("Sum:"),
          choices = list(
            "Number of backers" = "backers",
            "Amount pledged (in USD)" = "pledged",
            "Goal amount (in USD)" = "goal",
            "Total projects" = "tote"
          ),
          selected = "backers"
        ),
        radioButtons(
          "mean",
          label = strong("Mean:"),
          choices = list(
            "Number of backers" = "backers",
            "Amount pledged (in USD)" = "pledged",
            "Goal amount (in USD)" = "goal"
          ),
          selected = "backers"
        ),
        tags$em("*In order to make a more compelling argument,
           the United States of America has the option to be
           disqualified as its data values are so large that
           other countries often become obscure and difficult to
           identify, which makes the statistics less useful.")
      ),
      mainPanel(
        h2("Kickstarter Sum Statistic between Countries"),
        plotlyOutput("sumplot"),
        p(strong("Include USA: "), "The US dominates all other countries by
             an astronomical margin in terms of total backers, amount pledged,
             and goal amount needed for the project."),
        p(strong("Non USA: "), "Other first countries such as UK, Canada and
             Australia also hold the highest spots across all source for
             total funding."),
        p(strong("Hypothesis: "), "First world countries has the ability to
             invest a lot more in funding, which translates to higher
             success rates for projects."),
        p(),
        p(),
        h2("Kickstarter Mean Statistic between Countries"),
        plotlyOutput("meanplot"),
        p(strong("Hypothesis: "), "Countries with higher average support
          sources means their projects are generally more desired,
          which means there are higher success rates for projects."),
        p(),
        p(),
        h2("Kickstarter Funding Ratio"),
        plotlyOutput("percent"),
        p("Hong Kong, Austria, and Singapore are the top contenders for having
            the highest funding relatives to their projected goals"),
        p(strong("Hypothesis: "), "Countries with higher funding ratio means
          there are more projects with more than enough resources needed to
          accomplish their goals, which translates to higher success rates."),
        h2("To Answer the Question?"),
        h4("Does the country from which a Kickstarter is launched significantly
           affect its project success rate (as measured by funding)?"),
        plotlyOutput("highest"),
        p(
          "The data presented are interesting to say the least, different countries
          excel in different areas, they all seem to have an equal potential source
          of what, thereotically, would produce the highest rates of successful
          projects."
        ),
        p(h3("Conclusion: ")),
        p(
          "The answer varies. Data for mean statistic proves no real arguments as
          it fluctuates greatly across countries. In the case of Singapore and Hong 
          Kong, countries with higher fulfillment rate in their funding ratio
          have high success rates. In the case of the US and UK, countries with ",
          tags$em("more"), " resources (higher number of backers, amount
          pledged relative to goal, number of overall planned projects, etc.)
          have high success rates. In the case of Japan, countries with fewer
          projects to begin with have higher chance to boost their overall success
          rate. No single country dominates across all data enough to determine a 
          definite answer. So to reitirate, the country origin do not determine a 
          project's success rate, there are many other factors to consider that 
          affect the success rate of a project: government funding, advertisement,
          country economy, backers, audience interest, etc."
        )
      )
    )
  ),
  # ruthvik's viz
  tabPanel(
    "3D Plot",
    titlePanel("Determining the Pledged Amount"),
    sidebarLayout(
      sidebarPanel(
        choose_maincateg("main_categ2"),
        
        strong("Overall Interprepretation"),
        
        p("Looking at the relations between the three variables for different
        categories confirms us that there is no relation between the Number
        of Backers or the Pledged Amount and the Time taken for project 
        completion. So, the backers are really interested in the outcome of 
        the project rather than the early completion of them. Hence the 
        students need not be thinking only about quick projects which gives 
        them better scope to apply their knowledge and expertise."),
        
        p("But it is interesting to note that in every category, Number
        of Backers and the Pledged Amount (USD) are positively correlated. So, 
        attracting more backers can provide students with more funds. This might
        also mean that having low number of backers brings only little funds. But
        that is not the cvase with every project as there are projects with low 
        number of backers but high investments. So we need to explore more variables
        using better statistical analysis techniques to get more insight into the 
          relation between Number of Backers and Pledged Amount (USD)")
      ),
      mainPanel(
        strong("Description"),
        textOutput("description"),
        plotlyOutput("threed"),
        strong("Summary"),
        textOutput("summary")
      )
    )
  ),
  tabPanel (
    "Data Insights",
    sidebarLayout(
      mainPanel(
        style = "overflow-y:scroll; max-height: 1000px",
        h4("Here is a convenient summary table that allows you to
               filter individual projects. It helps the user to look at
               specific important things like projects with zero backers
               or projects with lowest pledged amount"),
        h4(strong("Kickstarter Projects (2009-2018)")),
        tableOutput("table")
      ),
      sidebarPanel(
        choose_maincateg("main_categ1"),
        sliderInput("year", label = "Years of Interest",
                    value = range(latest_data$year),
                    min = range(latest_data$year)[1],
                    max = range(latest_data$year)[2]),
        sliderInput("backers", label= "Number of Backers",
                    value = range(latest_data$backers),
                    min = range(latest_data$backers)[1],
                    max = range(latest_data$backers)[2]),
        sliderInput("pledged", label = "Amount Pledged",
                    value = range(latest_data$usd_pledged_real),
                    min = range(latest_data$usd_pledged_real)[1],
                    max = range(latest_data$usd_pledged_real)[2]),
        sliderInput("goal", label = "Goal Amount",
                    value = range(latest_data$usd_goal_real),
                    min = range(latest_data$usd_goal_real)[1],
                    max = range(latest_data$usd_goal_real)[2])
      )
    )
  )
)
