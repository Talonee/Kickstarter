# Project Proposal
*Ruthvik, Cynthia, & Talon*

## Project Description
Increasingly, crowdfunding is being used by startups -- and even more established companies -- to fund their projects; sometimes quite ambitious ones. We are interested in learning more about the predictors of a successful crowdfunding project, and to this end, we have decided to use [this](https://www.kaggle.com/kemical/kickstarter-projects) robust dataset of Kickstarter projects through 2018. The data was collected by [Mikael Mouille](https://twitter.com/keamlic); a script by [Anton Savchenko](https://www.researchgate.net/profile/Anton_Savchenko) was used to handle USD conversion rates. It is published on Kaggle.

The very broad question we are interested in is, "What makes a Kickstarter project successful?" We intend to answer this using multiple subquestions, namely:

- What are the most popular categories of Kickstarter project over time? Does the popularity of a certain category influence its funding?
- Does the country from which a Kickstarter is launched significantly affect its success (as measured by funding)?
- Does the success rate of a Kickstarter change depending on its goal amount (i.e., are $5,000 Kickstarters more funded more reliably than $100,000) Kickstarters?
- Does the length of a project's funding period affect its funding?

Since we have four proposed visualizations and only three members, one of these questions may be cut.

We think this information could be highly pertinent to *anyone* interested in crowdfunding, but we are specifically interested in the success of young companies. For this reason, we may, for instance, choose to constrain the data such that only mid-sized projects are considered.

## Technical Description

Our data is in a (very large) .csv file; so, not an API. We will be reading it into R using the standard read.csv() function. 

We anticipate that the data will need to be permuted in multiple ways -- for instance, for Question #2, we are planning to group Kickstarters by country and display their funding status and amount funded in a world map visualization, which will (most likely) require finding latitudes and longitudes for each country. Separately, the data in Question #3 will be a line plot with "Funding Goal" along the x-axis and "% of Funding Reached" along the y-axis. This would require mutating the data to create a column representing the y-axis value. 

Proposed implementations for other plots:
- Determining the relationship between funding and funding period may be done by using difftime() to find the time difference between the "launched" and "deadline" columns; following this, we plan to use a line plot.
- The relative popularity of different categories over time could be shown by counting the number of entries in each category in each year, then displaying them in a line plot with "Time" along the x-axis and "# of entries" along the y-axis. A different color would represent each type of Kickstarter. The follow-up subquestion -- "Does the popularity of a certain category influence its funding?" -- could be answered with a bar plot, one bar representing each category and the y-axis representing the average success rate of that category. If we wanted to look at the average success rate in different years, we might introduce multiple bars per category -- say, one for 2010 & one for 2018.

We are also interested in creating a 3D plot, but this is tentative since none of us have worked with one before.

For this project, we will be using Shiny and -- potentially -- Plotly. We anticipate that using Shiny will present some challenges, since none of us have as much experience with it as with, say, ggplot2. 
