# Project Proposal
author: Ruthvik, Cynthia, & Talon
date: March 1, 2019

## Project Description
Increasingly, crowdfunding is being used by startups -- and even more established companies -- to fund their projects; sometimes quite ambitious ones. We are interested in learning more about the predictors of a successful crowdfunding project, and to this end, we have decided to use [this](https://www.kaggle.com/kemical/kickstarter-projects) robust dataset of Kickstarter projects through 2018. The data was collected by [Mikael Mouille](https://twitter.com/keamlic), a script by [Anton Savchenko](https://www.researchgate.net/profile/Anton_Savchenko) was used to handle USD conversion rates. It is published on Kaggle.

The very broad question we are interested in is, "What makes a Kickstarter project successful?" We intend to answer this using multiple subquestions, namely:

- What are the most popular categories of Kickstarter project over time? Does the popularity of a certain category influence its funding?
- Does the country from which a Kickstarter is launched significantly affect its success (as measured by funding)?
- Does the success rate of a Kickstarter change depending on its goal amount (i.e., are $5,000 Kickstarters more funded more reliably than $100,000) Kickstarters?
- Does the length of a project's funding period affect its funding?

We think this information could be highly pertinent to *anyone* interested in crowdfunding, but we are specifically interested in the success of young companies. For this reason, we may, for instance, choose to constrain the data such that only mid-sized projects are considered.

## Technical Description

Our data is in a (very large) .csv file; so, not an API. We will be reading it into R using the standard read.csv() function. 

We anticipate that the data will need to be permuted in multiple ways -- for instance, for Question #2, we are planning to group Kickstarters by country and display their funding status and amount funded in a world map visualization, which will (most likely) require finding latitudes and longitudes for each country. Separately, the data in Question #3 will be a line plot with "Goal" along the x-axis and "Success Rate" along the y-axis; to do this, it is neccesary to compare each Kickstarter's *goal* with its *actual funding* and then develop a predictive line.

For this project, we will be using Shiny and -- potentially -- Plotly. We anticipate that using Shiny will present some challenges, since none of us have as much experience with it as with, say, ggplot2. 
