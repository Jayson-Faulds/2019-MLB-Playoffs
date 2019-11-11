# 2019-MLB-Playoffs

Welcome to my first ever Github repository! This is a side project I completed while in graduate school. It attempts to predict the 2019 
MLB postseason by using 2019 regular season data. It uses an ordinal logistic regression model, which generates a probability that a team will fall in a certain class. In this case, the class is the round of the postseason the team will lose in.

There are three files in the repository: The pure R script, called postseason_predictor.R, is the entire R code used to create this project. I have also posted the R markdown file, in case you want to see how I built the PDF/HTML version of the report. Finally, I posted the PDF of the final report as well. Note: the data is accessed internally in the R script through the Lahman Package in R.

When I get time, I would like to dive back into this project and improve it. After speaking with my Machine Learning professor, he suggested that I use a regular logistic regression model instead of an ordinal model, due to an issue of independence between the classes. I would apply the model to each team, for each round. The probabilities from the output would be interpreted as the conditional probability that a team makes it to the next round, given that it has made it to the current round. We would then multiply these conditional probabilities together, grouped by team. This result should give us the probability a team wins the world series. 

Please feel free to give me feedback on this, as this is my first time running this type of model.
Email me at j.faulds711@gmail.com
Thank you!
