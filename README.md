# COVID-19 Travel Assistant

## Background

In the past two years, the global tourism industry had been significantly impacted by border restrictions due to the COVID-19 pandemic. Now in 2022, as those restrictions are gradually lifted, international travel resumes with a considerable increase of travel plans among the public. However, in a post-pandemic world, travellers usually need to compare an overwhelming amount of COVID-19 data and government regulations to determine whether their destination is COVID-safe. This leads to an increasing demand for travel suggestions and recommendations to make this process easier. Although many websites offer specific information about COVID-19 and travelling, travellers often want to receive quick and simple suggestions rather than complicated data points and statistics.

Therefore, the aim of this project is to provide a simple travel recommendation system for travellers, via building a predictive model which estimates the risk level of a country or district with regards to COVID-19 safety.

## Usage
The final product is a COVID-19 Travel Assistant Shiny app that aims to assist travellers to make informed decisions about their next overseas trip. This app has three main features to guide users navigate the overwhelming and often conflicting COVID-19 related information available online.

1. Destination Explorer: Series of interactive map plots that describe the vaccinations, new cases and restrictions rates across the world. Users can interact with these plots and understand the various COVID-19 related risks and policies that may affect their trip. Once users have explored these plots and decided which destinations are safe, they can shortlist destinations that they are interested in visiting.

2. Travel Advice: Once the user has shortlisted destinations of interest, they can view travel advice for these destinations. The recommendations are generated from the logistic model in part A. Along with a travel recommendation, a 6-month trend of new cases and vaccinations is also displayed so that users can compare the safety of various destinations.

3. Personalised Recommendations: If the user is not sure where to begin their search for their next overseas destination, a list of recommended countries are displayed - ranked by safety rating. This eliminates the challenge of selecting

