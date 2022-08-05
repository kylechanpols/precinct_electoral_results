# Proof of Concept: Precinct-level Electoral Results Dashboard

This is a Proof of Concept R-Shiny dashboard to display electoral results in a Shiny Dashboard.

This project shows the electoral results in 2020 and 2016 presidential elections by precinct in California to allow campaign managers identify target precincts to mobilize support.

## Data Sources
Precinct-level electoral results and shapefiles from [Voting and Election Science Team (VEST)](https://dataverse.harvard.edu/dataverse/electionscience)/

## How to use

Run `data.R`, then in RStudio, open `app-dynamic.R` and click "run app"

## File Description
- `data.R` - Prepares the data for plotting purposes
- `app-dynamic.R` - The body of the Shiny App.

## Future Development
- If available, incorporate precinct-level demographic data.
