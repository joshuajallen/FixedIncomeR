#
# An example of how to use the event study functions to e.g. investigate
# how spreads perform around bond issuance. The sample data set
# 'spread_data' is entirely fabricated.
#

library(FIRVr)
library(readr)

# Specify some events

event_dates <- readr::read_csv(
  "Series,EventDate
  A,2018-09-07
  A,2018-08-24
  B,2018-10-08
  B,2018-09-10
  B,2018-09-27
  B,2018-12-10"
)

# Create event study data from our timeseries and events

df <- event_study(spread_data, event_dates)

# Plot the results

plot_event_study(df, series = "A")
