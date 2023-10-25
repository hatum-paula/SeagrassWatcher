# Load the required library
library(DT)

# Create the data frame to hold the nodes, their descriptions, and their states
nodes <- data.frame(
  Node = c(
    "Realised Shoot Density", "Realised Biomass", "Aerial Extent", "Time of Year",
    "Genera Presence", "Habitat", "Tropical-Temperate", "Meadow Type", "Location Type",
    "Seed Density", "Recruitment Rate from Seeds", "Seed Quality", "Immigrant Seed Density",
    "Immigrant Seed Quality", "Lateral Growth from Existing Individuals", "Overall Lateral Growth",
    "Immigrant Vegetative Fragments", "Ability to Recover", "Rate of Recovery in Shoot Density",
    "Rate of Recovery in Biomass", "Physiological status of plants", "Above to below-ground biomass ratio",
    "Ability to resist hazard", "Accumulated light", "Accumulated burial", "Sediment Quality",
    "Temperature", "Salinity", "Loss in Shoot Density", "Loss in Biomass",
    "Baseline Shoot Density", "Baseline Biomass", "Net Change Shoot Density",
    "Net Change Biomass", "Heat Stress"
  ),
  Definition = c(
    "Number of shoots, clusters of leaves, or leaf pairs per m2",
    "Grams of dry matter per m2",
    "Meadow area in m2",
    "Month of the year",
    "Categorical, the proportion of meadow of that genera",
    "Categorical, relating to the location of the meadow on the coast",
    "Categorical, whether the meadow is located in tropical or temperate climates",
    "Categorical, whether the meadow is present year-round or not",
    "An aggregate of habitat, tropical-temperate and meadow type",
    "Density of seeds per m^2",
    "Rate of recruitment into the adult population from seeds",
    "Quality score of the seed that affects successful recruitment rate",
    "Capture connectivity between meadows with immigrant seed density",
    "Capture connectivity between meadows with immigrant seed quality",
    "Rate of lateral growth of rhizomes representing all existing individuals in a meadow",
    "Combined lateral growth rate from existing and immigrant individuals",
    "Capture connectivity between meadows with immigrant vegetative fragments",
    "Aggregated score of the ability for the meadow to recover",
    "Rate of recovery in shoot density in that month",
    "Rate of recovery in biomass in that month",
    "The physiological status captures the degree to which the plant can function normally",
    "Represents the ratio of above to below-ground biomass",
    "Aggregate score similar to the ability to recover but focused on the ability to resist a hazard",
    "Probability of meeting light requirements for the normal function of the plant",
    "Probability of experiencing an effect accumulated over a month from burial by sediment",
    "Probability of experiencing an effect accumulated over a month from sediment quality",
    "The suitability temperature-wise to optimal plant function",
    "The suitability, salinity-wise to optimal plant function",
    "Loss in shoot density for that month",
    "Loss in biomass for that month",
    "Best case expected shoot density for a given month",
    "Best case expected biomass for a given month",
    "Probabilistic subtraction of loss in shoot density from realised biomass then adding the rate of recovery in shoot density",
    "Probabilistic subtraction of loss in biomass from realised biomass then adding the rate of recovery in biomass",
    "Probability of experiencing an effect accumulated over a month from suboptimal temperature"
  ),
  States = c(
    "High, moderate, low, zero", "High, moderate, low, zero", "Increase or decrease",
    "January through December", "Zostera", "Inter-tidal, Shallow sub-tidal, Deep sub-tidal",
    "Tropical, Temperate", "Transitory, Enduring", "All combinations of states from habitat, tropical-temperate, and meadow type",
    "High, low, absent", "High, low, absent", "High, low",
    "High, low, absent", "High, low", "Fast, moderate, slow, zero", "Fast, moderate, slow, zero",
    "High, low, zero", "High, low, zero", "High, moderate, low, zero", "High, moderate, low, zero",
    "Good, medium, poor", "More above, more below", "Strong, weak",
    "Above saturation, below saturation", "Effect, no effect", "Effect, no effect",
    "Optimal, sub-optimal", "Optimal, sub-optimal", "High, moderate, low, zero",
    "High, moderate, low, zero", "High, moderate, low, zero", "High, moderate, low, zero",
    "High, moderate, low, zero", "High, moderate, low, zero", "Effect, no effect"
  )
)



