 Social Inflation Expectation
A project to analyse the effect of networks on inflation expectations

## Preparation Files
Here are the files we need to shape the data in the right format so they can run from a1-a3.
Run these first to set up all the datasets needed for the codes below.


### pre: Shape collected data for different geographical units into the same format so it can be used in the main codes
So far I am working on EU and US, FRBNY and Michigan data


# Preparation Files

## Inflation Data

### b1: Allocate inflation timeseries to each CZ2000
Using regional US Bureau of Labour Statistics Data

### b2: Allocate inflation timeseries to each CZ2000
Using State level data from Hazell 2022

## Aligning data code

### pre: Shape collected data for different geographical units into the same format so it can be used in the main codes
So far I am working on EU and US, FRBNY and Michigan data

## Main Codes

### a: Panel Regression

#### 1 : Indices
Creates the indices needed in the regression using social and physical proximity between geographical units and their inflation expectations

#### 2: Combines all variables to allow for a panel regression
Creates a combined dataset and runs a number of regression specifications

### b: Case Study
Identifies inflation events and checks how connectedness to this region impacts expectations.
This analysis has not been written.

### c: Further analysis
Want to look at questions like: 
1. Are biased commuting zones more connected?
2. Is outwardness indicative of expectations?
3. Can we preidct the variance of expectations

