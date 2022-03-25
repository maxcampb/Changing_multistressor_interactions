# **Interactions among multiple stressors vary with exposure duration and biological response**

## _Outline:_

Using an experimental study, we present an approach for analysing regression-based designs with generalised additive models (GAMs) that allowed us to capture non-linear effects of exposure duration and stressor intensity, and access interactions among stressors.

## _Aims:_

The aims of this research were to; 1) predict the non-linear effects of diuron exposure, DIN enrichment and reduced light on two response variables, photosynthesis and growth; 2) identify how interaction types for the two response variables vary over different exposure durations; and, 3) compare the interaction types for the two response variables at the same experimental treatment levels. To address the aims, we used an experimental study and applied a novel approach to access stressor interaction types. This was done by integrating a regression-based blocks design with continuous non-linear modelling, which subsequently translates the non-linear effects into a standard interaction-type classification. This allowed us to assess how interactions change with experimental context. The innovation of this study is in the integration of the experimental design with the statistical framework for non-linear analysis.

***

## _Github repository files:_

1. Data - contains all experimental data (i.e. four replicates each of chlorophyll-a fluorescence and absorbance (cell density) data).
2. Output - contains various GAM, main effects, interactions and model validation figures.
3. Scripts - contains the R-scripts that produce all output figures.

***

## _Role of each script:_

**01A_DIN_growth_analysis:** Step one script for processing the data, fitting the GAM, and finding the stressor interactions for the DIN experiment with the growth endpoint.

**01B_DIN_photosynthesis_analysis:** Step one script for processing the data, fitting the GAM, and finding the stressor interactions for the DIN experiment with the photosynthetic endpoint.

**01C_Diuron_growth_analysis:** Step one script for processing the data, fitting the GAM, and finding the stressor interactions for the diuron experiment with the growth endpoint.

**01D_Diuron_photosynthesis_analysis:** Step one script for processing the data, fitting the GAM, and finding the stressor interactions for the diuron experiment with the photosynthetic endpoint.

**02_interaction_plotting_all_models:** Step two script for plotting the interaction results of both experiments (DIN and diuron) for both endpoints (growth and photsynthesis).

**02_main_effects_plotting_all_models:** Step two script for plotting the GAM main effects of both experiments (DIN and diuron) for both endpoints (growth and photsynthesis).

**2021-05-13_average_specific_growth_rate_plots:** Script that processes the raw absorbance data to 'growth' by calculating the percentage inhibition for the growth endpoint.

**model_validation_investigation:** Script with the investigation of the our approaches ability to capture know stressor interactions.

**bayes_CI_functions:** Functions used for computing the empirical bayesian 95% credible intervals, which is adapted from the mgcv documentation.

**model_validation_functions:** Functions used in the model validation investigation.

***

## _Information for R software used:_

Data processing and analyses was carried out using R statistical software version 4.1.0.

Packages used: mgcv (version 1.8.36), rlang (version 1.0.1), tidyverse (version 1.3.1), visreg (version 2.7.0), ggplot2 (version 3.3.4), patchwork (version 1.1.1) and readxl (version 1.3.1). 

**Data also available via Dryad DOI: https://doi.org/10.5061/dryad.6m905qg22**
