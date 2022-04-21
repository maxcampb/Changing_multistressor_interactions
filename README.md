# **Interactions among multiple stressors vary with exposure duration and biological response**

This README file was finalised on 2022-04-21 by Olivia King.

## _1. GENERAL INFORMATION:_

## _1.1. Title of dataset:_

Data from: Interactions among multiple stressors vary with exposure duration and biological response

## _1.2. Author information:_

Corresponding author:
Name: Olivia C. King 
Institution: Coastal and Marine Research Centre, Australian Rivers Institute, School of Environment and Science, Griffith University, Gold Coast, Queensland, Australia 
Email: olivia.king@griffithuni.edu.au

Co-author 1: Jason P. van de Merwe
Institution: Coastal and Marine Research Centre, Australian Rivers Institute, School of Environment and Science, Griffith University, Gold Coast, Queensland, Australia 
Email: j.vandemerwe@griffith.edu.au

Co-author 2: Max D. Campbell
Institution: Coastal and Marine Research Centre, Australian Rivers Institute, School of Environment and Science, Griffith University, Gold Coast, Queensland, Australia 
Email: maxcampbe@gmail.com

Co-author 3: Rachael A. Smith
Institution: Office of the Great Barrier Reef, Department of Environment and Science, Queensland Government, Brisbane, Queensland, Australia
Email: rachael.smith@des.qld.gov.au

Co-author 4: Michael St. J Warne
Institution: Water Quality and Investigations, Queensland Department of Environment and Science, Brisbane, Queensland, Australia
Email: michael.warne@uq.edu.au

Co-author 5: Christopher J. Brown
Institution: Coastal and Marine Research Centre, Australian Rivers Institute, School of Environment and Science, Griffith University, Gold Coast, Queensland, Australia 
Email: chris.brown@griffith.edu.au

## _1.3. Date of collection:_

2019-2021

## _1.4. Geographical location of data collection:_

Gold Coast, Queensland, Australia

## _1.5. Funding:_

Olivia C. King was supported by an Australian Government Research Training Program Stipend Scholarship. Christopher J. Brown and Max D. Campbell were supported by a Discovery Project grant (DP180103124) from the Australian Research Council.

## _1.6. Recommended citation for this dataset:_

King, Olivia et al. (2022), Interactions among multiple stressors vary with exposure duration and biological response, Dryad, Dataset, https://doi.org/10.5061/dryad.6m905qg22

## _2. DATA AND FILE OVERVIEW:_

## _2.1. Outline:_

Using an experimental study, we present an approach for analysing regression-based designs with generalised additive models (GAMs) that allowed us to capture non-linear effects of exposure duration and stressor intensity, and access interactions among stressors.

## _2.2. Aims:_

The aims of this research were to; 1) predict the non-linear effects of diuron exposure, DIN enrichment and reduced light on two response variables, photosynthesis and growth; 2) identify how interaction types for the two response variables vary over different exposure durations; and, 3) compare the interaction types for the two response variables at the same experimental treatment levels. To address the aims, we used an experimental study and applied a novel approach to access stressor interaction types. This was done by integrating a regression-based blocks design with continuous non-linear modelling, which subsequently translates the non-linear effects into a standard interaction-type classification. This allowed us to assess how interactions change with experimental context. The innovation of this study is in the integration of the experimental design with the statistical framework for non-linear analysis.

## _2.3. Description of dataset:_

These data were generated to investigate the physiological effects of varying concentrations/levels of diuron, dissolved inorganic nitrogen (DIN) and reduced light availability on marine microalga, _Phaeodactylum tricornutum_. Specifically, we tested how the simultaneous acute and chronic exposures of varying levels of stressor intensity affect the physiological responses of _P. tricornutum_. Individual, combined and interactive effects on photosynthetic yield (chlorophyll-a fluorescence) and growth rate (cell density) were measured over a 72-hour exposure period.

## _2.4. Repository file list and descriptions:_

**DATA FILES**

Data File 1: DIN_light_PSII_long_new.csv

Data File 1 Description: Contains all experimental raw data (i.e. four replicates) for the responses of DIN and light exposure on chlorophyll-a fluorescence.


Data File 2: diuron_light_PSII_long_NEW.csv

Data File 2 Description: Contains all experimental raw data (i.e. four replicates) for the responses of diuron and light exposure on chlorophyll-a fluorescence.


Data File 3: DIN_growth_model_and_data2.RDA

Data File 3 Description: All experimental data (i.e. four replicates) for the responses of DIN and light exposure on absorbance (cell density), plus the model.


Data File 4: Diuron_growth_model_and_data2.RDA

Data File 4 Description: All experimental data (i.e. four replicates) for the responses of diuron and light exposure on absorbance (cell density), plus the model.


Data File 5: DIN_photo_model_and_data2.RDA

Data File 5 Description: Also contains experimental data (i.e. four replicates) for the responses of DIN and light exposure on chlorophyll-a fluorescence, plus the model.


Data File 6: Diuron_photo_model_and_data2.RDA

Data File 6 Description: Also contains experimental data (i.e. four replicates) for the responses of diuron and light exposure on chlorophyll-a fluorescence, plus the model.

**OUTPUT FILES**

Output File 1: 01A_DIN_growth_analysis.R

Output File 1 Description: Step one script for processing the data, fitting the GAM, and finding the stressor interactions for the DIN experiment with the growth endpoint.


Output File 2: 01B_DIN_photosynthesis_analysis.R

Output File 2 Description: Step one script for processing the data, fitting the GAM, and finding the stressor interactions for the DIN experiment with the photosynthetic endpoint.


Output File 3: 01C_Diuron_growth_analysis.R

Output File 3 Description: Step one script for processing the data, fitting the GAM, and finding the stressor interactions for the diuron experiment with the growth endpoint.


Output File 4: 01D_Diuron_photosynthesis_analysis.R

Output File 4 Description: Step one script for processing the data, fitting the GAM, and finding the stressor interactions for the diuron experiment with the photosynthetic endpoint.


Output File 5: 02_interaction_plotting_all_models.R

Output File 5 Description: Step two script for plotting the interaction results of both experiments (DIN and diuron) for both endpoints (growth and photsynthesis).


Output File 6: 02_main_effects_plotting_all_models.R

Output File 6 Description: Step two script for plotting the GAM main effects of both experiments (DIN and diuron) for both endpoints (growth and photsynthesis).


Output File 7: 2021-05-13_average_specific_growth_rate_plots.R

Output File 7 Description: Script that processes the raw absorbance data to 'growth' by calculating the percentage inhibition for the growth endpoint.


Output File 8: model_validation_investigation.R

Output File 8 Description: Script with the investigation of the our approaches ability to capture know stressor interactions.


Output File 9: model_validation_functions.R

Output File 9 Description: Functions used in the model validation investigation.


Output File 10: bayes_CI_functions.R

Output File 10 Description: Functions used for computing the empirical bayesian 95% credible intervals, which is adapted from the mgcv documentation.

## _2.5 Data specific information:_
All toxicity tests were performed on four separate occasions (‘blocks’), using independent algae cultures.

Number of replicates (blocks) for each experiment: 4

Number of pseudo-replicates taken from each treatment: 2

For the DIN experiments, a total of nine flasks were used per block, containing two NH4Cl treatments (2.76 and 27.6 mg L-1) plus an algae control, each at three light levels of 5, 20 and 80 μmol photons m-2 s-1 (n = 36).

For the diuron experiments, 18 flasks were used per block, containing four diuron concentrations (0.1, 0.3, 1 and 3 µg L-1) plus a methanol control (at 0.08%) and an algae control, each at three light levels of 5, 20 and 80 μmol photons m-2 s-1 (n = 72).

Missing data codes: NA

## _2.6 Information for R software used:_

Data processing and analyses was carried out using R statistical software version 4.1.0.

Packages used: mgcv (version 1.8.36), rlang (version 1.0.1), tidyverse (version 1.3.1), visreg (version 2.7.0), ggplot2 (version 3.3.4), patchwork (version 1.1.1) and readxl (version 1.3.1). 

**Data also available via Dryad DOI: https://doi.org/10.5061/dryad.6m905qg22**

## _3. METHODOLOGICAL INFORMATION:_

This raw dataset was collected through a series of laboratory exposure experiments - see Supplementary Material, section 'S1. Methods for laboratory-based experimental work'.

A novel approach to access stressor interaction types was then carried out, based on the experimental data. This was done by integrating a regression-based block design with continuous non-linear modelling, which subsequently translates the non-linear effects into a standard interaction-type classification.
