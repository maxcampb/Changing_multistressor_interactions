**Interactions among multiple stressors vary with exposure duration and biological response**

_Outline:_
Using an experimental study, we present an approach for analysing regression-based designs with generalised additive models (GAMs) that allowed us to capture non-linear effects of exposure duration and stressor intensity, and access interactions among stressors. 

_Aims:_
The aims of this research were to; 1) predict the non-linear effects of diuron exposure, DIN enrichment and reduced light on two response variables, photosynthesis and growth; 2) identify how interaction types for the two response variables vary over different exposure durations; and, 3) compare the interaction types for the two response variables at the same experimental treatment levels. To address the aims, we used an experimental study and applied a novel approach to access stressor interaction types. This was done by integrating a regression-based blocks design with continuous non-linear modelling, which subsequently translates the non-linear effects into a standard interaction-type classification. This allowed us to assess how interactions change with experimental context. The innovation of this study is in the integration of the experimental design with the statistical framework for non-linear analysis.

_Github repository:_
On the Github repository, there are 


I had a look through the GitHub repository, but I found it a bit confusing. It might be a good idea to add more details to the readme file about the role of each script, the contents of each data file and the version information for R and the packages. When I tried to run some of the scripts, I found quite a lot of data exploration/notes/previous analyses – maybe some this could be streamlined so that the results from the paper can be easily reproduced. 

All experimental data is provided in the "Data" folder. This includes four replicates worth of photosynthesis and growth data.

The R scripts to perform the analyses are provided in the "Scripts" folder. This includes:
- 
- the main effects plots (Figures 1 and 2)
- the interactions plots (Figures 3 and 4)
- the model validation functions (Supplementary Material)
