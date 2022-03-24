**Interactions among multiple stressors vary with exposure duration and biological response**

_Outline:_

Using an experimental study, we present an approach for analysing regression-based designs with generalised additive models (GAMs) that allowed us to capture non-linear effects of exposure duration and stressor intensity, and access interactions among stressors. 

_Aims:_

The aims of this research were to; 1) predict the non-linear effects of diuron exposure, DIN enrichment and reduced light on two response variables, photosynthesis and growth; 2) identify how interaction types for the two response variables vary over different exposure durations; and, 3) compare the interaction types for the two response variables at the same experimental treatment levels. To address the aims, we used an experimental study and applied a novel approach to access stressor interaction types. This was done by integrating a regression-based blocks design with continuous non-linear modelling, which subsequently translates the non-linear effects into a standard interaction-type classification. This allowed us to assess how interactions change with experimental context. The innovation of this study is in the integration of the experimental design with the statistical framework for non-linear analysis.

_Github repository files:_

On the Github repository, there are three visible folders:
1. Data - contains all experimental data (i.e. four replicates each of chlorophyll-a fluorescence and absorbance (cell density) data).
2. Output - contains various GAM, main effects, interactions and model validation figures.
3. Scripts - contains the R-scripts that produce all output figures.

_Role of each script:_

******

_Information for R software used:_

Data processing and analyses was carried out using R statistical software version 4.1.0.
Packages used: mgcv (version ???),

**@cbrown5 this was a comment from the reviewer (below):**
_When I tried to run some of the scripts, I found quite a lot of data exploration/notes/previous analyses – maybe some this could be streamlined so that the results from the paper can be easily reproduced._ **Can you please help me clean up the scripts? I have cleaned up the data folder and outputs folder, but I don't want to remove the wrong scripts from the file... After that, I can write the 'role of each script' in the above section**
