﻿Effect of Radiation and Diet on Aortic damage




Requirements: 
RMD file automatically installs 'RequieRments' and 'rstudioapi' packages in order to install missing libraries from the Requierments2.txt list. 
If new versions of any of the libraries are found, these will not be updated by this script.
For handling all requirements manually, remove the first chunck of the code (line 65 to 83)




Files: 
aorta2.txt: Dataset consisting of 5 variables and 80 observations.

Aorta.RMD: full R-Markdown code used to perform EDA, Statistical Analysis and Visualizations.

Aorta.html:  Output file. Analysis report with presentation of statistical results, graphs and results interpretation in a 'journal like' manner.




Summary:
The dataset was obtained from ‘Análisis de la Varianza: Carrera de Especialización en Estadísticas para Ciencias de la Salud (CEECS). Universidad de Buenos Aires. Instituto de Cálculo’.




The Experiment: 
40 Mice were treated with ionizing radiation (four different intensities) and exposed to standard or high-fat diet. Mice were divided equally between treatments and two samples were obtained per mouse to determine aortic area lesion. 



Statistical Analysis and Model:
Mixed ANOVA model was proposed to analyze differences between treatment combinations. 



Statistical and Visualization tools: 
Exploratory Data Analysis (EDA), Statistical Analysis were performed with R. Visualization of data and results were done mostly with ggplot2 and sjPlot. 





