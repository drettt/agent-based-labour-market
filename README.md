# agent-based-labour-market-dissertation

This project inlcudes an agent-based labour market model coded in NetLogo that is used to investigate the effects of the internet on the job market. The R package [`nlrx`](https://cran.r-project.org/web/packages/nlrx/index.html) was used to link NetLogo to R. 

## Contents

1. In the model folder is the `NetLogo` code that runs the model. 
2. In the calibration folder is the `R` code for a genetic algorithm that calibrates the parameters of the model. 
3. In the simulation folder is the `R` code for the monte carlo simulations of the model, which should be run with the calibrated parameters.
4. In the analysis folder is the `R` code to analyse the data (also in the folder) and to generate the graphs seen in the final report.
