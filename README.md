# Replication materials: Determining the impact of strategic voting on election results

This repository provides replication materials for the paper "Determining the impact of strategic voting on election results" by Michael Herrmann, Simon Munzert and Peter Selb, forthcoming in the [Journal of the Royal Statistical Society Series A](http://onlinelibrary.wiley.com/journal/10.1111/(ISSN)1467-985X). An ungated version of the paper can be found [here](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2127621).

## Description of the folders

bugs_simulations
- provides WinBUGS model code (text files) to reproduce the validation and strategic voting models
- simulation results are not uploaded due to GitHub's file size limits; if you are interested in the data, please [let us know](mailto:simon.munzert@gmail.com) and we will gladly share them with you

code
- provides R code needed for analysis

data
- provides processed constituency, geographic and survey data needed for analysis

figures
- provides pdfs of figures presented in the paper as well as supporting figures


## Description of the code files

01_data_preparation.r
- pre-processes geographic and survey data 
- provides descriptive summary statistics for survey data
- generates data/data_strategic_uk9701.RData

02_preparation_estimation_1997.r
- prepares data/data_strategic_uk9701.RData for estimation
- generates data/data_strategic_bayesprep97.RData

02_preparation_estimation_2001.r
- prepares data/data_strategic_uk9701.RData for estimation
- generates data/data_strategic_bayesprep01.RData

03_validation.r
- provides a validation analysis of the district preference estimation model
- generates bugs_simulations/model2b_vote97.RData
- generates bugs_simulations/model2b_vote01.RData
- generates various supporting plots for the appendix

04_estimation.r
- runs the estimations for the main model of the paper (via WinBUGS)
- generates bugs_simulations/stratvotemodel97_14.RData
- generates bugs_simulations/stratvotemodel01_14.RData

05_postprocess_estimates_1997.r
- post-processes the model estimates for further analysis

05_postprocess_estimates_2001.r
- post-processes the model estimates for further analysis

06_figure1_parameter_estimates.r
- generates figure 1 of the paper ("Posterior means and 95% credible intervals of parameter estimates")

06_figure2a_strategic_switches_1997.r
- generates figure 2a of the paper ("Constituency-level strategic switches (posterior means and 95% credible intervals)")

06_figure2b_strategic_switches_2001.r
- generates figure 2b of the paper ("Constituency-level strategic switches (posterior means and 95% credible intervals)")

06_figure3_table_netmargins_1997.r
- generates figure 3 of the paper ("Constituencies in which strategic voting possibly made a difference in 1997")

06_figure4_table_netmargins_2001.r
- generates figure 4 of the paper ("Constituencies in which strategic voting possibly made a difference in 2001")

07_figures_appendix_1997.r
- generates various supporting plots for the appendix

07_figures_appendix_2001.r
- generates various supporting plots for the appendix

functions.r
- contains custom functions for the analysis

packages.r
- contains code to import required R packages for the analysis


