# Replication materials: Determining the impact of strategic voting on election results

This repository provides replication materials for the paper "Determining the impact of strategic voting on election results" by Michael Herrmann, Simon Munzert and Peter Selb, forthcoming in the [Journal of the Royal Statistical Society Series A](http://onlinelibrary.wiley.com/journal/10.1111/(ISSN)1467-985X). An ungated version of the paper can be found [here](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2127621).

## Description of the folders

code
- provides R code needed for analysis

data
- provides processed survey, constituency and geographic data needed for analysis
- the survey data we use (British Social Attitudes Survey 1996, 1998, 2000, 2001, 2002 and General Election Study 1997 (Cross Section), 2001 (Cross-Section and Campaign Panel), 1992-1997 (Panel) and 1997-2001 (Panel)) are available from the [UK Data Service](http://discover.ukdataservice.ac.uk/series/?sn=200006) and the [British Election Studies Information System](http://www.besis.org/), run by the Centre for Comparative European Survey Data. These data are prepared for analysis using the Stata dofile `01_data_preparation.do` and the R script `01_data_preparation.r` and produce the Stata datafile `prefs_9602full.dta` that is used for further analysis (but not released on GitHub)
- constituency data stem from the [Pippa Norris Constituency Dataset](http://www.hks.harvard.edu/fs/pnorris/Data/Data.htm) and are enriched with incumbeny information
- constituency shapefiles were provided by the [Office for National Statistics](http://www.ons.gov.uk/ons/index.html)
- data on national-level polls to inform an alternative version of our strategic voting model are taken from [UKPollingReport.co.uk](http://ukpollingreport.co.uk/)

bugs_simulations
- provides WinBUGS model code (text files) to reproduce the validation and strategic voting models
- simulation results are not uploaded due to GitHub's file size limits; if you are interested in the data, please [let us know](mailto:simon.munzert@gmail.com) and we will gladly share them with you

figures
- provides pdfs of figures presented in the paper as well as supporting figures


## Description of the code files

`01_data_preparation.do`
- pre-processes raw survey data from the BSA and GES
- generates `prefs_9602full.dta' and `constituency_data.dta`

`01_data_preparation.r`
- pre-processes geographic and survey data 
- provides descriptive summary statistics for survey data
- generates `data/data_strategic_uk9701.RData`

`02_preparation_estimation_1997.r`
- prepares `data/data_strategic_uk9701.RData` for estimation
- generates `data/data_strategic_bayesprep97.RData`

`02_preparation_estimation_2001.r`
- prepares `data/data_strategic_uk9701.RData` for estimation
- generates `data/data_strategic_bayesprep01.RData`

`03_validation.r`
- provides a validation analysis of the district preference estimation model
- generates `bugs_simulations/model2b_vote97.RData`
- generates `bugs_simulations/model2b_vote01.RData`
- generates various supporting plots for the appendix

`04_estimation.r`
- runs the estimations for the main model of the paper (via WinBUGS)
- generates `bugs_simulations/stratvotemodel97_14.RData`
- generates `bugs_simulations/stratvotemodel01_14.RData`

`05_postprocess_estimates_1997.r`
- post-processes the model estimates for further analysis

`05_postprocess_estimates_2001.r`
- post-processes the model estimates for further analysis

`06_figure1_parameter_estimates.r`
- generates figure 1 of the paper ("Posterior means and 95% credible intervals of parameter estimates")

`06_figure2a_strategic_switches_1997.r`
- generates figure 2a of the paper ("Constituency-level strategic switches (posterior means and 95% credible intervals)")

`06_figure2b_strategic_switches_2001.r`
- generates figure 2b of the paper ("Constituency-level strategic switches (posterior means and 95% credible intervals)")

`06_figure3_table_netmargins_1997.r`
- generates figure 3 of the paper ("Constituencies in which strategic voting possibly made a difference in 1997")

`06_figure4_table_netmargins_2001.r`
- generates figure 4 of the paper ("Constituencies in which strategic voting possibly made a difference in 2001")

`07_figures_appendix_1997.r`
- generates various supporting plots for the appendix

`07_figures_appendix_2001.r`
- generates various supporting plots for the appendix

`functions.r`
- contains custom functions for the analysis

`packages.r`
- contains code to import required R packages for the analysis


## Author information

Michael Herrmann (corresponding author), Simon Munzert (repository maintainer), Peter Selb

University of Konstanz

Department of Politics and Public Administration

Box 92, D-78457 Konstanz

E-mail: michael.herrmann@uni-konstanz.de
