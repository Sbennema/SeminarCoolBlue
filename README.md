# Using Bayesian Structural Time Series and the Elastic Net to investigate the short-term effect of television advertisements.

### This Github contains all Julia/Python/R code used in the Seminar project of Coolblue
This project was part of the Consultancy project for Coolblue and the Seminar Case Studies at the Erasmus University Rotterdam. The goal of this project was to study the short-term effect of TV ads on online visits and provide actionable insights. This code was used for the corresponding report and presentation on this subject.

### In short: 
- The Julia files (1) generate greedy baselines and spot lifts, (2) evaluate the elastic net of the biggest spot lifts using feat. per min. and (3) provide all figures.
- The Python/Jupyter Notebook files (1) prepare the data, (2) generate the the results of optimal post effect and (3) show results of the regression and machine learning models.
- The R files generate the AR and BSTS baselines and spot lifts.

### Detailed instructions (10 steps):

1) Insert the datasets of Coolblue ('broadcasting_data' and 'traffic_data') in '\Coolblue_datasets'.
2) Open 'data_prep.ipynb' in Jupyter Notebook and run all cells to process the data.
3) Run 'Coolblue.jl' to load the CB module in Julia and type CB.generate_feat_min(save = true) to save the feature per min. data.*
4) Open 'Baseline Script.R' and run the first 37 lines to obtain the BSTS spot lifts.
5) Type CB.validate() in the Julia REPL to get the greedy baseline/spotlifts and diagnostics for all dataset and minute combinations.*
6) Open 'post_eff_and_data_selection.ipynb' in Jupyter notebook and run all cells to get the results for post effect and dataset selection. The results are shown in Jupyter Notebook.
7) Open 'regressions.ipynb' and run all cells to obtain the results for all discussed methods. The results are only shown in Jupyter Notebook.
8) Type CB.optimal_largest_postEff(param_eval = true) to get the all different feature per minute results.*
9) Type CB.optimal_largest_postEff(param_eval = false) to get the optimal feature per minute results.*
10) Type CB.plot_all() to get .png files of all figures used in the report.*

\*Alternatively, type Cb.run_all() in the Julia REPL after loading the CB module to perform all these steps at once.
