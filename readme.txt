
Instructions:

1) Insert the datasets of Coolblue ('broadcasting_data' and 'traffic_data') in '\Coolblue_datasets'.
2) Open 'data_prep.ipynb' in Jupyter Notebook and run all cells to process the data.
3) Run 'Coolblue.jl' to load the CB module in Julia and type CB.generate_feat_min(save = true) to save the feature per min. data.*
4) Open 'Baseline Script.R' and run the first 37 lines to obtain the BSTS spot lifts. These spot lifts are exported to '\spotlifts\BSTS_4min'
5) Type CB.validate() in the Julia REPL to get the greedy baseline/spotlifts and diagnostics for all dataset and minute combinations.*
6) Open 'post_eff_and_data_selection.ipynb' in Jupyter notebook and run all cells to get the reuslts for post effect and dataset selection. The results are shown in Jupyter Notebook and exported to '\results'
7) Open 'regressions.ipynb' and run all cells to obtain the results for all discussed methods. The results are only shown in Jupyter Notebook.
8) Type CB.optimal_largest_postEff(param_eval = true) to get the all different feature per minute results.*
9) Type CB.optimal_largest_postEff(param_eval = false) to get the optimal feature per minute results.*
10) Type CB.plot_all() to get .png files of all figures used in the report.*

*Alternatively, type Cb.run_all() in the Julia REPL after loading the CB module to perform all these steps at once.