# Texas-Wind-Speed-Forecsting-ESN
Columbia Water Center project on predicting space-time fields of wind and solar energy, using data from Texas as an example.

## Reposutory Structure
`data_preparation.R` : Load wind speed dataset and make it a "space-wide" table, where columns correspond to different grid points and rows correspond to time points.

`Train_Test_Data_Processing.R`: Split the dataset into training and test sets, detrend and normalize the dataset, retain the first several EOFs (PCs), and project data onto basis functions.

`QESN_Model_one_month.R`: Ensemble of quadratic-echo-state-network models to obtain forecasts.

`Post_Processing_Outputs.R`: Visualizatin and calculate MSEs.
