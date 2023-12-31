# Welcome to NWDBT3
This is the Github repository for National Wind Database Task 3! T3 uses machine learning algorithms to provide stochastic power curve modeling.
# Installing packages
NOTE: The installation instruction below assume that you have python installed on your machine and are using conda as your package/environment manager.
1. Create a new environment: conda create --name nwdbt3 python=3.8  
2. Activate environment: conda activate nwdbt3  
3. Install packages listed in rq.txt
# Code scipts
1. manupc_cleaning.py cleans the raw data in Data/All/.<br>
   Example of data cleaning.<br>
   <img src="/images/data_clean.png" width="600" height="200" alt="Alt text">
3. ANN_trainging.ipynb trains the model for a single turbine.<br>
   Example of wind turbine uncertainty modeling.<br>
   <img src="/images/modeling_results.png" width="600" height="300" alt="Alt text">
5. ProbMetrics_2.R calcualates the metrics for wind turbine uncertainty quantification.
6. Analysis_SPC_WindRegion.R outputs the relialibility and sharpness plots.<br>
<pre>
  Example of reliability plot.          Example of sharpness plot.
</pre>
<div>
  <img src="/images/reliability.png" width="300" height="300" alt="First Image">
  <img src="/images/sharpness.png" width="300" height="300" alt="Second Image">
</div><br>
   Example of time series plot.<br>
   <img src="/images/TS.png" width="600" height="200" alt="Alt text"><br>
   
# Recommended Citation
Please check back in the future
# Acknowledgments
Funding provided by the DOE Wind Energy Technologies Office (WETO).
