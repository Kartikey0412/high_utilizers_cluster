# high_utilizers_cluster.R

The emergence of Electronic Health Records (EHR) containing structured longitudinal patient records on diagnosis, medications, labs or demographic information like age, gender or race have led to the possibility of using advanced machine learning algorithms to classify and cluster millions of patient data. Usually the medical feature space is high dimensional and moreover highly correlated, leading to over fitting, lower accuracy and high computational time during evaluation. Optimized feature selection combined with clinical knowledge removes redundancy and helps in better understanding of the model for bigger datasets in the health informatics domain. 

Objectives

•	Evaluate various feature selection algorithms for predicting high cost in 2012 Medicare EHR data. 
•	 Compare statistical models to models based on features, ascertained clinically relevant, to predict high cost patients.



Divided the code into 4 sections:

HU_making_dataframe_features_cost: Combining raw medical feature space from Medicare 2012, and with cost data for each patient.

HU_feature_importance: Running different feature selection algorithms in the medical feature space and using corresponding coefficients. Finally comparing different feature selection algorithms.

HU_CART_vs_clinical: Running CART algorithm using (rpart) library for predicting high utilizers. Features are selected by the CART algorithm from more than 250 features. Also running CART algorithm on features chosen by a clinician for final comparison. 

HU_t-SNE: Running daisy package to calculate Gower distances and converting to similarity measures using Gaussian transformation. Finally calculating t-SNE embedding from Rtsne package in R. Embedding’s are clustered using k-means to evaluate sub-groups in the high utilizers population. 
