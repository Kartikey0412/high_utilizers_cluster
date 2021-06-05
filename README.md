High Utilizers
==============================

The emergence of Electronic Health Records (EHR) containing structured longitudinal patient records on diagnosis, medications, labs or demographic information like age, gender or race have led to the possibility of using advanced machine learning algorithms to classify and cluster millions of patient data. Usually the medical feature space is high dimensional and moreover highly correlated, leading to over fitting, lower accuracy and high computational time during evaluation. Optimized feature selection combined with clinical knowledge removes redundancy and helps in better understanding of the model for bigger datasets in the health informatics domain.

Objectives

• Evaluate various feature selection algorithms for predicting high cost in 2012 Medicare EHR data. • Compare statistical models to models based on features, ascertained clinically relevant, to predict high cost patients.

Divided the code into 4 sections:

HU_making_dataframe_features_cost: Combining raw medical feature space from Medicare 2012, and with cost data for each patient.

HU_feature_importance: Running different feature selection algorithms in the medical feature space and using corresponding coefficients. Finally comparing different feature selection algorithms.

HU_CART_vs_clinical: Running CART algorithm using (rpart) library for predicting high utilizers. Features are selected by the CART algorithm from more than 250 features. Also running CART algorithm on features chosen by a clinician for final comparison.

HU_t-SNE: Running daisy package to calculate Gower distances and converting to similarity measures using Gaussian transformation. Finally calculating t-SNE embedding from Rtsne package in R. Embedding’s are clustered using k-means to evaluate sub-groups in the high utilizers population.

Project Organization
------------

    ├── LICENSE
    ├── Makefile           <- Makefile with commands like `make data` or `make train`
    ├── README.md          <- The top-level README for developers using this project.
    ├── data
    │   ├── external       <- Data from third party sources.
    │   ├── interim        <- Intermediate data that has been transformed.
    │   ├── processed      <- The final, canonical data sets for modeling.
    │   └── raw            <- The original, immutable data dump.
    │
    ├── docs               <- A default Sphinx project; see sphinx-doc.org for details
    │
    ├── models             <- Trained and serialized models, model predictions, or model summaries
    │
    ├── notebooks          <- Jupyter notebooks. Naming convention is a number (for ordering),
    │                         the creator's initials, and a short `-` delimited description, e.g.
    │                         `1.0-jqp-initial-data-exploration`.
    │
    ├── references         <- Data dictionaries, manuals, and all other explanatory materials.
    │
    ├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
    │   └── figures        <- Generated graphics and figures to be used in reporting
    │
    ├── requirements.txt   <- The requirements file for reproducing the analysis environment, e.g.
    │                         generated with `pip freeze > requirements.txt`
    │
    ├── setup.py           <- makes project pip installable (pip install -e .) so src can be imported
    ├── src                <- Source code for use in this project.
    │   ├── __init__.py    <- Makes src a Python module
    │   │
    │   ├── data           <- Scripts to download or generate data
    │   │   └── make_dataset.py
    │   │
    │   ├── features       <- Scripts to turn raw data into features for modeling
    │   │   └── build_features.py
    │   │
    │   ├── models         <- Scripts to train models and then use trained models to make
    │   │   │                 predictions
    │   │   ├── predict_model.py
    │   │   └── train_model.py
    │   │
    │   └── visualization  <- Scripts to create exploratory and results oriented visualizations
    │       └── visualize.py
    │
    └── tox.ini            <- tox file with settings for running tox; see tox.readthedocs.io


--------

<p><small>Project based on the <a target="_blank" href="https://drivendata.github.io/cookiecutter-data-science/">cookiecutter data science project template</a>. #cookiecutterdatascience</small></p>
