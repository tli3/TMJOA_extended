Scripts for the mansucript: A Comprehensive Patient-Specific Prediction Model for Temporomandibular Joint Osteoarthritis Progression

Steps:

Step0: prepare.r: filter out highly correlated features to reduce colinearity of covariates and generate interaction features.

Step1: step1_parallel.m: main script to run 77 combinations of individual methods  (7 feature selection methods * 11 baseline machine learning methods)

Step2: step2_comb_*.r: main script for model ensemble

Step3: step3_summ.r: model evaluations, summary statistics, and plots (SHAP, feature importance, ROC curves, box plot comparison, etc.)

