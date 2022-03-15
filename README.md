# Predicting Fitness (VO<sub>2peak</sub>) in Individuals with Cardiovascular Disease

## Summary:
Predictions of peak oxygen consumption (VO<sub>2peak</sub>) are used clinically, yet current equations were developed from cohorts of apparently healthy individuals and may not be applicable to individuals with cardiovascular disease (CVD). **The purpose of this study was to develop a VO<sub>2peak</sub> prediction equation specifically for individuals with CVD.** The results indicated that the new equation for individuals with CVD had lower error between measured and predicted VO<sub>2peak</sub> meaning this new equation can improve risk stratification and clinical care in individuals with CVD.


## The Rationale:
Peak oxygen consumption (VO<sub>2peak</sub>) is a singular measure of whole-body physiological function. In patients with cardiovascular disease (CVD), a considerable body of research indicates lower VO<sub>2peak</sub> is associated with greater risk for early mortality, future CVD events, and higher healthcare costs. As such, a [2016 Scientific Statement](https://pubmed.ncbi.nlm.nih.gov/27881567/) from the American Heart Association suggested VO<sub>2peak</sub> should be considered a clinical vital sign that is regularly assessed alongside other established risk factors. 

The interpretation of VO<sub>2peak</sub> requires reference standards to stratify patient risk and guide clinical care. While I have previously published [normative reference standards for individuals with CVD](https://www.ahajournals.org/doi/10.1161/JAHA.121.022336), there remains a need for a convenient prediction equation to facilitate the interpretation of VO<sub>2peak</sub> and improve risk stratification in this population.

Further, a prediction equation could also address the American Heart Association’s recommendation that all routine clinical visits minimally include an estimation of VO<sub>2peak</sub> using a non-exercise prediction equation. Non-exercise prediction equations are a simple method for assessing VO<sub>2peak</sub> because exercise is not required, and the needed information is readily available in electronic medical records. Therefore, a non-exercise prediction equation developed from a cohort of individuals with CVD is needed to improve estimations of VO<sub>2peak</sub> and subsequently improve clinical care in patients with CVD.


## The Process:
### Dataset creation.
The data comes from the FRIEND database. The data wrangling and cleaning relied heavily on the R package dplyr. Data were filtered to exclude extraneous values and to meet the inclusion criteria of the study (e.g., known status regarding current or previous occurrence of coronary artery bypass surgery (CABG), myocardial infarction (MI), percutaneous coronary intervention (PCI), and heart failure (HF)). 

### Analysis.
From the dataset, 80% (n = 12,798) of the subjects were randomly selected for development of the prediction equation and the remaining 20% (n = 3,199) were used for validation of the equation. With the goal of creating an equation that is easy to use and interpret, a linear regression model was created. Multivariate forward stepwise regression with the caret package in R was used to identify which variables to include in the model (the model that minimized the RMSE was chosen as the best model).

For comparison with the new CVD-specific equation, predicted VO<sub>2peak</sub> for each subject of the validation cohort was determined using a [previously published](https://pubmed.ncbi.nlm.nih.gov/29517365/) equation developed on a FRIEND cohort without known CVD. The variables in this previously published equation were the same as those considered for the CVD-specific equation, with the exception of CVD diagnosis

#### Statistics:
The differences between sexes as well as between development and validation groups were examined using independent _t_-tests and Chi-square tests.

Comparisons between measured and predicted VO<sub>2peak</sub> were performed with dependent _t_-tests and Pearson correlations. Correlation coefficients were compared following _Z_ transformation. To further assess the agreement between each prediction equation and directly measured VO<sub>2peak</sub>, intraclass correlation coefficient (ICC) estimates and their 95% confidence intervals were calculated using the irr package in R based on a single-score one-way model. The ICC estimates were then interpreted according to the [guidelines](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/) proposed by Koo and Li.

#### Visualizations:
Bland-Altman plots were constructed to illustrate the agreement between measured and predicted VO<sub>2peak</sub> from each equation. 

Scatterplots were created to illustrate the differences between the new equation and the previously published equation developed on a healthy cohort. Colors were added to indicate differences between predicted and measured VO<sub>2peak</sub> that were represented as metabolic equivalents (METs) to facilitate interpretation of errors. Groups were then created according to the absolute MET difference between predicted and measured VO<sub>2peak</sub>: <0.50, 0.50–0.99, 1.00–1.99, and ≥2.00 METs. The distribution of the MET groups for each prediction equation was compared with a Chi-Square test. 


## The Final Product/Results:
The initial results for this study were submitted and presented as a poster at the 2022 American Heart Association EPI|Lifestyle Conference in Chicago, IL (_you can view the poster that was presented at this conference as the AHA_poster.pdf file in this repository_).

After the conference, additional data was contributed to FRIEND, allowing for the analysis to be reperformed on a larger dataset. Following this final data analysis, I summarized the findings of this study and submitted the scientific manuscript for publication. The manuscript is currently under peer review by experts in the field.
