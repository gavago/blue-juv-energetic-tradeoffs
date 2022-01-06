---
title: "Juvenile energetic trade-offs analysis"
author: "Nic TG"
date: "1/6/2022"
output: 
   prettydoc::html_pretty:
    theme: cayman
    highlight: github
    toc: true
    keep_md: true
toc-title: "Contents"

---

### Data Dictionary

#### C-peptide of insulin dataset variables:

<font size = "2">
In `cp_raw` every row is a unique urine sample, which is from a subject sampled on a given date and time.




`group`: chr, social group of juvenile subject  
`subj`: chr, 4 character code of subject  
`date`: Date, date of urine sample collection  
`time`: POSIXlt, time of sample collection  
`sample_number`: int, unique identifier of sample  
`Cr`: num, creatinine value of sample  
`SG`: num, specific gravity of sample, value is SG-1 * 100 (e.g. 1.23 - 1 * 100 = 23)  
`Cr.plate.number`: int, original plate number of creatinine assay  
`Cr.date`: Date, date of creatinine assay  
`cp_dilution`: int, a dilution factor X, i.e. sample ran for cp at 1:X dilution  
`cp_run`: int, group of cp samples put through gamma counter  
`cp_assay_date`: chr, date when cp was read in gamma counter  
`pg.ml_ucp`: num, raw concentration of cp in assayed sample in pg/ml * dilution factor (dilution controlled for in gamma counter input)  
`cp_batch`: int, set of 50 samples used with a given cp ria kit  
`stdcr_CP`: num,  
`stsg_CP`: num, pg.ml_ucp controlling for sample specific gravity, stdsg_CP = pg.ml_ucp * mean(SG)/sample SG  
`cp_note`: chr, any relevant notes about cp value  
`month`: num, month of sample collection  
`year`: num, year of sample collection  

</font>

#### Neopterin dataset variables:

behav dataset, is a unique subject-month of behavioral observation, and variables. in here are proportion of observed time in a given behavioral activity (giving grooming, playing, etc.)
or number of counts in a given month that a subject gave or received aggression.

### Analysis

#### Background and hypothesis
#### Modeling approach
