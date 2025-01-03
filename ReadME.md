Juvenile energetic trade-offs analysis
================
Nic TG
1/6/2022

-   [Data Dictionary](#data-dictionary)
-   [Analysis](#analysis)

### Data Dictionary

#### C-peptide of insulin dataset variables:

<font size = "2"> In `cp_raw` every row is a unique urine sample, which
is from a subject sampled on a given date and time.

`group`: chr, social group of juvenile subject  
`subj`: chr, 4 character code of subject  
`date`: Date, date of urine sample collection  
`time`: POSIXlt, time of sample collection  
`sample_number`: int, unique identifier of sample  
`Cr`: num, creatinine value of sample  
`SG`: num, specific gravity of sample, value is SG-1 \* 100 (e.g. 1.23 -
1 \* 100 = 23)  
`Cr.plate.number`: int, original plate number of creatinine assay  
`Cr.date`: Date, date of creatinine assay  
`cp_dilution`: int, a dilution factor X, i.e. sample ran for cp at 1:X
dilution  
`cp_run`: int, group of cp samples put through gamma counter  
`cp_assay_date`: chr, date when cp was read in gamma counter  
`pg.ml_ucp`: num, raw concentration of cp in assayed sample in pg/ml \*
dilution factor (dilution controlled for in gamma counter input)  
`cp_batch`: int, set of 50 samples used with a given cp ria kit  
`stdcr_CP`: num,  
`stsg_CP`: num, pg.ml_ucp controlling for sample specific gravity,
stdsg_CP = pg.ml_ucp \* mean(SG)/sample SG  
`cp_note`: chr, any relevant notes about cp value  
`month`: num, month of sample collection  
`year`: num, year of sample collection

</font>

#### Neopterin dataset variables:

<font size = "2"> Like `cp_raw`, every row in `neo_data_full` is a
unique urine sample, which is from a subject sampled on a given date and
time. Many columns in these two datasets are shared.

**Shared columns**:  
`group`: chr, social group of juvenile subject  
`subj`: chr, 4 character code of subject  
`date`: Date, date of urine sample collection
`time`: POSIXlt, time of sample collection  
`sample_number`: int, unique identifier of sample  
`Cr`: num, creatinine value of sample  
`SG`: num, specific gravity of sample, value is SG-1 \* 100 (e.g. 1.23 -
1 \* 100 = 23)  
**Unique to Neo**:  
`month.name`: factor, 3 letter code for month  
`mum`: factor, 3 letter code for juv’s mum, can be optionally used as
clustering variable in regression  
`bday`: Date, subject’s birthday  
`weaning`: Date, subject’s last day observed nursing from mother  
`mid`: Date, the midday of each month, used for calculating monthly
ages  
`neo_value`: num, raw value of neopterin assay  
`CV`: num, coefficient of variation of neopterin sample assayed in
duplicate  
`neo_plate_number`: num, immuno plate on which final value of sample was
assayed  
`neo_dilution`: int, a dilution factor X, sample was diluted 1:X for its
final value  
`neo_date_assayed`: Date, assay date of final neo value  
`neo_note`: chr, any relevant note about sample when assayed for neo  
`SG_corr_fac`: num, mean(SG)/sample SG  
`neo_sg`: num, final value of neo to be used for analysis,
`neo_sg = neo_value * neo_dilution * SG_corr_fac`

</font>

#### Behavioral dataset

a row is a unique subject-month of behavioral observation. variables are proportion of observed time (point samples taken on the minute) in a given behavioral activity (giving grooming, playing, etc.) OR the number of times (all occurrence during focal follow) that a subject gave or received aggression.

`pl`: proportion of observation time engaging in social play
`gm`: proportion of observation time giving grooming to a partner
`gmd`: "" receiving grooming from a partner
`r`: "" sitting and resting (not feeding or socializing)
`f`: "" feeding
`m`: "" moving
`sl`: "" sleeping
`n_agg_g`: counts of aggression given
`n_agg_r`: counts of aggression received
`n_obs`: number of point samples taken, 1 record per minute
Additional variables:
`mrank`: maternal rank during study period (aug 2015-apr 2016) or mom's rank during last year of life if dead during study period
`fai`: fruit availability based on the mid point estimate
`fai_cat`: fai divided into high medium and low
`avg_rain`: mean daily rainfall during the month

### Added to merged dataset
`cr_resid`:  residuals of a linear regression of sample creatinine over specific gravity, meant to approximate lean body mass

### Analysis

#### Background and hypothesis

#### Modeling approach
