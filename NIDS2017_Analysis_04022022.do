Stata version
15.1
*** 
************************************************************************
* PART 0: TABLE OF CONTENTS
************************************************************************
clear

* start log 
*capture log close
*log using 

* Project: [Multimorbidity in South Africa]
* Creator: [Rifqah Roomaney, rifqah.roomaney@mrc.ac.za, 24/02/2022] - Based on Annibale's Cois files 
* Purpose of do-file: Data analysis for NIDS 2017

*Outline
*Part 1: Main analysis - Table 1 description of variables
*Part 2: Data exploration
*Part 3: Regression


**************************************
* PART 1: MAIN ANALYSIS - Survey set *
**************************************

use "C:\Users\rroom\OneDrive - South African Medical Research Council\Documents\PhD\Phase 2\SURVEY DATASETS\1 My project\Data\NIDS2017_ReadytoUse_24032022.dta", replace

svyset psu[pweight=csweight], strata(stratum) vce(linearized) singleunit(certainty) 
svydescribe


************************************************************************
*Table 1: Tabulations demographics and diseases
************************************************************************
describe //lists variables in the dataset
count 

tab gender, m
graph pie, over(gender)

tab age
summarize age, detail
by gender, sort : summarize age, detail  //summarize age by gender


summarize ind_income, detail
by gender, sort : summarize ind_income, detail  //summarize age by gender
kwallis ind_income, by(gender)
by asset_index, sort : summarize ind_income, detail  //summarize age by gender
kwallis ind_income, by(asset_index)


histogram age, normal
swilk age //not normally distributed
graph box age, over(gender)
graph box age, over(asset_index)

ranksum age, by(gender) porder  //test for difference in age between males and females - wilcoxon rank
kwallis age, by(gender)
kwallis age, by(asset_index)


tab age_10
tab age_10 gender, col chi 


tab race

tab urban
tab urban gender, col chi 
tab urban asset_index, col chi 
svy linearized : proportion urban
svy linearized : proportion urban, over(gender)

tab prov
tab prov gender, col chi
svy linearized : proportion prov
svy linearized : proportion prov, over(gender)

tab educat
tab educat gender, col chi
svy linearized : proportion educat
svy linearized : proportion educat, over(gender)

tab employed
tab employed gender, col chi 
svy linearized : proportion employed
svy linearized : proportion employed, over(gender)

tab bmi_cat
tab bmi_cat gender, col chi

tab CURRSMOK
tab CURRSMOK gender, col chi

tab asset_index
tab asset_index gender, col chi

tab medaid
tab medaid gender, col chi


** Variables by asset_index (Table 1 alternative)
summarize age, detail
by asset_index, sort : summarize age, detail  //summarize age by asset index
kwallis age, by(asset_index) //test for difference in age by asset index
graph box age, over(asset_index)

tab gender
tab gender asset_index, col chi 

tab urban
tab urban asset_index, col chi 

tab race
tab race asset_index, col chi 

tab prov
tab prov asset_index, col chi

tab educat
tab educat asset_index, col chi

tab employed
tab employed asset_index, col chi 

summarize ind_income, detail
by asset_index, sort : summarize ind_income, detail  
kwallis hh_inc, by(asset_index) 

tab bmi_cat
tab bmi_cat asset_index, col chi

tab CURRSMOK
tab CURRSMOK asset_index, col chi

tab medaid
tab medaid asset_index, col chi

*************************************************
*Table 2/ Graph 2: Single disease by sex and age*
*************************************************

** Self reported disease tabulations by gender and age
recode age_10 (8=7)
tab age_10


tab SELFDIAB
tab SELFDIAB gender, col chi
logistic SELFDIAB gender
svy linearized : proportion SELFDIAB
svy linearized : proportion SELFDIAB, over(gender)

svy linearized : proportion SELFDIAB, over(asset_index)

svy linearized : proportion SELFDIAB, over(age_cat)
svy linearized : proportion SELFDIAB, over(age_10)
svy linearized, subpop(if gender==1) : proportion SELFDIAB, over(age_10)
svy linearized, subpop(if gender==2) : proportion SELFDIAB, over(age_10)

tabulate SELFHEART
tab SELFHEART gender, col  chi
logistic SELFHEART gender
svy linearized : proportion SELFHEART
svy linearized : proportion SELFHEART, over(gender)

svy linearized : proportion SELFHEART, over(asset_index)


svy linearized, subpop(if gender==1) : proportion SELFHEART, over(age_10)
svy linearized, subpop(if gender==2) : proportion SELFHEART, over(age_10)
svy linearized : proportion SELFHEART, over(age_10)

tabulate SELFSTROKE
tab SELFSTROKE gender, col  chi
logistic SELFSTROKE gender
svy linearized : proportion SELFSTROKE
svy linearized : proportion SELFSTROKE, over(gender)

svy linearized : proportion SELFSTROKE, over(asset_index)


svy linearized : proportion SELFSTROKE, over(age_10)
svy linearized, subpop(if gender==1) : proportion SELFSTROKE, over(age_10)
svy linearized, subpop(if gender==2) : proportion SELFSTROKE, over(age_10)

tabulate htn 
tab htn gender, col  chi
logistic htn gender
svy linearized : proportion htn 
svy linearized : proportion htn , over(gender)

svy linearized : proportion htn, over(asset_index)

svy linearized : proportion htn, over(age_10)
svy linearized, subpop(if gender==1) : proportion htn, over(age_10)
svy linearized, subpop(if gender==2) : proportion htn, over(age_10)

svy linearized : proportion mm_index, over(age_10) 
svy linearized, subpop(if gender==1) : proportion mm_index, over(age_10)
svy linearized, subpop(if gender==2) : proportion mm_index, over(age_10)

**age and disease
tab age_10 diabetes, row //
tab age_10 htn, row // fairly high in young people
tab age_10 SELFHEART, row
tab age_10 SELFSTROKE, row
tab age_10 mm_index, row

***Alternative Table 2 (by asset index)

svy linearized : proportion SELFDIAB
svy linearized : proportion SELFDIAB, over(asset_index)

svy linearized : proportion SELFHEART
svy linearized : proportion SELFHEART, over(asset_index)

svy linearized : proportion SELFSTROKE
svy linearized : proportion SELFSTROKE, over(asset_index)

svy linearized : proportion htn 
svy linearized : proportion htn , over(asset_index)


************************************************************************
* Table 3: Multimorbidity index explore
************************************************************************
tab index
tab index gender, col chi
svy: tabulate index
svy linearized : proportion index 
svy linearized : proportion index , over(gender)
svy: tabulate index gender
svy: tabulate mm_index gender


generate index0= index 
recode index0 4=3
tab index0
tab index0 gender, col chi
svy linearized : proportion index0 
svy linearized : proportion index0 , over(gender)
svy: tabulate index0 gender

generate index1= index 
recode index1 1=0 0=1 2/6=1
svy: tabulate index1 gender

generate index2= index 
recode index2 2=1 0/1=0 3/6=0
svy: tabulate index2 gender

tab mm_index 
tab mm_index gender, col chi
svy: tabulate mm_index
svy linearized : proportion mm_index 
svy linearized : proportion mm_index , over(gender)



svy linearized : proportion mm_index , over(age_10)
svy linearized, subpop(if gender==1) : proportion mm_index, over(age_10)
svy linearized, subpop(if gender==2) : proportion mm_index, over(age_10)

svy linearized : proportion mm_index , over(asset_index)



graph box age, over(index)
graph box age, over(mm_index)
graph box age [pw = csweight ], by(mm_index)

by mm_index, sort : summarize age, detail

tabulate age_10 mm_index, row exp chi
tabulate mm_index gender, row exp chi

tabulate age_10 mm_index if gender ==1, row col
tabulate age_10 mm_index if gender ==2, row col
tabulate age_10 mm_index

**Alternative Table 3
svy: tabulate index0 

svy linearized : proportion index0 , over(asset_index)
svy linearized : proportion mm_index , over(asset_index)

*For comparison paper
recode age_10 (8=7)
svy linearized : proportion mm_index , over(age_10)
svy linearized, subpop(if gender==1) : proportion mm_index, over(age_10)
svy linearized, subpop(if gender==2) : proportion mm_index, over(age_10)


********************
*PART 3: Regression*
********************
use "C:\Users\rroom\OneDrive - South African Medical Research Council\Documents\PhD\Phase 2\SURVEY DATASETS\1 My project\Data\NIDS2017_ReadytoUse_24032022.dta", replace

svyset psu[pweight=csweight], strata(stratum) vce(linearized) singleunit(certainty) 
svydescribe

*Income
xtile income_grp=ind_income[aw=csweight], n(3) // Split into three groups as it did not want to split into 5 groups

* Age
recode age_cat (1=1) (2=2) (3=2) (4=3) (5=3) (6=4)
label def age_cat1 1 "15-24 years" 2 "25-44 years" 3 "45-64 years" 4 "65+ years" 
label val age_cat age_cat1

*Crude models
svy: logistic mm_index i.age_cat
svy: logistic mm_index gender
svy: logistic mm_index urban
svy: logistic mm_index i.educat

svy: logistic mm_index employed
svy: logistic mm_index i.asset_index
svy: logistic mm_index i.income_grp
svy: logistic mm_index medaid

svy: logistic mm_index CURRSMOK
svy: logistic mm_index ib1.bmi_cat


svy: logistic medaid i.asset_index


* Model exploration

svy: logistic mm_index i.age_cat gender  i.age_cat#gender urban employed i.educat i.income_grp  i.asset_index medaid ib1.bmi_cat CURRSMOK // Full model with interaction

// Note - the interaction term was not significant

* Full model without interaction term
svy: logistic mm_index i.age_cat gender urban i.educat employed i.asset_index i.income_grp  medaid CURRSMOK ib1.bmi_cat  // Full model income group
svylogitgof   

                  
**Model checking:
logistic mm_index i.age_cat gender urban employed i.educat i.income_grp  i.asset_index medaid ib1.bmi_cat CURRSMOK  //for model checking  we use the normal model (not svy)


capture drop p stdres
predict p
predict stdres, rstand
scatter stdres p, mlabel(pid) ylab(-4(2) 16) yline(0)

list pid mm_index age_cat gender  urban educat  CURRSMOK  bmi_cat if (stdres >4 & stdres <.)

scatter stdres id, mlab(pid) ylab(-4(2) 16) yline(0)
predict dv, dev
scatter dv p, mlab(pid) yline(0)
scatter dv id, mlab(pid)
predict hat, hat
scatter hat p, mlab(pid)  yline(0)
scatter hat id, mlab(pid)

drop if (stdres >4 & stdres <.)

predict dx2, dx2
predict dd, dd
scatter dx2 id, mlab(id)
scatter dd id, mlab(id)

predict dbeta, dbeta
scatter dbeta id, mlab(id)

qnorm stdres












***Extra code ***
tabstat age, statistics( p50 p25 p75 ) by(SELFHEART ) //Median age of people who have heart disease
tabstat age, statistics( p50 p25 p75 ) by(SELFSTROKE) // Median age of people with stroke
tabstat age, statistics( p50 p25 p75 ) by(SELFCHOL) // Median age of people with high cholesterol
tabstat age, statistics( p50 p25 p75 ) by(SELFDIAB) // Median age of people with diabetes
tabstat age, statistics( p50 p25 p75 ) by(SELFBRONCH) // Median age of people with COPD


**Exploration of the logistic model
gen logincome= log(ind_income)
svy: logistic employed logincome


svy: logistic mm_index i.age_cat gender urban  i.educat logincome employed i.asset_index medaid ib1.bmi_cat CURRSMOK //Log income included

svy: logistic mm_index i.age_cat gender urban  i.educat ind_income employed i.asset_index medaid ib1.bmi_cat CURRSMOK // income included
logistic mm_index i.age_cat gender i.age_cat#gender urban employed i.asset_index i.educat  CURRSMOK ib1.bmi_cat //logincome
svy: logistic mm_index i.age_cat gender urban  i.educat employed i.asset_index medaid ib1.bmi_cat CURRSMOK //Employed and logincome seems to be colliear?? Remove income


***REVIEWER Missing data
tab gender, m
graph pie, over(gender)

tab age
summarize age, detail
by gender, sort : summarize age, detail  //summarize age by gender


summarize ind_income, detail
by gender, sort : summarize ind_income, detail  //summarize age by gender
kwallis ind_income, by(gender)


histogram age, normal
swilk age //not normally distributed
graph box age, over(gender)
graph box age, over(asset_index)

ranksum age, by(gender) porder  //test for difference in age between males and females - wilcoxon rank
kwallis age, by(gender)
kwallis age, by(asset_index)


tab age_10
tab age_10 gender, col chi 


tab race

tab urban
tab urban gender, col chi 
tab urban asset_index, col chi 
svy linearized : proportion urban
svy linearized : proportion urban, over(gender)

tab prov
tab prov gender, col chi
svy linearized : proportion prov
svy linearized : proportion prov, over(gender)

tab educat
tab educat gender, col chi
svy linearized : proportion educat
svy linearized : proportion educat, over(gender)

tab employed
tab employed gender, col chi 
svy linearized : proportion employed
svy linearized : proportion employed, over(gender)

tab bmi_cat
tab bmi_cat gender, col chi

tab CURRSMOK
tab CURRSMOK gender, col chi

tab asset_index
tab asset_index gender, col chi

tab medaid
tab medaid gender, col chi


** Variables by asset_index (Table 1 alternative)


tab age, missing
tab gender, missing
tab urban, missing
tab prov, missing
tab educat, missing
tab employed, missing
count if ind_income == .
tab medaid, missing
tab bmi_cat, missing
tab CURRSMOK, missing

tabulate htn 
tab htn, missing
tab htn asset_index, col chi

tab SELFDIAB
tab SELFDIAB, missing
tab SELFDIAB asset_index, col chi

tabulate SELFHEART
tab SELFHEART, missing

tabulate SELFSTROKE
tabulate SELFSTROKE, missing


tab SELFSTROKE gender, col  chi
logistic SELFSTROKE gender
svy linearized : proportion SELFSTROKE
svy linearized : proportion SELFSTROKE, over(gender)

svy linearized : proportion SELFSTROKE, over(asset_index)


svy linearized : proportion SELFSTROKE, over(age_10)
svy linearized, subpop(if gender==1) : proportion SELFSTROKE, over(age_10)
svy linearized, subpop(if gender==2) : proportion SELFSTROKE, over(age_10)


