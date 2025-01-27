/*******************************************************************************
Data: MARCH 2024 : 1_elab_coe_filt - L. Bisio

This program starts from the firm/country/product dataset from COE source,
combining:
- the filtered original dataset "G:\Task_3.2_revisions\source_datasets\coefilt_11_19n.dta", #245,191 firms
- dataset centered on the sample of filtered firms that were included in the estimation (about 180k), 
  tracked for import/export flows in 2005-2010: source_datasets\coe0510_impexpf.dta (#85,427 firms)

The script follows steps to create old and new spike measures and relevant automation 
import variables, collapsing the dataset into an unbalanced panel where firm/year 
observations are valued only when firms were importers in that year.
*******************************************************************************/


*use "G:\Task_3.2_revisions\source_datasets\coefilt_11_19.dta";
use "G:\Task_3.2_revisions\source_datasets\coefilt_11_19n.dta", clear
append using "G:\Task_3.2_revisions\source_datasets\coe0510_impexpf.dta"
bysort codice_unita : gen nimp=_n
count if nimp==1
 245,191
count
 66,356,789


sort codice_unita anno
bysort codice_unita anno: egen tot_import_val=sum(import_val)
bysort codice_unita anno: egen tot_import_xue_val=sum(import_xue_val)
bysort codice_unita anno: egen tot_import_intra_val=sum(imp_intra_val)
bysort codice_unita anno: egen tot_export_val=sum(export_val)
bysort codice_unita anno: egen tot_export_xue_val=sum(export_xue_val)


*From nc8 a hs6
gen double hs6=floor(nc8/100)

/*Defining AUTOMATION techs */
*Industrial robots
gen flag_indrob= 0
recode flag_indrob (0=1) if hs6==847950
*Dedicated machinery
gen flag_dedmac=0
recode flag_dedmac(0=1) if hs6==847989
*Automatic machine tools 
gen flag_autmac=0
recode flag_autmac (0=1) if (hs6>=845600 & hs6<=846699)
recode flag_autmac(0=1) if (hs6>=846820 & hs6<=846899)
recode flag_autmac (0=1) if (hs6>=851511 & hs6<=851519)
*Automatic welding machines 
gen flag_weld=0
recode flag_weld (0=1) if hs6==851521
recode flag_weld (0=1) if hs6==851531
recode flag_weld (0=1) if hs6==851580
recode flag_weld (0=1) if hs6==851590
*Weaving and knitting machines 
gen flag_weav=0
recode flag_weav (0=1) if (hs6>=844600 & hs6<=844699)
recode flag_weav (0=1) if (hs6>=844700 & hs6<=844799)
*Other textile dedicated machinery 
gen flag_text=0
recode flag_text (0=1) if (hs6>=844400 & hs6<=844590)
* Automatic conveyors 
gen flag_conv=0
recode flag_conv (0=1) if (hs6>=842831 & hs6<=842839)
* Automatic regulating instruments 
gen flag_reg=0
recode flag_reg (0=1) if (hs6>=903200 & hs6<=903299)
*. 3-D printers 
gen flag_3dp=0
recode flag_3dp (0=1) if hs6==847780

gen flag_ai1= 0
* Automatic data processing machines
recode flag_ai1 (0=1) if (hs6>=847141 & hs6<=847150)
recode flag_ai1 (0=1) if hs6==847321
recode flag_ai1 (0=1) if hs6==847330
*Electronic calculating machines 
gen flag_ai2=0
recode flag_ai2 (0=1) if (hs6>=847010 & hs6<=847029)


*Generate flag import of goods robot+AI based on HS6 classification
***************        AUTOMATION     *****************
gen flag_autom= 0
*Dedicated machinery
recode flag_autom (0=1) if hs6==847989
*Automatic machine tools 
recode flag_autom (0=1) if (hs6>=845600 & hs6<=846699)
recode flag_autom (0=1) if (hs6>=846820 & hs6<=846899)
recode flag_autom (0=1) if (hs6>=851511 & hs6<=851519)
*Automatic welding machines 
recode flag_autom (0=1) if hs6==851521
recode flag_autom (0=1) if hs6==851531
recode flag_autom (0=1) if hs6==851580
recode flag_autom (0=1) if hs6==851590
*Weaving and knitting machines 
recode flag_autom (0=1) if (hs6>=844600 & hs6<=844699)
recode flag_autom (0=1) if (hs6>=844700 & hs6<=844799)
*Other textile dedicated machinery 
recode flag_autom (0=1) if (hs6>=844400 & hs6<=844590)
* Automatic conveyors 
recode flag_autom (0=1) if (hs6>=842831 & hs6<=842839)
* Automatic regulating instruments 
recode flag_autom (0=1) if (hs6>=903200 & hs6<=903299)
*. 3-D printers 
recode flag_autom (0=1) if hs6==847780

***************        AI     *****************
gen flag_ai= 0
* Automatic data processing machines
recode flag_ai (0=1) if (hs6>=847141 & hs6<=847150)
recode flag_ai (0=1) if hs6==847321
recode flag_ai (0=1) if hs6==847330
*Electronic calculating machines 
recode flag_ai (0=1) if (hs6>=847010 & hs6<=847029)


***************  ROBOT ****************************
gen flag_robot= 0
*Industrial robots
recode flag_robot (0=1) if hs6==847950

*************** AUTOMATION + ROBOT   **************
gen flag_autom_rob = flag_autom + flag_robot
recode flag_autom_rob(.=0)

*************** AUTOMATION + ROBOT + AI   **************
gen flag_autom_rob_ai = flag_autom + flag_robot + flag_ai
recode flag_autom_rob_ai(.=0)

**************** NEW AUTOMATION (excluding weaving and knitting and other textile, benchmark automation definition) ******************
gen flag_automn= 0
*Automatic machine tools 
recode flag_automn (0=1) if (hs6>=845600 & hs6<=846699)
recode flag_automn (0=1) if (hs6>=846820 & hs6<=846899)
recode flag_automn (0=1) if (hs6>=851511 & hs6<=851519)
*Automatic welding machines 
recode flag_automn (0=1) if hs6==851521
recode flag_automn (0=1) if hs6==851531
recode flag_automn (0=1) if hs6==851580
recode flag_automn (0=1) if hs6==851590
* Automatic conveyors 
recode flag_automn (0=1) if (hs6>=842831 & hs6<=842839)
* Automatic regulating instruments 
recode flag_automn (0=1) if (hs6>=903200 & hs6<=903299)
*. 3-D printers 
recode flag_automn (0=1) if hs6==847780

*****************NEW AUTOMATION + ROB *********************
gen flag_automn_rob = flag_automn + flag_robot
recode flag_automn_rob(.=0)

***************NEW AUTOMATION + ROB +AI *********************
gen flag_automn_rob_ai = flag_automn + flag_robot + flag_ai
recode flag_automn_rob_ai(.=0)

*** NEW DIGITAL TECH= ROB + AI + 3DPRINTERS*
gen flag_ndt= flag_robot + flag_ai + flag_3dp
recode flag_ndt (.=0)


/*Renaming import and export val variables */
rename import_val imp_val
rename export_val exp_val

*Creating variables with import/export value sums AI/AUTOM/AUTOM+AI
global var2 "imp exp"

foreach v in $var2 {

gen `v'_aut1=`v'_val*flag_autom
bysort codice_unita anno: egen `v'_autom=sum(`v'_aut1)
gen `v'_ai1=`v'_val*flag_ai
bysort codice_unita anno: egen `v'_ai=sum(`v'_ai1)
gen `v'_rob1=`v'_val*flag_robot
bysort codice_unita anno: egen `v'_robot=sum(`v'_rob1)
gen `v'_autrob1=`v'_val*flag_autom_rob
bysort codice_unita anno: egen `v'_aut_rob=sum(`v'_autrob1)
gen `v'_autrobai1=`v'_val*flag_autom_rob_ai
bysort codice_unita anno: egen `v'_aut_rob_ai=sum(`v'_autrobai1)
gen `v'_automn1=`v'_val*flag_automn
bysort codice_unita anno: egen `v'_automn=sum(`v'_automn1)
gen `v'_automnrob1=`v'_val*flag_automn_rob
bysort codice_unita anno: egen `v'_autn_rob=sum(`v'_automnrob1)
gen `v'_automnrobai1=`v'_val*flag_automn_rob_ai
bysort codice_unita anno: egen `v'_autn_rob_ai=sum(`v'_automnrobai1)
gen `v'_ndt1=`v'_val*flag_ndt
bysort codice_unita anno: egen `v'_ndt=sum(`v'_ndt1)

}

/*Reapeating for single technologies */
global var2 "imp exp"
global var3 "autmac weld conv reg 3dp ai1 ai2 "

foreach v in $var2 {
	foreach z in $var3 {
		gen `v'_`z'1=`v'_val*flag_`z'
		bysort codice_unita anno: egen `v'_`z'=sum(`v'_`z'1)
		drop `v'_`z'1
}
}

/*global var1 "autom ai robot aut_ai aut_rob aut_rob_ai automn autn_rob autn_rob_ai"
foreach v in $var1 {
rename import_`v' imp_`v' 
rename export_`v' exp_`v' 
}*/
save "G:\Task_3.2_revisions\source_datasets\coe0519.dta", replace



* Compute statistics of spike variables

collapse (mean)  tot_import_val tot_import_xue_val tot_import_intra_val tot_export_val tot_export_xue_val imp_ai imp_robot imp_aut_ai imp_autom imp_aut_rob imp_aut_rob_ai imp_automn imp_autn_rob imp_autn_rob_ai imp_ndt exp_ai exp_rob exp_autom exp_aut_rob exp_aut_rob_ai exp_automn exp_autn_rob exp_autn_rob_ai exp_ndt imp_autmac imp_weld imp_conv imp_reg imp_3dp imp_ai1 imp_ai2 exp_autmac exp_weld exp_conv exp_reg exp_3dp exp_ai1 exp_ai2 flag_indrob flag_indrob flag_autmac flag_weld flag_conv flag_reg flag_3dp flag_ai1 flag_ai2 flag_ai flag_robot flag_automn flag_automn_rob flag_automn_rob_a, by (codice_unita anno)

collapse (mean)  tot_import_val tot_import_xue_val tot_import_intra_val tot_export_val tot_export_xue_val imp_autmac imp_weld imp_conv imp_reg imp_3dp imp_ai1 imp_ai2 imp_ai imp_robot imp_autn_rob imp_autn_rob_ai, by (codice_unita anno)

* Save statistics tab
save "G:\Task_3.2_revisions\source_datasets\coe0519_coll.dta", replace


/*CREATION SPIKE VARIABILES - DEFLATED IMPs */


/* Merge with firm-level variables */

merge 1:1 codice_unita anno using "G:\Task_3.2_revisions\source_datasets\frame1119_selvar.dta",keepusing(ateco)
/*
 
    Result                           # of obs.
    -----------------------------------------
    not matched                     1,328,124
        from master                   390,225  (_merge==1)
        from using                    937,899  (_merge==2)

    matched                         1,049,029  (_merge==3)
    -----------------------------------------
*/
rename _merge mergefr
drop if mergefr==2
count if ateco==""
rename ateco atecofr

/*Merge with firm-level variables from ASIA register: ateco e number of employees*/

merge 1:1 codice_unita anno using "G:\Task_3.2_revisions\source_datasets\varasia.dta"
drop if _merge==2

gen atefrasia= ateco
destring atefrasia, replace
destring ateco, replace
destring atecofr, replace

recast double atefrasia
recast double atefr
recast double ateco

replace atefrasia=atecofr if atefrasia==.

count if atefrasia==.

/*Check observations with missing ateco (NACE) codes*/

 tab anno if atefrasia==.
/*
       ANNO |      Freq.     Percent        Cum.
------------+-----------------------------------
       2005 |      9,138       38.72       38.72
       2006 |     13,083       55.43       94.15
       2007 |          1        0.00       94.16
       2008 |          1        0.00       94.16
       2019 |      1,378        5.84      100.00
------------+-----------------------------------
      Total |     23,601      100.00
*/



*Imputation of NACE codes missing for 2005 and 2006 using 2017
bysort codice_unita: gen atefrasia07=atefrasia if anno==2007
bysort codice_unita: egen atefrasia07n=max(atefrasia07)
replace atefrasia=atefrasia07n if (atefrasia==. & anno==2006)
replace atefrasia=atefrasia07n if (atefrasia==. & anno==2005)
 
tab anno if atefrasia==.
/*
       ANNO |      Freq.     Percent        Cum.
------------+-----------------------------------
       2005 |        980       28.37       28.37
       2006 |      1,291       37.38       65.75
       2007 |          1        0.03       65.78
       2008 |          1        0.03       65.81
       2019 |      1,181       34.19      100.00
------------+-----------------------------------
      Total |      3,454      100.00
*/


preserve
	drop if atefrasia==.
	bysort codice_unita: egen atecom= mode(atefrasia)
	bysort codice_unita: egen atecom2= mode(atefrasia), minmode
	replace atecom=atecom2 if atecom==.
	br atecom atecom2
	keep codice_unita anno atecom
	collapse (mean) atecom , by (codice_unita)
	save "G:\Task_3.2_revisions\source_datasets\atemode.dta"
restore


merge m:1 codice_unita using "G:\Task_3.2_revisions\source_datasets\atemode.dta"
replace atefrasia= atecom if atefrasia==.
count if atefrasia==.

tab anno if atefrasia==.
drop _merge atecom atefrasia07n atefrasia07 ateco2007 ateco atecofr mergefr
rename atefrasia ateco

***Recover NACE 3 digits definining sectors tech/kis***
tostring ateco, gen(atecostr)
gen ateco5= "0" + substr(atecostr, 1, 4) if strlen(atecostr) < 5
replace ateco5=atecostr if ateco5==""

drop atecostr
rename ateco5 atecostr

*drop ateco
gen ate2 = substr(atecostr,1,2)
gen ate2n = real(ate2)
gen ate3 = substr(atecostr,1,3)
gen ate3n = real(ate3)

drop atesez
gen str atesez="."
replace atesez="B" if ate2n >=05 & ate2n<=09
replace atesez="C" if ate2n >=10 & ate2n<=33
replace atesez="D" if ate2n ==35
replace atesez="E" if ate2n >=36 & ate2n<=39
replace atesez="F" if ate2n >=40 & ate2n<=43
replace atesez="G"  if ate2n >=44 & ate2n<=47
replace atesez="H"  if ate2n >=48 & ate2n<=53
replace atesez="I"  if ate2n >=54 & ate2n<=57
replace atesez="J" if ate2n >=58 & ate2n<=63
replace atesez="K" if ate2n >=64 & ate2n<=66
replace atesez="L" if ate2n >=67 & ate2n<=68
replace atesez="M" if ate2n >=69 & ate2n<=75
replace atesez="N" if ate2n >=76 & ate2n<=82
replace atesez="O" if ate2n==84
replace atesez="P" if ate2n==85
replace atesez="Q" if ate2n >=86 & ate2n<=88
replace atesez="R" if ate2n >=89 & ate2n<=93
replace atesez="S" if ate2n >=94 & ate2n<=96
replace atesez="T" if ate2n >=97

count if atesez==""

replace atecostr="" if ateco==.
replace ate2="" if ateco==.
replace ate3="" if ateco==.
replace atesez="" if ateco==.
replace ate2n=. if ateco==.
replace ate3n=. if ateco==.


/*Create size class for firms*/
gen clad=1 if addetti<10
replace clad=2 if addetti >=10 &  addetti<50
replace clad=3 if addetti >=50 &  addetti<250
replace clad=4 if addetti >=250


****************************************************************
************* Create spike variables per importers *************
****************************************************************

**Investment in imported automated capital goods, AI imported capital goods and entire category
gen importer_autom=(imp_autom!=0 )
gen importer_ai=(imp_ai!=0 )
gen importer_robot=(imp_robot!=0) 
gen importer_aut_rob=(imp_aut_rob!=0) 
gen importer_aut_rob_ai=(imp_aut_rob_ai!=0) 
gen importer_automn=(imp_automn!=0 )
gen importer_autn_rob=(imp_autn_rob!=0 )
gen importer_autn_rob_ai=(imp_autn_rob_ai!=0 )
gen importer_ndt=(imp_ndt!=0 )

global var4 "autmac weld conv reg 3dp ai1 ai2"

foreach y in $var4 {
	gen importer_`y'=(imp_`y'!=0 )
	}


**Generate spike variables
foreach i in autom ai robot aut_rob aut_rob_ai automn autn_rob autn_rob_ai ndt{
*egen cum_`i'=total(`i'_defl), by(codice_unita)
egen cum_`i'=total(imp_`i'), by(codice_unita)
*gen year_`i'_share=`i'_defl/cum_`i'
gen year_`i'_share=imp_`i'/cum_`i'
egen rank_`i'=rank(year_`i'_share) if cum_`i'>0, by(codice_unita) field
}
*drop cum_* 

foreach i in autom ai robot aut_rob aut_rob_ai automn autn_rob autn_rob_ai ndt{
gen spike_`i'=(importer_`i'==1 & rank_`i'==1)
egen imp_`i'_years=total(importer_`i'), by(codice_unita)
}

*MEASURE of SPIKE (#2) WITHIN: imp_val max by firm and greater than 3 times the import val average in other years
foreach i in autom ai robot aut_rob aut_rob_ai automn autn_rob autn_rob_ai ndt{
egen rankw_`i'=rank(imp_`i') if cum_`i'>0, by(codice_unita) field
bysort codice_unita: egen avew_`i'=mean(imp_`i') if rankw_`i'>1
bysort codice_unita: egen mx_avew_`i'=max(avew_`i')
drop avew_`i'
gen trip_mx_avew_`i'=3*mx_avew_`i'
gen spike2_`i'=(importer_`i'==1 & rankw_`i'==1 & imp_`i'>trip_mx_avew_`i')
}

*MEASURE SPIKE #3 BETWEEN: imp_val max by firm and greater than median of ateco/size class in the year

* Compute median for ateco and size
foreach i in autom ai robot aut_rob aut_rob_ai automn autn_rob autn_rob_ai ndt{
bysort ateco clad anno: egen str_imp_`i'=mean(imp_`i')
}

* Assign spike variables
foreach i in autom ai robot aut_rob aut_rob_ai automn autn_rob autn_rob_ai ndt{
gen spike3_`i'=(importer_`i'==1 & rankw_`i'==1 & imp_`i'>=str_imp_`i')
}


*MEASURE SPIKE #4: INTERSECTION OF MEASURES #2 e #3
foreach i in autom ai robot aut_rob aut_rob_ai automn autn_rob autn_rob_ai ndt{
gen spike4_`i'=spike2_`i'*spike3_`i'
}

bysort codice_unita: gen nimp=_n
count if nimp==1


*COMPUTE NUMBER OF ADOPTERS PER YEAR AND SPIKE TYPE
foreach i in autom ai robot aut_rob aut_rob_ai automn autn_rob autn_rob_ai ndt{
bysort anno: egen nsp1=sum(spike_`i')
bysort anno: egen nsp2=sum(spike2_`i')
bysort anno: egen nsp3=sum(spike3_`i')
bysort anno: egen nsp4=sum(spike4_`i')
}
gen sh_ad2=nsp2/nsp1
gen sh_ad3=nsp3/nsp1
gen sh_ad4=nsp4/nsp1

tabstat sh_ad2, by(anno)
tabstat sh_ad3, by(anno)
tabstat sh_ad4, by(anno)

save "G:\Task_3.2_revisions\source_datasets\coe0519_coll_newspk.dta", replace


/*#1 CLEAN INTERMEDIARIES */

/*Drop NACE codes 46140, 46190, 46500, 46699, 46900*/
drop if ateco==46140 | ateco==46190 |  ateco==46510 | ateco==46520 | ateco ==46699 |  ateco==46900

/*Drop other machine intermediaries */
 drop if ateco==46620 | ateco==46640
*(12,585 observations deleted)

bysort codice_unita: gen nimp3= _n
count if nimp3 ==1 

 /*#2 CLEAN MISSING NACE CODES */
count if ateco==.
bysort codice_unita: gen nimp5= _n if ateco==.
count if nimp5==1
 
drop if ateco==.
count
drop nimp*
bysort codice_unita: gen nimp= _n
count if nimp ==1


save "G:\Task_3.2_revisions\source_datasets\coe0519_coll_newspk_clean.dta", replace

bysort anno: gen nimpy=_N
tabstat nimpy, by(anno) stat(max)


use "G:\Task_3.2_revisions\source_datasets\coe0519_coll_newspk_clean.dta",  clear


keep anno-imp_autom_years spike_ndt spike_autn_rob_ai spike_autn_rob spike_automn spike_aut_rob_ai spike_aut_rob spike_robot spike_ai spike2_ndt spike2_autn_rob_ai spike2_autn_rob spike2_automn spike2_aut_rob_ai spike2_aut_rob spike2_robot spike2_ai spike2_autom spike3_autom-nsp4

drop atecostr ate2 ate3 atesez

collapse (mean) tot_import_val-nsp4, by (codice_unita anno)
save "G:\Task_3.2_revisions\source_datasets\coe0519_coll_newspk_clean_red.dta", replace


count 
  
bysort codice_unita: gen nimp=_n
count if nimp==1


**********************************************************************************
* Correlation of spikes (spike2 - within autom-rob-ai-automnew) - NEW TETRACHORIC SU CAMPIONE COLLAPSED
**********************************************************************************

bysort codice_unita: egen adopt1_autom=sum(spike_autom)
bysort codice_unita: egen adopt1_automn=sum(spike_automn)
bysort codice_unita: egen adopt1_ai=sum(spike_ai)
bysort codice_unita: egen adopt1_rob=sum(spike_rob)

bysort codice_unita: egen adopt2_autom=sum(spike2_autom)
bysort codice_unita: egen adopt2_automn=sum(spike2_automn)
bysort codice_unita: egen adopt2_ai=sum(spike2_ai)
bysort codice_unita: egen adopt2_rob=sum(spike2_robot)

bysort codice_unita: egen adopt3_autom=sum(spike3_autom)
bysort codice_unita: egen adopt3_automn=sum(spike3_automn)
bysort codice_unita: egen adopt3_ai=sum(spike3_ai)
bysort codice_unita: egen adopt3_rob=sum(spike3_robot)


gen str atesez="."
replace atesez="A" if ate2n <5
replace atesez="B" if ate2n >=5 & ate2n<=9
replace atesez="C" if ate2n >=10 & ate2n<=33
replace atesez="D" if ate2n >=34 & ate2n<=35
replace atesez="E" if ate2n >=36 & ate2n<=40
replace atesez="F" if ate2n >=41 & ate2n<=43
replace atesez="GHI" if ate2n >=44 & ate2n<=57
replace atesez="J" if ate2n >=58 & ate2n<=63
replace atesez="K" if ate2n >=64 & ate2n<=66
replace atesez="L" if ate2n >=67 & ate2n<=69
replace atesez="MN" if ate2n >=70 & ate2n<=82
replace atesez="OPQ" if ate2n >=83 & ate2n<=88
replace atesez="RST" if ate2n >=89 & ate2n<=96
gen manser=.
replace manser=1 if atesez=="C"
replace manser=2 if ate2n>=44
recode manser(.=0)

save "G:\Task_3.2_revisions\source_datasets\coe0519_coll_newspk_clean_red.dta", replace
use "G:\Task_3.2_revisions\source_datasets\coe0519_coll_newspk_clean_red.dta", clear



bysort codice_unita: gen nimp=_n
count if nimp==1
*#179,788

/* Remove observations/firms that have multiple spikes in the period: these occurrences happen 
when a firm, during its existence period, imports the same value amount in the same automation 
category in 2 different years; due to how the spike is defined (original version), in those 
two years 2 "spikes" occur and, since the "adopter" variable is obtained as the sum of the 
spike variable by firm, in these cases the adopters variable is greater than 1 (we also have 
cases of adopters=3). This also happens for within and between spikes. By removing these firms, 
the sample goes from 179,778 to 179,709 firms. */

foreach y in 1 2 3 {
drop if adopt`y'_autom ==2
drop if adopt`y'_autom ==3
drop if adopt`y'_automn ==2
drop if adopt`y'_automn ==3
drop if adopt`y'_rob ==2
drop if adopt`y'_rob ==3
drop if adopt`y'_ai ==3
drop if adopt`y'_ai ==2
}

 *PREPARE FILE SUMMARY FOR SPIKES NEWSPIKE.XLSX*/
bysort anno: gen nimpy=_N
tabstat nimpy, by(anno) stat(max)


/*SPIKE NUMBERS FOR TAVOLA NEWSPIKE.XLSX*/
tabstat spike_autom spike_ai spike_rob spike_aut_rob spike_aut_rob_ai  spike_automn spike_autn_rob spike_autn_rob_ai spike_ndt, by(anno) stat(sum) 
tabstat spike2_autom spike2_ai spike2_robot spike2_aut_rob spike2_aut_rob_ai  spike2_automn spike2_autn_rob spike2_autn_rob_ai spike2_ndt, by(anno) stat(sum) 
tabstat spike3_autom spike3_ai spike3_robot spike3_aut_rob spike3_aut_rob_ai  spike3_automn spike3_autn_rob spike3_autn_rob_ai spike3_ndt, by(anno) stat(sum) 
tabstat spike4_autom spike4_ai spike4_robot spike4_aut_rob spike4_aut_rob_ai  spike4_automn spike4_autn_rob spike4_autn_rob_ai spike4_ndt, by(anno) stat(sum) 

count if nimp==1


preserve
	collapse (mean) adopt1_rob adopt1_ai adopt1_automn adopt1_autom adopt2_rob adopt2_ai adopt2_automn adopt2_autom adopt3_rob adopt3_ai adopt3_automn adopt3_autom ate2n manser, by(codice_unita)
	
	*CONTINGENCY TABLES
	tab adopt1_autom adopt1_automn
	tab adopt2_autom adopt2_automn
	tab adopt3_autom adopt3_automn
	

	tab adopt1_automn adopt1_rob
	tab adopt1_autom adopt1_rob
	tab adopt2_automn adopt2_rob
	tab adopt2_autom adopt2_rob
	tab adopt3_automn adopt3_rob
	tab adopt3_autom adopt3_rob
	
		
	tab adopt1_automn adopt1_ai
	tab adopt1_autom adopt1_ai
	tab adopt2_automn adopt2_ai
	tab adopt2_autom adopt2_ai
	tab adopt3_automn adopt_ai
	tab adopt3_autom adopt3_ai
	
	tab adopt1_rob adopt1_ai
	tab adopt2_rob adopt2_ai
	tab adopt3_rob adopt3_ai
	
	

	tetrachoric adopt1_autom adopt1_automn adopt1_rob adopt1_ai
	tetrachoric adopt2_autom adopt2_automn adopt2_rob adopt2_ai
	tetrachoric adopt3_autom adopt3_automn adopt3_rob adopt3_ai
	
	*SPIKE 2 manufacturing
	tetrachoric adopt2_autom adopt2_automn adopt2_rob adopt2_ai if manser==1
	*SPIKE 2 services
	tetrachoric adopt2_autom adopt2_automn adopt2_rob adopt2_ai if manser==2
	

restore

count


save "G:\Task_3.2_revisions\source_datasets\coe0519_coll_newspk_clean_red.dta", replace



*Generate dummy 0/1 adopter spike2 in the 4 automation classifications
gen adopt=spike2_autom+spike2_ai+spike2_robot+spike2_automn
bysort codice_unita: egen adopter2=max(adopt)
recode adopter2 (2=1) (3=1) (4=1)
tab adopter2

*Generate dummy 0/1 adopter spike2 of all possible types of automation
gen adopt2n=spike2_autom+spike2_ai+spike2_robot+ spike2_aut_rob+ spike2_aut_rob_ai+ spike2_automn+ spike2_autn_rob+ spike2_autn_rob_ai+ spike2_ndt
bysort codice_unita: egen adopter2n=max(adopt2n)
recode adopter2n (2=1) (3=1) (4=1) (5=1) (6=1) (7=1) (8=1) (9=1)
tab adopter2 adopter2n
/*
           |       adopter2n
  adopter2 |         0          1 |     Total
-----------+----------------------+----------
         0 | 1,179,743        221 | 1,179,964 
         1 |         0    377,940 |   377,940 
-----------+----------------------+----------
     Total | 1,179,743    378,161 | 1,557,904 
*/

*Correlation on all the sample
count 
count if nimp==1
pwcorr spike2_autom spike2_automn spike2_robot spike2_ai

count if atesez=="C"
count if nimp5==1 & atesez=="C"
pwcorr spike2_autom spike2_automn spike2_robot spike2_ai if atesez=="C"

count if atesez=="C"
count if nimp5==1 & (atesez!="B" & atesez!="C" & atesez!="D" & atesez!="E" & atesez!="F")
pwcorr spike2_autom spike2_automn spike2_robot spike2_ai if (atesez!="B" & atesez!="C" & atesez!="D" & atesez!="E" & atesez!="F")

*Correlations on adopters spike2 by automation/autonew/rob/ai
count if adopter2==1
count if nimp5==1 & adopter2==1
pwcorr spike2_autom spike2_automn spike2_robot spike2_ai & adopter2==1

count if atesez=="C" & adopter2==1
count if nimp5==1 & atesez=="C" & adopter2==1
pwcorr spike2_autom spike2_automn spike2_robot spike2_ai if atesez=="C" & adopter2==1

count if (atesez!="B" & atesez!="C" & atesez!="D" & atesez!="E" & atesez!="F") & adopter2==1
count if nimp5==1 & (atesez!="B" & atesez!="C" & atesez!="D" & atesez!="E" & atesez!="F") & adopter2==1
pwcorr spike2_autom spike2_automn spike2_robot spike2_ai if (atesez!="B" & atesez!="C" & atesez!="D" & atesez!="E" & atesez!="F") & adopter2==1

*Correlation on adopters spike2 
count if adopter2n==1
count if nimp5==1 & adopter2n==1
pwcorr spike2_autom spike2_automn spike2_robot spike2_ai & adopter2n==1

count if atesez=="C" & adopter2n==1
count if nimp5==1 & atesez=="C" & adopter2n==1
pwcorr spike2_autom spike2_automn spike2_robot spike2_ai if atesez=="C" & adopter2n==1

count if (atesez!="B" & atesez!="C" & atesez!="D" & atesez!="E" & atesez!="F") & adopter2n==1
count if nimp5==1 & (atesez!="B" & atesez!="C" & atesez!="D" & atesez!="E" & atesez!="F") & adopter2n==1
pwcorr spike2_autom spike2_automn spike2_robot spike2_ai if (atesez!="B" & atesez!="C" & atesez!="D" & atesez!="E" & atesez!="F") & adopter2n==1
  
   
