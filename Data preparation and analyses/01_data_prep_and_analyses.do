
********************************************************************************
* Data code for Widding-Havneraas, Demange, Elwert, Borgen, Ystrom, Zachrisson
* Estimating returns to education using genetically informed designs
* 
* Programming:
* 	Stata version: Stata MP 18
*	Original author: TW
* 	Contact: tarjei.widding-havneras@create.uio.no
*	Created: Nov-11-2023
*	Last modified: Aug-15-2025
********************************************************************************
* Data is based on raw files as issued by Statistics Norway and MoBa 

********************************************************************************
**# A. DATA PREPARATION
********************************************************************************

********************************************************************************
**# 1. Population sample and demographic data
********************************************************************************	
	cd "N:\durable\projects\EQOP\MR_Income\temp"
	sysdir set PLUS "C:\Users\p805-tarjeiw\Documents\Stata\PlusPersonal\plus"
	* Import population data file
	import delimited "N:\durable\data\registers\SSB\01_data\data_v4.0\csv\POPULATION_FASTE_OPPLYSNINGER.csv"
	save fast.dta, replace
	* Birth year
		cap destring foedsels_aar_mnd doeds_aar_mnd, replace
		gen faar=int(foedsels_aar_mnd/100)
		gen daar=int(doeds_aar_mnd/100)
	* Birth month	
		gen bmonth = real(substr(string(foedsels_aar_mnd),5,2))	
	* Country of birth 
	// https://www.ssb.no/klass/klassifikasjoner/545
		cap destring fodeland, replace
		gen nor=.
		replace nor=1 if fodeland ==000 
		replace nor=0 if fodeland !=000  
		gen eur=.
		replace eur=1 if fodeland >100 & fodeland<200
		replace eur=0 if fodeland <100 | (fodeland>200 & fodeland!=.)
		gen afr=.
		replace afr=1 if fodeland >200 & fodeland<400 
		replace afr=0 if fodeland <200 | (fodeland>400 & fodeland!=.)
		gen asi=.
		replace asi=1 if fodeland >400 & fodeland<600 
		replace asi=0 if fodeland <400 | (fodeland>600 & fodeland!=.)
		gen ncam=.
		replace ncam=1 if fodeland >600 & fodeland<700 
		replace ncam=0 if fodeland <600 | (fodeland>700 & fodeland!=.)
		gen sam=.
		replace sam=1 if fodeland >700 & fodeland<800 
		replace sam=0 if fodeland <700 | (fodeland>800 & fodeland!=.)
		gen oce=.
		replace oce=1 if fodeland >800 & fodeland<900 
		replace oce=0 if fodeland <800 | (fodeland>900 & fodeland!=.)
	* Gender
		gen male=.
		replace male=1 if kjoenn==1
		replace male=0 if kjoenn==2
		save all_sample_preres.dta, replace
	* Add parental variables: birth year, death, country of birth
	frame create faar_f
	frame change faar_f
		use w19_0634_lnr faar daar eur nor afr asi ncam sam oce ///
		using all_sample_preres.dta, clear
		ren w19_0634_lnr lopenr_far
		ren (faar daar eur nor afr asi ncam sam oce) ///
		(faar_f daar_f eur_f nor_f afr_f asi_f ncam_f sam_f oce_f)
		sort lopenr_far
		save "temp_f_all.dta", replace
	frame create faar_m
	frame change faar_m
		use w19_0634_lnr faar daar eur nor afr asi ncam sam oce /// 
		using all_sample_preres.dta, clear
		ren w19_0634_lnr lopenr_mor
		ren (faar daar eur nor afr asi ncam sam oce) ///
		(faar_m daar_m eur_m nor_m afr_m asi_m ncam_m sam_m oce_m)
		sort lopenr_mor
		save "temp_m_all.dta", replace	
	frame change default
		merge m:1 lopenr_far using "temp_f_all.dta", keep (3 1) nogen ///
		keepus(faar_f daar_f eur_f nor_f afr_f asi_f ncam_f sam_f oce_f)
		merge m:1 lopenr_mor using "temp_m_all.dta", keep (3 1) nogen ///
		keepus(faar_m daar_m eur_m nor_m afr_m asi_m ncam_m sam_m oce_m)
	* Parents age at birth 
		sum faar faar_f faar_m
		gen f_age_birth = faar-faar_f
		gen m_age_birth = faar-faar_m
		sum f_age_birth m_age_birth, d
	// bottomcode <1%: set 14 as lowest
		fre f_age_birth, all
		fre m_age_birth, all
		replace f_age_birth = 14 if f_age_birth <= 14 & f_age_birth != . 
		replace m_age_birth = 14 if m_age_birth <= 14 & m_age_birth != . 
		sum f_age_birth m_age_birth
		gen famage_birth = (f_age_birth+m_age_birth)/2
	* Sample restriction
	* 1. Keep Norwegians
		keep if nor==1 
		keep if nor_f==1 | nor_m==1
		// drop non-european parents
		drop if asi_f==1 | asi_m==1
		drop if afr_f==1 | afr_m==1
		drop if sam_f==1 | sam_m==1
		drop if ncam_f==1 | ncam_m==1
		drop if oce_f==1 | oce_m==1
		sum *_f *_m
	* 2. Keep all with registered mother & father
		keep if lopenr_mor!="" & lopenr_far!=""
	* 3. Keep study cohorts
		keep if faar>=1959 & faar<=1982
	* Keep variables
		keep w19_0634_lnr lopenr_mor lopenr_far faar daar bmonth nor ///
		male faar_f daar_f nor_f eur_f faar_m daar_m nor_m eur_m ///
		f_age_birth m_age_birth famage_birth
		save all_sample.dta, replace	
	
********************************************************************************
**# 2. Education data
********************************************************************************	
*SOURCE Education registry for the population
********************************************************************************
	* 1. Add education 
	import delimited "N:\durable\data\registers\SSB\01_data\data_v4.0\csv\EDUCATION_BU_UTD.csv"
	save utd_1970_2022.dta
	* 1970, 1980/2022
	foreach yr of numlist 1970 1980/2022 {
	use w19_0634_lnr foedsels_aar_mnd using fast.dta, clear
	cap destring foedsels_aar_mnd, replace
	gen faar=int(foedsels_aar_mnd/100)
	sort w19_0634_lnr
	save all_edu_`yr'.dta, replace
	preserve
		use w19_0634_lnr bu_`yr' using utd_1970_2022.dta
		*dropping duplicates with the lowest value
		duplicates t w19_0634_lnr , gen (dup_ed)
		gsort dup_ed w19_0634_lnr - bu_`yr'
		duplicates drop w19_0634_lnr, force
		drop dup_ed
		cap destring bu_`yr', replace
		gen edu_all_`yr'=int(bu_`yr'/100000)
		sort w19_0634_lnr
		save temp.dta, replace
	restore 
	merge m:1 w19_0634_lnr using temp.dta, keep (3) keepus(edu_all_`yr') nogen
	save all_edu_`yr'.dta, replace
	}
	* Merge files
	use w19_0634_lnr using fast.dta, clear
	foreach yr of numlist 1970 1980/2022 {
		merge 1:1 w19_0634_lnr using all_edu_`yr'.dta, nogen
		save all_ed_1970_2022.dta, replace
	}
	* 2. Educational attainment age 33
	use all_ed_1970_2022.dta, clear
	keep if faar >=1959 & faar<=1982
	gen yr33 = faar+33
	gen educ33 = .
	foreach yr of numlist 1970 1980/2022 {
		replace educ33 = edu_all_`yr' if yr33==`yr'
	}
	* Label
	recode educ33 0=0 1=6 2=9 3=11 4=12 5=13 6=16 7=18 8=23
	label define educlbl /// 
			6 "PS" /// 			 // 1 "1-7gr" 
			9 "LSS" /// 		 // 2 lower secondary school, "8-10gr" 
			11 "Some HS" /// 	 // 3 "some HS"
			12 "Complete HS" /// // 4 "complete high school" 
			13 "Extra HS" /// 	 // 5 "extra high school" 
			16 "BA" ///  		 //	6 "BA"
			18 "MA" ///  		 // 7 "MA"
			23 "PhD", modify 	 // 8 "PhD"
	label values educ33 educlbl
	keep w19_0634_lnr educ33
	save educ_age33.dta, replace
	* Merge education to sample 
	use all_sample.dta, clear
	merge 1:1 w19_0634_lnr using educ_age33.dta, keep(3) nogen
	clonevar yos=educ33
	* Few final sample with 0/6; keep with yos
	replace yos=. if educ33<=6
	keep if yos!=.
	drop educ33
	* College	
	gen college = .
	replace college = 1 if yos >= 16 & yos != .
	replace college = 0 if yos < 16
	label define collegelbl /// 
		0 "Less than college" ///
		1 "College", modify
	label values college collegelbl
	save all_sample_educ.dta, replace
	
	* Parents education lvl when individuals' age 11
	/* For cohort 1959-1982 => year 1970-1993
	As no education data 1971-1979; parents educ lvl 1980
	used for those without educ data in 1970 */
	* Father and mother files
	* Individuals="child" (barn_lnr) and parents="w19_0634_lnr"
	use all_sample_educ.dta, clear 	 
	frame copy default father
	frame copy default mother
	frame change father
		unique lopenr_far
		unique w19_0634_lnr if lopenr_far == ""
		keep lopenr_far faar_f daar_f w19_0634_lnr faar
		rename (w19_0634_lnr faar lopenr_far) (barn_lnr faar_b w19_0634_lnr)
		sort w19_0634_lnr
		save father_child.dta, replace
	frame change mother
		unique lopenr_mor
		unique w19_0634_lnr if lopenr_mor=="" 
		keep lopenr_mor faar_m daar_m w19_0634_lnr faar 
		rename (w19_0634_lnr faar lopenr_mor) (barn_lnr faar_b w19_0634_lnr)
		save mother_child.dta, replace
	frame change default 
	frame drop father
	frame drop mother 
	* Parents education when age 11
		* Father
		use all_ed_1970_2022.dta, clear
		keep w19_0634_lnr edu_all_1970 edu_all_1980-edu_all_1993
		merge 1:m w19_0634_lnr using father_child.dta, keep(3) nogen
		gen yr_child_11 = faar_b+11
		sum yr_child_11
		gen feduc_age_11 = .
		replace edu_all_1970 = edu_all_1980 if edu_all_1970 == .
		replace feduc_age_11 = edu_all_1970 if inrange(yr_child_11,1970,1979)
		forvalues yr = 1980(1)1993 {
		replace feduc_age_11 = edu_all_`yr' if yr_child_11==`yr'
		}
		keep w19_0634_lnr barn_lnr feduc_age_11
		rename (w19_0634_lnr barn_lnr) (lopenr_far w19_0634_lnr)
		recode feduc_age_11 0=. 1=6 2=9 3=11 4=12 5=13 6=16 7=18 8=23
		label define edulvl /// 
			6 "PS" /// 			 // 1 "1-7gr" 
			9 "LSS" /// 		 // 2 lower secondary school, "8-10gr" 
			11 "Some HS" /// 	 // 3 "some HS"
			12 "Complete HS" /// // 4 "complete high school" 
			13 "Extra HS" /// 	 // 5 "extra high school" 
			16 "BA" ///  		 //	6 "BA"
			18 "MA" ///  		 // 7 "MA"
			23 "PhD", modify 	 // 8 "PhD"
		label values feduc edulvl
		save feduc_age_11.dta, replace
		* Mother
		use all_ed_1970_2022.dta, clear
		keep w19_0634_lnr edu_all_1970 edu_all_1980-edu_all_1993
		merge 1:m w19_0634_lnr using mother_child.dta, keep(3) nogen
		gen yr_child_11 = faar_b+11
		gen meduc_age_11 = .
		replace edu_all_1970 = edu_all_1980 if edu_all_1970 == .
		replace meduc_age_11 = edu_all_1970 if inrange(yr_child_11,1970,1979)
		forvalues yr = 1980(1)1993 {
		replace meduc_age_11 = edu_all_`yr' if yr_child_11==`yr'
		}
		keep w19_0634_lnr barn_lnr meduc_age_11
		rename (w19_0634_lnr barn_lnr) (lopenr_mor w19_0634_lnr)
		recode meduc_age_11 0=. 1=6 2=9 3=11 4=12 5=13 6=16 7=18 8=23
		label define edulvl /// 
			6 "PS" /// 			 // 1 "1-7gr" 
			9 "LSS" /// 		 // 2 lower secondary school, "8-10gr" 
			11 "Some HS" /// 	 // 3 "some HS"
			12 "Complete HS" /// // 4 "complete high school" 
			13 "Extra HS" /// 	 // 5 "extra high school" 
			16 "BA" ///  		 //	6 "BA"
			18 "MA" ///  		 // 7 "MA"
			23 "PhD", modify 	 // 8 "PhD"
		label values meduc edulvl
		save meduc_age_11.dta, replace
	* Merge parents education to sample
	* Cohorts 1959-1982
		use all_sample_educ.dta, clear
		merge 1:1 w19_0634_lnr using feduc_age_11, keep(1 3) nogen
		merge 1:1 w19_0634_lnr using meduc_age_11, keep(1 3) nogen
		save all_sample_educ.dta, replace
		
********************************************************************************
**# 3. Earnings data
********************************************************************************

	use "N:\durable\data\registers\SSB\01_data\data_v5.0\dta\INCOME_SKATT_PENSJON.dta", clear
	save pincome_copy.dta, replace
	destring aargang, replace
	rename pensjgiv_inntekt pincome
	* Highest id-year value and nonmissing circumvent low-high/high-high duplicates 
	drop if missing(pincome)
	collapse (max) pincome, by(w19_0634_lnr aargang)
	save pincome.dta, replace
	* Subtract disability insurance (uforetrygd) from earnings >=2015-2020 consistent with Kirkeboen/SSB
/* Eika & Kirkeboen 2023, p.10: "F.o.m. 2015 har SSB inkludert uføretrygd i sitt mål for pensjonsgivende inntekt, siden uføretrygd gav pensjonsopptjening fra dette året. Vi trekker uføretrygden ut av 	pensjonsgivende inntekt; både for å ha et mål som er konsistent over tid, og for å være konsistent med den juridiske forståelsen av pensjonsgivende inntekt (folketrygdloven §3-15). See also SSB Table 09855: For inntektsårene fra og med 2015 til og med 2020 har personinntekt fra uføretrygd feilaktig blitt lagt til i pensjonsgivende inntekt. Vi jobber med å rette feilen."*/
frame create pincome_sub_di
frame change pincome_sub_di
	use "N:\durable\data\registers\SSB\01_data\data_v5.0\dta\INCOME_INNTEKT.dta", clear
	keep w19_0634_lnr aargang uforetrygd
	destring aargang, replace
	keep if aargang>=2015 & aargang<=2020
	quietly bys w19_0634_lnr aargang: gen dup = cond(_N==1,0,_n)
	drop if dup==2	
	drop dup
	save uforetrygd_2015_2020.dta, replace
frame change default
frame drop pincome_sub_di
	* Generate earnings subtracting DI
 	merge 1:1 w19_0634_lnr aargang using uforetrygd_2015_2020.dta, keep(1 3) nogen
	clonevar pincome_sub_di=pincome 
	replace pincome_sub_di=pincome-uforetrygd if uforetrygd!=.
	drop uforetrygd
	rename (pincome pincome_sub_di) (raw_pincome pincome)
* Inflation-adjust earnings:
** Wage adjust (Bhuller)
/* 	
* From Bhuller, Mogstad, Salvanes (2017) JPE
"* Annual Wage Index - Full Time Equivalent Annual Wages (average growth 7.6 % p.a.)
* Sources:
* 1967-2002: Stein Hansen og Tor Skoglund (2003): "Lonnsutviklingen 1962-2002", 
* Okonomiske analyser 5/2003. 
* Tabell 3. Lonn per normalaarsverk, nominelt og reelt. Gjennomsnitt for alle naeringer.
* "Lonn per normalaarsverk i kroner"
* https://www.ssb.no/a/kortnavn/hist_tab/ht-0901-lonn.html
*2003-2005: NOU 2013:7 "Grunnlaget for inntektsoppgjorene 2013", Vedlegg 2.
* Tabell 2.2 Lonnsnivaa per aarsverk. 1000 kroner (rad "Lonn per arsverk")
*           https://www.regjeringen.no/contentassets/269972a082a64f679bcc38ff02d03eee/no/pdfs/nou201320130007000dddpdfs.pdf
* 2006-2015: NOU 2016:6 "Grunnlaget for inntektsoppgj¿rene 2016", Vedlegg 2.
* Tabell 2.2 L¿nnsnivŒ per Œrsverk. 1000 kroner (rad "Lonn per aarsverk")
*           https://www.regjeringen.no/contentassets/c4743e5493dd4464ac16cd0d588717a0/no/pdfs/nou201620160006000dddpdfs.pdf"
Note: 
Bhuller used preliminary 2014 and 2015 numbers from Table 2.2., which we replace with final numbers  
Updated numbers up to 2021 are used for 2012-2020: NOU 2023:12 "Grunnlaget for inntektsoppgjørene 2023", vedlegg 6, p.225.
Tabell 6.3 Lønnsnivå og lønnskostnader per årsverk. 1000 kr (total, rad "Lonn per aarsverk")
for 2020-2021: NOU 2024:6 "Grunnlaget for inntektsoppgjørene 2024", vedlegg 6, p.223.
Tabell 6.3 Lønnsnivå og lønnskostnader per årsverk. 1000 kr (total, rad "Lonn per aarsverk")
*/
	scalar w2022 = 648000
	scalar w2021 = 619100
	scalar w2020 = 593900
	scalar w2019 = 581200
	scalar w2018 = 561900
	scalar w2017 = 546100
	scalar w2016 = 535700
	scalar w2015 = 528000
	scalar w2014 = 514000
	scalar w2013 = 499600
	scalar w2012 = 479900
	scalar w2011 = 461400 
	scalar w2010 = 442300
	scalar w2009 = 425300
	scalar w2008 = 410400
	scalar w2007 = 389000
	scalar w2006 = 370300
	scalar w2005 = 356500
	scalar w2004 = 345500
	scalar w2003 = 331700
	scalar w2002 = 320310
	scalar w2001 = 303840
	scalar w2000 = 288580
	scalar w1999 = 276020
	scalar w1998 = 261880
	scalar w1997 = 245870
	scalar w1996 = 234530
	scalar w1995 = 224550
	scalar w1994 = 217280
	scalar w1993 = 210920
	scalar w1992 = 203900
	scalar w1991 = 196290
	scalar w1990 = 186710
	scalar w1989 = 178180
	scalar w1988 = 170730
	scalar w1987 = 161140
	scalar w1986 = 148200
	scalar w1985 = 135830
	scalar w1984 = 126320
	scalar w1983 = 117380
	scalar w1982 = 107650
	scalar w1981 = 96620
	scalar w1980 = 86320
	scalar w1979 = 78620
	scalar w1978 = 76020
	scalar w1977 = 69920
	scalar w1976 = 63410
	scalar w1975 = 55810
	scalar w1974 = 47530
	scalar w1973 = 41890
	scalar w1972 = 37920
	scalar w1971 = 34610
	scalar w1970 = 30760
	scalar w1969 = 28580
	scalar w1968 = 26820
	scalar w1967 = 25000
	*Wage-adjust to 2022 level (adjust for overall nominal wage growth over time)
	gen pincome_w = .
	forvalues t=1967(1)2022 {
		replace pincome_w = pincome*scalar(w2022)/scalar(w`t') if aargang == `t'
	}
	save pincome_inf_adj.dta, replace
	* Merge to sample
	use w19_0634_lnr faar daar using all_sample_educ.dta, clear
	merge 1:m w19_0634_lnr using pincome_inf_adj.dta, keep(1 3) nogen
	gen age_at_income = aargang-faar 
	save pincome_pre_res.dta, replace
	
********************************************************************************
* Earnings age 34-40
********************************************************************************
	* Define income: use Markussen & Røed (2020) "best year rule"
	* Best year: keep top 3 income years ages 34-40
		keep if age_at_income>=34 & age_at_income<=40
		gsort w19_0634_lnr -pincome_w
		bys w19_0634_lnr: gen ranking = _n
		keep if ranking <= 3
		collapse (mean) pincome_w, by(w19_0634_lnr)
		save w34to40.dta, replace	
	* Merge income to sample
		use all_sample_educ.dta, clear
		merge 1:1 w19_0634_lnr using w34to40, keep(3) nogen
	* Bottom-top code and log
		egen lower_cutoff = pctile(pincome_w), p(2)
		egen upper_cutoff = pctile(pincome_w), p(98)
		sum lower_cutoff upper_cutoff
		clonevar c2c98_pincome_w=pincome_w
		replace c2c98_pincome_w = . if pincome_w < lower_cutoff
		replace c2c98_pincome_w = . if pincome_w > upper_cutoff
		drop lower_cutoff upper_cutoff
		keep if c2c98_pincome_w!=.
		gen log_c2c98_pincome_w=log(c2c98_pincome_w)
		rename (c2c98_pincome_w log_c2c98_pincome_w) ///
		(earnings log_earnings)
		save all_sample_educ_income.dta, replace
********************************************************************************	
* Parents income 
********************************************************************************	
	* Parents earnings when individual aged 8-14	
	* Father and mother files 
	use (w19_0634_lnr lopenr_mor lopenr_far faar faar_f faar_m) ///
	using all_sample_educ_income.dta, clear 
	frame copy default father
	frame copy default mother
	frame change father
		keep lopenr_far faar_f w19_0634_lnr faar
		rename (w19_0634_lnr faar lopenr_far) (barn_lnr faar_b w19_0634_lnr)
		save father_inc.dta, replace
	frame change mother
		keep lopenr_mor faar_m w19_0634_lnr faar 
		rename (w19_0634_lnr faar lopenr_mor) (barn_lnr faar_b w19_0634_lnr)
		save mother_inc.dta, replace
	* Income file reshape wide to merge to parents
	frame change default
		use pincome_inf_adj.dta, clear
		keep w19_0634_lnr pincome_w aargang
		reshape wide pincome_w, i(w19_0634_lnr) j(aargang)
		save pincome_w_1967_2022_wide.dta, replace	
	frame copy default father_pincome_w
	frame copy default mother_pincome_w
********************************
* Father
********************************
	frame change father_pincome_w
		merge 1:m w19_0634_lnr using father_inc.dta, keep(3) nogen	
		gen yr_child_8=faar_b+8
		gen yr_child_14=faar_b+14
		reshape long pincome_w, i(barn_lnr) 
		rename (_j pincome_w) (aargang finc_w)
		keep if aargang >= yr_child_8 & aargang <= yr_child_14
		* Keep 3 top income years for parent when child 8-14
		gsort barn_lnr -finc_w
		bys barn_lnr: gen ranking = _n
		keep if ranking <= 3
		collapse (mean) finc_w, by(barn_lnr)
		rename barn_lnr w19_0634_lnr
		save finc_w.dta, replace
********************************
* Mother
********************************
	frame change mother_pincome_w
		merge 1:m w19_0634_lnr using mother_inc.dta, keep(3) nogen	
		gen yr_child_8=faar_b+8
		gen yr_child_14=faar_b+14
		reshape long pincome_w, i(barn_lnr) 
		rename (_j pincome_w) (aargang minc_w)
		keep if aargang >= yr_child_8 & aargang <= yr_child_14
		* Keep 3 top income years for parent when child 8-14
		gsort barn_lnr -minc_w
		bys barn_lnr: gen ranking = _n
		keep if ranking <= 3
		collapse (mean) minc_w, by(barn_lnr)
		rename barn_lnr w19_0634_lnr
		save minc_w.dta, replace
		* Merge parents income 
		frame reset
		use all_sample_educ_income.dta, clear
		merge 1:1 w19_0634_lnr using finc_w, keep(1 3) nogen
		merge 1:1 w19_0634_lnr using minc_w, keep(1 3) nogen
		save all_sample_educ_income.dta, replace

********************************************************************************
* FINAL SAMPLES 
********************************************************************************		
*********************************************
**# Sample 1: Full population
*********************************************
	* Final preparation: 
		use all_sample_educ_income.dta, clear
	* Years of schooling
		sum yos, meanonly
		gen yos_c = yos - r(mean)		
	* Family income
		gen faminc_sum = finc_w+minc_w
		egen faminc = xtile(faminc_sum), n(20)
		* use father/mother ventile if not data on both
		egen finc_vent = xtile(finc_w), n(20)
		egen minc_vent = xtile(minc_w), n(20)
		replace faminc = finc_vent if faminc==.
		replace faminc = minc_vent if faminc==.
	* Family education
		gen fameduc = (feduc_age_11+meduc_age_11)/2 
		* use father/mother education if not data on both
		replace fameduc=feduc_age_11 if fameduc==.
		replace fameduc=meduc_age_11 if fameduc==.
		/* Group: LSS or below: <=9; ///
		Some HS: >9-<11; Complete or some HS: >9-12; 
		Extra HS: 13-<16; BA 16-<18; MA or above: >18 */
		gen fameduc_c = .
		replace fameduc_c=1 if fameduc>=0 & fameduc<=9
		replace fameduc_c=2 if fameduc>9 & fameduc<12
		replace fameduc_c=3 if fameduc>=12 & fameduc<16
		replace fameduc_c=4 if fameduc>=16 & fameduc!=.
			label define famed_lbl /// 
			1 "LSS or below" /// 
			2 "Some HS" ///
			3 "Complete HS or Extra HS" /// 
			4 "Tertiary", modify 
			label values fameduc_c famed_lbl
			fre fameduc_c
	* Parent's mean age at birth
		egen famage_birth_c = cut(famage_birth), ///
		at(14 20 25 30 35 40 65) 
		label define famage_lbl ///
			14 "14-19" 20 "20-24" 25 "25-29" 30 "30-34" ///
			35 "35-39" 40 "40+", modify
		label values famage_birth_c famage_lbl
		fre famage_birth_c
	* Family id 
		egen famid = group(lopenr_mor lopenr_far)
	* Biological siblings 
		sort famid faar
		by famid: gen birthorder=_n
		fre birthorder
		*<1% have more than 5 sibs
		replace birthorder=5 if birthorder>5 & birthorder!=.
	* Total number of siblings
		by famid: gen kids=_N if famid!=.
		fre kids
		*<1% have more than 5 kids
		replace kids=5 if kids>5 & kids!=.
		la var birthorder "Birth order"
		la var kids "Total number of offspring"
			
		save full_population.dta, replace
		
*********************************************
**# Sample 2: Siblings
*********************************************
	* Keep >=2 siblings 
	keep if kids>=2 & kids!=.
	save siblings.dta, replace

*********************************************
**# Sample 3: Twins 
*********************************************
	use full_population.dta, clear
	gen SSBLNr = w19_0634_lnr
	merge 1:1 SSBLNr using "N:\durable\data\registers\twin registry\PDB2601_NTR_2023.dta", ///
	keep(3) nogen
	egen twinpair = group(lopenr_mor lopenr_mor) if lopenr_mor!="" & lopenr_far!=""
	bys twinpair: gen n_twins=_N if lopenr_mor!="" & lopenr_far!=""
	keep if n_twins>=2 & n_twins!=.
	keep if Zygo!=.
	save twins.dta, replace
		
*********************************************
**# Sample 4: MoBa genetic data 
*********************************************
	/* Note: EA_MR.dta constructed from R script: 
	"N:\durable\projects\EQOP\MR_Income\Code for MR returns\EA MR\PGI_for_MR" 
	which reformats *.profile to *.dta; former based on procedure PAD */
	use EA_MR.dta, clear
	rename IID sentrix_id
	save ea_mr_parents.dta, replace	
	use "N:\durable\data\moba\linkage\merged_IDs\MoBa_SSB_IDs.dta", clear	
	drop if w19_0634_lnr=="NA"
	drop if sentrix_id=="NA"
	duplicates drop w19_0634_lnr, force
	merge 1:1 sentrix_id using ea_mr_parents.dta, keep(3) nogen
	merge 1:1 w19_0634_lnr using full_population.dta, keep(3) nogen
	egen piv=std(SCORE)
	save moba_genetics.dta, replace
	
*********************************************
**# Sample 5: MoBa genotyped siblings
*********************************************
	* Redefine total number of kids for sibling sample 
	drop kids
	bys famid: gen kids=_N if famid!=.
	replace kids=3 if kids>3 & kids!=.	
	* Keep >=2 siblings 
	keep if kids>=2 & kids!=.
	save moba_sib_genetics.dta, replace
	
*********************************************
**# Sample 6: Norway-only sample
*********************************************	
	use wf_EA_MR_335.dta, clear // From wf-analysis
	rename IID sentrix_id
	save wf_ea_mr_parents.dta, replace	
	use "N:\durable\data\moba\linkage\merged_IDs\MoBa_SSB_IDs_20250317", clear
	drop if w19_0634_lnr=="NA"
	drop if sentrix_id=="NA"
	duplicates drop w19_0634_lnr, force
	merge 1:1 sentrix_id using wf_ea_mr_parents.dta, keep(3) nogen
	merge 1:1 w19_0634_lnr using full_population.dta, keep(3) nogen
		gen SCORE1=SCORE*(-1)
		egen wf_piv=std(SCORE1)
	save wf_moba_genetics.dta, replace
	* Unrelated: 
	use w19_0634_lnr using moba_sib_genetics.dta, clear
	save id_moba_sib_genetics.dta, replace
	use wf_moba_genetics, clear
	merge 1:1 w19_0634_lnr using id_moba_sib_genetics_v2.dta, keep(1) nogen
	save wf_moba_genetics.dta, replace
	

********************************************************************************
**# B. MAIN ANALYSIS
********************************************************************************
frame reset
frame create fullpop
frame create sibling 
frame create twins
frame create mobagen 
frame create mobagensib
frame create mobanorwayonly
*********************************************
**# 1. OLS
*********************************************
frame change fullpop
use full_population.dta, clear
	* Figure 2 and Table S2
	* Define covariate lists
	estimates clear
	local covs c.fameduc c.faminc##c.faminc
	local covab male faar famage_birth_c kids bmonth birthorder
	local covabsib famid male faar famage_birth_c bmonth birthorder
	local covabsib_nofe male faar famage_birth_c bmonth birthorder
	* Full pop
	** Log 
	* adj.
	reghdfe log_earnings yos_c `covs', absorb(`covab') cl(faar)
	gen estsample_adj = e(sample)
	est sto fpop_adj
	* unadj.
	reghdfe log_earnings yos_c i.male i.faar if estsample_adj==1, cl(faar)
	est sto fpop_unadj
	** NOK
	* adj.
	reghdfe earnings yos_c `covs', absorb(`covab') cl(faar)
	est sto fpop_nok_adj
	* unadj.
	reghdfe earnings yos_c i.male i.faar if estsample_adj==1, cl(faar)
	est sto fpop_nok_unadj

*********************************************
**# 2. Sibling model
*********************************************
frame change sibling 
use siblings.dta, clear	
	** Log 
	* adj.
	reghdfe log_earnings yos_c `covs', absorb(`covabsib') cl(famid faar)
	gen estsample_adj = e(sample)
	est sto sib_adj	
	* unadj
	reghdfe log_earnings yos_c i.male i.faar if estsample_adj==1, ///
	absorb(famid) cl(famid)
	est sto sib_unadj
	** NOK
	reghdfe earnings yos_c `covs', absorb(`covabsib') cl(famid faar)
	est sto sib_nok_adj	
	* unadj
	reghdfe earnings yos_c i.male i.faar if estsample_adj==1, ///
	absorb(famid) cl(famid faar)
	est sto sib_nok_unadj
	
	* Sibling without FE (Table S3)
	reghdfe log_earnings yos_c `covs', absorb(`covabsib_nofe') cl(faar)
	gen estsample_sib_nofe_adj = e(sample)
	est sto sib_nofe_adj	
	* unadj (OLS no FE)
	reghdfe log_earnings yos_c i.male i.faar ///
	if estsample_sib_nofe_adj==1, cl(faar)
	est sto sib_nofe_unadj			

*********************************************
**# 3. Twins 
*********************************************	
frame change twins
use twins.dta, clear
	** DZ
	** Log 
	reghdfe log_earnings yos_c i.male if Zygo>=2, ///
	absorb(twinpair) cl(twinpair faar)
	est sto dz	
	** NOK
	reghdfe earnings yos_c i.male if Zygo>=2, ///
	absorb(twinpair) cl(twinpair faar)
	est sto dz_nok
	
	** MZ
	** Log 
	reghdfe log_earnings yos_c if Zygo==1, ///
	absorb(twinpair) cl(twinpair faar)
	est sto mz
	** NOK
	reghdfe earnings yos_c if Zygo==1, ///
	absorb(twinpair) cl(twinpair faar)
	est sto mz_nok
	
	* OLS no FE: 
	reghdfe log_earnings yos_c i.male i.faar, cl(faar)
	est sto twin_nofe
	
	reg log_earnings yos_c i.male i.faar if Zygo>=2, r
	est sto dz_nofe
	reg log_earnings yos_c i.faar if Zygo==1, r	
	est sto mz_nofe

*********************************************	
**# 4. MR
*********************************************	
frame change mobagen
use moba_genetics.dta, clear 
		
	* MR
	** Log 
	* adj.
	ivreghdfe log_earnings (yos_c=piv) `covs', absorb(`covab') cl(faar)
	gen estsample_adj = e(sample)
	est sto mr_adj
	* unadj.
	ivreghdfe log_earnings (yos_c=piv) if estsample_adj==1, /// 
	absorb(male faar) cl(faar)
	est sto mr_unadj
	** NOK
	* adj.
	ivreghdfe earnings (yos_c=piv) `covs', absorb(`covab') cl(faar)
	est sto mr_nok_adj
	* unadj.
	ivreghdfe earnings (yos_c=piv) if estsample_adj==1, /// 
	absorb(male faar) cl(faar)
	est sto mr_nok_unadj
	
	* OLS comparison (Table S3)
	reghdfe log_earnings yos_c if estsample_adj==1, /// 
	absorb(male faar) cl(faar) 
	est sto mr_ols
	
	* Reduced form 
	reghdfe earnings c.piv `covs', absorb(`covab') cl(faar)
	est sto reduced_form
	
	
*********************************************	
**# 5. Sibling MR
*********************************************	
frame change mobagensib
use moba_sib_genetics.dta, clear

	* Sibling-MR
	** Log 
	* adj.
	ivreghdfe log_earnings (yos_c=piv) `covs', ///
	absorb(`covabsib') cl(famid faar)
	gen estsample_adj = e(sample)
	est sto sibmr_adj
	* unadj.
	ivreghdfe log_earnings (yos_c=piv) if estsample_adj==1, /// 
	absorb(famid male faar) cl(famid faar)
	est sto sibmr_unadj
	** NOK
	* adj.
	ivreghdfe earnings (yos_c=piv) `covs', ///
	absorb(`covabsib') cl(famid faar)
	est sto sibmr_nok_adj
	* unadj.
	ivreghdfe earnings (yos_c=piv) if estsample_adj==1, /// 
	absorb(famid male faar) cl(famid faar)
	est sto sibmr_nok_unadj
	
	* Sibling without FE (Table S3)
	ivreghdfe log_earnings (yos_c=piv) `covs', ///
	absorb(`covabsib_nofe') cl(faar)
	est sto sibmr_nofe		
	* unadj (OLS no FE)
	reghdfe log_earnings yos_c if estsample_adj==1, ///
	absorb(male faar) cl(faar)
	est sto moba_sib_unadj	
	
*********************************************	
**# 6. Norway-only MR
*********************************************	

	frame change mobanorwayonly
	use wf_moba_genetics.dta, clear
	
	* Norway-only MR
	** Log 
	* adj.
	ivreghdfe log_earnings (yos_c=wf_piv) `covs', absorb(`covab') cl(faar)
	gen estsample_adj = e(sample)
	est sto wfmr_adj
	* unadj.
	ivreghdfe log_earnings (yos_c=wf_piv) if estsample_adj==1, /// 
	absorb(male faar) cl(faar)
	est sto wfmr_unadj
	** NOK
	* adj.
	ivreghdfe earnings (yos_c=wf_piv) `covs', absorb(`covab') cl(faar)
	est sto wfmr_nok_adj
	* unadj.
	ivreghdfe earnings (yos_c=wf_piv) if estsample_adj==1, /// 
	absorb(male faar) cl(faar)
	est sto wfmr_nok_unadj
	
	* OLS comparison (Table S3)
	reghdfe log_earnings yos_c if estsample_adj==1, /// 
	absorb(male faar) cl(faar) 
	est sto wfmr_ols
	
	
******************************************************
* AR test statistic and 95% CI for MR and Sibling-MR
******************************************************
	
	frame change mobagen
	local c c.fameduc c.faminc#c.faminc i.male i.faar ///
	i.famage_birth_c i.kids i.bmonth i.birthorder
	* Log
	weakiv ivregress 2sls log_earnings (yos_c=piv) `c', vce(cluster faar)
	weakiv ivregress 2sls log_earnings (yos_c=piv) i.male i.faar, vce(cluster faar)
	* NOK
	weakiv ivregress 2sls earnings (yos_c=piv) `c', vce(cluster faar)
	weakiv ivregress 2sls earnings (yos_c=piv) i.male i.faar, vce(cluster faar)
	
	frame change mobanorwayonly
	* Log
	weakiv ivregress 2sls log_earnings (yos_c=wf_piv) `c', vce(cluster faar)
	weakiv ivregress 2sls log_earnings (yos_c=wf_piv) i.male i.faar, vce(cluster faar)
	* NOK
	weakiv ivregress 2sls earnings (yos_c=wf_piv) `c', vce(cluster faar)
	weakiv ivregress 2sls earnings (yos_c=wf_piv) i.male i.faar, vce(cluster faar)
	
	frame change mobagensib
	* as weakiv incompatible ivreghdfe; using xtivreg2, making indicator sets
	tab faar, gen(cohb)
	tab famage_birth_c, gen(fam_age_birth)
	tab birthorder, gen(bo)
	tab bmonth, gen(bm)
	gen faminc2=faminc*faminc
	xtset famid
	local c male coh1-coh23 fameduc faminc faminc2 ///
	fam_age_birth1-fam_age_birth5 bo1-bo4 bm1-bm11
	* Log
	weakiv xtivreg2 log_earnings (yos_c=piv) `c', fe cl(famid faar)
	weakiv xtivreg2 log_earnings (yos_c=piv) coh1-coh23 male, fe cl(famid faar)
	* NOK
	weakiv xtivreg2 earnings (yos_c=piv) `c', fe cl(famid faar)
	weakiv xtivreg2 log_earnings (yos_c=piv) coh1-coh23 male, fe cl(famid faar)
	
		
********************************************************************************
**# C. FIGURES AND TABLES
********************************************************************************	
********************************************************************************
* Table S2: Results from OLS, sibling model, twin model, MR and sibling MR
* Note: Estimates in Table S2 presented in Figure 2 
******************************************************************************** 
* Table S2:
* Log
	esttab fpop_unadj fpop_adj sib_unadj sib_adj ///
	using "log_results.rtf", ///
	keep(yos_c) b(3) se(3) ///
	mtitles("Full Pop" "Full Pop Adj" "Siblings" "Siblings Adj") ///
	replace
	esttab dz mz mr_unadj mr_adj ///
	using "log_results.rtf", ///
	keep(yos_c) b(3) se(3) ///
	mtitles("DZ" "MZ" "MR" "MR Adj") ///
	append
	esttab sibmr_unadj sibmr_adj ///
	using "log_results.rtf", ///
	keep(yos_c) b(3) se(3) ///
	mtitles("Sib MR" "Sib MR Adj") ///
	append
	esttab wfmr_unadj wfmr_adj ///
	using "log_results.rtf", ///
	keep(yos_c) b(3) se(3) ///
	mtitles("Norway-only MR" "Norway-only MR Adj") ///
	append
	
* NOK
	esttab fpop_nok_unadj fpop_nok_adj sib_nok_unadj sib_nok_adj ///
	using "nok_results.rtf", ///
	keep(yos_c) b(0) se(0) ///
	mtitles("Full Pop" "Full Pop Adj" "Siblings" "Siblings Adj") ///
	replace
	esttab dz_nok mz_nok mr_nok_unadj mr_nok_adj ///
	using "nok_results.rtf", ///
	keep(yos_c) b(0) se(0) ///
	mtitles("DZ" "MZ" "MR" "MR Adj") ///
	append
	esttab sibmr_nok_unadj sibmr_nok_adj ///
	using "nok_results.rtf", ///
	keep(yos_c) b(0) se(0) ///
	mtitles("Sib MR" "Sib MR Adj") ///
	append
	esttab wfmr_nok_unadj wfmr_nok_adj ///
	using "log_results.rtf", ///
	keep(yos_c) b(3) se(3) ///
	mtitles("Norway-only MR" "Norway-only MR Adj") ///
	append
	
* Table S3 
	esttab fpop_unadj sib_nofe_unadj twin_nofe ///
	using "ols_results.rtf", ///
	keep(yos_c) b(3) se(3) ///
	mtitles("Full Pop" "Siblings" "Twins") ///
	replace
	esttab mr_ols moba_sib_unadj wfmr_ols ///
	using "ols_results.rtf", ///
	keep(yos_c) b(3) se(3) ///
	mtitles("MR" "Sib MR" "Norway-only MR") ///
	append


**************************************************
* Table S1
**************************************************	
	use full_population.dta
	tabstat earnings, by(male) stat(p50 iqr) 
	tabstat yos, by(male) stat(mean sd) 
	gen sample_number=1
	append using siblings.dta
	replace sample_number=2 if sample_number==.
	append using twins.dta
	replace sample_number=3 if sample_number==.
	append using moba_genetics.dta
	replace sample_number=4 if sample_number==.
	append using moba_sib_genetics.dta
	replace sample_number=5 if sample_number==.
	append using wf_moba_genetics.dta
	replace sample_number=6 if sample_number==.
	save table_s1.dta
	* Table S1
	tabstat earnings, by(sample_number) stat(mean sd) 
	tabstat earnings, by(sample_number) stat(p50 iqr) 
	estpost tabstat yos faar bmonth male faminc fameduc famage_birth ///
	kids birthorder, by(sample_number) statistics(mean sd) ///
    columns(statistics) 	
	
********************************************************************************
* Figure 1
* 1B: Age-earnings and life-cycle figure 
* 1A and 1B made in R; below data prepped for 1B
********************************************************************************	
	use w19_0634_lnr pincome_w aargang using pincome_inf_adj.dta, clear
	merge m:1 w19_0634_lnr using full_population.dta, ///
	keepusing(faar college) keep(3) nogen
	g age_at_income = aargang-faar
	keep if inrange(age_at_income,17,62)
	gen c2c98_pincome_w = pincome_w
	* Loop age 17-62
	forvalues a = 17/62 {
	* Calculate 2nd and 98th percentiles for current age
	_pctile pincome_w if age_at_income == `a', percentiles(2 98)
		local p2 = r(r1)
		local p98 = r(r2)	
	replace c2c98_pincome_w = . if age_at_income == `a' & ///
	(pincome_w < `p2' | pincome_w > `p98')
	}
	save lifetime_income_college.dta, replace
	collapse (mean) c2c98_pincome_w, by(college age_at_income)
	g income_1k = c2c98_pincome_w/1000
	rename age_at_income age
	keep age college income_1k
	save income_by_age_and_college.dta, replace
	
********************************************************************************
**# D. TWO-SAMPLE MR: SUMMARY-DATA 
********************************************************************************
	* Prep file import R 
	use w19_0634_lnr log_earnings yos male faar using full_population.dta, clear
	rename (log_earnings faar) (log_income birthyear)
	keep if log_income!=.
	merge 1:1 w19_0634_lnr using moba_genetics.dta	
	save income_for_twosample.dta, replace	

********************************************************************************
**# E. EFFECT HETEROGENEITY
********************************************************************************
frame change mobagen
use moba_genetics.dta, clear 
* Sex 	
	estimates clear
	local covs c.fameduc c.faminc##c.faminc
	local covab faar famage_birth_c kids bmonth birthorder
	* OLS
	** Log 
	forvalues q = 0/1 {
	reghdfe log_earnings yos_c `covs' if male==`q', absorb(`covab') cl(faar)
	est sto ols_`q'
	** NOK
	ivreghdfe earnings yos_c `covs' if male==`q', absorb(`covab') cl(faar)
	est sto ols_nok_`q'
	* MR
	** Log 
	ivreghdfe log_earnings (yos_c=piv) `covs' if male==`q', ///
	absorb(`covab') cl(faar)
	est sto mr_`q'
	** NOK
	ivreghdfe earnings (yos_c=piv) `covs' if male==`q', ///
	absorb(`covab') cl(faar)
	est sto mr_nok_`q'
	}
* Family background
	* Family quartile
	egen faminc_q = xtile(faminc_sum), n(4)
	egen finc_q = xtile(finc_w), n(4)
	egen minc_q = xtile(minc_w), n(4)
	replace faminc_q=finc_q if faminc_q==.
	replace faminc_q=minc_q if faminc_q==.
	local covs c.fameduc 
	local covab male faar famage_birth_c kids bmonth birthorder
	* OLS
	** Log 
	forvalues q = 1/4 {
    reghdfe log_earnings yos_c `covs' if faminc_q==`q', absorb(`covab') cl(faar)
    est sto ols_faminc_`q'
	** NOK
    reghdfe earnings yos_c `covs' if faminc_q==`q', absorb(`covab') cl(faar)
    est sto ols_nok_faminc_`q'
	* MR
	** Log 
    ivreghdfe log_earnings (yos_c=piv) `covs' if faminc_q==`q', ///
	absorb(`covab') cl(faar)
    est sto mr_faminc_`q'
	** NOK
    ivreghdfe earnings (yos_c=piv) `covs' if faminc_q==`q', ///
	absorb(`covab') cl(faar)
    est sto mr_nok_faminc_`q'
	}
* AR test and 95% CI
	* Sex
	local c c.fameduc c.faminc#c.faminc i.faar ///
	i.famage_birth_c i.kids i.bmonth i.birthorder
	* Log
	weakiv ivregress 2sls log_earnings (yos_c=piv) `c' if male==1, vce(cluster faar)
	weakiv ivregress 2sls log_earnings (yos_c=piv) i.faar if male==1, vce(cluster faar)
	* NOK
	weakiv ivregress 2sls log_earnings (yos_c=piv) `c' if male==1, vce(cluster faar)
	weakiv ivregress 2sls earnings (yos_c=piv) i.faar if male==1, vce(cluster faar)
	* Family background
	local c c.male c.fameduc i.faar ///
	i.famage_birth_c i.kids i.bmonth i.birthorder
	forvalues q = 1/4 {
	** Log
	weakiv ivregress 2sls log_earnings (yos_c=piv) `c' if faminc_q==`q', ///
	vce(cluster faar)
	** NOK
	weakiv ivregress 2sls earnings (yos_c=piv) `c' if faminc_q==`q', ///
	vce(cluster faar)
	}	
	
* Table S7:
* Sex
	esttab ols_0 ols_1 mr_0 mr_1 ///
	using "results_sex.rtf", ///
	keep(yos_c) b(3) se(3) ///
	mtitles("OLS F" "OLS M" "MR F" "MR M") ///
	replace
	esttab ols_nok_0 ols_nok_1 mr_nok_0 mr_nok_1 ///
	using "results_sex.rtf", ///
	keep(yos_c) b(0) se(0) ///
	mtitles("OLS NOK F" "OLS NOK M" "MR NOK F" "MR NOK M") ///
	append
* Family income
	esttab ols_faminc_1 ols_faminc_2 ols_faminc_3 ols_faminc_4 ///
	using "results_faminc.rtf", ///
	keep(yos_c) b(3) se(3) ///
	mtitles("OLS 1" "OLS 2" "OLS 3" "OLS 4") ///
	replace
	esttab mr_faminc_1 mr_faminc_2 mr_faminc_3 mr_faminc_4 ///
	using "results_faminc.rtf", ///
	keep(yos_c) b(3) se(3) ///
	mtitles("MR 1" "MR 2" "MR 3" "MR 4") ///
	append
	esttab ols_nok_faminc_1 ols_nok_faminc_2 ols_nok_faminc_3 ols_nok_faminc_4 ///
	using "results_faminc.rtf", ///
	keep(yos_c) b(0) se(0) ///
	mtitles("OLS NOK 1" "OLS NOK 2" "OLS NOK 3" "OLS NOK 4") ///
	append
	esttab mr_nok_faminc_1 mr_nok_faminc_2 mr_nok_faminc_3 mr_nok_faminc_4 ///
	using "results_faminc.rtf", ///
	keep(yos_c) b(0) se(0) ///
	mtitles("MR NOK 1" "MR NOK 2" "MR NOK 3" "MR NOK 4") ///
	append
	tabstat earnings, by(faminc_q) stat(p50 iqr) 
	tabstat yos, by(faminc_q) stat(mean sd) 

********************************************************************************
**# F. PLAUSEXOG
********************************************************************************	
	frame change mobagen
	local covs c.fameduc c.faminc##c.faminc ///
	i.faar i.male i.famage_birth_c i.kids i.bmonth i.birthorder
	plausexog uci earnings `covs' (yos=piv), gmin(0) gmax(20000) level(.95) ///
    vce(robust) graph(yos)
	* Creates Figure S6
	* Examine bounds at gmax 12,650 => positive
	*plausexog uci earnings `covs' (yos=piv), gmin(0) gmax(12650) level(.95) ///
    vce(robust) 
	* Compare to reduced form 
	di 12650/14133
	
********************************************************************************
**# G. IV-OLS DECOMPOSITION
********************************************************************************
frame copy mobagen ivolsweight
frame change ivolsweight
	* cgroup must be binary
	tab faminc_q, gen(faminc_lvl)
	gen fameduc_category = . 
	replace fameduc_category = 1 if fameduc < 12
	replace fameduc_category = 2 if fameduc >=12 & fameduc<16
	replace fameduc_category = 3 if fameduc >=16 & fameduc!=.
	tab fameduc_category, gen(fameduc_lvl)	
	tab famage_birth_c, gen(fambage)
	gen qbirth = .
		replace qbirth=1 if bmonth >=1 & bmonth<=3
		replace qbirth=2 if bmonth >=4 & bmonth<=6
		replace qbirth=3 if bmonth >=7 & bmonth<=9
		replace qbirth=4 if bmonth >=10 & bmonth<=12
	tab qbirth, g(bqtr)
	gen byear=.
	replace byear=1 if faar>=1959 & faar<=1964
	replace byear=2 if faar>=1965 & faar<=1970
	replace byear=3 if faar>=1971 & faar<=1976
	replace byear=4 if faar>=1977 & faar<=1982
	tab byear, g(cohort)
	tab male, gen(sex)
	tab birthorder, gen(border)
	tab kids, gen(chld)
	* IV-OLS decomposition: weights on treatment levels and covariates
	ivolsdec log_earnings ///
	faminc_lvl* fameduc_lvl* sex* cohort* bqtr* border* chld* fambage* ///
	(yos=piv), xnbasis(i.yos) ///
	cgroup(faminc_lvl* fameduc_lvl* sex* cohort* bqtr* border* chld* fambage*) ///
	tlevel(11 12 13 16 18 23) format(%7.3f)
	tabstat kids, by(faminc_q) stat(mean sd) 
	* Figure S7 
	ivolsdec log_earnings ///
	faminc_lvl* fameduc_lvl* sex* cohort* bqtr* border* chld* fambage* ///
	(yos=piv), xnbasis(i.yos) ///
	cgroup(faminc_lvl* fameduc_lvl* sex* cohort* bqtr* border* chld* fambage*) ///
	tlevel(10/23) format(%7.4f)
	frame create ivols_fig
	frame change ivols_fig
	clear
		set obs 14
		gen yos = _n + 9
		gen ols_wgt=. 
		gen ols_se=.
		gen iv_wgt=.
		gen iv_se=. 
		* ols weight
		replace ols_wgt = .0568 in 1
		replace ols_wgt = .0568 in 2
		replace ols_wgt = .0710 in 3
		replace ols_wgt = .1561 in 4
		replace ols_wgt = .1610 in 5
		replace ols_wgt = .1610 in 6
		replace ols_wgt = .1610 in 7
		replace ols_wgt = .0689 in 8
		replace ols_wgt = .0689 in 9
		replace ols_wgt = .0077 in 10
		replace ols_wgt = .0077 in 11
		replace ols_wgt = .0077 in 12
		replace ols_wgt = .0077 in 13
		replace ols_wgt = .0077 in 14
		replace ols_se = .0004 in 1
		replace ols_se = .0004 in 2
		replace ols_se = .0004 in 3
		replace ols_se = .0004 in 4
		replace ols_se = .0004 in 5
		replace ols_se = .0004 in 6
		replace ols_se = .0004 in 7
		replace ols_se = .0004 in 8
		replace ols_se = .0004 in 9
		replace ols_se = .0003 in 10
		replace ols_se = .0003 in 11
		replace ols_se = .0003 in 12
		replace ols_se = .0003 in 13
		replace ols_se = .0003 in 14
		* iv weight
		replace iv_wgt = .0535 in 1
		replace iv_wgt = .0535 in 2
		replace iv_wgt = .0722 in 3
		replace iv_wgt = .1530 in 4
		replace iv_wgt = .1555 in 5
		replace iv_wgt = .1555 in 6
		replace iv_wgt = .1555 in 7
		replace iv_wgt = .0870 in 8
		replace iv_wgt = .0870 in 9
		replace iv_wgt = .0055 in 10
		replace iv_wgt = .0055 in 11
		replace iv_wgt = .0055 in 12
		replace iv_wgt = .0055 in 13
		replace iv_wgt = .0055 in 14
		replace iv_se = .0023 in 1
		replace iv_se = .0023 in 2
		replace iv_se = .0026 in 3
		replace iv_se = .0023 in 4
		replace iv_se = .0021 in 5
		replace iv_se = .0021 in 6
		replace iv_se = .0021 in 7
		replace iv_se = .0030 in 8
		replace iv_se = .0030 in 9
		replace iv_se = .0008 in 10
		replace iv_se = .0008 in 11
		replace iv_se = .0008 in 12
		replace iv_se = .0008 in 13
		replace iv_se = .0008 in 14
		gen ols_lb=ols_wgt-(1.96*ols_se)
		gen ols_ub=ols_wgt+(1.96*ols_se)
		gen iv_lb=iv_wgt-(1.96*iv_se)
		gen iv_ub=iv_wgt+(1.96*iv_se)
	* Figure
	twoway ///
	(rcap ols_lb ols_ub yos, color(gs4)) ///
    (rcap iv_lb iv_ub yos, color(gs8)) ///
    (connected ols_wgt yos, color(gs4) msymbol(O) ///
	msize(small) lpattern(dash)) ///
    (connected iv_wgt yos, color(gs8) msymbol(T) ///
	msize(small) lpattern(dash)) ///
    , legend(order(3 "OLS weights" 4 "IV weights") ring(0) pos(2)) ///
    xlabel(10(1)23) ylabel(0(.1).4) ///
    xtitle("Years of schooling") ///
	ytitle("Schooling-level specific weights") ///
    graphregion(color(white)) bgcolor(white)
	frame change ivolsweight
	frame drop ivols_fig
	* Subsample OLS coefficient
		local covs i.male i.faar 
		* Family income
		forvalues q = 1/4 {
		reg log_earnings yos_c `covs' if faminc_q==`q', r
		}
		* Family education
		forvalues q = 1/3 {
		reg log_earnings yos_c `covs' if fameduc_category==`q', r
		}
		* Male
		reg log_earnings yos_c i.faar if male==1, r
		reg log_earnings yos_c i.faar if male==0, r
		* Birth year 
		forvalues q = 1/4 {
		reg log_earnings yos_c i.male if byear==`q', r
		}
		* Birth month
		forvalues q = 1/4 {
		reg log_earnings yos_c `covs' if qbirth==`q', r
		}
		* Birth order 
		forvalues q = 1/5 {
		reg log_earnings yos_c `covs' if birthorder==`q', r
		}
		* Children
		forvalues q = 1/5 {
		reg log_earnings yos_c `covs' if kids==`q', r
		}
		* Parents age at birth
		foreach q in 14 20 25 30 35 40 {
		reg log_earnings yos_c `covs' if famage_birth_c==`q', r
		}
		
********************************************************************************
**# H. COVARIANCE BALANCE PLOT
********************************************************************************
	frame change mobagen
	label var male "Male"
	label var faar "Birth year"
	label var fameduc "Parents' education"
	label var faminc "Parents' earnings" 
	label var famage_birth_c "Parents' age at birth"
	label var kids "Number of children in family" 
	label var birthorder "Birth order"
	label var bmonth "Birth month"
	reg piv c.faar i.male  ///
	c.faminc c.fameduc ///
	c.bmonth c.birthorder c.famage_birth_c ///
	c.kids, cl(faar)
	coefplot, drop(_cons) xline(0) /// 
	xscale(range(-.15(.05).15)) xlabel(-.15(.05).15) ///
	scheme(s1mono) ///
	title("{bf:A.} MoBa genotyped sample", size(medsmall) position(11)) ///
	xtitle("PIV{superscript:EA}") grid(none)
	graph save "balance_plot.gph", replace
	
	frame change mobagensib
	label var male "Male"
	label var faar "Birth year"
	label var fameduc "Parents' education"
	label var faminc "Parents' earnings" 
	label var famage_birth_c "Parents' age at birth"
	label var kids "Number of children in family" 
	label var birthorder "Birth order"
	label var bmonth "Birth month"
	reghdfe piv c.faar i.male ///
	c.faminc c.fameduc ///
	c.bmonth c.birthorder c.famage_birth_c, ///
	absorb(famid) cl(famid)
	coefplot, drop(_cons) xline(0) /// 
	xscale(range(-.15(.05).15)) xlabel(-.15(.05).15) ///
	scheme(s1mono) ///
	title("{bf:B.} MoBa genotyped sibling sample", size(medsmall) position(11)) ///
	xtitle("PIV{superscript:EA}") grid(none)
	graph save "balance_plot_sibling.gph", replace 
		
	frame change mobanorwayonly
	use wf_moba_genetics_clean_v2.dta, clear
	label var male "Male"
	label var faar "Birth year"
	label var fameduc "Parents' education"
	label var faminc "Parents' earnings" 
	label var famage_birth_c "Parents' age at birth"
	label var kids "Number of children in family" 
	label var birthorder "Birth order"
	label var bmonth "Birth month"
	reg wf_piv c.faar i.male  ///
	c.faminc c.fameduc ///
	c.bmonth c.birthorder c.famage_birth_c ///
	c.kids, cl(faar)
	coefplot, drop(_cons) xline(0) /// 
	xscale(range(-.15(.05).15)) xlabel(-.15(.05).15) ///
	scheme(s1mono) ///
	title("{bf:C.} MoBa genotyped sample (Norway-only)", size(medsmall) position(11)) ///
	xtitle("PIV{superscript:EA}") grid(none)
	graph save "balance_plot_wf_piv.gph", replace
	
	graph combine "balance_plot.gph" "balance_plot_sibling.gph" "balance_plot_wf_piv.gph", rows(1) ycommon
	graph save "FigureS3_balanceplot.gph", replace
	
********************************************************************************
**# I. EXPERIENCE
********************************************************************************
/*
Follow Eika & Kirkebøen (2023): I hovedspesifikasjonen antar vi at personer får ett år med erfaring for hvert år med yrkesinntekt over 200.000 2019-kroner. Erfaring beregnes på bakgrunn av årlig informasjon om pensjonsgivende inntekt tilbake til 1967. Hvis en person et år har inntekt svarende til 200.000 2019 kroner (deflatert med tall for lønn per normalårsverk fra Nasjonalregnskapet, SSB tabell 09786) gis vedkommende full erfaring for det aktuelle året. For inntekter mellom 100.000 og 200.000 tilordnes erfaring som en lineær funksjon av inntekten, fra 0,5 til 1.
200000 2019 kr tilsvarer 221 660,65 2022 kr => https://www.ssb.no/kalkulatorer/priskalkulator
100000 2019 kr = 110 830,32
*/
	frame change default
	use w19_0634_lnr faar using full_population.dta, clear
	merge 1:m w19_0634_lnr using pincome_inf_adj.dta, keep(3) nogen
	gen age_at_income = aargang-faar
	* Generate experience 
	keep if age_at_income>=17 & age_at_income<=33
	keep if pincome_w!=.
	gen exper=0
	replace exper = 1 if pincome_w > 221660
	replace exper = 0.5 + 0.5*(pincome_w - 110830)/(221660 - 110830) if ///
	pincome_w > 110830 & pincome_w <= 221660
	collapse (sum) exper, by(w19_0634_lnr)
	save experience.dta, replace
	* Merge to sample 
	* Full population 
	use full_population, clear 
	merge 1:1 w19_0634_lnr using experience.dta, keep(1 3) nogen
	save exper_full_population.dta, replace 
	* Siblings 
	use siblings, clear 
	merge 1:1 w19_0634_lnr using experience.dta, keep(1 3) nogen
	save exper_siblings.dta, replace 
	* Twins 
	use twins, clear 
	merge 1:1 w19_0634_lnr using experience.dta, keep(1 3) nogen
	save exper_twins.dta, replace 
	* MoBa 
	use moba_genetics, clear 
	merge 1:1 w19_0634_lnr using experience.dta, keep(1 3) nogen
	save exper_moba_genetics.dta, replace 
	* MoBa siblings 
	use moba_sib_genetics, clear 
	merge 1:1 w19_0634_lnr using experience.dta, keep(1 3) nogen
	save exper_moba_sib_genetics.dta, replace 
	
* Compare analysis with experience 
	* Define covariate lists
	estimates clear
	local covs c.fameduc c.faminc##c.faminc
	local expcovs c.fameduc c.faminc##c.faminc c.exper##c.exper
	local covab male faar famage_birth_c kids bmonth birthorder
	local covabsib famid male faar famage_birth_c bmonth birthorder
	* Full population 
	use exper_full_population, clear 
	reghdfe log_earnings yos_c `covs', absorb(`covab') cl(faar)
	est sto m_fullpop0
	reghdfe log_earnings yos_c `expcovs', absorb(`covab') cl(faar)
	est sto m_fullpop1
	* Siblings 
	use exper_siblings, clear 
	reghdfe log_earnings yos_c `covs', absorb(`covabsib') cl(famid faar)
	est sto m_sib0
	reghdfe log_earnings yos_c `expcovs', absorb(`covabsib') cl(famid faar)
	est sto m_sib1
	* Twins 
	use exper_twins, clear 
	** DZ
	reghdfe log_earnings yos_c i.male if Zygo>=2, ///
	absorb(twinpair) cl(twinpair faar)
	est sto m_dz0
	reghdfe log_earnings yos_c i.male c.exper##c.exper if Zygo>=2, ///
	absorb(twinpair) cl(twinpair faar)
	est sto m_dz1
	** MZ
	reghdfe log_earnings yos_c if Zygo==1, ///
	absorb(twinpair) cl(twinpair faar)
	est sto m_mz0
	reghdfe log_earnings yos_c i.male c.exper##c.exper if Zygo==1, ///
	absorb(twinpair) cl(twinpair faar)
	est sto m_mz1
	** MoBa 
	use exper_moba_genetics, clear 
	ivreghdfe log_earnings (yos_c=piv) `covs', absorb(`covab') cl(faar)
	est sto m_iv_moba_gen0
	ivreghdfe log_earnings (yos_c=piv) `expcovs', absorb(`covab') cl(faar)
	est sto m_iv_moba_gen1
	** MoBa siblings 
	use exper_moba_sib_genetics, clear 
	ivreghdfe log_earnings (yos_c=piv) `covs', ///
	absorb(`covabsib') cl(famid faar)
	est sto m_iv_moba_gen_sib0
	ivreghdfe log_earnings (yos_c=piv) `expcovs', ///
	absorb(`covabsib') cl(famid faar)
	est sto m_iv_moba_gen_sib1
* Estimates for Figure S11	
	esttab m_fullpop0 m_fullpop1 m_sib0 m_sib1 ///
	using "exp_results.rtf", ///
	keep(yos_c) b(3) se(4) ///
	mtitles("OLS" "OLS+Exp" "Siblings" "Siblings+Exp") ///
	replace
	esttab m_dz0 m_dz1 m_mz0 m_mz1 ///
	using "exp_results.rtf", ///
	keep(yos_c) b(3) se(4) ///
	mtitles("DZ" "DZ+Exp" "MZ" "MZ+Exp") ///
	append
	esttab m_iv_moba_gen0 m_iv_moba_gen1 ///
	m_iv_moba_gen_sib0 m_iv_moba_gen_sib1 ///
	using "exp_results.rtf", ///
	keep(yos_c) b(3) se(4) ///
	mtitles("MR" "MR+Exp" "MR Sib" "MR Sib+Exp") ///
	append

		