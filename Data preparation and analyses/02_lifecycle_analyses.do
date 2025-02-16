
********************************************************************************
* Data code for Widding-Havneraas, Demange, Elwert, Borgen, Ystrom, Zachrisson
* Estimating returns to education using genetically informed designs
* 
* Programming:
* 	Stata version: Stata MP 18
*	Original author: TW
* 	Contact: tarjei.widding-havneras@create.uio.no
*	Created: Nov-11-2023
*	Last modified: Jan-30-2025
********************************************************************************
* Data is based on raw files as issued by Statistics Norway and MoBa 

********************************************************************************
* NOTE: Following code based on code made available by Bhuller et al. 2017. 
* We are grateful to them making code openly available, and we have adapted their 
* code to our application. Any mistakes is the sole responsibility of author of 
* this adapted code. Code used as basis can be found here: 
* https://www.journals.uchicago.edu/doi/suppl/10.1086/692509
********************************************************************************	

********************************************************************************
**# A. DATA PREPARATION
********************************************************************************
********************************************************************************
**# 1. Population sample, education, income
********************************************************************************
	cd "N:\durable\projects\EQOP\MR_Income\temp"
	* Obtain registered status (migration)
	use "N:\durable\data\registers\SSB\01_data\data_v4.0\dta\POPULATION_REGSTAT.dta" 
	g out_year=aar if regstatus=="3"
	keep w19_0634_lnr out_year
	destring out_year, replace
	keep if out_year!=.
	sort w19_0634_lnr out_year
	* keep nonmissing out_year with first year migrated
	by w19_0634_lnr out_year, sort: gen nvals = _n == 1 
	keep if nvals==1
	drop nvals
	by w19_0634_lnr, sort: gen nvals = _n == 1 
	keep if nvals==1
	drop nvals
	save out_year.dta, replace
* Load population sample: using full population sample
	use w19_0634_lnr faar daar lopenr_far lopenr_mor using full_population.dta, clear
	rename (faar daar) (cohort dead_year)
	merge 1:1 w19_0634_lnr using faste_opplysninger_kopi, keepusing(foede_kommnune) keep(1 3) nogen
	merge 1:1 w19_0634_lnr using out_year.dta, keep(1 3) nogen
	save lt_pop_preres.dta, replace
* Add parental variables: 
	* municipality of residence when individual born (cohort)
	* 1975 earliest data for municipalities
	* Father
	frame create resid_f
	frame change resid_f
		use lopenr_far cohort using lt_pop_preres.dta, clear
		rename lopenr_far w19_0634_lnr
		merge m:1 w19_0634_lnr using ///
		"N:\durable\data\registers\SSB\01_data\data_v1.1\dta\POPULATION_BOSTEDSKOMMUNE", keep(3) nogen
		keep w19_0634_lnr cohort bostedskommune_01_01_1975-bostedskommune_01_01_1982
		gen munic_f=.
		destring bostedskommune_01_01_*, replace
		forvalues yr = 1975(1)1982 {
		replace munic_f = bostedskommune_01_01_`yr'	if cohort==`yr'
		}
	keep if munic_f!=.
	rename w19_0634_lnr lopenr_far
	keep lopenr_far cohort munic_f
	duplicates drop lopenr_far cohort, force
	save munic_f.dta, replace
	* Mother
	frame create resid_m
	frame change resid_m
		use lopenr_mor cohort using lt_pop_preres.dta, clear
		rename lopenr_mor w19_0634_lnr
		merge m:1 w19_0634_lnr using ///
		"N:\durable\data\registers\SSB\01_data\data_v1.1\dta\POPULATION_BOSTEDSKOMMUNE", keep(3) nogen
		keep w19_0634_lnr cohort bostedskommune_01_01_1975-bostedskommune_01_01_1982
		gen munic_m=.
		destring bostedskommune_01_01_*, replace
		forvalues yr = 1975(1)1982 {
		replace munic_m = bostedskommune_01_01_`yr'	if cohort==`yr'
		}
	keep if munic_m!=.
	rename w19_0634_lnr lopenr_mor
	duplicates drop lopenr_mor cohort, force
	keep lopenr_mor cohort munic_m
	save munic_m.dta, replace
	frame change default
	frame drop resid_f
	frame drop resid_m
	* Merge to sample
	use lt_pop_preres.dta, clear
		merge m:1 lopenr_far cohort using "munic_f.dta", keep (3 1) nogen ///
		keepus(munic_f)
		merge m:1 lopenr_mor cohort using "munic_m.dta", keep (3 1) nogen ///
		keepus(munic_m)
	* Generate childhood municipality: 
	* Municipality at birth or parents municipality when born if missing
	g tkom = foede_kommnune
	replace tkom = munic_m if tkom == .
	replace tkom = munic_f if tkom == .
	keep if tkom!=.
	save lt_pop_preres.dta, replace
	
	* Make indicators for death and migration
	use lt_pop_preres.dta, clear
	*Indicators for first year and last observation (year, age)
	g first_year = 1967 // income data starts in 1967
	qui forv c = 1959(1)1982 {
	replace first_year = `c' + 17 if (`c' + 17 > 1967) & (cohort==`c')
	}
	replace first_year = . if (dead_year<=first_year&dead_year!=.|out_year<=first_year&first_year!=.) 
	// never observed if died or outmigrated before age 17 or year 1967
	g last_year = 2022 // income data ends in 2022
	qui forv c = 1959(1)1982 {
	replace last_year = `c' + 62 if (`c' + 17 < 2022) & (cohort==`c')
	}
	 replace last_year = dead_year if (dead_year<=last_year&dead_year!=.) 
	 // not observed up to 2022 or age 62 if dead before that
	 replace last_year = out_year if (out_year<=last_year&out_year!=.)  
	 // not observed up to 2022 or age 62 if outmigrated before that
	 replace last_year = . if first_year == . 
	 // never observed if died or outmigrated before age 17 or year 1967
	 g first_age = (first_year-cohort)
	 g last_age = (last_year-cohort)
	 g obs_age = max(last_age - first_age,0) // total number of years observed
 
	*Generate age-specific indicators for being dead
	qui forv a = 0(1)62 {
	g dead_a`a' = 0
	cap replace dead_a`a' = 1 if (cohort + `a' >= dead_year & dead_year !=.) 
	cap replace dead_a`a' = . if (cohort + `a' > 2022)|(cohort + `a' < 1967) 
	// attrition happens outside observation window
	}

	*Generate age-specific indicators for having outmigrated
	qui forv a = 0(1)62 {
	g out_a`a' = 0
	cap replace out_a`a' = 1 if (cohort + `a' >= out_year & out_year !=.) 
	cap replace out_a`a' = . if (cohort + `a' > 2022)|(cohort + `a' < 1967) 
	// attrition happens outside observation window
	}
	save "lt_clean_basesample.dta", replace

********************************************************************************
**# 2. Municipality standard 1977-1988
********************************************************************************	
	use w19_0634_lnr tkom using "lt_clean_basesample.dta", clear	
	* recode tkom to unified 1977 standard (used 1977-1988)
	// https://www.ssb.no/klass/klassifikasjoner/131/versjon/1299/endringer
	* Generate new variable 
	gen kommune = tkom
	* 1958 changes
	replace kommune = 1106 if tkom == 1153
	* 1960 changes
	replace kommune = 815 if inlist(tkom, 801, 815, 816)
	replace kommune = 914 if inlist(tkom, 902, 914, 915)
	replace kommune = 929 if inlist(tkom, 929, 930)
	replace kommune = 937 if inlist(tkom, 936, 937)
	replace kommune = 1046 if inlist(tkom, 1045, 1046, 1047)
	replace kommune = 1563 if inlist(tkom, 1561, 1562, 1563)
	replace kommune = 1573 if inlist(tkom, 1573, 1574, 1575)
	* 1961 changes
	replace kommune = 125 if inlist(tkom, 125, 126)
	* 1962 changes
	replace kommune = 215 if inlist(tkom, 203, 215)
	replace kommune = 226 if inlist(tkom, 225, 226)
	replace kommune = 231 if inlist(tkom, 231, 232)
	replace kommune = 522 if inlist(tkom, 522, 523)
	replace kommune = 534 if inlist(tkom, 534, 535)
	replace kommune = 538 if inlist(tkom, 536, 537, 538, 539)
	replace kommune = 633 if inlist(tkom, 633, 634)
	replace kommune = 926 if inlist(tkom, 905)
	replace kommune = 918 if inlist(tkom, 914, 916, 917, 918)
	replace kommune = 926 if inlist(tkom, 924, 925, 926, 927)
	replace kommune = 940 if inlist(tkom, 939, 940)
	replace kommune = 1719 if inlist(tkom, 1701, 1716, 1719, 1720)
	replace kommune = 1714 if inlist(tkom, 1712, 1713, 1714, 1715)
	replace kommune = 1729 if inlist(tkom, 1728, 1729, 1730)
	replace kommune = 1824 if inlist(tkom, 1802, 1823, 1824, 1829)
	replace kommune = 1827 if inlist(tkom, 1818, 1819, 1827)
	replace kommune = 1854 if inlist(tkom, 1851, 1854)
	* 1963 changes
	replace kommune = 425 if inlist(tkom, 424, 425)
	replace kommune = 1032 if inlist(tkom, 1031, 1032, 1033)
	replace kommune = 1034 if inlist(tkom, 1034, 1035)
	replace kommune = 1037 if inlist(tkom, 1036, 1037, 1038)
	replace kommune = 1219 if inlist(tkom, 1218, 1219, 1220)
	replace kommune = 1426 if inlist(tkom, 1425, 1426, 1427)
	replace kommune = 1638 if inlist(tkom, 1637, 1638, 1639, 1659)
	replace kommune = 1860 if inlist(tkom, 1860, 1861, 1862, 1863)
	replace kommune = 1870 if inlist(tkom, 1869, 1870)
	* 1964 changes
	replace kommune = 103 if inlist(tkom, 103, 112, 132)
	replace kommune = 119 if inlist(tkom, 119, 120)
	replace kommune = 128 if inlist(tkom, 128, 129)
	replace kommune = 211 if inlist(tkom, 201, 211)
	replace kommune = 213 if inlist(tkom, 212, 213)
	replace kommune = 237 if inlist(tkom, 237, 240)
	replace kommune = 421 if inlist(tkom, 402, 421, 422)
	replace kommune = 412 if inlist(tkom, 411, 412, 413, 414)
	replace kommune = 417 if inlist(tkom, 416, 417)
	replace kommune = 501 if inlist(tkom, 501, 524)
	replace kommune = 502 if inlist(tkom, 502, 525, 526, 527)
	replace kommune = 528 if inlist(tkom, 528, 531)
	replace kommune = 529 if inlist(tkom, 529, 530)
	replace kommune = 601 if inlist(tkom, 601, 611, 612, 613, 614)
	replace kommune = 604 if inlist(tkom, 604, 629, 630)
	replace kommune = 711 if inlist(tkom, 701, 711)
	replace kommune = 702 if inlist(tkom, 702, 715)
	replace kommune = 602 if inlist(tkom, 712)
	replace kommune = 805 if inlist(tkom, 805, 804, 813)
	replace kommune = 806 if inlist(tkom, 806, 812, 818)
	replace kommune = 807 if inlist(tkom, 807, 823, 825)
	replace kommune = 814 if inlist(tkom, 802, 803)
	replace kommune = 819 if inlist(tkom, 819, 820)
	replace kommune = 833 if inlist(tkom, 832, 833)
	replace kommune = 834 if inlist(tkom, 834, 835)
	replace kommune = 901 if inlist(tkom, 901, 913)
	replace kommune = 1014 if inlist(tkom, 1014, 1015, 1016)
	replace kommune = 1002 if inlist(tkom, 1002, 1019, 1020)
	replace kommune = 1021 if inlist(tkom, 1021, 1022, 1024)
	replace kommune = 1119 if inlist(tkom, 1117, 1118, 1119)
	replace kommune = 1214 if inlist(tkom, 1214, 1215)
	replace kommune = 1216 if inlist(tkom, 1216, 1217)
	replace kommune = 1228 if inlist(tkom, 1228, 1229)
	replace kommune = 1230 if inlist(tkom, 1230, 1231, 1232)
	replace kommune = 1235 if inlist(tkom, 1235, 1236, 1237)
	replace kommune = 1241 if inlist(tkom, 1241, 1239, 1240)
	replace kommune = 1401 if inlist(tkom, 1434, 1435, 1436, 1437)
	replace kommune = 1511 if inlist(tkom, 1511, 1512, 1513)
	replace kommune = 1520 if inlist(tkom, 1520, 1521, 1522)
	replace kommune = 1532 if inlist(tkom, 1532, 1533)
	replace kommune = 1535 if inlist(tkom, 1535, 1536)
	replace kommune = 1539 if inlist(tkom, 1537, 1538, 1539, 1540)
	replace kommune = 1548 if inlist(tkom, 1548, 1549, 1550)
	replace kommune = 1617 if inlist(tkom, 1615, 1616, 1617, 1618)
	replace kommune = 1620 if inlist(tkom, 1619, 1620)
	replace kommune = 1627 if inlist(tkom, 1627, 1628, 1629)
	replace kommune = 1640 if inlist(tkom, 1640, 1641, 1642, 1643, 1646, 1647, 1649)
	replace kommune = 1653 if inlist(tkom, 1650, 1651, 1652, 1653)
	replace kommune = 1738 if inlist(tkom, 1737, 1738)
	replace kommune = 1742 if inlist(tkom, 1741, 1742)
	replace kommune = 1751 if inlist(tkom, 1751, 1752, 1754)
	replace kommune = 1814 if inlist(tkom, 1801, 1812, 1813, 1814)
	replace kommune = 1833 if inlist(tkom, 1831, 1833)
	replace kommune = 1848 if inlist(tkom, 1846, 1847, 1848)
	replace kommune = 1865 if inlist(tkom, 1806, 1864, 1865)
	replace kommune = 1868 if inlist(tkom, 1868, 1869)
	replace kommune = 1871 if inlist(tkom, 1871, 1872, 1873)
	replace kommune = 1901 if inlist(tkom, 1901, 1912, 1914)
	replace kommune = 1902 if inlist(tkom, 1902, 1930, 1934, 1937)
	replace kommune = 1924 if inlist(tkom, 1923, 1924, 1932, 1933)
	replace kommune = 2012 if inlist(tkom, 2012, 2013)
	replace kommune = 2025 if inlist(tkom, 2025, 2026)
	replace kommune = 1133 if inlist(tkom, 1132, 1133, 1138)
	* 1965 changes
	replace kommune = 402 if inlist(tkom, 421)
	replace kommune = 430 if inlist(tkom, 430, 431)
	replace kommune = 432 if inlist(tkom, 432, 433)
	replace kommune = 517 if inlist(tkom, 516, 517)
	replace kommune = 717 if inlist(tkom, 704, 717, 721)
	replace kommune = 1001 if inlist(tkom, 1001, 1011, 1012, 1013)
	replace kommune = 1003 if inlist(tkom, 1003, 1039, 1040, 1041)
	replace kommune = 1004 if inlist(tkom, 1004, 1042, 1043, 1044, 1045)
	replace kommune = 1101 if inlist(tkom, 1101, 1113, 1115, 1116)
	replace kommune = 1102 if inlist(tkom, 1102, 1123, 1126, 1128)
	replace kommune = 1103 if inlist(tkom, 1103, 1125, 1126)
	replace kommune = 1149 if inlist(tkom, 1104, 1105, 1148, 1149, 1150, 1152)
	replace kommune = 1134 if inlist(tkom, 1134, 1136, 1137)
	replace kommune = 1141 if inlist(tkom, 1140, 1141)
	replace kommune = 1142 if inlist(tkom, 1142, 1143)
	replace kommune = 1146 if inlist(tkom, 1139, 1146, 1147)
	replace kommune = 1154 if inlist(tkom, 1154, 1155, 1156, 1157, 1158)
	replace kommune = 1211 if inlist(tkom, 1211, 1212)
	replace kommune = 1224 if inlist(tkom, 1213, 1224, 1225)
	replace kommune = 1238 if inlist(tkom, 1226, 1238)
	replace kommune = 1438 if inlist(tkom, 1438, 1442)
	replace kommune = 1443 if inlist(tkom, 1443, 1444)
	replace kommune = 1448 if inlist(tkom, 1447, 1448)
	replace kommune = 1525 if inlist(tkom, 1523, 1525)
	replace kommune = 1527 if inlist(tkom, 1526, 1527, 1529)
	replace kommune = 1534 if inlist(tkom, 1530, 1531, 1534)
	replace kommune = 1545 if inlist(tkom, 1545)
	replace kommune = 1547 if inlist(tkom, 1547)
	replace kommune = 1557 if inlist(tkom, 1557, 1558)
	replace kommune = 1566 if inlist(tkom, 1564, 1565, 1566)
	replace kommune = 1569 if inlist(tkom, 1568, 1569, 1570, 1572)
	replace kommune = 1657 if inlist(tkom, 1656, 1657, 1658)
	replace kommune = 1820 if inlist(tkom, 1817, 1820, 1821)
	* 1966 changes
	replace kommune = 221 if inlist(tkom, 221, 222, 223, 224)
	replace kommune = 435 if inlist(tkom, 435, 436)
	replace kommune = 437 if inlist(tkom, 437, 440)
	replace kommune = 518 if inlist(tkom, 518, 519) 
	replace kommune = 1635 if inlist(tkom, 440, 1635)
	* 1967 changes 
	replace kommune = 101 if inlist(tkom, 101, 116, 117)
	replace kommune = 919 if inlist(tkom, 919, 932)
	replace kommune = 928 if inlist(tkom, 928, 933, 934)
	replace kommune = 929 if inlist(tkom, 929, 931)
	* 1968 changes
	replace kommune = 706 if inlist(tkom, 706, 724)
	replace kommune = 1501 if inlist(tkom, 1501, 1531)
	replace kommune = 1804 if inlist(tkom, 1804, 1843)
	* 1971 changes
	replace kommune = 904 if inlist(tkom, 904, 923, 924)
	* 1972 changes
	replace kommune = 1201 if inlist(tkom, 1248, 1249, 1250, 1255, 1301)
	replace kommune = 1644 if inlist(tkom, 1644, 1645)
	* 1974 changes
	replace kommune = 1805 if inlist(tkom, 1805, 1855)
	* 1976 changes
	replace kommune = 436 if tkom == 435 
	replace kommune = 1859 if tkom == 1858 
	unique kommune // n=513 reduced to correct n=457 in 1977-standard
	keep w19_0634_lnr kommune
	rename kommune tkom
	save "lt_clean_tkom.dta", replace	
	
********************************************************************************
**# 3. Education
********************************************************************************	
	* Use maximum years of schooling 
	use all_ed_1970_2022.dta, clear
	g yos = 0
	qui forv t = 1970(1)1970 {
	 replace yos = edu_all_`t'    if (edu_all_`t' != . & edu_all_`t' > yos)
	 }
	qui forv t = 1980(1)2022 {
	 replace yos = edu_all_`t'    if (edu_all_`t' != . & edu_all_`t' > yos)
	 }
	replace yos = . if yos == 0
	fre yos
	recode yos 0=0 1=6 2=9 3=11 4=12 5=13 6=16 7=18 8=23
	label define educlbl /// 
			0 "No educ." /// 
			6 "PS" /// 			 // 1 "1-7gr" 
			9 "LSS" /// 		 // 2 lower secondary school, "8-10gr" 
			11 "Some HS" /// 	 // 3 "some HS"
			12 "Complete HS" /// // 4 "complete high school" 
			13 "Extra HS" /// 	 // 5 "extra high school" 
			16 "BA" ///  		 //	6 "BA"
			18 "MA" ///  		 // 7 "MA"
			23 "PhD", modify 	 // 8 "PhD"
	label values yos educlbl
	* Drop few final sample with <=6
	drop if yos<=6
	keep if yos!=.
	keep w19_0634_lnr yos
	save "lt_clean_edudata.dta", replace

********************************************************************************
**# 4. Earnings
********************************************************************************
	use w19_0634_lnr pincome_w aargang using pincome_inf_adj.dta, clear 
	rename pincome_w linntW_
	reshape wide linntW_, i(w19_0634_lnr) j(aargang)
	save lt_incdata.dta, replace
	merge 1:1 w19_0634_lnr using "lt_clean_basesample.dta", keep(3) nogen	
	* Note: for birth cohorts 1959-1982 and earnings ages 17-62, 
	* using earnings data from 1976 onwards
	di 1959+17
	drop linntW_1967-linntW_1975
	*Account for death and out-migration & missing income observations
	 qui foreach inc in linntW {
	  forv t = 1976(1)2022 {
	  replace `inc'_`t' =  0 if (`t'<dead_year|dead_year==.)&(`t'<out_year|out_year==.)&`inc'_`t'==.
	  replace `inc'_`t' =  . if (`t'>=dead_year&dead_year!=.)|(`t'>=out_year&out_year!=.)  
	  } 
	 }
	 *Construct age-specific income varibales, ages 17-62
	 qui foreach inc in linntW {

	  forv a = 17(1)62 {
	  g `inc'_a`a' = 0
	   forv c = 1959(1)1982 {
		loc t = `c' + `a'
		cap replace `inc'_a`a' =  `inc'_`t' if cohort == `c'
		cap replace `inc'_a`a' = .          if cohort == `c' & (`t'<1976|`t'>2022)  
	   }
	  } 
	  forv t = 1976(1)2022 {
	   drop `inc'_`t'
	  }  
	 
	 }	
 	 * NOTE: The ordering av variables in your dataset should be correct 
	 *            when using rowmean() or rownonmiss() commands below!!
	 *LIFETIME INCOME MEASURES 1: Mean income
	 *  original: age intervals: 17-62, 17-50, 17-24, 25-44, 45-62
	  qui foreach inc in linntW {
	   egen double mlt_`inc'_a1762 = rowmean(`inc'_a17-`inc'_a62)
	   egen double mlt_`inc'_a1750 = rowmean(`inc'_a17-`inc'_a50)   
	   egen double mlt_`inc'_a1724 = rowmean(`inc'_a17-`inc'_a24)
	   egen double mlt_`inc'_a2544 = rowmean(`inc'_a25-`inc'_a44)   
	   egen double mlt_`inc'_a4562 = rowmean(`inc'_a45-`inc'_a62)    
	  }
	 *LIFETIME INCOME MEASURES 2: Annuity income
	 *   use a real interest rate of 2.3 for discounting
	qui foreach inc in linntW {
	   forv a = 17(1)62 {
		g `inc'_pv16_a`a' = `inc'_a`a' / 1.023^`a' // discounted to age 16
	   }
	   egen double pv_`inc'_a1762 = rowtotal(`inc'_pv16_a17-`inc'_pv16_a62)    // present value formula
	   egen double n_`inc'_a1762 = rownonmiss(`inc'_pv16_a17-`inc'_pv16_a62)  // no. of periods
	   g alt_`inc'_a1762 = pv_`inc'_a1762*0.023/(1.023*(1-(1.023)^(-n_`inc'_a1762))) // annuity value formula
	   drop `inc'_pv16_a* pv_`inc'_a1762 n_`inc'_a1762 
	  }
	drop cohort dead_year out_year 
	save "lt_clean_incdata.dta", replace	

********************************************************************************
**# 5. Merge cleaned datasets
********************************************************************************
** Create estimation samples
*** OLS estimation sample
	u "lt_clean_basesample.dta", clear
	* tkom
	merge 1:1 w19_0634_lnr using "lt_clean_tkom.dta", keep(3) keepusing(tkom) nogen
	*education data
	merge 1:1 w19_0634_lnr using "lt_clean_edudata.dta", keep(3) nogen
	*income data
	merge 1:1 w19_0634_lnr using "lt_clean_incdata.dta", keep(3) nogen
	* Define indicators for attrition (dead/out-migrated) over ages 17-62
	* attrition at individual level /
	qui forv a = 17(1)62 {
	g att_a`a' = (dead_a`a'==1|out_a`a'==1) if (dead_a`a'!=.|out_a`a'!=.) 
	// missing if attrition happens outside observation window
	} 
	*Retain relevant variables and construct estimation sample
	*drop dead_a* out_a*
	order w19_0634_lnr cohort-out_year yos
	save "lt_final_estsample_ols.dta", replace 
*** Sibling sample
	use "lt_final_estsample_ols.dta", clear 
	* Keep >=2 siblings 
	* Family id 
		egen famid = group(lopenr_mor lopenr_far)
	* Total number of siblings
		bys famid: gen kids=_N if famid!=.
		fre kids
		*<1% have more than 5 kids
		replace kids=5 if kids>5 & kids!=.
		keep if kids >=2	
	*Sibling FEs: attrition at sibling level
	*merge 1:1 w19_0634_lnr using lt_clean_basesample, keepusing(dead_a* out_a*) nogen keep(3)
	qui forv a = 17(1)62 {
	bys famid: egen tot_att_a`a' = total(att_a`a')
	g att_sib_a`a' = (tot_att_a`a'==1|tot_att_a`a'==2|tot_att_a`a'==3|tot_att_a`a'==4|tot_att_a`a'==5) ///
	if famid !=. & (dead_a`a'!=.|out_a`a'!=.)
	drop tot_att_a*
	}
	*drop dead_a* out_a*
	save "lt_final_estsample_sib.dta", replace
*** Twins sample 
	use "lt_final_estsample_ols.dta", clear
	gen SSBLNr = w19_0634_lnr
	merge 1:1 SSBLNr using "N:\durable\data\registers\twin registry\PDB2601_NTR_2023.dta", ///
	keep(3) nogen
	egen twinpair = group(lopenr_mor lopenr_mor) if lopenr_mor!="" & lopenr_far!=""
	bys twinpair: gen n_twins=_N if lopenr_mor!="" & lopenr_far!=""
	fre n_twins
	keep if n_twins>=2 & n_twins!=.
	keep if Zygo!=.
	*Twin pair FEs: attrition at twin pair level
	qui forv a = 17(1)62 {
	bys twinpair: egen tot_att_a`a' = total(att_a`a')
	g att_tw_a`a' = (tot_att_a`a'==1|tot_att_a`a'==2|tot_att_a`a'==3|tot_att_a`a'==4) if twinpair !=. & (dead_a`a'!=.|out_a`a'!=.)
	drop tot_att_a*
	}
	*drop dead_a* out_a*
	save "lt_final_estsample_twin.dta", replace
*** Generate OLS group data
 	** Collapse OLS to cohort x municipality level (with weights)
	*OLS estimation sample -- group-level
	u "lt_final_estsample_ols.dta", clear
	*overall weights
	bys cohort tkom: g N_a1762 = _N
	*age-specific weights -- non-attrited observations
	global N_list "N_a17=linntW_a17"
	forv a = 18(1)62 {
	global N_list "$N_list N_a`a'=linntW_a`a'" 
	}
	*age-interval specific weights
	global NT_list  "N_a1724=mlt_linntW_a1724 N_a2544=mlt_linntW_a2544 N_a4562=mlt_linntW_a4562"
	*collapse         
	collapse (mean) N_a1762 yos mlt* alt* linntW* ///
	(count) $N_list $Nimp_list $Nw_list $NT_list $NTw_list, by(cohort tkom) fast
	save "lt_final_estsample_olsG.dta", replace		
	
********************************************************************************
** MR
********************************************************************************
	use lt_incdata.dta, clear
	merge 1:1 w19_0634_lnr using "lt_clean_basesample.dta", keep(3) nogen	
	* Note: for birth cohorts 1959-1982 and earnings ages 17-62, 
	* using earnings data from 1976 onwards
	di 1959+17
	drop linntW_1967-linntW_1975
	*Account for death and out-migration & missing income observations
	 qui foreach inc in linntW {
	  forv t = 1976(1)2022 {
	  replace `inc'_`t' =  0 if (`t'<dead_year|dead_year==.)&(`t'<out_year|out_year==.)&`inc'_`t'==.
	  replace `inc'_`t' =  . if (`t'>=dead_year&dead_year!=.)|(`t'>=out_year&out_year!=.)  
	  } 
	 }
	 *Construct age-specific income varibales, ages 17-52
	 qui foreach inc in linntW {

	  forv a = 17(1)52 {
	  g `inc'_a`a' = 0
	   forv c = 1959(1)1982 {
		loc t = `c' + `a'
		cap replace `inc'_a`a' =  `inc'_`t' if cohort == `c'
		cap replace `inc'_a`a' = .          if cohort == `c' & (`t'<1976|`t'>2022)  
	   }
	  } 

	  forv t = 1976(1)2022 {
	   drop `inc'_`t'
	  }  
	 
	 }	
 	 * NOTE: The ordering av variables in your dataset should be correct 
	 * when using rowmean() or rownonmiss() commands below!!

	 *LIFETIME INCOME MEASURES 1: Mean income
	 *  age intervals: 17-52, 17-50, 17-24, 25-44, 45-52
	  qui foreach inc in linntW {
	   egen double mlt_`inc'_a1752 = rowmean(`inc'_a17-`inc'_a52)
	   egen double mlt_`inc'_a1750 = rowmean(`inc'_a17-`inc'_a50)   
	   egen double mlt_`inc'_a1724 = rowmean(`inc'_a17-`inc'_a24)
	   egen double mlt_`inc'_a2544 = rowmean(`inc'_a25-`inc'_a44)   
	   egen double mlt_`inc'_a4552 = rowmean(`inc'_a45-`inc'_a52)    
	  }

	 *LIFETIME INCOME MEASURES 2: Annuity income
	 *   use a real interest rate of 2.3 for discounting
	qui foreach inc in linntW {
	forv a = 17(1)52 {
	g `inc'_pv16_a`a' = `inc'_a`a' / 1.023^`a' // discounted to age 16
	}
	  egen double pv_`inc'_a1752 = rowtotal(`inc'_pv16_a17-`inc'_pv16_a52)    // present value formula
	  egen double n_`inc'_a1752 = rownonmiss(`inc'_pv16_a17-`inc'_pv16_a52)  // no. of periods
	  g alt_`inc'_a1752 = pv_`inc'_a1752*0.023/(1.023*(1-(1.023)^(-n_`inc'_a1752))) // annuity value formula
	  drop `inc'_pv16_a* pv_`inc'_a1752 n_`inc'_a1752 
	  }
	drop cohort dead_year out_year 
	save "lt_clean_incdata_1752.dta", replace	
	* PART 2: Merge cleaned datasets: Estimation samples	
	u "lt_clean_basesample.dta", clear
	* tkom
	merge 1:1 w19_0634_lnr using "lt_clean_tkom.dta", keep(3) keepusing(tkom) nogen
	*education data
	merge 1:1 w19_0634_lnr using "lt_clean_edudata.dta", keep(3) nogen
	*income data
	merge 1:1 w19_0634_lnr using "lt_clean_incdata_1752.dta", keep(3) nogen
	* Define indicators for attrition (dead/out-migrated) over ages 17-52
	* attrition at individual level /
	qui forv a = 17(1)52 {
	g att_a`a' = (dead_a`a'==1|out_a`a'==1) if (dead_a`a'!=.|out_a`a'!=.) 
	// missing if attrition happens outside observation window
	} 
 	*Retain relevant variables and construct estimation sample
	order w19_0634_lnr cohort-out_year yos
	save "lt_final_estsample_ols_1752.dta", replace 
	* MR sample   
	* Get PGI-EA
	use "N:\durable\data\moba\linkage\merged_IDs\MoBa_SSB_IDs.dta", clear	
	drop if w19_0634_lnr=="NA"
	drop if sentrix_id=="NA"
	duplicates drop w19_0634_lnr, force
	merge 1:1 sentrix_id using ea_mr_parents.dta, keep(3) nogen
	merge 1:1 w19_0634_lnr using "lt_final_estsample_ols_1752.dta", keep(3) nogen
	egen piv=std(SCORE)
	save "lt_final_estsample_iv.dta", replace
*** Generate MR group data
	** Collapse MR to cohort x municipality level (with weights)
	u "lt_final_estsample_iv.dta", clear
	*overall weights
	bys cohort tkom: g N_a1752 = _N
	*age-specific weights -- non-attrited observations
	global N_list "N_a17=linntW_a17"
	forv a = 18(1)52 {
	global N_list "$N_list N_a`a'=linntW_a`a'" 
	}
	*age-interval specific weights
	global NT_list  "N_a1724=mlt_linntW_a1724 N_a2544=mlt_linntW_a2544 N_a4552=mlt_linntW_a4552"
	*collapse         
	collapse (mean) N_a1752 piv yos mlt* alt* linntW* ///
	(count) $N_list $Nimp_list $Nw_list $NT_list $NTw_list, by(cohort tkom) fast
	save "lt_final_estsample_ivG.dta", replace	
	
********************************************************************************
**# B. LIFECYCLE ANALYSIS
********************************************************************************	
	
********************************************************************************
**# B1. TABLES
********************************************************************************	
**# 1. OLS
u mlt_linntW_a* alt_linntW_a* cohort tkom yos using "lt_final_estsample_ols.dta", clear
*Panel: Education premiums over the life-cycle (means)
qui foreach t in a1724 a2544 a4562 {
 foreach y in mlt_linntW {
 loc var_`y'_`t' `y'_`t'
 xi: ivreg2 `y'_`t' yos i.cohort i.tkom, cluster(tkom cohort) partial(i.tkom i.cohort)
 est store est_`y'_`t'
 scalar est_`y'_`t' = _b[yos]
 su `y'_`t'
 scalar rest_`y'_`t' = scalar(est_`y'_`t')/r(mean)
 local rest_`y'_`t' = round(scalar(rest_`y'_`t'),0.0001) 
 }
}
*Panel: Lifetime education premiums (means & annuity values)
qui foreach t in a1762 {
 foreach y in mlt_linntW alt_linntW {
 loc var_`y'_`t' `y'_`t'
 xi: ivreg2 `y'_`t' yos i.cohort i.tkom, cluster(tkom cohort) partial(i.tkom i.cohort)
 est store est_`y'_`t'
 scalar est_`y'_`t' = _b[yos]
 su `y'_`t'
 scalar rest_`y'_`t' = scalar(est_`y'_`t')/r(mean)
 local rest_`y'_`t' = round(scalar(rest_`y'_`t'),0.0001) 
 }
}
* Output estimation results, panels:
forv  t=1(1)1 { 
 noi di as text "{hline 90}"
 noi di "Outcomes: `mlt_linntW_a1724' `mlt_linntW_a2544' `mlt_linntW_a4562' `mlt_linntW_a1762' `alt_linntW_a1762'"
 noi di as text "{hline 90}"
 noi estout est_mlt_linntW_a1724 est_mlt_linntW_a2544 est_mlt_linntW_a4562 est_mlt_linntW_a1762 est_alt_linntW_a1762, cells(b(star fmt(%9.1f)) se(par fmt(%9.1f))) starlevel(* 0.10 ** 0.05 *** 0.01) keep(yos) stats(r2_a F N, fmt(%9.3f %9.3f %9.0f))
 noi di "Scaled wrt DM: --- `rest_mlt_linntW_a1724' --------- `rest_mlt_linntW_a2544' ---------- `rest_mlt_linntW_a4562' ---------- `rest_mlt_linntW_a1762' ---------- `rest_alt_linntW_a1762'"
 noi di as text "{hline 90}"
}
est drop _all 
**Panel: Internal rate of return (bootstrap SEs with 250 reps)
u "lt_final_estsample_olsG.dta", clear // Use weighted group-level data to reduce computation time
cap program drop irr
program irr, eclass
version 12
tempname b

 qui forv a = 17(1)62 {
  foreach y in linntW {
  xtset tkom
  xi: xtivreg2 `y'_a`a' yos i.cohort [aw=N_a`a'], fe
  scalar est`a' = _b[yos]
  }
 }

 g       xvar_l = 1 if _n == 1
 replace xvar_l = 0 if _n != 1
 g       xvar_r = 0 if _n == 1
 replace xvar_r = 1 if _n != 1

qui nl ( xvar_l =   scalar(est17)/(1+{irr})^(17-16) + scalar(est18)/(1+{irr})^(18-16) + scalar(est19)/(1+{irr})^(19-16) +scalar(est20)/(1+{irr})^(20-16) + scalar(est21)/(1+{irr})^(21-16) + scalar(est22)/(1+{irr})^(22-16) + scalar(est23)/(1+{irr})^(23-16) + scalar(est24)/(1+{irr})^(24-16) + scalar(est25)/(1+{irr})^(25-16) + scalar(est26)/(1+{irr})^(26-16) + scalar(est27)/(1+{irr})^(27-16) + scalar(est28)/(1+{irr})^(28-16) + scalar(est29)/(1+{irr})^(29-16) + scalar(est30)/(1+{irr})^(30-16) + scalar(est31)/(1+{irr})^(31-16) + scalar(est32)/(1+{irr})^(32-16) + scalar(est33)/(1+{irr})^(33-16) + scalar(est34)/(1+{irr})^(34-16) + scalar(est35)/(1+{irr})^(35-16) + scalar(est36)/(1+{irr})^(36-16) + scalar(est37)/(1+{irr})^(37-16) + scalar(est38)/(1+{irr})^(38-16) + scalar(est39)/(1+{irr})^(39-16) + scalar(est40)/(1+{irr})^(40-16) + scalar(est41)/(1+{irr})^(41-16) + scalar(est42)/(1+{irr})^(42-16) + scalar(est43)/(1+{irr})^(43-16) + scalar(est44)/(1+{irr})^(44-16) + scalar(est45)/(1+{irr})^(45-16) + scalar(est46)/(1+{irr})^(46-16) + scalar(est47)/(1+{irr})^(47-16) + scalar(est48)/(1+{irr})^(48-16) + scalar(est49)/(1+{irr})^(49-16) + scalar(est50)/(1+{irr})^(50-16) + scalar(est51)/(1+{irr})^(51-16) + scalar(est52)/(1+{irr})^(52-16) + scalar(est53)/(1+{irr})^(53-16) + scalar(est54)/(1+{irr})^(54-16) + scalar(est55)/(1+{irr})^(55-16) + scalar(est56)/(1+{irr})^(56-16) + scalar(est57)/(1+{irr})^(57-16) + scalar(est58)/(1+{irr})^(58-16) + scalar(est59)/(1+{irr})^(59-16) + scalar(est60)/(1+{irr})^(60-16) + scalar(est61)/(1+{irr})^(61-16) + scalar(est62)/(1+{irr})^(62-16) + 1 - xvar_r ) if _n < 3
 matrix `b' = e(b)
 ereturn post `b'
 cap drop xvar*

end

*Output:
bootstrap _b, reps(250) seed(30031699) cluster(tkom cohort) nowarn: irr
estat bootstrap, all	

**# 2. Siblings
u att_sib* mlt_linntW* alt_linntW* linntW* cohort tkom yos famid using "lt_final_estsample_sib.dta", clear
*Panel: Education premiums over the life-cycle (means)
qui foreach t in a1724 a2544 a4562 {
 foreach y in mlt_linntW {
 loc var_`y'_`t' `y'_`t'
 xtset famid
 xtreg `y'_`t' yos, fe
 est store est_`y'_`t'
 scalar est_`y'_`t' = _b[yos]
 su `y'_`t'
 scalar rest_`y'_`t' = scalar(est_`y'_`t')/r(mean)
 local rest_`y'_`t' = round(scalar(rest_`y'_`t'),0.0001) 
 }
}
*Panel: Lifetime education premiums (means & annuity values)
qui foreach t in a1762 {
 foreach y in mlt_linntW alt_linntW {
 xtset famid
 xtreg `y'_`t' yos, fe
 est store est_`y'_`t'
 scalar est_`y'_`t' = _b[yos]
 su `y'_`t'
 scalar rest_`y'_`t' = scalar(est_`y'_`t')/r(mean)
 local rest_`y'_`t' = round(scalar(rest_`y'_`t'),0.0001) 
 }
}
* Output estimation results, panels:
forv  t=1(1)1 { 
 noi di as text "{hline 90}"
 noi di "Outcomes: `mlt_linntW_a1724' `mlt_linntW_a2544' `mlt_linntW_a4562' `mlt_linntW_a1762' `alt_linntW_a1762'"
 noi di as text "{hline 90}"
 noi estout est_mlt_linntW_a1724 est_mlt_linntW_a2544 est_mlt_linntW_a4562 est_mlt_linntW_a1762 est_alt_linntW_a1762, cells(b(star fmt(%9.1f)) se(par fmt(%9.1f))) starlevel(* 0.10 ** 0.05 *** 0.01) keep(yos) stats(r2_a F N, fmt(%9.3f %9.3f %9.0f))
 noi di "Scaled wrt DM: --- `rest_mlt_linntW_a1724' --------- `rest_mlt_linntW_a2544' ---------- `rest_mlt_linntW_a4562' ---------- `rest_mlt_linntW_a1762' ---------- `rest_alt_linntW_a1762'"
 noi di as text "{hline 90}"
}
est drop _all 
*Panel: Internal rate of return (bootstrap SEs with 250 reps)
u att_sib* mlt_linntW* alt_linntW* linntW* cohort tkom yos famid using "lt_final_estsample_sib.dta", clear
cap program drop irr
program irr, eclass
version 12
tempname b

 qui forv a = 17(1)62 {
  foreach y in linntW {
  xtset famid
  xtreg `y'_a`a' yos if att_sib_a`a' == 0, fe
  scalar est`a' = _b[yos]
  }
 }

 g       xvar_l = 1 if _n == 1
 replace xvar_l = 0 if _n != 1
 g       xvar_r = 0 if _n == 1
 replace xvar_r = 1 if _n != 1

 qui nl ( xvar_l =   scalar(est17)/(1+{irr})^(17-16) + scalar(est18)/(1+{irr})^(18-16) + scalar(est19)/(1+{irr})^(19-16) + scalar(est20)/(1+{irr})^(20-16) + scalar(est21)/(1+{irr})^(21-16) + scalar(est22)/(1+{irr})^(22-16) + scalar(est23)/(1+{irr})^(23-16) + scalar(est24)/(1+{irr})^(24-16) + scalar(est25)/(1+{irr})^(25-16) + scalar(est26)/(1+{irr})^(26-16) + scalar(est27)/(1+{irr})^(27-16) + scalar(est28)/(1+{irr})^(28-16) + scalar(est29)/(1+{irr})^(29-16) + scalar(est30)/(1+{irr})^(30-16) + scalar(est31)/(1+{irr})^(31-16) + scalar(est32)/(1+{irr})^(32-16) + scalar(est33)/(1+{irr})^(33-16) + scalar(est34)/(1+{irr})^(34-16) + scalar(est35)/(1+{irr})^(35-16) + scalar(est36)/(1+{irr})^(36-16) + scalar(est37)/(1+{irr})^(37-16) + scalar(est38)/(1+{irr})^(38-16) + scalar(est39)/(1+{irr})^(39-16) + scalar(est40)/(1+{irr})^(40-16) + scalar(est41)/(1+{irr})^(41-16) + scalar(est42)/(1+{irr})^(42-16) + scalar(est43)/(1+{irr})^(43-16) + scalar(est44)/(1+{irr})^(44-16) + scalar(est45)/(1+{irr})^(45-16) + scalar(est46)/(1+{irr})^(46-16) + scalar(est47)/(1+{irr})^(47-16) + scalar(est48)/(1+{irr})^(48-16) + scalar(est49)/(1+{irr})^(49-16) + scalar(est50)/(1+{irr})^(50-16) + scalar(est51)/(1+{irr})^(51-16) + scalar(est52)/(1+{irr})^(52-16) + scalar(est53)/(1+{irr})^(53-16) + scalar(est54)/(1+{irr})^(54-16) + scalar(est55)/(1+{irr})^(55-16) + scalar(est56)/(1+{irr})^(56-16) + scalar(est57)/(1+{irr})^(57-16) + scalar(est58)/(1+{irr})^(58-16) + scalar(est59)/(1+{irr})^(59-16) + scalar(est60)/(1+{irr})^(60-16) + scalar(est61)/(1+{irr})^(61-16) + scalar(est62)/(1+{irr})^(62-16)  + 1 - xvar_r ) if _n < 3
 matrix `b' = e(b)
 ereturn post `b'
 cap drop xvar*

end
*Output:
bootstrap _b, reps(250) seed(30031699) cluster(famid) nowarn: irr
estat bootstrap, all

**# 3. Twins
*Panel: Education premiums over the life-cycle (means)
u att_tw* mlt_linntW* alt_linntW* linntW* cohort tkom yos twinpair using "lt_final_estsample_twin.dta", clear
qui foreach t in a1724 a2544 a4562 {
 foreach y in mlt_linntW {
 loc var_`y'_`t' `y'_`t'
 xtset twinpair
 xtreg `y'_`t' yos, fe
 est store est_`y'_`t'
 scalar est_`y'_`t' = _b[yos]
 su `y'_`t'
 scalar rest_`y'_`t' = scalar(est_`y'_`t')/r(mean)
 local rest_`y'_`t' = round(scalar(rest_`y'_`t'),0.0001) 
 }
}
*Panel: Lifetime education premiums (means & annuity values)
qui foreach t in a1762 {
 foreach y in mlt_linntW alt_linntW {
 xtset twinpair
 xtreg `y'_`t' yos, fe
 est store est_`y'_`t'
 scalar est_`y'_`t' = _b[yos]
 su `y'_`t'
 scalar rest_`y'_`t' = scalar(est_`y'_`t')/r(mean)
 local rest_`y'_`t' = round(scalar(rest_`y'_`t'),0.0001) 
 }
}
* Output estimation results, panels:
forv  t=1(1)1 { 
 noi di as text "{hline 90}"
 noi di "Outcomes: `mlt_linntW_a1724' `mlt_linntW_a2544' `mlt_linntW_a4562' `mlt_linntW_a1762' `alt_linntW_a1762'"
 noi di as text "{hline 90}"
 noi estout est_mlt_linntW_a1724 est_mlt_linntW_a2544 est_mlt_linntW_a4562 est_mlt_linntW_a1762 est_alt_linntW_a1762, cells(b(star fmt(%9.1f)) se(par fmt(%9.1f))) starlevel(* 0.10 ** 0.05 *** 0.01) keep(yos) stats(r2_a F N, fmt(%9.3f %9.3f %9.0f))
 noi di "Scaled wrt DM: --- `rest_mlt_linntW_a1724' --------- `rest_mlt_linntW_a2544' ---------- `rest_mlt_linntW_a4562' ---------- `rest_mlt_linntW_a1762' ---------- `rest_alt_linntW_a1762'"
 noi di as text "{hline 90}"
}
est drop _all 
*Panel: Internal rate of return (bootstrap SEs with 250 reps)
u att_tw* mlt_linntW* alt_linntW* linntW* cohort tkom yos twinpair using "lt_final_estsample_twin.dta", clear
cap program drop irr
program irr, eclass
version 12
tempname b

 qui forv a = 17(1)62 {
  foreach y in linntW {
  xtset twinpair
  xtreg `y'_a`a' yos if att_tw_a`a' == 0, fe
  scalar est`a' = _b[yos]
  }
 }

 g       xvar_l = 1 if _n == 1
 replace xvar_l = 0 if _n != 1
 g       xvar_r = 0 if _n == 1
 replace xvar_r = 1 if _n != 1

 qui nl ( xvar_l =   scalar(est17)/(1+{irr})^(17-16) + scalar(est18)/(1+{irr})^(18-16) + scalar(est19)/(1+{irr})^(19-16) + scalar(est20)/(1+{irr})^(20-16) + scalar(est21)/(1+{irr})^(21-16) + scalar(est22)/(1+{irr})^(22-16) + scalar(est23)/(1+{irr})^(23-16) + scalar(est24)/(1+{irr})^(24-16) + scalar(est25)/(1+{irr})^(25-16) + scalar(est26)/(1+{irr})^(26-16) + scalar(est27)/(1+{irr})^(27-16) + scalar(est28)/(1+{irr})^(28-16) + scalar(est29)/(1+{irr})^(29-16) + scalar(est30)/(1+{irr})^(30-16) + scalar(est31)/(1+{irr})^(31-16) + scalar(est32)/(1+{irr})^(32-16) + scalar(est33)/(1+{irr})^(33-16) + scalar(est34)/(1+{irr})^(34-16) + scalar(est35)/(1+{irr})^(35-16) + scalar(est36)/(1+{irr})^(36-16) + scalar(est37)/(1+{irr})^(37-16) + scalar(est38)/(1+{irr})^(38-16) + scalar(est39)/(1+{irr})^(39-16) + scalar(est40)/(1+{irr})^(40-16) + scalar(est41)/(1+{irr})^(41-16) + scalar(est42)/(1+{irr})^(42-16) + scalar(est43)/(1+{irr})^(43-16) + scalar(est44)/(1+{irr})^(44-16) + scalar(est45)/(1+{irr})^(45-16) + scalar(est46)/(1+{irr})^(46-16) + scalar(est47)/(1+{irr})^(47-16) + scalar(est48)/(1+{irr})^(48-16) + scalar(est49)/(1+{irr})^(49-16) + scalar(est50)/(1+{irr})^(50-16) + scalar(est51)/(1+{irr})^(51-16) + scalar(est52)/(1+{irr})^(52-16) + scalar(est53)/(1+{irr})^(53-16) + scalar(est54)/(1+{irr})^(54-16) + scalar(est55)/(1+{irr})^(55-16) + scalar(est56)/(1+{irr})^(56-16) + scalar(est57)/(1+{irr})^(57-16) + scalar(est58)/(1+{irr})^(58-16) + scalar(est59)/(1+{irr})^(59-16) + scalar(est60)/(1+{irr})^(60-16) + scalar(est61)/(1+{irr})^(61-16) + scalar(est62)/(1+{irr})^(62-16)  + 1 - xvar_r ) if _n < 3
 matrix `b' = e(b)
 ereturn post `b'
 cap drop xvar*

end

*Output:
bootstrap _b, reps(250) seed(30031699) cluster(twinpair) nowarn: irr
estat bootstrap, all

**# 4. MR
*Panel: Education premiums over the life-cycle (means)
u mlt_linntW_a* alt_linntW_a* cohort tkom yos piv using "lt_final_estsample_iv.dta", clear
qui foreach t in a1724 a2544 a4552 {
 foreach y in mlt_linntW {
 loc var_`y'_`t' `y'_`t'
 xi: ivreg2 `y'_`t' (yos=piv) i.cohort i.tkom, cluster(tkom cohort) partial(i.tkom i.cohort)
 est store est_`y'_`t'
 scalar est_`y'_`t' = _b[yos]
 su `y'_`t'
 scalar rest_`y'_`t' = scalar(est_`y'_`t')/r(mean)
 local rest_`y'_`t' = round(scalar(rest_`y'_`t'),0.0001) 
 }
}
*Panel: Lifetime education premiums (means & annuity values)
qui foreach t in a1752 {
 foreach y in mlt_linntW alt_linntW {
 loc var_`y'_`t' `y'_`t'
 xi: ivreg2 `y'_`t' (yos=piv) i.cohort i.tkom, cluster(tkom cohort) partial(i.tkom i.cohort)
 est store est_`y'_`t'
 scalar est_`y'_`t' = _b[yos]
 su `y'_`t'
 scalar rest_`y'_`t' = scalar(est_`y'_`t')/r(mean)
 local rest_`y'_`t' = round(scalar(rest_`y'_`t'),0.0001) 
 }
}
* Output estimation results, panels:
forv  t=1(1)1 { 
 noi di as text "{hline 90}"
 noi di "Outcomes: `mlt_linntW_a1724' `mlt_linntW_a2544' `mlt_linntW_a4552' `mlt_linntW_a1752' `alt_linntW_a1752'"
 noi di as text "{hline 90}"
 noi estout est_mlt_linntW_a1724 est_mlt_linntW_a2544 est_mlt_linntW_a4552 est_mlt_linntW_a1752 est_alt_linntW_a1752, cells(b(star fmt(%9.1f)) se(par fmt(%9.1f))) starlevel(* 0.10 ** 0.05 *** 0.01) keep(yos) stats(r2_a F N, fmt(%9.3f %9.3f %9.0f))
 noi di "Scaled wrt DM: --- `rest_mlt_linntW_a1724' --------- `rest_mlt_linntW_a2544' ---------- `rest_mlt_linntW_a4552' ---------- `rest_mlt_linntW_a1752' ---------- `rest_alt_linntW_a1752'"
 noi di as text "{hline 90}"
}
est drop _all 
* fixest1=-23093
* fixest2=84730
*Panel: Internal rate of return (bootstrap SEs with 250 reps)
use "lt_final_estsample_ivG.dta", clear
sysdir set PLUS "C:\Users\p805-tarjeiw\Documents\Stata\PlusPersonal\xtivreg2\plus"
cap program drop irr_fix
program irr_fix, eclass
cap syntax [if] [, fixest1(real -23444) fixest2(real 72083)]
version 12
marksample touse
tempname b
 
 *use average estimates as fixed parameters in bootstrap
 qui forv a = 17(1)24 {
  g est = `fixest1' 
  su est
  scalar est`a' = r(mean)
  cap drop est
 }

 *bootstrap sampling over ages 25-44
 qui forv a = 25(1)44 {
  foreach y in linntW {
  xtset tkom
  xi: xtivreg2 `y'_a`a' (yos=piv) i.cohort [aw=N_a`a'], fe
  scalar est`a' = _b[yos]
  }
  }

 *use average estimates as fixed parameters in bootstrap
 qui forv a = 45(1)52 {
  g est = `fixest2' 
  su est
  scalar est`a' = r(mean)
  cap drop est
  }

 g       xvar_l = 1 if _n == 1
 replace xvar_l = 0 if _n != 1
 g       xvar_r = 0 if _n == 1
 replace xvar_r = 1 if _n != 1

 qui nl ( xvar_l =   scalar(est17)/(1+{irr})^(17-16) + scalar(est18)/(1+{irr})^(18-16) + scalar(est19)/(1+{irr})^(19-16) + scalar(est20)/(1+{irr})^(20-16) + scalar(est21)/(1+{irr})^(21-16) + scalar(est22)/(1+{irr})^(22-16) + scalar(est23)/(1+{irr})^(23-16) + scalar(est24)/(1+{irr})^(24-16) + scalar(est25)/(1+{irr})^(25-16) + scalar(est26)/(1+{irr})^(26-16) + scalar(est27)/(1+{irr})^(27-16) + scalar(est28)/(1+{irr})^(28-16) + scalar(est29)/(1+{irr})^(29-16) + scalar(est30)/(1+{irr})^(30-16) + scalar(est31)/(1+{irr})^(31-16) + scalar(est32)/(1+{irr})^(32-16) + scalar(est33)/(1+{irr})^(33-16) + scalar(est34)/(1+{irr})^(34-16) + scalar(est35)/(1+{irr})^(35-16) + scalar(est36)/(1+{irr})^(36-16) + scalar(est37)/(1+{irr})^(37-16) + scalar(est38)/(1+{irr})^(38-16) + scalar(est39)/(1+{irr})^(39-16) + scalar(est40)/(1+{irr})^(40-16) + scalar(est41)/(1+{irr})^(41-16) + scalar(est42)/(1+{irr})^(42-16) + scalar(est43)/(1+{irr})^(43-16) + scalar(est44)/(1+{irr})^(44-16) + scalar(est45)/(1+{irr})^(45-16) + scalar(est46)/(1+{irr})^(46-16) + scalar(est47)/(1+{irr})^(47-16) + scalar(est48)/(1+{irr})^(48-16) + scalar(est49)/(1+{irr})^(49-16) + scalar(est50)/(1+{irr})^(50-16) + scalar(est51)/(1+{irr})^(51-16) + scalar(est52)/(1+{irr})^(52-16)  + 1 - xvar_r ) if _n < 3
 matrix `b' = e(b)
 ereturn post `b'
 cap drop xvar*

end

*compute average premiums 20-24 and 45-55: input for bootstrap
qui foreach t in a1724 a4552 {
 foreach y in mlt_linntW {
 xtset tkom
 xi: xtivreg2 `y'_`t' (yos=piv) i.cohort [aw=N_`t'], fe
 loc fixest_`t' = _b[yos]
 }
}

*Output:
bootstrap _b, reps(250) seed(30031699) cluster(tkom cohort) nowarn: irr_fix, fixest1(`fixest_a1724') fixest2(`fixest_a4555') 
estat bootstrap, all

********************************************************************************
**# B2. FIGURES
********************************************************************************	
**# 1. OLS
********************************************************************************
*** FIGURE 3: ESTIMATES OF AGE-SPECIFIC EDUCATION PREMIUMS - OLS sample
********************************************************************************
 *Load baseline sample
 u cohort tkom linntW_a* yos using "lt_final_estsample_ols.dta", clear
 *age list
 g age = .
 qui forv a = 17(1)62 {
  replace age = `a' if _n == `a'-16
 }  
 *age-specific estimations
 g est_age = .
 g se_age = .
 *xtset tkom
 qui forv a = 17(2)61 {
   xi: ivreg2 linntW_a`a' yos i.cohort i.tkom, cluster(tkom cohort) partial(i.tkom i.cohort)
   replace est_age = _b[yos] if age == `a'
   replace se_age = _se[yos] if age == `a'
  }
 qui forv a = 62(1)62 {
   xi: ivreg2 linntW_a`a' yos i.cohort i.tkom, cluster(tkom cohort) partial(i.tkom i.cohort)
   replace est_age = _b[yos] if age == `a'
   replace se_age = _se[yos] if age == `a'
  }
 replace est_age = est_age/1000
 replace se_age = se_age/1000
 g lest_age = est_age - 1.96*se_age
 g hest_age = est_age + 1.96*se_age
 sum lest* hest* 
 *keep
 bys age: keep if _n == 1
 g zeroline = 0
 *Fig
 qui twoway (rarea lest_age hest_age age, color(gs14)) ///
			(line zeroline age, lcolor(black) lpat("--"))                                           ///   
			(line est_age age, lcolor(black)),                                                      ///
			ylabel(-40(20)100, angle(0) ticks grid) xlabel(17(5)62) xtitle(Age, size(medsmall))     /// 
			ytitle("OLS estimates of the effect of an extra year of"                                ///
			"schooling on earnings (in 1000 NOK) at a given age", size(medsmall))                   /// 
			scheme(s1mono) legend(on order(3) label(3 "OLS estimate: {&beta}{sub:a}")               ///
			bmargin(zero) cols(1) pos(5) ring(0))  ///
			title("{bf:A.} OLS", place(nw) size(medsmall))  
			graph save ols_age_fig.gph, replace

**# 2. Siblings
********************************************************************************
*** FIGURE 3: ESTIMATES OF AGE-SPECIFIC EDUCATION PREMIUMS - Siblings sample
********************************************************************************
*Load baseline sample
 u famid linntW_a* yos att_sib_a* using "lt_final_estsample_sib.dta", clear
 *age list
 g age = .
 qui forv a = 17(1)62 {
  replace age = `a' if _n == `a'-16
 }  
 *age-specific estimations
 g est_age = .
 g se_age = .
 xtset famid
 qui forv a = 17(2)61 {
   xtivreg2 linntW_a`a' yos if att_sib_a`a'==0, fe
   replace est_age = _b[yos] if age == `a'
   replace se_age = _se[yos] if age == `a'
  }
 qui forv a = 62(1)62 {
   xtivreg2 linntW_a`a' yos if att_sib_a`a'==0, fe
   replace est_age = _b[yos] if age == `a'
   replace se_age = _se[yos] if age == `a'
  }
 replace est_age = est_age/1000
 replace se_age = se_age/1000
 g lest_age = est_age - 1.96*se_age
 g hest_age = est_age + 1.96*se_age
 sum lest* hest* 
 *keep
 bys age: keep if _n == 1
 g zeroline = 0
 *Fig
 qui twoway (rarea lest_age hest_age age, color(gs14)) ///
			(line zeroline age, lcolor(black) lpat("--"))                                          ///   
			(line est_age age, lcolor(black)),                                                     ///
			ylabel(-40(20)100, angle(0) ticks grid) xlabel(17(5)62) xtitle(Age, size(medsmall))    /// 
			ytitle("Sibling estimates of the effect of an extra year of"                           ///
			"schooling on earnings (in 1000 NOK) at a given age", size(medsmall))                  /// 
			scheme(s1mono) legend(on order(3) label(3 "Sibling FE estimate: {&beta}{sub:a}")       ///
			bmargin(zero) cols(1) pos(5) ring(0)) ///
			title("{bf:B.} Sibling fixed effects", place(nw) size(medsmall))  
			graph save sib_age_fig.gph, replace
			
**# 3. Twins
********************************************************************************
*** FIGURE 3: ESTIMATES OF AGE-SPECIFIC EDUCATION PREMIUMS - Twins sample
********************************************************************************
*Load baseline sample
 u twinpair linntW_a* yos att_tw_a* cohort using "lt_final_estsample_twin.dta", clear
 *age list
 g age = .
 qui forv a = 17(1)62 {
  replace age = `a' if _n == `a'-16
 }  
 *age-specific estimations
 g est_age = .
 g se_age = .
 xtset twinpair
 qui forv a = 17(2)61 {
   xtivreg2 linntW_a`a' yos if att_tw_a`a'==0, fe 
   replace est_age = _b[yos] if age == `a'
   replace se_age = _se[yos] if age == `a'
  }
 qui forv a = 62(1)62 {
   xtivreg2 linntW_a`a' yos if att_tw_a`a'==0, fe 
   replace est_age = _b[yos] if age == `a'
   replace se_age = _se[yos] if age == `a'
  }
 replace est_age = est_age/1000
 replace se_age = se_age/1000
 g lest_age = est_age - 1.96*se_age
 g hest_age = est_age + 1.96*se_age
 sum lest* hest* 
 *keep
 bys age: keep if _n == 1
 g zeroline = 0
 *Fig
 qui twoway (rarea lest_age hest_age age, color(gs14)) ///
			(line zeroline age, lcolor(black) lpat("--"))                                           ///   
			(line est_age age, lcolor(black)),                                                      ///
			ylabel(-40(20)100, angle(0) ticks grid) xlabel(17(5)62) xtitle(Age, size(medsmall))     /// 
			ytitle("Twin estimates of the effect of an extra year of"                               ///
			"schooling on earnings (in 1000 NOK) at a given age", size(medsmall))                   /// 
			scheme(s1mono) legend(on order(3) label(3 "Twin model estimate: {&beta}{sub:a}")        ///
			bmargin(zero) cols(1) pos(5) ring(0)) ///
			title("{bf:C.} Twin model", place(nw) size(medsmall))  
			graph save twin_age_fig.gph, replace
	
**# 4. MR
********************************************************************************
*** FIGURE 3: ESTIMATES OF AGE-SPECIFIC EDUCATION PREMIUMS - IV sample
********************************************************************************
 *Load baseline sample
 u cohort tkom linntW_a* piv yos using "lt_final_estsample_iv.dta", clear
 *age list
 g age = .
 qui forv a = 17(1)52 {
  replace age = `a' if _n == `a'-16
 }  
 *age-specific estimations
 g est_age = .
 g se_age = .
 qui forv a = 17(1)52 {
   xi: ivreg2 linntW_a`a' (yos=piv) i.cohort i.tkom, cluster(tkom cohort) partial(i.tkom i.cohort)
   replace est_age = _b[yos] if age == `a'
   replace se_age = _se[yos] if age == `a'
  }

 replace est_age = est_age/1000
 replace se_age = se_age/1000
 g lest_age = est_age - 1.96*se_age
 g hest_age = est_age + 1.96*se_age
 sum lest* hest* 
*window grid (to make sure thay only area along y-axis (-40(20)100) is drawn)
qui foreach g in age {
forv a = 17(2)52 {
  replace lest_`g' = max(min(lest_`g',100),-40) if age == `a'
  replace hest_`g' = max(min(hest_`g',100),-40) if age == `a'
 }
 }
 *keep
 bys age: keep if _n == 1
 g zeroline = 0
 *Fig
 qui twoway (rarea lest_age hest_age age, color(gs14)) ///
	    (line zeroline age, lcolor(black) lpat("--")) ///
		(line est_age age, lcolor(black)),                                                      ///
		ylabel(-40(20)100, angle(0) ticks grid) xlabel(17(5)62) xtitle(Age, size(medsmall))     /// 
		ytitle("MR estimates of the effect of an extra year of"                                 ///
	    "schooling on earnings (in 1000 NOK) at a given age", size(medsmall))                   /// 
	    scheme(s1mono) legend(on order(3) label(3 "MR estimate: {&beta}{sub:a}")                ///
	    bmargin(zero) cols(1) pos(5) ring(0))  ///
		title("{bf:D.} Mendelian randomization", place(nw) size(medsmall))  
	    graph save mr_age_fig.gph, replace	
		
*******************************
* Figure 3 
*******************************
graph combine ols_age_fig.gph sib_age_fig.gph twin_age_fig.gph mr_age_fig.gph, rows(2) 
graph save "Graph" "N:\durable\projects\EQOP\MR_Income\figures_and_tables\Figure_3.gph", replace		


********************************************************************************
**# C. SENSITIVITY
********************************************************************************
**# 1. OLS
********************************************************************************
*** FIGURE 3: ESTIMATES OF AGE-SPECIFIC EDUCATION PREMIUMS - OLS sample
********************************************************************************
 *Load baseline sample
 u cohort tkom linntW_a* yos using "lt_final_estsample_ols.dta", clear
 keep if inrange(cohort,1959,1967)
 *age list
 g age = .
 qui forv a = 17(1)55 {
  replace age = `a' if _n == `a'-16
 }  
 *age-specific estimations
 g est_age = .
 g se_age = .
 xtset tkom
 qui forv a = 17(2)55 {
   *xtivreg2 linntW_a`a' yos coh_D*, fe cluster(tkom)
   xi: ivreg2 linntW_a`a' yos i.cohort i.tkom, cluster(tkom cohort) partial(i.tkom i.cohort)
   replace est_age = _b[yos] if age == `a'
   replace se_age = _se[yos] if age == `a'
  }
 replace est_age = est_age/1000
 replace se_age = se_age/1000
 g lest_age = est_age - 1.96*se_age
 g hest_age = est_age + 1.96*se_age
 sum lest* hest* 
 *keep
 bys age: keep if _n == 1
 g zeroline = 0
 *Fig
 qui twoway (rarea lest_age hest_age age, color(gs14)) ///
            (line zeroline age, lcolor(black) lpat("--"))                                        ///   
			(line est_age age, lcolor(black)),                                                   ///
			ylabel(-40(20)100, angle(0) ticks grid) xlabel(17(2)55) xtitle(Age, size(medsmall))  /// 
			ytitle("OLS estimates of the effect of an extra year of"                             ///
			"schooling on earnings (in 1000 NOK) at a given age", size(medsmall))                /// 
			scheme(s1mono) legend(on order(3) label(3 "OLS estimate: {&beta}{sub:a}")            ///
			bmargin(zero) cols(1) pos(5) ring(0))  ///
			title("{bf:A.} OLS", place(nw) size(medsmall))  
			graph save ols_age_fig_sensitivity.gph, replace

**# 2. Siblings
********************************************************************************
*** FIGURE 3: ESTIMATES OF AGE-SPECIFIC EDUCATION PREMIUMS - Siblings sample
********************************************************************************
*Load baseline sample
 u famid cohort linntW_a* yos att_sib_a* using "lt_final_estsample_sib.dta", clear
keep if inrange(cohort,1959,1967)
*age list
 g age = .
 qui forv a = 17(1)55 {
  replace age = `a' if _n == `a'-16
 }  
 *age-specific estimations
 g est_age = .
 g se_age = .
 xtset famid
 qui forv a = 17(2)55 {
   xtivreg2 linntW_a`a' yos if att_sib_a`a'==0, fe
   replace est_age = _b[yos] if age == `a'
   replace se_age = _se[yos] if age == `a'
  }
 replace est_age = est_age/1000
 replace se_age = se_age/1000
 g lest_age = est_age - 1.96*se_age
 g hest_age = est_age + 1.96*se_age
 sum lest* hest* 
 *keep
 bys age: keep if _n == 1
 g zeroline = 0
 *Fig
 qui twoway (rarea lest_age hest_age age, color(gs14)) ///
			(line zeroline age, lcolor(black) lpat("--"))                                        ///   
			(line est_age age, lcolor(black)),                                                   ///
			ylabel(-40(20)100, angle(0) ticks grid) xlabel(17(2)55) xtitle(Age, size(medsmall))  /// 
			ytitle("Sibling estimates of the effect of an extra year of"                         ///
			"schooling on earnings (in 1000 NOK) at a given age", size(medsmall))                /// 
			scheme(s1mono) legend(on order(3) label(3 "Sibling FE estimate: {&beta}{sub:a}")     ///
			bmargin(zero) cols(1) pos(5) ring(0)) ///
			title("{bf:B.} Sibling fixed effects", place(nw) size(medsmall)) 
			graph save sib_age_fig_sensitivity.gph, replace
			
**# 3. Twins
******************************************************************************** 
*** FIGURE 3: ESTIMATES OF AGE-SPECIFIC EDUCATION PREMIUMS - Twins sample
********************************************************************************
*Load baseline sample
 u twinpair linntW_a* yos att_tw_a* cohort using "lt_final_estsample_twin.dta", clear
 keep if inrange(cohort,1959,1967)
 *age list
 g age = .
 qui forv a = 17(1)55 {
  replace age = `a' if _n == `a'-16
 }  
 *age-specific estimations
 g est_age = .
 g se_age = .
 xtset twinpair
 qui forv a = 17(2)55 {
   xtivreg2 linntW_a`a' yos if att_tw_a`a'==0, fe
   replace est_age = _b[yos] if age == `a'
   replace se_age = _se[yos] if age == `a'
  }
 replace est_age = est_age/1000
 replace se_age = se_age/1000
 g lest_age = est_age - 1.96*se_age
 g hest_age = est_age + 1.96*se_age
 sum lest* hest* 
 *keep
 bys age: keep if _n == 1
 g zeroline = 0
 *Fig
 qui twoway (rarea lest_age hest_age age, color(gs14)) ///
			(line zeroline age, lcolor(black) lpat("--"))                                        ///   
			(line est_age age, lcolor(black)),                                                   ///
			ylabel(-40(20)100, angle(0) ticks grid) xlabel(17(2)55) xtitle(Age, size(medsmall))  /// 
			ytitle("Twin FE estimates of the effect of an extra year of"                         ///
			"schooling on earnings (in 1000 NOK) at a given age", size(medsmall))                /// 
			scheme(s1mono) legend(on order(3) label(3 "Twin FE estimate: {&beta}{sub:a}")        ///
			bmargin(zero) cols(1) pos(5) ring(0)) ///
			title("{bf:C.} Twin fixed effects", place(nw) size(medsmall))   
			graph save twin_age_fig_sensitivity.gph, replace
**# 4. MR
******************************************************************************** 
*** FIGURE 3: ESTIMATES OF AGE-SPECIFIC EDUCATION PREMIUMS - IV sample
********************************************************************************
 *Load baseline sample
 u cohort tkom linntW_a* piv yos using "lt_final_estsample_iv.dta", clear
 // complete panel up to 1970
 keep if inrange(cohort,1959,1970)
 *age list
 g age = .
 qui forv a = 17(1)52 {
  replace age = `a' if _n == `a'-16
 }  
 *age-specific estimations
 g est_age = .
 g se_age = .
 qui forv a = 17(1)52 {
   xi: ivreg2 linntW_a`a' (yos=piv) i.cohort i.tkom, cluster(tkom cohort) partial(i.tkom i.cohort)
   replace est_age = _b[yos] if age == `a'
   replace se_age = _se[yos] if age == `a'
  }
 replace est_age = est_age/1000
 replace se_age = se_age/1000
 g lest_age = est_age - 1.96*se_age
 g hest_age = est_age + 1.96*se_age
 sum lest* hest* 
 *window grid (to make sure thay only area along y-axis (-40(20)100) is drawn)
 qui foreach g in age {
  forv a = 17(2)52 {
   replace lest_`g' = max(min(lest_`g',100),-40) if age == `a'
   replace hest_`g' = max(min(hest_`g',100),-40) if age == `a'
  }
  }
 *keep
 bys age: keep if _n == 1
 g zeroline = 0
 *Fig
 qui twoway (rarea lest_age hest_age age, color(gs14)) ///
			(line zeroline age, lcolor(black) lpat("--")) ///
			(line est_age age, lcolor(black)),                                                   ///
			ylabel(-40(20)100, angle(0) ticks grid) xlabel(17(5)62) xtitle(Age, size(medsmall))  /// 
			ytitle("MR estimates of the effect of an extra year of"                              ///
			"schooling on earnings (in 1000 NOK) at a given age", size(medsmall))                /// 
			scheme(s1mono) legend(on order(3) label(3 "MR estimate: {&beta}{sub:a}")             ///
			bmargin(zero) cols(1) pos(5) ring(0))  ///
			title("{bf:D.} Mendelian randomization", place(nw) size(medsmall))  
			graph save mr_age_fig_sensitivity.gph, replace	

*******************************
* Figure 3 Sensitivity
*******************************
	graph combine ols_age_fig_sensitivity.gph sib_age_fig_sensitivity.gph ///
	twin_age_fig_sensitivity.gph mr_age_fig_sensitivity.gph, rows(2) ycommon
	graph save "Graph" "N:\durable\projects\EQOP\MR_Income\figures_and_tables\Figure_3_sensitivity.gph", replace
	
********************************************************************************
**# 5. IRR
********************************************************************************	
**# 1. OLS
u "lt_final_estsample_olsG.dta", clear // Use weighted group-level data to reduce computation time
keep if inrange(cohort,1959,1967)
**Panel: Internal rate of return (bootstrap SEs with 250 reps)
cap program drop irr
program irr, eclass
version 12
tempname b
 qui forv a = 17(1)55 {
  foreach y in linntW {
  xtset tkom
  xi: xtivreg2 `y'_a`a' yos i.cohort [aw=N_a`a'], fe
  scalar est`a' = _b[yos]
  }
 }
 g       xvar_l = 1 if _n == 1
 replace xvar_l = 0 if _n != 1
 g       xvar_r = 0 if _n == 1
 replace xvar_r = 1 if _n != 1

qui nl ( xvar_l =   scalar(est17)/(1+{irr})^(17-16) + scalar(est18)/(1+{irr})^(18-16) + scalar(est19)/(1+{irr})^(19-16) +scalar(est20)/(1+{irr})^(20-16) + scalar(est21)/(1+{irr})^(21-16) + scalar(est22)/(1+{irr})^(22-16) + scalar(est23)/(1+{irr})^(23-16) + scalar(est24)/(1+{irr})^(24-16) + scalar(est25)/(1+{irr})^(25-16) + scalar(est26)/(1+{irr})^(26-16) + scalar(est27)/(1+{irr})^(27-16) + scalar(est28)/(1+{irr})^(28-16) + scalar(est29)/(1+{irr})^(29-16) + scalar(est30)/(1+{irr})^(30-16) + scalar(est31)/(1+{irr})^(31-16) + scalar(est32)/(1+{irr})^(32-16) + scalar(est33)/(1+{irr})^(33-16) + scalar(est34)/(1+{irr})^(34-16) + scalar(est35)/(1+{irr})^(35-16) + scalar(est36)/(1+{irr})^(36-16) + scalar(est37)/(1+{irr})^(37-16) + scalar(est38)/(1+{irr})^(38-16) + scalar(est39)/(1+{irr})^(39-16) + scalar(est40)/(1+{irr})^(40-16) + scalar(est41)/(1+{irr})^(41-16) + scalar(est42)/(1+{irr})^(42-16) + scalar(est43)/(1+{irr})^(43-16) + scalar(est44)/(1+{irr})^(44-16) + scalar(est45)/(1+{irr})^(45-16) + scalar(est46)/(1+{irr})^(46-16) + scalar(est47)/(1+{irr})^(47-16) + scalar(est48)/(1+{irr})^(48-16) + scalar(est49)/(1+{irr})^(49-16) + scalar(est50)/(1+{irr})^(50-16) + scalar(est51)/(1+{irr})^(51-16) + scalar(est52)/(1+{irr})^(52-16) + scalar(est53)/(1+{irr})^(53-16) + scalar(est54)/(1+{irr})^(54-16) + scalar(est55)/(1+{irr})^(55-16) + 1 - xvar_r ) if _n < 3
 matrix `b' = e(b)
 ereturn post `b'
 cap drop xvar*
end
*Output:
bootstrap _b, reps(250) seed(30031699) cluster(tkom cohort) nowarn: irr
estat bootstrap, all	

**# 2. Siblings
u att_sib* mlt_linntW* alt_linntW* linntW* cohort tkom yos famid using "lt_final_estsample_sib.dta", clear
keep if inrange(cohort,1959,1967)
*Panel: Internal rate of return (bootstrap SEs with 250 reps)
cap program drop irr
program irr, eclass
version 12
tempname b
 qui forv a = 17(1)55 {
  foreach y in linntW {
  xtset famid
  xtreg `y'_a`a' yos if att_sib_a`a' == 0, fe
  scalar est`a' = _b[yos]
  }
 }

 g       xvar_l = 1 if _n == 1
 replace xvar_l = 0 if _n != 1
 g       xvar_r = 0 if _n == 1
 replace xvar_r = 1 if _n != 1

 qui nl ( xvar_l =   scalar(est17)/(1+{irr})^(17-16) + scalar(est18)/(1+{irr})^(18-16) + scalar(est19)/(1+{irr})^(19-16) + scalar(est20)/(1+{irr})^(20-16) + scalar(est21)/(1+{irr})^(21-16) + scalar(est22)/(1+{irr})^(22-16) + scalar(est23)/(1+{irr})^(23-16) + scalar(est24)/(1+{irr})^(24-16) + scalar(est25)/(1+{irr})^(25-16) + scalar(est26)/(1+{irr})^(26-16) + scalar(est27)/(1+{irr})^(27-16) + scalar(est28)/(1+{irr})^(28-16) + scalar(est29)/(1+{irr})^(29-16) + scalar(est30)/(1+{irr})^(30-16) + scalar(est31)/(1+{irr})^(31-16) + scalar(est32)/(1+{irr})^(32-16) + scalar(est33)/(1+{irr})^(33-16) + scalar(est34)/(1+{irr})^(34-16) + scalar(est35)/(1+{irr})^(35-16) + scalar(est36)/(1+{irr})^(36-16) + scalar(est37)/(1+{irr})^(37-16) + scalar(est38)/(1+{irr})^(38-16) + scalar(est39)/(1+{irr})^(39-16) + scalar(est40)/(1+{irr})^(40-16) + scalar(est41)/(1+{irr})^(41-16) + scalar(est42)/(1+{irr})^(42-16) + scalar(est43)/(1+{irr})^(43-16) + scalar(est44)/(1+{irr})^(44-16) + scalar(est45)/(1+{irr})^(45-16) + scalar(est46)/(1+{irr})^(46-16) + scalar(est47)/(1+{irr})^(47-16) + scalar(est48)/(1+{irr})^(48-16) + scalar(est49)/(1+{irr})^(49-16) + scalar(est50)/(1+{irr})^(50-16) + scalar(est51)/(1+{irr})^(51-16) + scalar(est52)/(1+{irr})^(52-16) + scalar(est53)/(1+{irr})^(53-16) + scalar(est54)/(1+{irr})^(54-16) + scalar(est55)/(1+{irr})^(55-16) + 1 - xvar_r ) if _n < 3
 matrix `b' = e(b)
 ereturn post `b'
 cap drop xvar*
end
*Output:
bootstrap _b, reps(250) seed(30031699) cluster(famid) nowarn: irr
estat bootstrap, all

**# 3. Twins 
u att_tw* mlt_linntW* alt_linntW* linntW* cohort tkom yos twinpair using "lt_final_estsample_twin.dta", clear
keep if inrange(cohort,1959,1967)
*Panel: Internal rate of return (bootstrap SEs with 250 reps)
cap program drop irr
program irr, eclass
version 12
tempname b
 qui forv a = 17(1)55 {
  foreach y in linntW {
  xtset twinpair
  xtreg `y'_a`a' yos if att_tw_a`a' == 0, fe
  scalar est`a' = _b[yos]
  }
 }

 g       xvar_l = 1 if _n == 1
 replace xvar_l = 0 if _n != 1
 g       xvar_r = 0 if _n == 1
 replace xvar_r = 1 if _n != 1

 qui nl ( xvar_l =   scalar(est17)/(1+{irr})^(17-16) + scalar(est18)/(1+{irr})^(18-16) + scalar(est19)/(1+{irr})^(19-16) + scalar(est20)/(1+{irr})^(20-16) + scalar(est21)/(1+{irr})^(21-16) + scalar(est22)/(1+{irr})^(22-16) + scalar(est23)/(1+{irr})^(23-16) + scalar(est24)/(1+{irr})^(24-16) + scalar(est25)/(1+{irr})^(25-16) + scalar(est26)/(1+{irr})^(26-16) + scalar(est27)/(1+{irr})^(27-16) + scalar(est28)/(1+{irr})^(28-16) + scalar(est29)/(1+{irr})^(29-16) + scalar(est30)/(1+{irr})^(30-16) + scalar(est31)/(1+{irr})^(31-16) + scalar(est32)/(1+{irr})^(32-16) + scalar(est33)/(1+{irr})^(33-16) + scalar(est34)/(1+{irr})^(34-16) + scalar(est35)/(1+{irr})^(35-16) + scalar(est36)/(1+{irr})^(36-16) + scalar(est37)/(1+{irr})^(37-16) + scalar(est38)/(1+{irr})^(38-16) + scalar(est39)/(1+{irr})^(39-16) + scalar(est40)/(1+{irr})^(40-16) + scalar(est41)/(1+{irr})^(41-16) + scalar(est42)/(1+{irr})^(42-16) + scalar(est43)/(1+{irr})^(43-16) + scalar(est44)/(1+{irr})^(44-16) + scalar(est45)/(1+{irr})^(45-16) + scalar(est46)/(1+{irr})^(46-16) + scalar(est47)/(1+{irr})^(47-16) + scalar(est48)/(1+{irr})^(48-16) + scalar(est49)/(1+{irr})^(49-16) + scalar(est50)/(1+{irr})^(50-16) + scalar(est51)/(1+{irr})^(51-16) + scalar(est52)/(1+{irr})^(52-16) + scalar(est53)/(1+{irr})^(53-16) + scalar(est54)/(1+{irr})^(54-16) + scalar(est55)/(1+{irr})^(55-16) + 1 - xvar_r ) if _n < 3
 matrix `b' = e(b)
 ereturn post `b'
 cap drop xvar*
end
*Output:
bootstrap _b, reps(250) seed(30031699) cluster(twinpair) nowarn: irr
estat bootstrap, all	
**# 4. MR
use "lt_final_estsample_ivG.dta", clear
keep if inrange(cohort,1959,1970)
*Panel: Internal rate of return (bootstrap SEs with 250 reps)
cap program drop irr_fix
program irr_fix, eclass
cap syntax [if] [, fixest1(real -23444) fixest2(real 72083)]
version 12
marksample touse
tempname b
 *use average estimates as fixed parameters in bootstrap
 qui forv a = 17(1)24 {
  g est = `fixest1' 
  su est
  scalar est`a' = r(mean)
  cap drop est
 }
 *bootstrap sampling over ages 25-44
 qui forv a = 25(1)44 {
  foreach y in linntW {
  xtset tkom
  xi: xtivreg2 `y'_a`a' (yos=piv) i.cohort [aw=N_a`a'], fe
  scalar est`a' = _b[yos]
  }
  }
 *use average estimates as fixed parameters in bootstrap
 qui forv a = 45(1)52 {
  g est = `fixest2' 
  su est
  scalar est`a' = r(mean)
  cap drop est
  }
 g       xvar_l = 1 if _n == 1
 replace xvar_l = 0 if _n != 1
 g       xvar_r = 0 if _n == 1
 replace xvar_r = 1 if _n != 1

 qui nl ( xvar_l =   scalar(est17)/(1+{irr})^(17-16) + scalar(est18)/(1+{irr})^(18-16) + scalar(est19)/(1+{irr})^(19-16) + scalar(est20)/(1+{irr})^(20-16) + scalar(est21)/(1+{irr})^(21-16) + scalar(est22)/(1+{irr})^(22-16) + scalar(est23)/(1+{irr})^(23-16) + scalar(est24)/(1+{irr})^(24-16) + scalar(est25)/(1+{irr})^(25-16) + scalar(est26)/(1+{irr})^(26-16) + scalar(est27)/(1+{irr})^(27-16) + scalar(est28)/(1+{irr})^(28-16) + scalar(est29)/(1+{irr})^(29-16) + scalar(est30)/(1+{irr})^(30-16) + scalar(est31)/(1+{irr})^(31-16) + scalar(est32)/(1+{irr})^(32-16) + scalar(est33)/(1+{irr})^(33-16) + scalar(est34)/(1+{irr})^(34-16) + scalar(est35)/(1+{irr})^(35-16) + scalar(est36)/(1+{irr})^(36-16) + scalar(est37)/(1+{irr})^(37-16) + scalar(est38)/(1+{irr})^(38-16) + scalar(est39)/(1+{irr})^(39-16) + scalar(est40)/(1+{irr})^(40-16) + scalar(est41)/(1+{irr})^(41-16) + scalar(est42)/(1+{irr})^(42-16) + scalar(est43)/(1+{irr})^(43-16) + scalar(est44)/(1+{irr})^(44-16) + scalar(est45)/(1+{irr})^(45-16) + scalar(est46)/(1+{irr})^(46-16) + scalar(est47)/(1+{irr})^(47-16) + scalar(est48)/(1+{irr})^(48-16) + scalar(est49)/(1+{irr})^(49-16) + scalar(est50)/(1+{irr})^(50-16) + scalar(est51)/(1+{irr})^(51-16) + scalar(est52)/(1+{irr})^(52-16)  + 1 - xvar_r ) if _n < 3
 matrix `b' = e(b)
 ereturn post `b'
 cap drop xvar*

end
*compute average premiums 20-24 and 45-55: input for bootstrap
qui foreach t in a1724 a4552 {
 foreach y in mlt_linntW {
 xtset tkom
 xi: xtivreg2 `y'_`t' (yos=piv) i.cohort [aw=N_`t'], fe
 loc fixest_`t' = _b[yos]
 }
}
*Output:
bootstrap _b, reps(250) seed(30031699) cluster(tkom cohort) nowarn: irr_fix, fixest1(`fixest_a1724') fixest2(`fixest_a4555') 
estat bootstrap, all

	