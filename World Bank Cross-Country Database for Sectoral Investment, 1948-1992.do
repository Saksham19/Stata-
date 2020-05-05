global root "C:\Users\Saksham\Documents\ntu study stuff\trimester 3\Data Analytics - Stata\Stata"

cd "$root"

local today= c(current_date)
capture log using "$root/WB_Sectoral_Investment_`today'.smcl",replace




******using dataset http://fmwww.bc.edu/ec-p/data/macro/cap797wa


use http://fmwww.bc.edu/ec-p/data/macro/cap797wa,clear
des

/*

Contains data from http://fmwww.bc.edu/ec-p/data/macro/cap797wa.dta
  obs:         2,835                          World Bank Cross-Country Database for Sectoral Investment, 1948-1992
 vars:            16                          28 May 2001 13:43
 size:       232,470                          
------------------------------------------------------------------------------------------------------------------------------
              storage   display    value
variable name   type    format     label      variable label
------------------------------------------------------------------------------------------------------------------------------
ccode           str6    %6s                   country code
year            int     %9.0g                 year
AgCap           float   %9.0g                 Total Agricultural Capital, 1990 US$
AgDef           float   %9.0g                 Agricultural Sector Deflator (1990=100)
AgSECap         float   %9.0g                 Agricultural Fixed Capital, 1990 US$
AgSEInv         float   %9.0g                 Ag Fixed Investment, 1990 Local Currency
Ex_Rate         float   %9.0g                 US$ to Local Currency Exchange Rate
Livestok        float   %9.0g                 Value of Livestock Capital, 1990 US$
MfgSECap        float   %9.0g                 Manufacturing Fixed Capital, 1990 US$
MfgSEInv        float   %9.0g                 Manufacturing Fixed Investment, 1990 Local Currency
TotDef          float   %9.0g                 Economy-Wide Fixed Capital Deflator (1990=100)
TotSECap        float   %9.0g                 Economy-Wide Fixed Capital, 1990 US$
TotSEInv        float   %9.0g                 Economy-Wide Fixed Investment, 1990 Local Currency
TreeCap         float   %9.0g                 Orchard Capital, 1990 US$
country         str22   %22s                  country
iccode          long    %8.0g      iccode     numeric country code
------------------------------------------------------------------------------------------------------------------------------
Sorted by: iccode  year

*/
*cross sectional data

rename iccode id



*data cleaning required 
*First, check for duplicates

bysort id: gen temp=_n
tab temp

bys id: egen temp2= max(temp)
tab temp2
drop temp*
*Can see 0 duplicates. We have data for 45 unique ids

*Next, we drop all unnecessary values

drop if AgCap- id==.
drop if MfgSECap==.
drop if MfgSEInv==.

*Basic tabulations and graphs:

*average amount of fixed capital (1990 -fixed) by country:
table country, c(mean AgCap mean Livestok mean MfgSECap)

corr AgCap Livestok MfgSECap
graph matrix AgCap Livestok MfgSECap
*no apparaent corr
twoway (spike Ex_Rate year)


scatter AgCap Livestok MfgSECap


*can do some regressional analysis:

*How much is total agricultural capital affected by the other agri variables?
reg AgCap AgDef AgSECap AgSEInv 
*The overall Rsqrd is around 94% which is quite high, hence high goodness of fit
* The p value for AgSEInv is quite high and hence, we fail to reject null at even 10% sig level, hence AgSeInv seems like an insig var.

*can do the above reg for logAgCap

gen agcap_log=log(AgCap)
reg agcap_log AgDef AgSECap AgSEInv 
*we can see that AgDef has quite a high p value and a really low t value. Hence insig

*prediction for the y var and residuals 
predict p_lw
predict r, resi

twoway(scatter r p_lw)(lfit r p_lw)
*can see some corr for sure. Error terms not independent of y. Violation of the classical assumptions.

log close
