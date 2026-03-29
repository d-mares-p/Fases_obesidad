**Estimación con panel espacial datos para obesidad**
**Instalar los siguientes paquetes**

ssc install spwmatrix
ssc install sppack
ssc install xsmle



use "/Users/dmares/Downloads/wrn.dta", clear
spmat dta wrn X*, normalize(row)
spmat save wrn using "/Users/dmares/Downloads/wrn.spmat", replace
spmat summarize wrn



import excel "/Users/dmares/Downloads/RegresionFinal.xlsx", ///
    sheet("bdatos") firstrow clear

rename CVE id_ent
destring id_ent, replace ignore(" ")
drop if missing(id_ent)
duplicates drop id_ent, force
isid id_ent

global stubs "Inci ingr gini pobreza CrecimientoVA CrecimientoGastosalud escolaridad IDS densidad"
reshape long $stubs, i(id_ent) j(t)


xtset id_ent t
save "/Users/dmares/Downloads/basedatos_panel.dta", replace
use "/Users/dmares/Downloads/basedatos_panel.dta", clear




foreach v in Inci gini pobreza ingr IDS escolaridad CrecimientoGastosalud CrecimientoVA densidad {
    capture confirm numeric variable `v'
    if _rc destring `v', replace ignore(",") force
}

misstable summarize ingr gini pobreza
summ ingr gini pobreza

xtset id_ent t
save "/Users/dmares/Downloads/basedatos_panel_clean.dta", replace




spmat summarize wrn

  
isid id_ent t
duplicates report id_ent  

*Modelo pool
reg Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad 
reg Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad i.t
reg Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad i.t, vce(cluster id_ent)




                           *Parte I, pruebas*
***para todas las variables
xtreg Inci gini pobreza  IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad, fe 
est store FE_noyear
xtreg Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad i.t , fe 
est store FE_full

xtreg Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad, re
est store RE_noyear
xtreg Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad i.t, re 
est store RE

hausman FE RE
hausman FE_noyear RE_noyear


			 

*prueba de CRE porque no es concluyente Hausman

local vars gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad
foreach x in `vars' {
    capture drop m_`x'  // Esto borra la variable si ya existe para evitar  error
    bysort id_ent: egen m_`x' = mean(`x')
}


xtreg Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad m_gini m_pobreza m_IDS m_ingr m_escolaridad m_CrecimientoGastosalud m_CrecimientoVA m_densidad, re  //Mundlack
test m_gini m_pobreza m_IDS m_ingr m_escolaridad m_CrecimientoGastosalud m_CrecimientoVA m_densidad   //Wald




*pruebas y correcciones de errores robustos
xttest3                      // heterocedasticidad groupwise en FE
xtserial y x1 x2             // Wooldridge AR(1)
xtcsd, pesaran abs           // dependencia transversal
xtscc y x i.t, fe            // * Sensibilidad: Driscoll–Kraay (instalar xtscc si hace falta)
							// resolver heterocedasticidad y AR(1) con cluster


ssc install spwmatrix, replace
ssc install sppack, replace
ssc install xsmle, replace




*prueba de significancia de uso del tiempo
xtreg Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad i.t, fe vce(cluster id_ent)
testparm i.t




* Heterocedasticidad entre paneles (Wald para FE)
xtreg Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad i.t, fe
xttest3
* autocorrelacion de Wooldridge
xtserial Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad
*CDPesaran
ssc install xtcsd
xtreg Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad i.t, fe
xtcsd, pesaran abs
*Driscoll-Kraay
*xtscc Inci gini pobreza IDS escolaridad CrecimientoGastosalud CrecimientoVA densidad  i.t, fe
*est store FE_DK
 

 *presentacion
xtreg Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad i.t, fe vce(cluster id_ent)
eststo model1
esttab model1 using "Resultados_Finales.rtf", b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) replace label

    
 


**errores robustos  ecuacion completa con tiempo
xtset id_ent t
xtreg Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad i.t, ///
    fe vce(cluster id_ent)
est store FE_full


save "/Users/dmares/Downloads/basedatos_panel.dta", replace
use "/Users/dmares/Downloads/basedatos_panel.dta", clear
xtset id_ent t
sort id_ent t





                                *Parte II, regresiones espaciales*



spmat use wrn using "/Users/dmares/Downloads/wrn.spmat"
spmat summarize wrn
								
local C " ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad"
local Xfull "gini pobreza IDS `C'"


* SAR robusto (usa matriz wmat que es matriz de rezago)


xtset id_ent t
xsmle Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad, ///
    wmat(wrn) model(sar) fe type(both) vce(cluster id_ent) ///
    effects nsim(500) nolog
est store SAR_full


xtset id_ent t
xsmle Inci gini pobreza IDS  escolaridad CrecimientoGastosalud CrecimientoVA densidad, ///
    wmat(wrn) model(sar) fe type(both) vce(cluster id_ent) ///
    effects nsim(500) nolog
est store SAR_


xsmle Inci gini pobreza IDS  escolaridad  CrecimientoVA densidad, ///
    wmat(wrn) model(sar) fe type(both) vce(cluster id_ent) ///
    effects nsim(500) nolog
est store SAR_SG



* SEM (usa emat, no es matriz de rezago)


xtset id_ent t
xsmle Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad, ///
    emat(wrn) model(sem) fe type(both) vce(cluster id_ent) nolog
est store SEM_full

xsmle Inci gini pobreza IDS  escolaridad CrecimientoGastosalud CrecimientoVA densidad, ///
    emat(wrn) model(sem) fe type(both) vce(cluster id_ent) nolog
est store SEM_

xsmle Inci gini pobreza IDS  escolaridad  CrecimientoVA densidad, ///
    emat(wrn) model(sem) fe type(both) vce(cluster id_ent) nolog
est store SEM_SG




*SDM	- Durbin	

xsmle Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad, ///
    wmat(wrn) model(sdm) fe type(both) vce(cluster id_ent) ///
    effects nsim(500) nolog
est store SDM_full


xsmle Inci gini pobreza IDS  escolaridad CrecimientoGastosalud CrecimientoVA densidad, ///
    wmat(wrn) model(sdm) fe type(both) vce(cluster id_ent) ///
    effects nsim(500) nolog
est store SDM_

xsmle Inci gini pobreza IDS  escolaridad  CrecimientoVA densidad, ///
    wmat(wrn) model(sdm) fe type(both) vce(cluster id_ent) ///
    effects nsim(500) nolog
est store SDM_SG






xsmle Inci IDS gini pobreza, ///
    wmat(wrn) model(sdm) fe type(both) vce(cluster id_ent) ///
    effects nsim(500) nolog
est store SDM_mini2




*SDEM = "Durbin con errores" (WX + error espacial)
*   (es SEM + Durbin: durbin(varlist) y dmat() para WX)



xsmle Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad, ///
    wmat(wrn) model(sdm) vce(robust) fe type(both) ///
  dmatrix(wrn) 
    durbin(gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad) ///
est store SDEM_full




* SAC = "SARMA / SARAR" (Wy + error espacial)


xsmle Inci gini pobreza IDS ingr escolaridad CrecimientoGastosalud CrecimientoVA densidad, ///
    wmat(wrn) emat(wrn) model(sac) ///
    fe type(both) vce(cluster id_ent) ///
    effects nsim(500) nolog
est store SAC_full




***Durbin con rezagos en errores
xtset id_ent t
cap spmat drop wrn
spmat use wrn using "/Users/dmares/Downloads/wrn.spmat"
* crea archivo de lags solo para W_IDS
clear
save "/Users/dmares/Downloads/_Wx_IDS.dta", emptyok replace
use "/Users/dmares/Downloads/basedatos_panel_clean_v2.dta", clear
levelsof t, local(years)
foreach yr of local years {
    preserve
    keep if t==`yr'
    sort id_ent
    spmat lag double W_IDS wrn IDS
    keep id_ent t W_IDS
    append using "/Users/dmares/Downloads/_Wx_IDS.dta"
    save "/Users/dmares/Downloads/_Wx_IDS.dta", replace
    restore
}
* pegar W_IDS al panel
use "/Users/dmares/Downloads/basedatos_panel_clean_v2.dta", clear
merge 1:1 id_ent t using "/Users/dmares/Downloads/_Wx_IDS.dta", nogen
save "/Users/dmares/Downloads/_panel_con_WIDS.dta", replace
		use "/Users/dmares/Downloads/_panel_con_WIDS.dta", clear
xtset id_ent t
xsmle Inci gini pobreza IDS escolaridad CrecimientoGastosalud CrecimientoVA densidad W_IDS, ///
    emat(wrn) model(sem) fe type(both) vce(cluster id_ent) nolog
est store SDEM_mini
		

		
		
		
**comparar modelos

estimates stats FE_full SAR_full SEM_full SAC_full SDM_full


*efectos

xsmle, effects
margins, dydx(IDS)



****** analisis con rezago temporal en variables independientes
* Generar rezagos para las variables que tardan en hacer efecto
gen gini_lag = L.gini
gen pobreza_lag = L.pobreza
gen escolaridad_lag = L.escolaridad
gen IDS_lag = L.IDS
gen CrecimientoGastosalud_lag = L.CrecimientoGastosalud
gen CrecimientoVA_lag = L.CrecimientoVA
gen densidad_lag = L.densidad
gen ingr_lag = L.ingr


xtset id_ent t
xsmle Inci gini_lag pobreza_lag IDS_lag ingr_lag escolaridad_lag CrecimientoGastosalud_lag CrecimientoVA_lag densidad_lag, ///
    wmat(wrn) model(sar) fe type(both) vce(cluster id_ent) ///
    effects nsim(500) nolog
xsmle Inci gini_lag pobreza_lag IDS_lag ingr_lag escolaridad_lag CrecimientoGastosalud_lag CrecimientoVA_lag densidad_lag, ///
    emat(wrn) model(sem) fe type(both) vce(cluster id_ent) nolog
xsmle Inci gini_lag pobreza_lag IDS_lag ingr_lag escolaridad_lag CrecimientoGastosalud_lag CrecimientoVA_lag densidad_lag, ///
    wmat(wrn) model(sdm) fe type(both) vce(cluster id_ent) ///
    effects nsim(500) nolog
xsmle Inci gini_lag pobreza_lag IDS_lag ingr_lag escolaridad_lag CrecimientoGastosalud_lag CrecimientoVA_lag densidad_lag, ///
    wmat(wrn) emat(wrn) model(sac) ///
    fe type(both) vce(cluster id_ent) ///
    effects nsim(500) nolog

	
	
	
******* ANÁLISIS CON UN CAMBIO DE MATRIZ ESPACIAL PORQUE SALE RHO NEGATIVO NO CONGRUENTE CON ANALISIS LISA
clear all
set more off

ssc install shp2dta
shp2dta using "/Users/dmares/Downloads/areas_geoestadisticas_estatales.shp", ///
    database(mex_map) coordinates(mex_coord) genid(id_nueva) replace

use mex_map, clear
destring CVE_ENT, replace
rename CVE_ENT CVE_ENT_num  // Lo guardamos como número

* Extraemos los centroides (x, y) de la base de coordenadas
use mex_coord, clear
collapse (mean) x=_X y=_Y, by(_ID)
rename _ID id_nueva
save "centroides_finales.dta", replace

* 2. PREPARAR LA BASE DE DATOS (PANEL)
use "/Users/dmares/Downloads/basedatos_panel.dta", clear
rename id_ent CVE_ENT_num  // Estandarizamos el nombre para el merge
destring CVE_ENT_num, replace
save "panel_listo.dta", replace

* 3. EL MERGE MAESTRO (Unir Mapa + Coordenadas + Datos)
use mex_map, clear
* Unimos con centroides (x, y)
merge 1:1 id_nueva using "centroides_finales.dta", nogenerate
* Unimos con datos de incidencia (Panel)
capture rename CVE_ENT CVE_ENT_num
destring CVE_ENT_num, replace
merge 1:m CVE_ENT_num using "panel_listo.dta"

*unimos matriz de distancia
spset, clear
spset CVE_ENT_num, coord(x y)
spmatrix create idistance W_inv, replace 
spmatrix normalize W_inv = row






* 4. CONFIGURACIÓN DE LESAGE
xtset CVE_ENT_num t
spset CVE_ENT_num, coord(x y) modify

* Matriz de distancia inversa
spmatrix create idistance W_inv, replace
spmatrix normalize W_inv = row




******restaurar 
clear all
set more off

* 1. PREPARAR LAS COORDENADAS (ASEGURAR EL ID)
use "centroides_finales.dta", clear
* Forzamos que el ID se llame CVE_ENT_num y sea numérico
capture rename id_nueva CVE_ENT_num
capture rename CVE_ENT CVE_ENT_num
destring CVE_ENT_num, replace
save "centroides_limpios.dta", replace

* 2. PREPARAR EL PANEL Y UNIR COORDENADAS
use "/Users/dmares/Downloads/basedatos_panel.dta", clear
capture rename id_ent CVE_ENT_num
destring CVE_ENT_num, replace

* Unimos con las coordenadas que acabamos de limpiar
merge m:1 CVE_ENT_num using "centroides_limpios.dta", keepusing(x y)
* Solo nos quedamos con los que tienen coordenadas (los 32 estados)
keep if _merge == 3
drop _merge

* 3. CONFIGURACIÓN ESPACIAL (RESET TOTAL)
xtset CVE_ENT_num t
spset, clear 
spset CVE_ENT_num, coord(x y)


* 1. CREAR LA MATRIZ EN EL FORMATO QUE XSMLE ENTIENDE (spmat)
* Usamos las coordenadas x e y que ya verificamos que tienes en tu base
preserve
    keep if t == 2014
    
    * Creamos la matriz de distancia inversa directamente como objeto 'spmat'
    * 'id(CVE_ENT_num)' vincula la matriz con tus estados
    spmat idistance W_inv x y, id(CVE_ENT_num) normalize(row) replace
restore

* 2. EJECUTAR EL MODELO XSMLE (SAR)
* Ahora W_inv es un objeto 'spmat' y el error r(498) debe desaparecer
xsmle Inci ingr gini pobreza CrecimientoVA escolaridad densidad, ///
    wmat(W_inv) model(sar) fe type(both)

* 3. IMPACTOS DE LESAGE
* Una vez que el modelo corra, este comando nos dará los spillovers
estat impact














