/*Trabajo final
Grupo 10:
Angie Quispe	20183499
Luis  Quispe	20170860
Renzo Mosquera  20181960  */

global main  	"C:\Users\Luis PUCP\Desktop\2021-2 pucp\stata\trabajo_final"
global maps		"$main\maps"


**Usamos la base de datos
use "$main/Peru LAPOP AmericasBarometer 2019 v1.0_W (1).dta", clear
*o tambien puede usar la base en linea:
*use "http://datasets.americasbarometer.org/database/files/Peru%20LAPOP%20AmericasBarometer%202019%20v1.0_W.dta", clear


*Nos quedamos con las variables de nuestro interés 
keep pais estratopri prov ur q2 q1 env2b ed q10new l1 drk1 env1 envp41 envp42 psc6

*vemos la descripción de las variables
label list q2_esp q1_esp env2b_esp ed_esp q10new_esp l1_esp drk1_esp env1_esp envp41_esp envp42_esp psc13_10_esp psc13_7_esp psc6_esp

*Recodificamos nuestra variable endógena para que sea dummy
recode 	env2b (1/2=1 "Serio") (3/4=0 "No importa"), gen(env_import) 
		label variable env_import "Importancia del cambio climático"
tab env_import	

*Pasamos con las varaibles explicativas
	//renombramos variables
	rename (q2 ed) (edad years_educ)

	//sexo
	tab q1
	recode q1 (1=1 "Masculino") (2=0 "Femenino"), gen(sexo)
	label variable sexo "Sexo"
	tab sexo
	drop q1
	
	//urbano
	tab ur
	recode ur (1=1 "Urb") (2=0 "Rur"), gen (urbano)
	label variable urbano "(Urbano/Rural)"
	tab urbano
	drop ur
	
	
	//rmv: menos de 1000 soles o no
	tab q10new
	recode q10new (0/7=0 "Ingreso menor a 1000") (8/16=1 "Ingreso mayor a 1000"), gen(rmv)
	label variable rmv "Percibe la remuneracion mínima vital"
	tab rmv
	drop q10new
	
	//left: es ideología izquierda política
	tab l1
	recode l1 (1/5=1 "Izquierda") (6/10=0 "Derecha"), gen(left)
	label variable left "Espectro político"
	tab left
	drop l1 
	
	//perjuicio_nat: probabilidad de ser dañado por un desastre natural
	tab drk1
	recode drk1 (1/2=0 "No le pasa nada") (3/4=1 "Puede perjudicarse"), gen(perjuicio_nat)
	label variable perjuicio_nat "Daño ante desastre natural"
	tab perjuicio_nat
	drop drk1
	
	//dilemma: proteger la economía o el medio ambiente
	tab env1	//hay una tercera opción ambigua, eliminar esas observaciones
	drop if env1==3
	recode env1 (1=1 "Proteger medio ambiente") (2=0 "Proteger la economía"), gen(dilemma)
	label variable dilemma "¿Qué prioriza: economía o ambiente?"
	tab dilemma
	drop env1
	
	//fault: quién tiene la culpa de la contaminacion envp41 envp42
	label list envp41_esp envp42_esp
	egen bbb = rowmin(envp41 envp42)	//generamos la variable culpa a partir 
	label values bbb envp42_esp			//le aplicamos una etiqueta existente
	recode bbb (1=1 "Desperdicios de las personas") (2/7=0 "Desperdicios de empresas/Estado"), gen(fault)
	label variable fault "Responsable de la contaminación"
	drop envp41 envp42 bbb
	
	//floods: quién tiene la culpa de las inundaciones
	tab psc6
	recode psc6 (5=1 "Cambio climatico") (1/4=0 "La población") (6/77=0 "La población"), gen(floods)
	label variable floods "Culpable de las inundaciones/huaycos"
	tab floods
	drop psc6
	

*realizams un sum para verificar datos
sum
*eliminamos las missing
 foreach var of varlist floods fault dilemma perjuicio_nat left rmv urbano sexo env_import years_educ env2b edad prov estratopri {
 drop if missing(`var')
 } 
*verificamos que no haya missing
sum edad env2b years_educ env_import sexo urbano rmv left perjuicio_nat dilemma fault floods
outreg2 using "$main/summary.doc", replace sum(log)

*guardamos
save "$main/base_peru.dta", replace

*========================
*Mapa
*========================
//generamos un identificador por departamento 
gen provincia = prov
tostring provincia, replace
gen dpto = substr(provincia,3,4)

//colapsamos para obtener algunos promedios de interés
collapse (mean) env_import perjuicio_nat fault, by(dpto)

//guardamos nuestra base "colapsada"
save "$maps/means_maps.dta", replace	

//instalamos los paquetes necesarios para usar funciones de mapas
*ssc install shp2dta, replace
*ssc install spmap, replace
*ssc install mif2dta, replace

//importamos la base de mapas
shp2dta using "$maps/DEPARTAMENTOS.shp", database("$maps/peru_data.dta") ///
coordinates("$maps/peru_coord.dta") genid(id) genc(c) replace

//unimos las bases de peru_data con la de la pregunta anterior para graficar
use "$maps/peru_data.dta", clear
preserve
generate label = DEPARTAMEN
keep IDDPTO x_c y_c label
gen  length = length(label)
save "$maps/Labels.dta", replace
restore
rename IDDPTO dpto
merge 1:1 dpto using "$maps/means_maps.dta", nogen
rename dpto IDDPTO
format env_import perjuicio_nat fault %12.3f

//empezamos a graficar
//grafica de env_import donde 1 es cercano a SERIO
spmap 	env_import using "$maps/peru_coord.dta", id(id) clmethod(q) ///
		title("Importancia del cambio climático" "para la población de cada departamento") ///
		legend(size(small) position (8) fcolor(Greens) note("Fuente: LAPOP, elab. propia" "No se tomaron muestras para Madre de Dios") ) ///
		graphregion(color(white)) label(data("$maps/Labels.dta") x(x_c) y(y_c) ///
		label(label) size(*0.4 ..) position (0 6)  length (21)) fcolor(Blues2) ndfcolor(white)
graph export "$maps/env_import.png", as(png) replace

//grafica de perjuicio_nat donde 1 es cercano a SERIO
spmap 	perjuicio_nat using "$maps/peru_coord.dta", id(id) clmethod(q) ///
		title("Porcentaje de la población" "que cree puede ser víctima de un" "desastre natural") ///
		legend(size(small) position (7) fcolor(sand) note("Fuente: LAPOP, elab. propia" "No se tomaron muestras para Madre de Dios") ) ///
		graphregion(color(white)) label(data("$maps/Labels.dta") x(x_c) y(y_c) ///
		label(label) size(*0.4 ..) position (0 8) length (21)) fcolor(Heat) ndfcolor(white)
graph export "$maps/perjuicio_nat.png", as(png) replace

//grafica de fault donde 1 es cercano a SERIO
spmap 	fault using "$maps/peru_coord.dta", id(id) clmethod(q) ///
		title("Porcentaje de la población" "que cree la contaminación" "es culpa de los mismos ciudadanos") ///
		legend(size(small) position (7) fcolor(olive) note("Fuente: LAPOP, elab. propia" "No se tomaron muestras para Madre de Dios") ) ///
		graphregion(color(white)) label(data("$maps/Labels.dta") x(x_c) y(y_c) ///
		label(label) size(*0.4 ..) position (0 6) length (21)) fcolor(Greens2) ndfcolor(white)
graph export "$maps/fault.png", as(png) replace


*========================
*Gráficas
*========================
use "$main/base_peru.dta", clear 

//barras de importancia frente al cambio climático por provincia
	//colapsamos la base
	collapse (mean) env_import, by(prov)
	sum env_import
	local min_env_import=r(min)	
graph hbar env_import , ///
 over(prov, sort (env_import) label(labsize(vsmall))) ///
 blabel(total, format(%12.3f) size(vsmall))  yline(`min_env_import') ///
 title("Importancia del cambio climático", size(medium)) ///
 ytitle("Porcentaje de la población") ///
 graphregion(color(white)) ylabel(,nogrid)
 
graph export "$main/polbacion_import_cc.png", as(png) replace


//pastel: quien tiene la culpa de la contaminación
use "$main/base_peru.dta", replace

graph pie, over(dilemma) ///
 plabel(_all percent, size(vsmall)) graphregion(color(white)) ///
 legend(rows(2) region(lcolor(white))) ///
 title("¿Qué le importa más?")
 graph export "$main/pie_dilemma.png", as(png) replace
 

*Tabla
//tabla: inundación vs quien tiene culpa de la contaminación
tabulate floods fault

//peligro de perjudicarse ante un desastre por zona
tabulate ur perjuicio_nat


*Regresion
use "$main/base_peru.dta", replace


* Modelo 1
*----------

* Planteamos un primer modelo con variables exógenas básicas como el sexo, edad y años de educación del encuestado. Progresivamente, iremos aumentando las variables.
*ssc install fitstat
	//instalamos este paquete para obtener información del modelo
	
eststo m1:probit env_import sex edad years_educ, vce(r) nolog level(95) cformat(%6.4fc)
fitstat					//R2-McFadde 0.061; McKelvey&Zavoina 0.092
estat ic				//AIC 711.06
estat classification	//90.32% correctamente clasificadas
lroc					//69.99% debajo de la curva, bien
lsens 

//outreg m1
probit env_import sex edad years_educ, vce(r) nolog level(95) cformat(%6.4fc)
outreg2 using "m1.doc", bdec(4) sdec(4) stats(coef se) ctitle(Modelo 1) addstat(Pseudo-R: , e(r2_p), Log-Likelihood:, e(ll),  Chi-squared, e(chi2), Prob Wald:, e(p)) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") replace
	// Efectos Marginales
	mfx 
	outreg2 using "m1.doc", mfx ctitle("Efecto marginal")
	

* Modelo 2
*----------
use "$main/base_peru.dta", replace

* Planteamos un segundo agregando variables más específicas como la remuneración mínima vital, la ideología política de los entrevistados y la idea que tienen sobre si podrían perjudicarse por un desastre natural.

eststo m2:probit env_import sex edad years_educ rmv left perjuicio_nat, vce(r) nolog level(95) cformat(%6.4fc)
fitstat					//R2-McFadde 0.078; McKelvey&Zavoina 0.12
estat ic				//AIC 704.3004
estat classification	//90.32% correctamente clasificadas
lroc					//70.69% debajo de la curva, bien
lsens

probit env_import sex edad years_educ rmv left perjuicio_nat, vce(r) nolog level(95) cformat(%6.4fc)
outreg2 using "m2.doc", bdec(4) sdec(4) stats(coef se) ctitle(Modelo 2) addstat(Pseudo-R: , e(r2_p), Log-Likelihood:, e(ll),  Chi-squared, e(chi2), Prob Wald:, e(p)) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") replace 
	// Efectos Marginales
	mfx 
	outreg2 using "m2.doc", mfx ctitle("Efecto marginal")
	
* Modelo 3
*-----------
use "$main/base_peru.dta", replace

* Por último, planteamos un tercer modelo que incluya otras variables sobre preferencias y adjudicación de responsabilidad por la contaminación o las inundaciones. 

eststo m3:probit env_import sex edad years_educ rmv left perjuicio_nat dilemma fault floods , vce(r) nolog level(95) cformat(%6.4fc)
fitstat					//R2-McFadde 0.089; McKelvey&Zavoina 0.142
estat ic				//AIC 702.4721
estat classification	//90.32% correctamente clasificadas
lroc					//71.03% debajo de la curva, bien
lsens

probit env_import sex edad years_educ rmv left perjuicio_nat dilemma fault floods , vce(r) nolog level(95) cformat(%6.4fc)
mfx
outreg2 using "m3.doc", bdec(4) sdec(4) stats(coef se) ctitle(Modelo 3) addstat(Pseudo-R: , e(r2_p), Log-Likelihood:, e(ll),  Chi-squared, e(chi2), Prob Wald:, e(p)) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") replace 


	// Efectos Marginales
	mfx
	outreg2 using "m3.doc", mfx ctitle("Efecto marginal")


* Tablas de regresión:

esttab m1 m2 m3, replace label title("Estimaciones") ///
					   b(3) se(3) stats(N r2, fmt(0 3) ///
					   labels("Observations" "R2")) ///
					   star(* 0.10 ** 0.05 *** 0.01) ///
					   mtitle("Modelo 1" "Modelo 2" "Modelo 3") ///
		               note("Standard errors in parentheses")

* Exportamos las tablas:
use "$main/base_peru.dta", replace

probit env_import sex edad years_educ, vce(r) nolog level(95) cformat(%6.4fc)
outreg2 using "regresiones.doc", bdec(4) sdec(4) stats(coef se) ctitle(Modelo 1) addstat(Pseudo-R: , e(r2_p), Log-Likelihood:, e(ll),  Chi-squared, e(chi2), Prob Wald:, e(p)) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") replace

probit env_import sex edad years_educ rmv left perjuicio_nat, vce(r) nolog level(95) cformat(%6.4fc)
outreg2 using "regresiones.doc", bdec(4) sdec(4) stats(coef se) ctitle(Modelo 2) addstat(Pseudo-R: , e(r2_p), Log-Likelihood:, e(ll),  Chi-squared, e(chi2), Prob Wald:, e(p)) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") 

probit env_import sex edad years_educ rmv left perjuicio_nat dilemma fault floods , vce(r) nolog level(95) cformat(%6.4fc)
outreg2 using "regresiones.doc", bdec(4) sdec(4) stats(coef se) ctitle(Modelo 3) addstat(Pseudo-R: , e(r2_p), Log-Likelihood:, e(ll),  Chi-squared, e(chi2), Prob Wald:, e(p)) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") 

* Exportamos las tablas con efectos marginales:
use "$main/base_peru.dta", replace

probit env_import sex edad years_educ, vce(r) nolog level(95) cformat(%6.4fc)
mfx 
outreg2 using "regresionesm.doc", bdec(4) sdec(4) mfx stats(coef se) ctitle(Efectos Marginales-Modelo 1) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%")  replace

probit env_import sex edad years_educ rmv left perjuicio_nat, vce(r) nolog level(95) cformat(%6.4fc)
mfx
outreg2 using "regresionesm.doc", bdec(4) sdec(4) mfx stats(coef se) ctitle(Efectos Marginales-Modelo 2) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") 

probit env_import sex edad years_educ rmv left perjuicio_nat dilemma fault floods , vce(r) nolog level(95) cformat(%6.4fc)
mfx
outreg2 using "regresionesm.doc", bdec(4) sdec(4) mfx stats(coef se) ctitle(Efectos Marginales-Modelo 3) addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") 







