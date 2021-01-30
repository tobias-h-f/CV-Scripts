

/*

Script for the exam for the course Advanced applied quantitative methods

First recoding then multilevel models and marginal plots.

*/
//Afhængig
sum V95
recode V95 (
-5=.) (-4=.) (-3=.) (-2=.) (-1=.), gen (Ideologi_n)
gen Ideologi=((Ideologi_n-1)/(10-1))*(9-0)
//Uafhængige
//Køn
sum V240
recode V240 (-5-2 =.) (2=1 "kvinde") (1=0 "mand"), gen(Kvinde)
//Uddannelse
sum V248
recode V248 (-5=.) (-2=.) (-1=.), gen (Uddannelse_n)
gen Uddannelse=((Uddannelse_n-1)/(9-1))*(1-0)
//Religion
sum V145
recode V145 (-5=.)(-4=.) (-2=.) (-1=.) (1=7) (2=6) (3=5) (5=3) (6=2) (7=1), gen (Religion_n)
gen Religion=((Religion_n-1)/(7-1))*(1-0)
//Employment
sum V229
recode V229 (-5=.)(-4=.) (-3=.) (-2=.) (-1=.) (1 2 3=1) (4 5 6 7 8=0), gen (Beskæftigelse)
//Postmaterialisme
sum Y001
recode Y001 (-2=.), gen(Postmaterialisme_n)
gen Postmaterialisme=((Postmaterialisme_n-0)/(5-0))*(1-0)
//Abort
sum V204
recode V204 (-5-4-2-1=.), gen(Abort_n)
gen Abort=((Abort_n-1)/(10-1))*(1-0)
//Støtte til kvindebevægelser
sum V123
recode V123(-5-2-1=.), gen(Kvindebevægelse_n)
gen Kvindebevægelse=((Kvindebevægelse_n-1)/(4-1))*(1-0)
//Alder
sum V242
recode V242 (-5-3-2-1=.), gen(Alder)
//deskriptiv statistik
sum Ideologi Kvinde Uddannelse Religion Beskæftigelse Alder Postmaterialisme Abort Kvindebevægelse
//Centrering
g Uddannelse_gc = .
g Religion_gc = .
g Postmaterialisme_gc = .
g Abort_gc = .
g Kvindebevægelse_gc = .
g Alder_gc = .
levelsof V2, local(levels)
foreach l of local levels {
di
di "V2=" `l'
di
quietly su Alder if V2 == `l'
di "age, mean = " r(mean)
replace Alder_gc = Alder
-
r(mean) if V2 == `l'
quietly su Uddannelse if V2 == `l'
di "eur_unific, mean = " r(mean)
replace Uddannelse_gc = Uddannelse
-
r(mean) if V2 == `l'
quietly su Religion if V2 == `l'
di "eu_know, mean = " r(mean)
replace Religion_gc = Religion
-
r(mean) if V2==
`l'
13
quietly su Postmaterialisme if V2 == `l'
di "conf_nat_parl, mean = " r(mean)
replace Postmaterialisme_gc = Postmaterialisme
-
r(mean) if V2 == `l'
quietly su Abort if V2 == `l'
di "conf_ep, mean = " r(mean)
replace Abort_gc = Abort
-
r(mean) if V2 == `l'
quietly su Kvindebevægelse if V2 == `l'
di "age, eu_resp = " r(mean)
replace Kvindebevægelse_gc = Kvindebevægelse
-
r(mean) if V2 == `l'
}
ta V2, sum(Uddannelse) nol
ta V2, sum(Religion) nol
ta V2, sum(Postmaterialis
me) nol
ta V2, sum(Abort) nol
ta V2, sum(Kvindebevægelse) nol
ta V2, sum(Alder) nol
//forudsætningstest
reg Ideologi Kvinde Uddannelse_gc Religion_gc Beskæftigelse Postmaterialisme_gc Abort_gc
Kvindebevægelse_gc Alder_gc
//lineariet
foreach var in Uddannelse Religion Postmaterialisme Abort Kvindebevægelse Alder{
acprplot `var', name(acpr_`var')
}
//Indfl
ydelsesrige observationer
lvr2plot
predict tempcooksd, cooksd
display 4/e(N)
summarize tempcooksd if tempcooksd >(4/e(N))
browse if tempcooksd >(4/e(N)) & tempcooksd <.
dfbeta
di 2/e(N)
sum _dfbeta_1 if abs(_dfbeta_1)>2/sqrt(e(N))
sum _dfbeta_2 if abs(_dfbeta_2)>2/sqrt(e(N))
sum _dfbeta_3 if abs(_dfbeta_3)>2/sqrt(e(N))
sum _dfbeta_4 if abs(_dfbeta_4)>2/sqrt(e(N))
sum _dfbeta_5 if abs(_dfbeta_5)>2/sqrt(e(N))
sum _dfbeta_6 if abs(_dfbeta_6)
>2/sqrt(e(N))
sum _dfbeta_7 if abs(_dfbeta_7)>2/sqrt(e(N))
sum _dfbeta_8 if abs(_dfbeta_8)>2/sqrt(e(N))
//Normalfordelte fejlled
predict tempresid, residuals
histogram tempresid, normal
qnorm tempresid
pnorm tempresid
qnorm tempresid
//Varianshomogenitet
estat hettest
estat imtest, white
//multikollinaritet
vif
//Modelbygning
xtset V2
eststo fixed: xtreg Ideologi Kvinde Uddannelse_gc Religion_gc Beskæftigelse Alder_gc
Postmaterialisme_gc Abort_gc Kvindebevægelse_gc, fe
eststo random: xtreg Ideologi Kvinde Uddannelse_gc Religion_gc Beskæftigelse Alder_gc
Postmaterialisme_gc Abort_gc K
vindebevægelse_gc, re
hausman fixed random
mixed Ideologi Kvinde Uddannelse_gc Religion_gc Beskæftigelse Postmaterialisme_gc Abort_gc
Kvindebevægelse_gc Alder_gc|| V2: , mle
14
est sto m2
mixed Ideologi || V2: if e(sample), mle
est sto m1
lrtest m2 m1
mixed Ideologi Kvinde Uddannelse_gc Religion_gc Beskæftigelse Postmaterialisme_gc Abort_gc
Kvindebevægelse_gc Alder_gc || V2: Kvinde if e(sample), mle
est sto m3
lrtest m3 m2
//Tabelbygning
mixed Ideologi || V2: if e(sample), mle varia
nce robust
estat icc
mixed Ideologi Kvinde || V2: Kvinde if e(sample), mle cov(unstructured) robust
estat icc
//Med social structure
mixed Ideologi Kvinde Uddannelse_gc Religion_gc Beskæftigelse Alder_gc || V2: Kvinde if e(sample),
mle cov(unstructu
red) robust
estat icc
//Med social structure og attitude
mixed Ideologi Kvinde Uddannelse_gc Religion_gc Beskæftigelse Alder_gc Postmaterialisme_gc Abort_gc
Kvindebevægelse_gc || V2: Kvinde if e(sample), mle cov(unstructured) robust
estat icc
predict ris_u*, reffects
describe ris_u*
gen predicted_val
ue_ris = ris_u2 + ris_u1*Kvinde
sort V2 Kvinde
twoway line predicted_value_ris Kvinde, xlabel(0 1)
connect(ascending) lcolor(black) ///
scheme(s1mono) title("Graf 1: Variation over lande af
kønsforskelle på ideologi") ///
xtitle(" " "Køn") ytitle("Ideologi ")
describe ris_u*
sort ris_u2
tablist ris_u2 ris_u1 V2, sort(v)