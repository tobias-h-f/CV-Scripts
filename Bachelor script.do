/*

Script from my Bachelor thesis

The script starts with recoding of variables and continues with multilevel models
with interaction and marginal effect plots before ending with prerequisite tests.


*/ 

 

*Postmaterialisme* 

 

*Omkodning* 

 

generate First_prio=V9 

 

generate Second_prio=V10 

 

recode First_prio 1=. if V10==1 

 

recode First_prio 2=. if V10==2  

 

recode First_prio 3=. if V10==3  

 

recode First_prio 4=. if V10==4 

 

recode First_prio 1=0 3=0 2=1 4=1 

 

 

recode Second_prio 1=. if V9==1 

 

recode Second_prio 2=. if V9==2  

 

recode Second_prio 3=. if V9==3  

 

recode Second_prio 4=. if V9==4 

 

recode Second_prio 1=0 3=0 2=1 4=1 

 

 

 

*Indeks* 

 

alpha First_prio Second_prio, item casewise gen (Indeks_postmat) 

 

generate Indeks_postmat_100= ((Indeks_postmat-2)/(0-2))*100 

 

sum Indeks_postmat_100 

 

gen Postmaterialisme= Indeks_postmat_100-60.92218  

 

 

*Miljøparadigmet* 

 

codebook V20  

 

codebook V21  

 

codebook V27  

 

codebook V24  

 

*omkodning* 

 

gen Sci_faith=V20 

 

recode Sci_faith 1=5 2=4 4=2 5=1 

 

gen Sci_harm=V21  

 

recode Sci_harm 1=5 2=4 4=2 5=1 

 

gen Econ_harm=V27 

 

recode Econ_harm 1=5 2=4 4=2 5=1 

 

gen Mod_harm=V24 

 

recode Mod_harm 1=5 2=4 4=2 5=1 

 

 

alpha Sci_faith Sci_harm Econ_harm Mod_harm, casewise generate(Indeks_NEP) 

 

generate Indeks_NEP_100= ((Indeks_NEP-5)/(1-5))*100 

 

gen Miljøparadigmet=Indeks_NEP_100-49.55308 

 

 

*Bekymring for miljøet 

 

codebook V39 /*bilforurening*/ 

 

gen Car_Pol=V39 

 

recode Car_Pol 5=1 4=2 2=4 1=5 

 

codebook V40 /*industriforurening*/ 

 

gen Indu_Pol=V40 

 

recode Indu_Pol 5=1 4=2 2=4 1=5 

 

codebook V41 /*Pesticideforurening*/ 

 

gen Pest_Pol=V41 

 

recode Pest_Pol 5=1 4=2 2=4 1=5 

 

codebook V42 /*forurening af loder og søer*/ 

 

gen Lake_Pol=V42 

 

recode Lake_Pol 5=1 4=2 2=4 1=5 

 

codebook V43 

 

 *klimaet er skyld i temperaturstigninger* 

. gen Temp_Cause_Climate=V43 

 

. recode Temp_Cause_Climate 5=1 4=2 2=4 1=5 

 

alpha Car_Pol Indu_Pol Pest_Pol Lake_Pol Temp_Cause_Clima, casewise generate(Concern_Climate) 

 

gen Concern_Climate_2= ((Concern_Climate-5)/(1-5))*100 

 

gen Bekymring=Concern_Climate_2- 28.36944  

 

*Villighed til at påtage sig omkostninger* 

 

codebook V30 /*højere skatter*/ 

 

gen High_Tax=V30 

recode High_Tax 5=1 4=2 2=4 1=5 

 

codebook V29 /*højere priser*/ 

 

gen High_Price=V29 

recode High_Price 5=1 4=2 2=4 1=5 

 

codebook V31 /* begrænsning af levestandard */ 

 

gen Cut_Standard=V31 

recode Cut_Standard 5=1 4=2 2=4 1=5 

 

alpha High_Tax High_Price Cut_Standard, casewise generate(Take_Cost) 

 

 

gen Take_Cost_100= ((Take_Cost-5)/(1-5))*100 

 

gen Omkostninger=Take_Cost_100-59.35932 

 

*Bilkørsel* 

 

gen Pract_car=V57 

 

recode Pract_car 4=1 3=2 2=3 1=4 

 

*Centrering af bilkørsel* 

 

generate Car_100= ((Pract_car-4)/(1-4))*100 

 

gen Bilkørsel=Car_100- 70.09084 

 

*Affaldssortering* 

 

gen Pract_waste=V55 

 

recode Pract_waste 4=1 3=2 2=3 1=4 

 

generate Waste_100= ((Pract_waste-4)/(1-4))*100 

 

gen Affald=Waste_100-32.56035 

 

*Nationalkultur* 

 

gen Kultur=0 

 

replace Kultur=1 if inlist(V4,158,392,420,608) 

 

replace Kultur=. if inlist(V4,152,376,554,710,792,840,124,643,32,484) 

 

*Kontrolvariable* 

 

*Social kapital* 

 

*Social tillid* 

 

codebook V11 

 

codebook V12 

 

. alpha V11 V12, casewise generate(Social_Trust) 

 

generate Social_Trust_100= ((Social_Trust-5)/(1-5))*100 

 

gen Social_tillid=Social_Trust_100-54.01288 

 

*Institutional tillid* 

 

codebook V13  

 

gen Inst_trust=V13 

 

recode Inst_trust 1=5 2=4 4=2 5=1 

 

generate Inst_Trust_100= ((Inst_trust-5)/(1-5))*100 

 

gen Institutionel_tillid= Inst_trust-58.58276 

 

 

 

 

*Civil deltagelse* 

 

*Gruppe aktivitet* 

 

codebook V61 /*medlem af gruppe */ 

 

gen Memb_Group=V61 

 

recode Memb_Group 2=0 

 

codebook UNION /*fagforening */  

 

.  gen Memb_Union=UNION 

 

. recode Memb_Union 2=0 3=0 

 

*Individaktivitet* 

 

codebook V63 /*givet penge*/ 

 

gen Give_Money=V63 

 

recode Give_Money 2=0 

 

codebook V64 /*demonstration */ 

 

 gen Part_Dem=V64 

 

. recode Part_Dem 2=0 

 

codebook VOTE_LE /*stemte ved sidste valg */ 

 

 gen Vote=VOTE_LE 

  

. recode Vote 2=0 

 

alpha Give_Money Part_Dem Vote Memb_Group Memb_Union, casewise generate (Civ_Part) 

 

generate Civ_Part_100= ((Civ_Part-1)/(0-1))*100 

 

gen Civil_deltagelse= Civ_Part_100-75.77157  

 

*køn* 

codebook AGE 

 

gen Alder=AGE-47.30321 

 

codebook SEX 

 

gen Køn=SEX 

 

recode Køn 1=0 2=1 

 

*Uddannelse* 

codebook EDUCYRS 

 

gen Uddannelse=EDUCYRS-15.79191 

 

codebook TOPBOT 

 

gen Socialklasse=TOPBOT- 5.126962 

 

*BNP per indbygger* 

 

gen GDP_Pr_Capita=. 

 

recode GDP_Pr_Capita .=10.41 if V4==32 

 

recode GDP_Pr_Capita .=56.45 if V4==36 

 

recode GDP_Pr_Capita .=46.96 if V4==40 

 

recode GDP_Pr_Capita .=44.69 if V4==56 

 

recode GDP_Pr_Capita .=6.74 if V4==100 

 

recode GDP_Pr_Capita .=47.63 if V4==124 

 

recode GDP_Pr_Capita .=12.79 if V4==152 

 

recode GDP_Pr_Capita .=19.26 if V4==158 

 

recode GDP_Pr_Capita .=13.55 if V4==191 

 

recode GDP_Pr_Capita .=19.83 if V4==203 

 

recode GDP_Pr_Capita .=58.18 if V4==208 

 

recode GDP_Pr_Capita .=46.39 if V4==246 

 

recode GDP_Pr_Capita .=42.18 if V4==250 

 

recode GDP_Pr_Capita .=42.64 if V4==276 

 

recode GDP_Pr_Capita .=30.67 if V4==376 

 

recode GDP_Pr_Capita .=44.67 if V4==392 

 

recode GDP_Pr_Capita .=22.09 if V4==410 

 

recode GDP_Pr_Capita .=11.23 if V4==428 

 

recode GDP_Pr_Capita .=12.01 if V4==440 

 

recode GDP_Pr_Capita .=9.26 if V4==484 

 

recode GDP_Pr_Capita .=33.22 if V4==554 

 

recode GDP_Pr_Capita .=87.43 if V4==578 

 

recode GDP_Pr_Capita .=2.16 if V4==608 

 

recode GDP_Pr_Capita .=11.41 if V4==643 

 

recode GDP_Pr_Capita .=16.63 if V4==703 

 

recode GDP_Pr_Capita .=7.38 if V4==710 

 

recode GDP_Pr_Capita .=30.8 if V4==724 

 

recode GDP_Pr_Capita .=51.93 if V4==752 

 

recode GDP_Pr_Capita .=74.88 if V4==756 

 

recode GDP_Pr_Capita .=10.48 if V4==792 

 

recode GDP_Pr_Capita .=39.12 if V4==826 

 

recode GDP_Pr_Capita .=48.4 if V4==840 

 

gen BNP_pr_indbygger= GDP_Pr_Capita-29.53991 

 

 

 

 

 

*Korrelationsanalyse* 

 

pwcorr Postmaterialisme Miljøparadigmet Omkostninger Bekymring Bilkørsel Affald, sig star(0.05) 

 

*MLA - tomme modeller og random intercept* 

 

*Holdning* 

 

mixed Omkostninger || V4:, mle variance 

 

est sto m1 

 

estat icc 

 

mixed Omkostninger Postmaterialisme Miljøparadigmet Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

estat icc 

 

est sto m2 

 

mixed Bekymring || V4:, mle variance 

 

est sto m3 

 

estat icc 

 

mixed Bekymring Postmaterialisme Miljøparadigmet Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

estat icc 

 

est sto m4 

 

 

*Adfærd* 

 

mixed Bilkørsel || V4:, mle variance 

 

est sto m5 

 

estat icc 

 

mixed Bilkørsel Postmaterialisme Miljøparadigmet Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

estat icc 

 

est sto m6 

 

mixed Affald || V4:, mle variance 

 

est sto m7 

 

estat icc 

 

mixed Affald Postmaterialisme Miljøparadigmet Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse  || V4:, mle variance 

 

estat icc 

 

est sto m8 

 

mixed Bilkørsel Postmaterialisme Miljøparadigmet Bekymring Omkostninger Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

est sto m9 

 

estat icc 

 

mixed Affald Postmaterialisme Miljøparadigmet Bekymring Omkostninger Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse  || V4:, mle variance 

 

est sto m10 

 

estat icc 

 

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 using m2AASOO2tabeltranstabel1.rtf, b(4) /// 

se star(* 0.05 ** 0.01 *** 0.001)  /* Kovarians, individ- og landevarians indført manuelt efter export*/ 

 

*LR Test for analysedel 2* 

 

*Lrtest for postmaterialisme* 

 

*Bekymring - postmaterialisme* 

 

*Uden ustrukturerede keofficienter* 

 

mixed Bekymring Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

est sto m11 

 

mixed Bekymring Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Postmaterialisme, mle variance 

 

est sto m12 

 

lrtest m11 m12 

 

*Omkostninger - postmaterialisme* 

mixed Omkostninger Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

est sto m13 

 

mixed Omkostninger Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Postmaterialisme, mle variance cov(unstructured) 

 

est sto m14 

 

lrtest m13 m14 

 

*Bilkørsel med variation over postmaterialisme* 

 

*Uden ustruktureret koefficient 

 

mixed Bilkørsel Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse|| V4:, mle variance 

 

est sto m15 

 

mixed Bilkørsel Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Postmaterialisme, mle variance 

 

estat icc 

 

est sto m16 

 

lrtest m15 m16 

 

*Affald - postmaterialisme* 

 

mixed Affald Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

est sto m17 

 

mixed Affald Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Postmaterialisme, mle variance cov(unstructured) 

 

estat icc 

 

est sto m18 

 

lrtest m17 m18 

 

*LRtest for miljøparadigme* 

 

*Bekymring - miljøparadigme* 

 

mixed Bekymring Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

est sto m19 

 

mixed Bekymring Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

est sto m20 

 

lrtest m19 m20 

 

*Omkostninger med miljøparadigme* 

mixed Omkostninger Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

est sto m21 

 

mixed Omkostninger Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

estat icc 

 

est sto m22 

 

lrtest m21 m22 

 

 

*Bilkørsel - miljøparadigme* 

 

mixed Bilkørsel Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

est sto m23 

 

mixed Bilkørsel Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

estat icc 

 

est sto m24 

 

lrtest m23 m24 

 

*Affald - miljøparadigme* 

 

mixed Affald Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

est sto m25 

 

mixed Affald Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

estat icc 

 

est sto m26 

 

lrtest m25 m26 

 

*LRtest med holdningsvariable* 

 

*Variation over Postmaterialisme* 

 

*Uden ustrukturerede koefficienter) 

 

mixed Affald Postmaterialisme Miljøparadigmet Bekymring Omkostninger Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

est sto m27 

 

mixed Affald Postmaterialisme Miljøparadigmet Bekymring Omkostninger Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Postmaterialisme, mle variance 

 

estat icc 

 

est sto m28 

 

lrtest m27 m28 

 

mixed Bilkørsel Postmaterialisme Miljøparadigmet Bekymring Omkostninger Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

est sto m29 

 

mixed Bilkørsel Postmaterialisme Miljøparadigmet Bekymring Omkostninger Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Postmaterialisme, mle variance 

 

estat icc 

 

est sto m30 

 

lrtest m29 m30 

 

*Variation over postmodernisme* 

 

mixed Bilkørsel Postmaterialisme Miljøparadigmet Bekymring Omkostninger Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

est sto m31 

 

mixed Bilkørsel Postmaterialisme Miljøparadigmet Bekymring Omkostninger Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Postmodernisme, mle variance cov(unstructured) 

 

estat icc 

 

est sto m32 

 

lrtest m31 m32 

 

mixed Affald Postmaterialisme Miljøparadigmet Bekymring Omkostninger Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4:, mle variance 

 

est sto m33 

 

mixed Affald Postmaterialisme Miljøparadigmet Bekymring Omkostninger Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Postmodernisme, mle variance cov(unstructured) 

 

estat icc 

 

est sto m34 

lrtest m33 m34 

*Spaghettiplots og tilhørende tabeller* 

 

mixed Bekymring Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

predict ris_u*, reffects 

describe ris_u* 

gen predicted_value_ris = ris_u2 + ris_u1*Miljøparadigmet 

 

sort V4 Miljøparadigmet 

twoway line predicted_value_ris Miljøparadigmet, connect(ascending) lcolor(black) /// 

scheme(s1mono) title("Lande-niveau effekt fra miljøparadigmet" "på bekymring for miljø og klima") /// 

xtitle(" " "miljøparadigmet") ytitle("Bekymring for miljøet ")	 

 

 

graph export Bekymringmiljøparadigmet.suffix, as(pdf) 

 

describe ris_u* 

sort ris_u2 

tablist ris_u2 ris_u1 V4, sort(v)					 

 

drop ris_u* predicted_value_ris 

 

 

mixed Omkostninger Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

predict ris_u*, reffects 

describe ris_u* 

gen predicted_value_ris = ris_u2 + ris_u1*Miljøparadigmet 

 

 

sort V4 Miljøparadigmet 

twoway line predicted_value_ris Miljøparadigmet, connect(ascending) lcolor(black) /// 

scheme(s1mono) title("Lande-niveau effekt fra miljøparadigmet" "på omkostninger") /// 

xtitle(" " "Miljøparadigmet") ytitle("Omkostninger")	 

 

graph export omkostningermiljøparadigmet.suffix, as(pdf) 

 

 

describe ris_u* 

sort ris_u2 

tablist ris_u2 ris_u1 V4, sort(v)					 

 

drop ris_u* predicted_value_ris 

 

 

mixed Affald Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

 

 

predict ris_u*, reffects 

describe ris_u* 

gen predicted_value_ris = ris_u2 + ris_u1*Miljøparadigmet 

 

 

sort V4 Miljøparadigmet  

twoway line predicted_value_ris Miljøparadigmet, connect(ascending) lcolor(black) /// 

scheme(s1mono) title("Lande-niveau effekt fra miljøparadigmet" "affaldssortering") /// 

xtitle(" " "Miljøparadigmet") ytitle("Affaldssortering ")	 

 

 

graph export affaldssorteringmiljøparadigmet.suffix, as(pdf)	 

 

describe ris_u* 

sort ris_u2 

tablist ris_u2 ris_u1 V4, sort(v)	 

 

 

drop ris_u* predicted_value_ris 

 

mixed Affald Postmaterialisme Miljøparadigmet Bekymring Omkostninger Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

 

 

predict ris_u*, reffects  

describe ris_u* 

gen predicted_value_ris = ris_u2 + ris_u1*Miljøparadigmet  

 

 

sort V4 Miljøparadigmet  

twoway line predicted_value_ris Miljøparadigmet, connect(ascending) lcolor(black) /// 

scheme(s1mono) title("Lande-niveau effekt fra miljøparadigmet" "affaldssortering") /// 

xtitle(" " "miljøparadigmet med kontrol for holdning") ytitle("Affaldssortering ")	 

 

 

graph export holdningmiljøparadigmet.suffix, as(pdf)	 

 

describe ris_u* 

sort ris_u2 

tablist ris_u2 ris_u1 V4, sort(v)					 

 

drop ris_u* predicted_value_ris 

 

 

*Analysedel 2 - stokastiske hældninger* 

 

 

*Interaktionsanalyse - random slopes* 

 

mixed Bekymring Postmaterialisme Miljøparadigmet Kultur c.Miljøparadigmet##i.Kultur c.Postmaterialisme##i.Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

est sto m35 

 

est sto m39 

 

mixed Omkostninger Postmaterialisme Miljøparadigmet Kultur c.Miljøparadigmet##i.Kultur c.Postmaterialisme##i.Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

est sto m36 

 

est sto m40 

 

mixed Affald Postmaterialisme Miljøparadigmet Kultur c.Miljøparadigmet##i.Kultur c.Postmaterialisme##i.Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

est sto m37 

 

est sto m41 

 

mixed Affald Postmaterialisme Miljøparadigmet Bekymring Omkostninger Kultur c.Miljøparadigmete##i.Kultur c.Postmaterialisme##i.Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

est sto m38 

 

est sto m42 

 

*Udtræk til tabel 2* 

 

esttab m35 m36 m37 m38 using m2AASOO2tabeltranstabel2.rtf, b(4) /// 

se star(* 0.05 ** 0.01 *** 0.001)  /* Kovarians, individ- og landevarians, samt kovarians indført manuelt efter export*/ 

 

 

*Marginsplot* 

 

mixed Bekymring c.Miljøparadigmet##i.Kultur c.Postmaterialisme##i.Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

margins, dydx(Miljøparadigmet) over(Kultur) 

 

margins, at(Miljøparadigmet=(1(10)100) Kultur=(0 1)) 

 

marginsplot, recast(line) ciopts(lpattern("_"))  xlabel(#10) ylabel(#3)  /// 

scheme(s2mono) graphregion(fcolor(white)) title("Marginale effekter fra kultur på bekymring for miljøet") ytitle("Bekymring for miljøet") /// 

xtitle("Miljøparadigme") 

 

graph export bekymring-paradigme.suffix, as(pdf) 

 

margins, dydx(Postmaterialisme) over(Kultur) 

 

margins, at(Postmaterialisme=(1(10)100) Kultur=(0 1)) 

 

marginsplot, recast(line) ciopts(lpattern("_"))  xlabel(#10) ylabel(#3)  /// 

scheme(s2mono) graphregion(fcolor(white)) title("Marginale effekter af kultur på bekymring for miljøet") ytitle("Bekymring for miljøet") /// 

xtitle("Postmaterialisme") 

 

graph export bekymring-postmat.suffix, as(pdf) 

 

mixed Omkostninger c.Miljøparadigmet##i.Kultur c.Postmaterialisme##i.Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

margins, dydx(Miljøparadigmet) over(Kultur) 

 

margins, at(Miljøparadigmet=(1(10)100) Kultur=(0 1)) 

 

marginsplot, recast(line) ciopts(lpattern("_"))  xlabel(#10) ylabel(#3)  /// 

scheme(s2mono) graphregion(fcolor(white)) title("Marginale effekter af kultur på omkostninger for miljøet") ytitle("Omkostninger for miljøet") /// 

xtitle("Miljøparadigme") 

 

graph export ostninger-paradigme.suffix, as(pdf) 

 

margins, dydx(Postmaterialisme) over(Kultur) 

 

margins, at(Postmaterialisme=(1(10)100) Kultur=(0 1)) 

 

marginsplot, recast(line) ciopts(lpattern("_"))  xlabel(#10) ylabel(#3)  /// 

scheme(s2mono) graphregion(fcolor(white)) title("Marginale effekter af kultur på omkostninger for miljøet") ytitle("Omkostninger for miljøet") /// 

xtitle("Postmaterialisme") 

 

graph export omkostninger-postmaterialisme.suffix, as(pdf) 

 

mixed Affald c.Miljøparadigmet##i.Kultur c.Postmaterialisme##i.Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

margins, dydx(Miljøparadigmet) over(Kultur) 

 

margins, at(Miljøparadigmet=(1(10)100) Kultur=(0 1)) 

 

marginsplot, recast(line) ciopts(lpattern("_"))  xlabel(#10) ylabel(#3)  /// 

scheme(s2mono) graphregion(fcolor(white)) title("Marginale effekter af kultur på affaldssortering") ytitle("Affaldssortering") /// 

xtitle("Miljøparadigme") 

 

graph export affald-paradigme.suffix, as(pdf) 

 

margins, dydx(Postmaterialisme) over(Kultur) 

 

margins, at(Postmaterialisme=(1(10)100) Kultur=(0 1)) 

 

marginsplot, recast(line) ciopts(lpattern("_"))  xlabel(#10) ylabel(#3)  /// 

scheme(s2mono) graphregion(fcolor(white)) title("Marginale effekter af kultur på affaldssortering") ytitle("Affaldssortering") /// 

xtitle("Postmaterialisme") 

 

graph export affald-postmat.suffix, as(pdf) 

 

mixed Affald Bekymring Omkostninger c.Miljøparadigmet##i.Kultur c.Postmaterialisme##i.Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse || V4: Miljøparadigmet, mle variance cov(unstructured) 

 

margins, dydx(Miljøparadigmet) over(Kultur) 

 

margins, at(Miljøparadigmet=(1(10)100) Kultur=(0 1)) 

 

marginsplot, recast(line) ciopts(lpattern("_"))  xlabel(#10) ylabel(#3)  /// 

scheme(s2mono) graphregion(fcolor(white)) title("Marginale effekter af kultur på""affaldssortering med kontrol for holdning") ytitle("Affaldssortering med kontrol for holdning") /// 

xtitle("Miljøparadigme") 

 

graph export affaldholdning-paradigme2.suffix, as(pdf) 

 

margins, dydx(Postmaterialisme) over(Kultur) 

 

margins, at(Postmaterialisme=(1(10)100) Kultur=(0 1)) 

 

marginsplot, recast(line) ciopts(lpattern("_"))  xlabel(#10) ylabel(#3)  /// 

scheme(s2mono) graphregion(fcolor(white)) ytitle("Marginale effekter af kultur på""affaldssortering med kontrol for holdning") /// 

xtitle("Postmaterialisme") 

 

graph export affaldholdning-postmat.suffix, as(pdf) 

  

*Forudsætningstest* 

 

*Forudsætningstest* 

reg Bekymring Postmaterialisme Miljøparadigmet Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse 

 

*liniaritet 

 

acprplot Postmaterialisme, lowess 

 

acprplot Miljøparadigmet, lowess 

 

acprplot Alder, lowess 

 

acprplot Køn, lowess 

 

acprplot Uddannelse, lowess 

 

acprplot Socialklasse, lowess 

 

acprplot Bnp_pr_indbygger, lowess 

 

acprplot Social_tillid, lowess 

 

acprplot Institutionel_tillid, lowess 

 

acprplot Civil_deltagelse, lowess 

 

*Indflydelsesrige observationer* 

lvr2plot 

 

predict tempcooksd, cooksd 

display 4/e(N) 

sum tempcooksd if tempcooksd >(4/e(N)) 

 

browse if tempcooksd>(4/e(N)) & tempcooksd<. 

 

predict tempdfbeta, dfbeta(V24) 

 

sum tempdfbeta if abs(tempdfbeta)>2/sqrt(e(N)) 

 

*Normalfordelte fejlled* 

 

predict tempresid, residuals 

histogram tempresid, normal 

 

qnorm tempresid 

pnorm tempresid 

 

drop temp* 

 

*varianshomogenitet 

 

rvfplot, yline(0) 

 

rvpplot V24, yline(0) 

 

*tjek for multikolinaritet* 

 

vif 

 

reg Omkostninger Postmaterialisme Miljøparadigmet Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse 

 

*liniaritet 

 

acprplot Postmaterialisme, lowess 

 

acprplot Miljøparadigmet, lowess 

 

acprplot Alder, lowess 

 

acprplot Køn, lowess 

 

acprplot Uddannelse, lowess 

 

acprplot Socialklasse, lowess 

 

acprplot Bnp_pr_indbygger, lowess 

 

acprplot Social_tillid, lowess 

 

acprplot Institutionel_tillid, lowess 

 

acprplot Civil_deltagelse, lowess 

 

*Indflydelsesrige observationer* 

lvr2plot 

 

predict tempcooksd, cooksd 

display 4/e(N) 

sum tempcooksd if tempcooksd >(4/e(N)) 

 

browse if tempcooksd>(4/e(N)) & tempcooksd<. 

 

predict tempdfbeta, dfbeta(V24) 

 

sum tempdfbeta if abs(tempdfbeta)>2/sqrt(e(N)) 

 

*Normalfordelte fejlled* 

 

predict tempresid, residuals 

histogram tempresid, normal 

qnorm tempresid 

pnorm tempresid 

 

drop temp* 

 

*varianshomogenitet 

 

rvfplot, yline(0) 

 

rvpplot V24, yline(0) 

 

*tjek for multikolinaritet* 

 

vif 

 

reg Affald Postmaterialisme Miljøparadigmet Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse 

 

*liniaritet 

 

acprplot Postmaterialisme, lowess 

 

acprplot Miljøparadigmet, lowess 

 

acprplot Alder, lowess 

 

acprplot Køn, lowess 

 

acprplot Uddannelse, lowess 

 

acprplot Socialklasse, lowess 

 

acprplot Bnp_pr_indbygger, lowess 

 

acprplot Social_tillid, lowess 

 

acprplot Institutionel_tillid, lowess 

 

acprplot Civil_deltagelse, lowess 

 

*Indflydelsesrige observationer* 

lvr2plot 

 

predict tempcooksd, cooksd 

display 4/e(N) 

sum tempcooksd if tempcooksd >(4/e(N)) 

 

browse if tempcooksd>(4/e(N)) & tempcooksd<. 

 

predict tempdfbeta, dfbeta(V24) 

 

sum tempdfbeta if abs(tempdfbeta)>2/sqrt(e(N)) 

 

*Normalfordelte fejlled* 

 

predict tempresid, residuals 

histogram tempresid, normal 

 

qnorm tempresid 

pnorm tempresid 

 

drop temp* 

 

*varianshomogenitet 

 

rvfplot, yline(0) 

 

rvpplot V24, yline(0) 

 

*tjek for multikolinaritet* 

 

vif 

 

reg Bilkørsel Postmaterialisme Miljøparadigmet Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse 

 

*liniaritet 

 

acprplot Postmaterialisme, lowess 

 

acprplot Miljøparadigmet, lowess 

 

acprplot Alder, lowess 

 

acprplot Køn, lowess 

 

acprplot Uddannelse, lowess 

 

acprplot Socialklasse, lowess 

 

acprplot Bnp_pr_indbygger, lowess 

 

acprplot Social_tillid, lowess 

 

acprplot Institutionel_tillid, lowess 

 

acprplot Civil_deltagelse, lowess 

 

*Indflydelsesrige observationer* 

lvr2plot 

 

predict tempcooksd, cooksd 

display 4/e(N) 

sum tempcooksd if tempcooksd >(4/e(N)) 

 

browse if tempcooksd>(4/e(N)) & tempcooksd<. 

 

predict tempdfbeta, dfbeta(V24) 

 

sum tempdfbeta if abs(tempdfbeta)>2/sqrt(e(N)) 

 

*Normalfordelte fejlled* 

 

predict tempresid, residuals 

histogram tempresid, normal 

 

qnorm tempresid 

pnorm tempresid 

 

drop temp* 

 

*varianshomogenitet 

 

rvfplot, yline(0) 

 

rvpplot V24, yline(0) 

 

*tjek for multikolinaritet* 

 

vif 

 

reg Afaldssortering Postmaterialisme Miljøparadigmet Kultur Bekymring Omkostninger Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse 

 

*liniaritet 

 

acprplot Postmaterialisme, lowess 

acprplot Miljøparadigmet, lowess 

 

acprplot Alder, lowess 

 

acprplot Køn, lowess 

 

acprplot Uddannelse, lowess 

 

acprplot Socialklasse, lowess 

 

acprplot Bnp_pr_indbygger, lowess 

 

acprplot Social_tillid, lowess 

 

acprplot Institutionel_tillid, lowess 

 

acprplot Civil_deltagelse, lowess 

 

*Indflydelsesrige observationer* 

lvr2plot 

 

predict tempcooksd, cooksd 

display 4/e(N) 

sum tempcooksd if tempcooksd >(4/e(N)) 

 

browse if tempcooksd>(4/e(N)) & tempcooksd<. 

 

predict tempdfbeta, dfbeta(V24) 

 

sum tempdfbeta if abs(tempdfbeta)>2/sqrt(e(N)) 

 

*Normalfordelte fejlled* 

 

predict tempresid, residuals 

histogram tempresid, normal 

 

qnorm tempresid 

pnorm tempresid 

 

drop temp* 

 

*varianshomogenitet 

 

rvfplot, yline(0) 

 

rvpplot V24, yline(0) 

 

*tjek for multikolinaritet* 

 

vif 

 

reg Bilkørsel Postmaterialisme Miljøparadigmet Kultur Bekymring Omkostninger Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse 

 

*liniaritet 

 

acprplot Postmaterialisme, lowess 

 

acprplot Miljøparadigmet, lowess 

 

acprplot Alder, lowess 

 

acprplot Køn, lowess 

 

acprplot Uddannelse, lowess 

 

acprplot Socialklasse, lowess 

 

acprplot Bnp_pr_indbygger, lowess 

 

acprplot Social_tillid, lowess 

 

acprplot Institutionel_tillid, lowess 

 

acprplot Civil_deltagelse, lowess 

 

*Indflydelsesrige observationer* 

lvr2plot 

 

predict tempcooksd, cooksd 

display 4/e(N) 

sum tempcooksd if tempcooksd >(4/e(N)) 

 

browse if tempcooksd>(4/e(N)) & tempcooksd<. 

 

predict tempdfbeta, dfbeta(V24) 

 

sum tempdfbeta if abs(tempdfbeta)>2/sqrt(e(N)) 

 

*Normalfordelte fejlled* 

 

predict tempresid, residuals 

histogram tempresid, normal 

 

qnorm tempresid 

pnorm tempresid 

 

drop temp* 

 

*varianshomogenitet 

 

rvfplot, yline(0) 

 

rvpplot V24, yline(0) 

 

*tjek for multikolinaritet* 

 

vif 

 

reg Bekymring Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse 

 

*liniaritet 

 

acprplot Postmaterialisme, lowess 

 

acprplot Miljøparadigmet, lowess 

 

acprplot Alder, lowess 

 

acprplot Køn, lowess 

 

acprplot Uddannelse, lowess 

 

acprplot Socialklasse, lowess 

 

acprplot Bnp_pr_indbygger, lowess 

 

acprplot Social_tillid, lowess 

 

acprplot Institutionel_tillid, lowess 

 

acprplot Civil_deltagelse, lowess 

 

 

 

*Indflydelsesrige observationer* 

lvr2plot 

 

predict tempcooksd, cooksd 

display 4/e(N) 

sum tempcooksd if tempcooksd >(4/e(N)) 

 

browse if tempcooksd>(4/e(N)) & tempcooksd<. 

 

predict tempdfbeta, dfbeta(V24) 

 

sum tempdfbeta if abs(tempdfbeta)>2/sqrt(e(N)) 

 

*Normalfordelte fejlled* 

 

predict tempresid, residuals 

histogram tempresid, normal 

 

qnorm tempresid 

pnorm tempresid 

 

drop temp* 

 

*varianshomogenitet 

 

rvfplot, yline(0) 

 

rvpplot V24, yline(0) 

 

*tjek for multikolinaritet* 

 

vif 

 

reg Omkostninger Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse 

 

*liniaritet 

 

acprplot Postmaterialisme, lowess 

 

acprplot Miljøparadigmet, lowess 

 

acprplot Alder, lowess 

 

acprplot Køn, lowess 

 

acprplot Uddannelse, lowess 

 

acprplot Socialklasse, lowess 

 

acprplot Bnp_pr_indbygger, lowess 

 

acprplot Social_tillid, lowess 

 

acprplot Institutionel_tillid, lowess 

 

acprplot Civil_deltagelse, lowess 

 

*Indflydelsesrige observationer* 

lvr2plot 

 

predict tempcooksd, cooksd 

display 4/e(N) 

sum tempcooksd if tempcooksd >(4/e(N)) 

 

browse if tempcooksd>(4/e(N)) & tempcooksd<. 

 

predict tempdfbeta, dfbeta(V24) 

 

sum tempdfbeta if abs(tempdfbeta)>2/sqrt(e(N)) 

 

*Normalfordelte fejlled* 

 

predict tempresid, residuals 

histogram tempresid, normal 

 

qnorm tempresid 

pnorm tempresid 

 

drop temp* 

 

*varianshomogenitet 

 

rvfplot, yline(0) 

 

rvpplot V24, yline(0) 

 

*tjek for multikolinaritet* 

 

vif 

 

reg Affald Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse 

 

*liniaritet 

 

acprplot Postmaterialisme, lowess 

 

acprplot Miljøparadigmet, lowess 

 

acprplot Alder, lowess 

 

acprplot Køn, lowess 

 

acprplot Uddannelse, lowess 

 

acprplot Socialklasse, lowess 

 

acprplot Bnp_pr_indbygger, lowess 

 

acprplot Social_tillid, lowess 

 

acprplot Institutionel_tillid, lowess 

 

acprplot Civil_deltagelse, lowess 

 

*Indflydelsesrige observationer* 

lvr2plot 

 

predict tempcooksd, cooksd 

display 4/e(N) 

sum tempcooksd if tempcooksd >(4/e(N)) 

 

browse if tempcooksd>(4/e(N)) & tempcooksd<. 

 

predict tempdfbeta, dfbeta(V24) 

 

sum tempdfbeta if abs(tempdfbeta)>2/sqrt(e(N)) 

 

*Normalfordelte fejlled* 

 

predict tempresid, residuals 

histogram tempresid, normal 

 

qnorm tempresid 

pnorm tempresid 

drop temp* 

 

*varianshomogenitet 

 

rvfplot, yline(0) 

 

rvpplot V24, yline(0) 

 

*tjek for multikolinaritet* 

 

vif 

 

reg Bilkørsel Postmaterialisme Miljøparadigmet Kultur Alder Køn Uddannelse Socialklasse BNP_pr_indbygger Social_tillid Institutionel_tillid Civil_deltagelse 

 

*liniaritet 

 

acprplot Postmaterialisme, lowess 

 

acprplot Miljøparadigmet, lowess 

 

acprplot Alder, lowess 

 

acprplot Køn, lowess 

 

acprplot Uddannelse, lowess 

 

acprplot Socialklasse, lowess 

 

acprplot Bnp_pr_indbygger, lowess 

 

acprplot Social_tillid, lowess 

 

acprplot Institutionel_tillid, lowess 

 

acprplot Civil_deltagelse, lowess 

 

*Indflydelsesrige observationer* 

lvr2plot 

 

predict tempcooksd, cooksd 

display 4/e(N) 

sum tempcooksd if tempcooksd >(4/e(N)) 

 

browse if tempcooksd>(4/e(N)) & tempcooksd<. 

 

predict tempdfbeta, dfbeta(V24) 

 

sum tempdfbeta if abs(tempdfbeta)>2/sqrt(e(N)) 

 

*Normalfordelte fejlled* 

 

predict tempresid, residuals 

histogram tempresid, normal 

 

qnorm tempresid 

pnorm tempresid 

 

drop temp* 

 

*varianshomogenitet 

 

rvfplot, yline(0) 

 

rvpplot V24, yline(0) 

 

*tjek for multikolinaritet* 

vif 