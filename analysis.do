*변수 생성
use 2018_HIES.dta
rename v2 quarter
rename v3 hid
rename v1 year
rename v7 seniorhh
rename v98 ownhouse
rename v102 laborinc
rename v106 bussinc
rename v111 assetinc
*2017-19
rename v117 pensionv
rename v118 basicv
rename v119 benefit
rename v121 taxreturn
rename v127 tax
rename v129 pensioncont
rename v130 socinsurance

*2012-2016
rename v118 pensionv
rename v119 basicv
rename v120 benefit
rename v122 taxreturn
rename v123 hhtrans
rename v124 discount
rename v125 othertrans
rename v248 tax
rename v250 pensioncont
rename v251 socinsurance

*2010-2011
rename v116 pensionv
rename v117 basicv
rename v118 benefit
rename v120 taxreturn
rename v121 hhtrans
rename v122 discount
rename v123 othertrans
rename v246 tax
rename v248 pensioncont
rename v249 socinsurance

*시장소득 = 근로소득 + 사업소득 + 재산소득
*공적이전소득 = 공적연금 + 기초연금 + 사회수혜금 + 세금환급금
*사적이전소득 = 가구간이전 + 할인혜택 + 기타이전
*총소득 = 시장소득+공적이전+사적이전
*가처분소득=총소득-경상조세-사회보험료-연금기여금
*균등화가처분소득 = 가처분소득/균등화지수
rename v122 privtrans

gen marketinc = laborinc+bussinc+assetinc
gen publictrans=pensionv+basicv+benefit+taxreturn
gen privtrans= hhtrans + discount + othertrans
gen grossinc=marketinc+publictrans+privtrans
gen disposinc=grossinc-tax-pensioncont-socinsurance 
rename v5 hhno
gen eqscale = sqrt(hhno)
gen eqdisposinc= disposinc/eqscale
recode basicv (1/max=1), gen(basic)
tab basic
recode pensionv (1/max=1), gen(pension)
tab pension

*가구원 취업여부
tab v6
rename v6 empno
recode empno (1/6=1), gen(hhemp)
tab hhemp

*배우자 유무
tab v12
recode v12 (1/2=1) (3=0), gen(marital)
tab marital

rename v13 rel1
rename v22 rel2
rename v31 rel3
rename v40 rel4
rename v49 rel5
rename v58 rel6
rename v67 rel7
rename v76 rel8 // 가구주와의 관계
rename v14 sex1
rename v23 sex2
rename v32 sex3
rename v41 sex4
rename v50 sex5
rename v59 sex6
rename v68 sex7
rename v77 sex8 // 성별
rename v15 age1
rename v24 age2
rename v33 age3
rename v42 age4
rename v51 age5
rename v60 age6
rename v69 age7
rename v78 age8 // 나이
rename v16 edu1
rename v25 edu2
rename v34 edu3
rename v43 edu4
rename v52 edu5
rename v61 edu6
rename v70 edu7
rename v79 edu8 // 교육수준
rename v17 pedu1
rename v26 pedu2
rename v35 pedu3
rename v44 pedu4
rename v53 pedu5
rename v62 pedu6
rename v71 pedu7
rename v80 pedu8 // 졸업, 재학여부
rename v18 emp1
rename v27 emp2
rename v36 emp3
rename v45 emp4
rename v54 emp5
rename v63 emp6
rename v72 emp7
rename v81 emp8  // 취업여부
rename v99 weight // 가구가중치
save HIES_2018_master.dta

*노인did샘플 생성
destring quarter, replace
keep if quarter==14
reshape long rel sex age edu pedu emp, i(hid) j(individual)
drop if rel==. // 1분기 개인별데이터 구성
egen sen65no = total(age>=65), by(hid)
tab sen65no // 65세 이상 노인가구원 수
egen sen60no = total(age>=60), by(hid)
tab sen60no // 60세 이상 노인가구원 수
egen sen55no = total(age>=55), by(hid)
tab sen55no // 55세 이상 노인가구원 수

egen no2059 = total (age>19 & age<60), by(hid)
tab no2059
replace no2059=1 if no2059>=1// 20-59세 가구원 여부

*poverty1,2,3 = 중위소득의 50, 40, 30%
sum eqdisposinc [aw=weight], detail
recode eqdisposinc (min/1019706.5=1) (1019706.5/max=0), gen(poverty1)
recode eqdisposinc (min/815765.2=1) (815765.2/max=0), gen(poverty2)
recode eqdisposinc (min/611823.9=1) (611823.9/max=0), gen(poverty3)
tab poverty1
tab poverty2
tab poverty3
save 2018_HIES_indiv_14, replace
*같은 작업을 2사분기-4사분기 반복

use 2018_HIES_indiv_14.dta
append using 2018_HIES_indiv_24.dta
append using 2018_HIES_indiv_34.dta
append using 2018_HIES_indiv_44.dta
save 2018_HIES_indiv.dta, replace

keep if age>=55
tab edu pedu
gen edu1=.
replace edu1=0 if edu==0 | (edu==1 & (pedu==2 | pedu==3))
replace edu1=1 if (edu==1 & pedu==1) | edu==2 & (pedu==2 | pedu==3)
replace edu1=2 if (edu==2 & pedu==1) | (edu==3 & (pedu==2 | pedu==3))
replace edu1=3 if (edu==3 & pedu==1) | (edu==4 & (pedu==2 | pedu==3)) | (edu==5 & (pedu==2 | pedu==3))
label define 교육수준 0 "무학" 1 "초등학교" 2 "중학교" 3 "고등학교"
label values edu1 교육수준 // 고졸이하 교육수준 변수 생성

recode quarter (24=0) (44=1) (nonmissing=.), gen(period1) // pre-post 변수 생성
recode age (55/64=0) (65/max=1), gen(treat) // treatment-control 변수 생성
replace treat=. if treat==0 & basic==1 & sen65no>0 // control group에서 배우자가 65세 이상이여서 기초연금 수급하는 경우 제외

tab hhno sen60no
gen old1=0
replace old1=1 if hhno==1 // 60세 이상 노인단독가구 생성
tab old1
gen old2=0
replace old2=1 if hhno==2 & sen60no==2 // 60세이상 노인 부부가구 생성
tab old2
save 2018_HIES_didsample

tab hhno sen65no
gen old1=0
replace old1=1 if hhno==1
gen old2=0
replace old2=1 if hhno==2 & sen65no==2

*데이터 연도별 합치기
destring year, replace
destring hid, replace
destring seniorhh, replace
destring sex, replace
destring ownhouse, replace
destring emp, replace
append using "C:\Users\82102\Desktop\기초연금 연구\Data\2018\2018_HIES_didsample.dta", force

*treat/period/yr 변수 만들기
egen time=group(year quarter)
gen treat=.
replace treat=1 if age>=65
replace treat=0 if age<65
replace treat=. if treat==0 & sen65no>0 & basic==1
tab treat
gen period1=.
replace period1=1 if quarter==34
replace period1=0 if quarter==24
tab period1
tab quarter
gen period2=.
replace period2=1 if quarter==44
replace period2=0 if quarter==24
tab period2
gen period3=0
replace period3=1 if quarter==34 | quarter==44
tab period3
gen period4=0
replace period4=1 if quarter==44
replace period4=. if quarter==34
tab period4
gen yr14=0
replace yr14=1 if year==2014
replace yr14=. if year==2018
tab yr14
gen yr18=0
replace yr18=1 if year==2018
replace yr18=. if year==2014
tab yr18
tab year
recode emp (2=0), gen(employment)

*Descriptive data
tab treat after if edu1<4 & year==0 [aw=weight], sum(basic)
tab treat after if edu1<4 & year==1 [aw=weight], sum(basic)
tab treat after if edu1<4 & year==0 [aw=weight], sum(v118)
tab treat after if edu1<4 & year==1 [aw=weight], sum(v118)
tab treat after if edu1<4 & year==0 & basic>0 [aw=weight], sum(v118)
tab treat after if edu1<4 & year==1 & basic>0 [aw=weight], sum(v118)
tab treat after if edu1<4 & year==1 [aw=weight], sum(v117)
tab treat after if edu1<4 & year==0 [aw=weight], sum(v117)
tab treat after if edu1<4 & year==0 [aw=weight], sum(v119)
tab treat after if edu1<4 & year==1 [aw=weight], sum(v119)
tab treat after if edu1<4 & year==0 [aw=weight], sum(v121)
tab treat after if edu1<4 & year==1 [aw=weight], sum(v121)
tab treat after if edu1<4 & year==1 [aw=weight], sum(publictrans)
tab treat after if edu1<4 & year==0 [aw=weight], sum(publictrans)
tab treat after if edu1<4 & year==0 [aw=weight], sum(privtrans)
tab treat after if edu1<4 & year==0 [aw=weight], sum(v106)
tab treat after if edu1<4 & year==1 [aw=weight], sum(v106)
tab treat after if edu1<4 & year==1 [aw=weight], sum(v102)
tab treat after if edu1<4 & year==0 [aw=weight], sum(v102)
tab treat after if edu1<4 & year==0 [aw=weight], sum(v111)
tab treat after if edu1<4 & year==1 [aw=weight], sum(v111)
tab treat after if edu1<4 & year==0 [aw=weight], sum(marketinc)
tab treat after if edu1<4 & year==1 [aw=weight], sum(marketinc)
tab treat after if edu1<4 & year==1 [aw=weight], sum(privtrans)
tab treat after if edu1<4 & year==1 [aw=weight], sum(grossinc)
tab treat after if edu1<4 & year==0 [aw=weight], sum(grossinc)
tab treat after if edu1<4 & year==0 [aw=weight], sum(disposinc)
tab treat after if edu1<4 & year==1 [aw=weight], sum(disposinc)
tab treat after if edu1<4 & year==1 [aw=weight], sum(poverty)
tab treat after if edu1<4 & year==0 [aw=weight], sum(poverty)

preserve
collapse(mean) employment marketinc publictrans privtrans grossinc disposinc poverty1 poverty2 poverty3 if treat==1 & edu1<4 [aw=weight], by(time)
restore
preserve
collapse(mean) employment marketinc publictrans privtrans grossinc disposinc poverty1 poverty2 poverty3 if treat==0 & edu1<4 [aw=weight], by(time)
restore

*논문에 들어갈 표만들기
eststo: reg eqdisposinc treat##period3##yr11 if edu1<4 [pw=weight], vce(cluster hid)
eststo: reg eqdisposinc treat##period3##yr12 if edu1<4 [pw=weight], vce(cluster hid)
eststo: reg eqdisposinc treat##period3##yr13 if edu1<4 [pw=weight], vce(cluster hid)
eststo: reg eqdisposinc period3##yr11 if treat==1 & edu1<4 [pw=weight], vce(cluster hid)
eststo: reg eqdisposinc period3##yr12 if treat==1 & edu1<4 [pw=weight], vce(cluster hid)
eststo: reg eqdisposinc period3##yr13 if treat==1 & edu1<4 [pw=weight], vce(cluster hid)
esttab using simpledid.rtf, t r2 replace
eststo clear
eststo:reg employment treat##period3##yr14 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg marketinc treat##period3##yr14 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg publictrans treat##period3##yr14 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg privtrans treat##period3##yr14 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg eqdisposinc treat##period3##yr14 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg poverty1 treat##period3##yr14 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg poverty2 treat##period3##yr14 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg poverty3 treat##period3##yr14 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
esttab using main14.rtf, t r2
eststo clear
eststo:reg employment period3##yr14 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg marketinc period3##yr14 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg publictrans period3##yr14 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg privtrans period3##yr14 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg eqdisposinc period3##yr14 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg poverty1 period3##yr14 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg poverty2 period3##yr14 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg poverty3 period3##yr14 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
esttab using DiD14.rtf, t r2
eststo clear
eststo:reg employment treat##period4##yr18 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg marketinc treat##period4##yr18 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg publictrans treat##period4##yr18 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg privtrans treat##period4##yr18 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg eqdisposinc treat##period4##yr18 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg poverty1 treat##period4##yr18 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg poverty2 treat##period4##yr18 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:reg poverty3 treat##period4##yr18 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
esttab using DDD18.rtf, t r2
eststo:reg employment period4##yr18 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo clear
eststo:reg employment period4##yr18 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg marketinc period4##yr18 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg publictrans period4##yr18 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg privtrans period4##yr18 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg eqdisposinc period4##yr18 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg poverty1 period4##yr18 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg poverty2 period4##yr18 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:reg poverty3 period4##yr18 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
esttab using DiD18.rtf, t r2
eststo clear
eststo:logit employment treat##period3##yr14 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:logit poverty1 treat##period3##yr14 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:logit poverty2 treat##period3##yr14 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:logit poverty3 treat##period3##yr14 c.age##c.age i.sex i.edu1 hhno [pw=weight], vce(cluster hid)
eststo:logit employment period3##yr14 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:logit poverty1 period3##yr14 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:logit poverty2 period3##yr14 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
eststo:logit poverty3 period3##yr14 c.age##c.age i.sex i.edu1 hhno if treat==1 [pw=weight], vce(cluster hid)
esttab using logit14.rtf, z r2
eststo clear



eststo:reg grossinc treat##period3##yr14 if edu1<4 [pw=weight]
eststo:reg grossinc treat##period3##yr14 c.age##c.age i.sex i.edu1 ownhouse [pw=weight]
eststo:reg marketinc treat##period3##yr14 if edu1<4 [pw=weight]
eststo:reg marketinc treat##period3##yr14 c.age##c.age i.sex i.edu1 ownhouse [pw=weight]
eststo:reg publictrans treat##period3##yr14 if edu1<4 [pw=weight]
eststo:reg publictrans treat##period3##yr14 c.age##c.age i.sex i.edu1 ownhouse [pw=weight]
esttab using table.rtf, t r2 replace
eststo clear
eststo:reg privtrans treat##period3##yr14 if edu1<4 [pw=weight]
eststo:reg privtrans treat##period3##yr14 c.age##c.age i.sex i.edu1 ownhouse [pw=weight]
eststo:reg employment treat##period3##yr14 c.age##c.age i.sex i.edu1 ownhouse [pw=weight]
eststo:reg poverty1 treat##period3##yr14 c.age##c.age i.sex i.edu1 ownhouse [pw=weight]
eststo:reg poverty2 treat##period3##yr14 c.age##c.age i.sex i.edu1 ownhouse [pw=weight]
eststo:reg poverty3 treat##period3##yr14 c.age##c.age i.sex i.edu1 ownhouse [pw=weight]
esttab using table.rtf, t r2 replace

* DiD 모형 및 표만들기
eststo: reg grossinc treat1##after if edu1<4 [aw=weight]
eststo:reg grossinc treat1##after c.age##c.age i.sex i.edu1 pension hhemp marital no2059 [aw=weight]
eststo: reg disposinc treat1##after if edu1<4 [aw=weight]
eststo:reg disposinc treat1##after c.age##c.age i.sex i.edu1 pension hhemp marital no2059 [aw=weight]
eststo: reg poverty treat1##after if edu1<4 [aw=weight]
eststo:reg poverty treat1##after c.age##c.age i.sex i.edu1 pension hhemp marital no2059 [aw=weight]
esttab using table.rtf, se r2
eststo clear
eststo: reg grossinc treat##after##year if edu1<4 [aw=weight]
eststo:reg grossinc treat##after##year c.age##c.age i.sex i.edu1 pension hhemp marital no2059 [aw=weight]
eststo: reg disposinc treat##after##year if edu1<4 [aw=weight]
eststo:reg disposinc treat##after##year c.age##c.age i.sex i.edu1 pension hhemp marital no2059 [aw=weight]
eststo: reg poverty treat##after##year if edu1<4 [aw=weight]
eststo:reg poverty treat##after##year c.age##c.age i.sex i.edu1 pension hhemp marital no2059 [aw=weight]
esttab using table.rtf, se r2 replace
eststo clear
eststo: reg marketinc treat##after##year if edu1<4 [aw=weight]
eststo:reg marketinc treat##after##year c.age##c.age i.sex i.edu1 pension hhemp marital no2059 [aw=weight]
eststo: reg publictrans treat##after##year if edu1<4 [aw=weight]
eststo:reg publictrans treat##after##year c.age##c.age i.sex i.edu1 pension hhemp marital no2059 [aw=weight]
eststo: reg privtrans treat##after##year if edu1<4 [aw=weight]
eststo:reg privtrans treat##after##year c.age##c.age i.sex i.edu1 pension hhemp marital no2059 [aw=weight]
esttab using table.rtf, se r2 replace

preserve
keep if v15>=65
collapse(mean) publictrans v118 dispoinc, by(v2)
graph twoway (connected publictrans v2)
graph twoway (connected v118 v2)
graph twoway (connected dispoinc v2)
restore

preserve
collapse(mean) basic [aw=v99], by(age)
graph twoway connected (basic age) if age>=60 & age<75
restore

preserve
collapse(mean) v118 if age>=65 & basic>0 & senior==1 [aw=v99], by(v2)
graph twoway bar (v118 v2), yscale(range (190000 250000))
restore

*hid 내에서 조건에 맞는 사람 수 카운트
egen workage= total(age>=20 & age<60), by(hid)
*개별 관측치 바꾸기
replace workage=1 if workage>=1
egen nonsenior= total(age<62), by(hid)
replace nonsenior=1 if nonsenior>=1



* 11-13 December
gen post14=.
replace post14=0 if time>=5 & time<19
replace post14=1 if time>=19 & time<35
gen post18=.
replace post18=0 if time>=19 & time<35
replace post18=1 if time>35
gen post141=.
replace post141=0 if time>=5 & time<19
replace post141=1 if time>=19 & time<23
replace post141=2 if time>=23 & time<27
replace post141=3 if time>=27 & time<31
replace post141=4 if time>=31 & time<35
gen post181=.
replace post181=0 if time>=19 & time<35
replace post181=1 if time>=36 & time<38
replace post181=2 if time>=38
gen non65no=hhno-sen65no

*Preliminary results
eststo: reg poverty1 treat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 treat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty1 treat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 treat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using prelim.rtf, t r2
eststo clear
eststo: reg employment treat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg marketinc treat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg publictrans treat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg privtrans treat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using prelim14_1213.rtf, t r2
eststo clear
eststo: reg employment treat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg marketinc treat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg publictrans treat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg privtrans treat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using prelim18_1213.rtf, t r2
eststo clear
*robustness checks for prelim results
eststo: reg poverty1 ptreat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg publictrans ptreat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty1 ptreat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg publictrans ptreat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using prerobust.rtf, t r2
eststo clear

*results:2014 reform
eststo: reg poverty1 treat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty1 treat##post14 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty1 treat##post141 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 treat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 treat##post14 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 treat##post141 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using result141.rtf, t r2 replace
eststo clear
eststo: reg employment treat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg employment treat##post14 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg employment treat##post141 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqmarketinc treat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqmarketinc treat##post14 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqmarketinc treat##post141 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using result142.rtf, t r2 replace
eststo clear
eststo: reg eqpublictrans treat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqpublictrans treat##post14 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqpublictrans treat##post141 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqprivtrans treat##post14 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqprivtrans treat##post14 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqprivtrans treat##post141 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using result143.rtf, t r2 replace
eststo clear

*Results: 2018 Reform
eststo: reg poverty1 treat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty1 treat##post18 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty1 treat##post181 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 treat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 treat##post18 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 treat##post181 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using result181.rtf, t r2 replace
eststo clear
eststo: reg employment treat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg employment treat##post18 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg employment treat##post181 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqmarketinc treat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqmarketinc treat##post18 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqmarketinc treat##post181 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using result182.rtf, t r2 replace
eststo clear
eststo: reg eqpublictrans treat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqpublictrans treat##post18 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqpublictrans treat##post181 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqprivtrans treat##post18 c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqprivtrans treat##post18 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqprivtrans treat##post181 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using result183.rtf, t r2 replace
eststo clear

*DDD 2014/2018
eststo: reg poverty1 treat##period3##ref13 c.age##c.age i.sex i.edu1 non65no sen65no i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 treat##period3##ref13 c.age##c.age i.sex i.edu1 non65no sen65no i.quarter [pw=weight], vce(cluster hid)
eststo: reg employment treat##period3##ref13 c.age##c.age i.sex i.edu1 non65no sen65no i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqmarketinc treat##period3##ref13 c.age##c.age i.sex i.edu1 non65no sen65no i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqpublictrans treat##period3##ref13 c.age##c.age i.sex i.edu1 non65no sen65no i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqprivtrans treat##period3##ref13 c.age##c.age i.sex i.edu1 non65no sen65no i.quarter [pw=weight], vce(cluster hid)
esttab using robust14.rtf, t r2 replace
eststo clear
eststo: reg poverty1 treat##period4##ref17 c.age##c.age i.sex i.edu1 non65no sen65no i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 treat##period4##ref17 c.age##c.age i.sex i.edu1 non65no sen65no i.quarter [pw=weight], vce(cluster hid)
eststo: reg employment treat##period4##ref17 c.age##c.age i.sex i.edu1 non65no sen65no i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqmarketinc treat##period4##ref17 c.age##c.age i.sex i.edu1 non65no sen65no i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqpublictrans treat##period4##ref17 c.age##c.age i.sex i.edu1 non65no sen65no i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqprivtrans treat##period4##ref17 c.age##c.age i.sex i.edu1 non65no sen65no i.quarter [pw=weight], vce(cluster hid)
esttab using robust18.rtf, t r2 replace
eststo clear

* Non-linear time trends
reg poverty1 treat##post14 treat##c.time##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
reg poverty3 treat##post14 treat##c.time##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
reg employment treat##post14 treat##c.time##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
reg eqmarketinc treat##post14 treat##c.time##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
reg eqpublictrans treat##post14 treat##c.time##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
reg eqprivtrans treat##post14 treat##c.time##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
reg poverty1 treat##post18 treat##c.time##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
reg poverty3 treat##post18 treat##c.time##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
reg employment treat##post18 treat##c.time##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
reg eqmarketinc treat##post18 treat##c.time##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
reg eqpublictrans treat##post18 treat##c.time##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
reg eqprivtrans treat##post18 treat##c.time##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)

*Pseudo-treatment
eststo: reg poverty1 ptreat##post14 ptreat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 ptreat##post14 ptreat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg employment ptreat##post14 ptreat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqmarketinc ptreat##post14 ptreat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqpublictrans ptreat##post14 ptreat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqprivtrans ptreat##post14 ptreat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using ptreat14.rtf, t r2 replace
eststo clear
eststo: reg poverty1 ptreat##post18 ptreat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 ptreat##post18 ptreat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg employment ptreat##post18 ptreat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqmarketinc ptreat##post18 ptreat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqpublictrans ptreat##post18 ptreat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqprivtrans ptreat##post18 ptreat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using ptreat18.rtf, t r2 replace
eststo clear

*False Treatment Period
eststo: reg poverty1 treat##fpost1 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 treat##fpost1 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg employment treat##fpost1 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqmarketinc treat##fpost1 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqpublictrans treat##fpost1 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqprivtrans treat##fpost1 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using fpost14.rtf, t r2 replace
eststo clear
eststo: reg poverty1 treat##fpost2 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg poverty3 treat##fpost2 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg employment treat##fpost2 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqmarketinc treat##fpost2 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqpublictrans treat##fpost2 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
eststo: reg eqprivtrans treat##fpost2 treat##c.time c.age##c.age i.sex i.edu1 non65no sen65no i.year i.quarter [pw=weight], vce(cluster hid)
esttab using fpost2.rtf, t r2 replace
eststo clear

*graph
egen mpoverty1=mean(poverty1), by(time treat)
egen mpoverty3=mean(poverty3), by(time treat)
egen memployment=mean(employment), by(time treat)
egen meqmarketinc=mean(eqmarketinc), by(time treat)
egen meqpublictrans=mean(eqpublictrans), by(time treat)
egen meqprivtrans=mean(eqprivtrans), by(time treat)
twoway (line mpoverty1 time if treat==1 & time>4, sort(time)) (line mpoverty1 time if treat==0 & time>4, sort(time))
twoway (line mpoverty3 time if treat==1 & time>4, sort(time)) (line mpoverty3 time if treat==0 & time>4, sort(time))
twoway (line memployment time if treat==1 & time>4, sort(time)) (line memployment time if treat==0 & time>4, sort(time))
twoway (line meqmarketinc time if treat==1 & time>4, sort(time)) (line meqmarketinc time if treat==0 & time>4, sort(time))
twoway (line meqpublictrans time if treat==1 & time>4, sort(time)) (line meqpublictrans time if treat==0 & time>4, sort(time))
twoway (line meqprivtrans time if treat==1 & time>4, sort(time)) (line meqprivtrans time if treat==0 & time>4, sort(time))