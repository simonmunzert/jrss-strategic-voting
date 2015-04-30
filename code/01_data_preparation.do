/*  Data sources for constituency preferences in English contstituencies 
	in the 1997 and 2001 British General Elections  */

set more off

*** 1996 *** 	British Social Attitudes Survey 
use "g921au.dta", clear
drop if region==1 | region==4 /* Scotland & Wales */
replace stregion = stregion - 1
gen vote = 0
replace vote = 1 if votenow < 8
/* partyid1 also captures weak identifiers measured with a hypothetical election question and is possibly confounded by strategic voting decisions */
/* partyid2 only captures identifiers, supporters, and people who see themselves "closer" to a party than to another */
recode partyid1 (8=4) (9=4) (95=4) /* identification with other parties == 4 */
recode partyid1 (10=9) (98=9) (99=9) /* identification with no party == 9 */
replace partyid2 = partyid1 
replace partyid2 = 9 if closepty ==2
recode confeel5 labfeel5 ldfeel5 (8/9=.)
replace partyid2 = 1 if partyid2==9 & confeel5<labfeel5 & confeel5<ldfeel5
replace partyid2 = 2 if partyid2==9 & labfeel5<confeel5 & labfeel5<ldfeel5
replace partyid2 = 3 if partyid2==9 & ldfeel5<confeel5 & ldfeel5<labfeel5
replace partyid1 = partyid2 if partyid1==9 & partyid2!=9
gen partyid3 = 12 if partyid2 == 9 & confeel5==labfeel5 & ldfeel5>confeel5
replace partyid3 = 13 if partyid2 == 9 & confeel5==ldfeel5 & labfeel5>confeel5
replace partyid3 = 23 if partyid2 == 9 & labfeel5==ldfeel5 & confeel5>labfeel5
replace pano = panoo95 if panoo95~=. 
replace pano = panooth if panooth~=. & panoo95==.
/*  above: the preceding two lines replace constituencies resp. was sampled in with 
	constituencies resp. is actually registered in, if the two are not the same. 
	note: the CSES (see below 1997) identifies constituencies by the press 
	association constituency number, i.e., by the variable "pano"  */
recode futurvot (6/7=4) (8/98=9), gen(partyvote) /* Party vote; 4 == other, 8 = don't know, 9 = no vote, NA */
recode pastvot (6/7=4) (8/98=9), gen(partyvote_prev)
gen tacticalvote = .
gen tacticalvotefor = .
gen tacticalvote2 = .
replace tacticalvote2 = 0 if partyvote == partyid2
replace tacticalvote2 = 1 if partyvote != partyid2 & partyvote!= 9
gen thermo1pref = 1 if confeel5<labfeel5 & confeel5<ldfeel5
replace thermo1pref = 2 if labfeel5<confeel5 & labfeel5<ldfeel5
replace thermo1pref = 3 if ldfeel5<confeel5 & ldfeel5<labfeel5
gen thermo2pref = 1 if (confeel5<labfeel5 & confeel5>ldfeel5) | (confeel5>labfeel5 & confeel5<ldfeel5)
replace thermo2pref = 2 if (labfeel5<confeel5 & labfeel5>ldfeel5) | (labfeel5>confeel5 & labfeel5<ldfeel5)
replace thermo2pref = 3 if (ldfeel5<confeel5 & ldfeel5>labfeel5) | (ldfeel5>confeel5 & ldfeel5<labfeel5)
gen thermo3pref = 1 if confeel5>labfeel5 & confeel5>ldfeel5
replace thermo3pref = 2 if labfeel5>confeel5 & labfeel5>ldfeel5
replace thermo3pref = 3 if ldfeel5>confeel5 & ldfeel5>labfeel5
keep stregion pano wtfactor vote partyvote partyvote_prev partyid1 partyid2 partyid3 tacticalvote tacticalvotefor tacticalvote2 thermo1pref thermo2pref thermo3pref
gen year = 1996
merge m:m pano using wardkey /* attach shapefile master key */
drop if _merge==2 /* drop observations that only exist in key file */
gen study = "bsa1996"
save "prefs_bsa1996.dta", replace


*** 1998 *** 	British Social Attitudes Survey 
use "bsa98a.dta", clear
drop if region==1 | region==4 /* Scotland & Wales */
replace stregion = stregion - 1
gen vote = 0
replace vote = 1 if gevot97 ==1
replace vote = . if gevot97==.
recode partyid1 (6=4) (8=4) (9=4) (95=4) /* identification with other parties == 4 */
recode partyid1 (10=9) (97=9) (98=9) /* identification with no party == 9 */
replace partyid2 = partyid1 
replace partyid2 = 9 if closepty ==2
gen partyvote = .
gen partyvote_prev = .
gen tacticalvote = .
gen tacticalvotefor = .
gen tacticalvote2 = .
gen thermo1pref = .
gen thermo2pref = .
gen thermo3pref = .
keep stregion pano wtfactor vote partyvote partyvote_prev partyid1 partyid2 tacticalvote tacticalvotefor tacticalvote2  thermo1pref thermo2pref thermo3pref
gen year = 1998
merge m:m pano using wardkey /* attach shapefile master key */
drop if _merge==2 /* drop observations that only exist in key file */
gen study = "bsa1998"
save "prefs_bsa1998.dta", replace


*** 2000 *** 	British Social Attitudes Survey 
use "bsa00.dta", clear
drop if region==1 | region==4 /* Scotland & Wales */
replace stregion = stregion - 1
gen vote = 0
replace vote = 1 if vot9700 ==1
replace vote = . if vot9700==.
recode partyid1 (6=4) (8=4) (9=4) (95=4) /* identification with other parties == 4 */
recode partyid1 (10=9) (98=9) (99=9) /* identification with no party == 9 */
replace partyid2 = partyid1 
replace partyid2 = 9 if closepty ==2
gen partyvote = .
gen partyvote_prev = .
gen tacticalvote = .
gen tacticalvotefor = .
gen tacticalvote2 = .
gen thermo1pref = .
gen thermo2pref = .
gen thermo3pref = .
keep stregion concode wtfactor vote partyvote partyvote_prev partyid1 partyid2 tacticalvote tacticalvotefor tacticalvote2 thermo1pref thermo2pref thermo3pref
gen year = 2000
merge m:m concode using wardkey /* attach shapefile master key */
drop if _merge==2 | _merge==1 /* drop observations that only exist in key file */
gen study = "bsa2000"
save "prefs_bsa2000.dta", replace


*** 2001 *** 	British Social Attitudes Survey 
use "bsa01.dta", clear
rename vote vote2001
gen vote = 0
replace vote = 1 if voted ==1
drop if region==1 | region==4 /* Scotland & Wales */
replace stregion = stregion - 1
recode partyid1 (6=4) (7=4) (8=4) (9=4) (95=4) /* identification with other parties == 4 */
recode partyid1 (10=9) (98=9) (99=9) /* identification with no party == 9 */
replace partyid2 = partyid1 
replace partyid2 = 9 if closepty ==2
recode vote97 (0=9) (5/8=4) (97/98=9), gen(partyvote_prev)
recode vote2001 (-1=9) (6/7=4) (97/99=9), gen(partyvote)
gen tacticalvote = .
gen tacticalvotefor = .
gen tacticalvote2 = .
replace tacticalvote2 = 0 if partyvote == partyid2
replace tacticalvote2 = 1 if partyvote != partyid2 & partyvote!= 9
gen thermo1pref = .
gen thermo2pref = .
gen thermo3pref = .
keep stregion conname wtfactor vote partyvote partyvote_prev partyid1 partyid2 tacticalvote tacticalvotefor tacticalvote2 thermo1pref thermo2pref thermo3pref
gen year = 2001
merge m:m conname using wardkey /* attach shapefile master key */
drop if _merge==2 | _merge==1 /* drop observations that only exist in key file */
gen study = "bsa2001"
save "prefs_bsa2001.dta", replace


*** 2002 *** 	British Social Attitudes Survey 
use "bsa02.dta", clear
drop if region==1 | region==4 /* Scotland & Wales */
replace stregion = stregion - 1
gen vote=0
replace vote=1 if voted01==1
replace vote=. if voted01==-2
recode partyid1 (6=4) (7=4) (8=4) (9=4) (95=4) /* identification with other parties == 4 */
recode partyid1 (10=9) (98=9) (99=9) /* identification with no party == 9 */
replace partyid2 = partyid1 
replace partyid2 = 9 if closepty ==2
gen partyvote = .
gen partyvote_prev = .
gen tacticalvote = .
gen tacticalvotefor = .
gen tacticalvote2 = .
gen thermo1pref = .
gen thermo2pref = .
gen thermo3pref = .
keep stregion conname1 wtfactor vote partyvote partyvote_prev partyid1 partyid2 tacticalvote tacticalvotefor tacticalvote2 thermo1pref thermo2pref thermo3pref
gen year = 2002
merge m:m conname1 using wardkey /* attach shapefile master key */
drop if _merge==2 | _merge==1 /* drop observations that only exist in key file */
gen study = "bsa2002"
save "prefs_bsa2002.dta", replace


*** 1997 *** General Election Study, cross-section
use "besx97e.dta", clear
drop if stregion==10 | stregion==11  /* Scotland & Wales */
rename wtallgb wtfactor
rename isspano pano
gen vote97 = vote
replace vote = turnout /* validated vote */
recode vote (2=1) (3=1) (4=1) (6=0) (7=9) (8=0) (9=9) (10=9) (99=9) 
recode voted (2=0) (8=0)
replace vote = voted if vote==9
gen partyid2 = partyid
recode partyid2 (0=9) (4=4) (6=4) (7=4) (97=9) (98=9)
recode conlike lablike ldlike (-1=.) (96=.) (97=.) (98=.) (99=.)
replace partyid2 = 1 if partyid2==9 & conlike>lablike & conlike>ldlike
replace partyid2 = 2 if partyid2==9 & lablike>conlike & lablike>ldlike
replace partyid2 = 3 if partyid2==9 & ldlike>conlike & ldlike>lablike
gen partyid3 = 12 if partyid2 == 9 & conlike==lablike & ldlike<conlike
replace partyid3 = 13 if partyid2 == 9 & conlike==ldlike & lablike<conlike
replace partyid3 = 23 if partyid2 == 9 & lablike==ldlike & conlike<lablike
recode vote97 (-1=9) (6/8=4) (97=9) (99=9), gen(partyvote)
recode vote92 (0=9) (6/7=4) (96/98=9), gen(partyvote_prev)
tab yvotecls
gen tacticalvote = . /* tactical voting */
replace tacticalvote = 1 if yvotecls == 3 | yvotecls == 7 // reported tactical voting
replace tacticalvote = 0 if yvotecls == 1 | yvotecls == 2 | yvotecls == 4 | yvotecls == 5 | yvotecls == 6 | yvotecls == 8 | yvotecls == 9
replace tacticalvote = . if  tacticalvote == 1 & ptypref == vote97 // set inconsistent reporters 0
gen tacticalvotefor = vote97 if yvotecls == 3 | yvotecls == 7  /* tactical voting for ... */
recode tacticalvotefor (4/8 = 4) (97 = .)
gen tacticalvote2 = .
replace tacticalvote2 = 0 if partyvote == partyid2
replace tacticalvote2 = 1 if partyvote != partyid2 & partyvote!= 9
gen thermo1pref = 1 if conlike>lablike & conlike>ldlike
replace thermo1pref = 2 if lablike>conlike & lablike>ldlike
replace thermo1pref = 3 if ldlike>conlike & ldlike>lablike
gen thermo2pref = 1 if (conlike<lablike & conlike>ldlike) | (conlike>lablike & conlike<ldlike)
replace thermo2pref = 2 if (lablike<conlike & lablike>ldlike) | (lablike>conlike & lablike<ldlike)
replace thermo2pref = 3 if (ldlike<conlike & ldlike>lablike) | (ldlike>conlike & ldlike<lablike)
gen thermo3pref = 1 if conlike<lablike & conlike<ldlike
replace thermo3pref = 2 if lablike<conlike & lablike<ldlike
replace thermo3pref = 3 if ldlike<conlike & ldlike<lablike
keep stregion pano wtfactor vote partyvote partyvote_prev partyid2 partyid3 tacticalvote tacticalvotefor tacticalvote2 thermo1pref thermo2pref thermo3pref
gen year = 1997
merge m:m pano using wardkey /* attach shapefile master key */
drop if _merge==2 | _merge==1 /* drop observations that only exist in key file */
gen study = "gescs1997"
save "prefs_gescs1997.dta", replace


*** 2001 *** General Election Study, cross-section
use "xsectionagg.dta", clear
rename region stregion
drop if stregion==10 | stregion==11  /* Scotland & Wales */
rename ref pano
rename postoctw wtfactor
gen vote = 0
replace vote = 1 if voted==1 | voted==4 | voted==5 | voted==7
replace vote = . if voted==98
recode bq2a bq2c (1=2) (2=1) (6/7=4) (0=9) (8/99=9)
gen partyid2 = bq2a
replace partyid2 = bq2c if partyid2==9
replace partyid2 = 4 if partyid2==6
replace partyid2 = 9 if partyid2==9 | partyid2==99
recode bq11b bq11a bq11c (98=.) (99=.) (999=.)
replace partyid2 = 1 if partyid2==9 & bq11b>bq11a & bq11b>bq11c
replace partyid2 = 2 if partyid2==9 & bq11a>bq11b & bq11a>bq11c
replace partyid2 = 3 if partyid2==9 & bq11c>bq11a & bq11c>bq11b
gen partyid3 = 12 if partyid2 == 9 & bq11b==bq11a & bq11c<bq11b
replace partyid3 = 13 if partyid2 == 9 & bq11b==bq11c & bq11a<bq11b
replace partyid3 = 23 if partyid2 == 9 & bq11a==bq11c & bq11b<bq11a
recode vote97s (0=9) (1=2) (2=1) (4/8=4) (96/99=9) , gen(partyvote_prev)
recode bq8b (6/7=4) (9/99=9), gen(partyvote)
tab bq8d4
gen tacticalvote = .  /* tactical voting */
replace tacticalvote = 1 if bq8d4==1 | bq8d3==1 // reported tactical voting
replace tacticalvote = 0 if bq8d1==1 | bq8d2==1 | bq8d5==1
replace tacticalvote = . if  tacticalvote == 1 & bq8b == bq8e // set inconsistent reporters 0
gen tacticalvotefor = bq8b if bq8d4==1   /* tactical voting for... */
recode tacticalvotefor (4/7 = 4) (9 = .)
gen tacticalvote2 = .
replace tacticalvote2 = 0 if partyvote == partyid2
replace tacticalvote2 = 1 if partyvote != partyid2 & partyvote!= 9
gen thermo1pref = 1 if bq11b>bq11a & bq11b>bq11c
replace thermo1pref = 2 if bq11a>bq11b & bq11a>bq11c
replace thermo1pref = 3 if bq11c>bq11b & bq11c>bq11a
gen thermo2pref = 1 if (bq11b<bq11a & bq11b>bq11c) | (bq11b>bq11a & bq11b<bq11c)
replace thermo2pref = 2 if (bq11a<bq11b & bq11a>bq11c) | (bq11a>bq11b & bq11a<bq11c)
replace thermo2pref = 3 if (bq11c<bq11b & bq11c>bq11a) | (bq11c>bq11b & bq11c<bq11a)
gen thermo3pref = 1 if bq11b<bq11a & bq11b<bq11c
replace thermo3pref = 2 if bq11a<bq11b & bq11a<bq11c
replace thermo3pref = 3 if bq11c<bq11b & bq11c<bq11a
keep stregion pano wtfactor vote partyvote partyvote_prev partyid2 partyid3 tacticalvote tacticalvotefor tacticalvote2 thermo1pref thermo2pref thermo3pref
gen year = 2001
merge m:m pano using wardkey /* attach shapefile master key */
drop if _merge==2 | _merge==1 /* drop observations that only exist in key file */
gen study = "gescs2001"
save "prefs_gescs2001.dta", replace


*** 2001 General Election Study, RCS
use "gallupagg.dta", clear
rename region stregion
drop if stregion==10 | stregion==11  /* Scotland & Wales */
rename ref pano
rename weight wtfactor
tab qn5a
gen tacticalvote = .  /* tactical voting */
replace tacticalvote = 1 if qn5a==8 | qn5c==8
replace tacticalvote = 0 if qn5a==1 | qn5a==6 | qn5a==7 | qn5c==1 | qn5c==6 | qn5c==7 
replace tacticalvote = . if  tacticalvote == 1 & ((qn4b!=99 & qn4b==qn5b) | (qn4d!=99 & qn4d==qn5d))
gen tacticalvotefor = voteint if qn5a==8   /* tactical voting for... */
recode tacticalvotefor (4/7 = 4) (99 = .)
gen tacticalvote2 = .
gen vote = zqn2a
recode vote (2=0) (3=0) (4=0)
replace vote = 1 if vote==9 & voteint !=99
replace vote = 0 if vote==9 & voteint ==99
gen partyid2 = qn17a
recode partyid2 (1=4) (2=9) (3=9) (4=9) (6=1) (7=2) (8=3) (11=4)
recode qn17b (1=4) (2=9) (3=9) (4=9) (6=1) (7=2) (8=3) (9=9) (11=4)
replace partyid2 = qn17b if partyid2==9 & qn17b!=9
recode zqn11b  (1=4) (2=9) (3=9) (4=9) (6=1) (7=2) (8=3) (11=4) (99=9) (999=9)
replace partyid2 = zqn11b if partyid2==9 & zqn11b!=9
recode qn5b (1=4) (2=9) (4=9) (6=1) (7=2) (8=3) (11=4) (99=9)
recode zqn3b (1=4) (2=9) (3=9) (6=1) (7=2) (8=3) (11=4) (99=9) (999=9)
replace partyid2 = qn5b if qn5b!=9
replace partyid2 = zqn3b if zqn3b!=9
recode qn12c (1=4) (2/4=9) (6=1) (7=2) (8=3) (11/13=4), gen(partyvote_prev)
recode zqn2b qn4b qn4d (1=4) (2=9) (3=9) (6=1) (7=2) (8=3) (11=4) (99=9) (999=9)
gen partyvote = zqn2b
replace partyvote = qn4b if partyvote == 9
replace partyvote = qn4d if partyvote == 9
replace tacticalvote2 = 0 if partyvote == partyid2
replace tacticalvote2 = 1 if partyvote != partyid2 & partyvote!= 9
gen thermo1pref = .
gen thermo2pref = .
gen thermo3pref = .
keep stregion pano wtfactor vote partyvote partyvote_prev partyid2 tacticalvote tacticalvotefor tacticalvote2 thermo1pref thermo2pref thermo3pref
gen year = 2001
merge m:m pano using wardkey /* attach shapefile master key */
drop if _merge==2 | _merge==1 /* drop observations that only exist in key file */
gen study = "gesrcs2001"
save "prefs_gesrcs2001.dta", replace


*** 1992-1997 General Election Study, panel
use "beps9297.dta", clear
keep if datey97==97
rename streg97 stregion
drop if stregion==10 | stregion==11  /* Scotland & Wales */
gen vote = voted97
recode vote (2=0) 
gen partyid2 = ptytha97
replace partyid2 = ptythb97 if (partyid2==-2)
recode partyid2 (0=9) (6=4) (7=4) (10=4) (12=4) (14=4) (97=9) (98=9) (99=9)
replace partyid2 = ptycla97 if ptycla97==1 | ptycla97==2 | ptycla97==3
replace partyid2 = ptyclb97 if ptyclb97==1 | ptyclb97==2 | ptyclb97==3
recode pprfge97 (6=4) (12=4)
replace partyid2 = pprfge97 if pprfge97>0 & pprfge97<5
replace partyid2 = 1 if partyid2==9 & confel97<labfel97 & confel97<ldfel97 & confel97<grnfel97
replace partyid2 = 2 if partyid2==9 & labfel97<confel97 & labfel97<ldfel97 & labfel97<grnfel97
replace partyid2 = 3 if partyid2==9 & ldfel97<confel97 & ldfel97<labfel97 & ldfel97<grnfel97
replace partyid2 = 4 if partyid2==9 & grnfel97<confel97 & grnfel97<labfel97 & grnfel97<ldfel97
gen partyid3 = 12 if partyid2 == 9 & confel97==labfel97 & ldfel97<confel97
replace partyid3 = 13 if partyid2 == 9 & confel97==ldfel97 & labfel97<confel97
replace partyid3 = 23 if partyid2 == 9 & labfel97==ldfel97 & confel97<labfel97
gen year = 1997
recode vote92 (-1=9) (6/8=4) (97/99=9), gen(partyvote_prev)
recode vote97 (-1=9) (6/12=4) (97=9), gen(partyvote)
tab yvtcge97
gen tacticalvote = .  /* tactical voting */
replace tacticalvote = 1 if yvtcge97==3
replace tacticalvote = 0 if yvtcge97==1 | yvtcge97==2 | yvtcge97==4 | yvtcge97==5
replace tacticalvote = . if tacticalvote == 1 & pprfge97==vote97
gen tacticalvotefor = vote97 if yvtcge97==3   /* tactical voting for... */
recode tacticalvotefor (4/12 = 4)
gen tacticalvote2 = .
replace tacticalvote2 = 0 if partyvote == partyid2
replace tacticalvote2 = 1 if partyvote != partyid2 & partyvote!= 9
gen thermo1pref = 1 if confel97<labfel97 & confel97<ldfel97
replace thermo1pref = 2 if labfel97<confel97 & labfel97<ldfel97
replace thermo1pref = 3 if ldfel97<confel97 & ldfel97<labfel97
gen thermo2pref = 1 if (confel97<labfel97 & confel97>ldfel97) | (confel97>labfel97 & confel97<ldfel97)
replace thermo2pref = 2 if (labfel97<confel97 & labfel97>ldfel97) | (labfel97>confel97 & labfel97<ldfel97)
replace thermo2pref = 3 if (ldfel97<confel97 & ldfel97>labfel97) | (ldfel97>confel97 & ldfel97<labfel97)
gen thermo3pref = 1 if confel97>labfel97 & confel97>ldfel97
replace thermo3pref = 2 if labfel97>confel97 & labfel97>ldfel97
replace thermo3pref = 3 if ldfel97>confel97 & ldfel97>labfel97
keep stregion pano92 wtfactor vote partyvote partyvote_prev partyid2 partyid3 year tacticalvote tacticalvotefor tacticalvote2 thermo1pref thermo2pref thermo3pref
merge m:m pano92 using wardkey /* attach shapefile master key */
drop if _merge==2 | _merge==1 /* drop observations that only exist in key file */
gen study = "prefs_gespanel1997"
save "prefs_gespanel1997.dta", replace



*** 1997-2001 General Election Study, panel
use "beps9701.dta", clear
rename streg97 stregion
drop if stregion==10 | stregion==11  /* Scotland & Wales */
gen year = .
replace year = 2001 if wave5==1 | wave6==1 | wave7==1 | wave8==1
keep if year == 2001
rename wtallgb wtfactor
gen vote = 0 
replace vote= 1 if vote01 > 0 & vote01 < 97
gen partyid2 = ptyid00
replace partyid2 = 9 if partyid2 == -8 | partyid2 == 0 | partyid2 >=97
replace partyid2 = 4 if partyid2 == 4 | partyid2 == 6 | partyid2 == 7
replace partyid2 = ptypf01 if partyid2 == 9 & ptypf01 == 1 | ptypf01 == 2 | ptypf01 == 3
replace partyid2 = ptycls01 if partyid2 == 9 & ptycls01 == 1 | ptycls01 == 2 | ptycls01 == 3 
replace partyid2 = 1 if partyid2==9 & confel01<labfel01 & confel01<ldfel01
replace partyid2 = 2 if partyid2==9 & labfel01<confel01 & labfel01<ldfel01
replace partyid2 = 3 if partyid2==9 & ldfel01<confel01 & ldfel01<labfel01
gen partyid3 = 12 if partyid2 == 9 & confel01==labfel01 & ldfel01<confel01
replace partyid3 = 13 if partyid2 == 9 & confel01==ldfel01 & labfel01<confel01
replace partyid3 = 23 if partyid2 == 9 & labfel01==ldfel01 & confel01<labfel01
recode vote97 (-1=9) (6/12=4) (97=9), gen(partyvote_prev)
recode vote01 (-2/-1=9) (6/7=4) (97/99=9), gen(partyvote)
tab yvtcls01
tab gewhy011
gen tacticalvote = .  /* tactical voting */
replace tacticalvote = 1 if yvtcls01==3 | yvtcls01==7 | gewhy011==3 | gewhy011==6
replace tacticalvote = 0 if yvtcls01==1 | yvtcls01==2 | yvtcls01==4 | yvtcls01==5 | yvtcls01==6 | yvtcls01==8
replace tacticalvote = 0 if gewhy011==1 | gewhy011==2 | gewhy011==4 | gewhy011==5 | gewhy011==7 | gewhy011==8
replace tacticalvote = . if (yvtcls01==3 | yvtcls01==7) & (gewhy011==1 | gewhy011==2 | gewhy011==4 | gewhy011==5 | gewhy011==7 | gewhy011==8)
replace tacticalvote = . if (gewhy011==3 | gewhy011==6) & (yvtcls01==1 | yvtcls01==2 | yvtcls01==4 | yvtcls01==5 | yvtcls01==6 | yvtcls01==8)
replace tacticalvote = . if ptypf01==vote01
gen tacticalvotefor = vote01 if yvtcls01==3 | yvtcls01==7 | gewhy011==3 | gewhy011==6   /* tactical voting for... */
recode tacticalvotefor (4/7 = 4)
gen tacticalvote2 = .
replace tacticalvote2 = 0 if partyvote == partyid2
replace tacticalvote2 = 1 if partyvote != partyid2 & partyvote!= 9
gen thermo1pref = 1 if confel01<labfel01 & confel01<ldfel01
replace thermo1pref = 2 if labfel01<confel01 & labfel01<ldfel01
replace thermo1pref = 3 if ldfel01<confel01 & ldfel01<labfel01
gen thermo2pref = 1 if (confel01<labfel01 & confel01>ldfel01) | (confel01>labfel01 & confel01<ldfel01)
replace thermo2pref = 2 if (labfel01<confel01 & labfel01>ldfel01) | (labfel01>confel01 & labfel01<ldfel01)
replace thermo2pref = 3 if (ldfel01<confel01 & ldfel01>labfel01) | (ldfel01>confel01 & ldfel01<labfel01)
gen thermo3pref = 1 if confel01>labfel01 & confel01>ldfel01
replace thermo3pref = 2 if labfel01>confel01 & labfel01>ldfel01
replace thermo3pref = 3 if ldfel01>confel01 & ldfel01>labfel01
keep stregion pano wtfactor vote partyvote partyvote_prev partyid2 partyid3 year tacticalvote tacticalvotefor tacticalvote2 thermo1pref thermo2pref thermo3pref
merge m:m pano using wardkey /* attach shapefile master key */
drop if _merge==2 | _merge==1 /* drop observations that only exist in key file */
gen study = "prefs_gespanel2001"
save "prefs_gespanel2001.dta", replace


*** Create combined data set ***
use "prefs_bsa1996.dta", clear
append using "prefs_bsa1998.dta"
append using "prefs_bsa2000.dta"
append using "prefs_bsa2001.dta"
append using "prefs_bsa2002.dta"
append using "prefs_gescs1997.dta"
append using "prefs_gescs2001.dta"
*append using "prefs_gesrcs1997.dta" /* 1997 RCS is based on 1996 BSA and holds pretty much the same respondents  */
append using "prefs_gesrcs2001.dta"
append using "prefs_gespanel1997.dta"
append using "prefs_gespanel2001.dta"
order  study year england stregion shapefile_string shapefile_id shapefile_id2 wtfactor vote partyvote partyvote_prev partyid1 partyid2 partyid3 thermo1pref thermo2pref thermo3pref _merge  concode conname conname1 pano pano92
drop if stregion==.
replace partyid1 = partyid2 if partyid1==.  /* for surveys without ambiguous party identification variable */
* generate party vote variable
gen v_conser = partyvote == 1
gen v_labour = partyvote == 2
gen v_libdem = partyvote == 3
gen v_other = partyvote == 4
gen v_not = partyvote == 9
gen v_prev_conser = partyvote_prev == 1
gen v_prev_labour = partyvote_prev == 2
gen v_prev_libdem = partyvote_prev == 3
gen v_prev_other = partyvote_prev == 4
gen v_prev_not = partyvote_prev == 9
saveold "prefs_9602full.dta", replace




*** Create constituency data file
insheet using "constituency_data_norris.csv", delim(;) clear
rename ref pano
merge m:m pano using wardkey.dta, nogen
drop if shapefile_id==.
saveold constituency_data.dta, replace


*** Append 2001 census data to constituency data file 
use constituency_data.dta, clear
sort seatname
merge m:m seatname using constituency_data2005, nogen keep(1 3) keepusing(totpop01 workage01 pension01 nonwhite01 migrant01 working01 dole01 nonwork01)
saveold constituency_data.dta, replace


regress con01 pension01 migrant01 working01
regress lab01 pension01 migrant01 working01
regress ld01 pension01 migrant01 working01
regress con97 pension01 migrant01 working01
regress lab97 pension01 migrant01 working01
regress ld97 pension01 migrant01 working01

cor pension01 migrant01 working01 



*** Create datasets for poststratification
use constituency_data, clear
keep if shapefile_id2 <=528
keep shapefile_id2 con97 lab97 ld97
gen oth97 = 1 - (con97 + lab97 + ld97)
replace con97=0 if con97==.
replace lab97=0 if lab97==.
replace ld97=0 if ld97==.
replace oth97=0 if oth97==. | oth97<0
expand(4)
bysort shapefile_id2: gen index = _n
gen con = index==1
gen lab = index==2
gen lib = index==3
gen oth = index==4
gen prevote = con97 if index==1
replace prevote = lab97 if index==2
replace prevote = ld97 if index==3
replace prevote = oth97 if index==4
drop index con97 lab97 ld97 oth97
save prevvote01.dta, replace







