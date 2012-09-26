module Story_Cats where

import Parsing

names, nouns, verbs, aux, adjs, advs :: Lexset

names = [
	[Cat "polish"	"ADJ" [] [],
		Cat "polish" "NP" [Thrd,Neutr,Sg] []],
	[Cat "american"	"ADJ" [] []],
	[Cat "english"	"ADJ" [] [],
		Cat "english" "NP" [Thrd,Neutr,Sg] []],
	[Cat "german"	"ADJ" [] [],
		Cat "german" "NP" [Thrd,Neutr,Sg] []],
	[Cat "poland" "NP" [Thrd,Neutr,Sg] []],
	[Cat "the_united_states" "NP" [Thrd,Neutr,Sg] []],
	[Cat "germany" "NP" [Thrd,Neutr,Sg] []],
	[Cat "fast-track" "NP" [Thrd,Neutr,Sg] []],
	[Cat "barbara"	"NP" [Thrd,Fem,Sg] []],
	[Cat "eva"	"NP" [Thrd,Fem,Sg] []],
	[Cat "tadeusz"	"NP" [Thrd,Masc,Sg] []],
	[Cat "dr_bean"	"NP" [Thrd,Masc,Sg] []]
	]

nouns = [

	[Cat "secondary_school"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "college"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "marketing"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "engineering"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "history"	"NP" [Thrd,Neutr,Sg] []],

	[Cat "sales_representative"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "regional_manager"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "sales_manager"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "candidate"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "candidates"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "co-worker"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "co-workers"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "team_member"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "team_members"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "sales_record"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "sales_experience"	"NP" [Thrd,Neutr,Sg] [],
		Cat "sales_experience"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "team-player"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "team_player"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "team_building"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "personality"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "ability"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "experience"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "judgement"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "ideas"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "local_business_club"	"CN" [Thrd,Neutr,Sg] []],

	[Cat "interviewer"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "interviewers"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "interviewee"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "interviewees"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "boss"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "bosses"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "company"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "hospital"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "school"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "presentation"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "job"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "jobs"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "subject"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "subjects"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "name"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "names"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "upbringing" "CN" [Thrd,Neutr,Sg] []],
	[Cat "requests"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "request"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "comments"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "comment"	"CN" [Thrd,Neutr,Sg] []]
	]

verbs = [
	[Cat "helped"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "help"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "volunteered"	"VP" [Tense] [Cat "_" "PP" [At] []]],
	[Cat "volunteer"	"VP" [Infl] [Cat "_" "PP" [At] []],
		Cat "volunteer"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "appeared"	"VP" [Tense] [Cat "_" "ADJ" [AccOrDat] []]],
	[Cat "appear"	"VP" [Infl] [Cat "_" "ADJ" [AccOrDat] []]],
	[Cat "taught"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "taught"	"VP" [Tense] []],
	[Cat "teach"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "teach"	"VP" [Infl] []],
	[Cat "visited"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "visited"	"VP" [Tense] []],
	[Cat "visit"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "visit"	"VP" [Infl] []],

	[Cat "respected"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "respect"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "angered"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "anger"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "offended"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "offend"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "interviewed"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "interview"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "greeted"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "greet"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "wore"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "wear"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "wearing"	"GER" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	]

aux = [
	]

adjs = [
	[Cat "thirty"	"ADJ" [] []],
	[Cat "fifty-two"	"ADJ" [] []],
	[Cat "forty-two"	"ADJ" [] []],
	[Cat "thirty_years_old"	"ADJ" [] []],
	[Cat "fifty-two_years_old"	"ADJ" [] []],
	[Cat "forty-two_years_old"	"ADJ" [] []],
	[Cat "five"	"ADJ" [] []],
	[Cat "fifteen"	"ADJ" [] []],
	[Cat "strong"	"ADJ" [] []],
	[Cat "energetic"	"ADJ" [] []],
	[Cat "confident"	"ADJ" [] []],
	[Cat "aggressive"	"ADJ" [] []],
	[Cat "calm"	"ADJ" [] []],
	[Cat "relaxed"	"ADJ" [] []],
	[Cat "hard-working"	"ADJ" [] []],
	[Cat "creative"	"ADJ" [] []],
	[Cat "successful"	"ADJ" [] []],
	[Cat "practical"	"ADJ" [] []],
	[Cat "reliable"	"ADJ" [] []],
	[Cat "quiet"	"ADJ" [] []],
	[Cat "nervous"	"ADJ" [] []],
	[Cat "excellent"	"ADJ" [] []],
	[Cat "fluent"	"ADJ" [] []],
	[Cat "ambitious"	"ADJ" [] []],
	[Cat "patient"	"ADJ" [] []],
	[Cat "realistic"	"ADJ" [] []],
	[Cat "competitive"	"ADJ" [] []],
	[Cat "good"	"ADJ" [] []]
	]

advs = [
	[Cat "slowly"	"ADV" [] []]
	]
