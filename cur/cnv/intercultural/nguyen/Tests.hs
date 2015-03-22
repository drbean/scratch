module Tests where

import LogicalForm
import Parsing

test_text = [
	"Stephanie spoke Vietnamese.",
	"Stephanie's brother spoke Vietnamese.",
	"Stephanie had a brother and a brother spoke Vietnamese.",
	"Stephanie knew Vietnamese and Theresa spoke English.",
	"Stephanie spoke Vietnamese but Theresa didn't speak Vietnamese.",
	"Stephanie talked to Theresa and Theresa talked to Stephanie's father.",
	"Stephanie talked to Theresa and Theresa talked to Stephanie's father " ++
		"and Stephanie's father talked to Stephanie.",
	"Stephanie's brother looked back on Stephanie's upbringing. \
	\Stephanie talked to Theresa. \
	\Stephanie asked Theresa about Vietnam."
	]
test_possessives = [
	"Theresa looked back.",
	"Theresa's daughter looked back.",
	"Stephanie's father looked back.",
	"Theresa's daughter looked back on Stephanie's upbringing.",
	-- "Theresa's daughter looked back on Theresa's daughter's upbringing.",
	"Stephanie's father looked back on Stephanie's upbringing.",
	"Did Stephanie's father look back on Stephanie's upbringing?",
	"Did Stephanie's brother look back on Stephanie's upbringing?",
	"Did Stephanie's father speak English?",
	"Did Stephanie's brother speak English?",
	"Did Stephanie's brother know Vietnamese?",
	"Did Stephanie's mother speak Vietnamese?",
	"Did Stephanie's mother speak English?",
	"Did the sister of Stephanie know Vietnamese?",
	"Did the father of Stephanie look back on Stephanie's upbringing?",
	"Did the brother of Stephanie look back on Stephanie's upbringing?",
	"Did the mother of Stephanie speak English?",
	"Did the father of Stephanie speak English?",
	"Did the brother of Stephanie speak English?",
	"Did the mother of Stephanie speak Vietnamese?",
	"Did the brother of Stephanie know Vietnamese?",
	"Did the sister of Stephanie know Vietnamese?"
	]
haves = [
	"Did Ken have Henry?",
	"Did Ken have John?",
	"Did Ken have a mother?",
	"Did Ken have a son?",
	"Did Ken have a daughter?",
	"Did Henry have a mother?",
	"Did Ken have shoes?",
	"Did Ken have some shoes?",
	"Did Ken have money?",
	"Did Henry have money?",
	"Did John have money?",
	"Did Ken have a parent?",
	"Did Ken have some parents?",
	"Did Ken have parents?",
	"Did Henry have a parent?",
	"Did Henry have some parents?",
	"Did Henry have parents?",
	"Did Ken have look back?",
	"Did John have look back?",
	"Did Henry have look back?"
	]
ungrammatical = [
	"Did John looked back?",
	"Ken look back?",
	"Man looked back.",
	"Some man work.",
	"No looked back.",
	"No-one work.",
	"Did John teach?",
	"John teach Ken.",
	"Ken taught."
	]
intransitives = [
	"Did Stephanie look back?",
	"Did Theresa look back?",
	"Did Stephanie's father look back?",
	"A man looked back.",
	"Some man looked back.",
	"No one looked back.",
	"No-one looked back.",
	"Everybody looked back.",
	"Everyone looked back.",
	-- "Many persons looked back.",
	"No person looked back.",
	"Did the man look back?",
	"Did some man look back?",
	"Did some men look back?",
	"Did some woman look back?",
	"Did some women look back?",
	"Most men looked back.",
	"Most men didn't look back.",
	"Several men looked back.",
	"Several men didn't look back.",
	"Many men looked back.",
	"Many men didn't work.",
	"All men looked back.",
	"No man looked back.",
	"Did Henry look back at a farm?",
	"Henry looked back on a farm?",
	"Henry looked back in a farm?"
	]
transitives = [
	"Did John study law?",
	"John studied law.",
	"Ken studied law.",
	"Ken studied law at Michigan Law.",
	"Ken studied law at Colorado College",
	"Did Ken go to Colorado College.",
	"Some woman went to Colorado College.",
	"Some man went to Colorado College.",
	"Some boy went to Colorado College.",
	"Some man parented Ken.",
	"A man parented John",
	"Some woman told a story."
	]
ditransitive_tests = [
	"Ken told a story.",
	"Ken told Henry a story.",
	"Ken told a story to Henry.",
	"Ken told a story to John",
	"Ken gave some shoes to John.",
	"Did Ken give some shoes to John.",
	"Did Ken give the shoes to John?",
	"Did Ken give the shoes to someone?",
	"Ken gave several shoes to John.",
	"Did someone give something to John?",
	"A woman gave the shoes to John.",
	"A woman gave the shoes to someone.",
	"A woman gave something to someone.",
	"Someone gave something to someone.",
	"Ken gave John some shoes.",
	"Did Ken give John some shoes?",
	"Did Ken give John the shoes?",
	"Did Ken give someone the shoes?",
	"Ken gave John several shoes.",
	"Did someone give John something?",
	"A man gave John the shoes.",
	"A boy gave John the shoes.",
	"Leroy gave John the shoe.",
	"A man gave someone the shoes.",
	"A man gave someone something.",
	"Someone gave someone something."
	]
wh_questions =[
	"Who looked back?",
	"Who did John teach?",
	"Who taught John?",
	"Who gave the shoes to John?",
	"Who gave some shoes to John?",
	"Which person looked back?",
	"Which person did John teach?",
	"To whom did Ken give some shoes?",
	"Who did Ken give some shoes to?"
	]
relclauses = [
	"A woman who taught John looked back.",
	"The woman who taught John looked back.",
	"Did the woman who taught John look back?",
	"Did every woman who taught John look back?",
	"The woman who gave the shoes to John looked back.",
	"Ken divorced the man that she gave the shoes to.",
	"Who killed the man that helped the woman " 
	 ++ "that had a boyfriend."
	]


lf0 = Rel "worked" [ Const(ents!!17) ]
lf00 = (Conj [(Rel "person" [Var 0]), (Rel "worked" [Var 0]) ] ) 
-- lf000 = (Exists (Conj [(Rel "person" [Var 0]), (Rel "worked" [Var 0]) ] )) (Const(ents)!!17)

lf1 = (Equi  (Rel "married" [ Const(ents!!9), Const(ents!!1) ]) (Neg (Rel "married" [ Const(ents!!8), Const(ents!!17)]) ) )

lf2 = (Conj [ (Rel "married" [ Const (ents !! 9), Const       (ents !! 1)]), (Rel "married" [ Const (ents !! 8), Const (ents !!   17)]) ] )

lf3 = Rel "married" [ Const (ents !! 8), Const (ents !! 17)]
lf4 = (Impl  (Rel "married" [ Const (ents !! 9), Const        (ents !! 1)]) (Rel "married" [ Const (ents !! 8), Const (ents !!    17)])  )
lf5 = (Conj [ (Rel "married" [ Const (ents !! 9), Const       (ents !! 1)]), (Rel "married" [ Const (ents !! 8), Const (ents !!   17)]) ] )
lf6 = (Disj [ (Rel "married" [ Const (ents !! 9), Const       (ents !! 1)]), (Rel "married" [ Const (ents !! 8), Const (ents !!   17)]) ] )

lf70 = ( \x -> ( Conj [ (Rel "son" [x]), (Rel "have" [x, Const (ents !! 8)]) ] ) ) (Const (ents !! 12) )
lf71 = ( \x -> ( Conj [ (Rel "son" [x]), (Rel "have" [x, Const (ents !! 17)]) ] ) ) (Const (ents !! 12) )
lf72 = ( \x -> ( Conj [ (Rel "son" [x]), (Rel "have" [x, Const (ents !! 17)]) ] ) ) (Const (ents !! 12) )
lf73 = \x -> Conj [ (Rel "son" [x]), (Rel "have" [x, Const (ents !! 17)]) ]
lf74 = ( \x -> ( Conj [ (Rel "daughter" [x]), (Rel "have" [x, Const (ents !! 17)]) ] ) )
lf75 = \x -> Impl (Rel "son" [x]) (Rel "have" [x, Const (ents !! 17)])

