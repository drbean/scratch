module Tests where

import LogicalForm
import Parsing

test_text = [
	"Ileana spoke Spanish.",
	"Ileana's brother spoke Spanish.",
	"Ileana had a brother and a brother spoke Spanish.",
	"Ileana knew Spanish and Gus spoke English.",
	"Ileana spoke Spanish but Gus didn't speak Spanish.",
	"Ileana talked to Gus and Gus talked to Ileana's father.",
	"Ileana talked to Gus and Gus talked to Ileana's father " ++
		"and Ileana's father talked to Ileana.",
	"Ileana's brother looked back on Ileana's upbringing. \
	\Ileana talked to Gus. \
	\Ileana asked Gus about Vietnam."
	]
test_possessives = [
	"Gus looked back.",
	"Gus's daughter looked back.",
	"Ileana's father looked back.",
	"Gus's daughter looked back on Ileana's upbringing.",
	-- "Gus's daughter looked back on Gus's daughter's upbringing.",
	"Ileana's father looked back on Ileana's upbringing.",
	"Did Ileana's father look back on Ileana's upbringing?",
	"Did Ileana's brother look back on Ileana's upbringing?",
	"Did Ileana's father speak English?",
	"Did Ileana's brother speak English?",
	"Did Ileana's brother know Spanish?",
	"Did Ileana's mother speak Spanish?",
	"Did Ileana's mother speak English?",
	"Gus's daughter appreciated Ileana's father.",
	"Ileana's father  appreciated Gus's daughter.",
	"Did the sister of Ileana know Spanish?",
	"Did the father of Ileana look back on Ileana's upbringing?",
	"Did the brother of Ileana look back on Ileana's upbringing?",
	"Did the mother of Ileana speak English?",
	"Did the father of Ileana speak English?",
	"Did the brother of Ileana speak English?",
	"Did the mother of Ileana speak Spanish?",
	"Did the brother of Ileana know Spanish?",
	"Did the sister of Ileana know Spanish?"
	]
haves = [
	"Did Gus have Fidel?",
	"Did Gus have Ileana?",
	"Did Gus have a mother?",
	"Did Gus have a son?",
	"Did Gus have a daughter?",
	"Did Fidel have a mother?",
	"Did Gus have dolls?",
	"Did Gus have some dolls?",
	"Did Gus's daughter have some dolls?",
	"Did Gus's daughter have a doll?",
	"Did the daughter have some dolls?",
	"Did the daughter have no dolls?",
	"Did the parent have some dolls?",
	"Did the parent have no dolls?",
	"Did Gus have money?",
	"Did Fidel have money?",
	"Did Ileana have money?",
	"Did Gus have a parent?",
	"Did Gus have some parents?",
	"Did Gus have parents?",
	"Did Fidel have a parent?",
	"Did Fidel have some parents?",
	"Did Fidel have parents?",
	"Did Gus have a boat?",
	"Did Ileana have a boat?",
	"Did Fidel have a boat?",
	"Did someone have a boat?"
	]
ungrammatical = [
	"Did Ileana looked back?",
	"Gus look back?",
	"Man looked back.",
	"Some man work.",
	"No looked back.",
	"No-one work.",
	"Did Ileana teach?",
	"Ileana teach Gus.",
	"Gus raised."
	]
intransitives = [
	"Did Ileana leave?",
	"Did Gus leave?",
	"Did Ileana's father leave?",
	"A man left.",
	"Some man left.",
	"No one left.",
	"No-one left.",
	"Everybody left.",
	"Everyone left.",
	"Many persons left.",
	"No person left.",
	"Did the man leave?",
	"Did some man leave?",
	"Did some men leave?",
	"Did some woman leave?",
	"Did some women leave?",
	"Most persons left.",
	"Most men left.",
	"Most men didn't leave.",
	"Several men left.",
	"Several men didn't leave.",
	"Several persons left.",
	"Several persons didn't leave.",
	"Did Ileana look back?",
	"Did Gus look back?",
	"Did Ileana's father look back?",
	"A man looked back.",
	"Some man looked back.",
	"No one looked back.",
	"No-one looked back.",
	"Everybody looked back.",
	"Everyone looked back.",
	"Many persons looked back.",
	"No person looked back.",
	"Did the man look back?",
	"Did the girl look back?",
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
	"No man looked back."
	]
transitives = [
	"Did Gus work in the fields?",
	"Gus worked on the fields?",
	"Gus worked at the fields?",
	"Did Gus work at a motel?",
	"Gus worked on a motel?",
	"Gus worked in a motel?",
	"Gus worked as a doctor.",
	"Did Fidel work at a motel?",
	"Fidel worked on a motel?",
	"Fidel worked in a motel?",
	"Did Fidel disappoint Gus?",
	"Did Ileana study medicine?",
	"Ileana studied medicine.",
	"Gus studied medicine.",
	"Gus studied medicine at a motel.",
	"Gus studied medicine at medical school",
	"Did Gus go to medical school.",
	"Some woman went to medical school.",
	"Some man went to medical school.",
	"Some boy went to medical school.",
	"Some man raised Gus.",
	"A man raised Ileana",
	"Some woman told a story.",
	"Did Gus come from Cuba?",
	"Did Ofelia immigrate?",
	"Did Gus immigrate to the United States?",
	"Did Ofelia go to the United States?",
	"Did Ofelia come from Cuba?",
	"Did Ileana come to the United States?"
	]
ditransitive_tests = [
	"Gus told a story.",
	"Gus told Fidel a story.",
	"Gus told a story to Fidel.",
	"Gus told a story to Ileana",
	"Gus gave some dolls to Ileana.",
	"Did Gus give some dolls to Ileana.",
	"Did Gus give the dolls to Ileana?",
	"Did Gus give the dolls to someone?",
	"Gus gave several dolls to Ileana.",
	"Did someone give something to Ileana?",
	"A woman gave the dolls to Ileana.",
	"A woman gave the dolls to someone.",
	"A woman gave something to someone.",
	"Someone gave something to someone.",
	"Gus gave Ileana some dolls.",
	"Did Gus give Ileana some dolls?",
	"Did Gus give Ileana the dolls?",
	"Did Gus give someone the dolls?",
	"Gus gave Ileana several dolls.",
	"Did someone give Ileana something?",
	"A man gave Ileana the dolls.",
	"A man gave Ileana some dolls.",
	"A boy gave Ileana the dolls.",
	"Fidel gave Ileana the boat.",
	"A man gave someone the dolls.",
	"A man gave someone something.",
	"Someone gave someone something.",
	"Did Gus work at a motel?",
	"Gus did cleaning at a motel.",
	"Did Ileana pick tomatoes in the fields?"
	]
wh_questions =[
	"Who looked back?",
	"Which man looked back?",
	"Who raised Ileana?",
	"Which woman raised Ileana?",
	"Who gave the dolls to Ileana?",
	"Who gave some dolls to Ileana?",
	"Which person looked back?",
	"Which woman appreciated Gus?",
	"Which girl appreciated Gus?",
	"Which daughter appreciated Gus?",
	"Who did Ileana appreciate?",
	"Which person did Ileana appreciate?",
	"Which man did Ileana appreciate?",
	"Which woman did Ileana appreciate?",
	"Which thing did Ileana appreciate?",
	"Which boat did Ileana appreciate?",
	-- "To whom did Gus give some dolls?",
	-- "Who did Gus give some dolls to?",
	"Who had a boat?",
	"What did Ileana have?",
	"Who did Ileana have?",
	"Who did Fidel disappoint?",
	"Who did Gus's daughter appreciate?",
	"What did Gus's daughter appreciate?",
	"Which thing did Gus's daughter appreciate?",
	"Which man did Gus's daughter appreciate?",
	"Which woman did Gus's daughter appreciate?",
	"Which boat did Gus's daughter appreciate?",
	"What did someone have?"
	]
relclauses = [
	"A woman who raised Ileana looked back.",
	"The woman who raised Ileana looked back.",
	"Did the woman who raised Ileana look back?",
	"Did every person who raised Ileana look back?",
	"Did some person who raised Ileana look back?",
	"The woman who gave the dolls to Ileana looked back.",
	"Ofelia married the man that gave the dolls to Ileana.",
	"The man that Ofelia married gave the dolls to Ileana.",
	"The man Ofelia married gave the dolls to Ileana.",
	"Fidel disappointed the man that gave Ileana the dolls.",
	"The man that Fidel disappointed left Cuba.",
	"The man Fidel disappointed left Cuba.",
	"Who appreciated the man that gave the girl \
	 \that left Cuba a doll?"
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

