module Tests where

import LogicalForm
import Parsing

vicky = [
	"Vicky's mother was a babysitter."
	,"Vicky's mother was a teacher."
	-- ,"Vicky's mother taught children."
	, "Vicky had some sisters."
	, "Vicky's sisters were students."
	, "Vicky liked shopping."
	, "Vicky had some hobbies."
	, "Vicky's hobby was exercising."
	, "Vicky liked reading."
	, "Vicky went out with friends on weekends."
	, "Vicky worked at a drug_store."
	]

cindy = [
	"Cindy was born in Jiayi."
	, "Cindy had a mother."
	, "Cindy liked playing the piano."
	, "Cindy liked listening to music."
	, "Cindy was 24."
	]

studies	= [
	"Alex studied English in Minghsin University."
	, "Jeff studied in Minghsin University Applied Foreign Languages."
	, "Jeff's brothers and sisters studied in Minghsin University."
	, "Rena studied English."
	, "Rena studied Applied Foreign Languages."
	]

jeff	= [
	"Jeff liked basketball."
	, "Jeff had a father."
	, "Jeff's mother was a career woman."
	, "Jeff's brothers and sisters were students."
	, "Jeff's brothers and sisters were students in Minghsin University."
	]

residents	= [
	"Rena lived in Hsinchu."
	, "Alex lived in Hsinchu."
	, "Shane lived in Hukou."
	]

likes	= [
	"Rena liked America."
	, "Rena liked cats."
	, "Rena liked Hello Kitty."
	, "Did Rena like the color pink."
	, "Did Jeff like the color pink."
	]

work	= [
	"Rena worked in America."
	, "Rena wanted to work in America."
	, "Rena wanted to work in Hsinchu."
	, "Did Rena work in America."
	, "Did Rena want to work in America."
	, "Rena did want to work in America."
	, "Alex was a design assistant."
	]

birth	= [
	"Mindy was born in Hsinchu."
	, "Neil wasn't born in Hsinchu."
	, "Was Mindy born in Hsinchu?"
	, "Wasn't Neil born in Hsinchu?"
	-- , "Was Jensen Huang born in 1963?"
	-- , "Jensen Huang was born in 1963."
	-- , "Jensen Huang was born in Taiwan in 1963."
	, "Kelly born in Hsinchu."
	, "Did Neil born in Hsinchu?"
	, "Neil didn't born in Hsinchu."
	, "Mindy was born in Taiwan"
	]

birth_wh	= [
	"Where was Mindy born?"
	, "Who was born in Hsinchu?"
	, "Who wasn't born in Hsinchu?"
	]

birth_tag	= [
	"Mindy was born in Hsinchu, wasn't she?"
	, "Neil was born in Hsinchu, wasn't he?"
	, "Neil wasn't born in Hsinchu, was he?"
	, "Neil was born in America, wasn't he?"
	-- , "Neil wasn't born in Hsinchu, was Neil?"
	-- "Neil was born in Hsinchu in 1963, wasn't he?"
	]

passive_whs	= birth_wh ++ starts_wh

passive_parse	=  map ( birth_wh !! ) [0,2] ++ map ( birth !! ) [1,2] ++ map ( birth_tag !! ) [0,2]
	
-- passive_trans	=  (handler transWH $ map ( birth_wh !! ) [0,2]) ++ (handler transTXT $ map ( birth !! ) [1,2]) ++ (handler transTAG $ map ( birth_tag !! ) [0,2])
	
haves =	[
	"Did Jeff have a farmer?"
	, "Did Jeff have a basketball?"
	, "Did Jensen Huang have a master's degree in electrical engineering?"
	, "Did Jensen Huang have a master's degree in mechanical engineering?"
	, "Did Morris Chang have a master's degree in mechanical engineering?"
	, "Did Morris Chang have a PhD degree in mechanical engineering?"
	, "Did Morris Chang have a PhD degree in electrical engineering?"
	, "Jensen Huang had a master's degree?"
	]
have_whs = [
	"Who had a master's degree in electrical engineering?"
	, "Who had a master's degree in mechanical engineering?"
	, "Who had a PhD degree?"
	, "Who had a company?"
	]

starts_wh	= [
	"What did Jensen Huang start?"
	, "Which company did Jensen Huang start?"
	, "What company did Jensen Huang start?"
	, "When did Jensen Huang start NVIDIA?"
	, "When was NVIDIA started?"
	, "Where did Jensen Huang start NVIDIA?"
	, "Whose company was started by Jensen Huang?"
	, "Who was NVIDIA started by?"
	]

starts = [
	"Was NVIDIA started in 1993?"
	, "NVIDIA started in 1993."
	, "Jensen Huang started NVIDIA in 1993."
	, "Jensen Huang started NVIDIA."
	, "NVIDIA was started by Jensen Huang in 1993."
	, "NVIDIA was started in Jensen Huang by 1993."
	, "NVIDIA was started in 1993 by Jensen Huang."
	, "NVIDIA wasn't started in 1993 by Jensen Huang."
	, "Wasn't NVIDIA started in 1993 by Jensen Huang?"
	]

comp_wh = [
	"Whose company was NVIDIA?"
	]

comp_tests = [
	"Dr Bean was an ceo"
	, "Was Morris Chang a CEO?"
	, "Was Morris Chang TSMC's CEO?"
	, "Was Morris Chang a CEO of TSMC?"
	, "Was Morris Chang the CEO of TSMC?"
	, "Dr Bean was an teacher."
	, "Dr Bean was in Taiwan."
	, "Dr Bean was a teacher in Taiwan."
	, "Dr Bean was a person."
	, "Dr Bean wasn't an person."
	, "Morris Chang wasn't a person."
	, "Wasn't Dr Bean a person?"
	, "Wasn't Morris Chang a person?"
	, "Wasn't NVIDIA a person?"
	, "Wasn't Jensen Huang a teacher"
	]

relatives = [
	"Who was a man who was a teacher?"
	, "Who was the man who was a teacher?"
	, "Who was a man who was a student?"
	, "Who was the man who was a student?"
	]
