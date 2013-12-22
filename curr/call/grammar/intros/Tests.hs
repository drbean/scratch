module Tests where

import LogicalForm
import Parsing

tag = [
	"Vicky had sisters, didn't she?"
	-- , "Did Vicky have sisters?"
	, "Cindy liked playing the piano, didn't she?"
	-- , "Did Cindy like playing the piano?"
	, "Alex lived in Hsinchu, didn't he?"
	-- , "Did Alex live in Hsinchu?"
	, "Alex was a design_assistant, wasn't he?"
	, "Cindy was born in Jiayi, wasn't she?"
	, "Mi Mi was Rena's cat, wasn't it?"
	, "Jeff's father was a truck_driver, wasn't he?"
	, "Alex was born in Taoyuan, wasn't he?"
	]

hobbies_wh = [
	"What was Alex's hobby?"
	, "Whose hobby was drawing?"
	]

hobbies_yn = [
	"Was Rena's hobby listening to music?"
	, "Did Rena have a hobby?"
	]

vicky = [
	"Vicky's mother was a babysitter."
	,"Vicky's mother was a teacher."
	-- ,"Vicky's mother taught children."
	, "Vicky had some sisters."
	, "Vicky's sisters were students."
	, "Vicky liked shopping."
	-- , "Vicky had some hobbies."
	-- , "Vicky's hobby was exercising."
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
	-- , "Was Jeff born in 1963?"
	-- , "Jeff was born in 1963."
	-- , "Jeff was born in Taiwan in 1963."
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
	-- , "Did Jeff have a mother in Hsinchu?"
	-- , "Did Jeff have a mother in Taoyuan?"
	-- , "Did Alex have a mother in Taoyuan?"
	-- , "Did Alex have a sister in Taoyuan?"
	-- , "Did Alex have a sister in Hsinchu?"
	, "Jeff had a mother?"
	, "Some students had a mother?"
	]
have_whs = [
	"Who had a mother in Hsinchu?"
	, "Who had a mother in Taoyuan?"
	, "Who had a sister?"
	, "Who had a company?"
	]

starts_wh	= [
	"What did Jeff start?"
	, "Which company did Jeff start?"
	, "What company did Jeff start?"
	, "When did Jeff start NVIDIA?"
	, "When was NVIDIA started?"
	, "Where did Jeff start NVIDIA?"
	, "Whose company was started by Jeff?"
	, "Who was NVIDIA started by?"
	]

starts = [
	"Was NVIDIA started in 1993?"
	, "NVIDIA started in 1993."
	, "Jeff started NVIDIA in 1993."
	, "Jeff started NVIDIA."
	, "NVIDIA was started by Jeff in 1993."
	, "NVIDIA was started in Jeff by 1993."
	, "NVIDIA was started in 1993 by Jeff."
	, "NVIDIA wasn't started in 1993 by Jeff."
	, "Wasn't NVIDIA started in 1993 by Jeff?"
	]

comp_wh = [
	"Whose grandmother was a farmer?"
	]

comp_tests = [
	"Dr Bean was a teacher."
	, "Was Alex a teacher?"
	, "Was Alex a student?"
	, "Was Alex a student in Minghsin University?"
	, "Was Alex the student in Minghsin University?"
	, "Dr Bean was an teacher."
	, "Dr Bean was in Hshinchu."
	, "Dr Bean was a teacher in Hshinchu."
	, "Dr Bean was a person."
	, "Dr Bean wasn't an person."
	, "Alex wasn't a person."
	, "Wasn't Dr Bean a person?"
	, "Wasn't Alex a person?"
	, "Wasn't NVIDIA a person?"
	, "Wasn't Jeff a teacher"
	]

relatives = [
	"Who was a man who was a teacher?"
	, "Who was the man who was a teacher?"
	, "Who was a man who was a student?"
	, "Who was the man who was a student?"
	]
