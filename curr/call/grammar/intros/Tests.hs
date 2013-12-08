module Tests where

import LogicalForm
import Parsing

work	= [
	"Rena worked in America."
	, "Rena wanted to work in America."
	, "Rena wanted to work in Hsinchu."
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
	]

birth_wh	= [
	"When was Jensen Huang born?"
	, "Who was born in 1963?"
	, "Who wasn't born in 1931?"
	-- , "Where was Jensen Huang born?"
	]

birth_tag	= [
	"Jensen Huang was born in Taiwan, wasn't he?"
	, "Jensen Huang wasn't born in Taiwan, was he?"
	, "Jensen Huang was born in Stanford University, wasn't he?"
	-- , "Jensen Huang wasn't born in Taiwan, was Jensen Huang?"
	-- "Jensen Huang was born in Taiwan in 1963, wasn't he?"
	]

passive_whs	= birth_wh ++ starts_wh

passive_parse	=  map ( birth_wh !! ) [0,2] ++ map ( birth !! ) [1,2] ++ map ( birth_tag !! ) [0,2]
	
-- passive_trans	=  (handler transWH $ map ( birth_wh !! ) [0,2]) ++ (handler transTXT $ map ( birth !! ) [1,2]) ++ (handler transTAG $ map ( birth_tag !! ) [0,2])
	
haves =	[
	"Did Jensen Huang have a company?"
	, "Did Jensen Huang have a master's degree?"
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
	"Who was the person who NVIDIA was started by?"
	]

mia0009 = [
	"Jensen Huang moved to the U.S when he was eight years old, didn't he?"
	, "Where was Morris Chang born ?"
	, "How old is Jensen Huang?"
	, "Was Jensen Huang born in February?"
	, "Where did Jensen Huang graduate from before moving to Califovnia?"
	, "How much time did Morris Chang take to Master's degree?"
	, "Which university did Jensen Huang graduation from?"
	, "Where is Morris Chang from?"
	, "Jensen Huang moved to the U.S when he was 10,didn't he?"
	, "Where was Morris born?"
	, "Where did Jensen Huang graduate from?"
	, "Was NVIOIA starts in 1993?"
	, "Is Morris Chang graduated from M.I.T?"
	, "Where was Jensen Huang born?"
	, "What is Jensen Huang's chinese name?"
	, "How old is Morris Chang?"
	, "How old was Jensen Huang when he moved to the U.S?"
	, "Morris Chang's birthday is July 10,isn't he ?"
	, "Is Jensen Huang really a bankrupt?"
	, "How old did Morris Chang move to the U.S?"
	, "Does Jensen Huang like Taiwan?"
	, "Has Morris Chang worked with Jensen Huang?"
	, "Does Jensen Huang play table tennis?"
	, "Did Morris Chang move to the U.S when he was ten?"
	, "When is Jensen Huang's birthday?"
	, "How old is Morris Chang?"
	, "Zhang Zhong-Mo's college was MIT,wasn't it?"
	, "Where was Huang Ren-Xun's birthplace?"
	, "When did Jensen Huang move to the U.S?"
	, "Is Morris Chang birthplace in China?"
	, "When was Morris Chang born ?"
	, "How old is Jensen Huang?"
	, "Did Zhang Zhong-Mo take one year to do master's degree?"
	, "What company did Jensen Huang create?"
	, "Does Jensen Huang born in 1964?"
	, "Does Morris Chang born in Taiwan?"
	, "Did Jensen take more time than Morris to do Master's degree?"
	, "How old was Morris when he moved to the U.S?"
	]
