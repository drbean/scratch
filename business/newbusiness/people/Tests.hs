module Tests where

import LogicalForm
import Parsing

birth	= [
	"Was Jensen Huang born in 1963?"
	, "Jensen Huang was born in 1963."
	, "Jensen Huang was born in Taiwan."
	, "Jensen Huang was born in Taiwan in 1963."
	, "Jensen Huang born in 1963."
	, "Did Jensen Huang born in China?"
	]

birth_wh	= [
	"When was Jensen Huang born?"
	, "Where was Jensen Huang born?"
	, "Who was born in 1963?"
	]

passive_whs	= birth_wh ++ starts_wh

haves =	[
	"Did Jensen Huang have a company?"
	, "Who had a company?"
	, "Did Jensen Huang have a master's degree?"
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
	]

comp_wh = [
	"Whose company was NVIDIA?"
	]

comp_tests = [
	"Dr Bean was an teacher."
	, "Dr Bean was an ceo"
	, "Dr Bean was a person."
	, "Dr Bean wasn't an person."
	, "Morris Chang wasn't a person."
	, "Wasn't Dr Bean a person?"
	, "Wasn't Morris Chang a person?"
	, "Wasn't NVIDIA a person?"
	, "Wasn't Jensen Huang a teacher"
	, "Was Morris Chang a CEO?"
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
