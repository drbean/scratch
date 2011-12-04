module Tests where

import LogicalForm
import Parsing

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
	"Did Ken have work?",
	"Did John have work?",
	"Did Henry have work?"
	]
ungrammatical = [
	"Did John worked?",
	"Ken work?",
	"Man worked.",
	"Some man work.",
	"No worked.",
	"No-one work.",
	"Did John teach?",
	"John teach Ken.",
	"Ken taught."
	]
intransitives = [
	"Did John work?",
	"Did Ken work?",
	"Did Henry work?",
	"A man worked.",
	"Some man worked.",
	"No one worked.",
	"No-one worked.",
	"Everybody worked.",
	"Everyone worked.",
	-- "Many persons worked.",
	"No person worked.",
	"Did the man work?",
	"Did some man work?",
	"Did some men work?",
	"Did some woman work?",
	"Did some women work?",
	"Most men worked.",
	"Most men didn't work.",
	"Several men worked.",
	"Several men didn't work.",
	"Many men worked.",
	"Many men didn't work.",
	"All men worked.",
	"No man worked.",
	"Did Henry work at a farm?",
	"Henry worked on a farm?",
	"Henry worked in a farm?"
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
	"Who worked?",
	"Who did John teach?",
	"Who taught John?",
	"Who gave the shoes to John?",
	"Who gave some shoes to John?",
	"Which person worked?",
	"Which person did John teach?",
	"To whom did Ken give some shoes?",
	"Who did Ken give some shoes to?"
	]
relclauses = [
	"A woman who taught John worked.",
	"The woman who taught John worked.",
	"Did the woman who taught John work?",
	"Did every woman who taught John work?",
	"The woman who gave the shoes to John worked.",
	"Ken divorced the man that she gave the shoes to.",
	"Who killed the man that helped the woman " 
	 ++ "that had a boyfriend."
	]

