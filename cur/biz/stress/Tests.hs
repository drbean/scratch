module Tests where

import Evaluation
import Parsing

comp_tests = [
	"Lack of support was stressful.",
	"Uncertainty was stressful.",
	"Lack of control was stressful.",
	"Pressure was stressful."
	]

--all_tests =
--		student_tag_tests ++ student_yn_tests ++ student_tests_wh ++
--		test_text ++ comp_test1 ++ comp_test2 ++ tag_test ++ neg_tag_test ++
--		test_possessives ++ haves ++ intransitives ++ transitives ++ 
--		ditransitive_tests ++ relclauses ++ relppR_test
--		-- ++ wh_questions ++ comp_wh_questions
--		-- ++ ungrammatical
--
--student_tag_tests = [
--	-- "Eva had been with Fast-Track for one year, hadn't she?"
--	"Barbara wasn't a good team-player, was she?",
--	"Barbara was a good team-player, wasn't she?",
--	"Barbara was a confident person, wasn't she?",
--	"Barbara was thirty years old, wasn't she?"
--	]
--
--student_yn_tests = [
--	"Did Barbara want to improve her English?",
--	"Was Barbara a good team-player?",
--	"Did Eva have a college degree?",
--	"Was Tadeusz a hard worker?",
--	"Did Tadeuszs have successful sales experience?",
--	"Did Barbara have confidence?",
--	"Was Tadeusz a sales manager?",
--	"Could Barbara speak German?",
--	"Were the three people comfortable when they faced difficulties?",
--	"Were they all Polish?",
--	"Did Barbara feel nervous?",
--	"Could Eva speak Polish?",
--	"Could Barbara speak Polish?",
--	"Was Tadeusz thirty-years-old?"
--	]
--
--student_tests_wh = [
--	"What features did Tadeusz lack as business manager?",
--	"Who could be a great leader in this group?",
--	"Who was Tadeusz?",
--	"Who worked in many industries for five years?",
--	"Who worked for Fast-Track?",
--	"Was Barbara's English good?",
--	"Who had the best language skill?",
--	"Who knew lots of English words?",
--	"Who was the oldest?",
--
--	"What did Eva major in in university?",
--	"Who finished high school and had a marketing diploma",
--	"How long had Barbara been at Fast-Track",
--	"Who has excellent language ability?",
--	"What is Eva's occupation?",
--	"How many languages can you speak?",
--	"How old was Tadeusz?",
--	"How many percent did Tadeusz increse sales each year?",
--	"How many years did they work in Fast-Track?",
--	"What is Barbara's personality like?",
--	"Who is the oldest person?",
--	"Who is the longest in sales departments?",
--	"How old is Eva?",
--	"How many years did Barbara work in Fast-Track?",
--	"Who worked in Fast-Track the longest?",
--	"What kind of leader do you think is best?",
--	"Who has the longest sales experience?",
--	"Who worked shorter?",
--	"Who is not Polish?",
--	"Who has the highest degree in history?",
--	"Who is the oldest?",
--	"Who is the youngest?",
--	"How many years did Eva work at Fast-Track?",
--	"Who had the highest degree?",
--	"Who can speak most languages?",
--	"Who has the history diploma?",
--	"Who is the oldest?",
--	"Who is the best sales representative in Fast-Track?",
--	"Who worked in Fast-Track five years ago?",
--	"Where does Eva come from?",
--	"Who is the best in history?",
--	"Who worked in Fast-Track the shortest time?",
--	"How old is Eva?",
--	"Where does Barbara work?",
--	"Where is Eva from?"
--
--	]
--
--test_text = [
--	"Barbara's boss talked to Eva.",
--	"Barbara's boss had a boss and a boss was German.",
--	"Barbara's boss talked to Eva and a co-worker talked to Dr Bean.",
--	"Barbara's boss talked to Eva but a co-worker didn't talk to Dr Bean.",
--	"Barbara talked to a co-worker and a co-worker talked to Barbara's boss.",
--	"Barbara talked to a co-worker and a co-worker talked to Eva \
--	\and Eva talked to Barbara."
--	-- "Barbara hoped to get the job on a boss's upbringing. \
--	-- \Barbara talked to a co-worker. \
--	-- \Barbara's boss asked a co-worker about Barbara."
--	]

comp_test1 = [
	-- "A person was Steve Fossett."
	-- , "Steve Fossett was a man."
	"Steve Fossett was a person."
	, "Steve Fossett was a manager."
	, "Steve Fossett was the Million Dollar Homepage."
	, "Steve Fossett was the Million Dollar Homepage's manager."
	, "Steve Fossett was a person."
	, "Steve Fossett was a thing."
	, "A man was Tadeusz."
	, "A woman was the Million Dollar Homepage."
	, "The woman was the Million Dollar Homepage."
	, "Steve Fossett was a German woman."
	, "Steve Fossett was Alex Tew."
	, "A German woman was Steve Fossett."
	, "A ambitious woman was Steve Fossett."
	, "The ambitious woman was Steve Fossett."
	, "The German woman was Steve Fossett."
	, "Mark Zuckerberg was Steve Fossett's co-worker."
	, "Mark Zuckerberg was Steve Fossett's boss."
	, "Mark Zuckerberg was a German woman's boss."
	, "Mark Zuckerberg was an ambitious woman's boss."
	, "Mark Zuckerberg was an ambitious woman's co-worker."
	, "Mark Zuckerberg was the ambitious women's co-worker."
	, "the Million Dollar Homepage was the ambitious women's co-worker."
	, "the Million Dollar Homepage was an ambitious woman's co-worker."
	, "the Million Dollar Homepage was Steve Fossett's co-worker."
	, "the Million Dollar Homepage was a German woman's co-worker."
	, "the Million Dollar Homepage was Steve Fossett's co-worker."
	, "Mark Zuckerberg was Steve Fossett's co-worker."
	, "Mark Zuckerberg was Tadeusz's co-worker."
	, "Mark Zuckerberg was Steve Fossett's boss."
	, "Mark Zuckerberg was the Million Dollar Homepage's boss."
	, "Steve Fossett was the Million Dollar Homepage's boss."
	, "Steve Fossett was the Million Dollar Homepage's co-worker."
	, "Steve Fossett was Mark Zuckerberg's co-worker."
	, "Some women were ambitious."
	, "Mark Zuckerberg was a co-worker and the Million Dollar Homepage was a co-worker."
	, "Mark Zuckerberg was a woman and the Million Dollar Homepage was a woman"
	, "Some co-workers were women."
	, "Some women were co-workers."
	, "All co-workers were women."
	, "All women were co-workers."
	, "All ambitious women were successful."
	]

pressure_test = [
	"Olivier put pressure on Jacques.",
	"Jacques put pressure on Todd.",
	"Todd put pressure on Olivier.",
	"Olivier put pressure on Todd.",
	"Charles put pressure on Todd.",
	"Pressure was stressful."
	]

tag_test = [
	"The experiment was a good idea, wasn't it?"
	, "Some men were Punjabi farmers, weren't they."
	, "Some man wanted to sell oranges, didn't he?"
	, "Punjabi farmers wanted to sell oranges, didn't they?"
	, "Punjabi farmers wanted to sell oranges to Pepsi, didn't they?"
	, "The Punjabi government was a person, wasn't he?"
	]

neg_tag_test = [
	"The experiment wasn't a good idea, was it?"
	, "Punjabi farmers didn't want to sell oranges to Pepsi, did they?"
	, "The Punjabi government wasn't a person, was he?"
--	"Some woman wasn't ambitious, was she.",
--	"Some women weren't successful, was she.",
--	"Some women weren't successful, were they.",
--	"Eva wasn't a woman with a boss, was she?",
--	"Wasn't Eva a woman, was she?",
--	"Barbara wasn't an ambitious woman who was a co-worker, was she?",
--	"Barbara wasn't an ambitious woman who had a boss, was she?",
--	"The ambitious woman who had a boss wasn't Barbara, was she?",
--	"Barbara didn't have a boss, did she?",
--	"Barbara didn't have a boss, was she?",
--	"Barbara didn't have a co-worker, did she?",
--	"Barbara did have a co-worker, didn't she?",
--	"Barbara did have a co-worker, did she?",
--	"Barbara did have a co-worker, did he?",
--	"Some women didn't have co-workers, did they?"
	]

--test_possessives = [
--	-- "Dr Bean's co-worker hoped to get the job.",
--	"Barbara's co-worker talked to the boss.",
--	"Barbara's boss got Dr Bean's job.",
--	"Did Dr Bean's co-worker give a job to the German woman's boss?",
--	"Did Dr Bean's co-worker give a job to a German woman's boss?",
--	"Did Eva talk to Dr Bean?",
--	"The boss of Barbara talked to Eva.",
--	"Did the co-worker of Barbara talk to Dr Bean?",
--	"Did a co-worker of Barbara talk to Dr Bean?",
--	"Did the co-worker of a German woman talk to Barbara's boss?",
--	"Did the co-worker of the German woman have sales experience?",
--	"Did a co-worker of an ambitious woman have sales experience?",
--	"Did the co-workers of the ambitious women have sales experience?",
--	"Did the co-worker of Dr Bean have sales experience?",
--	"Did the boss of Barbara have sales experience?",
--	"Did the boss of the German woman have sales experience?",
--	"Did the boss of Barbara talk to Eva?"
--	]

experiment	= [
	"Was the Million Dollar Homepage a successful experiment?"
	, "Steve Fossett was a successful person."
	, "The experiment was successful."
	, "An experiment was successful."
	, "Was the experiment successful?"
	, "The Million Dollar Homepage was successful."
	, "Was the Million Dollar Homepage successful?"
	, "Was the One Million People Page successful?"
	, "The One Million People Page was successful."
	]
talks = [
	"Did Steve Fossett talk to someone about the Million Dollar Homepage?"
	, "Did Steve Fossett talk to radio and television about the Million Dollar Homepage?"
	, "Did Steve Fossett talk to the media about the Million Dollar Homepage?"
	, "Did Steve Fossett talk to the media?"
	, "Did Steve Fossett promote the website on the media?"
	, "Did Steve Fossett promote the Million Dollar Homepage on the media?"
	, "Did Steve Fossett promote the Million Dollar Homepage on radio and television?"
	, "Steve Fossett promoted the Million Dollar Homepage on radio and television."
	, "Did Steve Fossett promote the website on radio and television?"
	, "Did Steve Fossett promote the website on the radio and television?"
	]

wants = [
	"Did advertisers want advertising space?"
	, "Did advertisers want advertising space from Steve Fossett?"
	]

studies = [
	"Did Steve Fossett study?"
	, "Did Steve Fossett study business management?"
	, "Did Steve Fossett want to study?"
	, "Did Steve Fossett want to study something?"
	, "Did Steve Fossett want to study business management?"
	, "Did Mark Zuckerberg study?"
	, "Did Mark Zuckerberg study something?"
	, "Did Mark Zuckerberg want to study?"
	, "Did Mark Zuckerberg want to study business management?"
	, "Did Mark Zuckerberg want to study something?"
	, "Did Mark Zuckerberg want to study a thing?"
	, "Was something a thing?"
	]

buys_sells = [
	"Did advertisers buy?"
	, "Did advertisers buy from Steve Fossett?"
	, "Did advertisers buy advertising space?"
	, "Did advertisers buy advertising space from Steve Fossett?"
	, "Did advertisers want to buy from Steve Fossett?"
	, "Advertisers wanted to buy from Steve Fossett?"
	, "Advertisers wanted to buy advertising space from Steve Fossett on the Million Dollar Homepage."
	, "Did Steve Fossett sell advertising space to advertisers?"
	, "Did Steve Fossett sell to advertisers?"
	, "Did Steve Fossett sell advertising space?"
	, "Did he sell advertising space?"
	, "Did the Million Dollar Homepage sell advertising space?"
	, "Did Steve Fossett sell advertising space to advertisers on the Million Dollar Homepage?"
	, "Did Mark_Zuckerberg sell radio_and_television?"
	]

makes = [
	"Did Steve Fossett make the Million Dollar Homepage?"
	, "Did Mark Zuckerberg make the Million Dollar Homepage?"
	, "Did advertisers decide Steve Fossett to make the Million Dollar Homepage?"
	, "Did advertisers decide Mark Zuckerberg to make the Million Dollar Homepage?"
	, "Did Steve Fossett decide the Million Dollar Homepage to make with advertising space?"
	, "Did Mark Zuckerberg decide to make the Million Dollar Homepage?"
	, "Did Steve Fossett decide to make the Million Dollar Homepage?"
	, "Did Steve Fossett decide to make the Million Dollar Homepage with advertising space?"
	, "Did Steve Fossett want to make money?"
	, "Did Steve Fossett want to make money from the Million Dollar Homepage?"
	, "Did Steve Fossett want to make money with the Million Dollar Homepage?"
	, "Did Mark Zuckerberg want to make money?"
	]

helps = [
	"Did the Punjabi government help Punjabi farmers?"
	, "Did the Punjabi government want to help Punjabi farmers?"
	, "Did Punjabi farmers make money?"
	, "Did the Punjabi government help Punjabi farmers to make money?"
	, "Did the Punjabi government help Punjabi farmers make money?"
	, "Did the Punjabi government want to help Punjabi farmers to make money?"
	]

pays = [
	"Did advertisers on Steve Fossett's website pay Alex Tew a good price?"
	, "Did advertisers pay a good price?"
	, "Did advertisers pay Steve Fossett a good price for advertising space?"
	]

haves = [
	"Did Steve Fossett have a good idea after the Million Dollar Homepage?"
	]

knows = [
	"Steve Fossett knew Mark Zuckerberg?"
	, "Mark Zuckerberg knew about Steve Fossett."
	, "The One Million People Page knew about Steve Fossett."
	, "Did Facebook know about Steve Fossett?"
	, "Did people know Mark Zuckerberg?"
	, "Did people know about Mark Zuckerberg?"
	, "Did a manager know about Mark Zuckerberg?"
	]

likes = [
	"Did Pepsi like the Punjabi government?",
	"Did people like the Punjabi government?",
	"Did people like oranges?",
	"Did the woman like Pepsi?",
	"Did the woman like oranges?"
	]

ungrammatical = [
	"Was Pepsi buy a car?"
	, "Was a woman with a child?" 
	, "Was a woman with a child have to pay the entrance_fee?" 
	, "Was a woman with children have to pay the entrance_fee?" 
	, "Was a man with a child have to pay the entrance_fee?" 
	, "Was a man with children have to pay the entrance_fee?" 
	, "Did the Punjabi government have oranges?" 
	, "Was Pepsi knew the Punjabi government have oranges?" 
	, "Did Pepsi know the Punjabi government have oranges?" 
	, "Was people want to look at the oranges?" 
	, "Were people want to look at the oranges?"  
	, "Were the people want to look at the oranges?"
	]

intransitives = [
	"Pepsi paid",
	"Did Pepsi pay?",
	-- "Pepsi's boss paid.",
	-- "A woman's boss paid to enter the showroom.",
	-- "Did Linda Chen's boss pay?",
	-- "Did a woman's boss pay?",
	"A man paid.",
	"Some man paid.",
	"No one paid.",
	"No-one paid.",
	"Everybody paid.",
	"Everyone paid.",
	"Many persons paid.",
	"No person paid.",
	"Did the man pay?",
	"Did some man pay?",
	"Did some men pay?",
	"Did some woman pay?",
	"Did some women pay?",
	"Most women paid.",
	"Most women didn't pay.",
	"Several women paid.",
	"Several women didn't pay.",
	"Many women paid.",
	"Many women didn't pay.",
	"All women paid.",
	"No woman paid.",
	"Most men paid.",
	"Most men didn't pay.",
	"Several men paid.",
	"Several men didn't pay.",
	"Many men paid.",
	"Many men didn't pay.",
	"All men paid.",
	"No man paid.",
	"Pepsi paid."
	]

transitives = [
	"Pepsi bought oranges."
	, "Did Pepsi pay Punjabi farmers?"
	-- "Pepsi's boss bought oranges."
	-- "A woman's boss paid to talk to the showroom ten dollars."
	-- "Did Linda Chen's boss pay the entrance fee?"
	-- "Did a woman's boss pay the entrance fee?"
	, "A man bought oranges."
	, "Some man bought oranges."
	, "No one bought oranges."
	, "No-one bought oranges."
	, "Everybody bought oranges."
	, "Everyone bought oranges."
	, "Many persons bought oranges."
	, "No person bought oranges."
	, "Most women bought oranges."
	, "Most women didn't pay ten dollars."
	, "Several women bought oranges."
	, "Several women didn't pay ten dollars."
	, "Many women talked to the Punjabi government."
	, "Many women didn't talk to the Punjabi government."
	, "All women talked to the Punjabi government."
	, "No woman talked to the Punjabi government."
	, "Most men talked to the Punjabi government."
	, "Most men didn't talk to the Punjabi government."
	, "Several men talked to the Punjabi government."
	, "Several men didn't talk to the Punjabi government."
	, "Many men talked to the Punjabi government."
	, "Many men didn't talk to the Punjabi government."
	, "All men talked to the Punjabi government."
	, "No man talked to the Punjabi government."
	, "Visitors talked to the Punjabi government."
	, "Did all Visitors buy a car?"
	]

ditransitive_tests = [
	"Did Larry finish the website in one month.",
	"Did Michelle give Larry 6,000 dollars.",
	"Did Larry speak Michelle?",
	"Did Larry speak to Michelle about the website?"
--	"Barbara gave Barbara's boss a presentation.",
--	"Barbara gave a presentation to Barbara's boss.",
--	"Barbara gave a presentation to Eva",
--	"Barbara gave some job to Eva.",
--	"Did Barbara give some job to Eva.",
--	"Did Barbara give the job to Eva?",
--	"Did Barbara give the job to someone?",
--	"Barbara gave several jobs to Eva.",
--	"Did someone give something to Eva?",
--	"A woman gave the job to Eva.",
--	"A woman gave the job to someone.",
--	"A woman gave something to someone.",
--	"Someone gave something to someone.",
--	"Barbara gave Eva some job.",
--	"Did Barbara give Eva some job?",
--	"Did Barbara give Eva the job?",
--	"Did Barbara give someone the job?",
--	"Barbara gave Eva several jobs.",
--	"Did someone give Eva something?",
--	"A man gave Eva the job.",
--	"A boy gave Eva the job.",
--	"Dr Bean gave Eva a co-worker.",
--	"A man gave someone the job.",
--	"A man gave someone something.",
--	"Someone gave someone something.",
--	"A co-worker gave a presentation.",
--	"A co-worker gave the interviewer a presentation.",
--	"A co-worker gave a presentation to the interviewer.",
--	"A co-worker gave a presentation to Barbara's boss",
--	"A co-worker gave some job to Barbara's sales record.",
--	"Did a co-worker give some job to Barbara's sales record.",
--	"Did a co-worker give the job to Barbara's boss?",
--	"Did a co-worker give the job to someone?",
--	"A co-worker gave a job to Barbara's sales record.",
--	"Did someone give something to Barbara's boss?",
--	"A woman gave the job to Barbara's sales record.",
--	"A woman gave the job to someone.",
--	"A woman gave something to someone.",
--	"Someone gave something to someone.",
--	"A co-worker gave Barbara's boss some candidates.",
--	"Did a co-worker give Barbara's boss some candidates?",
--	"Did a co-worker give Barbara's boss the candidates?",
--	"Did a co-worker give someone the candidates?",
--	"A co-worker gave Barbara's boss several jobs.",
--	"Did someone give Barbara's boss something?",
--	"A team member gave Tadeusz some ideas.",
--	"A man gave Tadeusz some ideas.",
--	"Someone gave Tadeusz the ideas.",
--	"The job gave Barbara's boss the ideas.",
--	"A man gave someone the job.",
--	"A man gave someone something.",
--	"Someone gave someone something.",
--	"Did a co-worker work at a company?",
--	"A co-worker did work at a company."
--	-- "Did Barbara's boss give a name to Fast-Track?"
	]

wh_questions =[
	"Which website did Mark Zuckerberg found?"
	, "Which website did Steve Fossett found?"
--	"Who worked?",
--	"Who did Eva teach?",
--	"Who taught Eva?",
--	"Who gave the job to Eva?",
--	"Who gave some job to Eva?",
--	"Which person worked?",
--	"Which person did Eva teach?",
--	"What did Dr Bean teach?",
--	-- "Who did Barbara give some job?",
--	"Who did Barbara give some job to?",
--	"Who hoped to get the job?",
--	"Which man hoped to get the job?",
--	"Who was Barbara's boss?",
--	"Who interviewed Barbara's boss?",
--	"Which woman interviewed Barbara's boss?",
--	"Who gave the job to Barbara's boss?",
--	"Who gave some job to Barbara's boss?",
--	"Who was the ambitious administrative assistant?",
--	"Who was the administrative assistant who was ambitious?",
--	"Which person hoped to get the job?",
--	"Which woman appreciated a co-worker?",
--	"Which girl appreciated a co-worker?",
--	"Which boss appreciated a co-worker?",
--	"Who did Barbara's boss appreciate?",
--	"Which person did Barbara's boss appreciate?",
--	"Which man did Barbara's boss appreciate?",
--	"Which woman did Barbara's boss appreciate?",
--	"Which thing did Barbara's boss appreciate?",
--	"Which worker did Barbara's boss appreciate?",
--	-- "To whom did a co-worker give some languages?",
--	-- "Who did a co-worker give some languages to?",
--	"Who had a worker?",
--	"What did Barbara's boss have?",
--	"Who did Barbara's boss have?",
--	"Who did the job disappoint?",
--	"Who did a co-worker's boss appreciate?",
--	"What did a co-worker's boss appreciate?",
--	"Which thing did a co-worker's boss appreciate?",
--	"Which man did a co-worker's boss appreciate?",
--	"Which woman did a co-worker's boss appreciate?",
--	"Which worker did a co-worker's boss appreciate?",
--	"What did someone have?"
	]

--comp_wh_questions = [
--	"Who was ambitious?",
--	"Who was successful?",
--	"Who was a woman?",
--	"Who were women?",
--	"Who was the woman?",
--	"Who was Dr Bean's co-worker?",
--	"Who was Tadeusz's co-worker?",
--	"Who was Barbara's co-worker?",
--	"Who was the ambitious woman that had a boss?",
--	"Who was Barbara's boss?",
--	"Who was Barbara's boss's co-worker?"
--	]
--
--relclauses = [
--	"Barbara was an ambitious woman who was a co-worker.",
--	"Barbara was an ambitious woman who had a boss.",
--	"The ambitious woman who had a boss was Barbara.",
--	"A woman who taught Eva worked.",
--	"The woman who Eva helped worked.",
--	"Did the woman who taught Eva work?",
--	"Did every woman who taught Eva work?",
--	"The woman who gave the job to Eva worked.",
--	"Barbara spoke to the man that she gave the job to.",
--	"Barbara appreciated the man that helped the woman " 
--	 ++ "that was ambitious.",
--	-- "A woman who interviewed Barbara's boss hoped to get the job.",
--	-- "The woman who interviewed Barbara's boss hoped to get the job.",
--	"Did the woman who interviewed Barbara's boss work?",
--	"Did every person who interviewed Barbara's boss work?",
--	"Did some person who interviewed Barbara's boss work?",
--	-- "The woman who gave the job to Barbara's boss hoped to get the job.",
--	"Barbara gave a presentation to the man that gave the job to Barbara's co-worker.",
--	"The man that Barbara gave a presentation to gave the job to Barbara's co-worker.",
--	"The man Barbara gave a presentation to gave the job to Barbara's co-worker.",
--	"The job disappointed the man that gave Barbara's boss the job.",
--	"The man that the job disappointed worked for Poland.",
--	"The man the job disappointed worked for Poland.",
--	"Barbara's boss accepted the job that a man gave Barbara's sales record.",
--	"Barbara's boss accepted the job that a man gave to Barbara's sales record.",
--	"Barbara appreciated the man that gave the girl \
--	 \that worked for Poland a job?"
--	]
--
--
--
--relppR_test = [
--	"A woman with some job worked.",
--	"A woman with no job worked.",
--	"The woman with the job got a job.",
--	"The woman with the job gave a job to Dr Bean.",
--	"A woman who didn't have a job gave a job to Dr Bean.",
--	"The woman with the job gave a job to a co-worker."
--	]

to_inf_test = [
	]

to_inf_test_yn = [
	"Did steve wynn decide to charge   ten dollars ?"
	, "Did steve wynn decide to charge visitors  ten dollars ?"
	, "Did steve wynn decide to charge visitors  an entrance fee ?"
	, "Did steve wynn want to look at the cars?"
	, "Did some people want to look at some cars?"
	, "Did visitors want to look at some cars?"
	, "Did visitors want to buy some cars?"
	, "Did all visitors want to buy a car?"
	, "Did some visitors want to buy a car?"
	, "Did many visitors want to buy a car?"
	, "Didn't many visitors want to buy a car?"
	, "Did visitors who wanted to buy a car have to pay?"
	, "Did visitors who wanted to buy a car have to pay the entrance fee?"
	, "Did visitors have to pay to enter the Ferrari showroom?"
	, "Did visitors who wanted to buy a car have to pay to enter the Ferrari showroom?"
	, "Did visitors who didn't want to buy a car have to pay to enter the Ferrari showroom?"
	, "Did visitors who wanted to look at the cars have to pay to enter the Ferrari showroom?"
	, "Did someone pay to enter the Ferrari showroom?"
	, "Did some visitors pay to enter the Ferrari showroom?"
	, "Did Steve Wynn pay to enter the Ferrari showroom?"
	]

to_inf_test_tag = [
	"Steve Fossett wanted to make money, didn't he?"
	, "Steve Fossett decided to make a website, didn't he?"
	, "Some advertisers wanted to buy advertising space on the Million Dollar Homepage, didn't they?"
	]

to_inf_test_wh = [
	"Who decided to charge visitors an entrance fee?"
	, "Who decided to charge ten dollars?"
	, "Who wanted to look at the cars?"
	, "Who wanted to buy a car?"
	]

punctuation = [
	"Alex_Tew made few money,didn't he?"
	, "Alex_Tew founded facebook,didn't he?"
	]

-- vim: set ts=8 sts=4 sw=4 noet:
