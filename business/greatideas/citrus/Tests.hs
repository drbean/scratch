module Tests where

import LogicalForm
import Parsing

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
	"The experiment was successful."
	, "Was the experiment successful?"
	, "Pepsi was successful."
	, "Was Pepsi successful?"
	, "Was the Punjabi government successful?"
	, "The Punjabi government was successful."
--	"Barbara was aggressive.",
--	"Barbara was a sales representative.",
--	"Barbara was Eva's co-worker.",
--	"Barbara was a successful woman.",
--	"Barbara was a man.",
--	"Barbara was a woman.",
--	"Barbara was a sales manager",
--	"A man was Tadeusz.",
--	"A woman was Eva.",
--	"The woman was Eva.",
--	"Barbara was a German woman.",
--	"Barbara was the German woman.",
--	"A German woman was Barbara.",
--	"A ambitious woman was Barbara.",
--	"The ambitious woman was Barbara.",
--	"The German woman was Barbara.",
--	-- "The successful German woman was Barbara.",
--	-- "Barbara was the successful German woman.",
--	"Dr Bean was the German woman's co-worker.",
--	"Dr Bean was the German woman's boss.",
--	"Dr Bean was a German woman's boss.",
--	"Dr Bean was an ambitious woman's boss.",
--	"Dr Bean was an ambitious woman's co-worker.",
--	"Dr Bean was the ambitious women's co-worker.",
--	"Eva was the ambitious women's co-worker.",
--	"Eva was an ambitious woman's co-worker.",
--	"Eva was the German woman's co-worker.",
--	"Eva was a German woman's co-worker.",
--	"Eva was Barbara's co-worker.",
--	"Dr Bean was Barbara's co-worker.",
--	"Dr Bean was Tadeusz's co-worker.",
--	"Dr Bean was Barbara's boss.",
--	"Dr Bean was Eva's boss.",
--	"Barbara was Eva's boss.",
--	"Barbara was Eva's co-worker.",
--	"Barbara was Dr Bean's co-worker.",
--	"Some women were ambitious.",
--	"Dr Bean was a co-worker and Eva was a co-worker.",
--	"Dr Bean was a woman and Eva was a woman",
--	"Some co-workers were women.",
--	"Some women were co-workers.",
--	"All co-workers were women.",
--	"All women were co-workers.",
--	"All ambitious women were successful."
	]

--comp_test2 = [
--	"Some woman was ambitious.",
--	"Some women was successful.",
--	"Some women were successful.",
--	"Eva was a woman.",
--	"Was Eva a woman?",
--	"Were some women co-workers?",
--	"Was Tadeusz's co-worker a woman?",
--	"Eva was Dr Bean's co-worker.",
--	"Eva was Tadeusz's co-worker.",
--	"Eva was Barbara's co-worker.",
--	"Barbara was an ambitious woman who was a co-worker.",
--	"Barbara was an ambitious woman who had a boss.",
--	"Barbara was an ambitious woman with a boss.",
--	"Barbara was an ambitious woman from a boss.",
--	"The ambitious woman who had a boss was Barbara.",
--	"Barbara had a boss.",
--	"Barbara was an ambitious woman.",
--	"Barbara was a woman who was ambitious.",
--	"Tadeusz was Barbara's boss.",
--	"Barbara was Tadeusz's boss.",
--	"Tadeusz was Barbara's co-worker."
--	]

comp_test_wh = [
	"Who were people?"
	, "Who was Pepsi?"
	, "Who was a man?"
	, "Who was a child?"
	, "Who was a woman?"
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

takes = [
	"Did the Punjabi government take Punjabi farmers' land?"
	, "Did the Punjabi government take land from Punjabi farmers?"
	, "The Punjabi government took a land from Punjabi farmers."
	]

wants = [
	"Did Pepsi want citrus_fruit from Punjabi Farmers."
	, "Did Pepsi want oranges?"
	, "Pepsi wanted oranges."
	]

buys_sells = [
	"Did Pepsi buy?"
	, "Did Pepsi buy from Punjabi farmers?"
	, "Did Pepsi buy oranges?"
	, "Did Pepsi buy oranges from Punjabi farmers?"
	, "Did Pepsi want to buy from Punjabi farmers?"
	, "Pepsi wanted to buy from Punjabi farmers?"
	, "Pepsi wanted to buy oranges from Punjabi farmers?"
	, "Did Punjabi farmers sell to Pepsi?"
	, "Did Punjabi farmers sell oranges?"
	, "Did Punjabi farmers sell oranges to Pepsi?"
	]

grows = [
	"Did Punjabi farmers grow citrus fruit?"
	, "Did the Punjabi government grow citrus fruit?"
	, "Did Pepsi want Punjabi farmers to grow citrus fruit?"
	, "Did Pepsi want the Punjabi government to grow citrus fruit?"
	, "Did Punjabi farmers want to grow citrus fruit?"
	, "Did the Punjabi government want to grow citrus fruit?"
	]

haves = [
	"Did visitors have to pay the entrance fee?",
	"Didn't some visitors have to pay the entrance fee?",
	"Did every visitor have to pay the entrance fee?",
	"Did children have to pay the entrance fee?",
	"Did visitors with children have to pay the entrance fee?",
	"Did people with children have to pay the entrance fee?",
	"Did Steve Wynn have cars?",
	"Did Steve Wynn have the Ferrari showroom?",
	"Did the Ferrari showroom have cars?",
	"Did the Ferrari showroom have many visitors?",
	"Did the woman have ten dollars?"
--	"Did someone have a worker?"
	]

knows = [
	"Did Pepsi know the Punjabi government?",
	"Did people know the Punjabi government?",
	"The Punjabi government knew about Pepsi.",
	"Did the Punjabi government know about Pepsi?",
	"Did people know about the Punjabi government?",
	"Did Punjabi farmers know about Pepsi?",
	"Did the woman know Pepsi?",
	"Did the woman know about Pepsi?"
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
--
wh_questions =[
	"Who did Michelle pay?"
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
	"Steve Wynn decided to charge ten dollars, didn't he?"
	, "Steve Wynn decided to charge an entrance fee, didn't he?"
	, "Some visitors wanted to buy a car, didn't they?"
	]

to_inf_test_wh = [
	"Who decided to charge visitors an entrance fee?"
	, "Who decided to charge ten dollars?"
	, "Who wanted to look at the cars?"
	, "Who wanted to buy a car?"
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

lf70 = ( \x -> ( Conj [ (Rel "boss" [x]), (Rel "have" [x, Const (ents !! 8)]) ] ) ) (Const (ents !! 12) )
lf71 = ( \x -> ( Conj [ (Rel "boss" [x]), (Rel "have" [x, Const (ents !! 17)]) ] ) ) (Const (ents !! 12) )
lf72 = ( \x -> ( Conj [ (Rel "boss" [x]), (Rel "have" [x, Const (ents !! 17)]) ] ) ) (Const (ents !! 12) )
lf73 = \x -> Conj [ (Rel "boss" [x]), (Rel "have" [x, Const (ents !! 17)]) ]
lf74 = ( \x -> ( Conj [ (Rel "boss" [x]), (Rel "have" [x, Const (ents !! 17)]) ] ) )
lf75 = \x -> Impl (Rel "boss" [x]) (Rel "have" [x, Const (ents !! 17)])
