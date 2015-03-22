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
--
--comp_test1 = [
--	"Barbara was strong.",
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
--	]
--
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
--
tag_test = [
	"Michelle wanted the website to have a lot of images, didn't she.",
	"Some woman was a manager, wasn't she.",
	"Some man was a website designer, wasn't she.",
	"Michelle wanted to pay Larry 6,000 dollars, didn't she?"
	]

--neg_tag_test = [
--	"Barbara didn't work, did she.",
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
--	]
--
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
--
--haves = [
--	"Did Barbara have a co-worker?",
--	"Did Barbara have a boss?",
--	"Did Barbara have a strong personality?",
--	"Did Barbara's boss have a co-worker?",
--	"Did Eva have a job?",
--	"Did Eva have some job?",
--	"Did Eva have sales experience?",
--	"Did Barbara's boss have sales experience?",
--	"Did Eva have sales experience?",
--	"Did Tadeusz have a boss?",
--	"Did Tadeusz have some bosses?",
--	"Did Tadeusz have bosses?",
--	"Did Tadeusz's boss have a boss?",
--	"Did Tadeusz's boss have some bosses?",
--	"Did Tadeusz's boss have bosses?",
--	"Did Tadeusz have work?",
--	"Did Eva work?",
--	"Did Eva have work?",
--	"Did Eva have a job?",
--	"Did Barbara have work?",
--	"Did Barbara have a job?",
--	"Did Barbara's boss have work?",
--	"Did a co-worker have a job?",
--	"Did a co-worker have a woman's job?",
--	"Did a co-worker have Barbara's job?",
--	"Did a co-worker have a co-worker?",
--	"Did a co-worker have a boss?",
--	-- "Did a job have a salary?",
--	"Did an ambitious woman have sales experience?",
--	"Did an ambitious woman have some sales experience?",
--	"Did an ambitious woman's boss have some sales experience?",
--	"Did an ambitious woman's boss have a job?",
--	"Did the boss have some sales experience?",
--	"Did the boss have no sales experience?",
--	"Eva's boss had many jobs in Poland.",
--	"Did the boss have some job?",
--	"Did the boss have no job?",
--	"Did Poland have sales experience?",
--	"Did the United States have sales experience?",
--	"Did Fast-Track have sales experience?",
--	"Did Barbara's boss have sales experience?",
--	"Did a co-worker have a boss?",
--	"Did a co-worker have some bosses?",
--	"Did a co-worker have bosses?",
--	"Did Fast-Track have a boss?",
--	"Did Fast-Track have some bosses?",
--	"Did Fast-Track have bosses?",
--	"Did a co-worker have a worker?",
--	"Did Barbara's boss have a worker?",
--	"Did Fast-Track have a worker?",
--	"Did someone have a worker?"
--	]
--
--ungrammatical = [
--	"Did Eva worked?",
--	"Barbara work?",
--	"Man worked.",
--	"Some man work.",
--	"No worked.",
--	"No-one work.",
--	"Did Eva taught?",
--	"Eva teach Barbara.",
--	"Barbara taught.",
--	-- "Did Barbara's boss hoped to get the job?",
--	-- "Man hoped to get the job.",
--	"Some man work.",
--	-- "No hoped to get the job.",
--	"No-one work.",
--	"Did Barbara's boss taught?",
--	"Barbara's boss teach a co-worker.",
--	"A co-worker interviewed."
--	]
--
--intransitives = [
--	"Eva worked",
--	"Did Eva work?",
--	"Barbara's boss worked.",
--	"A woman's boss worked.",
--	"Did Barbara's boss work?",
--	"Did a woman's boss work?",
--	"A man worked.",
--	"Some man worked.",
--	"No one worked.",
--	"No-one worked.",
--	"Everybody worked.",
--	"Everyone worked.",
--	"Many persons worked.",
--	"No person worked.",
--	"Did the man work?",
--	"Did some man work?",
--	"Did some men work?",
--	"Did some woman work?",
--	"Did some women work?",
--	"Most women worked.",
--	"Most women didn't work.",
--	"Several women worked.",
--	"Several women didn't work.",
--	"Many women worked.",
--	"Many women didn't work.",
--	"All women worked.",
--	"No woman worked.",
--	"Most men worked.",
--	"Most men didn't work.",
--	"Several men worked.",
--	"Several men didn't work.",
--	"Many men worked.",
--	"Many men didn't work.",
--	"All men worked.",
--	"No man worked.",
--	"Did Barbara work in Poland?",
--	"Did Barbara work for Poland?",
--	"Did Eva work for Poland?",
--	"Did Eva work at a company?",
--	"Did Barbara's boss work for Poland?",
--	"Did Barbara's boss work?",
--	"Did the co-worker work?",
--	"Did Barbara's boss work?",
--	"A man worked.",
--	"Some man worked.",
--	"No one worked.",
--	"No-one worked.",
--	"Everybody worked.",
--	"Everyone worked.",
--	"Many persons worked.",
--	"No person worked.",
--	"Did the man work?",
--	"Did some man work?",
--	"Did some men work?",
--	"Did some woman work?",
--	"Did some women work?",
--	"Most persons worked.",
--	"Most men worked.",
--	"Most men didn't work.",
--	"Several men worked.",
--	"Several men didn't work.",
--	"Several persons worked.",
--	"Several persons didn't work.",
--	"Did Barbara's boss work?",
--	"Did a co-worker work?",
--	"Did Barbara work?",
--	-- "A man hoped to get the job.",
--	-- "Some man hoped to get the job.",
--	-- "No one hoped to get the job.",
--	-- "No-one hoped to get the job.",
--	-- "Everybody hoped to get the job.",
--	-- "Everyone hoped to get the job.",
--	-- "Many persons hoped to get the job.",
--	-- "No person hoped to get the job.",
--	"Did the man work?",
--	"Did the girl work?",
--	"Did some man work?",
--	"Did some men work?",
--	"Did some woman work?",
--	"Did some women work?",
--	-- "Most men hoped to get the job.",
--	"Most men didn't work.",
--	-- "Several men hoped to get the job.",
--	"Several men didn't work.",
--	-- "Many men hoped to get the job.",
--	"Many men didn't work."
--	-- "All men hoped to get the job.",
--	-- "No man hoped to get the job."
--	]
--
transitives = [
	"Did Larry finish the website."
--	"Did Eva study?",
--	"Did Eva study IT?",
--	"Eva studied marketing.",
--	"Barbara studied marketing.",
--	"Barbara studied marketing in Poland.",
--	"Barbara studied marketing at Poland.",
--	"Did Barbara go to Poland?",
--	"Some woman went to Poland.",
--	"Some man went to Poland.",
--	"Some boy went to Poland.",
--	"Some man interviewed Barbara.",
--	"A man interviewed Eva",
--	"Some woman gave a presentation.",
--	"Did a co-worker work in the Poland?",
--	"Did a co-worker work in the United States?",
--	"A co-worker worked in the United States.",
--	"A co-worker worked at the United States.",
--	"Did a co-worker work at a company?",
--	"A co-worker worked on a company.",
--	"A co-worker worked in a company.",
--	"A co-worker worked as a co-worker.",
--	"Did the job work at a company?",
--	"The job worked on a company.",
--	"The job worked in a company.",
--	"Did the job disappoint a co-worker?",
--	"Did Barbara's boss study marketing?",
--	"Barbara's boss studied marketing.",
--	"A co-worker studied marketing.",
--	"Did a co-worker study in Poland?",
--	"A co-worker studied marketing at a company.",
--	"A co-worker studied marketing at the company.",
--	"Did a co-worker go to the company.",
--	"Some woman went to the company.",
--	"Some man went to the company.",
--	"Some boy went to the company.",
--	"Some man interviewed a co-worker.",
--	"A man interviewed Barbara's boss",
--	"Some woman gave a presentation.",
--	"Did a co-worker come from Poland?",
--	"Did Barbara immigrate?",
--	"Did a co-worker immigrate to Poland?",
--	"Did Barbara go to Poland?",
--	"Did Barbara come from Poland?",
--	"Did Barbara's boss come to Poland?"
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
	"Larry wanted Michelle to pay 6,000 dollars.",
	"Larry wanted Michelle to pay 50 dollars an hour.",
	"Michelle wanted to pay Larry.",
	"Michelle wanted to pay Larry 6,000 dollars.",
	"Larry wanted to finish the website in two months.",
	"Michelle wanted Larry to finish the website in one month.",
	"Larry wanted to charge Michelle 50 dollars an hour.",
	"Larry was the person who wanted to charge Michelle 50 dollars an hour."
	]

to_inf_test_yn = [
	"Did Larry want Michelle to pay 6,000 dollars.",
	"Did Larry want Michelle to pay 50 dollars an hour.",
	"Did Michelle want to pay Larry.",
	"Did Michelle want to pay Larry 6,000 dollars.",
	"Did Larry want to finish the website in two months.",
	"Did Michelle want Larry to finish the website in one month.",
	"Did Larry want to charge Michelle 50 dollars an hour.",
	"Was Larry the person who wanted to charge Michelle 50 dollars an hour?"
	]

to_inf_test_tag = [
	"Larry wanted Michelle to pay 6,000 dollars, didn't he?",
	"Larry wanted Michelle to pay 50 dollars an hour, didn't he?",
	"Michelle wanted to pay Larry, didn't she.",
	"Michelle wanted to pay Larry 6,000 dollars, didn't she.",
	"Larry wanted to finish the website in two months, didn't he.",
	"Michelle wanted Larry to finish the website in one month, didn't she.",
	"Larry wanted to charge Michelle 50 dollars an hour, didn't he."
	]

to_inf_test_wh = [
	"Who wanted Michelle to pay Larry 6,000 dollars?",
	"Who wanted to charge 50 dollars an hour?",
	"Who wanted to charge Michelle 50 dollars an hour?",
	"Who wanted Larry to charge Michelle 50 dollars an hour?",
	"Who did Michelle want to pay?",
	"Who did Michelle want to pay 6,000 dollars?",
	"Who did Michelle want to finish the website?",
	"How much did Larry want to charge michelle?",
	"Who wanted 50 dollars an hour to finish the website in one month?"
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
