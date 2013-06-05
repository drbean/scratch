module Tests where

import LogicalForm
import Parsing

relatives = [
	"Was Dr Bean a man who bought bananas?"
	, "Was Dr Bean a man who bought oil?"
	, "Was Dr Bean the man who sold bananas?"
	]

haves = [
	"Did Dr Bean have autonomy?"
	, "Did students have autonomy?"
	, "Did some students have autonomy?"
	, "Did all students have autonomy?"
	, "Did students in the compcomp activity have autonomy?"
	, "Did Dr Bean have some students?"
	, "Did Dr Bean have some questions?"
	, "Did Dr Bean have a compcomp activity?"
	, "Did Dr Bean have a framework?"
	, "Did Dr Bean have a successful activity?"
	, "Did Dr Bean have answers?"
	, "Did questions have answers?"
	, "Did students have answers?"
	, "Did all students have answers?"
	, "Didn't students have questions?"
	, "Didn't all students have questions?"
	, "Did some students not have questions?"
	]

framings = [
	"Was autonomy an ingredient for success?"
	, "Was Dr Bean a teacher?"
	, "Were the ingredients for success a framework?"
	, "Did the compcomp activity have a framework?"
	, "Did Dr Bean ask questions about the ingredients for success?"
	]

asks = [
	"Did Dr Bean ask questions?"
	, "Did Dr Bean ask students questions?"
	, "Did Dr Bean ask questions students?"
	, "Did Dr Bean ask questions about the compcomp activity?"
	, "Did Dr Bean ask questions about the students?"
	, "Did students have to ask questions?"
	, "Did students have to ask questions in the activity?"
	, "Did students have to ask questions in some activity?"
	, "Did Dr Bean like questions?"
	, "Did Dr Bean like to ask questions?"
	, "Did Dr Bean like students to ask questions?"
	, "Did Dr Bean like questions to ask students?"
	, "Did Dr Bean want to ask questions?"
	, "Did Dr Bean want to ask students questions?"
	]

likes = [
	"Did Dr Bean like questions?"
	, "Did Dr Bean like to ask questions?"
	, "Did Dr Bean like students to ask questions?"
	, "Did Dr Bean like questions to ask students?"
	, "Did Dr Bean like the activity?"
	, "Didn't Dr Bean like the activity?"
	]

like_s = [
	"Dr Bean liked the activity."
	, "Dr Bean didn't like the activity."
	]

search = [
	"The person who was a teacher didn't like the activity, did he?"
	]

neg_pos_tags = [
	"Dr Bean liked the activity, didn't he?"
	, "Dr Bean didn't like the activity, did he?"
	, "Dr Bean liked all students, didn't he?"
	, "Dr Bean didn't like all students, did he?"
	, "Dr Bean liked all the students, didn't he?"
	, "Dr Bean didn't like all the students, did he?"
	, "The activity was innovative, wasn't it?"
	, "The activity wasn't innovative, was it?"
	]

like_yn = [
	"Did Dr Bean like the activity?"
	, "Didn't Dr Bean like the activity?"
	]

text_test = [
	"Dr Bean asked questions. He was a teacher."
	, "He was a teacher. Dr Bean asked questions."
	]

comp_tests = [
	"Dr Bean was an teacher."
	, "Dr Bean was an ingredient for success."
	, "Dr Bean wasn't an person."
	, "Dr Bean was successful."
	, "Dr Bean wasn't innovative."
	, "Autonomy was an ingredient for success."
	, "Ownership was an ingredient for success."
	, "Innovation was an ingredient for success."
	]

pressure_test = [
	"The group put pressure on the group.",
	"A group didn't put pressure on a group.",
	"The group put pressure on Dr Bean.",
	"The group didn't put pressure on Dr Bean.",
	"Dr Bean put pressure on the group.",
	"Dr Bean didn't put pressure on the group.",
	"Dr Bean didn't put pressure on a group.",
	"Dr Bean put pressure on Dr Bean.",
	"Dr Bean didn't put pressure on Dr Bean.",
	"The group put pressure on Dr Bean.",
	"The group didn't put pressure on Dr Bean.",
	"Dr Bean put pressure on the group.",
	"Dr Bean didn't put pressure on the group.",
	"Dr Bean put pressure on Dr Bean."
	]

sail_test = [
	"Dr Bean sailed around the world.",
	"The world sailed around Dr Bean.",
	"A man sailed around the world.",
	"Dr Bean sailed around the world.",
	"CUSP sailed around the world.",
	"Dr Bean sailed around the world.",
	"Someone sailed around the world.",
	"Dr Bean sailed around the world in a boat.",
	"Dr Bean sailed around the world in a boat.",
	"CUSP sailed around the world in a balloon.",
	"Dr Bean sailed around the world in an aircraft.",
	"Someone sailed around the world in an aircraft."
	]

mod_test = [
	"Dr Bean was a man who sailed around the world."
	, "Dr Bean was the man with a boat."
	, "Dr Bean was the man who sailed around the world."
	, "Dr Bean was the woman who sailed around the world."
	, "Dr Bean was the woman who had a boat."
	, "Dr Bean was the woman with the boat."
	, "Dr Bean was the man who felt stress."
	, "Dr Bean was a man who felt stress."
	, "Dr Bean was a man who felt stress because of lack of control."
	]

possessives = [
	"Dr Bean's group helped Dr Bean."
	, "Did Dr Bean's group talk to Dr Bean?"
	, "Did the group of Dr Bean talk to Dr Bean?"
	, "Dr Bean's stress talked to the group."
	-- , "Dr Bean's group caused Dr Bean's stress"
	, "Did Dr Bean's stress cause stress to Dr Bean's group?"
	, "Did Dr Bean's stress cause stress to Dr Bean's group?"
	, "Dr Bean was an adventurer."
	, "Dr Bean was ambitious."
	, "Stress was because of lack of support."
	, "Dr Bean's stress was because of lack of support?"
	, "The stress of Dr Bean was because of lack of support?"
	, "Was the stress of Dr Bean because of lack of support?"
	, "Was the stress of the ambitious women because of lack of support?"
	, "Was the stress of Dr Bean because of lack of support?"
	, "Was the group of Dr Bean because of lack of support?"
	, "Was the group of Dr Bean because of lack of support?"
	, "Did the group of Dr Bean talk to Dr Bean?"
	, "Did someone talk to Dr Bean?"
	]

talks = [
	"Did Dr Bean speak good English?"
	, "Did Dr Bean talk to students?"
	, "Did Dr Bean talk to students in English?"
	, "Someone talked to Dr Bean."
	, "Did Dr Bean want students to talk?"
	, "Did Dr Bean want students to talk in some activity?"
	, "Did Dr Bean want students to talk in the activity?"
	, "A group talked to Dr Bean."
	, "A group of students talked to Dr Bean."
	, "A group of Dr Bean talked to Dr Bean."
	, "The group of Dr Bean talked to Dr Bean."
	, "Did a stress of Dr Bean talk to Dr Bean?"
	-- , "Did the stress of Dr Bean talk to Dr Bean's group?"
	]

knows = [
	"Did Dr Bean know the answer?"
	, "Did Dr Bean have the answer?"
	, "Someone knew the answer to Dr Bean's question."
	, "A member of a group knew Dr Bean."
	, "The group of Dr Bean knew Dr Bean."
	, "A group of Dr Bean knew Dr Bean."
	, "The group of Dr Bean knew Dr Bean."
	, "Did the stress of Dr Bean know Dr Bean?"
	, "Did a stress of Dr Bean know Dr Bean?"
	-- , "Did the stress of Dr Bean know Dr Bean's group?"
	]

fly_test = [
	"Did Dr Bean fly around the world.",
	"Didn't Dr Bean fly around the world.",
	"Dr Bean flew around the world.",
	"Dr Bean flew around the world.",
	"CUSP flew around the world.",
	"Dr Bean flew around the world.",
	"Someone flew around the world.",
	"Dr Bean flew around the world in a balloon.",
	"Dr Bean flew around the world in a plane.",
	"Dr Bean flew around the world in a boat.",
	"CUSP flew around the world in a balloon.",
	"Dr Bean flew around the world in a plane.",
	"Dr Bean flew around the world in a balloon.",
	"Didn't Dr Bean fly around the world in a balloon.",
	"Someone flew around the world in a plane."
	]

stress_test = [
	"Dr Bean felt stress"
	, "Dr Bean felt stress"
	, "Dr Bean felt stress"
	, "Dr Bean felt stress because of lack of control."
	, "Dr Bean felt stress because of uncertainty."
	, "Dr Bean felt stress because of lack of support."
	, "Dr Bean felt stress because of pressure."
	, "Dr Bean felt stress because of lack of control."
	, "Dr Bean felt stress because of uncertainty."
	, "Dr Bean felt stress because of lack of support."
	, "Dr Bean felt stress because of pressure."
	, "Lack of control caused stress to Dr Bean."
	, "Uncertainty caused stress to Dr Bean."
	, "Lack of support caused stress to Dr Bean."
	, "Pressure caused stress to Dr Bean."
	]

shoes = [

	"Where did Dr Bean see QuanJiaFu?"
	, "Was YingTsni Lu toofar to Dr Bean?"
	, "What kinds of shoes will Dr Bean want buy?"
	, "Did Dr Bean buy the slippers for 800NT?"
	, "How much is Dr Bean willing to pay the shoes?"
	, "How much were the jogging shoes?"
	, "Has Dr Bean seen QuanJiaFu in Toufen and Zhunan?"
	, "Where did Dr Bean buy shoes?"
	, "How much did Dr Bean pay for shoes?"
	, "How much is Dr Bean's budget ?"
	, "How much is Dr Bean willing to pay?"
	, "Did Dr Bean want to buy shoes in JANPAN?"
	, "How much did Dr Bean pay for jogging shoes ?"
	, "What kind of shoes did the store have?"
	, "How many kinds of shoes did shoe store provide?"
	, "Where is  QuanJiaFu ?"
	, "Dr Bean wants to buy the shoes in Miaoli,didn't he?"
	, " What kinds of shoes will Dr Bean choose?"
	, "How much budget did Dr Bean have for buying shoes?"
	, "Did Dr Bean want to buy jogging shoes in QuanJiaFu ?"
	, "What color did  Dr Bean like in shoes?"

	, "Did Dr bean buy an uniform at NET?"
	, "How much will Dr Bean pay for his new uniform?"
	, "What kind of the shoes did the shoes store sale?"
	, "Dr Bean will buy the jogging shoes , didn's he?"
	, "Have you ever see QuanJiafu's advertisement on TV?"
	, "How much is Dr Bean willing to pay for shoes?"
	, "Did Dr Bean go to  QuanJiafu to buy shoes?"
	, "Dr Bean wants to buy eggs, didn't he?"
	, "Did  QuanJiafu give Dr Bean some discounts?"
	, "Dr Bean won't buy the shoes ,will he?"
	, "Where is the  QuanJiafu shoes store?"
	, "Dr Bean has see  QuanJiafu in Tonfen,hasn't he?"
	, "How much did Dr Bean willing to buy the shoes?"
	, "Where did the Dr Bean want to buy the shoes?"
	, "Are those shoes good for teacher?"
	, "How much are the jogging shoes?"
	, "Where did Dr Bean buy the shoes ?"
	, "Which kind of the shoes did Dr  Bean prefer?"
	, "What kind of shoes did Dr Bean choose?"
	, "Dr Bean will go to  QuanJiafu in Toufen won't he?"
	, "Can Dr Bean buy a jogging shoes at  QuanJiafu?"
	, "Did Dr Bean buy a jogging shoes lower than 499NT?"
	, "Dr Bean want to buy shoes in Moiaoli,didn't he?"
	, "Where did Dr Bean want to buy the man's shoes?"
	, "Where did Dr Bean want to buy shoes?"
	, "Did Dr Bean want to pay 1000NT for shoes?"
	, "Did Zoe want to pay 2000 NT to buy a bag?"
	, "Alen always buys drinks in 7-11 ,didid he?"
	, "Did Miaoli have QuanJiafu store?"
	, "Miaoli QuanJiafu store is in YongTsaiLu,isn't it?"
	, "Are the shoes in QuanJiafu cheapest?"
	, "Dr Bean wants to buy the shoes in Hsin Chu ,didn't he?"
	, "Dr Bean is willing to pay 1000NT isn't he?"
	, "Did Dr Bean want to buy the shoes in Miaoli?"
	, "How much dollar did Dr Bean"

	, "Dr Bean is willing to pay"
	, "Where did Dr Bean want to buy the shoes?"
	, "Where did Dr Bean buy the shoes"
	, "Where did Dr Bean want to buy the shoes?"
	, "How much is Dr Bean willing to pay for the shoes?"
	, "Dr Bean bought shoes in the QuanJiaFu store,didn't he?"
	, "How many styles of the shoes can Dr Bean choose?"
	, "Dr Bean wants to buy shoes in QuanJiaFu because it has many kinds of shoes didn't he?"
	, "Did Dr Bean buy shoes in QuanJiaFu store ?"
	, "Did Dr Bean pay more than 1000NT to buy the shoes?"
	, "What kinds of shoes dose Dr Bean want to buy ?"
	, "Did Dr Bean want to buy shoes?"
	, "Where did Dr Bean want to buy shoes?"
	, "Why didn't Dr Bean buy New Balance's shoes ?"
	, "Did Dr Bean buy milk from convenience store ?"
	, "Did Dr Bean buy shoes from the shoes store?"
	, "Did Dr Bean buy shoes on sale?"
	, "How much is Dr Bean willing to pay for the shoes?"
	, "Where dose Dr Bean buy shoes?"
	, "Dr Bean likes to drink expensive,dosen't he?"
	, "How much is Dr Bean willing to pay ?"
	, "Where dose Dr Bean want to buy shoes?"
	, "Why did Dr Bean buy shoes in QuanJiaFu?"
	, "How much is Dr Bean willing to pay?"
	, "Did Dr Bean want to buy the shoes in Miaoli?"
	, "Which shoe is Dr Bean want to buy?"
	, "How much is Dr Bean accept to pay to buy the shoes?"
	, "Did the promotion of shoes in Toufen better than Zunan?"
	, "How much doay Dr Bean willing to pay?"
	, "Where is the shoe store ?"
	, "Did the shoe shore have the women's formal shoes?"


	, "Dr Bean is willing to pay 1,000 NT, isn't he?"
	, "Where did Dr Bean buy the shies?"
	, "Was the shoe store in Ying Tsai u, behind Miao Li train station?"
	, "Where is the shoes store?"
	, "Did Dr Bean want to buy the shoes in Miaoli?"
	, "Whjat kind of shoes did the shoe store sell?"
	, "Are jogging shoes the cheapest kind of all?"
	, "Where did Dr Bean want to buy shoes?"
	, "What kind of shoes did QuanJiaFu sell?"
	, "What price is Dr Bean willing to paY for the shoes?"
	, "did Dr Bean want to buy the shoes in Miaoli?"
	, "How many kinds of shoes in the QuanJiaFu store?"
	, "Are women's formal shoes 1,200Nt?"
	, "Was the shoes store in YingTsaiLu?"
	, "Are women's formal shoes 1,200NT and up?"
	, "Where is hte shoes store?"
	, "Whre will D Bean buy the shoes?"
	, "Did the shoe store sell jogging shoes?"
	, "Where is the QuanJiaFu store?y ?"
	, "Did the shoe store sell shoese?"
	, "The shoe store is situated in YingTsaiLu, isn't it?"
	, "How much is Dr Bean willling to payt?"
	, "Did Dr Bean want to buy shoes in Miaoli?"
	, "Was the shoe store in YingTsaiLu?"
	, "Did the shoes strore hsave children's shoes?"
	, "Did Dr Bean llike buying shoes in Miaoli?"
	, "What kins of shoes are sold in the shoe store?"
	, "Why did Dr Bean look for shoeses?"
	, "wHERE IS THE shoe store?"
	, "Was the shoe store named QuanJiaFu?"
	, "Where did Dr Bean buy the shoes?"
	, "How much are the shoes?"
	]

clothes = [
	"What do you think about Ian's pants?"
	, "Ian's jacket is quite fasion, isn't it?"
	, "How often do you buy new coat?"
	, "Do you have budget when you buy new shoes?"
	, "She bought this shirt because it looked good on her, didn't she?"
	, "Why did she buy this shirt in that shop?"
	, "Where did you buy your red T-shirt?"
	, "His jaket is great and cheap, isn't it?"
	, "Where did you buy this coat?"
	, "Where can we see uniqlo's shop?"
	, "Did uniqlo's clothes cost low prices?"
	, "Where can we see uniqlo's advertising?"
	, "Japanese clothes are cute, aren't they?"
	, "Why do you like to eat sandwitch for breakfast?"
	, "Do you wear a skirt in summer?"
	, "Sandy indulges in buying on the net, didn't she ?"
	, "Why did Sandy like to wear Korean clothes?"
	, "Sandy buys clothes from Korea, didn't she?"
	, "What kinds of food did Sandy like?"
	, "Why did Aslyn buy new clothes so often?"
	, "You bought your clothes by your own money, didn't you?"
	, "Why uniqlo so cheap?"
	, "You usually buy new clothes, don't you?"
	, "What kinds of clothes do you buy?"
	, "Do you usually go to uniqlo to buy clothes?"
	, "How often go you go t uniqlo?"
	, "Why do you want to buy clothes in Zara?"
	, "What is different between Zara and uniqlo ?"
	, "When did you buy this shirt in Zara?"
	, "This pants are very cheap, isn't it ?"
	, "Where is Zara's company ?"
	, "Was uniqlo bound of Zara ?"
	, "Girls like clothes in Zara, don't they ?"
	, "Why can uniqlo be develop from Japan to Taiwan ?"
	, "Why uniqlo is so popular ?"
	, "Has Banana buy uniqlo's clothes ?"
	, "Do you like Zara's clothes ?"
	, "Do you prefer to wear suit or T-shirt ?"
	, "What kind of clothes do you like ?"
	, "How much did you buy it ?"
	, "You have Zara's clothes, don't you ?"
	, "Where did you buy your jacket ?"
	, "Your clothes are expensive, aren't they ?"
	, "How much did you spend when you go shopping?"

	, "What clothes style do you like to buy?"
	, "Do you like  shipping in outlet?"
	, "Where do you buy these clothes?"
	, "You like those clothes,don`t you?"
	, "When do you buy this T-shirt?"
	, "Where do you buy these clothes?"
	, "You like this clothes,don`t you?"
	, "What do those T-shirt make from?"
	, "The coat`s style is nice,isn`t it?"
	, "What kind of skirt do you like?"
	, "You like to shop on eBay,don`t you?"
	, "Who bought this cellphone for you?"
	, "Did you buy the computer ?"
	, "When did you buy this watch?"
	, "You bought book yesterday, didn`t you?"
	, "Where did you buy the pencilbox?"
	, "How often do you shop on the internet?"
	, "Do you like shopping online?"

	, "Do you like shopping online?"
	, "You will buy clothes for your family , won`t you?"
	, "Why do you buy those clothes?"
	, "Do you like to buy cheap products?"
	, "What kind of clothes do you like?"
	, "You like black shirt , don`t you?"
	, "Peter`s T-shirt is cheap, isn`t?"
	, "Why did Peter buy this T-shirt?"
	, "Do you like colorful shirt?"
	, "Zara`s clothes are expensive , aren`t they?"
	, "You like your T-shirt, don`t you?"
	, "What is your T-shirt`s color?"
	, "Was Alice`s T-shirt cheaper than Amy`s?"
	, "Where do you buy this book?"
	, "Why were you buy the cloth ?"
	, "The clothes is cheap or not?"
	, "Why did you buy the jacket?"
	, "Did you like your jacket you buy?"
	, "How much is it?"
	, "Do you have any other size of this T-shirt?"

	, "Where did you buy the jacket?"
	, "Do you like your jacket?"
	, "Are those designer jeans?"
	, "Where do you buy your clothes?"
	, "Do you know the brand of the jackey?"
	, "Where did you buy the pants?"
	, "What color of the clothes will you buy?"
	, "How often do you buy T-Shirts?"
	, "Do you like my coat?"
	, "What's this shirt size?"
	, "What size is your clothes?"
	, "Do you want this cloth?"
	, "Do you like to wash your pants?"
	, "Where do you buy your jeans?"
	, "Why do you choose this color?"
	, "Do you like to wash your jacket?"
	, "Which clothes do you like, cheap or expensive?"
	, "Do you like low price clothes?"
	, "Where did you buy your jeans?"
	, "Do you like my shirt?"
	, "What do you think of Leo's pants?"
	, "Do you like my pants?"
	, "What do you think about my new shoes?"
	, "Do you like my jacket?"
	, "Why did you buy it?"
	, "Do you like my pants?"
	, "Where did you buy the pants?"
	, "Do you like my shoes?"
	, "Why you buy the T-Shirt?"
	, "Do you like your jacket?"
	, "Your jacket is very cheap, isn't it?"
	, "Was the T-shirt on sale?"
	, "Where did you buy the T-shirt?"
	, "What color clothes do you prefer?"
	, "Do you like my T-shirts color?"
	, "Where did you buy your socks?"
	, "What kind of T-Shirts do you like?"


	]

food = [
	"What did you usually buy the breakfast?"
	, "Where do you like to buy your supplies?"
	, "When did you eat the breakfast?"
	, "Where did you buy the breakfast?"
	, "Do you eat breakfast everyday?"
	, "What do you consider to when you buy breakfast?"
	, "Did you eat breakfast today?"
	, "Why did you eat breakfast today?"
	, "Do you think that sandwich is healthy?"
	, "How much did you pay on breakfast?"
	, "Do you think breakfast is good for health?"
	, "What kind of breakfast did you buy?"
	, "How often did you eat breakfast ?"
	, "Yow ate breakfast this morning, didn't you ?"
	, "What ingredients in your breakfast?"
	, "Do you want to eat breakfast with Yuda ?"
	, "Where did you eat your breakfast ?"
	, "How did you eat your breakfast ?"
	, "Did you eat +breakfast with your friends ?"
	, "What time did you eat breakfast ?"
	, "Do you think that eating breakfast is healthy ?"
	, "How did you know the new product at breakfast shop?"
	, "Do you like breakfast?"
	, "Where did you usually eat breakfast, in the shop or take out?"
	]

wh_test = [
	"What did Gr Bean think about the rice?"
	, "How often did Dr Bean buy rice?"
	, "Why did he buy the rice at QuanLian?"
	, "Where did buy the rice?"
	, "Where can we buy rice?"
	, "Where can we see QuanLian's promotion?"
	, "Why did Dr Bean like to buy rice?"
	, "Why did Dr Bean like to buy QuanLian's rice?"
	, "What kinds of rice did Dr Bean like?"
	, "What kinds of rice did Dr Bean buy?"
	, "Why did Dr Bean buy rice so often?"
	, "Why was QuanLian's rice so cheap?"
	, "How often did Dr Bean go to QuanLian?"
	, "Why did Dr Bean want to buy rice from QuanLian?"
	, "What is the difference between QuanLian and QuanJiaFu?"
	, "When did Dr Bean buy the rice from QuanLian?"
	, "Where is QuanLian?"
	, "Why is rice so popular?"
	, "How much rice did Dr Bean buy?"
	, "Where did Dr Bean buy his rice?"
	, "How much did Dr Bean spend, shopping?"
	, "What shoes did Dr Bean buy?"
	, "Where did Dr Bean like to buy his shoes?"
	, "When did Dr Bean buy shoes?"
	, "Where did Dr Bean buy shoes?"
	, "What did Dr Bean consider when he buys shoes?"
	, "Why did Dr Bean buy shoes today?"
	, "How much did Dr Bean pay for his shoes?"
	, "What kind of oil did Dr Bean buy?"
	, "How often did Dr Bean buy oil ?"
	, "What ingredients in Dr Beanr breakfast?"
	, "Where did Dr Bean buy his oil?"
	, "How did Dr Bean buy oil?"
	, "What time did Dr Bean buy oil?"
	, "How did Dr Bean know about the oil at QuanLian?"
	, "Where did Dr Bean buy oil, at QuanLian or QuanJiaFu?"
	]


yn_test = [
	"Was YingTsaiLu too far for Dr Bean?"
	, "Did Dr Bean see QuanJiaFu in Zhunan?"
	, "Did Dr Bean have money to buy shoes?"
	, "Did Dr Bean buy shoes?"
	, "Did Dr Bean buy shoes from a shoe store?"
	, "Did Dr Bean buy shoes from the shoe store?"
	, "Did Dr Bean buy shoes at QuanJiaFu?"
	, "Did Dr Bean buy shoes in QuanJiafu?"
	, "Did Dr Bean buy shoes in Miaoli?"
	, "Did Dr Bean buy QuanJiaFu's shoes?"
	, "Did Dr Bean buy women's formal shoes?"
	, "Did Dr Bean buy women's_formal_shoes?"
	, "Did Dr Bean buy shoes from QuanJiaFu?"
	, "Did Dr Bean want to buy shoes?"
	, "Did Dr Bean want to buy shoes from QuanJiaFu?"
	, "Did Dr Bean want to buy shoes at QuanJiaFu?"
	, "Did Dr Bean want to buy shoes in QuanJiaFu?"
	, "Did Dr Bean want to buy shoes in Miaoli?"
	, "Did Dr Bean want to buy the shoes in Miaoli?"
	, "Did Dr Bean want to buy shoes in Toufen?"
	, "Did Dr Bean want to buy jogging shoes in QuanJiaFu ?"
	, "Did Dr Bean go to  QuanJiafu to buy shoes?"
	, "Were the shoes good for a teacher?"
	, "Did Dr Bean buy jogging shoes in QuanJiafu?"
	, "Did Dr Bean buy jogging shoes at QuanJiafu?"
	, "Did Miaoli have a QuanJiaFu shoe store?"
	, "Did Dr Bean buy shoes in the QuanJiaFu shoe store?"
	, "Was the promotion in Toufen good?"
	, "Did the shoe store have the women's formal shoes?"
	, "Were women's formal shoes 1,000 NT and up?"
	, "Was the shoe store in YingTsaiLu?"
	, "Were women's formal shoes 1,200 NT and up?"
	, "Did the shoe store sell jogging shoes?"
	, "Did the shoe store sell shoes?"
	, "Was the shoe store in YingTsaiLu?"
	, "Did Dr Bean go to QuanJiaFu?"
	, "Did Dr Bean go to QuanJiaFu to buy shoes?"
	, "Did Dr Bean like shoes?"
	, "Did Dr Bean like QuanJiaFu's shoes?"
	, "Did Dr Bean like women's formal shoes?"
	, "Did Dr Bean like shoes from QuanJiaFu?"
	, "Did Dr Bean like the jogging shoes from QuanJiaFu?"
	, "Did Dr Bean like the shoes from QuanJiaFu?"
	, "Did people buy slippers at Dr Bean's shoe store?"
	, "Did people buy slippers in Dr Bean's shoe store?"
	-- , "Did Dr Bean prefer to buy shoes or milk?"
	-- , "Did Dr Bean think that buying shoes is good?"
	-- , "Did Dr Bean think that shoes is good for health?"
	-- , "Did Dr Bean think that eating shoes is healthy ?"
	-- , "Was the promotion in Toufen better than Zhunan?"
	-- , "Did Dr Bean buy jogging shoes cheaper than 499NT?"
	-- , "Did Dr Bean want to pay 1000NT for shoes?"
	-- , "Did the shoes store have children's shoes?"
	-- , "Did Dr Bean buy the slippers for 800NT?"
	-- , "Did Dr Bean buy shoes on sale?"
	-- , "Did  QuanJiafu give Dr Bean some discount?"
	-- , "Were the shoes in QuanJiaFu cheap?"
	-- , "Did Dr Bean pay more than 1000NT to buy the shoes?"
	-- , "Were jogging shoes the cheapest?"
	-- , "Did Dr Bean like buying shoes in Miaoli?"
	-- , "Was the shoe store named QuanJiaFu?"
	]

tag_test = [
	"Dr Bean talked to students in English, didn't he?"
	, "Dr Bean wanted to buy shoes at QuanJiaFu, didn't he?"
	, "Dr Bean wanted to buy shoes in QuanJiaFu, didn't he?"
	, "Dr Bean wanted to buy shoes in Miaoli, didn't he?"
	, "Dr Bean wanted to buy the shoes in Miaoli, didn't he?"
	, "Dr Bean wanted to buy shoes in Toufen, didn't he?"
	, "Dr Bean wanted to buy jogging shoes in QuanJiaFu, didn't he?"
	, "The shoes were good, weren't they?"
	, "Dr Bean liked to buy shoes, didn't he?"
	, "Dr Bean bought shoes from QuanLian, didn't he?"
	, "Dr Bean bought shoes with his own money, didn't he?"
	, "Dr Bean bought old shoes, didn't he?"
	, "The shoes were very cheap, weren't they?"
	, "Everyone liked shoes from QuanLian, didn't they?"
	, "Dr Bean had QuanLian's shoes, didn't he?"
	, "Dr Bean's shoes were cheap, weren't they?"
	, "Dr Bean bought shoes this week, didn't he ?"
	, "He bought the shoes because they were cheap, didn't he?"
	, "QuanLian's shoes were cheap, weren't they?"
	, "His shoes were good and cheap, were't they?"
	]

