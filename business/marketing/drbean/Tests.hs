module Tests where

import LogicalForm
import Parsing

relatives = [
	"Was Dr Bean a man who bought bananas?"
	, "Was Dr Bean a man who bought oil?"
	, "Was Dr Bean the man who sold bananas?"
	]

haves = [
	"Did Dr Bean have money?"
	, "Dr Bean had rice."
	, "Dr Bean had some rice."
	, "Did Dr Bean have some bananas?"
	, "Did Dr Bean have some eggs?"
	, "Did Dr Bean have some oil?"
	, "Did Dr Bean have oil?"
	, "Did Dr Bean have milk?"
	, "Did Dr Bean have some milk?"
	, "Did Dr Bean have shoes?"
	, "Did Quanjiafu have shoes?"
	, "Did a fruit store have bananas?"
	]

buys_sells = [
	"Did Dr Bean buy?"
	, "Did Dr Bean buy from Quanlian?"
	, "Did Dr Bean buy bananas?"
	, "Did Dr Bean buy bananas from Quanlian?"
	, "Did Dr Bean want to buy from Quanlian?"
	, "Dr Bean wanted to buy bananas from Quanlian."
	, "Dr Bean wanted to buy bananas from Quanlian because of the price."
	, "Did Quanlian sell bananas to Dr Bean?"
	, "Did a fruit store sell bananas to Dr Bean?"
	, "Did Quanlian sell to Dr Bean?"
	, "Did Quanlian sell bananas?"
	, "Did Dr Bean sell bananas?"
	, "Did Quanjiafu sell bananas?"
	, "Did Quanlian sell bananas to Dr Bean because of the price?"
	, "Did Quanjiafu sell shoes?"
	]

text_test = [
	"Dr Bean sailed around the world. He was an adventurer."
	, "He was an adventurer. Dr Bean sailed around the world."
	, "Dr Bean sailed around the world. She was an adventurer."
	, "Dr Bean sailed around the world. He was an adventurer."
	, "Dr Bean sailed around the world. She was a woman, but she was an adventurer."
	, "Dr Bean sailed around the world. She wasn't a man, but she was an adventurer."
	]

comp_tests = [
	"Lack of support was stressful."
	, "Uncertainty was stressful."
	, "Lack of control was stressful."
	, "Pressure was stressful."
	, "Dr Bean was a man."
	, "Dr Bean was an adventurer."
	, "Dr Bean was a woman."
	, "Dr Bean was an adventurer."
	, "Dr Bean was a man."
	, "Dr Bean wasn't a woman."
	, "Dr Bean wasn't a woman."
	, "Dr Bean wasn't an adventurer."
	, "Dr Bean was ambitious."
	, "Dr Bean wasn't stressful."
	]

pressure_test = [
	"The team put pressure on the team.",
	"A team didn't put pressure on a team.",
	"The team put pressure on Dr Bean.",
	"The team didn't put pressure on Dr Bean.",
	"Dr Bean put pressure on the team.",
	"Dr Bean didn't put pressure on the team.",
	"Dr Bean didn't put pressure on a team.",
	"Dr Bean put pressure on Dr Bean.",
	"Dr Bean didn't put pressure on Dr Bean.",
	"The team put pressure on Dr Bean.",
	"The team didn't put pressure on Dr Bean.",
	"Dr Bean put pressure on the team.",
	"Dr Bean didn't put pressure on the team.",
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
	"Dr Bean's team helped Dr Bean."
	, "Did Dr Bean's team talk to Dr Bean?"
	, "Did the team of Dr Bean talk to Dr Bean?"
	, "Dr Bean's stress talked to the team."
	-- , "Dr Bean's team caused Dr Bean's stress"
	, "Did Dr Bean's stress cause stress to Dr Bean's team?"
	, "Did Dr Bean's stress cause stress to Dr Bean's team?"
	, "Dr Bean was an adventurer."
	, "Dr Bean was ambitious."
	, "Stress was because of lack of support."
	, "Dr Bean's stress was because of lack of support?"
	, "The stress of Dr Bean was because of lack of support?"
	, "Was the stress of Dr Bean because of lack of support?"
	, "Was the stress of the ambitious women because of lack of support?"
	, "Was the stress of Dr Bean because of lack of support?"
	, "Was the team of Dr Bean because of lack of support?"
	, "Was the team of Dr Bean because of lack of support?"
	, "Did the team of Dr Bean talk to Dr Bean?"
	, "Did someone talk to Dr Bean?"
	]

talks = [
	"Did Dr Bean talk to Dr Bean?"
	, "Someone talked to Dr Bean."
	, "A team talked to Dr Bean."
	, "The team of Dr Bean talked to Dr Bean."
	, "A team of Dr Bean talked to Dr Bean."
	, "The team of Dr Bean talked to Dr Bean."
	, "Did the stress of Dr Bean talk to Dr Bean?"
	, "Did a stress of Dr Bean talk to Dr Bean?"
	-- , "Did the stress of Dr Bean talk to Dr Bean's team?"
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
	, "Does uniqlo's clothes cost low prices?"
	, "Where can we see uniqlo's advertising?"
	, "Japanese clothes are cute, aren't they?"
	, "Why do you like to eat sandwitch for breakfast?"
	, "Do you wear a skirt in summer?"
	, "Sandy indulges in buying on the net, doesn't she ?"
	, "Why does Sandy like to wear Korean clothes?"
	, "Sandy buys clothes from Korea, doesn't she?"
	, "What kinds of food does Sandy like?"
	, "Why does Aslyn buy new clothes so often?"
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
	, "Is uniqlo bound of Zara ?"
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
	, "Is Alice`s T-shirt cheaper than Amy`s?"
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
	, "Is the T-shirt on sale?"
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
	"What did Dr Bean think about the rice?"
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
	"Did Dr Bean have money to buy rice?"
	, "Did Dr Bean buy rice at QuanLian?"
	, "Did Dr Bean go to QuanLian?"
	, "Did Dr Bean go to QuanLian to buy rice?"
	, "Did Dr Bean buy QuanLian's rice?"
	, "Did Dr Bean like QuanLian's rice?"
	, "Did Dr Bean buy shoes?"
	, "Did Dr Bean buy shoes from QuanJiaFu?"
	, "Did Dr Bean want to buy shoes from QuanJiaFu?"
	, "Did Dr Bean want to buy oil at QuanLian?"
	, "Did Dr Bean like oil?"
	, "Did Dr Bean like rice?"
	, "Did Dr Bean like oil from QuanLian?"
	, "Did Dr Bean like the oil from QuanLian?"
	, "Did Dr Bean like the rice from QuanLian?"
	-- , "Did Dr Bean prefer to buy rice or milk?"
	-- , "Did Dr Bean think that buying shoes is good?"
	-- , "Did Dr Bean think that oil is good for health?"
	-- , "Did Dr Bean think that eating oil is healthy ?"
	]

tag_test = [
	"The rice is quite good, isn't it?"
	, "He bought the rice because it was cheap, didn't he?"
	, "His rice was good and cheap, wasn't it?"
	, "QuanLian's rice is cheap, isn't it?"
	, "Dr Bean likes to buy rice, didn't he?"
	, "Dr Bean buys rice from QuanLian, didn't he?"
	, "Dr Bean bought rice with his own money, didn't he?"
	, "Dr Bean buys old rice, didn't he?"
	, "The rice is very cheap, isn't it?"
	, "Everyone likes rice from QuanLian, don't they?"
	, "Dr Bean has QuanLian's rice, didn't he?"
	, "Dr Bean's rice is cheap, isn't it?"
	, "Dr Bean bought oil this week, didn't he ?"
	]

