module Story_Interpretation where 

import Model

story_objects = [
	( "son",	\[x]	-> daughter x	),
	( "mother",	\[x]	-> isMother x	),
	( "father",	\[x]	-> isFather x	),

	( "disability",	\[x] -> disability x	),
	( "accident",	\[x]	-> accident x	),
	( "surfing_accident",	\[x]	-> surfing_accident x	),
	( "body",	\[x]	-> body x	),
	( "device",	\[x]	-> device x	),
	( "beach",	\[x]	-> beach x	),
	( "horse",	\[x]	-> horse x	),
	-- ( "friends",	\[x]	-> friends x	),
	( "lack_of_depression",	\[x]	-> lack_of_depression x	),
	( "appreciation",	\[x]	-> appreciation x	),
	( "state",	\[x]	-> state x	),
	( "hometown",	\[x]	-> hometown x	),
	( "neck",	\[x]	-> neck x	),
	( "water_sports",	\[x]	-> water_sports x	),
	( "surfing",	\[x]	-> surfing x	),
	( "part",	\[x]	-> part x	),
	( "ventilator",	\[x]	-> ventilator x	),
	( "bowel_movement",	\[x]	-> bowel_movement x	),

	( "story",	\[x] -> story x	)
	]

story_inflections = [
 ( "use", "used" ),
 ( "feel", "felt" ),
 ( "come", "came" ),
 ( "grow_up", "grew_up" ),
 ( "live", "lived" ),
 ( "took_care_of", "take_care_of" ),
 ( "looked_after", "look_after" ),
 ( "cared_for", "care_for" ),
 ( "help", "helped" ),
 ( "rely", "relied" ),
 ( "thanked", "thank" )
 ]

story_relations = [
	( "broken",	\[x]	-> broken x	),
	( "paralyzed",	\[x]	-> paralyzed x	),
	( "resuscitated",	\[x]	-> resuscitated x	),
	( "brain-damaged",	\[x]	-> brain_damaged x	),

	( "disabled",	\[x]	-> disabled x	),
	( "mentally-disabled",	\[x]	-> mentally_disabled x	),
	( "physically-disabled",	\[x]	-> physically_disabled x	),

	( "used",	\[x,y] -> used y x	),
	( "felt",	\[x,y] -> felt y x	),
	( "came",	\[x,y]	-> come_from y x	),
	( "grew_up",	\[x,y]	-> grew_up_in y x	),
	( "lived",	\args -> case args of
		[x,y] -> (lived_with y x || lived_in y x)
		[x,y,z] -> (lived_with_in z y x || lived_with_for z y x)
		[x,y,z,v] -> lived_with_in_for v z y x ),
	( "helped",	\[x,y]	-> helped y x	),
	( "relied",	\[x,y]	-> relied_on y x	),
	( "take_care_of",	\args -> case args of
		[x,y]	-> take_care_of y x
		[x,y,z]	-> take_care_of_after z y x	),
	( "look_after",	\args -> case args of
		[x,y]	-> look_after y x
		[x,y,z]	-> look_after_after z y x	),
	( "care_for",	\args -> case args of
		[x,y]	-> care_for y x
		[x,y,z]	-> care_for_after z y x	),
	( "appreciate",	\[x,y]	-> appreciate y x	),
	( "thank",	\[x,y]	-> thank y x	)

	]

