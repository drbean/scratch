module Story_Interpretation where 

import Model

story_objects = [
	( "daughter",	\[x]	-> daughter x	),
	( "mother",	\[x]	-> isMother x	),

	( "electrolarynx",	\[x]	-> electrolarynx x	),
	( "voice",	\[x]	-> voice x	),
	( "gym",	\[x]	-> gym x	),
	( "restaurant",	\[x]	-> restaurant x	),
	( "movie_star",	\[x]	-> movie_star x	),

	( "disability",	\[x] -> disability x	),
	( "throat_cancer",	\[x] -> throat_cancer x	),
	( "sound",	\[x] -> sound x	),
	( "hands_on_hips",	\[x] -> hands_on_hips x	),
	( "shrinking_violet",	\[x] -> shrinking_violet x	),

	( "phone",	\[x] -> phone x	),
	( "computer",	\[x] -> computer x	),
	( "joke",	\[x] -> joke x	),
	( "telemarketers",	\[x] -> telemarketers x	),

	( "year",	\[x] -> year x	),

	( "story",	\[x] -> story x	),
	( "operation",	\[x] -> operation x	)
	]

story_inflections = [
 ( "cancer", "throat_cancer" ),
 ( "daughters", "daughter" ),
 -- ( "lose", "lost" ),
 ( "sound", "sounded" ),
 ( "feel", "felt" ),
 ( "annoy", "annoyed" ),
 ( "offend", "annoyed" ),
 ( "offended", "annoyed" ),
 ( "embarrass", "annoyed" ),
 ( "embarrassed", "annoyed" ),
 ( "leave", "left" ),
 ( "come", "came" ),
 ( "live", "lived" ),
 ( "names", "name" ),
 ( "thanked", "thank" )
 ]

story_relations = [
	( "scared",	\[x]	-> scared x	),
	( "disabled",	\[x]	-> disabled x	),
	( "mentally-disabled",	\[x]	-> mentally_disabled x	),
	( "physically-disabled",	\[x]	-> physically_disabled x	),
	( "older",	\[x]	-> older x	),

	-- ( "lost",	\[x,y] -> lost y x	),
	( "sounded",	\[x,y] -> sounded y x	),
	( "felt",	\[x,y] -> felt y x	),
	( "annoyed",	\[x,y] -> annoyed y x	),
	( "left",	\[x,y]	-> left y x	),
	( "came",	\[x,y]	-> come_from y x	),
	( "lived",	\args -> case args of
		[x,y] -> (lived_with y x || lived_in y x)
		[x,y,z] -> (lived_with_in z y x || lived_with_for z y x)
		[x,y,z,v] -> lived_with_in_for v z y x ),
	( "helped",	\[x,y]	-> helped y x	),
	( "appreciate",	\[x,y]	-> appreciate y x	),
	( "thank",	\[x,y]	-> thank y x	)

	]

