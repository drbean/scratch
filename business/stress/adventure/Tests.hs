module Tests where

import LogicalForm
import Parsing

comp_tests = [
	"Lack of support was stressful.",
	"Uncertainty was stressful.",
	"Lack of control was stressful.",
	"Pressure was stressful."
	]

pressure_test = [
	"Olivier put pressure on Jacques.",
	"Jacques put pressure on Todd.",
	"Todd put pressure on Olivier.",
	"Olivier put pressure on Todd.",
	"Charles put pressure on Todd.",
	"Pressure was stressful."
	]

sail_test = [
	"Steve sailed around the world.",
	"Ellen sailed around the world.",
	"CUSP sailed around the world.",
	"Dr Bean sailed around the world.",
	"Someone sailed around the world.",
	"Steve sailed around the world in a boat.",
	"Ellen sailed around the world in a boat.",
	"CUSP sailed around the world in a balloon.",
	"Dr Bean sailed around the world in a plane.",
	"Someone sailed around the world in a plane."
	]

fly_test = [
	"Did Steve fly around the world.",
	"Steve flew around the world.",
	"Ellen flew around the world.",
	"CUSP flew around the world.",
	"Dr Bean flew around the world.",
	"Someone flew around the world.",
	"Steve flew around the world in a balloon.",
	"Steve flew around the world in a plane.",
	"Ellen flew around the world in a boat.",
	"CUSP flew around the world in a balloon.",
	"Dr Bean flew around the world in a plane.",
	"Someone flew around the world in a plane."
	]

stress_test = [
	"Steve felt stress",
	"Ellen felt stress",
	"Dr Bean felt stress",
	"Steve felt stress because of lack of control.",
	"Steve felt stress because of uncertainty.",
	"Steve felt stress because of lack of support.",
	"Steve felt stress because of pressure.",
	"Ellen felt stress because of lack of control.",
	"Ellen felt stress because of uncertainty.",
	"Ellen felt stress because of lack of support.",
	"Ellen felt stress because of pressure."
	]

len_test = [
	"Who was a man who flew in a balloon.",
	"Who flew a glider."
	]

lilith_test = [
	"Who sailed a boat around the world.",
	"Who flew a glider."
	]
