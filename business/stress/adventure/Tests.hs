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
