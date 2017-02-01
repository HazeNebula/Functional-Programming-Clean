SHELL:=bash
CLM:=clm
CLMFLAGS:=\
	-ms\
	-I /opt/clean/lib/Gast\
	-I /opt/clean/lib/Dynamics\
	-I /opt/clean/lib/Generics\
	-I /opt/clean/lib/MersenneTwister\
	-I /opt/clean/lib/StdLib\
	-dynamics

PROOFS:=ProofMapsAndTips ProofSubsAndMap ProofInitTake ProofPeano ProofMapFlatten ReturnAndBind ProofMapO ReduceLists Start
OBJECTIO:=Aligning Boid EchoMonad Figure FrequencylistGUI Hangman Mastermind OTP Pesten RandomNumbers Random ReturnAndBind StdIOMonad ToonFileMonad TestFigure WordFrequency WordFrequencyGUI Othello support
ITASK:=BinSearchTreeImage GalgjeWF WF GuessWhat SimpleSVG QA QA_shapes
DONTKNOWWHY:=NotationDynamics Braille IKS GuessingNumbers ListOverloading

NOTEST=AVLTreeTest ZFRemoveAtTest ZFUpdateAtTest StdAVLTreeTest VectorOverloadingTest
NOCOMPILE=$(PROOFS) $(OBJECTIO) $(ITASK) $(DONTKNOWWHY) $(NOTEST)

TESTS=$(filter-out $(NOTEST),$(basename $(wildcard *Test.icl)))
PROGRAMS=$(filter-out $(NOCOMPILE) $(TESTS),$(basename $(wildcard *.icl)))

all: $(PROGRAMS)

test: $(addprefix test,$(TESTS))

test%: %
	./$< 2>&1 | grep -q Passed

%Test: %Test.icl %.icl %.dcl
	$(CLM) $(CLMFLAGS) -b -nt -h 100M $@ -o $@

%: %.icl
	$(CLM) -O $(CLMFLAGS) $@ && touch $@

clean:
	$(RM) -r Clean\ System\ Files $(TESTS) $(PROGRAMS)

.PHONY: test clean all
.SECONDARY: $(TESTS)
