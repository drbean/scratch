import json, sys
import base64
from Datum import Datum

# import re

class FeatureFactory:
    """
    Add any necessary initialization steps for your features here
    Using this constructor is optional. Depending on your
    features, you may not need to intialize anything.
    """
    def __init__(self):
        self.namelist = ['Fischler', 'Alan']


    """
    Words is a list of the words in the entire corpus, previousLabel is the label
    for position-1 (or O if it's the start of a new sentence), and position
    is the word you are adding features for. PreviousLabel must be the
    only label that is visible to this method. 
    """

    def computeFeatures(self, words, previousLabel, position):
        features = []
        prevWord = ''
        if position > 0:
            prevWord = words[position - 1]
        prevprevWord = ''
        if position > 1:
            prevprevWord = words[position - 2]
        currentWord = words[position]
        self.currentWord = currentWord
        indexes = len(words)
        nextWord = ''
        if position < len(words) - 1:
            nextWord = words[position + 1]
        nextnextWord = ''
        if position < len(words) - 2:
            nextnextWord = words[position + 2]

        if previousLabel == 'PERSON':
            self.namelist.append( prevWord )
            if len(self.namelist) > 300:
                self.namelist = self.namelist[1:]

        """ Baseline Features """
        # features.append("word=" + currentWord)
        # features.append("prevLabel=" + previousLabel)
        # features.append("word=" + currentWord + ", prevLabel=" + previousLabel)
        """
        Warning: If you encounter "line search failure" error when
        running the program, considering putting the baseline features
        back. It occurs when the features are too sparse. Once you have
        added enough features, take out the features that you don't need. 
        """


        """ TODO: Add your features here """
        if currentWord in self.namelist:
            features.append("nameList")
        #else:
        #    features.append("nameNoList")
        if currentWord in self.namelist:
            features.append("nameList" + currentWord)
        #else:
        #    features.append("nameNoList")
        if currentWord[0].isupper() and not currentWord.isupper():
            features.append("case=Title" + currentWord)
        #else:
        #    features.append("case=NoTitle")
        if currentWord and currentWord.isalpha():
            features.append("case=Alpha" + currentWord)
        #else:
        #    features.append("case=NoAlpha")
        if currentWord and (len(currentWord) > 2) and ( currentWord[1] == "'"):
            features.append("case=Irish" + currentWord)
        #else:
        #    features.append("case=NoIrish")
        if currentWord and (len(currentWord) > 2) and ( currentWord[0:1] == "Mc"):
            features.append("case=Mc" + currentWord)
        #else:
        #    features.append("case=NoMac")
        if currentWord and (len(currentWord) > 2) and ( currentWord[0:2] == "Mac"):
            features.append("case=Mac" + currentWord)
        #else:
        #    features.append("case=NoMac")
        if not ( prevWord and prevWord.isupper() ):
            features.append("case=prevCAPS" + currentWord)
        #else:
        #    features.append("case=prevNoCAPS")
        if prevWord and prevWord[0].isupper():
            features.append("case=prevTitle" + currentWord)
        #else:
        #    features.append("case=prevNoTitle")
        if prevWord and previousLabel == "PERSON" and len(prevWord) == 2 and prevWord[0].isupper() and prevWord[-1] == ".":
            features.append("case=prevInitial" + currentWord)
        #else:
        #    features.append("case=prevNoInitial")
        if prevWord and not ( prevWord.lower == "the" or prevWord.lower == "a" ):
            features.append("prevTheArticle" + currentWord)
        #else:
        #    features.append("prevNoTheArticle")

        if prevWord and prevWord[-9:] == "-year-old":
            features.append("prev" + prevWord + currentWord)
        #else:
        #    features.append("prevNoSeeded")
        if prevWord and prevWord[-7:] == "-seeded":
            features.append("prevSeeded" + currentWord)
        #else + currentWord:
        #    features.append("prevNoSeeded")

        if nextWord and not ( nextWord.lower == "city" or currentWord.lower == "city" ):
            features.append("prevCity" + currentWord)
        #else:
        #    features.append("prevNoCity")
        if currentWord[0].isupper and prevWord and prevWord == "'":
            features.append("prevQuoted" + currentWord)
        #else:
        #    features.append("prevNoQuoted" + currentWord)
        if currentWord[0].isupper and prevWord  and prevWord[0].isupper and previousLabel == 'O' and prevprevWord and prevprevWord == "'":
            features.append("prevQuoted" + prevWord + currentWord)
        #else:
        #    features.append("prevNoQuoted" + prevWord)
        if prevWord and prevWord == '"':
            features.append("prevQuote" + currentWord)
        #else:
        #    features.append("prevNoQuote")
        if '"' in words[position-3:position+3]:
            features.append("nearQuote" + currentWord)
        #else:
        #    features.append("nearNoQuote")
        if prevWord and ( currentWord == '"' and prevWord == ',' ):
            features.append("prevCommaQuote" + currentWord)
        #else:
        #    features.append("prevCommaQuote")

        if prevWord and prevWord[0].isupper() and prevWord[-1] == '.':
            features.append("prevDot" + currentWord)
        #else:
        #    features.append("prevNoDot")

        if prevWord and prevWord.lower() == 'coach':
            features.append("prev" + prevWord + currentWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'striker':
            features.append("prev" + prevWord + currentWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'manager':
            features.append("prev" + prevWord + currentWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'leader':
            features.append("prev" + prevWord + currentWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'champion':
            features.append("prev" + prevWord + currentWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'broker':
            features.append("prev" + prevWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and previousLabel == "PERSON" and prevprevWord and prevprevWord.lower() == 'broker':
            features.append("prevprev" + prevWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'captain':
            features.append("prev" + prevWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and previousLabel == "PERSON" and prevprevWord and prevprevWord.lower() == 'captain':
            features.append("prevprev" + prevWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'prince':
            features.append("prev" + prevWord + currentWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'speaker':
            features.append("prev" + prevWord + currentWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'spokesman':
            features.append("prev" + prevWord + currentWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'director':
            features.append("prev" + prevWord + currentWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'doctor':
            features.append("prev" + prevWord + currentWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'minister':
            features.append("prevMinister" + currentWord)
        #else:
        #    features.append("prevNoMinister")
        if prevWord and prevWord.lower() == 'president':
            features.append("prevPres" + currentWord)
        #else:
        #    features.append("prevNoPres")
        if prevWord and prevWord.lower() == 'partner':
            features.append("prevpartner" + currentWord)
        #else:
        #    features.append("prevNopartner")
        if prevWord and prevWord.lower() == 'mate':
            features.append("prevmate" + currentWord)
        #else:
        #    features.append("prevNomate")
        if not (prevWord and prevWord == '\'s'):
            features.append("prevApos" + currentWord)
        #else:
        #    features.append("prevNoApos")
        if prevprevWord and prevprevWord == '\'s':
            features.append("prevprevApos" + currentWord)
        #else:
        #    features.append("prevprevNoApos")
        if prevWord and prevWord[0].isupper():
            features.append("prevWord=Title" + currentWord)
        #else:
        #    features.append("prevWord=NoTitle")
        if any(isinstance(e, str) and e[0].isupper for e in words[position-5:position] ):
            features.append("5prevWord=Title")
        #else:
        #    features.append("5prevWord=NoTitle")

        if prevprevWord and prevprevWord == '\'s':
            features.append("prevprevApos" + currentWord)
        #else:
        #    features.append("prevprevNoApos")

        if prevWord and prevWord == 'and' and prevprevWord and prevprevWord in self.namelist:
            features.append("prevAnd" + currentWord)
        #else:
        #    features.append("prevNoAnd")

        if prevWord and prevWord == ',' and prevprevWord and prevprevWord in self.namelist:
            features.append("prevComma" + currentWord)
        #else:
        #    features.append("prevNoComma")
        if nextWord and nextWord == ',':
            features.append("nextComma" + currentWord)
        #else:
        #    features.append("nextNoComma")

        if nextWord and nextWord == '\'s':
            features.append("nextApos" + currentWord)
        #else:
        #    features.append("nextNoApos")

        if nextWord and nextWord == ',' and nextnextWord and nextnextWord == 'who':
            features.append("nextWho" + currentWord)
        #else:
        #    features.append("nextNoWho")
        if nextWord and nextWord == ',' and nextnextWord and nextnextWord == 'whose':
            features.append("nextWhose" + currentWord)
        #else:
        #    features.append("nextNoWhose")

        if nextWord and nextWord[-2:] == 'ed' and nextnextWord and nextnextWord == 'his':
            features.append("nextnexthis" + currentWord)
        #else:
        #    features.append("nextnextNohis")

        if prevWord and prevWord == 'told':
            features.append("prevtold" + currentWord)
        #else:
        #    features.append("prevNotold")
        if 'told' in words[position-10:position]:
            features.append("10prevWordtoldTitle" + currentWord)
        #else:
        #    features.append("10prevWordtoldNoTitle")
        if nextWord and nextWord == 'told':
            features.append("nexttold" + currentWord)
        #else:
        #    features.append("nextNotold")
        if 'told' in words[position:position+10]:
            features.append("10nextWordtold" + currentWord)
        #else:
        #    features.append("10nextWordNotold")

        if prevWord and prevWord == 'said':
            features.append("prevSaid" + currentWord)
        #else:
        #    features.append("prevNoSaid")
        if 'said' in words[position-5:position]:
            features.append("10prevWordSaidTitle" + currentWord)
        #else:
        #    features.append("10prevWordSaidNoTitle")
        if nextWord and nextWord == 'said':
            features.append("nextSaid" + currentWord)
        #else:
        #    features.append("nextNoSaid")
        if 'said' in words[position:position+5]:
            features.append("10nextWordSaid" + currentWord)
        #else:
        #    features.append("10nextWordNoSaid")

        if prevWord and prevWord == 'spoke':
            features.append("prevSpoke" + currentWord)
        #else:
        #    features.append("prevNoSaid")
        if 'spoke' in words[position-5:position]:
            features.append("10prevWordSpoke" + currentWord)
        #else:
        #    features.append("10prevWordNoSpoke")
        if nextWord and nextWord == 'spoke':
            features.append("nextSpoke" + currentWord)
        #else:
        #    features.append("nextNoSpoke")
        if 'spoke' in words[position:position+5]:
            features.append("10nextWordSpoke" + currentWord)
        #else:
        #    features.append("10nextWordNoSpoke")

        if 'portrays' in words[position-5:position+5]:
            features.append("10nearWordPortray" + currentWord)
        #else:
        #    features.append("10nextWordNoSpoke")

        if previousLabel == "PERSON":
            features.append("prevPERSONLabel=" + currentWord)
        #else:
        #    features.append("prevOLabel="+ previousLabel)

        if previousLabel == "PERSON" and currentWord == "and":
            features.append("notAnd")
        if currentWord == "he":
            features.append("notHe")
        if currentWord == "bat":
            features.append("notBat")
        if currentWord == "against":
            features.append("notAgainst")
        if currentWord == "lbw":
            features.append("not" + currentWord)
        if currentWord == "c":
            features.append("notCaught")
        if currentWord == "b":
            features.append("notBowled")
        if currentWord == "st":
            features.append("notStumped")
        if currentWord == "lost":
            features.append("notlost")
        if currentWord == "the":
            features.append("notthe")
        if currentWord == "title":
            features.append("nottitle")
        if currentWord == "first":
            features.append("not" + currentWord)
        if currentWord == "second":
            features.append("not" + currentWord)
        if currentWord == "third":
            features.append("not" + currentWord)
        if currentWord == "fourth":
            features.append("not" + currentWord)
        if currentWord == "round":
            features.append("not" + currentWord)
        if currentWord == "that":
            features.append("not" + currentWord)
        if currentWord.lower == "president":
            features.append("notpresident" + currentWord)
        if currentWord == "quoted":
            features.append("notquoted")
        if currentWord == "campaign":
            features.append("notcampaign")
        if currentWord.lower == "manager":
            features.append("not" + currentWord) 
        if currentWord.lower == "faroe":
            features.append("not" + currentWord) 
        if currentWord.lower == "islands":
            features.append("not" + currentWord) 
        if currentWord == "&":
            features.append("not&")
        if currentWord == ".":
            features.append("notDot")
        if currentWord == ";":
            features.append("not" + currentWord)
        if any(isinstance(e, str) and e[0].isdigit for e in currentWord):
            features.append("notDigit" + currentWord)

        return features

    """ Do not modify this method """
    def readData(self, filename):
        data = [] 
        
        for line in open(filename, 'r'):
            line_split = line.split()
            # remove emtpy lines
            if len(line_split) < 2:
                continue
            word = line_split[0]
            label = line_split[1]

            datum = Datum(word, label)
            data.append(datum)

        return data

    """ Do not modify this method """
    def readTestData(self, ch_aux):
        data = [] 
        
        for line in ch_aux.splitlines():
            line_split = line.split()
            # remove emtpy lines
            if len(line_split) < 2:
                continue
            word = line_split[0]
            label = line_split[1]

            datum = Datum(word, label)
            data.append(datum)

        return data


    """ Do not modify this method """
    def setFeaturesTrain(self, data):
        newData = []
        words = []

        for datum in data:
            words.append(datum.word)

        ## This is so that the feature factory code doesn't
        ## accidentally use the true label info
        previousLabel = "O"
        for i in range(0, len(data)):
            datum = data[i]

            newDatum = Datum(datum.word, datum.label)
            newDatum.features = self.computeFeatures(words, previousLabel, i)
            newDatum.previousLabel = previousLabel
            newData.append(newDatum)

            previousLabel = datum.label

        return newData

    """
    Compute the features for all possible previous labels
    for Viterbi algorithm. Do not modify this method
    """
    def setFeaturesTest(self, data):
        newData = []
        words = []
        labels = []
        labelIndex = {}

        for datum in data:
            words.append(datum.word)
            if not labelIndex.has_key(datum.label):
                labelIndex[datum.label] = len(labels)
                labels.append(datum.label)
        
        ## This is so that the feature factory code doesn't
        ## accidentally use the true label info
        for i in range(0, len(data)):
            datum = data[i]

            if i == 0:
                previousLabel = "O"
                datum.features = self.computeFeatures(words, previousLabel, i)

                newDatum = Datum(datum.word, datum.label)
                newDatum.features = self.computeFeatures(words, previousLabel, i)
                newDatum.previousLabel = previousLabel
                newData.append(newDatum)
            else:
                for previousLabel in labels:
                    datum.features = self.computeFeatures(words, previousLabel, i)

                    newDatum = Datum(datum.word, datum.label)
                    newDatum.features = self.computeFeatures(words, previousLabel, i)
                    newDatum.previousLabel = previousLabel
                    newData.append(newDatum)

        return newData

    """
    write words, labels, and features into a json file
    Do not modify this method
    """
    def writeData(self, data, filename):
        outFile = open(filename + '.json', 'w')
        for i in range(0, len(data)):
            datum = data[i]
            jsonObj = {}
            jsonObj['_label'] = datum.label
            jsonObj['_word']= base64.b64encode(datum.word)
            jsonObj['_prevLabel'] = datum.previousLabel

            featureObj = {}
            features = datum.features
            for j in range(0, len(features)):
                feature = features[j]
                featureObj['_'+feature] = feature
            jsonObj['_features'] = featureObj
            
            outFile.write(json.dumps(jsonObj) + '\n')
            
        outFile.close()

# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4
