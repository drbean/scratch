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
        self.wordlist = ['the']


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

        if previousLabel == 'O':
            self.wordlist.append( prevWord )
            if len(self.wordlist) > 3000:
                self.wordlist = self.wordlist[1:]

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
        if (currentWord in self.namelist) and currentWord[0].isupper() and not currentWord.isupper():
            features.append("nameList" + currentWord)
        #else:
        #    features.append("nameNoList")
        if (currentWord in self.wordlist) and not currentWord[0].isupper():
            features.append("wordList" + currentWord)
        #else:
        #    features.append("nameNoList")

        if currentWord[0].isupper() and not currentWord.isupper():
            features.append("Title" + currentWord)
        #else:
        #    features.append("Title")
        if currentWord and currentWord.isalpha():
            features.append("case=Alpha" + currentWord)
        #else:
        #    features.append("case=NoAlpha")
        if currentWord and (len(currentWord) > 2) and ( currentWord[1] == "'"):
            features.append("case=Irish" )
        #else:
        #    features.append("case=NoIrish")
        if currentWord and (len(currentWord) > 2) and ( currentWord[0:1] == "Mc"):
            features.append("case=Mc" )
        #else:
        #    features.append("case=NoMac")
        if currentWord and (len(currentWord) > 2) and ( currentWord[0:2] == "Mac"):
            features.append("case=Mac" )
        #else:
        #    features.append("case=NoMac")
        if prevWord and previousLabel == "PERSON" and len(prevWord) == 2 and prevWord[0].isupper() and prevWord[-1] == ".":
            features.append("case=prevInitial" )
        #else:
        #    features.append("case=prevNoInitial")
        if prevWord and not ( prevWord.lower == "the" or prevWord.lower == "a" ):
            features.append("prevTheArticle" )
        #else:
        #    features.append("prevNoTheArticle")

        if prevWord and prevWord[-9:] == "-year-old":
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNoSeeded")
        if prevWord and prevWord[-7:] == "-seeded":
            features.append("prevSeeded" )
        #else + currentWord:
        #    features.append("prevNoSeeded")

        if nextWord and not ( nextWord.lower == "city" or currentWord.lower == "city" ):
            features.append("prevCity" )
        #else:
        #    features.append("prevNoCity")
        if currentWord[0].isupper and prevWord and prevWord == "'":
            features.append("prevQuoted" )
        #else:
        #    features.append("prevNoQuoted" + currentWord)
        if currentWord[0].isupper and nextWord and nextWord == "'":
            features.append("nextQuoted" )
        #else:
        #    features.append("prevNoQuoted" + currentWord)
        if prevWord and prevWord == '"':
            features.append("prevQuote" )
        #else:
        #    features.append("prevNoQuote")

        if prevWord and prevWord[0].isupper() and prevWord[-1] == '.':
            features.append("prevDot" )
        #else:
        #    features.append("prevNoDot")

        if prevWord and prevWord.lower() == 'chairman' and prevprevWord and prevprevWord[0].isupper():
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevprevWord and prevprevWord.lower() == 'chairman' and prevprevWord and prevprevWord[0].isupper() and previousLabel == 'PERSON':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'governor':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevprevWord and prevprevWord.lower() == 'governor' and previousLabel == 'PERSON':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'coach':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevprevWord and prevprevWord.lower() == 'coach' and previousLabel == 'PERSON':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'striker':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevprevWord and prevprevWord.lower() == 'striker' and previousLabel == 'PERSON':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'manager':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevprevWord and prevprevWord.lower() == 'manager' and previousLabel == 'PERSON':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'leader':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevprevWord and prevprevWord.lower() == 'leader' and previousLabel == 'PERSON':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'champion':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevprevWord and prevprevWord.lower() == 'champion' and previousLabel == 'PERSON':
            features.append("prevprev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'broker':
            features.append("prev" + prevWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevprevWord and prevprevWord.lower() == 'broker' and previousLabel == 'PERSON':
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
        if prevprevWord and prevprevWord.lower() == 'captain' and previousLabel == 'PERSON':
            features.append("prev" + prevWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and previousLabel == "PERSON" and prevprevWord and prevprevWord.lower() == 'captain':
            features.append("prevprev" + prevWord)
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'prince':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'speaker':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevprevWord and prevprevWord.lower() == 'speaker' and previousLabel == 'PERSON':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'spokesman':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevprevWord and prevprevWord.lower() == 'spokesman' and previousLabel == 'PERSON':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'director':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevprevWord and prevprevWord.lower() == 'director' and previousLabel == 'PERSON':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'doctor':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevprevWord and prevprevWord.lower() == 'doctor' and previousLabel == 'PERSON':
            features.append("prev" + prevWord )
        #else:
        #    features.append("prevNo" + prevWord)
        if prevWord and prevWord.lower() == 'minister':
            features.append("prevMinister" )
        #else:
        #    features.append("prevNoMinister")
        if prevprevWord and prevprevWord.lower() == 'minister' and previousLabel == 'PERSON':
            features.append("prevMinister" )
        #else:
        #    features.append("prevNoMinister")
        if prevWord and prevWord.lower() == 'president':
            features.append("prevPres" )
        #else:
        #    features.append("prevNoPres")
        if prevprevWord and prevprevWord.lower() == 'president' and previousLabel == 'PERSON':
            features.append("prevPres" )
        #else:
        #    features.append("prevNoPres")
        if prevWord and prevWord.lower() == 'partner':
            features.append("prevpartner" )
        #else:
        #    features.append("prevNopartner")
        if prevprevWord and prevprevWord.lower() == 'partner' and previousLabel == 'PERSON':
            features.append("prevpartner" )
        #else:
        #    features.append("prevNopartner")
        if prevWord and prevWord.lower() == 'winger':
            features.append("prevmate" )
        #else:
        #    features.append("prevNomate")
        if prevprevWord and prevprevWord.lower() == 'winger' and previousLabel == 'PERSON':
            features.append("prevmate" )
        #else:
        #    features.append("prevNomate")
        if prevWord and prevWord.lower() == 'mate':
            features.append("prevmate" )
        #else:
        #    features.append("prevNomate")
        if prevprevWord and prevprevWord.lower() == 'mate' and previousLabel == 'PERSON':
            features.append("prevmate" )
        #else:
        #    features.append("prevNomate")

        if prevWord and prevWord == 'and' and prevprevWord and prevprevWord in self.namelist:
            features.append("prevAnd" )
        #else:
        #    features.append("prevNoAnd")

        if prevWord and prevWord == ',' and prevprevWord and prevprevWord in self.namelist:
            features.append("prevComma" )
        #else:
        #    features.append("prevNoComma")
        if nextWord and nextWord == ',':
            features.append("nextComma" )
        #else:
        #    features.append("nextNoComma")

        if nextWord and nextWord == ',' and nextnextWord and nextnextWord == 'who':
            features.append("nextWho" )
        #else:
        #    features.append("nextNoWho")
        if nextWord and nextWord == ',' and nextnextWord and nextnextWord == 'whose':
            features.append("nextWhose" )
        #else:
        #    features.append("nextNoWhose")

        if nextWord and nextWord[-2:] == 'ed' and nextnextWord and nextnextWord == 'his':
            features.append("nextnexthis" )
        #else:
        #    features.append("nextnextNohis")

        if prevWord and prevWord == 'told':
            features.append("prevtold" )
        #else:
        #    features.append("prevNotold")
        if 'told' in words[position-3:position]:
            features.append("3prevWordtoldTitle" )
        #else:
        #    features.append("3prevWordtoldNoTitle")
        if nextWord and nextWord == 'told':
            features.append("nexttold" )
        #else:
        #    features.append("nextNotold")
        if 'told' in words[position:position+3]:
            features.append("3nextWordtold" )
        #else:
        #    features.append("3nextWordNotold")

        if currentWord[0].isupper() and prevWord and prevWord == 'said' and prevprevWord and prevprevWord == '"':
            features.append("prevSaid" )
        #else:
        #    features.append("prevNoSaid")
        if currentWord[0].isupper() and prevWord and prevWord[0].isupper and prevprevWord and prevprevWord == 'said':
            features.append("prevprevSaid" )
        #else:
        #    features.append("prevNoSaid")
        if nextWord and nextWord == 'said' and currentWord[0].isupper():
            features.append("nextSaid" )
        #else:
        #    features.append("nextNoSaid")
        if currentWord[0].isupper() and nextWord and nextWord[0].isupper() and nextnextWord and nextnextWord == 'said':
            features.append("nextnextSaid" )
        #else:
        #    features.append("nextNoSaid")

        if prevWord and prevWord == 'spoke':
            features.append("prevSpoke" + currentWord )
        #else:
        #    features.append("prevNoSaid")
        if nextWord and nextWord == 'spoke':
            features.append("nextSpoke" + currentWord )
        #else:
        #    features.append("nextNoSpoke")

        if prevWord and prevWord == 'portrays':
            features.append( prevWord + currentWord )
        #else:
        #    features.append("10nextWordNoSpoke")

        if previousLabel == "PERSON" and prevWord[0].isupper() and not prevWord.isupper() and currentWord[0].isupper() and not currentWord.isupper():
            features.append("prevPERSONLabel=" + prevWord + currentWord )
        #else:
        #    features.append("prevOLabel="+ previousLabel)


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
