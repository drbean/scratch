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
            if len(self.namelist) > 100:
                del self.namelist[0]
                self.namelist.append( prevWord )

        """ Baseline Features """
        # features.append("word=" + currentWord)
        features.append("prevLabel=" + previousLabel)
        features.append("word=" + currentWord + ", prevLabel=" + previousLabel)
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
        if currentWord[0].isupper():
            features.append("case=Title")
        #else:
        #    features.append("case=NoTitle")
        if currentWord and currentWord.isalpha():
            features.append("case=Alpha")
        #else:
        #    features.append("case=NoAlpha")
        if currentWord and (len(currentWord) > 1) and ( currentWord[1] == "'"):
            features.append("case=Irish")
        #else:
        #    features.append("case=NoIrish")
        if not currentWord and currentWord.isupper():
            features.append("case=CAPS")
        #else:
        #    features.append("case=NoCAPS")
        if not prevWord and prevWord.isupper():
            features.append("case=prevCAPS")
        #else:
        #    features.append("case=prevNoCAPS")
        if prevWord and prevWord[0].isupper():
            features.append("case=prevTitle")
        #else:
        #    features.append("case=prevNoTitle")
        if not prevWord == "the":
            features.append("prevTheArticle")
        #else:
        #    features.append("prevNoTheArticle")
        if prevWord == '"' and prevWord in words[position-5:position+5]:
            features.append("prevQuote")
        #else:
        #    features.append("prevNoQuote")
        if prevWord and prevWord[0].isupper() and prevWord[-1] == '.':
            features.append("prevDot")
        #else:
        #    features.append("prevNoDot")

        if prevWord and prevWord == 'Doctor':
            features.append("prevDoctor")
        #else:
        #    features.append("prevNoDoctor")
        if prevWord and prevWord == 'Minister':
            features.append("prevMinister")
        #else:
        #    features.append("prevNoMinister")
        if prevWord and prevWord == 'President':
            features.append("prevPres")
        #else:
        #    features.append("prevNoPres")
        if prevWord and prevWord == 'partner':
            features.append("prevpartner")
        #else:
        #    features.append("prevNopartner")
        if prevWord and prevWord == 'mate':
            features.append("prevmate")
        #else:
        #    features.append("prevNomate")
        if not prevWord and prevWord == '\'s':
            features.append("prevApos")
        #else:
        #    features.append("prevNoApos")
        if prevWord and prevWord[0].isupper():
            features.append("prevWord=Title")
        #else:
        #    features.append("prevWord=NoTitle")
        if any(isinstance(e, str) and e[0].isupper for e in words[position-5:position] ):
            features.append("5prevWord=Title")
        #else:
        #    features.append("5prevWord=NoTitle")

        if prevprevWord and prevprevWord == '\'s':
            features.append("prevprevApos")
        #else:
        #    features.append("prevprevNoApos")

        if prevWord and prevWord == ',' and prevprevWord in self.namelist:
            features.append("prevComma")
        #else:
        #    features.append("prevNoComma")
        if nextWord and nextWord == ',':
            features.append("nextComma")
        #else:
        #    features.append("nextNoComma")

        if nextWord and nextWord == '\'s':
            features.append("nextApos")
        #else:
        #    features.append("nextNoApos")

        if prevWord and prevWord == 'said':
            features.append("prevSaid")
        #else:
        #    features.append("prevNoSaid")
        if 'said' in words[position-5:position]:
            features.append("5prevWordSaidTitle")
        #else:
        #    features.append("5prevWordSaidNoTitle")
        if nextWord and nextWord == 'said':
            features.append("nextSaid")
        #else:
        #    features.append("nextNoSaid")
        if 'said' in words[position:position+5]:
            features.append("5nextWordSaid")
        #else:
        #    features.append("5nextWordNoSaid")

        if prevWord and prevWord == 'spoke':
            features.append("prevSpoke")
        #else:
        #    features.append("prevNoSaid")
        if 'spoke' in words[position-5:position]:
            features.append("5prevWordSpoke")
        #else:
        #    features.append("5prevWordNoSpoke")
        if nextWord and nextWord == 'spoke':
            features.append("nextSpoke")
        #else:
        #    features.append("nextNoSpoke")
        if 'spoke' in words[position:position+5]:
            features.append("5nextWordSpoke")
        #else:
        #    features.append("5nextWordNoSpoke")


        if previousLabel == "PERSON":
            features.append("prevPERSONLabel=" + previousLabel)
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
