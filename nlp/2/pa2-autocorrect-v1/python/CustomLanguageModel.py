import math, collections

class CustomLanguageModel:

  def __init__(self, corpus):
    """Initialize your data structures in the constructor."""
    self.bigramCounts = collections.defaultdict(lambda: collections.defaultdict(int))
    self.reverseCounts = collections.defaultdict(lambda: collections.defaultdict(int))
    self.unigramCounts = collections.defaultdict(lambda: 0)
    self.nCounts = collections.defaultdict(lambda: 0)
    self.d = { 0: 0, 1: 0.5, 2: 0.75 }
    self.totals = collections.defaultdict(lambda: 0)
    self.total = 0
    self.train(corpus)

  def train(self, corpus):
    """ Takes a corpus and trains your language model. 
        Compute any counts or other corpus statistics in this function.
    """  
    for sentence in corpus.corpus:
      previous = '' 
      for datum in sentence.data:  
        token = datum.word
        self.total = self.total + 1
	if previous:
          self.bigramCounts[previous][token] = self.bigramCounts[previous][token] + 1
          self.reverseCounts[token][previous] = self.reverseCounts[token][previous] + 1
          self.unigramCounts[previous] = self.unigramCounts[previous] + 1
	previous = token
    self.v = len(self.unigramCounts)
    self.newtotal = self.total + self.v
    for key in self.unigramCounts.keys():
      self.totals[key] = self.unigramCounts[key]
      self.n = self.unigramCounts[key]
      self.nCounts[self.n] = self.nCounts[ self.n ] + 1

  def score(self, sentence):
    """ Takes a list of strings as argument and returns the log-probability of the 
        sentence using your language model. Use whatever data you computed in train() here.
    """
    factor = 20.0 
    previous = '' 
    for token in sentence:
      if previous:
        bicount = self.bigramCounts[previous][token]
	unicount = self.unigramCounts[token]
        reversecount = len(self.reverseCounts[token])
        if bicount > 0:
          if unicount < 3:
            score = factor * (bicount - self.d[ unicount ]) / (self.totals[previous])
          else:
            score = factor * (bicount - 0.75 ) / (self.totals[previous])
          # score += 0.5 * reversecount / len(self.reverseCounts)
        elif unicount:
          score = factor * (unicount + 1) / (self.newtotal)
        else:
          score = factor * 1 / (self.newtotal)
      previous = token
    return score

# vim: tabstop=8 expandtab shiftwidth=2 softtabstop=2
