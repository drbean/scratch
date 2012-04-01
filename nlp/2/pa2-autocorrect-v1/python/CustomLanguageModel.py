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
      for datum in sentence.data:  
        token = datum.word
        self.total = self.total + 1
        self.unigramCounts[token] = self.unigramCounts[token] + 1
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
    score = 20.0 
    for token in sentence:
      unicount = self.unigramCounts[token]
      ncount = self.nCounts[ unicount ]
      it = ncount + 1
      if self.nCounts.has_key(it):
        ncountP = self.nCounts[it]
      else:
        ncountP = 0.9 * ncount
      if unicount:
        score += math.log( unicount + 1 ) + math.log(ncountP)
        score -= math.log( ncount )
      else:
        score += math.log( self.nCounts[ 1 ])
        score -= math.log(self.total)
    return score

# vim: tabstop=8 expandtab shiftwidth=2 softtabstop=2
