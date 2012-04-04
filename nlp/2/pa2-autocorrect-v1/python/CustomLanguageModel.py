from __future__ import division
import math, collections

class CustomLanguageModel:

  def __init__(self, corpus):
    """Initialize your data structures in the constructor."""
    self.bigramCounts = collections.defaultdict(lambda: collections.defaultdict(int))
    self.unigramCounts = collections.defaultdict(lambda: 0)
    self.unigramCounts['<unk>'] = 1;

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
          self.unigramCounts[previous] = self.unigramCounts[previous] + 1
	previous = token
    self.v = len(self.unigramCounts)
    self.newtotal = self.total + self.v
    for key in self.unigramCounts.keys():
      self.totals[key] = self.unigramCounts[key]

  def score(self, sentence):
    """ Takes a list of strings as argument and returns the log-probability of the 
        sentence using your language model. Use whatever data you computed in train() here.
    """
    score = 9.0 
    previous = '' 
    for token in sentence:
      if previous:
        bicount = self.bigramCounts[previous][token]
	unicount = self.unigramCounts[token]
        if bicount > 0:
          score += math.log(bicount)
          score -= math.log(self.totals[previous])
        elif unicount:
          score += math.log(unicount + 1)
          score -= math.log(self.newtotal)
        else:
          score -= math.log(self.newtotal)
          pass
      previous = token
    return score

# vim: tabstop=8 expandtab shiftwidth=2 softtabstop=2
