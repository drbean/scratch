import math, collections

class LaplaceBigramLanguageModel:

  def __init__(self, corpus):
    """Initialize your data structures in the constructor."""
    self.bigramCounts = collections.defaultdict(lambda: collections.defaultdict(int))
    self.unigramCounts = collections.defaultdict(lambda: 0)
    self.totals = collections.defaultdict(lambda: 0)
    self.train(corpus)

  def train(self, corpus):
    """ Takes a corpus and trains your language model. 
        Compute any counts or other corpus statistics in this function.
    """  
    for sentence in corpus.corpus:
      previous = '' 
      for datum in sentence.data:  
        token = datum.word
	if previous:
          self.bigramCounts[previous][token] = self.bigramCounts[previous][token] + 1
          self.unigramCounts[previous] = self.unigramCounts[previous] + 1
	previous = token
    self.v = len(self.unigramCounts)
    for key in self.unigramCounts.keys():
      self.totals[key] = self.unigramCounts[key] + self.v

  def score(self, sentence):
    """ Takes a list of strings as argument and returns the log-probability of the 
        sentence using your language model. Use whatever data you computed in train() here.
    """
    score = 9.0 
    previous = '' 
    for token in sentence:
      if previous:
        count = self.bigramCounts[previous][token]
        if count > 0:
          score += math.log(count + 1)
          score -= math.log(self.totals[previous])
        else:
          if self.totals[previous]:
            score -= math.log(self.totals[previous])
          else:
            score -= math.log(self.v)
      previous = token
    return score
