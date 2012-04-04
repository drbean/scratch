from __future__ import division
import math, collections

class CustomLanguageModel:

  def __init__(self, corpus):
    """Initialize your data structures in the constructor."""
    self.bigramCounts = collections.defaultdict(lambda: collections.defaultdict(int))
    self.reverseCounts = collections.defaultdict(lambda: collections.defaultdict(int))
    self.unkbigramCounts = collections.defaultdict(lambda: collections.defaultdict(int))
    self.unkreverseCounts = collections.defaultdict(lambda: collections.defaultdict(int))
    self.unigramCounts = collections.defaultdict(lambda: 0)
    self.unkunigramCounts = collections.defaultdict(lambda: 0)
    self.nCounts = collections.defaultdict(lambda: 0)
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
    for sentence in corpus.corpus:
      previous = '' 
      for datum in sentence.data:  
        token = datum.word
        if previous:
          if self.unigramCounts[ previous ] == 1:
            self.unkunigramCounts['<unk>'] = self.unkunigramCounts['<unk>'] + 1
            if self.unigramCounts[ token ] == 1:
              self.unkbigramCounts['<unk>']['<unk>'] = self.unkbigramCounts['<unk>']['<unk>'] + 1
              self.unkreverseCounts['<unk>']['<unk>'] = self.unkreverseCounts['<unk>']['<unk>'] + 1
            else:
              self.unkbigramCounts['<unk>'][token] = self.unkbigramCounts['<unk>'][token] + 1
              self.unkreverseCounts[token]['<unk>'] = self.unkreverseCounts[token]['<unk>'] + 1
          else:
            self.unkunigramCounts[previous] = self.unkunigramCounts[previous] + 1
            if self.unigramCounts[ token ] == 1:
              self.unkbigramCounts[previous]['<unk>'] = self.unkbigramCounts[previous]['<unk>'] + 1
              self.unkreverseCounts['<unk>'][previous] = self.unkreverseCounts['<unk>'][previous] + 1
            else:
              self.unkbigramCounts[previous][token] = self.unkbigramCounts[previous][token] + 1
              self.unkreverseCounts[token][previous] = self.unkreverseCounts[token][previous] + 1
        previous = token
    self.v = len(self.unigramCounts)
    self.newtotal = self.total + self.v
    for key in self.unigramCounts.keys():
      self.n = self.unigramCounts[key]
      self.nCounts[self.n] = self.nCounts[ self.n ] + 1
    self.totalreverse=0
    self.totalunkreverse=0
    for word in self.reverseCounts.keys():
      self.totalreverse += len(self.reverseCounts[word])
    for word in self.unkreverseCounts.keys():
      self.totalunkreverse += len(self.unkreverseCounts[word])

  def score(self, sentence):
    """ Takes a list of strings as argument and returns the log-probability of the 
        sentence using your language model. Use whatever data you computed in train() here.
    """
    score = 0
    unicount = 1
    lamda = 1
    bicount = 1
    reversecount = 1
    totalreverse = 1
    biscore = 0
    previous = '' 
    def d(x):
      if x == 0:
        return 0.99
      elif x == 1:
        return 0.9
      elif x == 2:
        return 0.8
      else:
        return 0.75
    for token in sentence:
      if previous:
        if self.unigramCounts.has_key(previous) and self.unigramCounts[previous]:
          unicount = self.unigramCounts[previous]
          if self.unigramCounts.has_key(token) and self.unigramCounts[token]:
            bicount = self.bigramCounts[previous][token]
            reversecount = len(self.reverseCounts[token])
            totalreverse = self.totalreverse
            lamda = d(bicount) * len(self.bigramCounts[previous]) / unicount
          else:
            bicount = self.unkbigramCounts[previous]['<unk>']
            reversecount = len(self.unkreverseCounts['<unk>'])
            totalreverse = self.totalunkreverse
            lamda = d(bicount) * len(self.unkbigramCounts[previous]) / unicount
        else:
          unicount = self.unkunigramCounts['<unk>']
          if self.unigramCounts.has_key(token) and self.unigramCounts[token]:
            bicount = self.unkbigramCounts['<unk>'][token]
            reversecount = len(self.reverseCounts[token])
            totalreverse = self.totalunkreverse
            lamda = d(bicount) * len(self.unkbigramCounts['<unk>']) / unicount
          else:
            bicount = self.unkbigramCounts['<unk>']['<unk>']
            reversecount = len(self.unkreverseCounts['<unk>'])
            totalreverse = self.totalunkreverse
            lamda = d(bicount) * len(self.unkbigramCounts['<unk>']) / unicount

        biscore = max( 0, (bicount - d(bicount))/ unicount)
        continuation = lamda * reversecount / totalreverse
        subscore = biscore + continuation
        score += math.log(subscore)

      previous = token

    return score

# vim: tabstop=8 expandtab shiftwidth=2 softtabstop=2
