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
    self.d = { 0: 0, 1: 0.5, 2: 0.75 }
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
        realtoken = token
        self.unkbigramCounts['<unk>']['<unk>'] = 1
        self.unkunigramCounts['<unk>'] = 1
	if previous:
          if self.unigramCounts[ previous ] == 1:
            previous = '<unk>'
          if self.unigramCounts[ token ] == 1:
            token = '<unk>'
          self.unkbigramCounts[previous][token] = self.unkbigramCounts[previous][token] + 1
          self.unkreverseCounts[token][previous] = self.unkreverseCounts[token][previous] + 1
          self.unkunigramCounts[previous] = self.unkunigramCounts[previous] + 1
	previous = realtoken
    self.v = len(self.unigramCounts)
    self.newtotal = self.total + self.v
    for key in self.unigramCounts.keys():
      self.n = self.unigramCounts[key]
      self.nCounts[self.n] = self.nCounts[ self.n ] + 1
    self.totalreverse=0
    self.totalunkreverse=0
    for thisword in self.bigramCounts.keys():
      for previousword in self.bigramCounts[thisword]:
        self.totalreverse += self.bigramCounts[thisword][previousword]
    for thisword in self.unkbigramCounts.keys():
      for previousword in self.unkbigramCounts[thisword]:
        self.totalunkreverse += self.unkbigramCounts[thisword][previousword]

  def score(self, sentence):
    """ Takes a list of strings as argument and returns the log-probability of the 
        sentence using your language model. Use whatever data you computed in train() here.
    """
    d = 0.75
    score = 0
    biscore = 0
    previous = '' 
    for token in sentence:
      if previous:
        bicount = self.bigramCounts[previous][token]
	unicount = self.unigramCounts[previous]
        reversecount = len(self.reverseCounts[token])
        unkbicount = self.unkbigramCounts[previous][token]
	unkunicount = self.unkunigramCounts[token]
        unkreversecount = len(self.unkreverseCounts[token])
        if bicount > 0 and unicount > 0:
          biscore = bicount / unicount
          continuation = reversecount / self.totalreverse
        else:
          if not self.unigramCounts.has_key(previous):
            previous = '<unk>'
            unkbicount = self.unkbigramCounts[previous][token]
            biscore = unkbicount / self.unkunigramCounts[previous]
          elif not self.unigramCounts.has_key(token):
            token = '<unk>'
            unkbicount = self.unkbigramCounts[previous][token]
            biscore = unkbicount / self.unkunigramCounts[previous]
          else:
            unkbicount = self.unkbigramCounts['<unk>']['<unk>']
          if previous == '<unk>' or token == '<unk>':
            continuation = unkreversecount / self.totalunkreverse
          else:
            continuation = reversecount / self.totalreverse
        score += math.log(biscore + continuation)
      previous = token
    return score

# vim: tabstop=8 expandtab shiftwidth=2 softtabstop=2
