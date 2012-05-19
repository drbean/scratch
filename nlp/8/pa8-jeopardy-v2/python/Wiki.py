import sys, traceback
# from lxml import etree
# from lxml import objectify
from BeautifulSoup import BeautifulStoneSoup
import re

class Wiki:
    
    # reads in the list of wives
    def addWives(self, wivesFile):
        try:
            input = open(wivesFile)
            wives = input.readlines()
            input.close()
        except IOError:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            traceback.print_tb(exc_traceback)
            sys.exit(1)    
        return wives
    
    # read through the wikipedia file and attempts to extract the matching husbands. note that you will need to provide
    # two different implementations based upon the useInfoBox flag. 
    def processFile(self, f, wives, useInfoBox):
        # pattern = re.compile('\|spouse\s*\=\s*([A-Z][a-z]+).*')
        spouse_of = {}
        xml = f.read()
        tree = BeautifulStoneSoup(xml)
        pages = tree.findAll('page')
        if useInfoBox:
            pattern = re.compile(r"""\| \s* [Ss]pouse
                    \s*=\s* (?: (?: .*\[\[(.*?)\]\])+
                    | ( \w+ \s+ \w+)
                    | ( \w+ \s+ \w+ \s+ \w+)
                    | ( \w+ \s+ \w+ \s+ \w+ \s+ \w+)
                    )""", re.X)
            for page in pages:
                title = page.title.string
                if title == "December 19":
                    continue
                text = page.text
                match = pattern.search(text)
                if not match:
                    continue
                for n in range(0, match.lastindex + 1):
                    group = match.group(n)
                    if group == None:
                        continue
                    words = match.group(n).split()
                    if not all( [ word.isalpha() for word in words ] ):
                        continue
                    wife = str(match.group(n))
                    husband = str(title)
                    spouse_of[ wife ] = husband
                    last_name = re.search('\s+\w+$', wife ).group(0)
                    if last_name:
                        maiden_name = wife.rstrip( last_name )
                        spouse_of[ maiden_name ] = husband
                pass
        else:
            married = re.compile(r""" (?:
                    ([Hh]e) \s+ (?:who)? \s+ married \s+ ((?:[A-Z] \w+ \s+)+ [A-Z] \w+ )
                    |  (\w+) \s+ (?:who)? \s+ married \s+ \[\[(.*?)\]\]
                    )""", re.X)
            marriage = re.compile(r"""(?:
                    [Mm]arriage \s+ to \s+ ((?:[A-Z] \w+ \s+)* [A-Z] \w+ )
                    )""", re.X)
            for page in pages:
                title = page.title.string
                title_parts = str(title).split(' ')
                last_name = title_parts[-1]
                text = page.text
                matches = married.finditer(text)
                for match in matches:
                    for n in range(0, match.lastindex):
                        if not match.group(n) or not match.group(n+1):
                            continue
                        m1 = match.group(n)
                        m2 = match.group(n+1)
                        if m1 == "He" or m1 == "he":
                            husband = str(title)
                        elif all( [ part.isalpha() and part.istitle() for part in title_parts ] ):
                            husband = str(title)
                        else:
                            husband = str(m1)
                        wife = str(m2)
                        names = wife.split('|')
                        for name in names:
                            spouse_of[ name ] = husband
                matches = marriage.finditer(text)
                for match in matches:
                    if not match:
                        continue
                    husband = str(title)
                    wife = str( match.group(1) )
                    wife_names = wife.split(' ')
                    spouse_of[ wife ] = husband
                    spouse_of[ wife + " " + last_name ] = husband

        
        husbands = [] 
        
        # TODO:
        # Process the wiki file and fill the husbands Array
        # +1 for correct Answer, 0 for no answer, -1 for wrong answers
        # add 'No Answer' string as the answer when you dont want to answer

        reverse_spouse = {}
        for person in spouse_of:
            reverse_spouse[ spouse_of[person] ] = person 
        
        for wife in [ string.rstrip() for string in wives]:
            if wife in spouse_of:
                husbands.append( "Who is " + spouse_of[wife] + "?" )
            elif wife in reverse_spouse:
                husbands.append( "Who is " + reverse_spouse[wife] + "?" )
            else:
                husbands.append('No Answer')
        f.close()
        return husbands
    
    # scores the results based upon the aforementioned criteria
    def evaluateAnswers(self, useInfoBox, husbandsLines, goldFile):
        correct = 0
        wrong = 0
        noAnswers = 0
        score = 0 
        try:
            goldData = open(goldFile)
            goldLines = goldData.readlines()
            goldData.close()
            
            goldLength = len(goldLines)
            husbandsLength = len(husbandsLines)
            
            if goldLength != husbandsLength:
                print('Number of lines in husbands file should be same as number of wives!')
                sys.exit(1)
            for i in range(goldLength):
                if husbandsLines[i].strip() in set(goldLines[i].strip().split('|')):
                    correct += 1
                    score += 1
                elif husbandsLines[i].strip() == 'No Answer':
                    noAnswers += 1
                else:
                    wrong += 1
                    score -= 1
        except IOError:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            traceback.print_tb(exc_traceback)
        if useInfoBox:
            print('Using Info Box...')
        else:
            print('No Info Box...')
        print('Correct Answers: ' + str(correct))
        print('No Answers: ' + str(noAnswers))
        print('Wrong Answers: ' + str(wrong))
        print('Total Score: ' + str(score)) 

if __name__ == '__main__':
    wikiFile = '../data/small-wiki.xml'
    wivesFile = '../data/wives.txt'
    goldFile = '../data/gold.txt'
    useInfoBox = True;
    wiki = Wiki()
    wives = wiki.addWives(wivesFile)
    # husbands = wiki.processFile(open(wikiFile), wives, useInfoBox)
    # wiki.evaluateAnswers(useInfoBox, husbands, goldFile)
    husbands = wiki.processFile(open(wikiFile), wives, False)
    wiki.evaluateAnswers(False, husbands, goldFile)

# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4
