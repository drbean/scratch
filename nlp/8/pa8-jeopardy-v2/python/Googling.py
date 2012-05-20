import re

# defines the components of a query result from Google.
class GoogleQuery:
    
    def __init__(self, title, snip, link):
        self.title = title
        self.snip = snip
        self.link = link
    
    '''
    returns the title, snip, and link associated with a Google result
    '''
    def __str__(self):
        return ('title: ' + self.title + '\nsnip: ' + self.snip + '\nlink: ' + self.link)

# note that you should not need to use this class. this class defines the possible locations 
# of a landmark. it differs from the location object in that it stores multiple possible location
# objects, while the location object only stores one possible guess for a city location.
class LocationPossibilities:
    
    def __init__(self, cities, country):
        self.cities = cities
        self.country = country

    '''
    returns the list of all the possible cities along with the country which contains the city
    '''
    def __str__(self):
        locations = ''
        for city in self.cities:
            locations += (city + ', ')
        locations = locations[:-2]
        return ('possible cities: ' + locations + '\ncountry: ' + self.country)

# defines the components of a location.
class Location:
    
    def __init__(self, city, country):
        self.city = city
        self.country = country

    '''
    returns the name of the city and country associated with the landmark
    '''
    def __str__(self):
        return ('city: ' + self.city + '\ncountry: ' + self.country)

class Googling:

    def __init__(self):
        self.us_states = [ "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming" ]
        self.world_countries = [ "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua & Deps", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina", "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Central African Rep(?:\\.|ublic)", "Chad", "Chile", "China", "Colombia", "Comoros", "Congo", "Congo", "Democratic Rep(?:\\.|ublic) of Congo", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Rep(?:\\.|ublic)", "Denmark", "Djibouti", "Dominica", "Dominican Rep(?:\\.|ublic)", "East Timor", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Rep(?:\\.|ublic) of Ireland", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Korea North", "Korea South", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Burma", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Norway", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russian Federation", "Russia", "Rwanda", "(?:Saint|St[\\.]?[ ]?)Kitts & Nevis", "(?:Saint|St[\\.]?[ ]?)Lucia", "(?:Saint|St[\\.]?[ ]?)Vincent & the Grenadines", "Samoa", "San Marino", "Sao Tome & Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Togo", "Tonga", "Trinidad & Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe" ]
    
    # reads in data for a set of results for a single query
    def readInSegment(self, lines):
        queryResults = []
        for i in range(0, len(lines), 3):
            queryResults.append(GoogleQuery(lines[i], lines[i + 1], lines[i + 2]))
        return queryResults
    
    # reads in data from a string rather than a file. assumes the same text file structure as readInData
    def readString(self, infoString):
        queryData = []
        lines = infoString
        startline = 0
        endline = 0
        while startline < len(lines):
            i = startline
            while i < len(lines) and len(lines[i].strip()) > 0: # reads for a query until an empty line or the end of the file
                i += 1
            endline = i
            queryData.append(self.readInSegment(lines[startline:endline]))
            startline = endline + 1 
        return queryData
    
    # reads in the tagged query results output by Google. takes in the name of the file containing the tagged Google results.
    def readInData(self, googleResultsFile):
        queryData = []
        infile = open(googleResultsFile)
        lines = infile.readlines()
        infile.close()
        startline = 0
        endline = 0
        while startline < len(lines):
            i = startline
            while i < len(lines) and len(lines[i].strip()) > 0: # reads for a query until an empty line or the end of the file
                i += 1
            endline = i
            queryData.append(self.readInSegment(lines[startline:endline]))
            startline = endline + 1 
        return queryData
    
    # takes a line and parses out the correct possible locations of the landmark for that line.
    # returns a LocationPossibilities object as well as the associated landmark
    def readGoldEntry(self, line):
        parts = line.split('\t')
        locationParts = parts[2].split(',')
        cities = locationParts[0].split('/')
        return LocationPossibilities(cities, locationParts[1].lower().strip()), parts[1].lower().strip()
    
    # reads in a file containing data about the landmark and where it's located 
    # returns a list of LocationPossibilities object as well as a list of landmarks. takes 
    # in the name of the gold file
    def readInGold(self, goldFile):
        goldData = []
        landmarks = []
        infile = open(goldFile)
        lines = infile.readlines()
        infile.close()
        for line in lines:
            goldEntry, landmark = self.readGoldEntry(line)
            goldData.append(goldEntry)
            landmarks.append(landmark)
        return goldData, landmarks
            
    # in this method, you must return Location object, where the first parameter of the constructor is the city where
    # the landmark is located and the second parameter is the state or the country containing the city. 
    # the return parameter is a Location object. if no good answer is found, returns a GoogleQuery object with
    # empty strings as parameters
    
    # note that the method does not get passed the actual landmark being queried. you do not need this information,
    # as your primary task in this method is to simply extract a guess for the location of the landmark given
    # Google results. you can, however, extract the landmark name from the given queries if you feel that helps.
    def guessLocation(self, data):
        #TODO: use the GoogleQuery object for landmark to generate a tuple of the location
        # of the landmark
        candidate = {}
        city = {}
        region = {}
        city_sort = {}
        region_sort = {}
	for i in range(len(data)):
            info = str(data[i]).split('\n\n')
            for j in range(len(info)):
                locs = self.loc_pattern.findall(info[j])
                for emloc in locs:
                    loc = self.em_pattern.sub('', emloc)
                    if loc in self.world_countries or loc in self.us_states:
                        if loc not in region:
                            region[loc] = 0
                        region[loc] += 1
                    else:
                        if loc not in city:
                            city[loc] = 0
                        city[loc] += 1
            cities = city.keys()
            regions = region.keys()
            city_sort = sorted(cities, key=city.__getitem__, reverse = True)
            region_sort = sorted(regions, key=region.__getitem__, reverse = True)
        else:
            city_sort.append('')
            region_sort.append('')
        return Location(city_sort[0],region_sort[0])
    
    # loops through each of the data associated with each query and passes it into the
    # guessLocation method, which returns the guess of the user
    def processQueries(self, queryData):
        #TODO: this todo is optional. this is for anyone who might want to write any initialization code that should only be performed once.
	self.loc_pattern = re.compile(r"<LOCATION>(.*?)</LOCATION>", re.X)
        self.em_pattern = re.compile(r"<\?em>")
        guesses = [''] * len(queryData)
        for i in range(len(queryData)):
            guesses[i] = self.guessLocation(queryData[i])
        return guesses
    
    # prints out the results as described in the handout
    def printResults(self, correctCities, incorrectCities, noguessCities, correctCountries, incorrectCountries, noguessCountries, landmarks, guesses, gold):
        print('LANDMARK\tYOUR GUESSED CITY\tCORRECT CITY/CITIES\tYOUR GUESSED COUNTRY\tCORRECT COUNTRY')
        correctGuesses = set(correctCities).intersection(set(correctCountries))
        noGuesses = set(noguessCities).union(set(noguessCountries))
        incorrectGuesses = set(incorrectCities).union(set(incorrectCountries))
        print('=====CORRECT GUESSES=====')
        for i in correctGuesses:
            print(landmarks[i] + '\t' + guesses[i].city + '\t' + str(gold[i].cities) + '\t' + guesses[i].country + '\t' + gold[i].country)
        print('=====NO GUESSES=====')
        for i in noGuesses:
            print(landmarks[i] + '\t' + guesses[i].city + '\t' + str(gold[i].cities) + '\t' + guesses[i].country + '\t' + gold[i].country)
        print('=====INCORRECT GUESSES=====')
        for i in incorrectGuesses:
            print(landmarks[i] + '\t' + guesses[i].city + '\t' + str(gold[i].cities) + '\t' + guesses[i].country + '\t' + gold[i].country)
        print('=====TOTAL SCORE=====')
        correctTotal = len(correctCities) + len(correctCountries)
        noguessTotal = len(noguessCities) + len(noguessCountries)
        incorrectTotal = len(incorrectCities) + len(incorrectCountries)
        print('correct guesses: ' + str(correctTotal))
        print('no guesses: ' + str(noguessTotal))
        print('incorrect guesses: ' + str(incorrectTotal))
        print('total score: ' + str(correctTotal - incorrectTotal) + ' out of ' + str(correctTotal + noguessTotal + incorrectTotal))
    
    # takes a list of Location objects and prints a list of correct and incorrect answers as well as scores the results
    def scoreAnswers(self, guesses, gold, landmarks):
        correctCities = []
        incorrectCities = []
        noguessCities = []
        correctCountries = []
        incorrectCountries = []
        noguessCountries = []
        for i in range(len(guesses)):
            if guesses[i].city.lower() in gold[i].cities:
                correctCities.append(i)
            elif guesses[i].city == '':
                noguessCities.append(i)
            else:
                incorrectCities.append(i)
            if guesses[i].country.lower() == gold[i].country.lower():
                correctCountries.append(i)
            elif guesses[i].country == '':
                noguessCountries.append(i)
            else:
                incorrectCountries.append(i)
        self.printResults(correctCities, incorrectCities, noguessCities, correctCountries, incorrectCountries, noguessCountries, landmarks, guesses, gold)
    
if __name__ == '__main__':
    googleResultsFile = '../data/googleResults_tagged.txt' # file where Google query results are read
    goldFile = '../data/landmarks.txt' # contains the results 
    googling = Googling()
    queryData = googling.readInData(googleResultsFile)
    goldData, landmarks = googling.readInGold(goldFile)
    guesses = googling.processQueries(queryData)
    googling.scoreAnswers(guesses, goldData, landmarks)

# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4
