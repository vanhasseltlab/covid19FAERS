### Scrape AERSMine ontology                                                            ###
### Patrick de Koning and Laura Zwep                                                    ###
### ---------------------------------------------------------------------------------

import json
from urllib.request import urlopen

codes = [
		 10005329,
		 10007541,
		 10010331,
		 10013993,
		 10014698,
		 10015919,
		 10017947,
		 10018065,
		 10019805,
		 10021428,
		 10021881,
		 10022117,
		 10022891,
		 10027433,
		 10028395,
		 10029104,
		 10029205,
		 10036585,
		 10037175,
		 10038359,
		 10038604,
		 10038738,
		 10040785,
		 10041244,
		 10042613,
		 10047065,
		 10077536
		]

def fetchRecursive(result, code):
	url = "https://research.cchmc.org/aers/ajax/ontologyTree.jsp?ontology=ONTOLOGY_TYPE_REACTIONS&parentNodeId="
	f = urlopen(url + str(code))
	root = json.loads(f.read())
	for child in root:
		result.append(child)
		print("{} : {}".format(child['name'], child['count']))
	
	for child in root:
		if child['hasChildren']:
			result = fetchRecursive(result, child['code'])
			print("{} : {}".format(child['name'], child['count']))
	
	return result

result = []
for code in codes:
	print("")
	print(code)
	result = fetchRecursive(result, code)

with open('out.json', 'w') as outfile:
	json.dump(result, outfile)
