LOAD CSV WITH HEADERS FROM "https://raw.githubusercontent.com/Cheshirrrrrr/TestTask/master/data/molecules.csv?token=AOoOidXQS2R9Cpy18CmYx7wMRd84mQLdks5af0kBwA%3D%3D" AS csvLine
CREATE (m:Molecule {id: toInt(csvLine.id), smiles: csvLine.smiles, iupacName: csvLine.iupacName})
;
CREATE CONSTRAINT ON (m:Molecule) ASSERT m.id IS UNIQUE
;
LOAD CSV WITH HEADERS FROM "https://raw.githubusercontent.com/Cheshirrrrrr/TestTask/master/data/catalysts.csv?token=AOoOieLTD9Ja7Qhy-2Br-IL6nC9gAQKAks5af0kWwA%3D%3D" AS csvLine
CREATE (c:Catalyst {id: toInt(csvLine.id), smiles: csvLine.smiles, name:csvLine.name})
;
CREATE CONSTRAINT ON (c:Catalyst) ASSERT c.id IS UNIQUE
;
LOAD CSV WITH HEADERS FROM "https://raw.githubusercontent.com/Cheshirrrrrr/TestTask/master/data/reactions.csv?token=AOoOiS8bH020fJeywCnvAfh0P_CN0N4Sks5af0kswA%3D%3D" AS csvLine
MATCH (molecule1:Molecule {id: toInt(csvLine.moleculeId1)}),(molecule2:Molecule {id: toInt(csvLine.moleculeId2)})
CREATE (reaction:Reaction {id: toInt(csvLine.id), name: csvLine.name})
CREATE (molecule1)-[:REAGENT_IN]->(reaction)
CREATE (molecule2)-[:REAGENT_IN]->(reaction)
;
CREATE CONSTRAINT ON (r:Reaction) ASSERT r.id IS UNIQUE
;
LOAD CSV WITH HEADERS FROM "https://raw.githubusercontent.com/Cheshirrrrrr/TestTask/master/data/accelerates.csv?token=AOoOiStGf3u6CS3WRP_8Ci_UziH1Nhv-ks5af0k6wA%3D%3D" AS csvLine
MATCH (reaction:Reaction { id: toInt(csvLine.reactionId)}),(catalyst:Catalyst { id: toInt(csvLine.catalystId)})
CREATE (catalyst)-[:ACCELERATE { temperature: toInt(csvLine.temperature), pressure: toInt(csvLine.pressure) }]->(reaction)
;
LOAD CSV WITH HEADERS FROM "https://raw.githubusercontent.com/Cheshirrrrrr/TestTask/master/data/created_molecules.csv?token=AOoOibk_brQYmUrQXyf5PrwkmK-D9rjEks5af0lMwA%3D%3D" AS csvLine
MATCH (molecule:Molecule { id: toInt(csvLine.moleculeId)}),(reaction:Reaction { id: toInt(csvLine.reactionId)})
CREATE (reaction)-[:PRODUCT_FROM { amount: toInt(csvLine.amount) }]->(molecule)
;
