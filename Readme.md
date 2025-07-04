# Readme for Repo

## Meeting July 1st:

### Update Jonas
- Did my minimal TODOs: add the DASS-21 dataset into the empirical data structure
- Question: need a way to get informative unique IDs for both questions and datasets; now it's still a bit awkward

Next steps:
- Set up definition for unique IDs for datasets and questions
- Make pipeline: taking all datasets and corresponding items, compute cors in data and between embedding vectors, and put this into a forest plot
- Then: make a timeline for adding more and more data

### Damiano
- Created item identifiers based on the following formula: identifier = Scale Name + First Three letters Factor + Item Number. Example: item 1 from DASS is "DASS21_DEP_1".
- Added almost all items to the "items_with_ids.json" file. 
- Tested file that calculates consine similarity matrices per scale (it works!)
- Downloaded data folders for most scales and stored in the "Raw Data Folders" folder. Note that these often (but not always) contain item texts as well as the csv data files. 
- Ran into issues for some of the pre-listed clinical (PhQ-9 and NPI) and personality scales (NEO-PI)

Next steps: 
- Output scale similarity matrices in the "Item_data/Processed Data" folder. 
- Discuss if we need to add more scales 

## Jonas
    - Finds empirical data
    - Creates processing function for empirical data
    - Pre-process empirical data
    - Output in Empirical_data/Processed_data folder

## Damiano
    - Finds item data -DONE
    - Creates processing function for item data -DONE
    - Pre-process item data - DONE
    - Output in Item_data/Processed_data folder

- logbook

- Explain folders structure
- Explains processing logic

..... 
