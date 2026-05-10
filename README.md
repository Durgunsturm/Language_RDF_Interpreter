# Language_RDF_Intepreter

## Overview

This language is designed for handling queries on specific turtle file formats.
Downloadable parts of the language are present in the packages folder.

### Required TTL Format

**Either present as a whole triple such as:**  
```
<http://example.org/subj> <http://pre.example.org/path> "Val" .  
<http://example.org/subj> <http://pre.example.org/path> <http://example.org/obj> .  
```

**Or abbreviated using prefix declarations:**  
```
@base <http://example.org/> .  
@prefix pre: <http://pre.example.org/> .  
<subj> pre:path "Val" .  
<subj> pre:path <obj> .  
```

**Further abbreviated with semi-colons if the subject is the same:**  
```
@base <http://example.org/> .  
@prefix pre: <http://pre.example.org/> .  
<subj> pre:path "Val" ; pre:path <obj> .  
```

**With final abbreviations with commas if the path is the same:**  
```
@base <http://example.org/> .  
@prefix pre: <http://pre.example.org/> .  
<subj> pre:path "Val" , <obj> .  
```


## Executing Source Code

### Running the Program
- stack build
- stack run tokeniser-exe -- {filepaths}
- stack run tokeniser-exe -- src/input.txt

### Downloading VSCode Extension
- Download plc-rdf-interpreter vsix file
- Ctrl+Shift+X to open extensions menu
- Press three dots in top right to open more options
- Press install from vsix at the bottom and select the downloaded extension file
- Rename tokeniser files to .plcrdf to prettyprint them