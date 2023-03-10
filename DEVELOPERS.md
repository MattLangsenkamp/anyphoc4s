# Overview

# Data Types

**Source**: The "initial data". Right now this is only an SVG.
later it will be regions from PROTables. 
In the future this could be Graph structures like SLTs and OPTs as we have
talked about potentially applying PHOC to those structures

**Token**: the atomic unit of each TokenSet. conceptually a token must be able to fit in a split or "bin".
Any data structure that can be broken up to a set of tokens that can be assigned to splits is PHOCable. 
For SVGs a token will be one symbol. 

**Token Set**: The data structure that is produced after the source is tokenized. at the very least it will have a list of tokens.
For an SVG source this would be a list of SVG symbol **Tokens**.

**Token Set Meta**: contains meta information about the **Token Set**, for SVGs this is stuff like the bounding 
box of the entire formula

**Spec**: A config structure that defines how the splits or bins are generated. 
An example would be the HorzLine Spec used to make rectangular splits that extend along the X-axis for SVG PHOCs

**PHOC**: A data structure that holds a map from each token to its PHOC vector,
that includes the sub-vectors from each spec, including the top-level bit

**Partial PHOC**: A data structure that holds a map from each token to its PHOC vector, relative to a given Spec

**Token Set Spec**: A configuration data structure which contains info on how a token set ought to be generated.
For SVG this will have things like scale, and token representation

**Split**: A data structure representing a PHOC region which determines whether
a given token belongs to that split. 
# Functions

**Tokenize**: The tokenizer type class takes in a source and a token-set spec and returns a token set\
Tokenize(Source, TokenSetSpec) -> TokenSet

**prep**: PrepSpec.prep takes in a spec and returns a function from a token set to PartialPhoc\
prep(spec) -> (TokenSet -> PartialPhoc)

**Expand**: Takes in a spec and a tokenset and produces a list of lists of splits.
The first list is a list containing each level (as a list). Each level is a list containing the splits.
The spec is needed as it dictates how each split should be created, and the token set
is needed as it has information about the global state (like formula size in the case of SVGs)\
Expand(Spec, TokenSet) -> List[List[Split]]

**Extract Phoc**: maps over a List[List[Split]] and tests each token
in a token set against that split, keeping track of that result and building a partial phoc out of it.
The proper **TestMembership type class is used.
Extract(TokenSet, List[List[Split]]) -> PartialPhoc

**ComposeSpecs**: a list of functions from TokenSet to PartialPhoc. returns a single function of Tokenset to PHOC
Compose(List(TokenSet -> PartialPhoc)) -> TokenSet -> Phoc
# Pattern
To put it succinctly: A series of specifications for splits are composed together, 
creating a function from a token set to a computed PHOC. No computation is done until a
TokenSet is fed into the function
```
                        Tokenize(Source, TokenSetSpec)->TokenSet
                                                                \
                                                                 \
ComposeSpecs(List(prep(spec1), prep(spec2),...,prep(specN))) -> (TokenSet) -> Phoc
```