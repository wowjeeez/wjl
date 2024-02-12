> I honestly have no clue how I would approach this and I am lazy to read thousands of pages of compiler designs, the below doc contains my braindump.

# Parser

The parser collects the tokens as per node branch, example: `TernaryExpr`
  - The ternary expression has 3 parts. Each part counts as a leaf, these are obviously recursive so I gotta figure something out fo that.
  - Each leaf receives its own hash and each hash is also indexed to the full branch (which is made up of the full token stream that the branch was generated from)
  - Upon wrapping the done expressions into spans, the iterator hashes and collects the consumed tokens and places it into the cache pointing to the expr. Since 1 stream of tokens can only represent 1 thing.
  - To avoid brute forcing the cache on the lexer, we define 3 more fields:
  - Edge (the start and end token of the expr) and the interim length. This will make it easier for the cache resolver to pinpoint nodes in the cache tree.


# IR/ Type checker
- The IR will do the same thing, except for the hash of an AST node, it will bind the inferred/static type information.


# How do we entry the cache? How do we resolve partially cached expressions?

Great question. I dont know.

UPDATE: I figured it out. We start from left to right with the cache edge finding, and if we get stuck, we individually call the parser methods on the yet un-cached token stream and the we put it into the cache. Kind of faking the cache. This way we can find holes in the cache and fill them back accordingly.
