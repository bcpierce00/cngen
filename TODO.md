* Short-term tasks
  - Make a typeclass for ownable things (plus conversions with
    individual word ownership)
  - Think aboutr arrays

* Malloc is interesting!
  - However:
      - pKVM programs don't actually allocate or free!
      - there are other tools that can tell when C programs access
        memory out of bounds, so perhaps there's less value in testing
        exhaustively for such behaviors
      - We may want to use our generators with runtime models where we
        don't control malloc (e.g., running our examples as actual C
        programs)
      - So doing complicated stuff for malloc may be lower value
  - Goals:
      - Malloc'd pointers should be able to appear in any order in
        the heap, with any adjacency, with respect to both
        statically and dynamically allocated data
  - Malloc needs to behave randomly (but putting C execution in the
    Gen monad will totally mess up shrinking; hence ...
  - Current proposal:
     - pre-generate an infinite list of instructions to malloc
     - Each instruction is either a number between 0 and 1, where 0
       means "first gap", 1 means "last gap" (after the last
       allocated block), plus another indicator (0-1?) for "where in
       the gap"
     - Think about shrinking, both for heaps and for "malloc schedules"
  - Does the analogy with concurrent testing help?

* Shrinking heaps is interesting!
   - Ultimately, we will want to keep the symbolic heap and shrink
     the concrete heap in light of the symbolic one
   - Shrinking should try to put gaps between data structures
   - Shrinking should also try to reduce the number of
     "inversions" - atoms that appear one way round in the symbolic
     heap and the other in the concrete heap

* Extensions
   - owned parameterized by a type / description of a type / typeclass
      - then: arrays (iterated ownership)
   - extracting SHeap generators from separation logic formulae
      - i.e., boilerplate for triples

* Printing
   - One could make examples easier to understand by running programs
     "semi-symbolically", maintaining the association between symbolic
     locations and concrete ones (plus an indication of whether the
     concrete one "has ever mattered during execution")

* Examples to try:
   - mergesort
   - array examples
   - A wrong version of append that that writes past the end of a
     cons cell (e.g., the one in the middle where the pointer
     swizzling is happening) -- this example is interesting because
     it exercises shrinking (will be hard to debug if we don't
     shrink well)
   - Something involving disjunction
   - queues
   - doubly linked lists
   - Tree whose leaves are also (doubly) linked into a list
   - also see Aiken, Fisher, and Hopkins paper on various
     interesting data structures

* Questions
   - Do we really need symbolic heaps?  Would it actually be harder to
     do the whole translation in one step (i.e., in one monad)?
   - Delete second argument of concretize?
   - Do we need the maximumValid field
   - DO we actually need malloc?  pKVM doesn't use it!
