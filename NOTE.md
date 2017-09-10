# NOTE
These notes are for where I got stuck.  
These are the list of notes.

- About Nothing and Maybe

## About Nothing and Maybe
-- Maybe is not a concrete type and can't have values. Maybe a is a concrete type for any a. Nothing is a value for Maybe a for any a.

-- The book defines "concrete type" to mean "fully applied type constructor", or more formally, type of kind *. This is what seems to be causing all the confusion. According to the book's definition Maybe a is concrete, and Maybe alone is not. So there's absolutely no problem with Nothing being of type Maybe a. That being said, "concrete" is in no way an official term, and everybody uses it differently

-- `Nothing :: Maybe a` does not take any parameters and therefore it can be called as-is to generate values of type Maybe a
