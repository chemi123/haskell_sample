# NOTE
These notes are for where I got stuck.  
These are the list of notes.

- About Type

## About Type
### What is `Maybe` and `Nothing`?
- `Maybe` is not a concrete type and can't have values. `Maybe` a is a concrete type for any `a`. `Nothing` is a value for `Maybe a` for any `a`.

- The book(Learn You Haskell for Great Good!) defines "concrete type" to mean "fully applied type constructor", or more formally, type of kind *. This is what seems to be causing all the confusion. According to the book's definition, `Maybe a` is concrete and `Maybe` alone is not. So there's absolutely no problem with `Nothing` being of type `Maybe a`. That being said, "concrete" is in no way an official term, and everybody uses it differently.

- `Nothing :: Maybe a` does not take any parameters and therefore it can be called as-is to generate values of type `Maybe a`.

### concreate type?
- "concrete type" means a type which does not take any type parameters or takes parameters with fully applied. The former example is `Int` or `Bool` or something like that and the latter one is `Mayber Char`, `Maybe Int` and so on.

### When type parameters are defined?
- type parameters are used for data type which takes any types like `Maybe a`

### Example for using type constructor which takes type parameters

Let's define two type constructor `Car` and `Car'`. `Car` does not take any type parameters, while `Car'` does.
```
data Car = Car { company :: String,
                 model :: String,
                 year :: Int
               } deriving (Show)


data Car' a b c = Car' { company' :: a,
                         model' :: b,
                         year' :: c
                       } deriving (Show)
```
When you define a value with `Car` type constructor, its fields(`conpany`, `model`, `year`) have to be defined with `String`, `String`, `Int` respectively.  

On the other hand, value with `Car'` type constructor could have fields with any type(like `Int`, `String`).  

In this case, it is not a good way to define type constructor which takes type parameters.  This example is just for an explanation.
